.to_deSolve_compiled <- function(model, parameters = list(), dimensions = NULL) {
    .check_class(model, "CompiledOdeModel")
    ode_model <- model$ode_model
    .check_class(ode_model, "OdeModel")

    odeinfo <- .to_deSolve(ode_model, parameters = parameters, dimensions = dimensions)
    build <- .compiled_ode_model_build(model, parameters = parameters, dimensions = dimensions)

    odeinfo$odefun <- build$func
    odeinfo$dllname <- build$dllname
    odeinfo$initfunc <- build$initfunc
    odeinfo$parms <- build$parms
    odeinfo$compiled <- build
    odeinfo
}

.compiled_ode_model_build <- function(model, parameters = list(), dimensions = NULL) {
    ode_model <- model$ode_model
    parameters <- .simulation_parameters_object(parameters)
    merged_parameters <- .merge_ode_parameters(ode_model$parameters, parameters)
    parameter_names <- model$parameterNames
    parameter_values <- .compiled_ode_model_parameter_values(
        parameter_names,
        merged_parameters,
        dimensions
    )

    source <- .compiled_ode_model_source(
        ode_model,
        parameter_names = parameter_names,
        dimensions = dimensions
    )
    paths <- .compiled_ode_model_compile(source)

    list(
        func = model$entryPoints$func,
        initfunc = model$entryPoints$initfunc,
        dllname = paths$dllname,
        source = paths$source,
        dll = paths$dll,
        parms = parameter_values,
        parameterNames = parameter_names
    )
}

.compiled_ode_model_parameter_values <- function(parameter_names, parameters, dimensions) {
    if (length(parameter_names) == 0L) return(numeric(0))

    values <- lapply(parameter_names, function(nm) {
        value <- parameters[[nm]]
        if (is.null(value)) {
            stop("Missing compiled ODE parameter: ", nm, ".", call. = FALSE)
        }
        value <- .to_dimensions_value(value, dimensions)
        if (!is.numeric(value) || length(value) != 1L) {
            stop("Compiled ODE parameter '", nm, "' must be a numeric scalar.", call. = FALSE)
        }
        as.numeric(value)
    })
    setNames(unlist(values, use.names = FALSE), parameter_names)
}

.compiled_ode_model_source <- function(ode_model, parameter_names, dimensions) {
    env <- .compiled_ode_model_codegen_env(parameter_names, dimensions)
    eq_lines <- character()
    for (i in seq_along(ode_model$equations)) {
        eq_lines <- c(
            eq_lines,
            sprintf(
                "  const double %s = %s;",
                .compiled_ode_model_c_identifier(names(ode_model$equations)[[i]]),
                .compiled_ode_model_expr(ode_model$equations[[i]], env)
            )
        )
        env$equations[[names(ode_model$equations)[[i]]]] <-
            .compiled_ode_model_c_identifier(names(ode_model$equations)[[i]])
    }

    rhs_lines <- vapply(seq_along(ode_model$rhs), function(i) {
        sprintf("  ydot[%i] = %s;", i - 1L, .compiled_ode_model_expr(ode_model$rhs[[i]], env))
    }, character(1))

    c(
        "#include <R.h>",
        "#include <math.h>",
        "",
        sprintf("static double parms[%i];", max(1L, length(parameter_names))),
        "",
        "void initmod(void (* odeparms)(int *, double *)) {",
        sprintf("  int N = %i;", length(parameter_names)),
        "  odeparms(&N, parms);",
        "}",
        "",
        "void derivs(int *neq, double *t, double *y, double *ydot, double *yout, int *ip) {",
        "  (void) neq;",
        "  (void) yout;",
        "  (void) ip;",
        eq_lines,
        rhs_lines,
        "}",
        ""
    )
}

.compiled_ode_model_codegen_env <- function(parameter_names, dimensions) {
    list(
        parameter_names = parameter_names,
        parameters = setNames(seq_along(parameter_names) - 1L, parameter_names),
        equations = list(),
        dimensions = dimensions
    )
}

.compiled_ode_model_expr <- function(expr, env) {
    expr <- .as_call(expr)

    render <- function(e) {
        if (is.numeric(e) || is.integer(e)) {
            if (length(e) != 1L) {
                stop("Compiled ODE expressions only support scalar numeric constants.", call. = FALSE)
            }
            return(.compiled_ode_model_number(as.numeric(e)))
        }
        if (inherits(e, "units")) {
            return(.compiled_ode_model_number(as.numeric(.to_dimensions_value(e, env$dimensions))))
        }
        if (is.symbol(e)) {
            nm <- as.character(e)
            if (identical(nm, "t")) return("(*t)")
            if (nm %in% names(env$equations)) return(env$equations[[nm]])
            if (nm %in% names(env$parameters)) {
                return(sprintf("parms[%i]", env$parameters[[nm]]))
            }
            if (identical(nm, "pi")) return(.compiled_ode_model_number(pi))
            if (identical(nm, "Inf")) return("R_PosInf")
            if (identical(nm, "NaN")) return("R_NaN")
            stop("Cannot compile ODE expression: unsupported symbol '", nm, "'.", call. = FALSE)
        }
        if (!is.call(e)) {
            stop("Cannot compile ODE expression: unsupported constant.", call. = FALSE)
        }

        fn <- as.character(e[[1]])
        args <- as.list(e)[-1L]
        if (identical(fn, "(")) return(sprintf("(%s)", render(args[[1]])))
        if (identical(fn, "[")) return(.compiled_ode_model_subset_expr(e, env, render))
        if (fn %in% c("+", "-", "*", "/")) return(.compiled_ode_model_arithmetic_expr(fn, args, render))
        if (identical(fn, "^")) {
            return(sprintf("pow(%s, %s)", render(args[[1]]), render(args[[2]])))
        }
        if (fn %in% .compiled_ode_model_math_functions()) {
            if (length(args) != 1L) {
                stop("Cannot compile ODE expression: function '", fn, "()' requires one argument.", call. = FALSE)
            }
            c_fn <- if (identical(fn, "abs")) "fabs" else fn
            return(sprintf("%s(%s)", c_fn, render(args[[1]])))
        }
        if (fn %in% c("min", "max")) {
            if (length(args) != 2L) {
                stop("Cannot compile ODE expression: function '", fn, "()' requires two arguments.", call. = FALSE)
            }
            c_fn <- if (identical(fn, "min")) "fmin" else "fmax"
            return(sprintf("%s(%s, %s)", c_fn, render(args[[1]]), render(args[[2]])))
        }
        stop("Cannot compile ODE expression: function call '", fn, "()' is not supported.", call. = FALSE)
    }

    render(expr)
}

.compiled_ode_model_subset_expr <- function(expr, env, render) {
    target <- expr[[2]]
    if (identical(target, as.name("y")) && length(expr) == 3L) {
        idx <- expr[[3]]
        if (!is.numeric(idx) || length(idx) != 1L) {
            stop("Cannot compile ODE expression: state index must be numeric.", call. = FALSE)
        }
        return(sprintf("y[%i]", as.integer(idx) - 1L))
    }
    stop("Cannot compile ODE expression: only y[i] state references are supported.", call. = FALSE)
}

.compiled_ode_model_arithmetic_expr <- function(fn, args, render) {
    if (identical(fn, "+") && length(args) == 1L) return(sprintf("(+%s)", render(args[[1]])))
    if (identical(fn, "-") && length(args) == 1L) return(sprintf("(-%s)", render(args[[1]])))
    if (length(args) != 2L) {
        stop("Cannot compile ODE expression: operator '", fn, "' requires one or two arguments.", call. = FALSE)
    }
    sprintf("(%s %s %s)", render(args[[1]]), fn, render(args[[2]]))
}

.compiled_ode_model_math_functions <- function() {
    c(
        "exp", "log", "log10", "sqrt",
        "sin", "cos", "tan",
        "asin", "acos", "atan",
        "sinh", "cosh", "tanh",
        "abs"
    )
}

.compiled_ode_model_number <- function(x) {
    if (is.nan(x)) return("R_NaN")
    if (is.infinite(x)) return(if (x > 0) "R_PosInf" else "R_NegInf")
    formatC(x, digits = 17L, format = "fg", flag = "#")
}

.compiled_ode_model_c_identifier <- function(x) {
    out <- gsub("[^[:alnum:]_]", "_", x)
    out <- gsub("^([0-9])", "_\\1", out)
    if (!nzchar(out)) out <- "x"
    out
}

.compiled_ode_model_compile <- function(source) {
    dir <- tempfile("compphysiol_compiled_ode_")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    stem <- basename(dir)
    source_path <- file.path(dir, paste0(stem, ".c"))
    writeLines(source, source_path, useBytes = TRUE)

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(dir)
    out <- system2(file.path(R.home("bin"), "R"), c("CMD", "SHLIB", basename(source_path)), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status") %||% 0L
    if (!identical(as.integer(status), 0L)) {
        stop(
            "Failed to compile generated ODE C source:\n",
            paste(out, collapse = "\n"),
            call. = FALSE
        )
    }

    dll_path <- file.path(dir, paste0(stem, .Platform$dynlib.ext))
    dyn.load(dll_path)
    list(
        source = source_path,
        dll = dll_path,
        dllname = tools::file_path_sans_ext(basename(dll_path))
    )
}
