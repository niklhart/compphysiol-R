.compiled_ode_model_status <- function(x) {
    if (is.null(.compiled_ode_model_cached_artifact(x))) return("pending")
    "compiled"
}

#' Print method for `CompiledOdeModel` class
#'
#' Pretty-prints the compiled model status, compact ODE model shape, and frozen
#' parameter interface.
#' @param x A `CompiledOdeModel` object.
#' @param ... ignored
#' @returns The `CompiledOdeModel` object (invisibly).
#' @export
print.CompiledOdeModel <- function(x, ...) {
    ode_model <- x$ode_model
    .check_class(ode_model, "OdeModel")

    cat("CompiledOdeModel (", .compiled_ode_model_status(x), "):\n", sep = "")
    cat(" ODE model:\n")
    cat("  States: ", .compiled_ode_model_count(nrow(ode_model$states)), "\n", sep = "")
    cat("  Equations: ", .compiled_ode_model_count(length(ode_model$equations)), "\n", sep = "")
    cat("  Observables: ", .compiled_ode_model_count(length(ode_model$observables)), "\n", sep = "")
    cat("  Dosing events: ", .compiled_ode_model_count(nrow(ode_model$dosing)), "\n", sep = "")
    dimensions <- .compiled_ode_model_print_dimensions(x)
    if (!is.null(dimensions)) {
        cat(" Solver dimensions:\n")
        cat(sprintf("  %s: %s\n", names(dimensions), unlist(dimensions, use.names = FALSE)), sep = "")
    }

    if (length(x$parameterNames) == 0L) {
        cat(" Parameters: none\n")
        return(invisible(x))
    }

    cat(" Parameters:\n")
    parameter_signature <- .compiled_ode_model_print_signature(x)
    parameter_lines <- vapply(
        x$parameterNames,
        .compiled_ode_model_parameter_line,
        character(1),
        model = x,
        signature = parameter_signature
    )
    cat(parameter_lines, sep = "")

    invisible(x)
}

.compiled_ode_model_count <- function(n) {
    if (identical(n, 0L) || identical(n, 0)) return("none")
    as.character(n)
}

.compiled_ode_model_print_signature <- function(x) {
    artifact <- .compiled_ode_model_cached_artifact(x)
    if (is.null(artifact)) return(NULL)
    artifact$parameterSignature
}

.compiled_ode_model_print_dimensions <- function(x) {
    dimensions <- .compiled_ode_model_fixed_dimensions(x)
    if (is.null(dimensions) || length(dimensions) == 0L) return(NULL)
    dimensions
}

.compiled_ode_model_parameter_line <- function(nm, model, signature = NULL) {
    ode_model <- model$ode_model
    if (nm %in% names(ode_model$parameters)) {
        value <- .ode_model_format_expr(ode_model$parameters[[nm]], model = ode_model)
        return(sprintf("  %s: default = %s\n", nm, value))
    }

    unit_label <- "unit signature pending"
    if (!is.null(signature) && nm %in% names(signature)) {
        unit_label <- .compiled_ode_model_signature_label(signature[[nm]])
    }
    sprintf("  %s: required, %s\n", nm, unit_label)
}

.compiled_ode_model_signature_label <- function(signature) {
    if (isTRUE(signature$has_units)) {
        return(paste0("[", as.character(signature$unit), "]"))
    }
    "unitless"
}

.to_deSolve_compiled <- function(model, parameters = list(), dimensions = NULL, merged_parameters = NULL) {
    .check_class(model, "CompiledOdeModel")
    ode_model <- model$ode_model
    .check_class(ode_model, "OdeModel")

    parameters <- .simulation_parameters_object(parameters)
    .compiled_ode_model_check_parameter_names(names(parameters), model$parameterNames)
    merged_parameters <- merged_parameters %||% .merge_ode_parameters(ode_model$parameters, parameters)
    build <- .compiled_ode_model_build_prepared(
        model,
        parameters = merged_parameters,
        dimensions = dimensions
    )
    output_state_names <- ode_model$states$output_name

    y0 <- .evaluate_initials(
        setNames(ode_model$initials, ode_model$states$dsl_name),
        merged_parameters,
        allow_unresolved = FALSE
    ) |>
        .to_dimensions_vec(dimensions)
    y0 <- setNames(unlist(y0), output_state_names)

    list(
        odefun = build$func,
        stateNames = output_state_names,
        dslStateNames = ode_model$states$dsl_name,
        obsFuncs = build$obsFuncs,
        obsParams = build$parms,
        stateUnitValues = build$stateUnitValues,
        stateUnitScales = build$stateUnitScales,
        observableUnitValues = build$observableUnitValues,
        observableExportUnitValues = build$observableExportUnitValues,
        observableUnitScales = build$observableUnitScales,
        freeParams = character(0),
        y0 = y0,
        events = build$events,
        dllname = build$dllname,
        initfunc = build$initfunc,
        parms = build$parms,
        compiled = build
    )
}

.compiled_ode_model_substitute_parameters <- function(expr, eq_names, param_values, dimensions) {
    free_params <- new.env(parent = emptyenv())
    free_params$list <- character()
    out <- .ode_model_substitute_parameters(
        expr,
        eq_names = eq_names,
        param_values = param_values,
        free_params = free_params,
        dimensions = dimensions
    )
    if (length(free_params$list) > 0L) {
        stop(
            "CompiledOdeModel expression contains parameter(s) outside the frozen interface: ",
            paste(free_params$list, collapse = ", "),
            ".",
            call. = FALSE
        )
    }
    out
}

.compiled_ode_model_observable_functions <- function(ode_model, parameter_names, dimensions = NULL) {
    eq_names <- names(ode_model$equations)
    output_state_names <- ode_model$states$output_name
    param_refs <- as.list(stats::setNames(rep("", length(parameter_names)), parameter_names))

    obs_funcs <- lapply(ode_model$observables, function(obs) {
        free_params <- new.env(parent = emptyenv())
        free_params$list <- character()
        expr <- .ode_model_substitute_parameters(
            obs,
            eq_names = eq_names,
            param_values = param_refs,
            free_params = free_params,
            dimensions = dimensions
        )
        outside_interface <- setdiff(free_params$list, parameter_names)
        if (length(outside_interface) > 0L) {
            stop(
                "CompiledOdeModel expression contains parameter(s) outside the frozen interface: ",
                paste(outside_interface, collapse = ", "),
                ".",
                call. = FALSE
            )
        }

        expr <- .ode_model_observable_backend_expr(expr, output_state_names)
        eval(parse(text = paste0("function(t,y,params) unname(", deparse1(expr), ")")))
    })
    names(obs_funcs) <- names(ode_model$observables)
    obs_funcs
}

.compiled_ode_model_build <- function(model, parameters = list(), dimensions = NULL) {
    ode_model <- model$ode_model
    parameters <- .simulation_parameters_object(parameters)
    merged_parameters <- .merge_ode_parameters(ode_model$parameters, parameters)

    .compiled_ode_model_build_prepared(
        model,
        parameters = merged_parameters,
        dimensions = dimensions
    )
}

.compiled_ode_model_build_prepared <- function(model, parameters, dimensions = NULL) {
    parameter_names <- model$parameterNames
    parameter_values <- .compiled_ode_model_parameter_values(
        parameter_names,
        parameters,
        dimensions
    )

    artifact <- .compiled_ode_model_artifact(
        model,
        parameters = parameters,
        dimensions = dimensions
    )

    c(
        artifact,
        list(
            parms = parameter_values,
            parameterNames = parameter_names
        )
    )
}

.compiled_ode_model_artifact <- function(model, parameters, dimensions = NULL) {
    artifact <- .compiled_ode_model_cached_artifact(model)
    if (!is.null(artifact)) {
        fixed_dimensions <- .compiled_ode_model_fixed_dimensions(model)
        if (!is.null(dimensions) && !identical(dimensions, fixed_dimensions)) {
            stop(
                "CompiledOdeModel uses fixed solver dimensions; requested dimensions differ from the compiled model.",
                call. = FALSE
            )
        }
        .compiled_ode_model_check_parameter_signature(
            artifact$parameterSignature,
            parameters = parameters
        )
        return(artifact)
    }

    ode_model <- model$ode_model
    .ode_model_check_unit_consistency(ode_model, parameters)
    source <- .compiled_ode_model_source(
        ode_model,
        parameter_names = model$parameterNames,
        dimensions = dimensions
    )
    paths <- .compiled_ode_model_compile(source)
    artifact <- list(
        func = model$entryPoints$func,
        initfunc = model$entryPoints$initfunc,
        dllname = paths$dllname,
        source = paths$source,
        dll = paths$dll,
        obsFuncs = .compiled_ode_model_observable_functions(
            ode_model,
            parameter_names = model$parameterNames,
            dimensions = dimensions
        ),
        stateUnitValues = .compiled_ode_model_state_unit_values(
            ode_model,
            parameters = parameters,
            dimensions = dimensions
        ),
        observableUnitValues = .simulation_observable_unit_values(
            ode_model,
            parameters = parameters
        ),
        events = .compiled_ode_model_events(
            ode_model,
            dimensions = dimensions
        ),
        dimensions = dimensions,
        parameterSignature = .compiled_ode_model_parameter_signature(
            model$parameterNames,
            parameters
        )
    )
    artifact$observableExportUnitValues <- lapply(artifact$observableUnitValues, function(x) {
        if (inherits(x, "units")) do.call(.to_dimensions, c(list(x), dimensions)) else x
    })
    artifact$stateUnitScales <- .compiled_ode_model_unit_scales(
        from = artifact$stateUnitValues,
        to = artifact$stateUnitValues
    )
    artifact$observableUnitScales <- .compiled_ode_model_unit_scales(
        from = artifact$observableExportUnitValues,
        to = artifact$observableUnitValues
    )

    if (is.environment(model$cache)) {
        assign(".artifact", artifact, envir = model$cache)
        assign(".dimensions", dimensions, envir = model$cache)
    }
    artifact
}

.compiled_ode_model_dimensions <- function(model, ode_model, time, dimensions = NULL, parameters) {
    fixed_dimensions <- .compiled_ode_model_fixed_dimensions(model)
    if (is.null(fixed_dimensions)) {
        resolved <- .simulation_dimensions(ode_model, time, dimensions, parameters = parameters)
        if (is.environment(model$cache)) assign(".dimensions", resolved, envir = model$cache)
        return(resolved)
    }

    if (!is.null(dimensions)) {
        requested <- .simulation_dimensions(ode_model, time, dimensions, parameters = parameters)
        if (!identical(requested, fixed_dimensions)) {
            stop(
                "CompiledOdeModel uses fixed solver dimensions; requested dimensions differ from the compiled model.",
                call. = FALSE
            )
        }
    }

    fixed_dimensions
}

.compiled_ode_model_fixed_dimensions <- function(model) {
    cache <- model$cache
    if (!is.environment(cache) || !exists(".dimensions", envir = cache, inherits = FALSE)) return(NULL)
    get(".dimensions", envir = cache, inherits = FALSE)
}

.compiled_ode_model_cached_artifact <- function(model) {
    cache <- model$cache
    if (!is.environment(cache) || !exists(".artifact", envir = cache, inherits = FALSE)) return(NULL)
    get(".artifact", envir = cache, inherits = FALSE)
}

.compiled_ode_model_events <- function(ode_model, dimensions = NULL) {
    output_state_names <- ode_model$states$output_name
    events <- list(data = data.frame(var = character(), time = numeric(), value = numeric(), method = character()))
    if (length(ode_model$dosing$state) == 0L) return(events)

    event_time <- .to_dimensions_vec(ode_model$dosing$time, dimensions)
    event_value <- .to_dimensions_vec(ode_model$dosing$value, dimensions)
    events$data <- data.frame(
        var = output_state_names[ode_model$dosing$state],
        time = unlist(event_time),
        value = unlist(event_value),
        method = ode_model$dosing$operation,
        stringsAsFactors = FALSE
    )
    events
}

.compiled_ode_model_state_unit_values <- function(ode_model, parameters, dimensions = NULL) {
    state_units <- .simulation_state_unit_values(ode_model, parameters = parameters)
    state_units <- lapply(state_units[ode_model$states$dsl_name], function(x) {
        if (inherits(x, "units")) do.call(.to_dimensions, c(list(x), dimensions)) else x
    })
    names(state_units) <- ode_model$states$output_name
    state_units
}

.compiled_ode_model_unit_scales <- function(from, to) {
    out <- vector("list", length(to))
    names(out) <- names(to)
    for (nm in names(to)) {
        from_unit <- from[[nm]]
        to_unit <- to[[nm]]
        if (inherits(from_unit, "units") && inherits(to_unit, "units")) {
            out[[nm]] <- as.numeric(units::set_units(
                units::set_units(1, .unit_label(from_unit), mode = "standard"),
                .unit_label(to_unit),
                mode = "standard"
            ))
        } else {
            out[[nm]] <- NULL
        }
    }
    out
}

.compiled_ode_model_check_parameter_names <- function(parameter_names, allowed_names) {
    parameter_names <- parameter_names %||% character(0)
    unknown <- setdiff(parameter_names, allowed_names)
    if (length(unknown) == 0L) return(invisible(NULL))

    stop(
        "CompiledOdeModel has a fixed parameter interface; unknown runtime parameter(s): ",
        paste(unknown, collapse = ", "),
        ".",
        call. = FALSE
    )
}

.compiled_ode_model_parameter_signature <- function(parameter_names, parameters) {
    out <- lapply(parameter_names, function(nm) {
        value <- parameters[[nm]]
        if (inherits(value, "units")) {
            list(has_units = TRUE, unit = units(value))
        } else {
            list(has_units = FALSE, unit = NULL)
        }
    })
    names(out) <- parameter_names
    out
}

.compiled_ode_model_check_parameter_signature <- function(signature, parameters) {
    for (nm in names(signature)) {
        expected <- signature[[nm]]
        value <- parameters[[nm]]
        has_units <- inherits(value, "units")
        if (!identical(has_units, expected$has_units)) {
            stop(
                "CompiledOdeModel parameter '",
                nm,
                "' must be ",
                if (expected$has_units) "unit-bearing" else "unitless",
                " to match the cached compiled model signature.",
                call. = FALSE
            )
        }
        if (has_units && !units::ud_are_convertible(units(value), expected$unit)) {
            stop(
                "CompiledOdeModel parameter '",
                nm,
                "' has units ",
                units(value),
                ", but cached compiled model signature expects units convertible to ",
                expected$unit,
                ".",
                call. = FALSE
            )
        }
    }

    invisible(NULL)
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
