#' Create an executable rxode2 model
#'
#' `to_rxode2_model()` lowers a compartment, process, or ODE model through the
#' solver-neutral ODE representation. Compilation is cached on first use,
#' because unit-aware source generation needs the solver dimensions selected at
#' the simulation boundary.
#'
#' @param model A compphysiol compartment, process, or ODE model.
#' @param ... Reserved for future use.
#' @returns A `compphysiol_rxode2_model` object.
#' @export
to_rxode2_model <- function(model, ...) UseMethod("to_rxode2_model")

#' @export
to_rxode2_model.compphysiol_compartment_model <- function(model, ...) {
    to_rxode2_model(to_ode_model(model), ...)
}

#' @export
to_rxode2_model.compphysiol_process_model <- function(model, ...) {
    to_rxode2_model(to_ode_model(model), ...)
}

#' @export
to_rxode2_model.compphysiol_ode_model <- function(model, ...) {
    parameter_names <- sort(unique(c(names(model$parameters), model$freeParams)))
    maps <- .rxode2_identifier_maps(model, parameter_names)
    structure(
        list(
            ode_model = model,
            identifiers = maps,
            source = NULL,
            compiled = NULL,
            parameterNames = parameter_names,
            stateNames = model$states$output_name,
            cache = new.env(parent = emptyenv())
        ),
        class = .class_rxode2_model()
    )
}

.rxode2_identifier_maps <- function(model, parameter_names) {
    safe <- function(x, prefix) {
        stem <- gsub("[^A-Za-z0-9_]", "_", x)
        stem <- gsub("_+", "_", stem)
        candidate <- paste0("cp_", prefix, "_", stem)
        make.unique(candidate, sep = "_")
    }
    list(
        states = stats::setNames(safe(model$states$output_name, "state"), model$states$output_name),
        parameters = stats::setNames(safe(parameter_names, "par"), parameter_names),
        equations = stats::setNames(safe(names(model$equations), "eq"), names(model$equations)),
        observables = stats::setNames(safe(names(model$observables), "obs"), names(model$observables))
    )
}

.rxode2_render_expr <- function(expr, model, dimensions, context) {
    original <- deparse1(expr)
    render <- function(x) {
        if (inherits(x, "units")) return(format(as.numeric(.to_dimensions_value(x, dimensions)), scientific = TRUE))
        if (is.numeric(x)) return(format(x, scientific = TRUE, trim = TRUE))
        if (is.symbol(x)) {
            nm <- as.character(x)
            if (nm %in% names(model$identifiers$parameters)) return(model$identifiers$parameters[[nm]])
            if (nm %in% names(model$identifiers$equations)) return(model$identifiers$equations[[nm]])
            if (nm %in% c("t", "time")) return("t")
            stop("Unsupported symbol '", nm, "' in rxode2 ", context, ": ", original, call. = FALSE)
        }
        if (!is.call(x)) stop("Unsupported value in rxode2 ", context, ": ", original, call. = FALSE)
        fun <- deparse1(x[[1]])
        if (identical(fun, "(")) return(paste0("(", render(x[[2]]), ")"))
        if (identical(fun, "[") && identical(as.character(x[[2]]), "y")) {
            idx <- as.integer(x[[3]])
            if (is.na(idx) || idx < 1L || idx > length(model$stateNames)) {
                stop("Invalid state reference in rxode2 ", context, ": ", original, call. = FALSE)
            }
            return(model$identifiers$states[[model$stateNames[[idx]]]])
        }
        allowed <- c("+", "-", "*", "/", "^", "exp", "log", "log10", "sqrt", "abs",
                     "sin", "cos", "tan", "sinh", "cosh", "tanh", "min", "max", "pmin", "pmax")
        if (!(fun %in% allowed)) {
            stop("Unsupported call '", fun, "' in rxode2 ", context, ": ", original, call. = FALSE)
        }
        args <- vapply(as.list(x[-1]), render, character(1))
        if (fun %in% c("+", "-", "*", "/", "^") && length(args) <= 2L) {
            if (length(args) == 1L) return(paste0("(", fun, args, ")"))
            return(paste0("(", args[[1]], " ", fun, " ", args[[2]], ")"))
        }
        paste0(fun, "(", paste(args, collapse = ", "), ")")
    }
    render(expr)
}

.rxode2_source <- function(model, dimensions) {
    ode <- model$ode_model
    lines <- character()
    for (i in seq_along(ode$equations)) {
        lines <- c(lines, paste0(model$identifiers$equations[[i]], " = ",
            .rxode2_render_expr(ode$equations[[i]], model, dimensions,
                                paste0("equation '", names(ode$equations)[[i]], "'")), ";"))
    }
    for (i in seq_along(ode$rhs)) {
        state <- model$identifiers$states[[i]]
        lines <- c(lines, paste0("d/dt(", state, ") = ",
            .rxode2_render_expr(ode$rhs[[i]], model, dimensions,
                                paste0("right-hand side for '", ode$states$dsl_name[[i]], "'")), ";"))
    }
    paste(lines, collapse = "\n")
}

.rxode2_artifact <- function(model, dimensions) {
    if (!requireNamespace("rxode2", quietly = TRUE)) {
        stop("Package 'rxode2' is required for the rxode2 simulation engine.", call. = FALSE)
    }
    if (exists("artifact", envir = model$cache, inherits = FALSE)) {
        artifact <- get("artifact", envir = model$cache, inherits = FALSE)
        if (!identical(artifact$dimensions, dimensions)) {
            stop("rxode2 model uses fixed solver dimensions; requested dimensions differ from the compiled model.", call. = FALSE)
        }
        return(artifact)
    }
    source <- .rxode2_source(model, dimensions)
    artifact <- list(source = source, compiled = rxode2::rxode2(source), dimensions = dimensions)
    assign("artifact", artifact, envir = model$cache)
    artifact
}

#' @export
print.compphysiol_rxode2_model <- function(x, ...) {
    artifact <- if (exists("artifact", envir = x$cache, inherits = FALSE)) get("artifact", x$cache) else NULL
    cat("rxode2 model (", if (is.null(artifact)) "pending" else "compiled", "):\n", sep = "")
    cat(" States: ", length(x$stateNames), "\n", sep = "")
    cat(" Parameters: ", if (length(x$parameterNames)) paste(x$parameterNames, collapse = ", ") else "none", "\n", sep = "")
    if (!is.null(artifact)) cat(" Source:\n", paste0("  ", strsplit(artifact$source, "\n", fixed = TRUE)[[1]], collapse = "\n"), "\n", sep = "")
    invisible(x)
}

#' @export
simulate.compphysiol_rxode2_model <- function(
    object, nsim = NULL, seed = NULL, time = numeric(0), unit = NULL,
    parameters = list(), dimensions = NULL, engine = rxode2_engine(), ...,
    experiment = NULL
) {
    if (!inherits(engine, "compphysiol_rxode2_engine")) stop("An rxode2 model requires rxode2_engine().", call. = FALSE)
    if (!is.null(experiment)) {
        if (!missing(time) || !missing(unit) || !missing(parameters)) stop("experiment cannot be combined with time, unit, or parameters.", call. = FALSE)
        return(.simulate_experiments(object, experiment, dimensions = dimensions, engine = engine, ...))
    }
    ode <- object$ode_model
    attr(ode, "observation_schedule") <- attr(object, "observation_schedule")
    attr(ode, "experiment_dosing") <- attr(object, "experiment_dosing")
    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    time <- .simulation_apply_time_unit(time, unit)
    .simulation_validate_time(time)
    supplied <- .simulation_parameters_object(parameters)
    unknown <- setdiff(names(supplied), object$parameterNames)
    if (length(unknown)) stop("Unknown rxode2 model parameter(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    merged <- .merge_ode_parameters(ode$parameters, supplied)
    .simulation_check_free_parameters_available(ode, merged)
    .ode_model_check_unit_consistency(ode, merged)
    .simulation_check_time_mode(ode, time, parameters = merged)
    dimensions <- .simulation_dimensions(ode, time, dimensions, parameters = merged)
    artifact <- .rxode2_artifact(object, dimensions)
    info <- .to_deSolve(ode, parameters = supplied, dimensions = dimensions, merged_parameters = merged)
    if (!is.null(attr(ode, "experiment_dosing"))) {
        info$events <- .simulation_experiment_events(ode, attr(ode, "experiment_dosing"), dimensions, merged)
    }
    solver_time <- .simulation_numeric_time(time, dimensions)
    event_data <- info$events$data
    event_data <- event_data[event_data$time >= min(solver_time) & event_data$time <= max(solver_time), , drop = FALSE]
    records <- data.frame(time = solver_time)
    if (nrow(event_data)) {
        dose <- data.frame(time = event_data$time, evid = 1L,
                           cmt = unname(object$identifiers$states[event_data$var]), amt = event_data$value)
        records <- rbind(dose, data.frame(time = solver_time, evid = 0L, cmt = NA_character_, amt = NA_real_))
        records <- records[order(records$time, -records$evid), , drop = FALSE]
    }
    numeric_params <- .simulation_solver_parameters(merged, dimensions)
    numeric_params <- unlist(numeric_params[object$parameterNames], use.names = TRUE)
    if (length(numeric_params)) {
        names(numeric_params) <- unname(object$identifiers$parameters[names(numeric_params)])
    }
    inits <- info$y0
    names(inits) <- unname(object$identifiers$states[names(inits)])
    args <- c(list(object = artifact$compiled, params = numeric_params, inits = inits,
                   events = records, returnType = "data.frame"), engine$arguments, list(...))
    out <- do.call(rxode2::rxSolve, args)
    out <- as.data.frame(out)
    out <- out[out$time %in% solver_time, c("time", unname(object$identifiers$states)), drop = FALSE]
    out <- out[match(solver_time, out$time), , drop = FALSE]
    names(out) <- c("time", object$stateNames)
    matrix_out <- as.matrix(out)
    states <- .simulation_attach_state_units(out, ode, info, dimensions, parameters = merged)
    states$time <- .simulation_attach_time_units(states$time, time, dimensions)
    observables <- .simulation_observables(matrix_out, states$time, ode, info, solver_time, dimensions, parameters = merged)
    structure(list(states = states, observables = observables), class = .compphysiol_class("compphysiol_simulation_result"))
}
