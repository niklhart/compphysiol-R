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
    for (i in seq_along(ode$initials)) {
        state <- model$identifiers$states[[i]]
        lines <- c(lines, paste0(state, "(0) = ",
            .rxode2_render_expr(ode$initials[[i]], model, dimensions,
                                paste0("initial value for '", ode$states$dsl_name[[i]], "'")), ";"))
    }
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

.rxode2_artifact <- function(model, dimensions, parameters = NULL, validate = TRUE) {
    if (!requireNamespace("rxode2", quietly = TRUE)) {
        stop("Package 'rxode2' is required for the rxode2 simulation engine.", call. = FALSE)
    }
    if (exists("artifact", envir = model$cache, inherits = FALSE)) {
        artifact <- get("artifact", envir = model$cache, inherits = FALSE)
        if (!identical(artifact$dimensions, dimensions)) {
            stop("rxode2 model uses fixed solver dimensions; requested dimensions differ from the compiled model.", call. = FALSE)
        }
        if (isTRUE(validate) && !is.null(parameters)) {
            .compiled_ode_model_check_parameter_signature(artifact$parameterSignature, parameters)
        }
        return(artifact)
    }
    if (is.null(parameters)) stop("Parameters are required to initialize the rxode2 model cache.", call. = FALSE)
    .ode_model_check_unit_consistency(model$ode_model, parameters)
    source <- .rxode2_source(model, dimensions)
    ode <- model$ode_model
    state_unit_values <- .compiled_ode_model_state_unit_values(ode, parameters, dimensions)
    observable_unit_values <- .simulation_observable_unit_values(ode, parameters)
    observable_export_unit_values <- lapply(observable_unit_values, function(x) {
        if (inherits(x, "units")) do.call(.to_dimensions, c(list(x), dimensions)) else x
    })
    artifact <- list(
        source = source,
        compiled = rxode2::rxode2(source),
        dimensions = dimensions,
        parameterIndex = stats::setNames(seq_along(model$parameterNames), model$parameterNames),
        defaultParms = .compiled_ode_model_parameter_values(model$parameterNames, parameters, dimensions),
        parameterSignature = .compiled_ode_model_parameter_signature(model$parameterNames, parameters),
        parameterTransforms = .compiled_ode_model_parameter_transforms(
            model$parameterNames, parameters, dimensions
        ),
        obsFuncs = .compiled_ode_model_observable_functions(ode, model$parameterNames, dimensions),
        stateUnitValues = state_unit_values,
        stateUnitScales = .compiled_ode_model_unit_scales(state_unit_values, state_unit_values),
        observableUnitValues = observable_unit_values,
        observableExportUnitValues = observable_export_unit_values,
        observableUnitScales = .compiled_ode_model_unit_scales(observable_export_unit_values, observable_unit_values),
        events = .compiled_ode_model_events(ode, dimensions),
        modelUsesTime = any(vapply(.simulation_dimension_values(ode, parameters), .has_time_dimension, logical(1))),
        modelHasTimeDependentProcess = .simulation_has_time_dependent_process(ode)
    )
    assign("artifact", artifact, envir = model$cache)
    artifact
}

.rxode2_runtime_parameter_values <- function(artifact, parameters, dimensions) {
    values <- artifact$defaultParms
    if (!length(parameters)) return(values)
    for (nm in names(parameters)) {
        idx <- artifact$parameterIndex[[nm]]
        transform <- artifact$parameterTransforms[[nm]]
        use_transform <- !is.null(transform) && (
            is.null(transform$unit) ||
            identical(units::deparse_unit(parameters[[nm]]), transform$unit)
        )
        value <- if (use_transform) {
            transform$offset + transform$scale * as.numeric(parameters[[nm]])
        } else {
            .to_dimensions_value(parameters[[nm]], dimensions)
        }
        if (is.null(idx) || !is.numeric(value) || length(value) != 1L) {
            stop("rxode2 model parameter '", nm, "' must be a numeric scalar in the fixed interface.", call. = FALSE)
        }
        values[[idx]] <- as.numeric(value)
    }
    values
}

.rxode2_runtime_info <- function(model, artifact, parameter_values, events = artifact$events) {
    list(
        stateNames = model$stateNames,
        dslStateNames = model$ode_model$states$dsl_name,
        obsFuncs = artifact$obsFuncs,
        obsParams = parameter_values,
        stateUnitValues = artifact$stateUnitValues,
        stateUnitScales = artifact$stateUnitScales,
        observableUnitValues = artifact$observableUnitValues,
        observableExportUnitValues = artifact$observableExportUnitValues,
        observableUnitScales = artifact$observableUnitScales,
        events = events
    )
}

.simulate_rxode2_experiments <- function(
    object, experiments, dimensions = NULL, engine, homogeneity, prepared = NULL, ...
) {
    ode <- object$ode_model
    supplied <- lapply(experiments, function(e) .simulation_parameters_object(e$parameters))
    parameter_indices <- if (homogeneity[["parameter_schema"]]) 1L else seq_along(supplied)
    unknown <- unique(unlist(lapply(supplied[parameter_indices], function(x) {
        setdiff(names(x), object$parameterNames)
    })))
    if (length(unknown)) stop("Unknown rxode2 model parameter(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)

    shared_time <- homogeneity[["start"]] && homogeneity[["observation_plan"]]
    times <- if (!is.null(prepared)) {
        prepared$times
    } else if (shared_time) {
        rep(list(.simulation_experiment_time(experiments[[1]])), length(experiments))
    } else {
        lapply(experiments, .simulation_experiment_time)
    }
    cached <- if (exists("artifact", envir = object$cache, inherits = FALSE)) get("artifact", object$cache) else NULL
    if (is.null(cached)) {
        merged <- .merge_ode_parameters(ode$parameters, supplied[[1]])
        .simulation_check_free_parameters_available(ode, merged)
        .simulation_check_time_mode(ode, times[[1]], parameters = merged)
        dimensions <- .simulation_dimensions(ode, times[[1]], dimensions, parameters = merged)
        artifact <- .rxode2_artifact(object, dimensions, parameters = merged)
    } else {
        if (!is.null(dimensions) && !identical(dimensions, cached$dimensions)) {
            stop("rxode2 model uses fixed solver dimensions; requested dimensions differ from the compiled model.", call. = FALSE)
        }
        dimensions <- cached$dimensions
        artifact <- cached
    }

    parameter_values <- vector("list", length(experiments))
    solver_times <- if (is.null(prepared)) vector("list", length(experiments)) else {
        prepared$solver_times
    }
    records <- if (!is.null(prepared)) {
        prepared$rx_records
    } else if (shared_time && homogeneity[["dosing"]]) NULL else {
        vector("list", length(experiments))
    }
    shared_events <- if (!is.null(prepared)) {
        prepared$events[[1]]$data
    } else if (homogeneity[["dosing"]]) {
        .simulation_experiment_events(
            ode, experiments[[1]]$dosing, dimensions, supplied[[1]],
            state_unit_values = artifact$stateUnitValues
        )$data
    } else {
        NULL
    }
    for (i in seq_along(experiments)) {
        if (!homogeneity[["parameter_schema"]] || i == 1L) {
            .simulation_check_free_parameter_names_available(
                ode,
                union(names(ode$parameters), names(supplied[[i]]))
            )
            .compiled_ode_model_check_runtime_parameter_signature(
                artifact$parameterSignature, supplied[[i]]
            )
        }
        if (!shared_time || i == 1L) {
            .compiled_ode_model_check_time_mode(artifact, times[[i]])
        }
        parameter_values[[i]] <- if (homogeneity[["parameters"]] && i > 1L) {
            parameter_values[[1]]
        } else {
            .rxode2_runtime_parameter_values(artifact, supplied[[i]], dimensions)
        }
        if (is.null(prepared)) {
            solver_times[[i]] <- .simulation_numeric_time(times[[i]], dimensions)
        }
        if (is.null(prepared) && !is.null(records)) {
            events <- shared_events %||% .simulation_experiment_events(
                ode, experiments[[i]]$dosing, dimensions, supplied[[i]],
                state_unit_values = artifact$stateUnitValues
            )$data
            records[[i]] <- .rxode2_individual_records(
                i, solver_times[[i]], events, object$identifiers$states
            )
        }
    }
    records <- if (!is.null(prepared)) {
        records
    } else if (is.null(records)) {
        .rxode2_shared_experiment_records(
            length(experiments), solver_times[[1]], shared_events,
            object$identifiers$states
        )
    } else {
        records <- do.call(rbind, records)
        records[order(records$id, records$time, -records$evid), , drop = FALSE]
    }

    params <- as.data.frame(do.call(rbind, parameter_values), check.names = FALSE)
    names(params) <- unname(object$identifiers$parameters[names(params)])
    params <- cbind(id = seq_along(experiments), params)
    args <- c(list(object = artifact$compiled, params = params, events = records,
                   returnType = "data.frame"), engine$arguments, list(...))
    solved <- as.data.frame(do.call(rxode2::rxSolve, args))
    solved_rows_by_id <- if (length(experiments) == 1L && !"id" %in% names(solved)) {
        list(`1` = seq_len(nrow(solved)))
    } else {
        split(seq_len(nrow(solved)), solved$id)
    }
    state_columns <- match(c("time", unname(object$identifiers$states)), names(solved))
    shared_observation_plan <- if (is.null(prepared) && shared_time) {
        .simulation_prepare_observation_plan(
            experiments[[1]]$observations,
            .simulation_numeric_time(times[[1]], dimensions),
            dimensions
        )
    } else {
        NULL
    }

    state_rows_by_experiment <- vector("list", length(experiments))
    observable_tables <- vector("list", length(experiments))
    for (i in seq_along(experiments)) {
        solver_time <- solver_times[[i]]
        individual_rows <- solved_rows_by_id[[as.character(i)]]
        candidate_rows <- individual_rows[solved$time[individual_rows] %in% solver_time]
        state_rows <- candidate_rows[match(solver_time, solved$time[candidate_rows])]
        state_rows_by_experiment[[i]] <- state_rows
        solver_output <- solved[state_rows, state_columns, drop = FALSE]
        names(solver_output) <- c("time", object$stateNames)
        matrix_out <- as.matrix(solver_output)
        parameter_values_i <- parameter_values[[i]]
        info <- .rxode2_runtime_info(object, artifact, parameter_values_i)
        ode_i <- ode
        attr(ode_i, "observation_schedule") <- experiments[[i]]$observations
        observation_time <- .simulation_attach_time_units(
            solver_output$time, times[[i]], dimensions
        )
        observables <- .simulation_observables(
            matrix_out, observation_time, ode_i, info, solver_time, dimensions,
            parameters = supplied[[i]],
            observation_plan = if (is.null(prepared)) {
                shared_observation_plan
            } else {
                prepared$observation_plans[[i]]
            }
        )
        observable_tables[[i]] <- observables
    }

    identifiers <- .collection_identifiers(experiments, "Experiment")
    state_rows <- unlist(state_rows_by_experiment, use.names = FALSE)
    states <- solved[state_rows, state_columns, drop = FALSE]
    rownames(states) <- NULL
    names(states) <- c("time", object$stateNames)
    info <- .rxode2_runtime_info(object, artifact, parameter_values[[1]])
    states <- .simulation_attach_state_units(
        states, ode, info, dimensions, parameters = supplied[[1]]
    )
    states$time <- .simulation_attach_time_units(states$time, times[[1]], dimensions)
    states$experiment <- factor(
        rep(identifiers, lengths(state_rows_by_experiment)), levels = identifiers
    )
    states <- states[c("experiment", setdiff(names(states), "experiment"))]
    has_observables <- !vapply(observable_tables, is.null, logical(1))
    if (any(has_observables) && !all(has_observables)) {
        stop("Experiment simulations must consistently return observable output.", call. = FALSE)
    }
    observables <- if (all(has_observables)) {
        .new_observation_data(.simulation_bind_experiment_tables(
            observable_tables, identifiers, "observable output"
        ))
    } else {
        NULL
    }
    structure(
        list(states = states, observables = observables),
        class = .compphysiol_class("compphysiol_simulation_result")
    )
}

.rxode2_individual_records <- function(id, solver_time, events, state_identifiers) {
    events <- events[
        events$time >= min(solver_time) & events$time <= max(solver_time), , drop = FALSE
    ]
    observations <- data.frame(
        id = id, time = solver_time, evid = 0L,
        cmt = NA_character_, amt = NA_real_
    )
    if (nrow(events)) {
        doses <- data.frame(
            id = id, time = events$time, evid = 1L,
            cmt = unname(state_identifiers[events$var]), amt = events$value
        )
        observations <- rbind(doses, observations)
    }
    observations
}

.rxode2_shared_experiment_records <- function(n, solver_time, events, state_identifiers) {
    template <- .rxode2_individual_records(
        1L, solver_time, events, state_identifiers
    )
    template <- template[order(template$time, -template$evid), , drop = FALSE]
    rows_per_individual <- nrow(template)
    data.frame(
        id = rep.int(seq_len(n), rep.int(rows_per_individual, n)),
        time = rep(template$time, times = n),
        evid = rep(template$evid, times = n),
        cmt = rep(template$cmt, times = n),
        amt = rep(template$amt, times = n)
    )
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
    cached <- if (exists("artifact", envir = object$cache, inherits = FALSE)) get("artifact", object$cache) else NULL
    if (is.null(cached)) {
        merged <- .merge_ode_parameters(ode$parameters, supplied)
        .simulation_check_free_parameters_available(ode, merged)
        .simulation_check_time_mode(ode, time, parameters = merged)
        dimensions <- .simulation_dimensions(ode, time, dimensions, parameters = merged)
        artifact <- .rxode2_artifact(object, dimensions, parameters = merged)
    } else {
        .compiled_ode_model_check_runtime_parameter_signature(cached$parameterSignature, supplied)
        .compiled_ode_model_check_time_mode(cached, time)
        if (!is.null(dimensions) && !identical(dimensions, cached$dimensions)) {
            stop("rxode2 model uses fixed solver dimensions; requested dimensions differ from the compiled model.", call. = FALSE)
        }
        dimensions <- cached$dimensions
        artifact <- cached
        merged <- supplied
    }
    parameter_values <- .rxode2_runtime_parameter_values(artifact, supplied, dimensions)
    info <- .rxode2_runtime_info(object, artifact, parameter_values)
    if (!is.null(attr(ode, "experiment_dosing"))) {
        info$events <- .simulation_experiment_events(
            ode, attr(ode, "experiment_dosing"), dimensions, supplied,
            state_unit_values = artifact$stateUnitValues
        )
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
    numeric_params <- parameter_values
    if (length(numeric_params)) {
        names(numeric_params) <- unname(object$identifiers$parameters[names(numeric_params)])
    }
    args <- c(list(object = artifact$compiled, params = numeric_params,
                   events = records, returnType = "data.frame"), engine$arguments, list(...))
    out <- do.call(rxode2::rxSolve, args)
    out <- as.data.frame(out)
    out <- out[out$time %in% solver_time, c("time", unname(object$identifiers$states)), drop = FALSE]
    out <- out[match(solver_time, out$time), , drop = FALSE]
    names(out) <- c("time", object$stateNames)
    matrix_out <- as.matrix(out)
    states <- .simulation_attach_state_units(out, ode, info, dimensions, parameters = supplied)
    states$time <- .simulation_attach_time_units(states$time, time, dimensions)
    observables <- .simulation_observables(matrix_out, states$time, ode, info, solver_time, dimensions, parameters = supplied)
    structure(list(states = states, observables = observables), class = .compphysiol_class("compphysiol_simulation_result"))
}
