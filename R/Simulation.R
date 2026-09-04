#' Simulate a compartment model
#'
#' `simulate()` solves a `CompartmentModel` and returns toolbox-level output.
#' The default route wraps the `deSolve` ODE export and returns ODE states and
#' observables.
#'
#' Simulation is either unit-free or unit-aware with respect to time. If the
#' model uses a time dimension, `time` must carry units through the unit
#' shorthand or the `unit` argument. Conversely, unit-aware simulation times
#' require a model with a time dimension.
#'
#' In unit-aware simulations, `dimensions` defines the solver-facing numerical
#' scale used when unit-bearing model quantities are converted to plain numbers
#' before calling [deSolve::ode()]. This scale also determines how solver
#' tolerances such as `atol` are interpreted. For example, if amounts are
#' converted to kilograms internally, an `atol` value is applied on the kilogram
#' scale even if results are displayed back in milligrams.
#'
#' @param object A `CompartmentModel` object.
#' @param nsim Number of stochastic realizations for `simulation_type = "ssa"`.
#'   Ignored by deterministic simulation routes.
#' @param seed Optional random seed for stochastic simulation routes.
#' @param time Simulation time points. Units can be supplied with the unit
#'   shorthand, e.g. `seq(0, 24) [h]`.
#' @param unit Optional time unit used when `time` is numeric without units.
#' @param parameters Free parameters passed to the ODE solver, as a named list
#'   or `Parameters` object.
#' @param dimensions Named list of unit dimensions defining the numerical scale
#'   used at the solver boundary. These dimensions affect the scale of solver
#'   tolerances such as `atol`.
#' @param simulation_type Simulation route. `"ode"` uses the established
#'   numerical ODE path. `"analytical"` uses matrix exponentials for supported
#'   linear models. `"ssa"` uses Gillespie's stochastic simulation algorithm.
#'   `"hybrid"` uses the Alfonsi hybrid stochastic-deterministic algorithm.
#' @param partition Partitioning for `simulation_type = "hybrid"`. A logical
#'   vector marks stochastic reactions with `TRUE` and deterministic reactions
#'   with `FALSE`. A non-negative numeric scalar enables adaptive partitioning:
#'   reactions with propensities below this threshold are stochastic.
#' @param include_event_times Include stochastic event times in stochastic
#'   simulation output in addition to the requested `time` points.
#' @param ... Additional arguments passed to [deSolve::ode()].
#' @returns A `SimulationResult` object.
#' @examples
#' M <- multiCompModel(ncomp = 1, type = "micro", unit = "mg") |>
#'     add_dosing(time = 0 [h], amount = 100 [mg], cmt = "cen") |>
#'     wire(what = "molec")
#'
#' out <- simulate(
#'     M,
#'     time = seq(0, 24, by = 1) [h],
#'     parameters = parameters(kc0 = 0.2 [1/h], Vcen = 10 [L])
#' )
#' out$states
#' out$observables
#' @importFrom stats simulate
#' @method simulate CompartmentModel
#' @export
simulate.CompartmentModel <- function(
    object,
    nsim = NULL,
    seed = NULL,
    time = numeric(0),
    unit = NULL,
    parameters = list(),
    dimensions = NULL,
    simulation_type = c("ode", "analytical", "ssa", "hybrid"),
    partition = NULL,
    include_event_times = FALSE,
    ...
) {
    simulation_type <- match.arg(simulation_type)

    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    time <- .simulation_apply_time_unit(time, unit)
    .simulation_validate_time(time)

    sim_parameters <- .simulation_parameters_object(parameters)
    if (simulation_type %in% c("ssa", "hybrid")) {
        stochastic_model <- .simulation_model_with_parameters(object, sim_parameters) |>
            to_stochastic_model()
        return(simulate(
            stochastic_model,
            nsim = nsim,
            seed = seed,
            time = time,
            parameters = list(),
            dimensions = dimensions,
            simulation_type = simulation_type,
            partition = partition,
            include_event_times = include_event_times,
            ...
        ))
    }

    export_model <- .simulation_model_with_parameters(object, sim_parameters)
    export_model <- export_model |> wire() |> make_depot()
    if (identical(simulation_type, "analytical")) {
        analytical_model <- to_analytical_model(export_model)
        .simulation_check_free_parameters_available(analytical_model, export_model$parameters)
        export_model <- .check_unit_consistency(export_model)
        .simulation_check_time_mode(export_model, time)

        dimensions <- .simulation_dimensions(export_model, time, dimensions)
        return(simulate(
            analytical_model,
            nsim = nsim,
            seed = seed,
            time = time,
            parameters = list(),
            dimensions = dimensions,
            ...
        ))
    }

    ode_model <- to_ode_model(export_model)
    .simulation_check_free_parameters_available(ode_model, export_model$parameters)
    export_model <- .check_unit_consistency(export_model)
    .simulation_check_time_mode(export_model, time)

    dimensions <- .simulation_dimensions(export_model, time, dimensions)
    odeinfo <- .to_deSolve(ode_model, dimensions = dimensions)

    .simulation_solve_ode_model(
        ode_model,
        odeinfo = odeinfo,
        time = time,
        dimensions = dimensions,
        parameters = export_model$parameters,
        ...
    )
}

#' @export
simulate.OdeModel <- function(
    object,
    nsim = NULL,
    seed = NULL,
    time = numeric(0),
    unit = NULL,
    parameters = list(),
    dimensions = NULL,
    ...
) {
    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    time <- .simulation_apply_time_unit(time, unit)
    .simulation_validate_time(time)

    sim_parameters <- .simulation_parameters_object(parameters)
    merged_parameters <- .merge_ode_parameters(object$parameters, sim_parameters)
    .simulation_check_free_parameters_available(object, merged_parameters)
    .ode_model_check_unit_consistency(object, merged_parameters)
    .simulation_check_time_mode(object, time, parameters = merged_parameters)

    dimensions <- .simulation_dimensions(object, time, dimensions, parameters = merged_parameters)
    odeinfo <- .to_deSolve(object, parameters = sim_parameters, dimensions = dimensions)

    .simulation_solve_ode_model(
        object,
        odeinfo = odeinfo,
        time = time,
        dimensions = dimensions,
        parameters = merged_parameters,
        ...
    )
}

#' @export
simulate.AnalyticalModel <- function(
    object,
    nsim = NULL,
    seed = NULL,
    time = numeric(0),
    unit = NULL,
    parameters = list(),
    dimensions = NULL,
    ...
) {
    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    time <- .simulation_apply_time_unit(time, unit)
    .simulation_validate_time(time)

    sim_parameters <- .simulation_parameters_object(parameters)
    merged_parameters <- .merge_ode_parameters(object$parameters, sim_parameters)
    .analytical_model_check_unit_consistency(object, merged_parameters)
    .simulation_check_time_mode(object, time, parameters = merged_parameters)

    dimensions <- .simulation_dimensions(object, time, dimensions, parameters = merged_parameters)
    solver_time <- .simulation_numeric_time(time, dimensions)
    system <- .analytical_model_numeric_system(object, sim_parameters, dimensions)
    state_matrix <- .analytical_model_solve_states(
        A = system$A,
        b = system$b,
        y0 = system$y0,
        time = solver_time - solver_time[[1]]
    )
    solver_output <- cbind(time = solver_time, state_matrix)
    colnames(solver_output) <- c("time", object$states$output_name)

    analytical_info <- .to_analytical(object, parameters = sim_parameters, dimensions = dimensions)
    states <- as.data.frame(solver_output)
    states <- .simulation_attach_state_units(states, object, analytical_info, dimensions, parameters = merged_parameters)
    states$time <- .simulation_attach_time_units(states$time, time, dimensions)
    observables <- .simulation_observables(
        solver_output,
        states$time,
        object,
        analytical_info,
        solver_time,
        dimensions,
        parameters = merged_parameters
    )

    structure(
        list(
            states = states,
            observables = observables
        ),
        class = "SimulationResult"
    )
}

#' @export
simulate.StochasticModel <- function(
    object,
    nsim = NULL,
    seed = NULL,
    time = numeric(0),
    unit = NULL,
    parameters = list(),
    dimensions = NULL,
    simulation_type = c("ssa", "hybrid"),
    partition = NULL,
    include_event_times = FALSE,
    ...
) {
    simulation_type <- match.arg(simulation_type)
    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    time <- .simulation_apply_time_unit(time, unit)
    .simulation_validate_time(time)
    nsim <- .stochastic_simulation_nsim(nsim)
    include_event_times <- .simulation_include_event_times(include_event_times)

    sim_parameters <- .simulation_parameters_object(parameters)
    merged_parameters <- .merge_ode_parameters(object$parameters, sim_parameters)
    .simulation_check_time_mode(object, time, parameters = merged_parameters)

    dimensions <- .simulation_dimensions(object, time, dimensions, parameters = merged_parameters)
    y0_storage_mode <- if (identical(simulation_type, "hybrid")) "double" else "integer"
    y0 <- .stochastic_model_initial_counts(object, merged_parameters, storage_mode = y0_storage_mode)
    solver_time <- .simulation_numeric_time(time, dimensions)
    propfun <- .stochastic_model_propensity_function(object, merged_parameters, dimensions)
    solver_parameters <- .simulation_solver_parameters(merged_parameters, dimensions)
    partition <- .hybrid_simulation_partition(partition, ncol(object$stoichiometry), simulation_type)

    if (!is.null(seed)) set.seed(seed)

    trajectories <- lapply(seq_len(nsim), function(rep_idx) {
        trajectory <- if (identical(simulation_type, "hybrid")) {
            .hybrid_simulate(
                stoichiometry = object$stoichiometry,
                propensity_function = propfun,
                time = solver_time,
                y0 = y0,
                parameters = solver_parameters,
                partition = partition,
                include_event_times = include_event_times,
                ...
            )
        } else {
            .ssa_simulate(
                stoichiometry = object$stoichiometry,
                propensity_function = propfun,
                time = solver_time,
                y0 = y0,
                parameters = solver_parameters,
                include_event_times = include_event_times
            )
        }

        states <- as.data.frame(trajectory)
        names(states) <- c("time", object$states$output_name)
        states$time <- .simulation_attach_time_units(states$time, time, dimensions)
        if (nsim > 1L) {
            states <- cbind(states["time"], rep = rep_idx, states[object$states$output_name])
        }

        observables <- .stochastic_simulation_observables(
            states = states,
            solver_time = solver_time,
            model = object,
            dimensions = dimensions,
            parameters = merged_parameters
        )
        if (nsim > 1L && !is.null(observables)) {
            observables <- cbind(
                observables["time"],
                rep = rep_idx,
                observables[setdiff(names(observables), "time")]
            )
        }

        list(states = states, observables = observables)
    })

    states <- do.call(rbind, lapply(trajectories, `[[`, "states"))
    rownames(states) <- NULL
    observables <- .stochastic_simulation_bind_observables(lapply(trajectories, `[[`, "observables"))

    structure(
        list(
            states = states,
            observables = observables
        ),
        class = "SimulationResult"
    )
}

.simulation_stop_unimplemented_type <- function(simulation_type) {
    label <- switch(
        simulation_type,
        ssa = "SSA",
        hybrid = "Hybrid",
        simulation_type
    )
    stop(label, " simulation is not implemented yet.", call. = FALSE)
}

.simulation_check_free_parameters_available <- function(model, parameters) {
    missing <- setdiff(model$freeParams, names(parameters))
    if (length(missing) > 0) {
        stop(
            "Missing parameter(s) for simulation: ",
            paste(missing, collapse = ", "),
            ".",
            call. = FALSE
        )
    }

    invisible(model)
}

.simulation_apply_time_unit <- function(time, unit) {
    if (!is.null(unit)) {
        if (inherits(time, "units")) {
            stop("Argument 'unit' can only be used when 'time' does not already have units.", call. = FALSE)
        }
        time <- units::set_units(time, unit, mode = "standard")
    }
    time
}

.simulation_solve_ode_model <- function(model, odeinfo, time, dimensions, parameters, ...) {
    solver_time <- .simulation_numeric_time(time, dimensions)

    solver_args <- list(...)
    solver_args$y <- odeinfo$y0
    solver_args$times <- solver_time
    solver_args$func <- odeinfo$odefun
    solver_args$parms <- .simulation_solver_parameters(parameters, dimensions)
    solver_args$events <- odeinfo$events
    solver_args$rtol <- solver_args$rtol %||% 1e-10
    solver_args$atol <- solver_args$atol %||% 1e-10

    out <- do.call(deSolve::ode, solver_args)
    out <- .simulation_apply_output_events(out, odeinfo$events)

    states <- as.data.frame(out)
    states <- .simulation_attach_state_units(states, model, odeinfo, dimensions, parameters = parameters)
    states$time <- .simulation_attach_time_units(states$time, time, dimensions)
    observables <- .simulation_observables(
        out,
        states$time,
        model,
        odeinfo,
        solver_time,
        dimensions,
        parameters = parameters
    )

    structure(
        list(
            states = states,
            observables = observables
        ),
        class = "SimulationResult"
    )
}

.simulation_apply_output_events <- function(out, events) {
    event_data <- events$data
    if (is.null(event_data) || nrow(event_data) == 0) return(out)

    output_times <- out[, "time"]
    for (i in seq_len(nrow(event_data))) {
        row_idx <- which(output_times == event_data$time[[i]])
        if (length(row_idx) == 0) next

        var <- event_data$var[[i]]
        if (!(var %in% colnames(out))) next

        value <- event_data$value[[i]]
        method <- event_data$method[[i]]
        if (method %in% c("add", "2")) {
            out[row_idx, var] <- out[row_idx, var] + value
        } else if (method %in% c("replace", "rep", "1")) {
            out[row_idx, var] <- value
        } else if (method %in% c("multiply", "mult", "3")) {
            out[row_idx, var] <- out[row_idx, var] * value
        } else {
            stop("Unknown event method: ", method, call. = FALSE)
        }
    }

    out
}

#' Print a simulation result
#'
#' @param x A `SimulationResult` object.
#' @param ... Additional arguments, currently ignored.
#' @returns The `SimulationResult` object, invisibly.
#' @export
print.SimulationResult <- function(x, ...) {
    has_replicates <- "rep" %in% names(x$states)
    n_rep <- if (has_replicates) length(unique(x$states$rep)) else 1L
    n_time <- if (has_replicates) {
        nrow(x$states) / n_rep
    } else {
        nrow(x$states)
    }
    state_names <- setdiff(names(x$states), c("time", "rep"))
    observable_names <- if (is.null(x$observables)) character(0) else setdiff(names(x$observables), c("time", "rep"))
    n_states <- length(state_names)
    n_observables <- length(observable_names)
    time_values <- if (has_replicates) x$states$time[x$states$rep == x$states$rep[[1]]] else x$states$time
    time_span <- if (length(time_values) > 0) {
        sprintf("%s to %s", format(time_values[[1]]), format(time_values[[length(time_values)]]))
    } else {
        "empty"
    }

    cat(" SimulationResult:\n")
    cat(sprintf("  time: %s (%s points)\n", time_span, n_time))
    if (has_replicates) {
        cat(sprintf("  replicates: %s\n", n_rep))
    }
    cat(sprintf("  states: %s", n_states))
    if (n_states > 0) {
        cat(sprintf(" (%s)", .simulation_format_names(state_names, prefix = "  states: ")))
    }
    cat("\n")
    cat(sprintf("  observables: %s", n_observables))
    if (n_observables > 0) {
        cat(sprintf(" (%s)", .simulation_format_names(observable_names, prefix = "  observables: ")))
    }
    cat("\n")

    invisible(x)
}

.simulation_format_names <- function(names, prefix) {
    width <- getOption("width", 80)
    max_chars <- max(width - nchar(prefix) - 8, 20)
    shown <- character(0)

    for (name in names) {
        candidate <- paste(c(shown, name), collapse = ", ")
        remaining <- length(names) - length(shown) - 1
        suffix <- if (remaining > 0) sprintf(", ... +%s more", remaining) else ""
        if (nchar(candidate) + nchar(suffix) > max_chars) break
        shown <- c(shown, name)
    }

    if (length(shown) == 0) {
        suffix <- if (length(names) > 1) sprintf(", ... +%s more", length(names) - 1) else ""
        name_width <- max(max_chars - nchar(suffix), 8)
        return(sprintf("%s%s", .simulation_shorten_name(names[[1]], name_width), suffix))
    }

    remaining <- length(names) - length(shown)
    out <- paste(shown, collapse = ", ")
    if (remaining > 0) {
        out <- sprintf("%s, ... +%s more", out, remaining)
    }
    out
}

.simulation_shorten_name <- function(name, width) {
    if (nchar(name) <= width) return(name)
    paste0(substr(name, 1, max(width - 3, 1)), "...")
}

.simulation_validate_time <- function(time) {
    if (!is.numeric(time)) {
        stop("Argument 'time' must be numeric.", call. = FALSE)
    }
    if (length(time) == 0) {
        stop("Argument 'time' must contain at least one time point.", call. = FALSE)
    }
    time_values <- as.numeric(time)
    if (anyNA(time_values) || any(!is.finite(time_values))) {
        stop("Argument 'time' must not contain missing or non-finite values.", call. = FALSE)
    }
    if (any(diff(time_values) < 0)) {
        stop("Argument 'time' must be sorted in non-decreasing order.", call. = FALSE)
    }

    invisible(NULL)
}

.simulation_dimensions <- function(model, time, dimensions, parameters = model$parameters) {
    dimensions <- dimensions %||% list()

    if (inherits(time, "units") && is.null(dimensions$time)) {
        dimensions$time <- .unit_label(time)
    }

    for (value in .simulation_dimension_values(model, parameters = parameters)) {
        dimensions <- .infer_dimensions_from_unit(value, dimensions)
    }

    dimensions
}

.simulation_check_time_mode <- function(model, time, parameters = model$parameters) {
    model_uses_time <- any(vapply(.simulation_dimension_values(model, parameters = parameters), .has_time_dimension, logical(1)))
    model_has_time_dependent_process <- .simulation_has_time_dependent_process(model)
    time_has_units <- inherits(time, "units")

    if (model_uses_time && !time_has_units) {
        stop("Cannot simulate: model uses time units but simulation time is unit-free.", call. = FALSE)
    }
    if (!model_uses_time && model_has_time_dependent_process && time_has_units) {
        stop("Cannot simulate: simulation time has units but the model is unit-free in time.", call. = FALSE)
    }

    invisible(NULL)
}

.simulation_has_time_dependent_process <- function(model) {
    if (inherits(model, "OdeModel")) {
        has_rhs <- any(vapply(model$rhs, function(expr) !identical(expr, 0), logical(1)))
        return(has_rhs || length(model$dosing$state) > 0)
    }
    if (inherits(model, "AnalyticalModel")) {
        has_A <- any(!vapply(as.list(model$A), .analytical_model_is_zero, logical(1)))
        has_b <- any(!vapply(model$b, .analytical_model_is_zero, logical(1)))
        return(has_A || has_b)
    }
    if (inherits(model, "StochasticModel")) {
        return(length(model$propensities) > 0)
    }
    model <- model |> wire() |> make_depot()
    length(model$transports) > 0 ||
        length(model$reactions) > 0 ||
        length(model$doses) > 0
}

.simulation_numeric_time <- function(time, dimensions) {
    if (!inherits(time, "units")) return(time)
    converted <- do.call(.to_dimensions, c(list(time), dimensions))
    units::set_units(converted, NULL)
}

.simulation_attach_time_units <- function(time, original_time, dimensions) {
    if (inherits(original_time, "units")) {
        return(units::set_units(time, .unit_label(do.call(.to_dimensions, c(list(original_time), dimensions))), mode = "standard"))
    }
    time
}

.simulation_attach_state_units <- function(states, model, odeinfo, dimensions, parameters = model$parameters) {
    state_units <- .simulation_state_unit_values(model, parameters = parameters)
    state_units <- lapply(state_units[odeinfo$dslStateNames], function(x) {
        if (inherits(x, "units")) do.call(.to_dimensions, c(list(x), dimensions)) else x
    })
    names(state_units) <- odeinfo$stateNames

    for (state_name in intersect(names(state_units), names(states))) {
        unit_value <- state_units[[state_name]]
        if (inherits(unit_value, "units")) {
            states[[state_name]] <- units::set_units(states[[state_name]], .unit_label(unit_value), mode = "standard")
        }
    }

    states
}

.simulation_observables <- function(solver_output, time, model, odeinfo, solver_time, dimensions, parameters = model$parameters) {
    if (length(odeinfo$obsFuncs) == 0) return(NULL)

    values <- lapply(odeinfo$obsFuncs, function(f) {
        f(solver_time, solver_output, list())
    })
    observables <- as.data.frame(values)
    names(observables) <- names(odeinfo$obsFuncs)
    observables <- cbind(data.frame(time = time), observables)

    obs_units <- .simulation_observable_unit_values(model, parameters = parameters)
    for (obs_name in intersect(names(obs_units), names(observables))) {
        unit_value <- obs_units[[obs_name]]
        if (inherits(unit_value, "units")) {
            export_unit_value <- do.call(.to_dimensions, c(list(unit_value), dimensions))
            value_with_export_units <- units::set_units(
                observables[[obs_name]],
                .unit_label(export_unit_value),
                mode = "standard"
            )
            observables[[obs_name]] <- units::set_units(
                value_with_export_units,
                .unit_label(unit_value),
                mode = "standard"
            )
        }
    }

    observables
}

.simulation_observable_unit_values <- function(model, parameters = model$parameters) {
    if (inherits(model, "OdeModel") || inherits(model, "StochasticModel") || inherits(model, "AnalyticalModel")) {
        return(.ode_model_observable_unit_values(model, parameters))
    }
    if (length(model$observables) == 0) return(list())

    unit_env <- .simulation_unit_env(model)
    lapply(model$observables, function(obs_expr) {
        .dsl_eval(obs_expr, envir = unit_env)
    })
}

.simulation_unit_env <- function(model) {
    model <- model |> wire() |> make_depot()
    inits <- initials(model) |>
        .evaluate_initials(model$parameters, allow_unresolved = TRUE)
    values <- c(unclass(inits), unclass(model$parameters))
    env <- list2env(values)

    .simulation_add_derived_states(env, model)
    .simulation_add_equations(env, model)
    .simulation_add_derived_states(env, model)

    env
}

.simulation_add_derived_states <- function(env, model) {
    volume_by_cmt <- setNames(model$compartments$volume, names(model$compartments))

    for (i in seq_along(model$molecules)) {
        molec <- model$molecules$name[[i]]
        cmt <- model$molecules$cmt[[i]]
        vol <- .simulation_eval_volume(volume_by_cmt[[cmt]], env)
        if (is.null(vol)) next

        amount_nm <- .dsl_make_state(molec, cmt, prefix = "a")
        conc_nm <- .dsl_make_state(molec, cmt, prefix = "c")

        if (exists(amount_nm, envir = env, inherits = FALSE) &&
            !exists(conc_nm, envir = env, inherits = FALSE)) {
            assign(
                conc_nm,
                get(amount_nm, envir = env, inherits = FALSE) / vol,
                envir = env
            )
        }

        if (exists(conc_nm, envir = env, inherits = FALSE) &&
            !exists(amount_nm, envir = env, inherits = FALSE)) {
            assign(
                amount_nm,
                get(conc_nm, envir = env, inherits = FALSE) * vol,
                envir = env
            )
        }
    }

    invisible(env)
}

.simulation_eval_volume <- function(vol, env) {
    if (is.null(vol)) return(NULL)
    if (length(vol) == 1 && is.atomic(vol) && is.na(vol)) return(NULL)

    tryCatch(
        .dsl_eval(.as_call(vol), envir = env),
        error = function(e) NULL
    )
}

.simulation_add_equations <- function(env, model) {
    pending_eq <- seq_along(model$equations)
    while (length(pending_eq) > 0) {
        resolved <- logical(length(pending_eq))
        varnames <- names(env)
        for (k in seq_along(pending_eq)) {
            i <- pending_eq[[k]]
            eq_expr <- model$equations[[i]]
            if (!all(.dsl_all_vars(eq_expr) %in% varnames)) next

            assign(names(model$equations)[[i]], .dsl_eval(eq_expr, envir = env), envir = env)
            .simulation_add_derived_states(env, model)
            resolved[[k]] <- TRUE
        }
        if (!any(resolved)) break
        pending_eq <- pending_eq[!resolved]
    }

    invisible(env)
}

.simulation_state_unit_values <- function(model, parameters = model$parameters) {
    if (inherits(model, "OdeModel") || inherits(model, "StochasticModel") || inherits(model, "AnalyticalModel")) {
        return(.ode_model_state_unit_values(model, parameters))
    }
    model <- model |> wire() |> make_depot()
    initials(model) |>
        .evaluate_initials(model$parameters, allow_unresolved = TRUE)
}

.simulation_dimension_values <- function(model, parameters = model$parameters) {
    if (inherits(model, "OdeModel")) {
        return(.ode_model_dimension_values(model, parameters))
    }
    if (inherits(model, "AnalyticalModel")) {
        return(.analytical_model_dimension_values(model, parameters))
    }
    if (inherits(model, "StochasticModel")) {
        return(.stochastic_model_dimension_values(model, parameters))
    }
    model <- model |> wire() |> make_depot()
    inits <- initials(model) |>
        .evaluate_initials(model$parameters, allow_unresolved = TRUE)
    c(
        inits,
        as.list(model$compartments$volume),
        unclass(model$parameters),
        as.list(model$doses$time),
        as.list(model$doses$amount),
        as.list(model$doses$rate),
        as.list(model$doses$duration)
    )
}

.simulation_parameters_object <- function(parameters) {
    if (inherits(parameters, "Parameters")) {
        return(parameters)
    }
    if (!is.list(parameters)) {
        stop("Argument 'parameters' must be a named list or Parameters object.", call. = FALSE)
    }
    structure(parameters, class = c("Parameters", "list"))
}

.simulation_model_with_parameters <- function(model, parameters) {
    if (length(parameters) == 0) return(model)
    if (is.null(names(parameters)) || any(names(parameters) == "")) {
        stop("Argument 'parameters' must be named.", call. = FALSE)
    }

    for (nm in names(parameters)) {
        if (nm %in% names(model$parameters)) {
            model$parameters[nm] <- parameters[nm]
        } else {
            model$parameters <- c(model$parameters, parameters[nm])
        }
    }

    model
}

.simulation_solver_parameters <- function(parameters, dimensions) {
    if (inherits(parameters, "Parameters")) {
        parameters <- unclass(parameters)
    }
    if (!is.list(parameters)) {
        stop("Argument 'parameters' must be a named list or Parameters object.", call. = FALSE)
    }

    lapply(parameters, function(x) {
        if (inherits(x, "units")) {
            x <- do.call(.to_dimensions, c(list(x), dimensions))
            units::set_units(x, NULL)
        } else {
            x
        }
    })
}

.stochastic_simulation_nsim <- function(nsim) {
    if (is.null(nsim)) return(1L)
    if (is.numeric(nsim) && length(nsim) == 1L && is.finite(nsim) && nsim >= 1 && nsim == round(nsim)) {
        return(as.integer(nsim))
    }

    stop("Argument 'nsim' must be a positive integer scalar for SSA simulation.", call. = FALSE)
}

.simulation_include_event_times <- function(include_event_times) {
    if (is.logical(include_event_times) && length(include_event_times) == 1L && !is.na(include_event_times)) {
        return(include_event_times)
    }

    stop("Argument 'include_event_times' must be a logical scalar.", call. = FALSE)
}

.hybrid_simulation_partition <- function(partition, n_reactions, simulation_type) {
    if (!identical(simulation_type, "hybrid")) return(NULL)

    if (is.null(partition)) {
        stop("Argument 'partition' is required for hybrid simulation.", call. = FALSE)
    }
    if (is.numeric(partition) && length(partition) == 1L && is.finite(partition) && partition >= 0) {
        return(partition)
    }
    if (is.logical(partition) && length(partition) == n_reactions && !anyNA(partition)) {
        return(partition)
    }

    stop(
        "Argument 'partition' must be a non-negative numeric scalar or a logical vector with one value per reaction.",
        call. = FALSE
    )
}

.stochastic_model_initial_counts <- function(model, parameters, storage_mode = "integer") {
    initials <- .evaluate_initials(
        setNames(model$initials, model$states$dsl_name),
        parameters,
        allow_unresolved = FALSE
    )
    .stochastic_model_check_initials(initials, allow_unresolved = FALSE)
    counts <- unlist(initials)
    if (identical(storage_mode, "integer") && any(counts > .Machine$integer.max)) {
        state_name <- names(counts)[[which(counts > .Machine$integer.max)[[1L]]]]
        stop(
            "Initial value for stochastic state '",
            state_name,
            "' exceeds the maximum supported integer count for SSA simulation.",
            call. = FALSE
        )
    }
    counts <- unname(counts)
    storage.mode(counts) <- storage_mode
    counts
}

.ssa_simulate <- function(stoichiometry, propensity_function, time, y0, parameters, include_event_times = FALSE) {
    if (length(time) == 0) {
        return(matrix(NA_real_, nrow = 0L, ncol = length(y0) + 1L))
    }

    t <- time[[1]]
    y <- as.numeric(y0)
    rows <- list(c(t, y))

    for (i in seq_along(time)[-1L]) {
        target_time <- time[[i]]

        while (t < target_time) {
            a <- propensity_function(y, parameters)
            if (length(a) == 0 || sum(a) <= 0) break

            a0 <- sum(a)
            tau <- stats::rexp(1, rate = a0)
            if ((t + tau) <= target_time) {
                j <- sample(seq_along(a), size = 1, prob = a / a0)
                y <- y + stoichiometry[, j]
                t <- t + tau
                if (include_event_times) rows[[length(rows) + 1L]] <- c(t, y)
            } else {
                break
            }
        }

        rows[[length(rows) + 1L]] <- c(target_time, y)
    }

    do.call(rbind, rows)
}

.hybrid_simulate <- function(
    stoichiometry,
    propensity_function,
    time,
    y0,
    parameters,
    partition,
    include_event_times = FALSE,
    ...
) {
    if (length(time) == 0) {
        return(matrix(NA_real_, nrow = 0L, ncol = length(y0) + 1L))
    }

    nx <- length(y0)
    t <- time[[1]]
    tf <- time[[length(time)]]
    y <- as.numeric(y0)
    rows <- list(c(t, y))

    get_partitioning <- .hybrid_partition_function(partition, propensity_function, parameters)
    is_stochastic_reaction <- get_partitioning(y)

    solver_args <- list(...)
    solver_args$method <- solver_args$method %||% "lsodar"

    while (t < tf) {
        xi <- stats::rexp(1, rate = 1)

        ode_function <- function(t, Y, parms) {
            current_y <- Y[seq_len(nx)]
            a <- propensity_function(current_y, parms)
            a[a < 0] <- 0

            stochastic_hazard <- sum(a[is_stochastic_reaction])
            deterministic_propensity <- a * !is_stochastic_reaction
            dydt <- as.vector(stoichiometry %*% deterministic_propensity)

            list(c(dydt, stochastic_hazard))
        }
        root_function <- function(t, Y, parms) {
            Y[[nx + 1L]] - xi
        }

        integration_times <- c(t, time[time > t])
        solver_args$y <- c(y, stochastic_hazard = 0)
        solver_args$times <- integration_times
        solver_args$func <- ode_function
        solver_args$parms <- parameters
        solver_args$rootfunc <- root_function

        out <- do.call(deSolve::ode, solver_args)
        if (nrow(out) > 1L) {
            for (i in seq.int(2L, nrow(out))) {
                out_time <- out[i, "time"]
                out_y <- .hybrid_clamp_state(unname(out[i, 1L + seq_len(nx)]))
                if (include_event_times || out_time %in% time) {
                    rows[[length(rows) + 1L]] <- c(out_time, out_y)
                }
            }
        }

        t <- out[nrow(out), "time"]
        y <- .hybrid_clamp_state(unname(out[nrow(out), 1L + seq_len(nx)]))
        root_reached <- isTRUE(attr(out, "iroot") == 1) && t < tf
        if (!root_reached) break

        a <- propensity_function(y, parameters)
        a[a < 0] <- 0
        a[!is_stochastic_reaction] <- 0
        a0 <- sum(a)
        if (a0 <= 0) {
            is_stochastic_reaction <- get_partitioning(y)
            next
        }

        j <- sample(seq_along(a), size = 1, prob = a / a0)
        y <- .hybrid_clamp_state(y + stoichiometry[, j])
        if (include_event_times) rows[[length(rows) + 1L]] <- c(t, y)
        is_stochastic_reaction <- get_partitioning(y)
    }

    do.call(rbind, rows)
}

.hybrid_partition_function <- function(partition, propensity_function, parameters) {
    if (is.numeric(partition)) {
        threshold <- partition
        return(function(y) propensity_function(y, parameters) < threshold)
    }

    force(partition)
    function(y) partition
}

.hybrid_clamp_state <- function(y, tolerance = sqrt(.Machine$double.eps)) {
    y[y < 0 & y > -tolerance] <- 0
    y
}

.ssa_falling <- function(x, n) {
    n <- as.integer(n)
    if (n <= 0L) return(1)
    if (x < n) return(0)
    prod(seq(from = x - n + 1, to = x))
}

.stochastic_model_propensity_function <- function(model, parameters, dimensions) {
    param_values <- .to_dimensions_vec(parameters, dimensions)
    free_params <- new.env(parent = emptyenv())
    free_params$list <- character()
    eq_names <- names(model$equations)

    subst <- function(expr) {
        .ode_model_substitute_parameters(expr, eq_names, param_values, free_params, dimensions)
    }

    lines <- "function(y, params) {"
    for (i in seq_along(model$equations)) {
        lines <- c(lines, paste0("    ", names(model$equations)[[i]], " <- ", deparse1(subst(model$equations[[i]]))))
        if (i == length(model$equations)) lines <- c(lines, "")
    }
    lines <- c(lines, paste0("    prop <- numeric(", length(model$propensities), ")"))
    for (i in seq_along(model$propensities)) {
        lines <- c(lines, paste0("    prop[", i, "] <- ", deparse1(subst(model$propensities[[i]]))))
    }
    lines <- c(lines, "    .ssa_validate_propensities(prop)", "}")

    eval(parse(text = paste(lines, collapse = "\n")))
}

.ssa_validate_propensities <- function(prop) {
    if (!is.numeric(prop) || anyNA(prop) || any(!is.finite(prop))) {
        stop("SSA propensity evaluated to a missing, non-finite, or non-numeric value.", call. = FALSE)
    }
    if (any(prop < 0)) {
        stop("SSA propensity evaluated to a negative value.", call. = FALSE)
    }
    unname(prop)
}

.stochastic_simulation_observables <- function(states, solver_time, model, dimensions, parameters) {
    if (length(model$observables) == 0) return(NULL)

    solver_output <- cbind(
        time = solver_time,
        as.matrix(states[, model$states$output_name, drop = FALSE])
    )
    storage.mode(solver_output) <- "numeric"

    output_state_names <- model$states$output_name
    param_values <- .to_dimensions_vec(parameters, dimensions)
    free_params <- new.env(parent = emptyenv())
    free_params$list <- character()
    eq_names <- names(model$equations)

    obs_funcs <- lapply(model$observables, function(obs) {
        expr <- .ode_model_substitute_parameters(obs, eq_names, param_values, free_params, dimensions)
        expr <- .ode_model_observable_backend_expr(expr, output_state_names)
        eval(parse(text = paste0("function(t,y,params) unname(", deparse1(expr), ")")))
    })
    names(obs_funcs) <- names(model$observables)

    odeinfo <- list(
        obsFuncs = obs_funcs,
        stateNames = output_state_names,
        dslStateNames = model$states$dsl_name
    )
    .simulation_observables(
        solver_output,
        states$time,
        model,
        odeinfo,
        solver_time,
        dimensions,
        parameters = parameters
    )
}

.stochastic_simulation_bind_observables <- function(observables) {
    observables <- Filter(Negate(is.null), observables)
    if (length(observables) == 0) return(NULL)

    out <- do.call(rbind, observables)
    rownames(out) <- NULL
    out
}

.stochastic_model_dimension_values <- function(model, parameters) {
    y0 <- tryCatch(
        .ode_model_state_unit_values(model, parameters, allow_unresolved = TRUE),
        error = function(e) list()
    )
    propensities <- tryCatch(
        .stochastic_raw_propensity_values(model, y0, parameters),
        error = function(e) list()
    )

    c(y0, unclass(parameters), propensities)
}

.stochastic_raw_propensity_values <- function(model, y0, parameters) {
    if (length(y0) != nrow(model$states)) return(list())
    if (any(vapply(y0, .initial_is_expr, logical(1)))) return(list())

    env <- list2env(unclass(parameters), parent = baseenv())
    env$y <- unname(unlist(y0))
    env$.ssa_falling <- .ssa_falling
    .ode_model_add_equations(model, env, y0)

    lapply(model$propensities, function(expr) eval(expr, envir = env))
}

.infer_dimensions_from_unit <- function(x, dimensions) {
    if (!inherits(x, "units")) return(dimensions)

    x <- .expand_registered_model_units(x)
    x_base <- .convert_to_si_base_without_custom(x)
    unit_obj <- units(x_base)
    si_units <- c("m", "kg", "s", "mol", "A", "K", "cd")
    dimension_names <- c("length", "mass", "time", "amount", "current", "temperature", "intensity")
    original_unit <- units(x)

    for (i in seq_along(si_units)) {
        if (!is.null(dimensions[[dimension_names[[i]]]])) next

        in_base <- si_units[[i]] %in% c(unit_obj$numerator, unit_obj$denominator)
        if (!in_base) next

        original_matches <- c(original_unit$numerator, original_unit$denominator)
        original_matches <- original_matches[
            vapply(original_matches, function(u) units::ud_are_convertible(u, si_units[[i]]), logical(1))
        ]
        if (length(original_matches) > 0) {
            dimensions[[dimension_names[[i]]]] <- original_matches[[1]]
        }
    }

    dimensions
}

.unit_label <- function(x) {
    as.character(units(x))
}

.has_time_dimension <- function(x) {
    if (!inherits(x, "units")) return(FALSE)
    x <- .expand_registered_model_units(x)
    unit_obj <- units(.convert_to_si_base_without_custom(x))
    "s" %in% c(unit_obj$numerator, unit_obj$denominator)
}
