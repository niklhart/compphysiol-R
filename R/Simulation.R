#' Simulate a compartment model
#'
#' `simulate()` solves a `CompartmentModel` and returns toolbox-level output.
#' The current implementation wraps the `deSolve` ODE export and returns ODE
#' states only; observables will be added separately.
#'
#' Simulation is either unit-free or unit-aware with respect to time. If the
#' model uses a time dimension, `time` must carry units through the unit DSL or
#' the `unit` argument. Conversely, unit-aware simulation times require a model
#' with a time dimension.
#'
#' @param object A `CompartmentModel` object.
#' @param nsim Ignored; included for compatibility with [stats::simulate()].
#' @param seed Ignored; included for compatibility with [stats::simulate()].
#' @param time Simulation time points. Units can be supplied with the unit DSL,
#'   e.g. `seq(0, 24) [h]`.
#' @param unit Optional time unit used when `time` is numeric without units.
#' @param parameters Free parameters passed to the ODE solver, as a named list
#'   or `Parameters` object.
#' @param dimensions Named list of unit dimensions passed to [to_ode()].
#' @param ... Additional arguments passed to [deSolve::ode()].
#' @returns A `SimulationResult` object.
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
    ...
) {
    .check_class(object, "CompartmentModel")

    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    if (!is.null(unit)) {
        if (inherits(time, "units")) {
            stop("Argument 'unit' can only be used when 'time' does not already have units.", call. = FALSE)
        }
        time <- units::set_units(time, unit, mode = "standard")
    }

    sim_parameters <- .simulation_parameters_object(parameters)
    export_model <- .simulation_model_with_parameters(object, sim_parameters)
    .simulation_check_time_mode(export_model, time)

    dimensions <- .simulation_dimensions(export_model, time, dimensions)
    odeinfo <- to_ode(export_model, dimensions = dimensions)
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

    states <- as.data.frame(out)
    states <- .simulation_attach_state_units(states, export_model, odeinfo, dimensions)
    states$time <- .simulation_attach_time_units(states$time, time, dimensions)

    structure(
        list(states = states),
        class = "SimulationResult"
    )
}

.simulation_dimensions <- function(model, time, dimensions) {
    dimensions <- dimensions %||% list()

    if (inherits(time, "units") && is.null(dimensions$time)) {
        dimensions$time <- .unit_label(time)
    }

    for (value in .simulation_dimension_values(model)) {
        dimensions <- .infer_dimensions_from_unit(value, dimensions)
    }

    dimensions
}

.simulation_check_time_mode <- function(model, time) {
    model_uses_time <- any(vapply(.simulation_dimension_values(model), .has_time_dimension, logical(1)))
    time_has_units <- inherits(time, "units")

    if (model_uses_time && !time_has_units) {
        stop("Cannot simulate: model uses time units but simulation time is unit-free.", call. = FALSE)
    }
    if (!model_uses_time && time_has_units) {
        stop("Cannot simulate: simulation time has units but the model is unit-free in time.", call. = FALSE)
    }

    invisible(NULL)
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

.simulation_attach_state_units <- function(states, model, odeinfo, dimensions) {
    state_units <- .simulation_state_unit_values(model)
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

.simulation_state_unit_values <- function(model) {
    model <- model |> wire() |> make_depot()
    initials(model)
}

.simulation_dimension_values <- function(model) {
    model <- model |> wire() |> make_depot()
    c(
        initials(model),
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

.infer_dimensions_from_unit <- function(x, dimensions) {
    if (!inherits(x, "units")) return(dimensions)

    x_base <- units::convert_to_base(x)
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
    unit_obj <- units(units::convert_to_base(x))
    "s" %in% c(unit_obj$numerator, unit_obj$denominator)
}
