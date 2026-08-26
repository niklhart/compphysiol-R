#' Simulate a compartment model
#'
#' `simulate()` solves a `CompartmentModel` and returns toolbox-level output.
#' The current implementation wraps the `deSolve` ODE export and returns ODE
#' states and observables.
#'
#' Simulation is either unit-free or unit-aware with respect to time. If the
#' model uses a time dimension, `time` must carry units through the unit DSL or
#' the `unit` argument. Conversely, unit-aware simulation times require a model
#' with a time dimension.
#'
#' In unit-aware simulations, `dimensions` defines the solver-facing numerical
#' scale used when unit-bearing model quantities are converted to plain numbers
#' before calling [deSolve::ode()]. This scale also determines how solver
#' tolerances such as `atol` are interpreted. For example, if amounts are
#' converted to kilograms internally, an `atol` value is applied on the kilogram
#' scale even if results are displayed back in milligrams.
#'
#' @param object A `CompartmentModel` object.
#' @param nsim Ignored; included for compatibility with [stats::simulate()].
#' @param seed Ignored; included for compatibility with [stats::simulate()].
#' @param time Simulation time points. Units can be supplied with the unit DSL,
#'   e.g. `seq(0, 24) [h]`.
#' @param unit Optional time unit used when `time` is numeric without units.
#' @param parameters Free parameters passed to the ODE solver, as a named list
#'   or `Parameters` object.
#' @param dimensions Named list of unit dimensions defining the numerical scale
#'   used at the ODE solver boundary. These dimensions are passed to [to_ode()]
#'   and affect the scale of solver tolerances such as `atol`.
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
    .simulation_validate_time(time)

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
    observables <- .simulation_observables(out, states$time, export_model, odeinfo, solver_time, dimensions)

    structure(
        list(
            states = states,
            observables = observables
        ),
        class = "SimulationResult"
    )
}

#' Print a simulation result
#'
#' @param x A `SimulationResult` object.
#' @param ... Additional arguments, currently ignored.
#' @returns The `SimulationResult` object, invisibly.
#' @export
print.SimulationResult <- function(x, ...) {
    .check_class(x, "SimulationResult")

    n_time <- nrow(x$states)
    n_states <- max(ncol(x$states) - 1, 0)
    n_observables <- if (is.null(x$observables)) 0 else max(ncol(x$observables) - 1, 0)
    time_span <- if (n_time > 0) {
        sprintf("%s to %s", format(x$states$time[[1]]), format(x$states$time[[n_time]]))
    } else {
        "empty"
    }

    cat(" SimulationResult:\n")
    cat(sprintf("  time: %s (%s points)\n", time_span, n_time))
    cat(sprintf("  states: %s", n_states))
    if (n_states > 0) {
        cat(sprintf(" (%s)", .simulation_format_names(setdiff(names(x$states), "time"), prefix = "  states: ")))
    }
    cat("\n")
    cat(sprintf("  observables: %s", n_observables))
    if (n_observables > 0) {
        cat(sprintf(" (%s)", .simulation_format_names(setdiff(names(x$observables), "time"), prefix = "  observables: ")))
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

.simulation_observables <- function(solver_output, time, model, odeinfo, solver_time, dimensions) {
    if (length(odeinfo$obsFuncs) == 0) return(NULL)

    values <- lapply(odeinfo$obsFuncs, function(f) {
        f(solver_time, solver_output, list())
    })
    observables <- as.data.frame(values)
    names(observables) <- names(odeinfo$obsFuncs)
    observables <- cbind(data.frame(time = time), observables)

    obs_units <- .simulation_observable_unit_values(model)
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

.simulation_observable_unit_values <- function(model) {
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

.simulation_state_unit_values <- function(model) {
    model <- model |> wire() |> make_depot()
    initials(model) |>
        .evaluate_initials(model$parameters, allow_unresolved = TRUE)
}

.simulation_dimension_values <- function(model) {
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
