.simulate_experiments <- function(object, experiment, dimensions = NULL, ...) {
    multiple <- inherits(experiment, "Experiments")
    experiments <- if (multiple) unclass(.new_experiments(unclass(experiment))) else {
        .check_class(experiment, "Experiment")
        list(experiment)
    }
    for (e in experiments) {
        validate_experiment(e)
        if (!nrow(e$schedule)) stop("Experiment simulation requires a nonempty observation schedule.", call. = FALSE)
        model <- if (inherits(object, "CompiledOdeModel")) object$ode_model else object
        unknown <- setdiff(e$schedule$observable, names(model$observables))
        if (length(unknown)) stop("Unknown observable(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    prepared_doses <- NULL
    if (inherits(object, "CompartmentModel") && length(experiments)) {
        # Wire each protocol before adding auxiliaries, then prepare their union.
        prepared <- lapply(experiments, function(e) {
            m <- object
            m$doses <- e$dosing
            wire(m)
        })
        prepared_doses <- lapply(prepared, function(m) make_depot(m)$doses)
        template <- prepared[[1]]
        template$doses <- do.call(c, lapply(prepared, `[[`, "doses"))
        object <- make_depot(template)
    }
    out <- lapply(seq_along(experiments), function(i) {
        e <- experiments[[i]]
        model <- object
        if (inherits(model, "CompartmentModel")) {
            model$doses <- prepared_doses[[i]]
        } else {
            attr(model, "experiment_dosing") <- e$dosing
        }
        attr(model, "observation_schedule") <- e$schedule
        # Use observation time units for the state grid, including the initial time.
        start <- e$start
        if (inherits(e$schedule$time, "units")) {
            start <- units::set_units(start, units::deparse_unit(e$schedule$time), mode = "standard")
        }
        time <- sort(unique(c(start, e$schedule$time)))
        simulate(model, time = time, parameters = e$parameters, dimensions = dimensions, ...)
    })
    names(out) <- names(experiments)
    if (multiple) out else out[[1]]
}

.simulation_experiment_events <- function(model, dosing, dimensions, parameters) {
    states <- lapply(model$states$dsl_name, .dsl_parse_state)
    state_molec <- vapply(states, `[[`, character(1), "molec")
    state_cmt <- vapply(states, `[[`, character(1), "cmt")
    biological <- !grepl("^(Depot|ReleaseRate)_", state_cmt)
    parameters <- .merge_ode_parameters(model$parameters, parameters)
    state_units <- .ode_model_state_unit_values(model, parameters, allow_unresolved = FALSE)
    events <- list()
    append_event <- function(molec, cmt, time, value) {
        dsl <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "a")
        idx <- match(dsl, model$states$dsl_name)
        if (is.na(idx)) stop("Dosing target is absent from this ODE model: ", dsl,
                            ". Prepare a CompartmentModel with all required dosing targets first.", call. = FALSE)
        .ode_model_check_same_units(state_units[[idx]], value, dsl, what = "dosing event")
        events[[length(events) + 1L]] <<- data.frame(
            var = model$states$output_name[idx],
            time = as.numeric(.to_dimensions_value(time, dimensions)),
            value = as.numeric(.to_dimensions_value(value, dimensions)), method = "add")
    }
    for (i in seq_along(dosing$time)) {
        molec <- dosing$molec[i]
        cmt <- dosing$cmt[i]
        if (is.na(molec)) {
            candidates <- unique(state_molec[biological & (is.na(cmt) | state_cmt == cmt)])
            if (length(candidates) != 1L) stop("Specify the dosing molecule for this ODE model.", call. = FALSE)
            molec <- candidates
        }
        if (is.na(cmt)) {
            candidates <- unique(state_cmt[biological & state_molec == molec])
            if (length(candidates) != 1L) stop("Specify the dosing compartment for this ODE model.", call. = FALSE)
            cmt <- candidates
        }
        if (is_bolus(dosing)[i]) {
            append_event(molec, cmt, dosing$time[i], dosing$amount[[i]])
        } else {
            bag <- paste("Depot", molec, cmt, sep = "_")
            rate <- paste("ReleaseRate", molec, cmt, sep = "_")
            append_event(molec, bag, dosing$time[i], dosing$rate[[i]] * dosing$duration[i])
            append_event(molec, rate, dosing$time[i], dosing$rate[[i]])
            append_event(molec, rate, dosing$time[i] + dosing$duration[i], -dosing$rate[[i]])
        }
    }
    data <- if (length(events)) do.call(rbind, events) else {
        data.frame(var = character(), time = numeric(), value = numeric(), method = character())
    }
    list(data = data)
}
