.simulate_experiments <- function(object, experiment, dimensions = NULL, engine = NULL, ...) {
    multiple <- inherits(experiment, "compphysiol_experiments")
    homogeneity <- if (multiple) {
        .experiments_component_homogeneity(experiment)
    } else {
        stats::setNames(rep(TRUE, length(.experiments_component_names())),
                        .experiments_component_names())
    }
    experiments <- if (multiple) .experiments_elements(experiment) else {
        .check_class(experiment, "compphysiol_experiment")
        list(experiment)
    }
    validation_indices <- if (!length(experiments)) {
        integer()
    } else if (homogeneity[["observation_plan"]]) {
        1L
    } else {
        seq_along(experiments)
    }
    for (i in validation_indices) {
        e <- experiments[[i]]
        if (!nrow(e$observations)) stop("Experiment simulation requires nonempty observations.", call. = FALSE)
        model <- if (inherits(object, c("compphysiol_desolve_compiled_model", "compphysiol_rxode2_model"))) object$ode_model else object
        unknown <- setdiff(e$observations$observable, names(model$observables))
        if (length(unknown)) stop("Unknown observable(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    prepared_doses <- NULL
    if (inherits(object, "compphysiol_compartment_model") && length(experiments)) {
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
    if (multiple && inherits(engine, "compphysiol_rxode2_engine")) {
        executable <- if (inherits(object, "compphysiol_rxode2_model")) object else to_rxode2_model(object)
        return(.simulate_rxode2_experiments(
            executable, experiments, dimensions = dimensions, engine = engine,
            homogeneity = homogeneity, ...
        ))
    }
    shared_time <- NULL
    if (length(experiments) && homogeneity[["start"]] && homogeneity[["observation_plan"]]) {
        shared_time <- .simulation_experiment_time(experiments[[1]])
    }
    shared_events <- NULL
    shared_observation_plan <- NULL
    if (multiple && inherits(object, "compphysiol_desolve_compiled_model")) {
        artifact <- .compiled_ode_model_cached_artifact(object)
        if (!is.null(artifact)) {
            if (homogeneity[["dosing"]]) {
                shared_events <- .simulation_experiment_events(
                    object$ode_model, experiments[[1]]$dosing, artifact$dimensions,
                    experiments[[1]]$parameters,
                    state_unit_values = artifact$stateUnitValues
                )
            }
            if (!is.null(shared_time)) {
                shared_observation_plan <- .simulation_prepare_observation_plan(
                    experiments[[1]]$observations,
                    .simulation_numeric_time(shared_time, artifact$dimensions),
                    artifact$dimensions
                )
            }
        }
    }
    out <- lapply(seq_along(experiments), function(i) {
        e <- experiments[[i]]
        model <- object
        if (inherits(model, "compphysiol_compartment_model")) {
            model$doses <- prepared_doses[[i]]
        } else {
            attr(model, "experiment_dosing") <- e$dosing
            attr(model, "prepared_experiment_events") <- shared_events
        }
        attr(model, "observation_schedule") <- e$observations
        attr(model, "prepared_observation_plan") <- shared_observation_plan
        time <- shared_time %||% .simulation_experiment_time(e)
        if (is.null(engine)) {
            simulate(model, time = time, parameters = e$parameters, dimensions = dimensions, ...)
        } else {
            simulate(model, time = time, parameters = e$parameters, dimensions = dimensions, engine = engine, ...)
        }
    })
    names(out) <- names(experiments)
    if (multiple) out else out[[1]]
}

.simulation_experiment_time <- function(experiment) {
    start <- experiment$start
    if (inherits(experiment$observations$time, "units")) {
        start <- units::set_units(
            start, units::deparse_unit(experiment$observations$time), mode = "standard"
        )
    }
    sort(unique(c(start, experiment$observations$time)))
}

.simulation_experiment_events <- function(
    model, dosing, dimensions, parameters, state_unit_values = NULL
) {
    states <- lapply(model$states$dsl_name, .dsl_parse_state)
    state_molec <- vapply(states, `[[`, character(1), "molec")
    state_cmt <- vapply(states, `[[`, character(1), "cmt")
    biological <- !grepl("^(Depot|ReleaseRate)_", state_cmt)
    if (is.null(state_unit_values)) {
        parameters <- .merge_ode_parameters(model$parameters, parameters)
        state_units <- .ode_model_state_unit_values(model, parameters, allow_unresolved = FALSE)
    } else {
        state_units <- state_unit_values[model$states$output_name]
    }
    events <- list()
    append_event <- function(molec, cmt, time, value) {
        dsl <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "a")
        idx <- match(dsl, model$states$dsl_name)
        if (is.na(idx)) stop("Dosing target is absent from this ODE model: ", dsl,
                            ". Prepare a compartment model with all required dosing targets first.", call. = FALSE)
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
