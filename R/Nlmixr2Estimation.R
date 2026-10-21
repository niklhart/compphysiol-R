.nlmixr2_individual_targets <- function(statistics) {
    names(statistics)[vapply(statistics, function(x) {
        identical(x$level, "individual")
    }, logical(1))]
}

.nlmixr2_observation_targets <- function(statistics) {
    names(statistics)[vapply(statistics, function(x) {
        identical(x$level, "observation")
    }, logical(1))]
}

.nlmixr2_scale_name <- function(distribution) {
    if (inherits(distribution, "compphysiol_normal_distribution")) {
        distribution$sd
    } else {
        distribution$sdlog
    }
}

.nlmixr2_number <- function(x) {
    format(as.numeric(x), digits = 17L, scientific = TRUE, trim = TRUE)
}

.nlmixr2_parameter_id <- function(executable, name, fallback = TRUE) {
    value <- unname(executable$identifiers$parameters[name])
    if (length(value) && !is.na(value)) return(value)
    if (fallback) paste0("cp_stat_", gsub("[^A-Za-z0-9_]", "_", name)) else NULL
}

.nlmixr2_solver_value <- function(value, dimensions) {
    if (inherits(value, "units")) as.numeric(.to_dimensions_value(value, dimensions)) else
        as.numeric(value)
}

.nlmixr2_log_likelihood_unit_jacobian <- function(problem, dimensions) {
    sum(vapply(.experiments_elements(problem$experiments), function(experiment) {
        observed <- experiment$observations$value
        sum(vapply(seq_along(observed), function(i) {
            value <- observed[[i]]
            if (is.na(value) || !inherits(value, "units")) return(0)
            solver_value <- do.call(.to_dimensions, c(list(value), dimensions))
            scale <- as.numeric(units::set_units(
                units::set_units(
                    1, units::deparse_unit(solver_value), mode = "standard"
                ),
                units::deparse_unit(value), mode = "standard"
            ))
            log(scale)
        }, numeric(1)))
    }, numeric(1)))
}

.nlmixr2_theta_bounds <- function(spec, dimensions = list()) {
    c(lower = .nlmixr2_solver_value(spec$lower, dimensions),
      initial = .nlmixr2_solver_value(spec$initial, dimensions),
      upper = .nlmixr2_solver_value(spec$upper, dimensions))
}

.nlmixr2_transform_value <- function(value, spec, dimensions = list()) {
    bounds <- .nlmixr2_theta_bounds(spec, dimensions)
    switch(spec$transform,
        identity = value,
        log = log(value),
        logit = stats::qlogis((value - bounds[["lower"]]) /
                              (bounds[["upper"]] - bounds[["lower"]]))
    )
}

.nlmixr2_theta_spec <- function(name, spec, dimensions = list()) {
    bounds <- .nlmixr2_theta_bounds(spec, dimensions)
    transformed <- vapply(bounds, .nlmixr2_transform_value, numeric(1),
                          spec = spec, dimensions = dimensions)
    paste0(name, " <- c(", paste(vapply(transformed, .nlmixr2_number, character(1)),
                                  collapse = ", "), ")")
}

.nlmixr2_restore_value <- function(value, spec, dimensions) {
    if (!inherits(spec$initial, "units")) return(value)
    solver_template <- do.call(.to_dimensions, c(list(spec$initial), dimensions))
    solver_value <- units::set_units(
        value, units::deparse_unit(solver_template), mode = "standard"
    )
    units::set_units(
        solver_value, units::deparse_unit(spec$initial), mode = "standard"
    )
}

.nlmixr2_theta_decode <- function(value, spec, dimensions = list()) {
    bounds <- .nlmixr2_theta_bounds(spec, dimensions)
    decoded <- switch(spec$transform,
        identity = value,
        log = exp(value),
        logit = bounds[["lower"]] + (bounds[["upper"]] - bounds[["lower"]]) *
            stats::plogis(value)
    )
    .nlmixr2_restore_value(decoded, spec, dimensions)
}

.nlmixr2_public_unit_scale <- function(spec, dimensions) {
    if (!inherits(spec$initial, "units")) return(1)
    as.numeric(.nlmixr2_restore_value(1, spec, dimensions))
}

.nlmixr2_uncertainty <- function(problem, raw, translated, coefficients) {
    covariance <- tryCatch(stats::vcov(raw), error = function(e) NULL)
    .nlmixr2_uncertainty_from_covariance(
        problem, covariance, raw$theta, raw$omega, translated, coefficients
    )
}

.nlmixr2_uncertainty_from_covariance <- function(
    problem, covariance, theta, omega, translated, coefficients
) {
    specs <- problem$parameters
    if (is.null(covariance) || !is.matrix(covariance) ||
        is.null(rownames(covariance)) || is.null(colnames(covariance))) {
        return(.estimation_public_uncertainty(NULL, numeric(), specs))
    }
    backend_names <- character()
    derivatives <- numeric()
    for (name in names(specs)) {
        omega_target <- names(translated$omega_parameters)[
            translated$omega_parameters == name
        ]
        spec <- specs[[name]]
        scale <- .nlmixr2_public_unit_scale(spec, translated$dimensions)
        if (length(omega_target)) {
            eta <- translated$eta_ids[[omega_target[[1]]]]
            backend <- paste0("om.", eta)
            variance <- as.numeric(omega[eta, eta])
            derivative <- if (is.finite(variance) && variance > 0) {
                scale / (2 * sqrt(variance))
            } else NA_real_
        } else {
            backend <- translated$theta_ids[[name]]
            coordinate <- unname(theta[[backend]])
            derivative <- switch(spec$transform,
                identity = scale,
                log = as.numeric(coefficients[[name]]),
                logit = {
                    bounds <- .nlmixr2_theta_bounds(spec, translated$dimensions)
                    probability <- stats::plogis(coordinate)
                    scale * (bounds[["upper"]] - bounds[["lower"]]) *
                        probability * (1 - probability)
                }
            )
        }
        backend_names[[name]] <- backend
        derivatives[[name]] <- derivative
    }
    available <- names(backend_names)[backend_names %in% rownames(covariance)]
    available <- available[backend_names[available] %in% colnames(covariance)]
    if (!length(available)) {
        return(.estimation_public_uncertainty(NULL, derivatives, specs))
    }
    covariance <- covariance[
        backend_names[available], backend_names[available], drop = FALSE
    ]
    dimnames(covariance) <- list(available, available)
    .estimation_public_uncertainty(covariance, derivatives, specs)
}

.nlmixr2_validate_scope <- function(problem) {
    if (length(.statistical_correlations(problem$statistics))) {
        stop("The nlmixr2 engine currently supports diagonal individual variability only; correlations are not yet supported.",
             call. = FALSE)
    }
    individual <- .nlmixr2_individual_targets(problem$statistics)
    for (target in individual) {
        distribution <- problem$statistics[[target]]
        scale <- .nlmixr2_scale_name(distribution)
        if (!is.character(scale)) {
            stop("The first nlmixr2 implementation requires an estimated SD or sdlog parameter for individual target '",
                 target, "'.", call. = FALSE)
        }
        if (inherits(scale, c("compphysiol_proportional_sd", "compphysiol_combined_sd"))) {
            stop("Individual-level normal distributions require a constant SD in the nlmixr2 engine.",
                 call. = FALSE)
        }
    }
    for (target in .nlmixr2_observation_targets(problem$statistics)) {
        distribution <- problem$statistics[[target]]
        location <- if (inherits(distribution, "compphysiol_normal_distribution")) {
            distribution$mean
        } else distribution$median
        if (!inherits(location, "compphysiol_prediction_location")) {
            stop("The nlmixr2 engine currently requires observation distributions to use the model prediction as their location.",
                 call. = FALSE)
        }
        if (inherits(distribution, "compphysiol_normal_distribution") &&
            inherits(distribution$sd, "compphysiol_combined_sd")) {
            stop("Combined residual SD is not yet supported by the nlmixr2 engine.",
                 call. = FALSE)
        }
    }
    invisible(NULL)
}

.nlmixr2_prepare <- function(problem, dimensions = NULL) {
    if (!requireNamespace("nlmixr2est", quietly = TRUE)) {
        stop("Package 'nlmixr2est' is required for the nlmixr2 estimation engine.",
             call. = FALSE)
    }
    .nlmixr2_validate_scope(problem)
    model <- problem$model
    if (inherits(model, "compphysiol_desolve_compiled_model")) model <- model$ode_model
    executable <- if (inherits(model, "compphysiol_rxode2_model")) model else to_rxode2_model(model)
    ode <- executable$ode_model
    experiments <- .experiments_elements(problem$experiments)
    initial_parameters <- .new_parameters(lapply(problem$parameters, `[[`, "initial"))
    supplied <- .merge_ode_parameters(experiments[[1]]$parameters, initial_parameters)
    typical <- lapply(.nlmixr2_individual_targets(problem$statistics), function(target) {
        distribution <- problem$statistics[[target]]
        location <- if (inherits(distribution, "compphysiol_normal_distribution")) {
            distribution$mean
        } else distribution$median
        .estimation_spec_value(location, supplied, "individual location")
    })
    names(typical) <- .nlmixr2_individual_targets(problem$statistics)
    supplied <- .new_parameters(c(unclass(supplied), typical))
    merged <- .merge_ode_parameters(ode$parameters, supplied)
    time <- .simulation_experiment_time(experiments[[1]])
    .simulation_check_time_mode(ode, time, parameters = merged)
    dimensions <- .simulation_dimensions(ode, time, dimensions, parameters = merged)
    artifact <- .rxode2_artifact(executable, dimensions, parameters = merged)

    individual <- .nlmixr2_individual_targets(problem$statistics)
    omega_parameters <- vapply(individual, function(target) {
        .nlmixr2_scale_name(problem$statistics[[target]])
    }, character(1))
    theta_parameters <- setdiff(names(problem$parameters), omega_parameters)
    theta_ids <- stats::setNames(paste0("cp_fix_", seq_along(theta_parameters)), theta_parameters)
    eta_ids <- stats::setNames(paste0("cp_eta_", seq_along(individual)), individual)

    ini <- vapply(theta_parameters, function(name) {
        .nlmixr2_theta_spec(theta_ids[[name]], problem$parameters[[name]], dimensions)
    }, character(1))
    omega <- vapply(individual, function(target) {
        parameter <- omega_parameters[[target]]
        initial <- problem$parameters[[parameter]]$initial
        variance <- .nlmixr2_solver_value(initial, dimensions)^2
        paste0(eta_ids[[target]], " ~ ", .nlmixr2_number(variance))
    }, character(1))

    decode_lines <- vapply(theta_parameters, function(name) {
        spec <- problem$parameters[[name]]
        id <- theta_ids[[name]]
        actual <- .nlmixr2_parameter_id(executable, name)
        rhs <- switch(spec$transform,
            identity = id,
            log = paste0("exp(", id, ")"),
            logit = {
                bounds <- .nlmixr2_theta_bounds(spec, dimensions)
                paste0(.nlmixr2_number(bounds[["lower"]]), " + (",
                       .nlmixr2_number(bounds[["upper"]] - bounds[["lower"]]),
                       ") * expit(", id, ")")
            }
        )
        paste0(actual, " = ", rhs, ";")
    }, character(1))

    individual_lines <- vapply(individual, function(target) {
        distribution <- problem$statistics[[target]]
        location <- if (inherits(distribution, "compphysiol_normal_distribution")) {
            distribution$mean
        } else distribution$median
        location_expr <- if (is.character(location)) {
            .nlmixr2_parameter_id(executable, location)
        } else {
            .nlmixr2_number(.to_dimensions_value(location, dimensions))
        }
        target_id <- executable$identifiers$parameters[[target]]
        rhs <- if (inherits(distribution, "compphysiol_lognormal_distribution")) {
            paste0(location_expr, " * exp(", eta_ids[[target]], ")")
        } else paste0(location_expr, " + ", eta_ids[[target]])
        paste0(target_id, " = ", rhs, ";")
    }, character(1))

    observation <- .nlmixr2_observation_targets(problem$statistics)
    observation_lines <- unlist(lapply(observation, function(target) {
        distribution <- problem$statistics[[target]]
        prediction <- executable$identifiers$observables[[target]]
        expression <- .rxode2_render_expr(
            ode$observables[[target]], executable, dimensions,
            paste0("observable '", target, "'")
        )
        scale <- .nlmixr2_scale_name(distribution)
        scale_expr <- if (inherits(scale, "compphysiol_proportional_sd")) {
            coefficient <- scale$coefficient
            value <- if (is.character(coefficient)) {
                .nlmixr2_parameter_id(executable, coefficient)
            } else .nlmixr2_number(coefficient)
            paste0("prop(", value, ")")
        } else {
            value <- if (is.character(scale)) {
                .nlmixr2_parameter_id(executable, scale)
            } else .nlmixr2_number(.to_dimensions_value(scale, dimensions))
            if (inherits(distribution, "compphysiol_lognormal_distribution")) {
                paste0("lnorm(", value, ")")
            } else paste0("add(", value, ")")
        }
        c(paste0(prediction, " = ", expression, ";"),
          paste0(prediction, " ~ ", scale_expr, ";"))
    }), use.names = FALSE)

    source <- paste0(
        "function() {\nini({\n", paste(c(ini, omega), collapse = "\n"),
        "\n})\nmodel({\n", paste(c(decode_lines, individual_lines,
                                      strsplit(.rxode2_source(executable, dimensions), "\n", fixed = TRUE)[[1]],
                                      observation_lines), collapse = "\n"),
        "\n})\n}"
    )
    model_function <- eval(parse(text = source), envir = parent.frame())
    data <- .nlmixr2_data(problem, executable, artifact, dimensions,
                          observation, theta_parameters, individual)
    list(model = model_function, source = source, data = data,
         executable = executable, artifact = artifact, dimensions = dimensions,
         theta_ids = theta_ids, eta_ids = eta_ids,
         omega_parameters = omega_parameters, observation = observation,
         log_likelihood_unit_jacobian =
             .nlmixr2_log_likelihood_unit_jacobian(problem, dimensions))
}

.nlmixr2_data <- function(problem, executable, artifact, dimensions,
                          observation, theta_parameters, individual) {
    experiments <- .experiments_elements(problem$experiments)
    rows <- lapply(seq_along(experiments), function(i) {
        experiment <- experiments[[i]]
        events <- .simulation_experiment_events(
            executable$ode_model, experiment$dosing, dimensions,
            experiment$parameters, state_unit_values = artifact$stateUnitValues
        )$data
        doses <- if (nrow(events)) data.frame(
            ID = i, TIME = events$time, EVID = 1L,
            CMT = match(events$var, executable$stateNames), AMT = events$value,
            DV = NA_real_, DVID = NA_integer_, CP_ROW = NA_integer_
        ) else NULL
        observed <- experiment$observations
        obs <- data.frame(
            ID = i,
            TIME = .simulation_numeric_time(observed$time, dimensions),
            EVID = 0L, CMT = 0L, AMT = NA_real_,
            DV = vapply(seq_len(nrow(observed)), function(j) {
                value <- observed$value[[j]]
                if (length(value) != 1L || is.na(value)) NA_real_ else
                    as.numeric(.to_dimensions_value(value, dimensions))
            }, numeric(1)),
            DVID = match(observed$observable, observation),
            CP_ROW = seq_len(nrow(observed))
        )
        fixed <- .merge_ode_parameters(executable$ode_model$parameters,
                                       experiment$parameters)
        excluded <- union(theta_parameters, individual)
        fixed <- fixed[setdiff(names(fixed), excluded)]
        for (name in names(fixed)) {
            backend <- .nlmixr2_parameter_id(executable, name, fallback = FALSE)
            if (!is.null(backend)) {
                value <- as.numeric(.to_dimensions_value(fixed[[name]], dimensions))
                obs[[backend]] <- value
                if (!is.null(doses)) doses[[backend]] <- value
            }
        }
        out <- rbind(doses, obs)
        out[order(out$TIME, -out$EVID), , drop = FALSE]
    })
    do.call(rbind, rows)
}

.estimate_nlmixr2 <- function(problem, engine, dimensions = NULL, ...) {
    translated <- .nlmixr2_prepare(problem, dimensions)
    control <- do.call(nlmixr2est::foceiControl, engine$control)
    table <- do.call(nlmixr2est::tableControl, engine$table)
    raw <- nlmixr2est::nlmixr2(
        translated$model, translated$data, est = engine$method,
        control = control, table = table, ...
    )
    .nlmixr2_result(problem, engine, raw, translated)
}

.nlmixr2_result <- function(problem, engine, raw, translated) {
    theta <- raw$theta
    coefficients <- lapply(names(problem$parameters), function(name) {
        omega_target <- names(translated$omega_parameters)[
            translated$omega_parameters == name
        ]
        if (length(omega_target)) {
            eta <- translated$eta_ids[[omega_target[[1]]]]
            value <- sqrt(as.numeric(raw$omega[eta, eta]))
            spec <- problem$parameters[[name]]
            .nlmixr2_restore_value(value, spec, translated$dimensions)
        } else {
            id <- translated$theta_ids[[name]]
            .nlmixr2_theta_decode(
                unname(theta[[id]]), problem$parameters[[name]], translated$dimensions
            )
        }
    })
    names(coefficients) <- names(problem$parameters)
    has_individual <- length(.nlmixr2_individual_targets(problem$statistics)) > 0L
    diagnostics <- .nlmixr2_diagnostics(
        problem, raw, translated, has_individual = has_individual
    )
    uncertainty <- .nlmixr2_uncertainty(
        problem, raw, translated, coefficients
    )
    neg_log_lik <- -as.numeric(stats::logLik(raw)) +
        translated$log_likelihood_unit_jacobian
    structure(list(
        coefficients = .new_parameters(coefficients),
        standard_errors = uncertainty$standard_errors,
        estimate_correlations = uncertainty$correlations,
        neg_log_lik = neg_log_lik,
        individual_predictions = diagnostics$individual_predictions,
        population_predictions = diagnostics$population_predictions,
        engine = engine, engine_result = raw, problem = problem
    ), class = .compphysiol_class("compphysiol_estimation_result"))
}

.nlmixr2_diagnostics <- function(problem, raw, translated, has_individual) {
    tab <- as.data.frame(raw)
    observation_rows <- if ("EVID" %in% names(tab)) {
        tab[is.na(tab$EVID) | tab$EVID == 0L, , drop = FALSE]
    } else tab
    expected <- sum(vapply(
        .experiments_elements(problem$experiments), function(x) nrow(x$observations), integer(1)
    ))
    if (nrow(observation_rows) != expected) {
        stop("nlmixr2 returned a different number of observation rows than supplied.",
             call. = FALSE)
    }
    input_observations <- translated$data[translated$data$EVID == 0L, , drop = FALSE]
    observation_rows <- observation_rows[
        order(input_observations$ID, input_observations$CP_ROW), , drop = FALSE
    ]
    experiment_names <- .collection_identifiers(problem$experiments, "Experiment")
    make <- function(column) {
        rows <- list()
        k <- 0L
        for (i in seq_along(problem$experiments)) {
            observed <- problem$experiments[[i]]$observations
            label <- experiment_names[[i]]
            for (j in seq_len(nrow(observed))) {
                k <- k + 1L
                value <- observation_rows[[column]][[k]]
                scale <- translated$artifact$observableUnitScales[[
                    observed$observable[[j]]
                ]]
                if (!is.null(scale)) value <- value * scale
                unit <- if (inherits(observed$value, "units")) units::deparse_unit(observed$value) else ""
                rows[[k]] <- list(experiment = label, time = observed$time[[j]],
                                  observable = observed$observable[[j]],
                                  value = .estimation_restore_unit(value, unit))
            }
        }
        .estimation_bind_diagnostic_rows(rows)
    }
    if (has_individual) {
        list(individual_predictions = make("IPRED"),
             population_predictions = make("PRED"))
    } else {
        # nlmixr2 labels the sole prediction column IPRED even without etas.
        list(individual_predictions = NULL,
             population_predictions = make("IPRED"))
    }
}
