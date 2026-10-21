estimation_test_model <- function() {
    compartment_model() |>
        add_compartment("Central", volume = 1 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", NULL, molec = "drug", const = "k") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k = 0.2 [1/h])
}

estimation_test_experiment <- function(observable = "C") {
    data <- observation_data(
        time = with_units(c(1, 2, 4) [h]),
        observable = observable,
        value = with_units(c(82, 68, 45) [mg/L])
    )
    experiment(observations = data, start = 0 [h])
}

test_that("estimation diagnostics preserve experiment order in a factor", {
    rows <- lapply(c("individual_1", "individual_2", "individual_10"), function(id) {
        list(
            experiment = id,
            time = units::set_units(1, h),
            observable = "C",
            value = units::set_units(1, mg/L)
        )
    })

    diagnostics <- .estimation_bind_diagnostic_rows(rows)

    expect_s3_class(diagnostics$experiment, "factor")
    expect_identical(
        levels(diagnostics$experiment),
        c("individual_1", "individual_2", "individual_10")
    )
})

test_that("parameter specifications combine into a named collection", {
    spec <- c(
        k = parameter_spec(
            initial = 0.1 [1/h],
            lower = 0 [1/h],
            upper = 2 [1/h],
            transform = "log"
        ),
        sigma = parameter_spec(
            initial = 1 [mg/L],
            lower = 0 [mg/L],
            transform = "log"
        )
    )

    expect_s3_class(spec, "compphysiol_parameter_specs")
    expect_named(spec, c("k", "sigma"))
    expect_s3_class(spec[["k"]], "compphysiol_parameter_spec")
    expect_equal(spec[["k"]]$initial, with_units(0.1 [1/h]))
    expect_equal(spec[["k"]]$lower, with_units(0 [1/h]))
    expect_equal(spec[["k"]]$upper, with_units(2 [1/h]))
    expect_identical(spec[["k"]]$transform, "log")
})

test_that("parameter_specs applies a shared transform with explicit overrides", {
    specs <- parameter_specs(
        CL = 0.8 [L/h],
        omega = 0.2,
        fraction = parameter_spec(
            0.5, lower = 0, upper = 1, transform = "logit"
        ),
        identity = parameter_spec(2),
        transform = "log"
    )

    expect_s3_class(specs, "compphysiol_parameter_specs")
    expect_named(specs, c("CL", "omega", "fraction", "identity"))
    expect_equal(specs$CL$initial, with_units(0.8 [L/h]))
    expect_identical(specs$CL$transform, "log")
    expect_identical(specs$omega$transform, "log")
    expect_identical(specs$fraction$transform, "logit")
    expect_equal(specs$fraction$lower, 0)
    expect_equal(specs$fraction$upper, 1)
    expect_identical(specs$identity$transform, "identity")

    initial <- with_units(3 [mg/L])
    from_variable <- parameter_specs(sigma = initial, transform = "log")
    expect_equal(from_variable$sigma$initial, initial)
    expect_identical(from_variable$sigma$transform, "log")
    expect_identical(parameter_specs(), .new_parameter_specs())
})

test_that("parameter_specs validates collection inputs", {
    expect_error(parameter_specs(1, transform = "log"), "named")
    expect_error(parameter_specs(x = 1, x = 2), "unique|duplicated")
    expect_error(parameter_specs(x = 1, transform = "unknown"), "transform")
    expect_error(parameter_specs(x = -1, transform = "log"), "positive")
    expect_error(
        parameter_specs(x = c(a = parameter_spec(1))),
        "numeric scalar|parameter"
    )
})

test_that("ParameterSpecs compose, subset, and print by row", {
    rates <- c(
        k = parameter_spec(
            initial = 1 [1/h], lower = 0 [1/h], upper = 2 [1/h], transform = "log"
        )
    )
    errors <- c(sigma = parameter_spec(initial = 0.5, lower = 0))
    spec <- c(rates, errors)

    expect_s3_class(spec, "compphysiol_parameter_specs")
    expect_named(spec, c("k", "sigma"))
    expect_s3_class(spec["k"], "compphysiol_parameter_specs")
    expect_named(spec["k"], "k")
    expect_s3_class(spec[["k"]], "compphysiol_parameter_spec")
    expect_identical(spec[], spec)
    expect_error(spec[c("k", "k")], "unique")
    expect_error(c(spec, k = parameter_spec(1)), "unique|duplicated")
    expect_error(c(spec, invalid = 1), "parameter specification")

    output <- capture.output(returned <- print(spec))
    output <- paste(output, collapse = "\n")
    expect_identical(returned, spec)
    expect_match(
        output,
        "k: initial = 1, bounds = [0, 2], transform = log, unit [1/h]",
        fixed = TRUE
    )
    expect_match(
        output,
        "sigma: initial = 0.5, bounds = [0, Inf], transform = identity, unit [1]",
        fixed = TRUE
    )
})

test_that("estimated parameter specifications reject ambiguous inputs", {
    expect_error(c(parameter_spec(initial = 1)), "named|parameter")
    expect_error(
        c(k = parameter_spec(1), k = parameter_spec(2)),
        "unique|duplicated"
    )
    expect_error(parameter_spec(initial = c(1, 2)), "scalar")
    expect_error(parameter_spec(initial = 1, lower = 2, upper = 1), "bound|lower")
    expect_error(parameter_spec(initial = -1, transform = "log"), "log|positive")
    expect_error(parameter_spec(initial = 2, transform = "logit"),
                 "bounds|between")
    expect_error(parameter_spec(initial = 1, transform = "unknown"), "transform")
})

test_that("parameter specification low-level construction trusts canonical input", {
    spec <- .new_parameter_spec(list(
        initial = -1,
        lower = 0,
        upper = Inf,
        transform = "log"
    ))
    expect_s3_class(spec, "compphysiol_parameter_spec")

    specs <- .new_parameter_specs(list(unnamed = spec, unnamed = spec))
    expect_s3_class(specs, "compphysiol_parameter_specs")
})

test_that("parameter specification defaults follow the transform and initial units", {
    identity <- parameter_spec(initial = 2 [L])
    log <- parameter_spec(initial = 2 [L], transform = "log")
    logit <- parameter_spec(initial = 0.5 [L], transform = "logit")
    unitless <- parameter_spec(initial = 2, transform = "log")

    expect_equal(identity$lower, with_units(-Inf [L]))
    expect_equal(identity$upper, with_units(Inf [L]))
    expect_equal(log$lower, with_units(0 [L]))
    expect_equal(log$upper, with_units(Inf [L]))
    expect_equal(logit$lower, with_units(0 [L]))
    expect_equal(logit$upper, with_units(1 [L]))
    expect_identical(unitless$lower, 0)
    expect_identical(unitless$upper, Inf)
})

test_that("ParameterSpec prints its unit once", {
    estimate <- parameter_spec(
        initial = 1 [L], lower = 500 [mL], upper = 2 [L], transform = "log"
    )

    output <- capture.output(returned <- print(estimate))
    output <- paste(output, collapse = "\n")

    expect_identical(returned, estimate)
    expect_match(output, "initial: 1", fixed = TRUE)
    expect_match(output, "bounds: [0.5, 2]", fixed = TRUE)
    expect_identical(sum(strsplit(output, "", fixed = TRUE)[[1]] == "L"), 1L)
    expect_match(output, "unit: L", fixed = TRUE)
})

test_that("estimation_problem is the engine-neutral estimation specification", {
    model <- estimation_test_model()
    study <- experiments(individual_1 = estimation_test_experiment())
    spec <- c(
        k = parameter_spec(0.1 [1/h], lower = 0 [1/h], transform = "log"),
        sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
    )
    statistics <- statistical_model(C = normal(sd = "sigma"))

    problem <- estimation_problem(
        model = model,
        experiments = study,
        parameters = spec,
        statistics = statistics
    )

    expect_s3_class(problem, "compphysiol_estimation_problem")
    expect_identical(problem$model, model)
    expect_identical(problem$experiments, study)
    expect_s3_class(problem$experiments[[1]]$observations, "compphysiol_observation_data")
    expect_identical(problem$parameters, spec)
    expect_s3_class(problem$statistics, "compphysiol_statistical_model")
    expect_identical(problem$statistics$C$level, "observation")
    expect_s3_class(problem$statistics$C$mean, "compphysiol_prediction_location")
})

test_that("estimation_problem normalizes one experiment to a collection", {
    problem <- estimation_problem(
        model = estimation_test_model(),
        experiments = estimation_test_experiment(),
        parameters = c(
            k = parameter_spec(0.1 [1/h]),
            sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        statistics = statistical_model(C = normal(sd = "sigma"))
    )

    expect_s3_class(problem$experiments, "compphysiol_experiments")
    expect_length(problem$experiments, 1L)
})

test_that("EstimationProblem has a concise print method", {
    problem <- estimation_problem(
        estimation_test_model(),
        estimation_test_experiment(),
        c(
            k = parameter_spec(0.1 [1/h]),
            sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        statistical_model(C = normal(sd = "sigma"))
    )

    output <- capture.output(returned <- print(problem))
    output <- paste(output, collapse = "\n")

    expect_identical(returned, problem)
    expect_match(output, "Estimation problem", fixed = TRUE)
    expect_match(output, "model: Compartment model", fixed = TRUE)
    expect_match(output, "experiments: 1", fixed = TRUE)
    expect_match(output, "observations: 3", fixed = TRUE)
    expect_match(output, "estimated parameters: k, sigma", fixed = TRUE)
    expect_match(output, "statistical targets: C", fixed = TRUE)
})

test_that("estimation problems accept deterministic lowered representations", {
    model <- estimation_test_model()
    representations <- list(
        model,
        to_process_model(model),
        to_ode_model(model),
        to_compiled_ode_model(model)
    )

    for (representation in representations) {
        problem <- estimation_problem(
            representation,
            estimation_test_experiment(),
            c(
                k = parameter_spec(0.1 [1/h]),
                sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
            ),
            statistical_model(C = normal(sd = "sigma"))
        )
        expect_s3_class(problem, "compphysiol_estimation_problem")
        expect_identical(problem$model, representation)
    }
})

test_that("estimation problems reject unsupported model representations", {
    model <- estimation_test_model()
    common <- list(
        experiments = estimation_test_experiment(),
        parameters = c(k = parameter_spec(0.1 [1/h])),
        statistics = statistical_model(C = normal(sd = "sigma"))
    )

    expect_error(
        do.call(estimation_problem, c(list(model = to_analytical_model(model)), common)),
        "AnalyticalModel|not supported"
    )
    expect_error(
        do.call(estimation_problem, c(list(model = structure(list(), class = "compphysiol_stochastic_model")), common)),
        "StochasticModel|not supported"
    )
})

test_that("estimation problems require observation data and validate it against the model", {
    model <- estimation_test_model()
    spec <- c(
        k = parameter_spec(0.1 [1/h]),
        sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
    )

    expect_error(
        estimation_problem(
            model,
            experiment(observations = observation_schedule(c(1, 2) [h], "C")),
            spec,
            statistical_model(C = normal(sd = "sigma"))
        ),
        "ObservationData|data"
    )
    expect_error(
        estimation_problem(
            model,
            estimation_test_experiment(observable = "unknown"),
            spec,
            statistical_model(unknown = normal(sd = "sigma"))
        ),
        "observable|unknown"
    )
    expect_error(
        estimation_problem(
            model,
            estimation_test_experiment(),
            spec,
            statistical_model(other = normal(sd = "sigma"))
        ),
        "statistical-model target|Statistical model|C"
    )
})

test_that("estimation problems resolve individual entries but optim rejects them", {
    model <- compartment_model() |>
        add_compartment("Central", volume = 1 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", NULL, molec = "drug", const = "k") |>
        add_observable(C = c[drug, Central])
    specs <- c(
        k_pop = parameter_spec(0.2 [1/h]),
        omega_k = parameter_spec(0.1, lower = 0),
        sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L])
    )
    statistics <- statistical_model(
        k = normal(mean = "k_pop", sd = proportional("omega_k")),
        C = normal(sd = "sigma")
    )

    problem <- estimation_problem(model, estimation_test_experiment(), specs, statistics)

    expect_identical(problem$statistics$k$level, "individual")
    expect_identical(problem$statistics$C$level, "observation")
    expect_false("k_pop" %in% names(to_ode_model(model)$parameters))
    expect_error(estimate(problem), "Mixed-effects|supporting estimation engine")
})

test_that("fixed observation scales need no parameter specification", {
    problem <- estimation_problem(
        estimation_test_model(),
        estimation_test_experiment(),
        c(k = parameter_spec(0.1 [1/h])),
        statistical_model(C = normal(sd = 1 [mg/L]))
    )

    expect_s3_class(problem, "compphysiol_estimation_problem")
    expect_named(problem$parameters, "k")
})

test_that("known experiment parameters cannot also be estimated", {
    study <- estimation_test_experiment()
    study$parameters <- parameters(k = 0.3 [1/h])

    expect_error(
        estimation_problem(
            estimation_test_model(),
            study,
            c(
                k = parameter_spec(0.1 [1/h]),
                sigma = parameter_spec(1 [mg/L])
            ),
            statistical_model(C = normal(sd = "sigma"))
        ),
        "known|experiment|k"
    )
})

test_that("optim_engine keeps optimizer configuration out of estimate", {
    engine <- optim_engine(
        method = "L-BFGS-B",
        control = list(maxit = 250, reltol = 1e-8)
    )

    expect_s3_class(engine, "compphysiol_estimation_engine")
    expect_s3_class(engine, "compphysiol_optim_engine")
    expect_identical(engine$method, "L-BFGS-B")
    expect_identical(engine$control, list(maxit = 250, reltol = 1e-8))
    expect_s3_class(engine$simulation, "compphysiol_simulation_engine")
    expect_s3_class(engine$simulation, "compphysiol_desolve_engine")
    expect_false(engine$simulation$compiled)
    expect_error(optim_engine(method = "not-an-optim-method"), "method")
    expect_error(optim_engine(control = 1), "control")
    expect_error(optim_engine(simulation = list()), "Simulation engine")
    expect_error(optim_engine(hessian = NA), "hessian")
})

test_that("deSolve_engine configures the representation used by estimation", {
    interpreted <- deSolve_engine()
    compiled <- deSolve_engine(compiled = TRUE)

    expect_s3_class(interpreted, "compphysiol_simulation_engine")
    expect_s3_class(interpreted, "compphysiol_desolve_engine")
    expect_false(interpreted$compiled)
    expect_true(compiled$compiled)
    expect_s3_class(
        .simulation_model_for_engine(estimation_test_model(), compiled),
        "compphysiol_desolve_compiled_model"
    )
    expect_error(deSolve_engine(compiled = NA), "TRUE or FALSE")
})

test_that("estimation forwards the configured simulation engine", {
    problem <- estimation_problem(
        estimation_test_model(),
        estimation_test_experiment(),
        c(
            k = parameter_spec(0.1 [1/h], transform = "log"),
            sigma = parameter_spec(1 [mg/L], transform = "log")
        ),
        statistical_model(C = normal(sd = "sigma"))
    )

    compiled <- estimate(
        problem,
        engine = optim_engine(
            control = list(maxit = 1),
            simulation = deSolve_engine(compiled = TRUE)
        )
    )
    expect_s3_class(compiled, "compphysiol_estimation_result")

    skip_if_not_installed("rxode2")
    expect_error(
        estimate(
            problem,
            engine = optim_engine(
                control = list(maxit = 1),
                simulation = rxode2_engine(rtol = "invalid")
            )
        ),
        "initial values|rtol|numeric"
    )
})

test_that("optim methods accept only bounds supported in optimizer coordinates", {
    identity_unbounded <- c(x = parameter_spec(0))
    identity_bounded <- c(x = parameter_spec(0.5, lower = 0, upper = 1))
    log_natural <- c(x = parameter_spec(1, transform = "log"))
    log_bounded <- c(x = parameter_spec(1, upper = 2, transform = "log"))
    logit <- c(x = parameter_spec(0.5, transform = "logit"))

    validate <- function(method, specs) {
        .estimation_validate_optim_method(method, specs, .estimation_coordinates(specs))
    }

    expect_no_error(validate("BFGS", identity_unbounded))
    expect_no_error(validate("BFGS", log_natural))
    expect_no_error(validate("BFGS", logit))
    expect_error(validate("BFGS", identity_bounded), "does not support.*x")
    expect_error(validate("BFGS", log_bounded), "does not support.*x")
    expect_no_error(validate("L-BFGS-B", identity_bounded))
})

test_that("Brent requires one bounded identity-transformed parameter", {
    bounded <- c(x = parameter_spec(0.5, lower = 0, upper = 1))
    unbounded <- c(x = parameter_spec(0.5))
    logit <- c(x = parameter_spec(0.5, transform = "logit"))
    two <- c(x = parameter_spec(0, lower = -1, upper = 1),
             y = parameter_spec(0, lower = -1, upper = 1))

    validate <- function(specs) {
        .estimation_validate_optim_method("Brent", specs, .estimation_coordinates(specs))
    }

    expect_no_error(validate(bounded))
    expect_error(validate(unbounded), "finite lower and upper")
    expect_error(validate(logit), "identity-transformed")
    expect_error(validate(two), "exactly one")
})

test_that("optimization results are validated before decoding", {
    bounded <- c(x = parameter_spec(0.5, lower = 0, upper = 1))
    coordinates <- .estimation_coordinates(bounded)

    expect_equal(.estimation_validate_optim_result(0.75, coordinates, bounded)$x, 0.75)
    expect_error(
        .estimation_validate_optim_result(1.1, coordinates, bounded),
        "outside their bounds"
    )
    expect_error(
        .estimation_validate_optim_result(Inf, coordinates, bounded),
        "invalid parameter coordinates"
    )

    log_spec <- c(x = parameter_spec(1, transform = "log"))
    expect_error(
        .estimation_validate_optim_result(1000, .estimation_coordinates(log_spec), log_spec),
        "invalid value.*x"
    )
})

test_that("optim uncertainty is transformed to the public parameter scale", {
    specs <- c(
        identity = parameter_spec(2),
        log = parameter_spec(2, transform = "log"),
        logit = parameter_spec(0.5, lower = 0, upper = 1, transform = "logit")
    )
    raw <- list(
        par = c(identity = 2, log = log(2), logit = 0),
        hessian = diag(3)
    )

    uncertainty <- .estimation_optim_uncertainty(raw, specs)

    expect_equal(
        vapply(uncertainty$standard_errors, as.numeric, numeric(1)),
        c(identity = 1, log = 2, logit = 0.25)
    )
    expect_equal(
        uncertainty$correlations,
        structure(
            diag(3),
            dimnames = rep(list(c("identity", "log", "logit")), 2)
        )
    )

    unavailable <- .estimation_optim_uncertainty(list(par = raw$par), specs)
    expect_true(all(is.na(vapply(
        unavailable$standard_errors, as.numeric, numeric(1)
    ))))
    expect_true(all(is.na(unavailable$correlations)))
})

test_that("estimate consumes an EstimationProblem and returns a stable result", {
    problem <- estimation_problem(
        estimation_test_model(),
        estimation_test_experiment(),
        c(
            k = parameter_spec(0.1 [1/h], lower = 0 [1/h], transform = "log"),
            sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        statistical_model(C = normal(sd = "sigma"))
    )

    fit <- estimate(
        problem,
        engine = optim_engine(method = "L-BFGS-B", control = list(maxit = 200))
    )

    expect_s3_class(fit, "compphysiol_estimation_result")
    expect_s3_class(fit$engine, "compphysiol_optim_engine")
    expect_named(coef(fit), c("k", "sigma"))
    expect_true(is.numeric(fit$neg_log_lik) && length(fit$neg_log_lik) == 1L)
    expect_named(fit, c(
        "coefficients", "standard_errors", "estimate_correlations",
        "neg_log_lik", "population_predictions", "individual_predictions",
        "engine", "engine_result", "problem"
    ))
    expect_named(standard_errors(fit), names(coef(fit)))
    expect_s3_class(standard_errors(fit)$k, "units")
    expect_s3_class(standard_errors(fit)$sigma, "units")
    expect_true(all(vapply(standard_errors(fit), function(x) {
        is.finite(as.numeric(x)) && as.numeric(x) >= 0
    }, logical(1))))
    expect_identical(
        dimnames(estimate_correlations(fit)),
        list(names(coef(fit)), names(coef(fit)))
    )
    expect_equal(
        diag(estimate_correlations(fit)),
        stats::setNames(rep(1, length(coef(fit))), names(coef(fit)))
    )
    expect_null(fit$individual_predictions)
    expect_s3_class(fitted(fit), "compphysiol_observation_data")
    expect_s3_class(fitted(fit)$time, "units")
    expect_s3_class(fitted(fit)$value, "units")
    expect_equal(fitted(fit)$time, with_units(c(1, 2, 4) [h]))
    expect_identical(units::deparse_unit(fitted(fit)$value), "mg L-1")
    expect_identical(names(fitted(fit)), c("time", "observable", "value", "experiment"))
    expect_true(is.list(fit$engine_result))
    expect_s3_class(fit$engine_result$residuals, "compphysiol_observation_data")
    expect_identical(
        names(fit$engine_result$residuals),
        c("time", "observable", "value", "experiment")
    )
    expect_equal(as.numeric(coef(fit)$k), 0.197, tolerance = 0.01)
    expect_identical(fit$engine_result$convergence, 0L)
    expect_error(fitted(fit, "PRED"), "only population predictions")

    output <- capture.output(returned <- print(fit))
    output <- paste(output, collapse = "\n")
    expect_identical(returned, fit)
    expect_match(output, "Estimation result", fixed = TRUE)
    expect_match(output, "negative log-likelihood", fixed = TRUE)
    expect_match(output, "optim engine (L-BFGS-B)", fixed = TRUE)
})

test_that("optim parameter overlays preserve fixed experiments and metadata", {
    studies <- experiments(
        first = experiment(parameters = parameters(BW = 70, group = "A")),
        second = experiment(parameters = parameters(BW = 80))
    )
    homogeneity <- .experiments_component_homogeneity(studies)
    plan <- .optim_parameter_overlay_plan(studies, c("k", "V"))

    first <- .optim_apply_parameter_overlay(
        plan, .new_parameters(list(k = 0.1, V = 3))
    )
    second <- .optim_apply_parameter_overlay(
        plan, .new_parameters(list(k = 0.2, V = 4))
    )

    expect_identical(names(first), c("first", "second"))
    expect_s3_class(first[[1]]$parameters, "compphysiol_parameters")
    expect_named(first[[1]]$parameters, c("BW", "group", "k", "V"))
    expect_named(first[[2]]$parameters, c("BW", "k", "V"))
    expect_identical(first[[1]]$parameters$k, 0.1)
    expect_identical(first[[2]]$parameters$V, 3)
    expect_identical(second[[1]]$parameters$k, 0.2)
    expect_identical(second[[2]]$parameters$V, 4)
    expect_identical(.experiments_component_homogeneity(first), homogeneity)
    expect_identical(.experiments_component_homogeneity(second), homogeneity)
    expect_false(homogeneity[["parameter_schema"]])
    expect_false(homogeneity[["parameters"]])
    expect_named(studies[[1]]$parameters, c("BW", "group"))
    expect_named(studies[[2]]$parameters, "BW")
})

test_that("optim parameter overlays preserve homogeneous parameter schemas", {
    studies <- experiments(
        first = experiment(parameters = parameters(BW = 70)),
        second = experiment(parameters = parameters(BW = 80))
    )
    homogeneity <- .experiments_component_homogeneity(studies)
    plan <- .optim_parameter_overlay_plan(studies, "k")
    overlaid <- .optim_apply_parameter_overlay(
        plan, .new_parameters(list(k = 0.1))
    )

    expect_true(homogeneity[["parameter_schema"]])
    expect_false(homogeneity[["parameters"]])
    expect_identical(.experiments_component_homogeneity(overlaid), homogeneity)
})

test_that("optim skips parameter overlays without estimated model parameters", {
    studies <- experiments(first = experiment(), second = experiment())
    plan <- .optim_parameter_overlay_plan(studies, character())

    expect_identical(
        .optim_apply_parameter_overlay(plan, .new_parameters()),
        studies
    )
})

test_that("estimation predictions retain rows with missing observations", {
    study <- estimation_test_experiment()
    study$observations$value[[2]] <- NA_real_ * study$observations$value[[2]]
    problem <- estimation_problem(
        estimation_test_model(),
        study,
        c(
            k = parameter_spec(0.1 [1/h], lower = 0 [1/h], transform = "log"),
            sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        statistical_model(C = normal(sd = "sigma"))
    )

    fit <- estimate(problem, engine = optim_engine(control = list(maxit = 50)))

    expect_equal(nrow(fitted(fit)), nrow(study$observations))
    expect_equal(nrow(fit$engine_result$residuals), nrow(study$observations))
    expect_false(is.na(fitted(fit)$value[[2]]))
    expect_true(is.na(fit$engine_result$residuals$value[[2]]))
})

test_that("estimate runs lowered deterministic representations", {
    model <- estimation_test_model()
    representations <- list(
        to_process_model(model),
        to_ode_model(model),
        to_compiled_ode_model(model)
    )
    for (representation in representations) {
        problem <- estimation_problem(
            representation,
            estimation_test_experiment(),
            c(
                k = parameter_spec(0.1 [1/h], lower = 0 [1/h], transform = "log"),
                sigma = parameter_spec(1 [mg/L], lower = 0 [mg/L], transform = "log")
            ),
            statistical_model(C = normal(sd = "sigma"))
        )
        fit <- estimate(problem, engine = optim_engine(control = list(maxit = 50)))
        expect_s3_class(fit, "compphysiol_estimation_result")
        expect_identical(fit$engine_result$convergence, 0L)
        expect_equal(as.numeric(coef(fit)$k), 0.197, tolerance = 0.01)
    }
})

test_that("estimate does not accept the model in place of an EstimationProblem", {
    expect_error(
        estimate(estimation_test_model(), engine = optim_engine()),
        "EstimationProblem|method"
    )
})
