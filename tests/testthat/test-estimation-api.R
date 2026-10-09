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

    expect_s3_class(spec, "ParameterSpecs")
    expect_named(spec, c("k", "sigma"))
    expect_s3_class(spec[["k"]], "ParameterSpec")
    expect_equal(spec[["k"]]$initial, with_units(0.1 [1/h]))
    expect_equal(spec[["k"]]$lower, with_units(0 [1/h]))
    expect_equal(spec[["k"]]$upper, with_units(2 [1/h]))
    expect_identical(spec[["k"]]$transform, "log")
})

test_that("ParameterSpecs compose, subset, and print by row", {
    rates <- c(
        k = parameter_spec(
            initial = 1 [1/h], lower = 0 [1/h], upper = 2 [1/h], transform = "log"
        )
    )
    errors <- c(sigma = parameter_spec(initial = 0.5, lower = 0))
    spec <- c(rates, errors)

    expect_s3_class(spec, "ParameterSpecs")
    expect_named(spec, c("k", "sigma"))
    expect_s3_class(spec["k"], "ParameterSpecs")
    expect_named(spec["k"], "k")
    expect_s3_class(spec[["k"]], "ParameterSpec")
    expect_identical(spec[], spec)
    expect_error(c(spec, k = parameter_spec(1)), "unique|duplicated")
    expect_error(c(spec, invalid = 1), "ParameterSpec")

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

    expect_s3_class(problem, "EstimationProblem")
    expect_identical(problem$model, model)
    expect_identical(problem$experiments, study)
    expect_s3_class(problem$experiments[[1]]$observations, "ObservationData")
    expect_identical(problem$parameters, spec)
    expect_s3_class(problem$statistics, "StatisticalModel")
    expect_identical(problem$statistics$C$level, "observation")
    expect_s3_class(problem$statistics$C$mean, "PredictionLocation")
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

    expect_s3_class(problem$experiments, "Experiments")
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
    expect_match(output, "EstimationProblem", fixed = TRUE)
    expect_match(output, "model: CompartmentModel", fixed = TRUE)
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
        expect_s3_class(problem, "EstimationProblem")
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
        do.call(estimation_problem, c(list(model = structure(list(), class = "StochasticModel")), common)),
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

    expect_s3_class(problem, "EstimationProblem")
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

    expect_s3_class(engine, "EstimationEngine")
    expect_s3_class(engine, "OptimEngine")
    expect_identical(engine$method, "L-BFGS-B")
    expect_identical(engine$control, list(maxit = 250, reltol = 1e-8))
    expect_s3_class(engine$simulation, "SimulationEngine")
    expect_s3_class(engine$simulation, "DeSolveEngine")
    expect_false(engine$simulation$compiled)
    expect_error(optim_engine(method = "not-an-optim-method"), "method")
    expect_error(optim_engine(control = 1), "control")
    expect_error(optim_engine(simulation = list()), "SimulationEngine")
})

test_that("deSolve_engine configures the representation used by estimation", {
    interpreted <- deSolve_engine()
    compiled <- deSolve_engine(compiled = TRUE)

    expect_s3_class(interpreted, "SimulationEngine")
    expect_s3_class(interpreted, "DeSolveEngine")
    expect_false(interpreted$compiled)
    expect_true(compiled$compiled)
    expect_s3_class(
        .simulation_model_for_engine(estimation_test_model(), compiled),
        "CompiledOdeModel"
    )
    expect_error(deSolve_engine(compiled = NA), "TRUE or FALSE")
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

    expect_s3_class(fit, "EstimationResult")
    expect_s3_class(fit$engine, "OptimEngine")
    expect_named(coef(fit), c("k", "sigma"))
    expect_true(is.numeric(fit$neg_log_lik) && length(fit$neg_log_lik) == 1L)
    expect_null(fit$objective)
    expect_true(is.list(fit$convergence))
    expect_s3_class(fitted(fit), "ObservationData")
    expect_s3_class(residuals(fit), "ObservationData")
    expect_s3_class(fitted(fit)$time, "units")
    expect_s3_class(fitted(fit)$value, "units")
    expect_s3_class(residuals(fit)$time, "units")
    expect_s3_class(residuals(fit)$value, "units")
    expect_equal(fitted(fit)$time, with_units(c(1, 2, 4) [h]))
    expect_identical(units::deparse_unit(fitted(fit)$value), "mg L-1")
    expect_identical(names(fitted(fit)), c("time", "observable", "value", "experiment"))
    expect_identical(names(residuals(fit)), c("time", "observable", "value", "experiment"))
    expect_true(is.list(fit$engine_result))
    expect_equal(as.numeric(coef(fit)$k), 0.197, tolerance = 0.01)
    expect_identical(fit$convergence$code, 0L)

    output <- capture.output(returned <- print(fit))
    output <- paste(output, collapse = "\n")
    expect_identical(returned, fit)
    expect_match(output, "EstimationResult", fixed = TRUE)
    expect_match(output, "negative log-likelihood", fixed = TRUE)
    expect_match(output, "OptimEngine (L-BFGS-B)", fixed = TRUE)
})

test_that("estimation diagnostics retain rows with missing observations", {
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
    expect_equal(nrow(residuals(fit)), nrow(study$observations))
    expect_false(is.na(fitted(fit)$value[[2]]))
    expect_true(is.na(residuals(fit)$value[[2]]))
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
        expect_s3_class(fit, "EstimationResult")
        expect_identical(fit$convergence$code, 0L)
        expect_equal(as.numeric(coef(fit)$k), 0.197, tolerance = 0.01)
    }
})

test_that("estimate does not accept the model in place of an EstimationProblem", {
    expect_error(
        estimate(estimation_test_model(), engine = optim_engine()),
        "EstimationProblem|method"
    )
})
