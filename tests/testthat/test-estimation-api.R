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

test_that("observation_model composes named error models", {
    observation <- observation_model(
        C = combined_error(
            additive = "sigma_add",
            proportional = "sigma_prop"
        ),
        biomarker = additive_error(sigma = "sigma_biomarker")
    )

    expect_s3_class(observation, "ObservationModel")
    expect_named(observation, c("C", "biomarker"))
    expect_s3_class(observation[["C"]], "ObservationError")
    expect_s3_class(observation[["C"]], "CombinedError")
    expect_identical(observation[["C"]]$additive, "sigma_add")
    expect_identical(observation[["C"]]$proportional, "sigma_prop")
})

test_that("observation error constructors validate their public contract", {
    expect_s3_class(additive_error(sigma = "sigma"), "AdditiveError")
    expect_s3_class(proportional_error(sigma = "sigma"), "ProportionalError")
    expect_s3_class(lognormal_error(sigma = "sigma"), "LognormalError")

    expect_error(observation_model(additive_error()), "named|observable")
    expect_error(
        observation_model(C = additive_error(), C = proportional_error()),
        "unique|duplicated"
    )
    expect_error(additive_error(sigma = ""), "sigma")
    expect_error(combined_error(additive = "sigma", proportional = "sigma"),
                 "distinct|parameter")
})

test_that("parameter estimates combine into a named collection", {
    estimates <- c(
        k = parameter_estimate(
            initial = 0.1 [1/h],
            lower = 0 [1/h],
            upper = 2 [1/h],
            transform = "log"
        ),
        sigma = parameter_estimate(
            initial = 1 [mg/L],
            lower = 0 [mg/L],
            transform = "log"
        )
    )

    expect_s3_class(estimates, "ParameterEstimates")
    expect_named(estimates, c("k", "sigma"))
    expect_s3_class(estimates[["k"]], "ParameterEstimate")
    expect_equal(estimates[["k"]]$initial, with_units(0.1 [1/h]))
    expect_equal(estimates[["k"]]$lower, with_units(0 [1/h]))
    expect_equal(estimates[["k"]]$upper, with_units(2 [1/h]))
    expect_identical(estimates[["k"]]$transform, "log")
})

test_that("ParameterEstimates compose, subset, and print by row", {
    rates <- c(
        k = parameter_estimate(
            initial = 1 [1/h], lower = 0 [1/h], upper = 2 [1/h], transform = "log"
        )
    )
    errors <- c(sigma = parameter_estimate(initial = 0.5, lower = 0))
    estimates <- c(rates, errors)

    expect_s3_class(estimates, "ParameterEstimates")
    expect_named(estimates, c("k", "sigma"))
    expect_s3_class(estimates["k"], "ParameterEstimates")
    expect_named(estimates["k"], "k")
    expect_s3_class(estimates[["k"]], "ParameterEstimate")
    expect_identical(estimates[], estimates)
    expect_error(c(estimates, k = parameter_estimate(1)), "unique|duplicated")
    expect_error(c(estimates, invalid = 1), "ParameterEstimate")

    output <- capture.output(returned <- print(estimates))
    output <- paste(output, collapse = "\n")
    expect_identical(returned, estimates)
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
    expect_error(c(parameter_estimate(initial = 1)), "named|parameter")
    expect_error(
        c(k = parameter_estimate(1), k = parameter_estimate(2)),
        "unique|duplicated"
    )
    expect_error(parameter_estimate(initial = c(1, 2)), "scalar")
    expect_error(parameter_estimate(initial = 1, lower = 2, upper = 1), "bound|lower")
    expect_error(parameter_estimate(initial = -1, transform = "log"), "log|positive")
    expect_error(parameter_estimate(initial = 2, transform = "logit"),
                 "bounds|between")
    expect_error(parameter_estimate(initial = 1, transform = "unknown"), "transform")
})

test_that("parameter estimate defaults follow the transform and initial units", {
    identity <- parameter_estimate(initial = 2 [L])
    log <- parameter_estimate(initial = 2 [L], transform = "log")
    logit <- parameter_estimate(initial = 0.5 [L], transform = "logit")
    unitless <- parameter_estimate(initial = 2, transform = "log")

    expect_equal(identity$lower, with_units(-Inf [L]))
    expect_equal(identity$upper, with_units(Inf [L]))
    expect_equal(log$lower, with_units(0 [L]))
    expect_equal(log$upper, with_units(Inf [L]))
    expect_equal(logit$lower, with_units(0 [L]))
    expect_equal(logit$upper, with_units(1 [L]))
    expect_identical(unitless$lower, 0)
    expect_identical(unitless$upper, Inf)
})

test_that("ParameterEstimate prints its unit once", {
    estimate <- parameter_estimate(
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

test_that("estimation_problem is the backend-neutral estimation specification", {
    model <- estimation_test_model()
    study <- experiments(subject_1 = estimation_test_experiment())
    estimates <- c(
        k = parameter_estimate(0.1 [1/h], lower = 0 [1/h], transform = "log"),
        sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
    )
    observation <- observation_model(C = additive_error(sigma = "sigma"))

    problem <- estimation_problem(
        model = model,
        experiments = study,
        parameters = estimates,
        observation = observation
    )

    expect_s3_class(problem, "EstimationProblem")
    expect_identical(problem$model, model)
    expect_identical(problem$experiments, study)
    expect_s3_class(problem$experiments[[1]]$observations, "ObservationData")
    expect_identical(problem$parameters, estimates)
    expect_identical(problem$observation, observation)
})

test_that("estimation_problem normalizes one experiment to a collection", {
    problem <- estimation_problem(
        model = estimation_test_model(),
        experiments = estimation_test_experiment(),
        parameters = c(
            k = parameter_estimate(0.1 [1/h]),
            sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        observation = observation_model(C = additive_error(sigma = "sigma"))
    )

    expect_s3_class(problem$experiments, "Experiments")
    expect_length(problem$experiments, 1L)
})

test_that("EstimationProblem has a concise print method", {
    problem <- estimation_problem(
        estimation_test_model(),
        estimation_test_experiment(),
        c(
            k = parameter_estimate(0.1 [1/h]),
            sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        observation_model(C = additive_error(sigma = "sigma"))
    )

    output <- capture.output(returned <- print(problem))
    output <- paste(output, collapse = "\n")

    expect_identical(returned, problem)
    expect_match(output, "EstimationProblem", fixed = TRUE)
    expect_match(output, "model: CompartmentModel", fixed = TRUE)
    expect_match(output, "experiments: 1", fixed = TRUE)
    expect_match(output, "observations: 3", fixed = TRUE)
    expect_match(output, "estimated parameters: k, sigma", fixed = TRUE)
    expect_match(output, "observation models: C", fixed = TRUE)
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
                k = parameter_estimate(0.1 [1/h]),
                sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
            ),
            observation_model(C = additive_error(sigma = "sigma"))
        )
        expect_s3_class(problem, "EstimationProblem")
        expect_identical(problem$model, representation)
    }
})

test_that("estimation problems reject unsupported model representations", {
    model <- estimation_test_model()
    common <- list(
        experiments = estimation_test_experiment(),
        parameters = c(k = parameter_estimate(0.1 [1/h])),
        observation = observation_model(C = additive_error(sigma = "sigma"))
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
    estimates <- c(
        k = parameter_estimate(0.1 [1/h]),
        sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
    )

    expect_error(
        estimation_problem(
            model,
            experiment(observations = observation_schedule(c(1, 2) [h], "C")),
            estimates,
            observation_model(C = additive_error(sigma = "sigma"))
        ),
        "ObservationData|data"
    )
    expect_error(
        estimation_problem(
            model,
            estimation_test_experiment(observable = "unknown"),
            estimates,
            observation_model(unknown = additive_error(sigma = "sigma"))
        ),
        "observable|unknown"
    )
    expect_error(
        estimation_problem(
            model,
            estimation_test_experiment(),
            estimates,
            observation_model(other = additive_error(sigma = "sigma"))
        ),
        "observation model|C"
    )
})

test_that("known experiment parameters cannot also be estimated", {
    study <- estimation_test_experiment()
    study$parameters <- parameters(k = 0.3 [1/h])

    expect_error(
        estimation_problem(
            estimation_test_model(),
            study,
            c(
                k = parameter_estimate(0.1 [1/h]),
                sigma = parameter_estimate(1 [mg/L])
            ),
            observation_model(C = additive_error(sigma = "sigma"))
        ),
        "known|experiment|k"
    )
})

test_that("optim_backend keeps optimizer configuration out of estimate", {
    backend <- optim_backend(
        method = "L-BFGS-B",
        control = list(maxit = 250, reltol = 1e-8)
    )

    expect_s3_class(backend, "EstimationBackend")
    expect_s3_class(backend, "OptimBackend")
    expect_identical(backend$method, "L-BFGS-B")
    expect_identical(backend$control, list(maxit = 250, reltol = 1e-8))
    expect_error(optim_backend(method = "not-an-optim-method"), "method")
    expect_error(optim_backend(control = 1), "control")
})

test_that("estimate consumes an EstimationProblem and returns a stable result", {
    problem <- estimation_problem(
        estimation_test_model(),
        estimation_test_experiment(),
        c(
            k = parameter_estimate(0.1 [1/h], lower = 0 [1/h], transform = "log"),
            sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        observation_model(C = additive_error(sigma = "sigma"))
    )

    fit <- estimate(
        problem,
        backend = optim_backend(method = "L-BFGS-B", control = list(maxit = 200))
    )

    expect_s3_class(fit, "EstimationResult")
    expect_s3_class(fit$backend, "OptimBackend")
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
    expect_true(is.list(fit$backend_result))
    expect_equal(as.numeric(coef(fit)$k), 0.197, tolerance = 0.01)
    expect_identical(fit$convergence$code, 0L)

    output <- capture.output(returned <- print(fit))
    output <- paste(output, collapse = "\n")
    expect_identical(returned, fit)
    expect_match(output, "EstimationResult", fixed = TRUE)
    expect_match(output, "negative log-likelihood", fixed = TRUE)
    expect_match(output, "OptimBackend (L-BFGS-B)", fixed = TRUE)
})

test_that("estimation diagnostics retain rows with missing observations", {
    study <- estimation_test_experiment()
    study$observations$value[[2]] <- NA_real_ * study$observations$value[[2]]
    problem <- estimation_problem(
        estimation_test_model(),
        study,
        c(
            k = parameter_estimate(0.1 [1/h], lower = 0 [1/h], transform = "log"),
            sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
        ),
        observation_model(C = additive_error(sigma = "sigma"))
    )

    fit <- estimate(problem, backend = optim_backend(control = list(maxit = 50)))

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
                k = parameter_estimate(0.1 [1/h], lower = 0 [1/h], transform = "log"),
                sigma = parameter_estimate(1 [mg/L], lower = 0 [mg/L], transform = "log")
            ),
            observation_model(C = additive_error(sigma = "sigma"))
        )
        fit <- estimate(problem, backend = optim_backend(control = list(maxit = 50)))
        expect_s3_class(fit, "EstimationResult")
        expect_identical(fit$convergence$code, 0L)
        expect_equal(as.numeric(coef(fit)$k), 0.197, tolerance = 0.01)
    }
})

test_that("estimate does not accept the model in place of an EstimationProblem", {
    expect_error(
        estimate(estimation_test_model(), backend = optim_backend()),
        "EstimationProblem|method"
    )
})
