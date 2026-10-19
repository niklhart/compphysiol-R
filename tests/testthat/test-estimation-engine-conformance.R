fixed_effects_conformance_problem <- function() {
    model <- compartment_model() |>
        add_compartment("Central", volume = 1) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(
            "Central", NULL, molec = "drug",
            rate = "CL * c[drug, Central]"
        ) |>
        add_observable(C = c[drug, Central])
    studies <- experiments(
        first = experiment(observations = observation_data(
            time = c(1, 2, 4, 6), observable = "C",
            value = c(82.1, 67.0, 44.8, 30.2)
        )),
        second = experiment(observations = observation_data(
            time = c(1, 2, 4, 6), observable = "C",
            value = c(81.5, 66.6, 45.2, 29.8)
        ))
    )
    estimation_problem(
        model, studies,
        c(
            CL = parameter_spec(
                0.15, lower = 0.001, upper = 1, transform = "log"
            ),
            sigma = parameter_spec(
                2, lower = 0.001, upper = 20, transform = "log"
            )
        ),
        statistical_model(C = normal(sd = "sigma"))
    )
}

test_that("optim and nlmixr2 conform on a fixed-effects problem", {
    skip_if_not_installed("nlmixr2est")
    problem <- fixed_effects_conformance_problem()

    optim_fit <- estimate(
        problem,
        optim_engine(method = "L-BFGS-B", control = list(maxit = 500L))
    )
    nlmixr2_fit <- estimate(
        problem,
        nlmixr2_engine(control = list(
            print = 0L, maxOuterIterations = 500L, sigdig = 8L,
            covMethod = ""
        ))
    )

    expect_equal(
        vapply(coef(nlmixr2_fit), as.numeric, numeric(1)),
        vapply(coef(optim_fit), as.numeric, numeric(1)),
        tolerance = 1e-4
    )
    expect_equal(
        nlmixr2_fit$population_predictions$value,
        optim_fit$population_predictions$value,
        tolerance = 1e-4
    )
    expect_equal(
        nlmixr2_fit$neg_log_lik,
        optim_fit$neg_log_lik,
        tolerance = 1e-4
    )

    expect_null(nlmixr2_fit$individual_predictions)
    expect_identical(
        fitted(nlmixr2_fit), nlmixr2_fit$population_predictions
    )
    expect_identical(
        fitted(nlmixr2_fit, "PRED"), nlmixr2_fit$population_predictions
    )
    expect_identical(
        fitted(nlmixr2_fit, "IPRED"), nlmixr2_fit$population_predictions
    )
})
