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

unit_aware_fixed_effects_conformance_problem <- function() {
    model <- compartment_model() |>
        add_compartment("Central", volume = 2 [L]) |>
        add_molecule(
            "drug", cmt = "Central", initial = 0 [mg], type = "amount"
        ) |>
        add_transport(
            "Central", NULL, molec = "drug",
            rate = "CL * c[drug, Central]"
        ) |>
        add_observable(C = c[drug, Central])
    dose <- dosing(
        time = 0 [min], amount = 100 [mg], cmt = "Central", molec = "drug"
    )
    observation_times <- with_units(c(1, 2, 4, 6) [h])
    concentration_unit <- units::set_units(1, "mg/L", mode = "standard")
    studies <- experiments(
        first = experiment(
            dosing = dose,
            observations = observation_data(
                time = observation_times, observable = "C",
                value = c(41.1, 33.4, 22.6, 15.0) * concentration_unit
            )
        ),
        second = experiment(
            dosing = dose,
            observations = observation_data(
                time = observation_times, observable = "C",
                value = c(40.8, 33.7, 22.3, 15.2) * concentration_unit
            )
        )
    )
    estimation_problem(
        model, studies,
        c(
            CL = parameter_spec(
                0.3 [L/h], lower = 0.01 [L/h], upper = 2 [L/h],
                transform = "log"
            ),
            sigma = parameter_spec(
                1 [mg/L], lower = 0.01 [mg/L], upper = 10 [mg/L],
                transform = "log"
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

test_that("optim and nlmixr2 conform on a fully unit-aware workflow", {
    skip_if_not_installed("nlmixr2est")
    problem <- unit_aware_fixed_effects_conformance_problem()

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

    expect_equal(coef(nlmixr2_fit)$CL, coef(optim_fit)$CL, tolerance = 1e-4)
    expect_equal(
        coef(nlmixr2_fit)$sigma, coef(optim_fit)$sigma, tolerance = 1e-4
    )
    expect_equal(
        fitted(nlmixr2_fit)$value, fitted(optim_fit)$value,
        tolerance = 1e-4
    )
    expect_equal(
        nlmixr2_fit$neg_log_lik, optim_fit$neg_log_lik,
        tolerance = 1e-4
    )

    expect_s3_class(coef(nlmixr2_fit)$CL, "units")
    expect_s3_class(coef(nlmixr2_fit)$sigma, "units")
    expect_true(units::ud_are_convertible(
        units::deparse_unit(coef(nlmixr2_fit)$CL), "L/h"
    ))
    expect_true(units::ud_are_convertible(
        units::deparse_unit(coef(nlmixr2_fit)$sigma), "mg/L"
    ))
    expect_s3_class(fitted(nlmixr2_fit)$time, "units")
    expect_s3_class(fitted(nlmixr2_fit)$value, "units")
    expect_equal(
        fitted(nlmixr2_fit)$time,
        with_units(rep(c(1, 2, 4, 6), 2) [h])
    )
    expect_true(units::ud_are_convertible(
        units::deparse_unit(fitted(nlmixr2_fit)$value), "mg/L"
    ))
})
