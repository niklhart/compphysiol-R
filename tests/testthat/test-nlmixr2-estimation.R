nlmixr2_test_problem <- function() {
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
            time = c(1, 2, 4), observable = "C", value = c(82, 67, 45)
        )),
        second = experiment(observations = observation_data(
            time = c(1, 2, 4), observable = "C", value = c(78, 61, 37)
        ))
    )
    estimation_problem(
        model, studies,
        c(
            CL_pop = parameter_spec(0.2, lower = 0.001, transform = "log"),
            omega_CL = parameter_spec(0.2, lower = 0.001, transform = "log"),
            sigma = parameter_spec(2, lower = 0.001, transform = "log")
        ),
        statistical_model(
            CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
            C = normal(sd = "sigma")
        )
    )
}

test_that("nlmixr2_engine represents the initial FOCEi scope", {
    engine <- nlmixr2_engine(control = list(print = 0L), table = list(cwres = TRUE))

    expect_s3_class(engine, "compphysiol_nlmixr2_engine")
    expect_true(is_nlmixr2_engine(engine))
    expect_identical(engine$method, "focei")
    expect_identical(engine$control$print, 0L)
    expect_error(nlmixr2_engine("saem"), "FOCEi")
    expect_error(nlmixr2_engine(control = list(1)), "named list")
})

test_that("nlmixr2 translation produces executable content and population data", {
    skip_if_not_installed("nlmixr2est")
    translated <- .nlmixr2_prepare(nlmixr2_test_problem())

    expect_match(translated$source, "cp_eta_1 ~ 4.0000000000000008e-02", fixed = TRUE)
    expect_match(translated$source, "cp_par_CL = cp_stat_CL_pop * exp(cp_eta_1)", fixed = TRUE)
    expect_match(translated$source, "cp_obs_C ~ add(cp_stat_sigma)", fixed = TRUE)
    expect_named(translated$data, c(
        "ID", "TIME", "EVID", "CMT", "AMT", "DV", "DVID", "CP_ROW"
    ))
    expect_identical(unique(translated$data$ID), 1:2)
    expect_identical(translated$data$DV, c(82, 67, 45, 78, 61, 37))
})

test_that("nlmixr2 scope rejects correlations and combined residual error", {
    problem <- nlmixr2_test_problem()
    attr(problem$statistics, "correlations") <- list(list(
        first = "CL", second = "other", value = 0
    ))
    expect_error(.nlmixr2_validate_scope(problem), "diagonal")

    problem <- nlmixr2_test_problem()
    problem$statistics$C$sd <- combined("sigma", "sigma_prop")
    expect_error(.nlmixr2_validate_scope(problem), "Combined residual")
})

test_that("nlmixr2 FOCEi estimation maps coefficients and both prediction levels", {
    skip_if_not_installed("nlmixr2est")
    fit <- estimate(
        nlmixr2_test_problem(),
        nlmixr2_engine(control = list(print = 0L, maxOuterIterations = 30L))
    )

    expect_s3_class(fit, "compphysiol_estimation_result")
    expect_s3_class(fit$engine, "compphysiol_nlmixr2_engine")
    expect_named(coef(fit), c("CL_pop", "omega_CL", "sigma"))
    expect_gt(as.numeric(coef(fit)$omega_CL), 0)
    expect_s3_class(fit$individual_predictions, "compphysiol_observation_data")
    expect_s3_class(fit$population_predictions, "compphysiol_observation_data")
    expect_equal(nrow(fit$individual_predictions), 6L)
    expect_false(anyNA(fit$individual_predictions$value))
    expect_false(anyNA(fit$population_predictions$value))
    expect_error(fitted(fit), "explicit prediction type")
    expect_identical(fitted(fit, "PRED"), fit$population_predictions)
    expect_identical(fitted(fit, "IPRED"), fit$individual_predictions)
    expect_identical(fitted(fit, "pred"), fit$population_predictions)
    expect_error(fitted(fit, "CWRES"), "PRED.*IPRED")
    expect_equal(fit$neg_log_lik, -as.numeric(logLik(fit$engine_result)))
    expect_false(any(c("convergence", "residuals", "translation") %in% names(fit)))
    expect_s3_class(fit$engine_result, "nlmixr2FitCore")
})
