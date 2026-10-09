statistical_test_model <- function(fixed_cl = FALSE, observable_cl = FALSE) {
    model <- compartment_model() |>
        add_compartment("Central", volume = 1 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", NULL, molec = "drug", rate = "CL * c[drug, Central]") |>
        add_observable(C = c[drug, Central])
    if (fixed_cl) model <- add_parameter(model, CL = 1 [L/h])
    if (observable_cl) model <- add_observable(model, CL = c[drug, Central])
    model
}

test_that("distribution constructors represent the unified vocabulary", {
    constant <- normal(mean = "CL_pop", sd = "omega_CL")
    prop <- normal(mean = "CL_pop", sd = proportional("omega_CL"))
    combo <- normal(sd = combined(
        constant = "sigma_add", proportional = "sigma_prop"
    ))
    log_dist <- lognormal(median = "V_pop", sdlog = "omega_V")

    expect_s3_class(constant, "NormalDistribution")
    expect_identical(constant$mean, "CL_pop")
    expect_identical(constant$sd, "omega_CL")
    expect_null(constant$level)
    expect_s3_class(prop$sd, "ProportionalSD")
    expect_identical(prop$sd$coefficient, "omega_CL")
    expect_s3_class(combo$sd, "CombinedSD")
    expect_identical(combo$sd$constant, "sigma_add")
    expect_identical(combo$sd$proportional, "sigma_prop")
    expect_s3_class(log_dist, "LognormalDistribution")
    expect_identical(log_dist$median, "V_pop")
    expect_identical(log_dist$sdlog, "omega_V")
})

test_that("distribution constructors reject ambiguous specifications", {
    expect_error(normal(), "sd")
    expect_error(normal(sd = c(1, 2)), "name|scalar")
    expect_error(normal(sd = ""), "name|scalar")
    expect_error(normal(sd = "sigma", level = "population"), "level")
    expect_error(proportional(""), "name|scalar")
    expect_error(combined("sigma", "sigma"), "distinct")
    expect_error(lognormal(), "sdlog")
    expect_error(lognormal(sdlog = c(0.1, 0.2)), "name|scalar")
    expect_error(lognormal(mean = "V_pop", sdlog = "omega"), "unused argument")
    expect_error(lognormal(logmean = "log_V", sdlog = "omega"), "unused argument")
})

test_that("distribution constructors accept fixed scalar specifications", {
    fixed <- normal(mean = 2 [L/h], sd = 0.5 [L/h])
    scaled <- normal(
        mean = 2 [L/h],
        sd = combined(constant = 0.1 [L/h], proportional = 0.2)
    )
    log_dist <- lognormal(median = 10 [L], sdlog = 0.3)

    expect_equal(fixed$mean, with_units(2 [L/h]))
    expect_equal(fixed$sd, with_units(0.5 [L/h]))
    expect_equal(scaled$sd$constant, with_units(0.1 [L/h]))
    expect_identical(scaled$sd$proportional, 0.2)
    expect_equal(log_dist$median, with_units(10 [L]))
    expect_identical(log_dist$sdlog, 0.3)
    expect_s3_class(proportional(0.2), "ProportionalSD")
})

test_that("statistical_model uses names as random-quantity targets", {
    model <- statistical_model(
        CL = normal(mean = "CL_pop", sd = "omega_CL"),
        V = lognormal(median = "V_pop", sdlog = "omega_V"),
        C = normal(sd = "sigma")
    )

    expect_s3_class(model, "StatisticalModel")
    expect_named(model, c("CL", "V", "C"))
    expect_s3_class(model["CL"], "StatisticalModel")
    expect_s3_class(model[["CL"]], "NormalDistribution")
    expect_error(statistical_model(normal(sd = "sigma")), "named|random quantity")
    expect_error(
        statistical_model(C = normal(sd = "a"), C = normal(sd = "b")),
        "unique"
    )
    expect_error(statistical_model(C = 1), "StatisticalDistribution")
})

test_that("correlated declares pairwise dependence between model targets", {
    dependence <- correlated(
        list("CL", "V", 0.5),
        list("CL", "Q", "rho_CL_Q")
    )
    model <- statistical_model(
        CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
        V = lognormal(median = "V_pop", sdlog = "omega_V"),
        Q = normal(mean = "Q_pop", sd = "omega_Q"),
        dependence
    )

    expect_s3_class(dependence, "Correlated")
    expect_named(model, c("CL", "V", "Q"))
    expect_length(.statistical_correlations(model), 2)
    expect_length(.statistical_correlations(model[c("CL", "V")]), 1)
    expect_length(.statistical_correlations(model["V"]), 0)
    expect_output(print(model), "Correlations (latent normal scale)", fixed = TRUE)
    expect_output(print(model), "rho_CL_Q", fixed = TRUE)
    expect_output(print(model), "0.5", fixed = TRUE)
})

test_that("correlated validates triplets and enclosing targets", {
    expect_error(correlated(list("CL", "V")), "list\\(first_target")
    expect_error(correlated(pair = list("CL", "V", 0.5)), "unnamed")
    expect_error(correlated(list("CL", "CL", 0.5)), "distinct")
    expect_error(correlated(list("CL", "V", 1.1)), "between -1 and 1")
    expect_error(
        correlated(list("CL", "V", 0.5), list("V", "CL", 0.4)),
        "only once"
    )
    expect_error(
        statistical_model(
            CL = lognormal(median = 1, sdlog = 0.2),
            correlated(list("CL", "V", 0.5))
        ),
        "Unknown correlated.*V"
    )
    expect_error(
        statistical_model(
            CL = lognormal(median = 1, sdlog = 0.2),
            C = normal(sd = 1, level = "observation"),
            correlated(list("CL", "C", 0.5))
        ),
        "only between individual-level"
    )
})

test_that("correlation level and marginal restrictions are resolved contextually", {
    dynamic <- statistical_test_model()
    observation_correlation <- statistical_model(
        CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
        C = normal(sd = "sigma"),
        correlated(list("CL", "C", 0.5))
    )
    expect_error(
        .resolve_statistical_model(observation_correlation, dynamic),
        "observation-level.*C"
    )

    unsupported_scale <- statistical_model(
        CL = normal(mean = "CL_pop", sd = proportional("omega_CL"),
                    level = "individual"),
        V = lognormal(median = "V_pop", sdlog = "omega_V", level = "individual"),
        correlated(list("CL", "V", 0.5))
    )
    expect_error(
        sample_population(
            unsupported_scale,
            parameters(CL_pop = 1, omega_CL = 0.2, V_pop = 2, omega_V = 0.3),
            n = 2
        ),
        "constant standard deviation"
    )
})

test_that("validation infers levels and observable prediction locations", {
    dynamic <- statistical_test_model()
    model <- statistical_model(
        CL = normal(mean = "CL_pop", sd = proportional("omega_CL")),
        C = lognormal(sdlog = "sigma")
    )

    resolved <- .resolve_statistical_model(model, dynamic)

    expect_identical(resolved$CL$level, "individual")
    expect_identical(resolved$CL$mean, "CL_pop")
    expect_identical(resolved$C$level, "observation")
    expect_s3_class(resolved$C$median, "PredictionLocation")
    expect_output(
        print(resolved),
        "CL: normal; location = CL_pop; sd = proportional(omega_CL)",
        fixed = TRUE
    )
    expect_output(
        print(resolved),
        "C: lognormal; location = prediction; sdlog = sigma",
        fixed = TRUE
    )
})

test_that("statistical model printing expands combined and fixed scales", {
    model <- statistical_model(
        C = normal(sd = combined(constant = 1 [mg/L], proportional = 0.1)),
        biomarker = lognormal(median = 2 [mg/L], sdlog = 0.2)
    )

    expect_output(
        print(model),
        "sd = combined(constant = 1 [mg/L], proportional = 0.1)",
        fixed = TRUE
    )
    expect_output(print(model), "sdlog = 0.2", fixed = TRUE)
})

test_that("validation rejects unresolved and ambiguous targets", {
    dynamic <- statistical_test_model()

    expect_error(
        .resolve_statistical_model(statistical_model(CL = normal(sd = "omega")), dynamic),
        "requires an explicit mean"
    )
    expect_error(
        .resolve_statistical_model(
            statistical_model(CL = lognormal(sdlog = "omega")), dynamic
        ),
        "requires an explicit median"
    )
    expect_error(
        .resolve_statistical_model(
            statistical_model(unknown = normal(mean = "mu", sd = "sigma")), dynamic
        ),
        "Unknown.*unknown"
    )
    expect_error(
        .resolve_statistical_model(
            statistical_model(C = normal(mean = "mu", sd = "sigma", level = "individual")),
            dynamic
        ),
        "not a structural parameter"
    )
    expect_error(
        .resolve_statistical_model(
            statistical_model(CL = normal(mean = "mu", sd = "sigma")),
            statistical_test_model(fixed_cl = TRUE)
        ),
        "unresolved/free"
    )

    both <- statistical_test_model(observable_cl = TRUE)
    ambiguous <- statistical_model(CL = normal(mean = "mu", sd = "sigma"))
    expect_error(.resolve_statistical_model(ambiguous, both), "both.*explicitly")
    expect_identical(
        .resolve_statistical_model(
            statistical_model(CL = normal(sd = "sigma", level = "observation")), both
        )$CL$level,
        "observation"
    )
})

test_that("validation checks distribution parameter units", {
    dynamic <- statistical_test_model()
    model <- statistical_model(
        CL = normal(mean = "CL_pop", sd = proportional("omega_CL")),
        C = normal(sd = combined(
            constant = "sigma_add", proportional = "sigma_prop"
        ))
    )
    values <- parameters(
        CL_pop = 1 [L/h],
        omega_CL = 0.2,
        sigma_add = 1 [mg/L],
        sigma_prop = 0.1
    )

    expect_no_error(.resolve_statistical_model(model, dynamic, values))
    expect_error(
        .resolve_statistical_model(model, dynamic, c(values[setdiff(names(values), "CL_pop")],
            parameters(CL_pop = 1 [kg]))),
        "unit|units|right-hand side"
    )
    expect_error(
        .resolve_statistical_model(model, dynamic, c(values[setdiff(names(values), "omega_CL")],
            parameters(omega_CL = 0.2 [kg]))),
        "dimensionless"
    )
    expect_error(
        .resolve_statistical_model(model, dynamic, c(values[setdiff(names(values), "sigma_add")],
            parameters(sigma_add = 1 [kg]))),
        "units"
    )
    expect_error(
        .resolve_statistical_model(model, dynamic, values[setdiff(names(values), "sigma_prop")]),
        "Missing statistical parameter: sigma_prop"
    )
})

test_that("validation supports fully fixed distribution parameters", {
    dynamic <- statistical_test_model()
    model <- statistical_model(
        CL = lognormal(median = 1 [L/h], sdlog = 0.2),
        C = normal(sd = 1 [mg/L])
    )

    expect_no_error(.resolve_statistical_model(model, dynamic, parameters()))
    expect_error(
        .resolve_statistical_model(
            statistical_model(
                CL = lognormal(median = 1 [L/h], sdlog = 0.2),
                C = normal(sd = 1)
            ),
            dynamic,
            parameters()
        ),
        "units"
    )
})

test_that("log-normal median uses target units and sdlog is dimensionless", {
    dynamic <- statistical_test_model()
    model <- statistical_model(
        CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
        C = lognormal(sdlog = "sigma")
    )
    values <- parameters(CL_pop = 1 [L/h], omega_CL = 0.2, sigma = 0.1)

    expect_no_error(.resolve_statistical_model(model, dynamic, values))
    expect_error(
        .resolve_statistical_model(
            model, dynamic,
            parameters(CL_pop = 1 [L/h], omega_CL = 0.2 [kg], sigma = 0.1)
        ),
        "dimensionless"
    )
    expect_error(
        .resolve_statistical_model(
            model, dynamic,
            parameters(CL_pop = 1 [L/h], omega_CL = 0.2, sigma = 0.1 [mg/L])
        ),
        "dimensionless"
    )
})
