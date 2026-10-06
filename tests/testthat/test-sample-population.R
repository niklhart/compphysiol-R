test_that("sample_population realizes independent individual distributions", {
    statistics <- statistical_model(
        CL = normal(
            mean = "CL_pop", sd = proportional("omega_CL"), level = "individual"
        ),
        V = lognormal(
            median = "V_pop", sdlog = "omega_V", level = "individual"
        ),
        C = normal(sd = "sigma", level = "observation")
    )
    population_parameters <- parameters(
        CL_pop = 1 [L/h], omega_CL = 0.2,
        V_pop = 10 [L], omega_V = 0.3,
        sigma = 1 [mg/L]
    )

    set.seed(123)
    population <- sample_population(statistics, population_parameters, n = 4)

    expect_s3_class(population, "ParameterSets")
    expect_named(population, paste0("individual_", 1:4))
    expect_true(all(vapply(population, inherits, logical(1), "Parameters")))
    expect_true(all(vapply(population, function(x) identical(names(x), c("CL", "V")), logical(1))))
    expect_true(all(vapply(population, function(x) inherits(x$CL, "units"), logical(1))))
    expect_true(all(vapply(population, function(x) inherits(x$V, "units"), logical(1))))
    expect_true(all(vapply(population, function(x) as.numeric(x$V) > 0, logical(1))))
    expect_false(any(c("CL_pop", "omega_CL", "V_pop", "omega_V") %in%
                     names(population[[1]])))
})

test_that("sample_population supports fixed and combined normal scales", {
    statistics <- statistical_model(
        fixed = normal(mean = 5 [L], sd = 0 [L], level = "individual"),
        combined = normal(
            mean = "mu",
            sd = combined(constant = "add", proportional = "prop"),
            level = "individual"
        )
    )
    values <- parameters(mu = 10 [L], add = 1 [L], prop = 0.1)

    set.seed(42)
    population <- sample_population(statistics, values, n = 3)

    expect_equal(
        unname(vapply(population, function(x) as.numeric(x$fixed), numeric(1))),
        rep(5, 3)
    )
    expect_true(all(vapply(population, function(x) inherits(x$combined, "units"), logical(1))))
})

test_that("sample_population validates its sampling boundary", {
    unresolved <- statistical_model(
        CL = normal(mean = "CL_pop", sd = "omega_CL")
    )
    expect_error(
        sample_population(unresolved, parameters(CL_pop = 1, omega_CL = 0.2), 2),
        "explicit or resolved levels"
    )
    selected <- sample_population(
        unresolved, parameters(CL_pop = 1, omega_CL = 0), 2, targets = "CL"
    )
    expect_named(selected[[1]], "CL")
    expect_equal(selected[[1]]$CL, 1)
    expect_error(
        sample_population(unresolved, parameters(CL_pop = 1, omega_CL = 0), 2,
                          targets = "unknown"),
        "Unknown statistical-model target"
    )
    expect_error(
        sample_population(unresolved, parameters(CL_pop = 1, omega_CL = 0), 2,
                          targets = c("CL", "CL")),
        "duplicate"
    )
    expect_error(
        sample_population(
            statistical_model(C = normal(sd = 1, level = "observation")),
            parameters(), 2, targets = "C"
        ),
        "Observation-level"
    )
    expect_error(
        sample_population(
            statistical_model(CL = normal(mean = 1, sd = -1, level = "individual")),
            parameters(), 2
        ),
        "non-negative"
    )
    expect_error(
        sample_population(
            statistical_model(CL = lognormal(
                median = 1, sdlog = -0.1, level = "individual"
            )),
            parameters(), 2
        ),
        "non-negative"
    )
    expect_error(
        sample_population(
            statistical_model(CL = normal(mean = 1, sd = 1, level = "individual")),
            parameters(), 0
        ),
        "positive whole number"
    )
})

test_that("explicit targets select exactly the requested unresolved entries", {
    statistics <- statistical_model(
        CL = normal(mean = "CL_pop", sd = 0),
        V = lognormal(median = "V_pop", sdlog = 0),
        C = normal(sd = "sigma")
    )
    values <- parameters(CL_pop = 1, V_pop = 10, sigma = 2)

    population <- sample_population(statistics, values, n = 2, targets = c("V", "CL"))

    expect_named(population[[1]], c("V", "CL"))
    expect_equal(population[[1]]$V, 10)
    expect_equal(population[[1]]$CL, 1)
    expect_false("C" %in% names(population[[1]]))
})

test_that("sample_population returns empty parameter sets for observation-only models", {
    statistics <- statistical_model(
        C = normal(sd = "sigma", level = "observation")
    )

    population <- sample_population(statistics, parameters(sigma = 1), n = 2)

    expect_named(population, c("individual_1", "individual_2"))
    expect_true(all(lengths(population) == 0L))
})
