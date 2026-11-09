test_that("logitnormal constructors normalize values and enforce fixed invariants", {
    d <- logitnormal(median = 5 [L], sdlogit = 0.4, lower = 2000 [mL], upper = 8 [L])
    expect_true(is_logitnormal_distribution(d))
    expect_true(is_statistical_distribution(d))
    expect_equal(d$median, with_units(5 [L]))
    expect_equal(d$lower, with_units(2000 [mL]))
    expect_error(logitnormal(), "sdlogit")
    expect_error(logitnormal(sdlogit = -1), "non-negative")
    expect_error(logitnormal(sdlogit = 1 [L]), "dimensionless")
    expect_error(logitnormal(sdlogit = 1, lower = 1, upper = 0), "lower")
    expect_error(logitnormal(median = 1, sdlogit = 1), "strictly between")
    expect_error(logitnormal(median = 5 [L], sdlogit = 1), "unit")
    expect_error(logitnormal(sdlogit = 1, lower = -Inf), "finite")
    expect_error(logitnormal(sdlogit = c(1, 2)), "scalar")
    expect_output(print(statistical_model(F = logitnormal(sdlogit = "s"))),
                  "logitnormal.*sdlogit = s; lower = 0; upper = 1")
})

test_that("bounded sampling preserves units and is invariant to unit conversion", {
    d <- logitnormal(median = 5 [L], sdlogit = "s", lower = "lo", upper = "hi")
    pars <- parameters(s = 0.4, lo = 2000 [mL], hi = 8 [L])
    latent <- c(-2, 0, 2)
    draws <- .sample_individual_distribution(d, pars, 3, latent)
    expect_equal(draws, with_units((2 + 6 * plogis(0.4 * latent)) [L]))
    d$median <- with_units(5000 [mL])
    expect_equal(units::set_units(.sample_individual_distribution(d, pars, 3, latent), "L"), draws)
    expect_error(.sample_individual_distribution(d, parameters(s = 0.4, lo = 6 [L], hi = 8 [L]), 3),
                 "strictly between")
    d$lower <- 0
    expect_error(.sample_individual_distribution(d, pars, 3), "unit")
})

test_that("population correlations act on the latent normal scale", {
    statistics <- statistical_model(
        F = logitnormal(median = 0.8, sdlogit = 0.5, level = "individual"),
        G = normal(mean = 0, sd = 1, level = "individual"),
        correlated(list("F", "G", 1))
    )
    set.seed(7)
    population <- sample_population(statistics, parameters(), 20)
    f <- vapply(population, function(x) x$F, numeric(1))
    g <- vapply(population, function(x) x$G, numeric(1))
    expect_equal((qlogis(f) - qlogis(0.8)) / 0.5, g)
    expect_true(all(f > 0 & f < 1))
})

test_that("observation sampling uses predictions as bounded conditional medians", {
    predictions <- observation_data(time = c(1, 2), observable = "V", value = c(4, 5) [L])
    statistics <- statistical_model(V = logitnormal(sdlogit = "s", lower = 2 [L], upper = 8 [L]))
    exact <- sample_observations(predictions, statistics, parameters(s = 0))
    expect_equal(exact, predictions)
    set.seed(12)
    expected <- 2 + 6 * plogis(qlogis((c(4, 5) - 2) / 6) + 0.4 * rnorm(2))
    set.seed(12)
    sampled <- sample_observations(predictions, statistics, parameters(s = 0.4))
    expect_equal(as.numeric(sampled$value), expected)
    predictions$value <- with_units(c(2, 8) [L])
    expect_error(sample_observations(predictions, statistics, parameters(s = 0.4)), "strictly between")
})

test_that("logitnormal likelihood includes the Jacobian and rejects endpoints", {
    d <- logitnormal(median = 5 [L], sdlogit = "s", lower = 2 [L], upper = 8 [L])
    x <- 4
    z <- log((x - 2) / (8 - x))
    expected <- -dnorm(z, 0, 0.4, log = TRUE) + log((x - 2) * (8 - x) / 6)
    nll <- function(obs, median = with_units(5 [L]), s = 0.4) {
        .estimation_distribution_nll(obs, median, d, parameters(s = s))$nll
    }
    expect_equal(nll(with_units(4 [L])), expected)
    expect_equal(nll(with_units(4000 [mL])), expected)
    for (x in c(1, 2, 8, 9)) expect_equal(nll(with_units(x [L])), Inf)
    expect_equal(nll(with_units(4 [L]), s = 0), Inf)
    # The density integrates to one on the original physical scale.
    density <- function(x) vapply(x, function(v) exp(-nll(with_units(v [L]))), numeric(1))
    expect_equal(integrate(density, 2, 8)$value, 1, tolerance = 1e-7)
    expect_equal(.estimation_statistical_parameter_names(statistical_model(
        F = logitnormal(median = "m", sdlogit = "s", lower = "lo", upper = "hi")
    )), c("m", "s", "lo", "hi"))
    expect_error(.nlmixr2_validate_scope(list(statistics = statistical_model(F = d))), "logitnormal")
})

test_that("statistical resolution checks bounds and units without treating unit probes as medians", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 1 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = 5 [mg], type = "amount") |>
        add_observable(C = c[drug, Central])
    stats <- statistical_model(C = logitnormal(sdlogit = 0.4, lower = 2 [mg/L], upper = 8 [mg/L]))
    resolved <- .resolve_statistical_model(stats, model, parameters())
    expect_s3_class(resolved$C$median, "compphysiol_prediction_location")
    stats$C$lower <- 0
    expect_error(.resolve_statistical_model(stats, model, parameters()), "unit")
    stats$C$lower <- with_units(9 [mg/L])
    expect_error(.resolve_statistical_model(stats, model, parameters()), "lower")
})

test_that("optim estimates logitnormal spread with fixed physical bounds", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 1 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = 5 [mg], type = "amount") |>
        add_transport(from = "Central", to = NULL, molec = "drug", const = "k") |>
        add_parameter(k = 0.2 [1/h]) |>
        add_observable(C = c[drug, Central])
    times <- c(1, 2, 3)
    median <- 5 * exp(-0.2 * times)
    errors <- c(-0.2, 0.4, -0.6)
    observed <- 8 * plogis(qlogis(median / 8) + errors)
    study <- experiment(start = 0 [h], observations = observation_data(
        time = times [h], observable = "C", value = observed [mg/L]
    ))
    problem <- estimation_problem(model, study,
        c(s = parameter_spec(0.3, lower = 0.01, transform = "log")),
        statistical_model(C = logitnormal(sdlogit = "s", lower = 0 [mg/L], upper = 8 [mg/L]))
    )
    fit <- estimate(problem, engine = optim_engine())
    expect_equal(as.numeric(coef(fit)$s), sqrt(mean(errors^2)), tolerance = 1e-4)
    expect_equal(.estimation_statistical_parameter_names(statistical_model(
        F = logitnormal(median = "m", sdlogit = "s")
    )), c("m", "s"))
})
