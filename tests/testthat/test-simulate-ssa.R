ssa_death_model <- compartment_model() |>
    add_compartment("Central", volume = NA_real_) |>
    add_molecule("drug", cmt = "Central", initial = 20, type = "amount") |>
    add_transport("Central", "", const = "ke") |>
    add_parameter(ke = 0.2)

test_that("simulate with SSA returns a SimulationResult at requested times", {
    time <- seq(0, 10, by = 1)

    out <- simulate(ssa_death_model, time = time, simulation_type = "ssa", seed = 1)

    expect_s3_class(out, "SimulationResult")
    expect_named(out, c("states", "observables"))
    expect_s3_class(out$states, "data.frame")
    expect_null(out$observables)
    expect_named(out$states, c("time", "a_drug_Central"))
    expect_equal(out$states$time, time)
})

test_that("simulate accepts a precompiled StochasticModel", {
    stochastic_model <- to_stochastic_model(ssa_death_model)
    time <- seq(0, 10, by = 1)

    out <- simulate(stochastic_model, time = time, seed = 1)

    expect_s3_class(out, "SimulationResult")
    expect_equal(out$states$time, time)
})

test_that("SSA output states are non-negative integer counts", {
    out <- simulate(ssa_death_model, time = seq(0, 10, by = 1), simulation_type = "ssa", seed = 1)
    counts <- out$states$a_drug_Central

    expect_true(all(is.finite(counts)))
    expect_true(all(counts >= 0))
    expect_true(all(counts == round(counts)))
})

test_that("SSA normalizes dimensionless unit initials to numeric counts", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 20 [1], type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2)

    out <- simulate(model, time = 0:1, simulation_type = "ssa", seed = 1)

    expect_false(inherits(out$states$a_drug_Central, "units"))
    expect_equal(out$states$a_drug_Central[[1]], 20)
})

test_that("SSA rejects initial counts above the integer limit", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = .Machine$integer.max + 1, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2)

    expect_error(
        simulate(model, time = 0:1, simulation_type = "ssa"),
        "a\\[drug, Central\\].*maximum supported integer count.*SSA",
        ignore.case = TRUE
    )
})

test_that("SSA rejects negative propensities", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 10, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = -0.2)

    expect_error(
        simulate(model, time = 0:1, simulation_type = "ssa"),
        "negative value",
        ignore.case = TRUE
    )
})

test_that("SSA simulation is reproducible with seed", {
    time <- seq(0, 10, by = 1)

    first <- simulate(ssa_death_model, time = time, simulation_type = "ssa", seed = 42)
    second <- simulate(ssa_death_model, time = time, simulation_type = "ssa", seed = 42)

    expect_equal(second, first)
})

test_that("SSA event times are optional", {
    time <- c(0, 10)

    requested_only <- simulate(ssa_death_model, time = time, simulation_type = "ssa", seed = 1)
    with_events <- simulate(
        ssa_death_model,
        time = time,
        simulation_type = "ssa",
        seed = 1,
        include_event_times = TRUE
    )

    expect_equal(requested_only$states$time, time)
    expect_true(nrow(with_events$states) > length(time))
    expect_true(all(time %in% with_events$states$time))
})

test_that("SSA simulation can limit stochastic events", {
    expect_error(
        simulate(ssa_death_model, time = c(0, 10), simulation_type = "ssa", seed = 1, max_events = 0),
        "max_events",
        ignore.case = TRUE
    )
    expect_error(
        simulate(ssa_death_model, time = c(0, 10), simulation_type = "ssa", seed = 1, max_events = 1.5),
        "non-negative integer scalar or Inf",
        ignore.case = TRUE
    )
})

test_that("SSA source and linear sink reactions approach Poisson counts", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = 1) |>
        add_molecule("A", cmt = "cyt", initial = 0, type = "amount") |>
        add_reaction(input = NULL, output = "A", cmt = "cyt", const = "ksyn") |>
        add_reaction(input = "A", output = NULL, cmt = "cyt", const = "kdeg") |>
        add_parameter(ksyn = 2, kdeg = 1)
    stochastic_model <- to_stochastic_model(model)
    n <- 1500
    out <- simulate(stochastic_model, time = c(0, 10), nsim = n, seed = 1)
    final_counts <- out$states$a_A_cyt[out$states$time == 10]

    lambda <- 2 * (1 - exp(-10))
    bins <- 0:5
    observed <- c(
        tabulate(factor(final_counts[final_counts <= 5], levels = bins), nbins = length(bins)),
        sum(final_counts >= 6)
    )
    expected <- c(
        stats::dpois(bins, lambda),
        stats::ppois(max(bins), lambda, lower.tail = FALSE)
    ) * n
    pearson_statistic <- sum((observed - expected)^2 / expected)

    expect_equal(mean(final_counts), lambda, tolerance = 0.12)
    expect_equal(stats::var(final_counts), lambda, tolerance = 0.18)
    expect_lt(pearson_statistic, stats::qchisq(0.999, df = length(observed) - 1))
})

test_that("SSA simulation supports multiple realizations in long format", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 5, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(Aobs = a[drug, Central]) |>
        add_parameter(ke = 0.2)
    time <- 0:2

    expect_no_error(simulate(model, time = 0:2, simulation_type = "ssa", nsim = NULL))
    expect_no_error(simulate(model, time = 0:2, simulation_type = "ssa", nsim = 1))
    out <- simulate(model, time = time, simulation_type = "ssa", nsim = 3, seed = 1)
    repeat_out <- simulate(model, time = time, simulation_type = "ssa", nsim = 3, seed = 1)

    expect_named(out$states, c("time", "rep", "a_drug_Central"))
    expect_named(out$observables, c("time", "rep", "Aobs"))
    expect_equal(out$states$time, rep(time, times = 3))
    expect_equal(out$states$rep, rep(1:3, each = length(time)))
    expect_equal(out$observables$rep, out$states$rep)
    expect_equal(out$observables$Aobs, out$states$a_drug_Central)
    expect_equal(repeat_out, out)
})

test_that("SSA simulation rejects invalid nsim values", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 5, type = "amount")

    expect_error(
        simulate(model, time = 0:2, simulation_type = "ssa", nsim = 1.5),
        "nsim|positive integer",
        ignore.case = TRUE
    )
})

test_that("SSA simulation evaluates observables at requested output times", {
    model <- ssa_death_model |>
        add_observable(Aobs = a[drug, Central])
    time <- seq(0, 10, by = 1)

    out <- simulate(model, time = time, simulation_type = "ssa", seed = 1)

    expect_s3_class(out$observables, "data.frame")
    expect_equal(out$observables$time, time)
    expect_equal(out$observables$Aobs, out$states$a_drug_Central)
})

test_that("SSA simulation supports unit-aware time and inverse-time rate constants", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 20, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])
    time <- units::set_units(seq(0, 10, by = 1), "h", mode = "standard")

    out <- simulate(model, time = time, simulation_type = "ssa", seed = 1)

    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_false(inherits(out$states$a_drug_Central, "units"))
    expect_true(all(out$states$a_drug_Central >= 0))
    expect_true(all(out$states$a_drug_Central == round(out$states$a_drug_Central)))
})

test_that("SSA simulation rejects time-unit mismatches", {
    unit_model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 20, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])
    expect_error(
        simulate(unit_model, time = seq(0, 10, by = 1), simulation_type = "ssa"),
        "model uses time units but simulation time is unit-free"
    )
    expect_error(
        simulate(ssa_death_model, time = seq(0, 10, by = 1) [h], simulation_type = "ssa"),
        "simulation time has units but the model is unit-free in time"
    )
})

test_that("SSA repeated-input propensities are zero below required counts", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = "V") |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(1, 0), type = "amount") |>
        add_reaction(input = c("A", "A"), output = "B", cmt = "cyt", const = "k") |>
        add_parameter(V = 1, k = 1e9)

    out <- simulate(model, time = c(0, 1), simulation_type = "ssa", seed = 1)

    expect_equal(out$states$a_A_cyt, c(1, 1))
    expect_equal(out$states$a_B_cyt, c(0, 0))
})

test_that("SSA stops cleanly when all propensities are zero", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 5, type = "amount")
    time <- 0:3

    out <- simulate(model, time = time, simulation_type = "ssa", seed = 1)

    expect_equal(out$states$time, time)
    expect_equal(out$states$a_drug_Central, c(5, 5, 5, 5))
    expect_null(out$observables)
})
