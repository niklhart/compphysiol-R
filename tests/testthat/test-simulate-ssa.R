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

test_that("SSA simulation is reproducible with seed", {
    time <- seq(0, 10, by = 1)

    first <- simulate(ssa_death_model, time = time, simulation_type = "ssa", seed = 42)
    second <- simulate(ssa_death_model, time = time, simulation_type = "ssa", seed = 42)

    expect_equal(second, first)
})

test_that("SSA simulation supports only one realization in V1", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 5, type = "amount")

    expect_no_error(simulate(model, time = 0:2, simulation_type = "ssa", nsim = NULL))
    expect_no_error(simulate(model, time = 0:2, simulation_type = "ssa", nsim = 1))
    expect_error(
        simulate(model, time = 0:2, simulation_type = "ssa", nsim = 2),
        "nsim|one realization|not implemented",
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
