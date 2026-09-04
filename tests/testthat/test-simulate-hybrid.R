birth_death_model <- compartment_model() |>
    add_compartment("cyt", volume = 1) |>
    add_molecule("A", cmt = "cyt", initial = 20, type = "amount") |>
    add_reaction(input = NULL, output = "A", cmt = "cyt", const = "ksyn") |>
    add_reaction(input = "A", output = NULL, cmt = "cyt", const = "kdeg") |>
    add_parameter(ksyn = 2, kdeg = 0.1)

test_that("hybrid simulation returns requested times by default", {
    time <- 0:5

    out <- simulate(
        birth_death_model,
        time = time,
        simulation_type = "hybrid",
        partition = c(TRUE, FALSE),
        seed = 1
    )

    expect_s3_class(out, "SimulationResult")
    expect_named(out, c("states", "observables"))
    expect_named(out$states, c("time", "a_A_cyt"))
    expect_equal(out$states$time, time)
    expect_null(out$observables)
})

test_that("hybrid simulation accepts a precompiled StochasticModel", {
    stochastic_model <- to_stochastic_model(birth_death_model)
    time <- 0:3

    out <- simulate(
        stochastic_model,
        time = time,
        simulation_type = "hybrid",
        partition = c(TRUE, FALSE),
        seed = 1
    )

    expect_s3_class(out, "SimulationResult")
    expect_equal(out$states$time, time)
})

test_that("hybrid deterministic partition matches deterministic solution", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2)
    time <- seq(0, 5, by = 1)

    out <- simulate(model, time = time, simulation_type = "hybrid", partition = FALSE, seed = 1)

    expect_equal(out$states$time, time)
    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * time), tolerance = 1e-5)
})

test_that("hybrid deterministic partition supports large initial counts", {
    initial_count <- .Machine$integer.max + 1000
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = initial_count, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2)
    time <- c(0, 1)

    out <- simulate(model, time = time, simulation_type = "hybrid", partition = FALSE)

    expect_true(all(is.finite(out$states$a_drug_Central)))
    expect_equal(out$states$a_drug_Central[[1]], initial_count)
    expect_equal(out$states$a_drug_Central[[2]], initial_count * exp(-0.2), tolerance = 1e-5)
})

test_that("hybrid adaptive partition and fixed all-stochastic partition are reproducible", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 20, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2)
    time <- 0:5

    fixed_first <- simulate(model, time = time, simulation_type = "hybrid", partition = TRUE, seed = 42)
    fixed_second <- simulate(model, time = time, simulation_type = "hybrid", partition = TRUE, seed = 42)
    adaptive <- simulate(model, time = time, simulation_type = "hybrid", partition = 100, seed = 42)

    expect_equal(fixed_second, fixed_first)
    expect_equal(adaptive, fixed_first)
    expect_true(all(fixed_first$states$a_drug_Central == round(fixed_first$states$a_drug_Central)))
})

test_that("hybrid simulation supports unit-aware time and fixed partitioning", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])
    time <- units::set_units(seq(0, 5, by = 1), "h", mode = "standard")

    out <- simulate(model, time = time, simulation_type = "hybrid", partition = FALSE, seed = 1)

    expect_equal(out$states$time, time)
    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * seq(0, 5, by = 1)), tolerance = 1e-5)
})

test_that("hybrid adaptive partition accepts unit-aware inverse-time thresholds", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 20, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])
    time <- units::set_units(seq(0, 5, by = 1), "h", mode = "standard")

    fixed <- simulate(model, time = time, simulation_type = "hybrid", partition = TRUE, seed = 42)
    adaptive <- simulate(model, time = time, simulation_type = "hybrid", partition = 100 [1/h], seed = 42)

    expect_equal(adaptive, fixed)
})

test_that("hybrid adaptive partition rejects incompatible threshold units", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 20, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])
    time <- units::set_units(seq(0, 5, by = 1), "h", mode = "standard")

    expect_error(
        simulate(model, time = time, simulation_type = "hybrid", partition = 1 [h]),
        "inverse time",
        ignore.case = TRUE
    )
})

test_that("hybrid event times are optional", {
    time <- c(0, 5)

    requested_only <- simulate(
        birth_death_model,
        time = time,
        simulation_type = "hybrid",
        partition = c(TRUE, FALSE),
        seed = 1
    )
    with_events <- simulate(
        birth_death_model,
        time = time,
        simulation_type = "hybrid",
        partition = c(TRUE, FALSE),
        seed = 1,
        include_event_times = TRUE
    )

    expect_equal(requested_only$states$time, time)
    expect_true(nrow(with_events$states) > length(time))
    expect_true(all(time %in% with_events$states$time))
})

test_that("hybrid simulation evaluates observables and supports multiple realizations", {
    model <- birth_death_model |>
        add_observable(Aobs = a[A, cyt])
    time <- 0:2

    out <- simulate(
        model,
        time = time,
        simulation_type = "hybrid",
        partition = c(TRUE, FALSE),
        nsim = 2,
        seed = 1
    )

    expect_named(out$states, c("time", "rep", "a_A_cyt"))
    expect_named(out$observables, c("time", "rep", "Aobs"))
    expect_equal(out$states$time, rep(time, times = 2))
    expect_equal(out$observables$Aobs, out$states$a_A_cyt)
})

test_that("hybrid simulation rejects invalid partitioning", {
    stochastic_model <- to_stochastic_model(birth_death_model)

    expect_error(
        simulate(stochastic_model, time = 0:1, simulation_type = "hybrid"),
        "partition.*required",
        ignore.case = TRUE
    )
    expect_error(
        simulate(stochastic_model, time = 0:1, simulation_type = "hybrid", partition = c(TRUE, FALSE, TRUE)),
        "one value per reaction",
        ignore.case = TRUE
    )
    expect_error(
        simulate(stochastic_model, time = 0:1, simulation_type = "hybrid", partition = -1),
        "non-negative",
        ignore.case = TRUE
    )
})
