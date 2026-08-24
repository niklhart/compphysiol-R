test_model_for_simulation <- function(amount_unit = NULL, time_unit = FALSE) {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, unit = amount_unit, type = "amount") |>
        add_transport("Central", "", const = "ke")

    if (time_unit) {
        add_parameter(model, ke = 0.2 [1/h])
    } else {
        add_parameter(model, ke = 0.2)
    }
}

test_that("simulate returns a SimulationResult with ODE states", {
    model <- test_model_for_simulation()

    out <- simulate(model, time = seq(0, 10, by = 1))

    expect_s3_class(out, "SimulationResult")
    expect_named(out, c("states"))
    expect_s3_class(out$states, "data.frame")
    expect_named(out$states, c("time", "a_drug_Central"))
    expect_equal(out$states$time, seq(0, 10, by = 1))
    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate accepts time units through the time DSL", {
    model <- test_model_for_simulation(amount_unit = "mg", time_unit = TRUE)

    out <- simulate(model, time = seq(0, 10, by = 1) [h])

    expect_s3_class(out, "SimulationResult")
    expect_s3_class(out$states, "data.frame")
    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)), "mg", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate accepts time units through the unit argument", {
    model <- test_model_for_simulation(amount_unit = "mg", time_unit = TRUE)

    out <- simulate(model, time = seq(0, 10, by = 1), unit = "h")

    expect_s3_class(out, "SimulationResult")
    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)), "mg", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate errors when model uses time units but simulation time is unit-free", {
    model <- test_model_for_simulation(amount_unit = "mg", time_unit = TRUE)

    expect_error(
        simulate(model, time = seq(0, 10, by = 1)),
        "model uses time units but simulation time is unit-free"
    )
})

test_that("simulate errors when simulation time has units but model is unit-free in time", {
    model <- test_model_for_simulation()

    expect_error(
        simulate(model, time = seq(0, 10, by = 1) [h]),
        "simulation time has units but the model is unit-free in time"
    )
})

test_that("simulate can pass free parameters to the ODE solver", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", const = "ke")

    out <- simulate(model, time = seq(0, 10, by = 1), parameters = list(ke = 0.2))

    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate can pass free parameters as a Parameters object", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", const = "ke")

    out <- simulate(
        model,
        time = seq(0, 10, by = 1),
        parameters = parameters(ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate can pass unit-aware free parameters as a Parameters object", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, unit = "mg", type = "amount") |>
        add_transport("Central", "", const = "ke")

    out <- simulate(
        model,
        time = seq(0, 10, by = 1) [h],
        parameters = parameters(ke = 0.2 [1/h])
    )

    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)), "mg", mode = "standard"),
        tolerance = 1e-6
    )
})
