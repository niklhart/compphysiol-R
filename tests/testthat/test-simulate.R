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
    expect_named(out, c("states", "observables"))
    expect_s3_class(out$states, "data.frame")
    expect_null(out$observables)
    expect_named(out$states, c("time", "a_drug_Central"))
    expect_equal(out$states$time, seq(0, 10, by = 1))
    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate accepts an explicit deterministic simulation type", {
    model <- test_model_for_simulation()
    time <- seq(0, 10, by = 1)

    default <- simulate(model, time = time)
    deterministic <- simulate(model, time = time, simulation_type = "deterministic")

    expect_equal(deterministic, default)
})

test_that("simulate reserves stochastic and hybrid simulation types", {
    model <- test_model_for_simulation()

    expect_error(
        simulate(model, time = seq(0, 1, by = 1), simulation_type = "ssa"),
        "SSA simulation is not implemented yet"
    )
    expect_error(
        simulate(model, time = seq(0, 1, by = 1), simulation_type = "hybrid"),
        "Hybrid simulation is not implemented yet"
    )
})

test_that("SimulationResult can be printed", {
    model <- test_model_for_simulation()

    out <- simulate(model, time = seq(0, 10, by = 1))

    expect_snapshot(print(out))
})

test_that("SimulationResult print truncates long state and observable lists", {
    old_options <- options(width = 50)
    on.exit(options(old_options), add = TRUE)

    out <- structure(
        list(
            states = data.frame(
                time = 0,
                very_long_state_name_1 = 1,
                very_long_state_name_2 = 2,
                very_long_state_name_3 = 3,
                check.names = FALSE
            ),
            observables = data.frame(
                time = 0,
                very_long_observable_name_1 = 1,
                very_long_observable_name_2 = 2,
                check.names = FALSE
            )
        ),
        class = "SimulationResult"
    )

    expect_snapshot(print(out))
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

test_that("simulate accepts time units for static models without processes", {
    model <- compartment_model() |>
        add_compartment("ex", volume = 1 [L]) |>
        add_molecule("D", cmt = "ex", type = "amount", initial = 0 [mol]) |>
        wire()

    out <- simulate(model, time = seq(0 [h], 1 [h], by = 1 [h]))

    expect_s3_class(out, "SimulationResult")
    expect_equal(out$states$time, units::set_units(c(0, 1), "h", mode = "standard"))
    expect_equal(out$states$a_D_ex, units::set_units(c(0, 0), "mol", mode = "standard"))
})

test_that("simulate rejects invalid time inputs with clear errors", {
    model <- test_model_for_simulation()

    expect_error(
        simulate(model, time = numeric(0)),
        "Argument 'time' must contain at least one time point"
    )
    expect_error(
        simulate(model, time = c(0, NA, 1)),
        "Argument 'time' must not contain missing or non-finite values"
    )
    expect_error(
        simulate(model, time = "1"),
        "Argument 'time' must be numeric"
    )
    expect_error(
        simulate(model, time = c(0, 2, 1)),
        "Argument 'time' must be sorted in non-decreasing order"
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

test_that("simulate accepts a precompiled OdeModel with different parameter values", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke")
    ode_model <- to_ode_model(model)
    time <- seq(0, 10, by = 1)

    out_fast <- simulate(
        ode_model,
        time = time,
        parameters = parameters(A0 = 100, ke = 0.4)
    )
    out_slow <- simulate(
        ode_model,
        time = time,
        parameters = parameters(A0 = 100, ke = 0.1)
    )

    expect_s3_class(out_fast, "SimulationResult")
    expect_equal(out_fast$states$a_drug_Central, 100 * exp(-0.4 * time), tolerance = 1e-6)
    expect_equal(out_slow$states$a_drug_Central, 100 * exp(-0.1 * time), tolerance = 1e-6)
    expect_true(out_fast$states$a_drug_Central[[length(time)]] < out_slow$states$a_drug_Central[[length(time)]])
})

test_that("simulate on an OdeModel applies runtime parameter values to initials and observables", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = "C0", type = "concentration") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(C = c[drug, Central])
    ode_model <- to_ode_model(model)

    out <- simulate(
        ode_model,
        time = seq(0, 2, by = 1),
        parameters = parameters(C0 = 5, V = 20, ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central[[1]], 100)
    expect_equal(out$observables$C, out$states$a_drug_Central / 20, tolerance = 1e-6)
})

test_that("simulate validates OdeModel right-hand side units with runtime parameters", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", "", const = "ke")
    ode_model <- to_ode_model(model)

    expect_no_error(
        simulate(
            ode_model,
            time = seq(0, 1, by = 1) [h],
            parameters = parameters(ke = 0.2 [1/h])
        )
    )
    expect_error(
        simulate(
            ode_model,
            time = seq(0, 1, by = 1) [h],
            parameters = parameters(ke = 1 [mg])
        ),
        "a\\[drug, Central\\]|right-hand side|unit"
    )
})

test_that("simulate reports CompartmentModel unit errors in component terms", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", "", const = "ke")

    expect_error(
        simulate(
            model,
            time = seq(0, 1, by = 1) [h],
            parameters = parameters(ke = 1 [mg])
        ),
        "transport \\(1\\)|rate constant"
    )
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

test_that("simulate can pass parametrized initial conditions as parameters", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke")

    out <- simulate(
        model,
        time = seq(0, 10, by = 1),
        parameters = parameters(A0 = 100, ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate preserves state units for parametrized initial conditions", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke")

    out <- simulate(
        model,
        time = seq(0, 10, by = 1) [h],
        parameters = parameters(A0 = 100 [mg], ke = 0.2 [1/h])
    )

    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)), "mg", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate applies bolus dosing events", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 1, amount = 100, cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2)

    time <- c(0, 1, 1 + 1e-6, 2, 3, 4)
    out <- simulate(model, time = time)

    expect_equal(out$states$a_drug_Central[[1]], 0, tolerance = 1e-8)
    expect_equal(out$states$a_drug_Central[[2]], 100, tolerance = 1e-8)
    expect_equal(
        out$states$a_drug_Central,
        c(0, 100, 100 * exp(-0.2 * (time[3:6] - 1))),
        tolerance = 1e-5
    )
})

test_that("simulate preserves state units for bolus dosing", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, unit = "mg", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 1 [h], amount = 100 [mg], cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2 [1/h])

    time <- c(0, 1, 1 + 1e-6, 2, 3, 4)
    out <- simulate(model, time = time [h])

    expect_equal(out$states$time, units::set_units(time, "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(c(0, 100, 100 * exp(-0.2 * (time[3:6] - 1))), "mg", mode = "standard"),
        tolerance = 1e-5
    )
})

test_that("simulate observables use post-dose event states", {
    model <- compartment_model() |>
        add_compartment("Central", volume = 10) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_dosing(time = 1, amount = 100, cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2)

    time <- c(0, 1, 2)
    out <- simulate(model, time = time)

    expect_equal(out$states$a_drug_Central, c(0, 100, 100 * exp(-0.2)), tolerance = 1e-5)
    expect_equal(out$observables$C, out$states$a_drug_Central / 10, tolerance = 1e-5)
})

test_that("simulate applies infusion dosing events", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 0, rate = 10, duration = 5, cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2)

    out <- simulate(model, time = seq(0, 10, by = 1))
    central <- out$states$a_drug_Central

    expect_true(all(diff(central[1:6]) > 0))
    expect_true(all(diff(central[6:11]) < 0))
})

test_that("simulate preserves state units for infusion dosing", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, unit = "mg", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 0 [h], rate = 10 [mg/h], duration = 5 [h], cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2 [1/h])

    out <- simulate(model, time = seq(0, 10, by = 1) [h])

    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_true(inherits(out$states$a_drug_Central, "units"))
    expect_equal(
        units(out$states$a_drug_Central),
        units(units::set_units(1, "mg", mode = "standard"))
    )
    expect_true(all(diff(out$states$a_drug_Central[1:6]) > units::set_units(0, "mg")))
    expect_true(all(diff(out$states$a_drug_Central[6:11]) < units::set_units(0, "mg")))
})

test_that("simulate returns observable trajectories", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(C = a[drug, Central] / V) |>
        add_parameter(ke = 0.2, V = 10)

    out <- simulate(model, time = seq(0, 10, by = 1))

    expect_s3_class(out, "SimulationResult")
    expect_s3_class(out$observables, "data.frame")
    expect_named(out$observables, c("time", "C"))
    expect_equal(out$observables$time, out$states$time)
    expect_equal(out$observables$C, out$states$a_drug_Central / 10, tolerance = 1e-6)
    expect_snapshot(print(out))
})

test_that("simulate reattaches units to observable trajectories", {
    model <- compartment_model() |>
        add_compartment("Central", volume =  10 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100, unit = "mg", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(ke = 0.2 [1/h])

    out <- simulate(model, time = seq(0, 10, by = 1) [h])

    expect_s3_class(out$observables, "data.frame")
    expect_equal(out$observables$time, out$states$time)
    expect_equal(
        out$observables$C,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)) / 10, "mg/L", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate uses unit-aware free parameters for observable units", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = 100, unit = "mg", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(C = a[drug, Central] / V)

    out <- simulate(
        model,
        time = seq(0, 10, by = 1) [h],
        parameters = parameters(ke = 0.2 [1/h], V = 10 [L])
    )

    expect_s3_class(out$observables, "data.frame")
    expect_equal(
        out$observables$C,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)) / 10, "mg/L", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate supports amount per custom base unit with volume per custom base unit", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("modelcell"), add = TRUE)
    install_model_unit("modelcell")

    model <- compartment_model() |>
        add_compartment("ex", volume = "Vex") |>
        add_molecule("D", cmt = "ex", type = "amount", initial = 1 [nmol/modelcell]) |>
        add_parameter(Vex = 1 [L/modelcell]) |>
        add_observable(Cex = c[D, ex]) |>
        wire()

    out <- simulate(model, time = seq(0 [h], 1 [h], by = 1 [h]))

    expect_equal(out$states$time, units::set_units(c(0, 1), "h", mode = "standard"))
    expect_equal(out$states$a_D_ex, units::set_units(c(1, 1), "nmol/modelcell", mode = "standard"))
    expect_equal(out$observables$Cex, units::set_units(c(1, 1), "nmol/L", mode = "standard"))
})

test_that("simulate supports registered derived custom units in model inputs", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("modelcellperL"), add = TRUE)
    on.exit(units::remove_unit("modelcelltwo"), add = TRUE)
    install_model_unit("modelcelltwo")
    install_model_unit("modelcellperL", "modelcelltwo/L")

    model <- compartment_model() |>
        add_compartment("ex", volume = 1 [L]) |>
        add_molecule("N", cmt = "ex", type = "amount", initial = 1 [modelcellperL] * 1 [L]) |>
        add_observable(Ndensity = a[N, ex] / Vex) |>
        add_parameter(Vex = 1 [L]) |>
        wire()

    out <- simulate(model, time = c(0, 1) [h])

    expect_true(inherits(out$observables$Ndensity, "units"))
    expect_equal(
        units::set_units(out$observables$Ndensity, "modelcelltwo/L", mode = "standard"),
        units::set_units(c(1, 1), "modelcelltwo/L", mode = "standard")
    )
})

test_that("simulate errors informatively for unregistered derived custom units", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("unregisteredcellperL"), add = TRUE)
    on.exit(units::remove_unit("unregisteredcell"), add = TRUE)
    units::install_unit("unregisteredcell")
    units::install_unit("unregisteredcellperL", "unregisteredcell/L")

    model <- compartment_model() |>
        add_compartment("ex", volume = 1 [L]) |>
        add_molecule("N", cmt = "ex", type = "amount", initial = 1 [unregisteredcellperL] * 1 [L]) |>
        wire()

    expect_error(
        simulate(model, time = c(0, 1) [h]),
        "not registered|install_model_unit|register_model_unit"
    )
})
