test_model_for_simulation <- function(amount_unit = NULL, time_unit = FALSE) {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, unit = amount_unit, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

    if (time_unit) {
        add_parameter(model, ke = 0.2 [1/h])
    } else {
        add_parameter(model, ke = 0.2)
    }
}

compiled_ode_backend_available <- function() {
    exists("to_compiled_ode_model", mode = "function") &&
        exists("simulate.CompiledOdeModel", mode = "function")
}

simulate_with_ode_backends <- function(model, time, parameters = list(), dimensions = NULL, ...) {
    ode_model <- to_ode_model(model)
    out <- list(
        ode = simulate(
            ode_model,
            time = time,
            parameters = parameters,
            dimensions = dimensions,
            ...
        )
    )

    if (compiled_ode_backend_available()) {
        compiled_model <- to_compiled_ode_model(ode_model)
        out$compiled <- simulate(
            compiled_model,
            time = time,
            parameters = parameters,
            dimensions = dimensions,
            ...
        )
    }

    out
}

expect_ode_backends_equal <- function(results, tolerance = 1e-6) {
    skip_if_not(
        "compiled" %in% names(results),
        "CompiledOdeModel backend is not implemented yet."
    )
    expect_equal(results$compiled$states, results$ode$states, tolerance = tolerance)
    expect_equal(results$compiled$observables, results$ode$observables, tolerance = tolerance)
}

test_that("simulate returns a SimulationResult with ODE states", {
    model <- test_model_for_simulation()

    out <- simulate(model, time = seq(0, 10, by = 1))

    expect_s3_class(out, "compphysiol_simulation_result")
    expect_named(out, c("states", "observables"))
    expect_s3_class(out$states, "data.frame")
    expect_null(out$observables)
    expect_named(out$states, c("time", "a_drug_Central"))
    expect_equal(out$states$time, seq(0, 10, by = 1))
    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("compiled ODE backend matches R ODE backend for representative deterministic models", {
    one_compartment <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

    two_compartment <- compartment_model() |>
        add_compartment(name = c("Central", "Peripheral"), volume = "V") |>
        add_molecule(name = "drug", cmt = c("Central", "Peripheral"), initial = c("A0", 0), type = "amount") |>
        add_transport(from = "Central", to = "", const = "k10") |>
        add_transport(from = "Central", to = "Peripheral", const = "k12") |>
        add_transport(from = "Peripheral", to = "Central", const = "k21") |>
        add_observable(C = c[drug, Central])

    equation_rate <- compartment_model() |>
        add_compartment(name = "Central", volume = "V") |>
        add_molecule(name = "drug", cmt = "Central", initial = "C0", type = "concentration") |>
        add_equation(ke = CL / V) |>
        add_transport(from = "Central", to = "", rate = "ke * a[drug, Central]") |>
        add_observable(C = c[drug, Central])

    bolus_dose <- compartment_model() |>
        add_compartment(name = "Central", volume = 10) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_dosing(time = 1, amount = 100, cmt = "Central", molec = "drug")

    infusion_dose <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_dosing(time = 0, rate = 10, duration = 5, cmt = "Central", molec = "drug")

    unit_aware <- compartment_model() |>
        add_compartment(name = "Central", volume = 10 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central])

    math_rate <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(from = "Central", to = "", rate = "ke * sqrt(a[drug, Central])")

    cases <- list(
        list(
            model = one_compartment,
            time = seq(0, 10, by = 1),
            parameters = parameters(A0 = 100, ke = 0.2)
        ),
        list(
            model = two_compartment,
            time = seq(0, 3, by = 1),
            parameters = parameters(A0 = 10, V = 2, k10 = 0.1, k12 = 0.2, k21 = 0.3)
        ),
        list(
            model = equation_rate,
            time = seq(0, 2, by = 1),
            parameters = parameters(C0 = 5, V = 20, CL = 4)
        ),
        list(
            model = bolus_dose,
            time = c(0, 1, 2),
            parameters = parameters(ke = 0.2)
        ),
        list(
            model = infusion_dose,
            time = seq(0, 10, by = 1),
            parameters = parameters(ke = 0.2)
        ),
        list(
            model = unit_aware,
            time = units::set_units(seq(0, 10, by = 1), "h", mode = "standard"),
            parameters = parameters(A0 = 100 [mg], ke = 0.2 [1/h])
        ),
        list(
            model = math_rate,
            time = seq(0, 10, by = 1),
            parameters = parameters(ke = 0.2)
        )
    )

    for (case in cases) {
        results <- simulate_with_ode_backends(
            case$model,
            time = case$time,
            parameters = case$parameters
        )
        expect_ode_backends_equal(results)
    }
})

test_that("compiled ODE backend rejects unsupported RHS calls before solving", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(from = "Central", to = "", rate = "ifelse(a[drug, Central] > 0, a[drug, Central], 0)")

    expect_error(
        simulate(to_compiled_ode_model(model), time = seq(0, 1, by = 1)),
        "ifelse\\(\\).*not supported"
    )
})

test_that("simulate accepts explicit ODE and analytical simulation types", {
    model <- test_model_for_simulation()
    time <- seq(0, 10, by = 1)

    default <- simulate(model, time = time)
    ode <- simulate(model, time = time, simulation_type = "ode")
    analytical <- simulate(model, time = time, simulation_type = "analytical")

    expect_equal(ode, default)
    expect_equal(analytical$states, default$states, tolerance = 1e-6)
    expect_equal(analytical$observables, default$observables)
})

test_that("simulate supports SSA and hybrid simulation types", {
    model <- test_model_for_simulation()

    expect_s3_class(
        simulate(model, time = seq(0, 1, by = 1), simulation_type = "ssa", seed = 1),
        "compphysiol_simulation_result"
    )
    expect_s3_class(
        simulate(model, time = seq(0, 1, by = 1), simulation_type = "hybrid", partition = FALSE, seed = 1),
        "compphysiol_simulation_result"
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
                time = c(0, 0),
                observable = c("very_long_observable_name_1", "very_long_observable_name_2"),
                value = c(1, 2),
                check.names = FALSE
            )
        ),
        class = "compphysiol_simulation_result"
    )

    expect_snapshot(print(out))
})

test_that("SimulationResult print summarizes repeated stochastic simulations", {
    out <- structure(
        list(
            states = data.frame(
                time = rep(c(0, 10), times = 1500),
                rep = rep(seq_len(1500), each = 2),
                a_A_cyt = 0,
                check.names = FALSE
            ),
            observables = NULL
        ),
        class = "compphysiol_simulation_result"
    )

    expect_snapshot(print(out))
})

test_that("simulate accepts time units through the time DSL", {
    model <- test_model_for_simulation(amount_unit = "mg", time_unit = TRUE)

    out <- simulate(model, time = seq(0, 10, by = 1) [h])

    expect_s3_class(out, "compphysiol_simulation_result")
    expect_s3_class(out$states, "data.frame")
    expect_equal(out$states$time, units::set_units(seq(0, 10, by = 1), "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)), "mg", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate wires scalar dosing targets before ODE export", {
    model <- multiCompModel(ncomp = 2, type = "micro", unit = "mg") |>
        add_dosing(time = 0 [h], amount = 100 [mg], cmt = "cen") |>
        add_parameter(
            kc0 = 0.15 [1/h],
            kcp = 0.08 [1/h],
            kpc = 0.05 [1/h],
            Vcen = 8 [L],
            Vper = 20 [L]
        )

    out <- simulate(model, time = seq(0, 24, by = 0.5) [h])

    expect_s3_class(out, "compphysiol_simulation_result")
    expect_named(out$states, c("time", "a_drug_cen", "a_drug_per"))
    expect_equal(out$states$time, units::set_units(seq(0, 24, by = 0.5), "h", mode = "standard"))
    expect_equal(out$states$a_drug_cen[[1]], units::set_units(100, "mg", mode = "standard"))
})

test_that("simulate accepts time units through the unit argument", {
    model <- test_model_for_simulation(amount_unit = "mg", time_unit = TRUE)

    out <- simulate(model, time = seq(0, 10, by = 1), unit = "h")

    expect_s3_class(out, "compphysiol_simulation_result")
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
        add_compartment(name = "ex", volume = 1 [L]) |>
        add_molecule(name = "D", cmt = "ex", type = "amount", initial = 0 [mol]) |>
        wire()

    out <- simulate(model, time = seq(0 [h], 1 [h], by = 1 [h]))

    expect_s3_class(out, "compphysiol_simulation_result")
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
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

    out <- simulate(model, time = seq(0, 10, by = 1), parameters = list(ke = 0.2))

    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate reports missing ODE parameters before solver evaluation", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_dosing(time = 0, amount = 10, cmt = "Central", molec = "drug")

    expect_error(
        simulate(model, time = 1:10),
        "Missing parameter\\(s\\) for simulation: ke\\."
    )
})

test_that("simulate accepts a precompiled OdeModel with different parameter values", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
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

    expect_s3_class(out_fast, "compphysiol_simulation_result")
    expect_equal(out_fast$states$a_drug_Central, 100 * exp(-0.4 * time), tolerance = 1e-6)
    expect_equal(out_slow$states$a_drug_Central, 100 * exp(-0.1 * time), tolerance = 1e-6)
    expect_true(out_fast$states$a_drug_Central[[length(time)]] < out_slow$states$a_drug_Central[[length(time)]])
})

test_that("simulate accepts a CompiledOdeModel with different parameter values", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
    compiled_model <- to_compiled_ode_model(to_ode_model(model))
    time <- seq(0, 10, by = 1)

    out_fast <- simulate(
        compiled_model,
        time = time,
        parameters = parameters(A0 = 100, ke = 0.4)
    )
    out_slow <- simulate(
        compiled_model,
        time = time,
        parameters = parameters(A0 = 100, ke = 0.1)
    )

    expect_s3_class(out_fast, "compphysiol_simulation_result")
    expect_equal(out_fast$states$a_drug_Central, 100 * exp(-0.4 * time), tolerance = 1e-6)
    expect_equal(out_slow$states$a_drug_Central, 100 * exp(-0.1 * time), tolerance = 1e-6)
    expect_true(out_fast$states$a_drug_Central[[length(time)]] < out_slow$states$a_drug_Central[[length(time)]])
})

test_that("CompiledOdeModel reuses compiled DLL across parameter changes", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
    compiled_model <- to_compiled_ode_model(model)
    time <- seq(0, 2, by = 1)

    expect_equal(ls(compiled_model$cache), character(0))
    simulate(compiled_model, time = time, parameters = parameters(A0 = 100, ke = 0.4))
    expect_equal(ls(compiled_model$cache), character(0))
    expect_true(exists(".artifact", envir = compiled_model$cache, inherits = FALSE))
    expect_true(exists(".dimensions", envir = compiled_model$cache, inherits = FALSE))
    cached_dll <- get(".artifact", envir = compiled_model$cache, inherits = FALSE)$dll

    simulate(compiled_model, time = time, parameters = parameters(A0 = 50, ke = 0.1))
    expect_equal(ls(compiled_model$cache), character(0))
    expect_equal(get(".artifact", envir = compiled_model$cache, inherits = FALSE)$dll, cached_dll)
})

test_that("CompiledOdeModel rejects runtime parameters outside the frozen interface", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
    compiled_model <- to_compiled_ode_model(model)

    expect_error(
        simulate(
            compiled_model,
            time = seq(0, 1, by = 1),
            parameters = parameters(A0 = 100, ke = 0.2, scale = 2)
        ),
        "fixed parameter interface.*scale"
    )
})

test_that("CompiledOdeModel checks cached parameter unit signatures", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
    compiled_model <- to_compiled_ode_model(model)
    time <- units::set_units(seq(0, 2, by = 1), "h", mode = "standard")
    dimensions <- list(mass = "mg", time = "h")

    simulate(
        compiled_model,
        time = time,
        parameters = parameters(A0 = 100 [mg], ke = 0.2 [1/h]),
        dimensions = dimensions
    )

    expect_error(
        simulate(
            compiled_model,
            time = time,
            parameters = parameters(A0 = 100 [L], ke = 0.2 [1/h]),
            dimensions = dimensions
        ),
        "A0.*cached compiled model signature"
    )
    expect_error(
        simulate(
            compiled_model,
            time = time,
            parameters = parameters(A0 = 0.1 [g], ke = 0.2 [1/h]),
            dimensions = dimensions
        ),
        "A0.*requires exactly"
    )
})

test_that("compiled parameter transforms support scale and offset conversions", {
    supplied <- parameters(
        rate = 1 [1/h],
        temperature = 0 [degC],
        unitless = 2
    )
    dimensions <- list(time = "min", temperature = "K")
    transforms <- .compiled_ode_model_parameter_transforms(
        names(supplied), supplied, dimensions
    )

    expect_equal(transforms$rate$offset, 0)
    expect_equal(transforms$rate$scale, 1 / 60)
    expect_identical(transforms$rate$unit, "h-1")
    expect_equal(transforms$temperature$offset, 273.15)
    expect_equal(transforms$temperature$scale, 1)
    expect_identical(transforms$temperature$unit, "°C")
    expect_equal(transforms$unitless, list(offset = 0, scale = 1))

    artifact <- list(
        defaultParms = c(rate = 0, temperature = 0, unitless = 0),
        parameterIndex = c(rate = 1L, temperature = 2L, unitless = 3L),
        parameterTransforms = transforms
    )
    runtime <- parameters(
        rate = 6 [1/h],
        temperature = 10 [degC],
        unitless = 4
    )
    expected <- c(rate = 0.1, temperature = 283.15, unitless = 4)

    expect_equal(
        .compiled_ode_model_runtime_parameter_values(
            artifact, runtime, dimensions
        ),
        expected
    )
    expect_equal(
        .rxode2_runtime_parameter_values(artifact, runtime, dimensions),
        expected
    )

    signature <- .compiled_ode_model_parameter_signature(
        names(supplied), supplied
    )
    alternate_units <- parameters(
        rate = 1 [1/min],
        temperature = 283.15 [K],
        unitless = 4
    )
    expect_error(
        .compiled_ode_model_check_parameter_signature(signature, alternate_units),
        "rate.*requires exactly"
    )
    expect_error(
        .compiled_ode_model_check_runtime_parameter_signature(
            signature, alternate_units
        ),
        "rate.*requires exactly"
    )
})

test_that("prepared compiled experiment designs preserve heterogeneous simulations", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 1 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = NULL, molec = "drug", const = "k") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k = 0.2 [1/h])
    studies <- c(
        first = experiment(
            observations = observation_schedule(c(0, 1) [h], c("C", "C")),
            start = 0 [h]
        ),
        second = experiment(
            dosing = dosing(
                time = 1 [h], amount = 10 [mg],
                molec = "drug", cmt = "Central"
            ),
            observations = observation_schedule(c(0, 2) [h], c("C", "C")),
            start = 0 [h]
        )
    )
    executable <- to_compiled_ode_model(model)
    expected <- simulate(executable, experiment = studies)
    prepared <- .simulation_prepare_compiled_experiments(executable, studies)
    actual <- .simulate_prepared_compiled_experiments(
        executable, studies, prepared, deSolve_engine(compiled = TRUE)
    )

    expect_length(prepared$times, 2L)
    expect_length(prepared$events, 2L)
    expect_length(prepared$observation_plans, 2L)
    expect_false(prepared$homogeneity[["dosing"]])
    expect_false(prepared$homogeneity[["observation_plan"]])
    expect_equal(actual, expected)
})

test_that("prepared rxode2 experiment designs preserve heterogeneous simulations", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 1 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = NULL, molec = "drug", const = "k") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k = 0.2 [1/h])
    studies <- c(
        first = experiment(
            observations = observation_schedule(c(0, 1) [h], c("C", "C")),
            start = 0 [h]
        ),
        second = experiment(
            dosing = dosing(
                time = 1 [h], amount = 10 [mg],
                molec = "drug", cmt = "Central"
            ),
            observations = observation_schedule(c(0, 2) [h], c("C", "C")),
            start = 0 [h]
        )
    )
    executable <- to_rxode2_model(model)
    engine <- rxode2_engine()
    expected <- simulate(executable, experiment = studies, engine = engine)
    prepared <- .simulation_prepare_compiled_experiments(executable, studies)
    actual <- .simulate_prepared_compiled_experiments(
        executable, studies, prepared, engine
    )

    expect_s3_class(prepared$rx_records, "data.frame")
    expect_equal(actual, expected, tolerance = 1e-8)
})

test_that("CompiledOdeModel uses fixed solver dimensions after compilation", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])
    compiled_model <- to_compiled_ode_model(model)

    simulate(
        compiled_model,
        time = seq(0, 1, by = 1) [h],
        dimensions = list(mass = "mg", time = "h")
    )

    expect_error(
        simulate(
            compiled_model,
            time = seq(0, 1, by = 1) [h],
            dimensions = list(mass = "kg", time = "h")
        ),
        "fixed solver dimensions"
    )
})

test_that("CompiledOdeModel converts parameter values to solver dimensions before RHS evaluation", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", rate = "Q / V * a[drug, Central]") |>
        add_parameter(
            Q = 0.001 [m^3/h],
            V = 1 [L]
        )
    compiled_model <- to_compiled_ode_model(model)
    time <- seq(0, 2, by = 1)

    out <- simulate(
        compiled_model,
        time = time [h],
        dimensions = list(mass = "kg", length = "m", time = "h")
    )

    expect_equal(
        out$states$a_drug_Central,
        units::set_units(1e-4 * exp(-time), "kg", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("CompiledOdeModel converts observable values from solver to model units", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 1 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(ke = 0 [1/h])
    compiled_model <- to_compiled_ode_model(model)

    out <- simulate(
        compiled_model,
        time = seq(0, 1, by = 1) [h],
        dimensions = list(mass = "kg", length = "m", time = "h")
    )

    expect_equal(
        subset(out$observables, observable == "C")$value,
        units::set_units(c(100, 100), "mg/L", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate reports all missing OdeModel parameters together", {
    model <- compartment_model() |>
        add_compartment(name = c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule(name = "drug", cmt = c("Central", "Peripheral"), initial = c("A0", 0), type = "amount") |>
        add_transport(from = "Central", to = "", const = "k10") |>
        add_transport(from = "Central", to = "Peripheral", const = "k12") |>
        add_transport(from = "Peripheral", to = "Central", const = "k21") |>
        add_observable(Acentral = a[drug, Central])
    ode_model <- to_ode_model(model)

    expect_error(
        simulate(ode_model, time = 1:10),
        "Missing parameter\\(s\\) for simulation: A0, k10, k12, k21\\."
    )
})

test_that("simulate on an OdeModel applies runtime parameter values to initials and observables", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = "V") |>
        add_molecule(name = "drug", cmt = "Central", initial = "C0", type = "concentration") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central])
    ode_model <- to_ode_model(model)

    out <- simulate(
        ode_model,
        time = seq(0, 2, by = 1),
        parameters = parameters(C0 = 5, V = 20, ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central[[1]], 100)
    expect_equal(subset(out$observables, observable == "C")$value, out$states$a_drug_Central / 20, tolerance = 1e-6)
})

test_that("simulate on a CompiledOdeModel applies runtime parameter values to initials and observables", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = "V") |>
        add_molecule(name = "drug", cmt = "Central", initial = "C0", type = "concentration") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central])
    compiled_model <- to_compiled_ode_model(model)

    out <- simulate(
        compiled_model,
        time = seq(0, 2, by = 1),
        parameters = parameters(C0 = 5, V = 20, ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central[[1]], 100)
    expect_equal(subset(out$observables, observable == "C")$value, out$states$a_drug_Central / 20, tolerance = 1e-6)
})

test_that("cached CompiledOdeModel observables use current runtime parameters", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = a[drug, Central] / V)
    compiled_model <- to_compiled_ode_model(model)
    time <- seq(0, 1, by = 1)

    out_v10 <- simulate(
        compiled_model,
        time = time,
        parameters = parameters(A0 = 100, ke = 0, V = 10)
    )
    out_v20 <- simulate(
        compiled_model,
        time = time,
        parameters = parameters(A0 = 100, ke = 0, V = 20)
    )

    expect_equal(subset(out_v10$observables, observable == "C")$value, c(10, 10), tolerance = 1e-6)
    expect_equal(subset(out_v20$observables, observable == "C")$value, c(5, 5), tolerance = 1e-6)
})

test_that("simulate accepts a precompiled AnalyticalModel", {
    model <- compartment_model() |>
        add_compartment(name = c("Central", "Peripheral"), volume = "V") |>
        add_molecule(name = "drug", cmt = c("Central", "Peripheral"), initial = c("A0", 0), type = "amount") |>
        add_transport(from = "Central", to = "", const = "k10") |>
        add_transport(from = "Central", to = "Peripheral", const = "k12") |>
        add_transport(from = "Peripheral", to = "Central", const = "k21") |>
        add_observable(C = c[drug, Central])
    analytical_model <- to_analytical_model(model)
    ode_model <- to_ode_model(model)
    time <- seq(0, 3, by = 1)
    parameters <- parameters(A0 = 10, V = 2, k10 = 0.1, k12 = 0.2, k21 = 0.3)

    analytical <- simulate(analytical_model, time = time, parameters = parameters)
    numerical <- simulate(ode_model, time = time, parameters = parameters)

    expect_s3_class(analytical, "compphysiol_simulation_result")
    expect_equal(analytical$states, numerical$states, tolerance = 1e-5)
    expect_equal(analytical$observables, numerical$observables, tolerance = 1e-5)
})

test_that("simulate on an AnalyticalModel is unit-aware", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
    analytical_model <- to_analytical_model(model)

    out <- simulate(
        analytical_model,
        time = seq(0, 2, by = 1) [h],
        parameters = parameters(A0 = 100 [mg], ke = 0.2 [1/h])
    )

    expect_equal(out$states$time, units::set_units(seq(0, 2, by = 1), "h", mode = "standard"))
    expect_equal(
        out$states$a_drug_Central,
        units::set_units(100 * exp(-0.2 * seq(0, 2, by = 1)), "mg", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate validates AnalyticalModel coefficient units with runtime parameters", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
    analytical_model <- to_analytical_model(model)

    expect_error(
        simulate(
            analytical_model,
            time = seq(0, 1, by = 1) [h],
            parameters = parameters(ke = 1 [mg])
        ),
        "a\\[drug, Central\\]|right-hand side|unit"
    )
})

test_that("simulate validates OdeModel right-hand side units with runtime parameters", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")
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
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

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
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

    out <- simulate(
        model,
        time = seq(0, 10, by = 1),
        parameters = parameters(ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate can pass unit-aware free parameters as a Parameters object", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, unit = "mg", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

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
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

    out <- simulate(
        model,
        time = seq(0, 10, by = 1),
        parameters = parameters(A0 = 100, ke = 0.2)
    )

    expect_equal(out$states$a_drug_Central, 100 * exp(-0.2 * out$states$time), tolerance = 1e-6)
})

test_that("simulate preserves state units for parametrized initial conditions", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke")

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
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
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
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, unit = "mg", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
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
        add_compartment(name = "Central", volume = 10) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_dosing(time = 1, amount = 100, cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2)

    time <- c(0, 1, 2)
    out <- simulate(model, time = time)

    expect_equal(out$states$a_drug_Central, c(0, 100, 100 * exp(-0.2)), tolerance = 1e-5)
    expect_equal(subset(out$observables, observable == "C")$value, out$states$a_drug_Central / 10, tolerance = 1e-5)
})

test_that("simulate applies infusion dosing events", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_dosing(time = 0, rate = 10, duration = 5, cmt = "Central", molec = "drug") |>
        add_parameter(ke = 0.2)

    out <- simulate(model, time = seq(0, 10, by = 1))
    central <- out$states$a_drug_Central

    expect_true(all(diff(central[1:6]) > 0))
    expect_true(all(diff(central[6:11]) < 0))
})

test_that("simulate preserves state units for infusion dosing", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", cmt = "Central", initial = 0, unit = "mg", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
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
        add_compartment(name = "Central", volume = "V") |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = a[drug, Central] / V) |>
        add_parameter(ke = 0.2, V = 10)

    out <- simulate(model, time = seq(0, 10, by = 1))

    expect_s3_class(out, "compphysiol_simulation_result")
    expect_s3_class(out$observables, "data.frame")
    expect_named(out$observables, c("time", "observable", "value"))
    expect_equal(out$observables$time, out$states$time)
    expect_equal(subset(out$observables, observable == "C")$value, out$states$a_drug_Central / 10, tolerance = 1e-6)
    expect_snapshot(print(out))
})

test_that("simulate reattaches units to observable trajectories", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume =  10 [L]) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, unit = "mg", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(ke = 0.2 [1/h])

    out <- simulate(model, time = seq(0, 10, by = 1) [h])

    expect_s3_class(out$observables, "data.frame")
    expect_equal(out$observables$time, out$states$time)
    expect_equal(
        subset(out$observables, observable == "C")$value,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)) / 10, "mg/L", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate uses unit-aware free parameters for observable units", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = "V") |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, unit = "mg", type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_observable(C = a[drug, Central] / V)

    out <- simulate(
        model,
        time = seq(0, 10, by = 1) [h],
        parameters = parameters(ke = 0.2 [1/h], V = 10 [L])
    )

    expect_s3_class(out$observables, "data.frame")
    expect_equal(
        subset(out$observables, observable == "C")$value,
        units::set_units(100 * exp(-0.2 * seq(0, 10, by = 1)) / 10, "mg/L", mode = "standard"),
        tolerance = 1e-6
    )
})

test_that("simulate supports amount per custom base unit with volume per custom base unit", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("modelcell"), add = TRUE)
    install_model_unit("modelcell")

    model <- compartment_model() |>
        add_compartment(name = "ex", volume = "Vex") |>
        add_molecule(name = "D", cmt = "ex", type = "amount", initial = 1 [nmol/modelcell]) |>
        add_parameter(Vex = 1 [L/modelcell]) |>
        add_observable(Cex = c[D, ex]) |>
        wire()

    out <- simulate(model, time = seq(0 [h], 1 [h], by = 1 [h]))

    expect_equal(out$states$time, units::set_units(c(0, 1), "h", mode = "standard"))
    expect_equal(out$states$a_D_ex, units::set_units(c(1, 1), "nmol/modelcell", mode = "standard"))
    expect_equal(subset(out$observables, observable == "Cex")$value, units::set_units(c(1, 1), "nmol/L", mode = "standard"))
})

test_that("simulate supports registered derived custom units in model inputs", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("modelcellperL"), add = TRUE)
    on.exit(units::remove_unit("modelcelltwo"), add = TRUE)
    install_model_unit("modelcelltwo")
    install_model_unit("modelcellperL", "modelcelltwo/L")

    model <- compartment_model() |>
        add_compartment(name = "ex", volume = 1 [L]) |>
        add_molecule(name = "N", cmt = "ex", type = "amount", initial = 1 [modelcellperL] * 1 [L]) |>
        add_observable(Ndensity = a[N, ex] / Vex) |>
        add_parameter(Vex = 1 [L]) |>
        wire()

    out <- simulate(model, time = c(0, 1) [h])

    expect_true(inherits(subset(out$observables, observable == "Ndensity")$value, "units"))
    expect_equal(
        units::set_units(subset(out$observables, observable == "Ndensity")$value, "modelcelltwo/L", mode = "standard"),
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
        add_compartment(name = "ex", volume = 1 [L]) |>
        add_molecule(name = "N", cmt = "ex", type = "amount", initial = 1 [unregisteredcellperL] * 1 [L]) |>
        wire()

    expect_error(
        simulate(model, time = c(0, 1) [h]),
        "not registered|install_model_unit|register_model_unit"
    )
})
