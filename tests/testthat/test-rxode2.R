test_that("rxode2 model and engine have the public taxonomy", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", initial = 1, type = "amount")

    executable <- to_rxode2_model(model)
    engine <- rxode2_engine(rtol = 1e-8)

    expect_s3_class(executable, "compphysiol_rxode2_model")
    expect_true(is_rxode2_model(executable))
    expect_true(is_executable_model(executable))
    expect_s3_class(engine, "compphysiol_rxode2_engine")
    expect_true(is_rxode2_engine(engine))
    expect_true(is_simulation_engine(engine))
    expect_equal(engine$arguments$rtol, 1e-8)
    expect_error(rxode2_engine(1), "named")
    expect_error(rxode2_engine(events = data.frame()), "managed")
})

test_that("rxode2 agrees with deSolve for equations, nonlinear rates, and units", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = "V") |>
        add_molecule(name = "drug", initial = "A0", type = "amount") |>
        add_equation(elim = "Vmax * a[drug, Central] / (Km + a[drug, Central])") |>
        add_transport(from = "Central", to = "", rate = "elim") |>
        add_observable(A = a[drug, Central], C = c[drug, Central]) |>
        add_parameter(V = 10 [L], Vmax = 5 [mg/h], Km = 20 [mg])
    params <- parameters(A0 = 100 [mg])
    time <- with_units(seq(0, 4, by = 0.5) [h])

    expected <- simulate(model, time = time, parameters = params)
    actual <- simulate(model, time = time, parameters = params, engine = rxode2_engine())

    expect_equal(actual$states, expected$states, tolerance = 1e-5)
    expect_equal(actual$observables, expected$observables, tolerance = 1e-5)
})

test_that("rxode2 handles bolus, infusion, experiments, and cache reuse", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 10 [L]) |>
        add_molecule(name = "drug", initial = 100 [mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = 0.2 [1/h]) |>
        add_observable(A = a[drug, Central])
    schedule <- observation_schedule(c(0, 1, 2) [h], c("A", "A", "A"))
    design <- experiment(
        start = 0 [h],
        observations = schedule,
        dosing = dosing(time = 1 [h], amount = 20 [mg], duration = 1 [h])
    )
    executable <- to_rxode2_model(model |> add_dosing(dose = .experiment_entry(design)$dosing))

    expected <- simulate(model, experiment = design)
    first <- simulate(executable, experiment = design)
    artifact <- get("artifact", executable$cache)
    second <- simulate(executable, experiment = design)

    expect_equal(first$states, expected$states, tolerance = 1e-5)
    expect_equal(first$observables, expected$observables, tolerance = 1e-5)
    expect_identical(get("artifact", executable$cache)$compiled, artifact$compiled)
    expect_named(
        artifact,
        c(
            "source", "compiled", "dimensions", "parameterIndex", "defaultParms",
            "parameterSignature", "parameterTransforms", "obsFuncs",
            "stateUnitValues", "stateUnitScales",
            "observableUnitValues", "observableExportUnitValues", "observableUnitScales",
            "events", "modelUsesTime", "modelHasTimeDependentProcess"
        )
    )
    expect_equal(second, first)
    expect_match(artifact$source, "d/dt\\(cp_state_")
})

test_that("rxode2 identifiers are safe and unsupported calls are diagnosed", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "rx_central", volume = NA_real_) |>
        add_molecule(name = "nlmixr_drug", initial = "rx_initial", type = "amount") |>
        add_transport(from = "rx_central", to = "", rate = "rx_k * a[nlmixr_drug, rx_central]")
    executable <- to_rxode2_model(model)
    expect_true(all(grepl("^cp_", unlist(executable$identifiers))))
    expect_no_error(simulate(executable, time = 0:1,
                             parameters = parameters(rx_initial = 10, rx_k = 0.2)))

    bad <- to_ode_model(model)
    bad$rhs[[1]] <- quote(stats::runif(1))
    expect_error(
        simulate(to_rxode2_model(bad), time = 0:1,
                 parameters = parameters(rx_initial = 10, rx_k = 0.2)),
        "Unsupported call.*stats::runif"
    )
})

test_that("non-ODE routes reject the rxode2 engine", {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = NA_real_) |>
        add_molecule(name = "drug", initial = 1, type = "amount")
    expect_error(
        simulate(model, time = 0:1, simulation_type = "analytical", engine = rxode2_engine()),
        "only supported.*ode"
    )
})

test_that("rxode2 batches experiment collections by individual", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 10 [L]) |>
        add_molecule(name = "drug", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "k") |>
        add_observable(A = a[drug, Central], C = c[drug, Central])
    schedule_a <- observation_schedule(c(0, 1, 2) [h], c("A", "C", "A"))
    schedule_b <- observation_schedule(c(0, 30, 120) [min], c("C", "A", "C"))
    designs <- c(
        control = experiment(
            parameters = parameters(A0 = 100 [mg], k = 0.2 [1/h]),
            observations = schedule_a,
            start = 0 [h]
        ),
        treated = experiment(
            parameters = parameters(A0 = 50 [mg], k = 0.1 [1/h]),
            dosing = dosing(time = 0.5 [h], amount = 20 [mg], duration = 0.5 [h]),
            observations = schedule_b,
            start = 0 [h]
        )
    )

    expected <- simulate(model, experiment = designs)
    actual <- simulate(model, experiment = designs, engine = rxode2_engine())

    expect_s3_class(actual, "compphysiol_simulation_result")
    expect_identical(levels(actual$states$experiment), names(designs))
    expect_equal(actual, expected, tolerance = 1e-5)
})

test_that("rxode2 uses length-one output shape for a one-experiment collection", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 10 [L]) |>
        add_molecule(name = "drug", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "k") |>
        add_observable(C = c[drug, Central])
    designs <- c(only = experiment(
        parameters = parameters(A0 = 100 [mg], k = 0.2 [1/h]),
        observations = observation_schedule(c(0, 1, 2) [h], "C"),
        start = 0 [h]
    ))

    expected <- simulate(model, experiment = designs)
    actual <- simulate(model, experiment = designs, engine = rxode2_engine())

    expect_false("experiment" %in% names(actual$states))
    expect_false("experiment" %in% names(actual$observables))
    expect_equal(actual, expected, tolerance = 1e-5)
})

test_that("rxode2 homogeneous preparation agrees with the fallback path", {
    skip_if_not_installed("rxode2")
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 10 [L]) |>
        add_molecule(name = "drug", initial = "A0", type = "amount") |>
        add_transport(from = "Central", to = "", const = "k") |>
        add_observable(A = a[drug, Central], C = c[drug, Central])
    schedule <- observation_schedule(c(0, 1, 2) [h], c("A", "C", "A"))
    dose <- dosing(time = 0.5 [h], amount = 20 [mg])
    designs <- c(
        first = experiment(parameters = parameters(A0 = 100 [mg], k = 0.2 [1/h]),
                           dosing = dose, observations = schedule),
        second = experiment(parameters = parameters(A0 = 50 [mg], k = 0.1 [1/h]),
                            dosing = dose, observations = schedule)
    )
    fallback <- designs
    attr(fallback, "component_homogeneity")[] <- FALSE

    optimized <- simulate(model, experiment = designs, engine = rxode2_engine())
    unoptimized <- simulate(model, experiment = fallback, engine = rxode2_engine())

    expect_equal(optimized, unoptimized, tolerance = 1e-5)
})
