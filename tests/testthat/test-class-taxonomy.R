test_that("model representations use the compphysiol hierarchy", {
    model <- compartment_model() |>
        add_compartment("central", volume = 1) |>
        add_molecule("drug", cmt = "central", initial = 0, type = "amount")
    process <- to_process_model(model)
    ode <- to_ode_model(process)
    compiled <- to_compiled_ode_model(ode)
    analytical <- to_analytical_model(process)
    stochastic <- to_stochastic_model(process)

    expect_identical(
        class(model)[1:2],
        c("compphysiol_compartment_model", "compphysiol_model")
    )
    expect_identical(
        class(process)[1:2],
        c("compphysiol_process_model", "compphysiol_model")
    )
    expect_identical(
        class(ode)[1:2],
        c("compphysiol_ode_model", "compphysiol_model")
    )
    expect_identical(
        class(analytical)[1:2],
        c("compphysiol_analytical_model", "compphysiol_model")
    )
    expect_identical(
        class(stochastic)[1:2],
        c("compphysiol_stochastic_model", "compphysiol_model")
    )
    expect_identical(
        class(compiled)[1:3],
        c(
            "compphysiol_desolve_compiled_model",
            "compphysiol_executable_model",
            "compphysiol_model"
        )
    )
    expect_false(inherits(compiled, "compphysiol_ode_model"))

    expect_false(inherits(model, "CompartmentModel"))
    expect_false(inherits(process, "ProcessModel"))
    expect_false(inherits(ode, "OdeModel"))
    expect_false(inherits(compiled, "CompiledOdeModel"))
})

test_that("engines use role-based prefixed classes", {
    simulation <- deSolve_engine()
    estimation <- optim_engine()
    mixed_effects <- nlmixr2_engine()

    expect_identical(
        class(simulation)[1:3],
        c(
            "compphysiol_desolve_engine",
            "compphysiol_simulation_engine",
            "compphysiol_engine"
        )
    )
    expect_identical(
        class(estimation)[1:3],
        c(
            "compphysiol_optim_engine",
            "compphysiol_estimation_engine",
            "compphysiol_engine"
        )
    )
    expect_identical(
        class(mixed_effects)[1:3],
        c(
            "compphysiol_nlmixr2_engine",
            "compphysiol_estimation_engine",
            "compphysiol_engine"
        )
    )
    expect_false(inherits(simulation, "DeSolveEngine"))
    expect_false(inherits(estimation, "OptimEngine"))
})

test_that("domain and statistical objects have prefixed primary classes", {
    objects <- list(
        compartments = compartments(),
        molecules = molecules(),
        transports = transports(),
        transport = transports("a", "b", const = 1)[[1]],
        reactions = reactions(),
        states = state(molec = "drug", cmt = "central"),
        equations = equations(),
        observables = observables(),
        parameters = parameters(),
        dosing = dosing(),
        experiments = experiment()[integer()],
        parameter_sets = parameter_sets(),
        observation_schedule = observation_schedule(),
        observation_data = observation_data(),
        parameter_spec = parameter_spec(1),
        parameter_specs = c(x = parameter_spec(1)),
        statistical_model = statistical_model(),
        normal_distribution = normal(sd = 1),
        lognormal_distribution = lognormal(sdlog = 1),
        proportional_sd = proportional(1),
        combined_sd = combined(1, 1),
        correlations = correlated(list("x", "y", 0))
    )

    expected <- paste0("compphysiol_", names(objects))
    actual <- vapply(objects, function(x) class(x)[[1]], character(1))
    expect_identical(unname(actual), unname(expected))

    data <- observation_data(time = 1, observable = "x", value = 2)
    expect_identical(
        class(data)[1:2],
        c("compphysiol_observation_data", "compphysiol_observation_schedule")
    )
})

test_that("class-preserving operations retain prefixed primary classes", {
    params <- parameters(a = 1, b = 2)
    expect_identical(class(params["a"])[[1]], "compphysiol_parameters")
    expect_identical(class(c(params["a"], params["b"]))[[1]], "compphysiol_parameters")

    schedule <- observation_schedule(time = 1:2, observable = "x")
    expect_identical(
        class(schedule[1, , drop = FALSE])[[1]],
        "compphysiol_observation_schedule"
    )

    model <- compartment_model() |>
        add_compartment(c("a", "b"), volume = 1) |>
        add_molecule("drug", initial = 1) |>
        add_transport(from = "a", to = "b", const = 1) |>
        wire()
    expect_identical(class(model$molecules)[[1]], "compphysiol_molecules")
    expect_identical(class(model$transports)[[1]], "compphysiol_transports")
})

test_that("simulation results use the prefixed result class", {
    model <- compartment_model() |>
        add_compartment("central", volume = 1) |>
        add_molecule("drug", cmt = "central", initial = 1, type = "amount")

    result <- simulate(model, time = 0)
    expect_identical(class(result)[[1]], "compphysiol_simulation_result")
    expect_false(inherits(result, "SimulationResult"))
})

test_that("public predicates mirror prefixed class names", {
    objects <- list(
        compartments = compartments(),
        molecules = molecules(),
        transports = transports(),
        transport = transports("a", "b", const = 1)[[1]],
        reactions = reactions(),
        states = state(molec = "drug", cmt = "central"),
        equations = equations(),
        observables = observables(),
        parameters = parameters(),
        parameter_sets = parameter_sets(),
        dosing = dosing(),
        experiments = experiment()[integer()],
        observation_schedule = observation_schedule(),
        observation_data = observation_data(),
        parameter_spec = parameter_spec(1),
        parameter_specs = c(x = parameter_spec(1)),
        statistical_model = statistical_model(),
        normal_distribution = normal(sd = 1),
        lognormal_distribution = lognormal(sdlog = 1),
        proportional_sd = proportional(1),
        combined_sd = combined(1, 1),
        correlations = correlated(list("x", "y", 0)),
        simulation_engine = deSolve_engine(),
        desolve_engine = deSolve_engine(),
        estimation_engine = optim_engine(),
        optim_engine = optim_engine(),
        nlmixr2_engine = nlmixr2_engine(),
        physiology = physiology(),
        drug = drug(),
        drugs = drugs()
    )

    for (suffix in names(objects)) {
        predicate <- get(paste0("is_", suffix), mode = "function")
        expect_true(predicate(objects[[suffix]]), info = suffix)
        expect_false(predicate(NULL), info = suffix)
    }
    expect_true(is_experiment(experiment()))
    expect_false(is_experiment(experiment()[integer()]))
})

test_that("model predicates follow concrete and abstract inheritance", {
    model <- compartment_model() |>
        add_compartment("central", volume = 1) |>
        add_molecule("drug", cmt = "central", initial = 0, type = "amount")
    process <- to_process_model(model)
    ode <- to_ode_model(process)
    compiled <- to_compiled_ode_model(ode)
    analytical <- to_analytical_model(process)
    stochastic <- to_stochastic_model(process)

    expect_true(is_compartment_model(model))
    expect_true(is_process_model(process))
    expect_true(is_ode_model(ode))
    expect_true(is_desolve_compiled_model(compiled))
    expect_true(is_analytical_model(analytical))
    expect_true(is_stochastic_model(stochastic))
    expect_true(all(vapply(
        list(model, process, ode, compiled, analytical, stochastic),
        is_model,
        logical(1)
    )))
    expect_false(is_compartment_model(process))
    expect_true(is_executable_model(compiled))
    expect_false(is_executable_model(ode))
    expect_true(is_engine(deSolve_engine()))
    expect_true(is_engine(optim_engine()))
    expect_true(is_engine(nlmixr2_engine()))
})
