test_that("t is reserved for model time", {
    expect_error(equations(t = 1), "reserved for the current model time", fixed = TRUE)
    expect_error(parameters(t = 1), "reserved for the current model time", fixed = TRUE)
})

test_that("time dependence is propagated between equations", {
    eq <- equations(
        growth = exp(mu * t),
        V = V0 * growth,
        unrelated = CL / V0
    )

    expect_identical(
        compphysiol:::.equation_time_dependence(eq),
        c(growth = TRUE, V = TRUE, unrelated = FALSE)
    )
})

test_that("time-dependent volumes use amount states", {
    model <- compartment_model() |>
        add_compartment(name = "cell", volume = "V") |>
        add_molecule(name = "x", cmt = "cell", initial = 10, type = "amount") |>
        add_equation(growth = exp(mu * t), V = V0 * growth) |>
        add_observable(concentration = c[x, cell]) |>
        add_parameter(V0 = 2, mu = 0.1)

    ode <- to_ode_model(model)
    expect_identical(ode$states$dsl_name, "a[x, cell]")
    expect_false("t" %in% ode$freeParams)

    result <- simulate(model, time = 0:2, engine = deSolve_engine())
    expect_equal(
        result$observables$value,
        10 / (2 * exp(0.1 * (0:2))),
        tolerance = 1e-6
    )
})

test_that("time-dependent equations are evaluated inside ODE rates", {
    model <- compartment_model() |>
        add_compartment(name = "cell") |>
        add_molecule(name = "x", cmt = "cell", initial = 10, type = "amount") |>
        add_transport(from = "cell", to = "", molec = "x", const = "ke") |>
        add_equation(ke = k0 * (1 + t / tau)) |>
        add_observable(amount = a[x, cell]) |>
        add_parameter(k0 = 0.2, tau = 2)

    result <- simulate(model, time = 0:2, engine = deSolve_engine())
    expected <- 10 * exp(-0.2 * ((0:2) + (0:2)^2 / 4))
    expect_equal(result$observables$value, expected, tolerance = 1e-5)
})

test_that("time-dependent volumes retain units across ODE backends", {
    skip_if_not_installed("units")
    model <- compartment_model() |>
        add_compartment(name = "cell", volume = "V") |>
        add_molecule(name = "x", cmt = "cell", initial = 10 [mg], type = "amount") |>
        add_equation(V = V0 * exp(mu * t)) |>
        add_observable(concentration = c[x, cell]) |>
        add_parameter(V0 = 2 [L], mu = 0.1 [1/h])
    time <- units::set_units(0:2, "h")
    expected <- units::set_units(10 / (2 * exp(0.1 * (0:2))), "mg/L")

    interpreted <- simulate(model, time = time, engine = deSolve_engine())
    compiled <- simulate(model, time = time, engine = deSolve_engine(compiled = TRUE))
    expect_equal(interpreted$observables$value, expected, tolerance = 1e-6)
    expect_equal(compiled$observables$value, expected, tolerance = 1e-6)

    if (requireNamespace("rxode2", quietly = TRUE)) {
        rxode <- simulate(model, time = time, engine = rxode2_engine())
        expect_equal(rxode$observables$value, expected, tolerance = 1e-6)
    }
})

test_that("time-dependent volumes convert concentration initials at simulation start", {
    model <- compartment_model() |>
        add_compartment(name = "cell", volume = "V") |>
        add_molecule(name = "x", cmt = "cell", initial = 5) |>
        add_observable(concentration = c[x, cell]) |>
        add_equation(V = V0 * exp(mu * t)) |>
        add_parameter(V0 = 2, mu = 0.1)

    expected <- 5 * exp(-0.1 * ((2:4) - 2))
    result <- simulate(model, time = 2:4, engine = deSolve_engine())
    expect_equal(result$observables$value, expected, tolerance = 1e-6)

    if (requireNamespace("rxode2", quietly = TRUE)) {
        rxode <- simulate(model, time = 2:4, engine = rxode2_engine())
        expect_equal(rxode$observables$value, expected, tolerance = 1e-6)
    }
})

test_that("unsupported simulation forms reject time-dependent equations", {
    model <- compartment_model() |>
        add_compartment(name = "cell", volume = 1) |>
        add_molecule(name = "x", cmt = "cell", initial = 1, type = "amount") |>
        add_equation(rate = k * exp(mu * t)) |>
        add_parameter(k = 1, mu = 0.1)
    process <- to_process_model(model)

    expect_error(
        to_analytical_model(process),
        "Analytical models do not support time-dependent equation(s): rate.",
        fixed = TRUE
    )
    expect_error(
        to_stochastic_model(process),
        "Stochastic models do not support time-dependent equation(s): rate.",
        fixed = TRUE
    )
})
