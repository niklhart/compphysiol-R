test_that("interpolate validates and sorts schedules", {
    profile <- interpolate(time = c(2, 0, 1), value = c(4, 1, 2))
    expect_equal(profile$time, 0:2)
    expect_equal(profile$value, c(1, 2, 4))
    expect_identical(profile$method, "linear")

    expect_error(interpolate(numeric(), numeric()), "nonempty numeric vector")
    expect_error(interpolate(c(0, 0), c(1, 2)), "must be unique")
    expect_error(interpolate(0:1, 1), "same length")
    expect_error(interpolate(c(0, Inf), 1:2), "only finite")
    expect_error(interpolate(0:1, c(1, NA)), "only finite")
    expect_error(interpolate(0:1, 1:2, "spline"), "arg")
})

test_that("interpolation methods use agreed boundary behavior", {
    linear <- interpolate(0:2, c(1, 2, 4), method = "linear")
    constant <- interpolate(0:2, c(1, 2, 4), method = "constant")

    expect_equal(compphysiol:::.interpolation_value(linear, 0.5), 1.5)
    expect_equal(compphysiol:::.interpolation_value(linear, 3), 4)
    expect_equal(compphysiol:::.interpolation_value(constant, 0.5), 1)
    expect_equal(compphysiol:::.interpolation_value(constant, 1), 2)
    expect_equal(compphysiol:::.interpolation_value(constant, 3), 4)
    expect_error(
        compphysiol:::.interpolation_value(linear, -1),
        "before its first scheduled time"
    )
})

.time_parameter_model <- function(profile, initial = 10) {
    compartment_model() |>
        add_compartment(name = "central") |>
        add_molecule(name = "drug", cmt = "central", initial = initial, type = "amount") |>
        add_transport(from = "central", to = "", molec = "drug", const = "k") |>
        add_observable(amount = a[drug, central]) |>
        add_parameter(k = profile)
}

test_that("linear time-dependent parameters work in ODE backends", {
    model <- .time_parameter_model(interpolate(0:2, c(1, 2, 4)))
    time <- seq(0, 3, by = 0.5)
    integrated_rate <- ifelse(
        time <= 1,
        time + time^2 / 2,
        ifelse(
            time <= 2,
            1.5 + 2 * (time - 1) + (time - 1)^2,
            4.5 + 4 * (time - 2)
        )
    )
    expected <- 10 * exp(-integrated_rate)

    interpreted <- simulate(model, time = time, engine = deSolve_engine())
    expect_equal(interpreted$observables$value, expected, tolerance = 1e-6)

    compiled <- simulate(to_compiled_ode_model(model), time = time)
    expect_equal(compiled$observables$value, expected, tolerance = 1e-6)

    if (requireNamespace("rxode2", quietly = TRUE)) {
        rxode <- simulate(model, time = time, engine = rxode2_engine())
        expect_equal(rxode$observables$value, expected, tolerance = 1e-5)
    }
})

test_that("a time-dependent parameter can override a scalar or free parameter", {
    base <- .time_parameter_model(1)
    ode <- to_ode_model(base)
    profile <- parameters(k = interpolate(0:1, c(1, 2)))
    expected <- 10 * exp(-c(0, 1.5, 3.5))

    interpreted <- simulate(ode, time = 0:2, parameters = profile)
    expect_equal(interpreted$observables$value, expected, tolerance = 1e-6)

    if (requireNamespace("rxode2", quietly = TRUE)) {
        rxode <- simulate(ode, time = 0:2, parameters = profile, engine = rxode2_engine())
        expect_equal(rxode$observables$value, expected, tolerance = 1e-5)
    }
})

test_that("a scalar parameter can override a time-dependent default", {
    ode <- to_ode_model(.time_parameter_model(interpolate(0:1, c(1, 2))))
    expected <- 10 * exp(-2 * (0:2))
    interpreted <- simulate(ode, time = 0:2, parameters = parameters(k = 2))
    expect_equal(interpreted$observables$value, expected, tolerance = 1e-6)

    if (requireNamespace("rxode2", quietly = TRUE)) {
        rxode <- simulate(
            ode, time = 0:2, parameters = parameters(k = 2), engine = rxode2_engine()
        )
        expect_equal(rxode$observables$value, expected, tolerance = 1e-5)
    }
})

test_that("constant time-dependent parameters carry observations forward", {
    model <- .time_parameter_model(
        interpolate(0:2, c(1, 2, 4), method = "constant")
    )
    time <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
    integrated_rate <- c(0, 0.5, 1, 2, 3, 5, 7)
    result <- simulate(model, time = time)
    expect_equal(result$observables$value, 10 * exp(-integrated_rate), tolerance = 1e-5)
    compiled <- simulate(to_compiled_ode_model(model), time = time)
    expect_equal(compiled$observables$value, 10 * exp(-integrated_rate), tolerance = 1e-5)
})

test_that("time-dependent parameters cannot be evaluated before their schedule", {
    model <- .time_parameter_model(interpolate(1:2, c(1, 2)))
    expect_error(
        simulate(model, time = 0:2),
        "parameter 'k' at the simulation start",
        fixed = TRUE
    )
    expect_silent(simulate(model, time = 1:3))
})

test_that("time-dependent parameters retain time and value units", {
    skip_if_not_installed("units")
    profile <- interpolate(
        units::set_units(0:2, "h"),
        units::set_units(c(1, 2, 4), "1/h")
    )
    model <- .time_parameter_model(profile, initial = units::set_units(10, "mg"))
    result <- simulate(model, time = units::set_units(0:2, "h"))

    expect_true(inherits(result$observables$value, "units"))
    expect_equal(units::deparse_unit(result$observables$value), "mg")
    expect_error(
        simulate(model, time = units::set_units(0:2, "s")),
        NA
    )
    expect_error(simulate(model, time = 0:2), "must both be unit-free")
})

test_that("analytical and stochastic models reject time-dependent parameters", {
    process <- to_process_model(.time_parameter_model(interpolate(0:1, 1:2)))
    expect_error(to_analytical_model(process), "time-dependent parameter(s): k", fixed = TRUE)
    expect_error(to_stochastic_model(process), "time-dependent parameter(s): k", fixed = TRUE)
})
