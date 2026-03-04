
test_that("Scalar parameter substitution works in ODE", {
    M <- compartment_model() |>
        add_compartment("Central", 10) |>
        add_compartment("Peripheral", 0) |>
        add_flow("Central", "Peripheral", const = "k12") |>
        add_parameter(k12 = 0.2)

    # Provide k12 as scalar
    odeinfo <- to_ode(M)
    y0 <- odeinfo$y0
    dydt <- odeinfo$odefun(0, y0, list())
    # Should evaluate using k12 = 0.2
    expect_equal(dydt[[1]][1], -0.2 * 10)
    expect_equal(dydt[[1]][2], 0.2 * 10)
    # No free parameters left
    expect_equal(length(odeinfo$freeParams), 0)
})

test_that("Free parameters remain in ODE", {
    M <- compartment_model() |>
        add_compartment("Central", 10) |>
        add_compartment("Peripheral", 0) |>
        add_flow("Central", "Peripheral", const = "k12")

    # Do not provide k12
    odeinfo <- to_ode(M)
    y0 <- odeinfo$y0
    dydt <- odeinfo$odefun(0, y0, list(k12 = 0.1))
    expect_equal(dydt[[1]][1], -0.1 * 10)
    expect_equal(dydt[[1]][2], 0.1 * 10)
    # k12 should be listed as free parameter
    expect_true("k12" %in% odeinfo$freeParams)
})
