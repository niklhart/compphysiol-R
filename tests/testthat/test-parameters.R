
test_that("Scalar parameter substitution works in ODE", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central", "k12_forward")

    # Provide k12 as scalar
    odeinfo <- M$toODE(paramValues = list(k12 = 0.2))
    y0 <- sapply(M$compartments, function(c) c$initial)
    dydt <- odeinfo$odefun(0, y0, list())
    # Should evaluate using k12 = 0.2
    expect_equal(dydt[[1]][1], -0.2 * 10)
    expect_equal(dydt[[1]][2], 0.2 * 10)
    # No free parameters left
    expect_equal(length(odeinfo$freeParams), 0)
})

test_that("Free parameters remain in ODE", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central", "k12_forward")

    # Do not provide k12
    odeinfo <- M$toODE(paramValues = list())
    y0 <- sapply(M$compartments, function(c) c$initial)
    dydt <- odeinfo$odefun(0, y0, list(k12 = 0.1))
    expect_equal(dydt[[1]][1], -0.1 * 10)
    expect_equal(dydt[[1]][2], 0.1 * 10)
    # k12 should be listed as free parameter
    expect_true("k12" %in% odeinfo$freeParams)
})
