
test_that("ODE function is generated correctly", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central")

    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))
    expect_true(is.function(odeinfo$odefun))
    expect_equal(odeinfo$stateNames, c("Central", "Peripheral"))
})

test_that("ODE can be evaluated", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central")

    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))
    y0 <- sapply(M$compartments, function(c) c$initial)
    dydt <- odeinfo$odefun(0, y0, list())
    expect_equal(length(dydt[[1]]), 2)
    # Central decreases, Peripheral increases
    expect_true(dydt[[1]][1] < 0)
    expect_true(dydt[[1]][2] > 0)
})
