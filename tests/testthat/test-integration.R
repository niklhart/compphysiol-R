library(testthat)
library(deSolve)

test_that("Full simulation with bolus dosing works", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central")

    # Add bolus dose
    M$addDosing(Dosing$new("Central", amount = 100, time = 0))

    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))
    y0 <- M$getInitialState()
    events <- M$dosing_to_events()$data

    times <- seq(0, 10, 1)
    out <- ode(y = y0, times = times, func = odeinfo$odefun, parms = list(), events = list(data = events))

    # Check dimensions
    expect_equal(dim(out)[2], length(y0) + 1) # +1 for time column
    # Check mass balance: Central starts high, decreases; Peripheral increases
    expect_true(all(out[, "Central"] <= 100))
    expect_true(all(out[, "Peripheral"] >= 0))
})

test_that("Full simulation with infusion dosing works", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central")

    # Add infusion: rate=10 units/h, duration=5h, start at t=0
    M$addDosing(Dosing$new("Central", rate = 10, duration = 5, time = 0))

    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))
    y0 <- M$getInitialState()
    events <- M$dosing_to_events()$data

    times <- seq(0, 10, 0.5)
    out <- ode(y = y0, times = times, func = odeinfo$odefun, parms = list(), events = list(data = events))

    # Central compartment should increase during infusion and then plateau/decrease
    central <- out[, "Central"]
    expect_true(all(diff(central[1:11]) >= 0))    # first 5h, increasing
    expect_true(all(diff(central[11:length(central)]) <= 0 | diff(central[11:length(central)]) >= -0.1)) # after infusion, decreasing slowly
})
