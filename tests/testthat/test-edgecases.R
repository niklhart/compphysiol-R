
test_that("Empty model returns zero-length ODE", {
    M <- CompartmentModel$new()
    odeinfo <- M$toODE()
    y0 <- M$getInitialState()

    expect_equal(length(y0), 0)
    dydt <- odeinfo$odefun(0, y0, list())
    expect_equal(length(dydt[[1]]), 0)
})

test_that("Simulation fails if target compartment missing", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)

    # Dose references a compartment that will not exist
    dose <- Dosing$new("Peripheral", amount = 10, time = 0)
    M$addDosing(dose)

    # Expected deSolve error
    expect_error({
        odeinfo <- M$toODE(paramValues = list())
        y0 <- M$getInitialState()
        events <- M$dosing_to_events()$data
        deSolve::ode(y = y0, times = 0:10, func = odeinfo$odefun, parms = list(), events = list(data = events))
    })
})

test_that("Missing parameters are listed as free", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central")

    # Do not provide k12
    odeinfo <- M$toODE()
    expect_true("k12" %in% odeinfo$freeParams)
})

test_that("Overlapping infusion events handled correctly", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)

    # Two overlapping infusions
    inf1 <- Dosing$new("Central", rate = 5, duration = 4, time = 0)
    inf2 <- Dosing$new("Central", rate = 3, duration = 4, time = 2)

    M$addDosing(inf1)
    M$addDosing(inf2)

    events <- M$dosing_to_events()$data

    # Check that infusion rate events are present for both start and end
    rateEvents <- events[grepl("InfusionRate_Central", events$var), ]
    expect_equal(nrow(rateEvents), 4) # 2 starts + 2 ends

    # Events sorted by time
    expect_true(all(diff(rateEvents$time) >= 0))
})
