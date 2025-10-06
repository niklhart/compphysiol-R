
test_that("Adding compartments works", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", initial = 10)
    M$addCompartment("Peripheral", initial = 0)

    compartments <- M$getStateNames()
    expect_equal(compartments, c("Central", "Peripheral"))

    initials <- M$getInitialState(name = FALSE)
    expect_equal(initials, c(10, 0))
})

test_that("Adding reactions works", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)

    M$addReaction("Central", "Peripheral", "k12 * Central")
    expect_equal(length(M$reactions), 1)
    r <- M$reactions[[1]]
    expect_equal(r$from, "Central")
    expect_equal(r$to, "Peripheral")
    expect_equal(r$rate, quote(k12 * Central))
})


test_that("Bolus dosing creates events correctly", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)
    dose <- Dosing$new("Central", amount = 100, time = 0)
    M$addDosing(dose)

    events_data <- M$dosing_to_events()$data
    expect_equal(nrow(events_data), 1)
    expect_equal(events_data$var, "Central")
    expect_equal(events_data$time, 0)
    expect_equal(events_data$value, 100)
})

test_that("Infusion dosing creates bag and rate compartments", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)
    infusion <- Dosing$new("Central", rate = 5, duration = 4, time = 8)
    M$addDosing(infusion)

    compartments <- M$getStateNames()
    expect_true("InfusionBag_Central" %in% compartments)
    expect_true("InfusionRate_Central" %in% compartments)

    events_data <- M$dosing_to_events()$data
    # One bolus to bag + start and end events for infusion rate
    expect_equal(sum(events_data$var == "InfusionBag_Central"), 1)
    expect_equal(sum(events_data$var == "InfusionRate_Central"), 2)
})

