
test_that("Adding compartments works", {
    M <- CompartmentModel$
        new()$
        addCompartment("Central", initial = 10)$
        addCompartment("Peripheral", initial = 0)
    
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

test_that("Bolus dosing is handled correctly", {
    M <- CompartmentModel$
        new()$
        addCompartment("Central", 0)$
        addDosing(target = "Central", amount = 100, time = 2)

    compartments <- M$getStateNames()
    expect_false("InfusionBag_Central" %in% compartments)
    expect_false("InfusionRate_Central" %in% compartments)

    events_data <- M$toODE()$events$data
    expect_equal(nrow(events_data), 1)
    expect_equal(events_data$var, "Central")
    expect_equal(events_data$time, 2)
    expect_equal(events_data$value, 100)
})

test_that("Infusion dosing creates bag and rate compartments", {
    M <- CompartmentModel$
        new()$
        addCompartment("Central", 0)$
        addDosing("Central", rate = 5, duration = 4, time = 8)

    compartments <- M$getStateNames()
    expect_true("InfusionBag_Central" %in% compartments)
    expect_true("InfusionRate_Central" %in% compartments)

    # One bolus to bag + start and end events for infusion rate
    events_data <- M$toODE()$events$data
    expect_equal(sum(events_data$var == "InfusionBag_Central"), 1)
    expect_equal(sum(events_data$var == "InfusionRate_Central"), 2)
})

