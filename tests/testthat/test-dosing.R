
test_that("Bolus dosing creates events correctly", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)
    dose <- Dosing$new("Central", amount = 100, time = 0)
    M$addDosing(dose)

    events <- M$dosing_to_events()$data
    expect_equal(nrow(events), 1)
    expect_equal(events$var, "Central")
    expect_equal(events$time, 0)
    expect_equal(events$value, 100)
})

test_that("Infusion dosing creates bag and rate compartments", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 0)
    infusion <- Dosing$new("Central", rate = 5, duration = 4, time = 8)
    M$addDosing(infusion)

    compartments <- M$getStateNames()
    expect_true("InfusionBag_Central" %in% compartments)
    expect_true("InfusionRate_Central" %in% compartments)

    events <- M$dosing_to_events()$data
    # One bolus to bag + start and end events for infusion rate
    expect_equal(sum(events$var == "InfusionBag_Central"), 1)
    expect_equal(sum(events$var == "InfusionRate_Central"), 2)
})
