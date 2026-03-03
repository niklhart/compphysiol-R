# Test the interplay of the Compartments and CompartmentModel classes


test_that("Adding compartments works", {
    M <- compartment_model() |>
        add_compartment("Central", initial = 10) |>
        add_compartment("Peripheral", initial = 0)

    compnames <- names(M$compartments)
    expect_equal(compnames, c("Central", "Peripheral"))

    initials <- initials(M$compartments, named = FALSE)
    expect_equal(initials, c(10, 0))
})

test_that("Adding flows works", {
    M <- compartment_model() |>
        add_compartment("Central", 10) |>
        add_compartment("Peripheral", 0) |>
        add_flow("Central", "Peripheral", const = "k12")

    expect_equal(length(M$flows), 1)
    expect_equal(M$flows$from[[1]], "Central")
    expect_equal(M$flows$to[[1]], "Peripheral")
    expect_equal(M$flows$rate[[1]], quote(k12 * Central))
})

test_that("Bolus dosing is handled correctly", {
    M <- compartment_model() |>
        add_compartment("Central", 0) |>
        add_dosing(target = "Central", amount = 100, time = 2)

    compnames <- names(M$compartments)
    expect_false("InfusionBag_Central" %in% compnames)
    expect_false("InfusionRate_Central" %in% compnames)

    events_data <- to_ode(M)$events$data
    expect_equal(nrow(events_data), 1)
    expect_equal(events_data$var, "Central")
    expect_equal(events_data$time, 2)
    expect_equal(events_data$value, 100)
})

test_that("Infusion dosing creates bag and rate compartments", {
    M <- compartment_model() |>
        add_compartment("Central", 0) |>
        add_dosing("Central", rate = 5, duration = 4, time = 8)

    compnames <- names(M$compartments)
    expect_all_true(c("InfusionBag_Central", "InfusionRate_Central") %in% compnames)

    # One bolus to bag + start and end events for infusion rate
    events_data <- to_ode(M)$events$data
    expect_equal(sum(events_data$var == "InfusionBag_Central"), 1)
    expect_equal(sum(events_data$var == "InfusionRate_Central"), 2)
})

