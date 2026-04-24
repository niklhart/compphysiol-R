# Test the interplay of the Compartments and CompartmentModel classes


test_that("Adding compartments works", {
    M <- compartment_model() |>
        add_compartment("Central", volume = 10) |>
        add_compartment("Peripheral", volume = 0)

    compnames <- names(M$compartments)
    expect_equal(compnames, c("Central", "Peripheral"))

})

test_that("Adding flows works", {
    M <- compartment_model() |>
        add_compartment("Central", 10) |>
        add_compartment("Peripheral", 0) |>
        add_transport("Central", "Peripheral", const = "k12")

    expect_equal(length(M$transports), 1)
    expect_equal(M$transports$from[[1]], "Central")
    expect_equal(M$transports$to[[1]], "Peripheral")
    expect_equal(M$transports$rate[[1]], quote(k12 * a[Central]))
})

test_that("Bolus dosing is handled correctly", {
    M <- compartment_model() |>
        add_compartment("Central") |>
        add_dosing(cmt = "Central", amount = 100, time = 2)

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

