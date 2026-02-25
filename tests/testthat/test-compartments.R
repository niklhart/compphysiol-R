
test_that("Adding compartments works", {
    M <- compartment_model() |>
        add_compartment("Central", initial = 10) |>
        add_compartment("Peripheral", initial = 0)

    compartments <- compartment_names(M)
    expect_equal(compartments, c("Central", "Peripheral"))

    initials <- initials(M, name = FALSE)
    expect_equal(initials, c(10, 0))
})

test_that("Adding flows works", {
    M <- compartment_model() |>
        add_compartment("Central", 10) |>
        add_compartment("Peripheral", 0) |>
        add_flow("Central", "Peripheral", "k12 * Central")

    expect_equal(length(M$flows), 1)
    f <- M$flows[[1]]
    expect_equal(f$from, "Central")
    expect_equal(f$to, "Peripheral")
    expect_equal(f$rate, quote(k12 * Central))
})

test_that("Bolus dosing is handled correctly", {
    M <- compartment_model() |>
        add_compartment("Central", 0) |>
        add_dosing(target = "Central", amount = 100, time = 2)

    compartments <- compartment_names(M)
    expect_false("InfusionBag_Central" %in% compartments)
    expect_false("InfusionRate_Central" %in% compartments)

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

    compartments <- compartment_names(M)
    expect_true("InfusionBag_Central" %in% compartments)
    expect_true("InfusionRate_Central" %in% compartments)

    # One bolus to bag + start and end events for infusion rate
    events_data <- to_ode(M)$events$data
    expect_equal(sum(events_data$var == "InfusionBag_Central"), 1)
    expect_equal(sum(events_data$var == "InfusionRate_Central"), 2)
})

