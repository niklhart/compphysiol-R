# Test the interplay of the Compartments and CompartmentModel classes


test_that("Adding compartments works", {
    M <- compartment_model() |>
        add_compartment("Central", volume = 10) |>
        add_compartment("Peripheral", volume = 0)

    compnames <- names(M$compartments)
    expect_equal(compnames, c("Central", "Peripheral"))

})

test_that("Adding transports works", {
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

    dos <- M$doses

    expect_equal(length(dos), 1)
    expect_true(is_bolus(dos))
    
})

test_that("Infusion dosing creates bag and rate compartments only if requested", {
    M1 <- compartment_model() |>
        add_compartment("cen") |>
        add_dosing(time = 8, rate = 5, duration = 4, cmt = "cen", molec = "drug")
    M2 <- make_depot(M1)

    nm1 <- names(M1$compartments)
    nm2 <- names(M2$compartments)

    infusion_cmt <- c("Depot_drug_cen","ReleaseRate_drug_cen")

    expect_all_false(infusion_cmt %in% nm1)
    expect_all_true(infusion_cmt %in% nm2)

    expect_all_true(is_infusion(M1$doses))
    expect_all_true(is_bolus(M2$doses))

    expect_length(M1$doses, 1)
    expect_length(M2$doses, 3)
})

