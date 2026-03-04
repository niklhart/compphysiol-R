
test_that("dosing + compartment amount mismatch errors", {
    M1 <- compartment_model() |>
        add_compartment(name = 'cen', unit = "g") |>
        add_dosing(target = 'cen', time = 0[h], amount = 1[mol])
    expect_error(compphysiol:::.check_unit_consistency(M1), "mol vs. g")

    M2 <- compartment_model() |>
        add_compartment(name = 'cen', unit = "g") |>
        add_dosing(target = 'cen', time = 0[h], amount = 1)
    expect_error(
        compphysiol:::.check_unit_consistency(M2),
        "one has units while the other does not"
    )
})

test_that("flow rate + compartment amount unit mismatch errors", {

    # Test for linear flow with rate constant unit mismatch
    M1 <- compartment_model() |>
        add_compartment(name = 'cen', unit = "g") |>
        add_compartment(name = 'per', unit = "mol") |>
        add_flow(from = 'cen', to = 'per', const = "k") |>
        add_parameter(k = 0.1 [1/h])
    expect_error(compphysiol:::.check_unit_consistency(M1), "g vs. mol")

    M2 <- compartment_model() |>
        add_compartment(name = c('cen','per'), unit = "g") |>
        add_flow(from = 'cen', to = 'per', const = "k") |>
        add_parameter(k = 0.1 [g/h])
    expect_error(compphysiol:::.check_unit_consistency(M2), "must be type 1/Time")

    M3 <- compartment_model() |>
        add_compartment(name = c('cen','per'), unit = "g") |>
        add_flow(from = 'cen', to = 'per', const = "k")
    expect_warning(compphysiol:::.check_unit_consistency(M3), "in the rate constant are not defined in the model")

    # Test for nonlinear flow with rate expression unit mismatch
    M4 <- compartment_model() |>
        add_compartment(name = 'cen', unit = "g") |>
        add_compartment(name = 'per', unit = "g") |>
        add_flow(from = 'cen', to = 'per', rate = "k * Acen") |>
        add_parameter(k = 0.1 [h])
    expect_error(compphysiol:::.check_unit_consistency(M4), "must be compatible with unit of compartment per time")

    M5 <- compartment_model() |>
        add_compartment(name = 'cen', unit = "g") |>
        add_compartment(name = 'per', unit = "g") |>
        add_flow(from = 'cen', to = 'per', rate = "k + Acen") |>
        add_parameter(k = 0.1 [1/h])
    expect_error(compphysiol:::.check_unit_consistency(M5), "unit inconsistency in rate expression")

})

test_that("unit mismatch in observable definition errors", {
    M <- compartment_model() |>
        add_compartment(name = 'cen', unit = "g") |>
        add_observable(Ccen = Acen + Vcen) |>
        add_parameter(Vcen = 10 [L])
    expect_error(compphysiol:::.check_unit_consistency(M), "unit inconsistency in expression")
})

test_that("unit mismatch in equation definition errors", {
    M <- compartment_model() |>
        add_equation(co = Q + V) |>
        add_parameter(V = 10 [L], Q = 0.5 [L/h])
    expect_error(compphysiol:::.check_unit_consistency(M), "unit inconsistency in expression")
})