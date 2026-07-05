
test_that("dosing + compartment amount mismatch errors", {
    M1 <- compartment_model() |>
        add_compartment(name = 'cen', volume = 1 [L]) |>
        add_molecule(name = 'drug', cmt = 'cen', unit = "g", type = "amount") |>
        add_dosing(time = 0[h], amount = 1[mol], cmt = 'cen', molec = 'drug')
    expect_error(compphysiol:::.check_unit_consistency(M1), "mol vs. g")

    M2 <- compartment_model() |>
        add_compartment(name = 'cen') |>
        add_molecule(name = 'drug', cmt = 'cen', unit = "g", type = "amount") |>
        add_dosing(cmt = 'cen', time = 0[h], amount = 1, molec = "drug")
    expect_error(
        compphysiol:::.check_unit_consistency(M2),
        "one has units while the other does not"
    )
})

test_that("transport rate + compartment amount unit mismatch errors", {

    # Test for linear transport with rate constant unit mismatch
    M1 <- compartment_model() |>
        add_compartment(name = c('cen','per')) |>
        add_molecule(name = 'drug', cmt = c('cen','per'), unit = c("g","mol"), type = "amount") |>
        add_transport(from = 'cen', to = 'per', const = "k", molec = "drug") |>
        add_parameter(k = 0.1 [1/h])
    expect_error(compphysiol:::.check_unit_consistency(M1), "g vs. mol")

    M2 <- compartment_model() |>
        add_compartment(name = c('cen','per')) |>
        add_molecule(name = 'drug', cmt = c('cen','per'), unit = "g", type = "amount") |>
        add_transport(from = 'cen', to = 'per', const = "k", molec = "drug") |>
        add_parameter(k = 0.1 [g/h])
    expect_error(compphysiol:::.check_unit_consistency(M2), "must be type 1/Time")

    M3 <- compartment_model() |>
        add_compartment(name = c('cen','per')) |>
        add_molecule(name = 'drug', cmt = c('cen','per'), unit = "g", type = "amount") |>
        add_transport(from = 'cen', to = 'per', const = "k", molec = "drug")
    expect_warning(compphysiol:::.check_unit_consistency(M3), "in the rate constant are not defined in the model")

    # Test for nonlinear transport with rate expression unit mismatch
    M4 <- compartment_model() |>
        add_compartment(name = c('cen','per')) |>
        add_molecule(name = 'drug', cmt = c('cen','per'), unit = "g", type = "amount") |>
        add_transport(from = 'cen', to = 'per', rate = "k * a[drug, cen]", molec = "drug") |>
        add_parameter(k = 0.1 [h])
    expect_error(compphysiol:::.check_unit_consistency(M4), "must be compatible with unit of compartment per time")

    M5 <- compartment_model() |>
        add_compartment(name = c('cen','per')) |>
        add_molecule(name = 'drug', cmt = c('cen','per'), unit = "g", type = "amount") |>
        add_transport(from = 'cen', to = 'per', rate = "k + a[drug, cen]", molec = 'drug') |>
        add_parameter(k = 0.1 [1/h])
    expect_error(compphysiol:::.check_unit_consistency(M5), "unit inconsistency in rate expression")

})

test_that("unit mismatch in observable definition errors", {
    M <- compartment_model() |>
        add_compartment(name = 'cen') |>
        add_molecule(name = 'drug', cmt = 'cen', unit = "g", type = "amount") |>
        add_observable(Acen = a[drug, cen] + Vcen) |>
        add_parameter(Vcen = 10 [L])
    expect_error(compphysiol:::.check_unit_consistency(M), "unit inconsistency in expression")
})

test_that("unit mismatch in equation definition errors", {
    M <- compartment_model() |>
        add_equation(co = Q + V) |>
        add_parameter(V = 10 [L], Q = 0.5 [L/h])
    expect_error(compphysiol:::.check_unit_consistency(M), "unit inconsistency in expression")
})

test_that("zero-order reaction constants have concentration/time units", {
    skip("Reaction definitions are currently not checked for unit consistency, but should be in the future.")

    M_valid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = "A", cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(input = NULL, output = "A", cmt = "cyt", const = "ksyn") |>
        add_parameter(ksyn = 1 [mol/L/h])
    expect_silent(compphysiol:::.check_unit_consistency(M_valid))

    M_invalid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = "A", cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(input = NULL, output = "A", cmt = "cyt", const = "ksyn") |>
        add_parameter(ksyn = 1 [mol/h])
    expect_error(compphysiol:::.check_unit_consistency(M_invalid), "reaction")
})

test_that("first-order reaction constants have inverse-time units", {
    skip("Reaction definitions are currently not checked for unit consistency, but should be in the future.")

    M_valid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = c("A", "B"), cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "kAB") |>
        add_parameter(kAB = 1 [1/h])
    expect_silent(compphysiol:::.check_unit_consistency(M_valid))

    M_invalid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = c("A", "B"), cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "kAB") |>
        add_parameter(kAB = 1 [L/h])
    expect_error(compphysiol:::.check_unit_consistency(M_invalid), "reaction")
})

test_that("second-order reaction constants convert concentration products to concentration/time", {
    skip("Reaction definitions are currently not checked for unit consistency, but should be in the future.")

    M_valid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = c("A", "B", "C"), cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(input = c("A", "B"), output = "C", cmt = "cyt", const = "kABC") |>
        add_parameter(kABC = 1 [L/mol/h])
    expect_silent(compphysiol:::.check_unit_consistency(M_valid))

    M_invalid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = c("A", "B", "C"), cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(input = c("A", "B"), output = "C", cmt = "cyt", const = "kABC") |>
        add_parameter(kABC = 1 [L^2/mol/h])
    expect_error(compphysiol:::.check_unit_consistency(M_invalid), "reaction")
})

test_that("complex reaction rates have concentration/time units", {
    skip("Reaction definitions are currently not checked for unit consistency, but should be in the future.")

    M_valid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = c("A", "B"), cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(
            input = "A",
            output = "B",
            cmt = "cyt",
            rate = "vmax * c[A, cyt] / (Km + c[A, cyt])"
        ) |>
        add_parameter(vmax = 1 [mol/L/h], Km = 1 [mol/L])
    expect_silent(compphysiol:::.check_unit_consistency(M_valid))

    M_invalid <- compartment_model() |>
        add_compartment(name = "cyt", volume = 1 [L]) |>
        add_molecule(name = c("A", "B"), cmt = "cyt", unit = "mol", type = "amount") |>
        add_reaction(
            input = "A",
            output = "B",
            cmt = "cyt",
            rate = "vmax * c[A, cyt] / (Km + c[A, cyt])"
        ) |>
        add_parameter(vmax = 1 [mol/h], Km = 1 [mol/L])
    expect_error(compphysiol:::.check_unit_consistency(M_invalid), "reaction")
})
