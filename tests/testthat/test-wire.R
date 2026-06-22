# Testing the `wire` function 

test_that("wire correctly adds compartments to molecules/reactions and molecules to transports", {

    # Create a simple model with compartments and molecules
    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule(c("A", "B")) |>
        add_transport(from = "cyt", to = "nuc", const = "k") |>
        add_reaction(input = "A", output = "B", const = "k2") |>
        wire()

    expect_equal(model$transports, transports(from = "cyt", to = "nuc", const = "k", molec = c("A", "B")))
    expect_equal(model$molecules, molecules(name = rep(c("A", "B"), each = 2), cmt = rep(c("cyt", "nuc"), times = 2)))
    expect_equal(model$reactions, reactions(input = "A", output = "B", const = "k2", cmt = c("cyt", "nuc")))
    
})

test_that("wire resolves scalar wildcard dosing targets", {

    model <- compartment_model() |>
        add_compartment("cyt") |>
        add_molecule("A", cmt = "cyt") |>
        add_dosing(time = 0, amount = 100) |>
        wire()

    expect_equal(length(model$doses), 1)
    expect_equal(model$doses$molec, "A")
    expect_equal(model$doses$cmt, "cyt")
})

test_that("wire errors for ambiguous wildcard dosing targets", {

    model_molec <- compartment_model() |>
        add_compartment("cyt") |>
        add_molecule(c("A", "B"), cmt = "cyt") |>
        add_dosing(time = 0, amount = 100, cmt = "cyt")

    expect_error(
        wire(model_molec),
        "Please specify dosing molec explicitly"
    )

    model_cmt <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule("A") |>
        add_dosing(time = 0, amount = 100, molec = "A")

    expect_error(
        wire(model_cmt),
        "Please specify dosing cmt explicitly"
    )
})

test_that("wire leaves explicit dosing targets intact", {

    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule(c("A", "B"), cmt = "cyt") |>
        add_dosing(time = 0, amount = 100, molec = "A", cmt = "cyt") |>
        wire()

    expect_equal(length(model$doses), 1)
    expect_equal(model$doses$molec, "A")
    expect_equal(model$doses$cmt, "cyt")
})

test_that("wire resolves scalar observable state shorthand", {

    model <- compartment_model() |>
        add_compartment("Central") |>
        add_molecule("drug", cmt = "Central") |>
        add_observable(C = a[Central] / V) |>
        wire()

    expect_equal(deparse1(model$observables$C), "a[drug, Central]/V")

    model <- compartment_model() |>
        add_molecule("drug") |>
        add_observable(A = a[drug]) |>
        wire()

    expect_equal(deparse1(model$observables$A), "a[drug, cmt]")
})

test_that("wire errors for ambiguous observable state shorthand", {

    model <- compartment_model() |>
        add_compartment("Central") |>
        add_molecule(c("parent", "metabolite"), cmt = "Central") |>
        add_observable(C = a[Central] / V)

    expect_error(
        wire(model),
        "Please specify observable molec explicitly"
    )
})
