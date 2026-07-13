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

test_that("wire treats NA compartments in states like old compartment shorthand", {

    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule(c("A", "B")) |>
        add_reaction(
            input = state(molec = "A", cmt = NA_character_),
            output = state(molec = "B", cmt = NA_character_),
            const = "k"
        ) |>
        wire()

    expect_equal(
        model$reactions,
        reactions(input = "A", output = "B", cmt = c("cyt", "nuc"), const = "k")
    )

    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule(c("A", "B")) |>
        add_reaction(
            input = state(molec = "A", cmt = NA_character_),
            output = state(molec = "B", cmt = NA_character_),
            rate = "k * c[A]"
        ) |>
        wire()

    expect_equal(
        vapply(model$reactions$rate, deparse1, character(1)),
        c("k * c[A, cyt]", "k * c[A, nuc]")
    )
})

test_that("wire expands partial NA state compartments in cross-compartment reactions", {

    model <- compartment_model() |>
        add_compartment(c("plasma", "membrane")) |>
        add_molecule("L") |>
        add_molecule(c("R", "LR"), cmt = "membrane") |>
        add_reaction(
            input = c(
                state(molec = "L", cmt = NA_character_),
                state(molec = "R", cmt = "membrane")
            ),
            output = state(molec = "LR", cmt = "membrane"),
            scale_cmt = "membrane",
            const = "kon"
        ) |>
        wire()

    expect_equal(length(model$reactions), 2)
    expect_equal(model$reactions$scale_cmt, c("membrane", "membrane"))
    expect_equal(
        vapply(model$reactions$rate, deparse1, character(1)),
        c(
            "kon * c[L, plasma] * c[R, membrane]",
            "kon * c[L, membrane] * c[R, membrane]"
        )
    )

    first_participants <- as.data.frame(model$reactions$participants[[1]])
    second_participants <- as.data.frame(model$reactions$participants[[2]])

    expect_equal(
        first_participants[, c("role", "molec", "cmt", "stoich")],
        data.frame(
            role = c("input", "input", "output"),
            molec = c("L", "R", "LR"),
            cmt = c("plasma", "membrane", "membrane"),
            stoich = c(1, 1, 1),
            stringsAsFactors = FALSE
        )
    )
    expect_equal(
        second_participants[, c("role", "molec", "cmt", "stoich")],
        data.frame(
            role = c("input", "input", "output"),
            molec = c("L", "R", "LR"),
            cmt = c("membrane", "membrane", "membrane"),
            stoich = c(1, 1, 1),
            stringsAsFactors = FALSE
        )
    )
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
