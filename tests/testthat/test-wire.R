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