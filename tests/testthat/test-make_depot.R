# Testing the `make_depot()` function

test_that("make_depot correctly creates depot/rate compartments and adds them to the model", {
    
    # Create a simple model with compartments and molecules
    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule(c("A", "B")) |>
        add_dosing(time  = 0, amount = 100, duration = 2, cmt = "cyt", molec = "A") |>
        wire() |>
        make_depot()
    
    expect_equal(model$compartments[3:4], compartments(c("Depot_A_cyt", "ReleaseRate_A_cyt"), volume = NA_real_))
    expect_equal(model$transports, transports(from = "Depot_A_cyt", to = "cyt", rate = "a[ReleaseRate_A_cyt]", molec = "A"))
    expect_equal(
        model$doses, 
        c(
            dosing(time = 0, amount = 100, cmt = "Depot_A_cyt", molec = "A"),
            dosing(time = 0, amount = 50, cmt = "ReleaseRate_A_cyt", molec = "A"),
            dosing(time = 2, amount = -50, cmt = "ReleaseRate_A_cyt", molec = "A")
        )
    )
})
