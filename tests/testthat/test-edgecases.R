
test_that("Empty model returns zero-length ODE", {
    M <- compartment_model()
    odeinfo <- to_ode(M)

    expect_equal(length(odeinfo$y0), 0)
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())
    expect_equal(length(dydt[[1]]), 0)
})

test_that("Simulation fails if target compartment missing", {
    M <- compartment_model() |>
        add_compartment("Central", 10)

    # Dose references a compartment that will not exist
    M <- add_dosing(M, target = "Peripheral", amount = 10, time = 0)

    # Expected deSolve error
    expect_error({
        odeinfo <- to_ode(M, paramValues = list())
        deSolve::ode(y = odeinfo$y0, times = 0:10, func = odeinfo$odefun, parms = list(), events = odeinfo$events)
    })
})

test_that("Missing parameters are listed as free", {
    M <- compartment_model() |>
         add_compartment("Central", 10) |>
         add_compartment("Peripheral", 0) |>
         add_flow("Central", "Peripheral", const = "k12")

    # Do not provide k12
    odeinfo <- to_ode(M)
    expect_true("k12" %in% odeinfo$freeParams)
})

test_that("Overlapping infusion events handled correctly", {
    M <- compartment_model() |>
        add_compartment("Central", 0) |>
        add_dosing(
            cmt = "Central",
            time = c(0, 2),
            amount = 1,
            duration = 4,
            molec = "drug"
        ) |>
        make_depot()

    dos <- M$doses

    expect_length(dos, 6) # 2 doses + 4 infusion rate events
    expect_all_true(diff(dos$time) >= 0) # Events sorted by time
})
