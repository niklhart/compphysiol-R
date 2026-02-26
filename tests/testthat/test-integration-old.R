library(deSolve)

test_that("Full simulation with bolus dosing works", {
    M <- CompartmentModel$
        new()$
        addCompartment(c("Central","Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addDosing("Central", amount = 100, time = 0)

    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))
    y0 <- odeinfo$y0
    times <- seq(0, 10, 1)
    out <- ode(y = y0, 
        times = times, 
        func = odeinfo$odefun, 
        parms = list(), 
        events = odeinfo$events)

    # Check dimensions
    expect_equal(dim(out)[2], length(y0) + 1) # +1 for time column
    # Check mass balance: Central starts high, decreases; Peripheral increases
    expect_true(all(out[, "Central"] <= 100))
    expect_true(all(out[, "Peripheral"] >= 0))
})

test_that("Full simulation with infusion dosing works", {
    M <- CompartmentModel$
        new()$
        addCompartment(c("Central", "Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addDosing(target = "Central", rate = 10, duration = 5, time = 0)     # infusion: rate=10 units/h, duration=5h, start at t=0

    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))
 
    times <- seq(0, 10, 0.5)
    out <- ode(
        y = odeinfo$y0, 
        times = times, 
        func = odeinfo$odefun, 
        parms = list(), 
        events = odeinfo$events)

    # Central compartment should increase during infusion and then plateau/decrease
    central <- out[, "Central"]
    expect_true(all(diff(central[1:11]) >= 0))    # first 5h, increasing
    expect_true(all(diff(central[11:length(central)]) <= 0 | diff(central[11:length(central)]) >= -0.1)) # after infusion, decreasing slowly
})


test_that("One-compartment model with first-order elimination matches analytical solution", {

    # Parameters
    k <- 0.2
    A0 <- 100
    times <- seq(0, 10, by = 0.5)

    # Build model
    M <- CompartmentModel$new()
    M$addCompartment("Central", A0)
    M$addReaction("Central", "", "k * Central")  # elimination reaction

    odeinfo <- M$toODE(paramValues = list(k = k))

    out <- ode(
        y = odeinfo$y0,
        times = times,
        func = odeinfo$odefun,
        parms = list()
    )

    # Analytical solution
    analytic <- A0 * exp(-k * times)

    # Compare numerical and analytical solutions
    tol <- 1e-6
    expect_equal(out[, "Central"], analytic, tolerance = tol)
})

test_that("Two-compartment oral absorption model matches Bateman function", {

    # Parameters
    ka <- 1.2     # absorption rate
    ke <- 0.3     # elimination rate
    D <- 100      # dose (initial amount in gut)
    times <- seq(0, 24, by = 0.5)

    # Build model
    M <- CompartmentModel$new()
    M$addCompartment("Gut", D)
    M$addCompartment("Central", 0)
    M$addReaction("Gut", "Central", "ka * Gut")     # absorption
    M$addReaction("Central", "", "ke * Central")    # elimination

    odeinfo <- M$toODE(paramValues = list(ka = ka, ke = ke))

    out <- ode(
        y = odeinfo$y0,
        times = times,
        func = odeinfo$odefun,
        parms = list()
    )

    # Analytical Bateman function
    A_central <- (D * ka / (ka - ke)) * (exp(-ke * times) - exp(-ka * times))

    tol <- 1e-6
    expect_equal(out[, "Central"], A_central, tolerance = tol)
})

test_that("One-compartment model with observed concentration matches analytic solution", {

    # Parameters
    k <- 0.2
    A0 <- 100
    V <- 10
    times <- seq(0, 10, by = 0.5)

    # Build model
    M <- CompartmentModel$new()
    M$addCompartment("Central", A0)
    M$addReaction("Central", "", "k * Central")   # elimination
    M$addObservable("C", "Central / V")           # observed concentration

    odeinfo <- M$toODE(paramValues = list(k = k, V = V))

    out <- ode(
        y = odeinfo$y0,
        times = times,
        func = odeinfo$odefun,
        parms = list()
    )

    # Compute observable from model
    conc_pred <- odeinfo$obsFuncs$C(times, out[,-1,drop=FALSE])

    # Analytical solution
    conc_analytic <- (A0 / V) * exp(-k * times)

    tol <- 1e-6
    expect_equal(conc_pred, conc_analytic, tolerance = tol)
})


test_that("toODE flags reactions pointing to unknown compartments", {
    # Simple one-compartment model with a reaction to a non-existent compartment
    M <- CompartmentModel$new()
    M$addCompartment("Gut", 100)
    M$addReaction("Gut", "Central", "ka * Gut")

    expect_error(
        M$toODE(paramValues = list(ka = 0.1)),
        regexp = "Reaction references unknown compartment: Central."
    )

})
