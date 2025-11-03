
test_that("ODE generation handles first-order reaction with bolus dosing", {

    # PK example with first-order reactions only
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)
    M$addReaction("Central", "Peripheral", "k12 * Central")
  
    odeinfo <- M$toODE(paramValues = list(k12 = 0.1))

    # Function generation and correct state names
    expect_true(is.function(odeinfo$odefun))
    expect_equal(odeinfo$stateNames, c("Central", "Peripheral"))

    y0   <- odeinfo$y0
    dydt <- odeinfo$odefun(0, y0, list())

    # Size of output
    expect_equal(length(dydt[[1]]), 2)

    # Central decreases, Peripheral increases
    expect_true(dydt[[1]][1] < 0)
    expect_true(dydt[[1]][2] > 0)

})


test_that("ODE generation handles various reaction orders", {

    # Systems biology example with zero- and second-order reactions
    M <- CompartmentModel$new()
    M$addCompartment("S", 0)
    M$addCompartment("S2", 1)
    M$addReaction("", "S", "ksyn")               # zero-order synthesis
    M$addReaction("S", "", "kdeg*S")             # first-order elimination
    M$addReaction(c("S","S"), "S2", "kass*S^2")  # second-order association
    M$addReaction("S2", c("S","S"), "kdis*S2")   # first-order dissociation
    
    odeinfo <- M$toODE(paramValues = list(ksyn = 1, kdeg = 2, kass = 0.1, kdis = 0.5))
    expect_true(is.function(odeinfo$odefun))
    expect_equal(odeinfo$stateNames, c("S", "S2"))

    y0   <- odeinfo$y0
    expect_equal(y0, c(S=0, S2=1))
  
    dydt <- odeinfo$odefun(0, y0, list())
    expect_equal(dydt[[1]], c(2,-0.5))


})
