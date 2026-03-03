test_that("1-CMT model can be solved analytically with / without hard-wired parameters", {

    # Testing 1-CMT model with and without hard-wired parameters
    M <- multiCompModel(ncomp = 1, type = "micro")
    M$compartments$initial <- list(10)

    params <- list(k10 = 1)

    sol1 <- to_analytical(M, paramValues = params)
    y1 <- sol1$statefun(t = 0:3)

    sol2 <- to_analytical(M)
    y2 <- sol2$statefun(t = 0:3, params)

    expect_equal(y1,y2)

})

test_that("2-CMT model can be solved analytically", {

    # Testing 2-CMT model via ODE or analytical solution
    M <- multiCompModel(ncomp = 2, type = "micro")
    M$compartments$initial[[1]] <- 10

    params <- list(k10 = 1, k12 = 2, k21 = 3)

    sol_aly <- to_analytical(M, paramValues = params)
    y_aly <- sol_aly$statefun(t = 0:3)

    sol_num <- to_ode(M, paramValues = params)
    y_num <- deSolve::ode(y=sol_num$y0, times = 0:3, func = sol_num$odefun)
    attributes(y_num) <- attributes(y_num)[c("dim", "dimnames")]

    expect_equal(y_aly,y_num, tolerance = 1e-4)

})

test_that("1-CMT model observables are correctly handled", {

    M <- multiCompModel(ncomp = 1, type = "micro")
    M$compartments$initial[[1]] <- 10

    params <- list(k10 = 1, V1 = 2)

    sol_aly <- to_analytical(M, paramValues = params)
    sol_num <- to_ode(M, paramValues = params)

    expect_identical(
        functionBody(sol_num$obsFuncs$C1Conc),
        functionBody(sol_aly$obsFuncs$C1Conc)
    )

})
