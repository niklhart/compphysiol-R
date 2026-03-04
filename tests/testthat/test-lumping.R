test_that("lumping is exact in a 3-CMT model with identical peripherals", {

    M <- multiCompModel(ncomp = 3, type = "micro") |>
        add_dosing(target = "comp1", time = 0, amount = 10) |>
        add_parameter(k10 = 5, V1 = 1, k12 = 5, k13 = 10, k21 = 3, k31 = 3)

    L <- lump_model(M,
                    partitioning = list(c("comp2","comp3")),
                    normalize = list(
                        comp2 = "k21",
                        comp3 = "k31"
                    )
    )

    odeinfoM <- to_ode(M)
    odeinfoL <- to_ode(L)

    times <- 0:6

    outM <- deSolve::ode(y = odeinfoM$y0, times = times, func = odeinfoM$odefun, events = odeinfoM$events)
    outL <- deSolve::ode(y = odeinfoL$y0, times = times, func = odeinfoL$odefun, events = odeinfoL$events)

    expect_equal(outM[,2],outL[,2], tolerance = 1e-5)
    expect_equal(outM[,3]+outM[,4], outL[,3] , tolerance = 1e-5)
})


test_that("lumping works for 2-CMT blood/tissue model with CL", {

    M <- compartment_model() |>
        add_compartment(c("blo","tis"), c(10,0)) |>
        add_flow("blo", "tis", const = "Q/Vblo") |>
        add_flow("tis", "blo", const = "Q/(Vtis*Ktis)") |>
        add_flow("tis", "", const = "CL/(Vtis*Ktis)") |>
        add_parameter(Q = 1, Vblo = 1, Vtis = 3, Ktis = 10, CL = 2)

    L <- lump_model(M,
                    partitioning = list(c("blo","tis")),
                    normalize = list(
                        blo = "Vblo",
                        tis = "Vtis*Ktis*Q/(Q+CL)"
                    ))

    Lref <- compartment_model() |>
        add_compartment("blo_tis", 10) |>
        add_flow("blo_tis", "", const = "CL*Q/(Vblo*(Q+CL)+Vtis*Ktis*Q)") |> 
        add_parameter(Q = 1, Vblo = 1, Vtis = 3, Ktis = 10, CL = 2)

    times <- 0:6

    odeinfoL    <- to_ode(L)
    odeinfoLref <- to_ode(Lref)

    outL    <- deSolve::ode(y = odeinfoL$y0, times = times, func = odeinfoL$odefun)
    outLref <- deSolve::ode(y = odeinfoLref$y0, times = times, func = odeinfoLref$odefun)

    expect_equal(outL,outLref)

})

test_that("lumping handles first-pass effect correctly", {

    skip("Illustration of first-pass effect issue")

    M <- compartment_model() |>
        add_compartment(c("gut","liv","blo"), c(10, 0, 0)) |>
        add_flow("gut", "liv", const = "ka") |>
        add_flow("liv", "blo", const = "Q/(Vliv*Kliv)") |>
        add_flow("blo", "liv", const = "Q/Vblo") |>
        add_flow("liv", "", const = "CL/(Vliv*Kliv)") |>
        add_parameter(ka = 1, Q = 1, Vliv = 1, Kliv = 10, Vblo = 10, CL = 10)

    L <- lump_model(M,
                    partitioning = list(sys=c("blo","liv")),
                    normalize = list(
                        blo = "Vblo",
                        liv = "Vliv*Kliv*Q/(Q+CL)"
                    ))

    times <- 0:6

    odeinfoM    <- to_ode(M)
    odeinfoL    <- to_ode(L)

    outM    <- deSolve::ode(y = odeinfoM$y0, times = times, func = odeinfoM$odefun)
    outL    <- deSolve::ode(y = odeinfoL$y0, times = times, func = odeinfoL$odefun)

    # blo+liv and sys should be in the same ballpark, but they are very
    # different due to the first-pass effect.
    expect_equal(
        outL[,'sys'],
        rowSums(outM[,c('blo','liv')]),
        tol = 1
    )

})

