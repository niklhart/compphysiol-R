
test_that("1-CMT analytical solution handles fixed and free parameters", {
    M <- compartment_model() |>
        add_compartment("cen", volume = NA_real_) |>
        add_molecule("drug", cmt = "cen", initial = 10, type = "amount") |>
        add_transport("cen", "", const = "kc0", molec = "drug")

    params <- list(kc0 = 1)
    times <- 0:3
    expected <- cbind(time = times, a_drug_cen = 10 * exp(-times))

    sol_free <- M |> 
        to_analytical()
    sol_fixed <- M |> 
        add_parameter(param = do.call(parameters, params)) |> 
        to_analytical()

    expect_equal(sol_fixed$statefun(times), expected)
    expect_equal(sol_free$statefun(times, params), expected)
    expect_equal(sol_fixed$freeParams, character(0))
    expect_equal(sol_free$freeParams, "kc0")
})

test_that("2-CMT analytical solution matches numerical ODE solution", {
    paramValues <- list(kc0 = 1, kcp = 2, kpc = 3)
    times <- 0:3

    M <- compartment_model() |>
        add_compartment(c("cen", "per"), volume = NA_real_) |>
        add_molecule(
            "drug",
            cmt = c("cen", "per"),
            initial = c(10, 0),
            type = "amount"
        ) |>
        add_transport("cen", "", const = "kc0", molec = "drug") |>
        add_transport("cen", "per", const = "kcp", molec = "drug") |>
        add_transport("per", "cen", const = "kpc", molec = "drug") |>
        add_parameter(param = do.call(parameters, paramValues))

    sol_aly <- to_analytical(M)
    y_aly <- sol_aly$statefun(times, params = paramValues)

    sol_num <- to_ode(M)
    y_num <- deSolve::ode(
        y = sol_num$y0,
        times = times,
        func = sol_num$odefun,
        parms = list()
    )
    attributes(y_num) <- attributes(y_num)[c("dim", "dimnames")]

    expect_equal(y_aly, y_num, tolerance = 1e-4)
})

test_that("1-CMT analytical observables follow ODE observable contract", {
    params <- list(kc0 = 1, Vcen = 2)
    times <- 0:3

    M <- compartment_model() |>
        add_compartment("cen", volume = "Vcen") |>
        add_molecule("drug", cmt = "cen", initial = 10, type = "amount") |>
        add_transport("cen", "", const = "kc0", molec = "drug") |>
        add_observable(Ccen = c[drug, cen]) |>
        add_parameter(param = do.call(parameters, params))

    sol_aly <- to_analytical(M)
    sol_num <- to_ode(M)

    y_aly <- sol_aly$statefun(times)
    y_num <- deSolve::ode(
        y = sol_num$y0,
        times = times,
        func = sol_num$odefun,
        parms = list()
    )

    expect_identical(
        functionBody(sol_aly$obsFuncs$Ccen),
        functionBody(sol_num$obsFuncs$Ccen)
    )
    expect_equal(
        sol_aly$obsFuncs$Ccen(c(0, 2), y_aly, list()),
        c(5, 5 * exp(-2))
    )
    expect_equal(
        sol_num$obsFuncs$Ccen(c(0, 2), y_num, list()),
        c(5, 5 * exp(-2)),
        tolerance = 1e-6
    )
})
