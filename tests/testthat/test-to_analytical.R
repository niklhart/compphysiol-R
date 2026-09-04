
test_that("1-CMT analytical solution handles fixed and free parameters", {
    M <- compartment_model() |>
        add_compartment("cen", volume = NA_real_) |>
        add_molecule("drug", cmt = "cen", initial = 10, type = "amount") |>
        add_transport("cen", "", const = "kc0", molec = "drug")

    params <- list(kc0 = 1)
    times <- 0:3
    expected <- cbind(time = times, a_drug_cen = 10 * exp(-times))

    sol_free <- M |> 
        .to_analytical()
    sol_fixed <- M |> 
        add_parameter(param = do.call(parameters, params)) |> 
        .to_analytical()

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

    sol_aly <- .to_analytical(M)
    y_aly <- sol_aly$statefun(times, params = paramValues)

    sol_num <- .to_ode(M)
    y_num <- deSolve::ode(
        y = sol_num$y0,
        times = times,
        func = sol_num$odefun,
        parms = list()
    )
    attributes(y_num) <- attributes(y_num)[c("dim", "dimnames")]

    expect_equal(y_aly, y_num, tolerance = 1e-4)
})

test_that("analytical export accepts AnalyticalModel inputs", {
    M <- compartment_model() |>
        add_compartment("cen", volume = NA_real_) |>
        add_molecule("drug", cmt = "cen", initial = 10, type = "amount") |>
        add_transport("cen", "", const = "kc0", molec = "drug")
    analytical_model <- to_analytical_model(M)
    times <- 0:3
    params <- list(kc0 = 0.2)

    from_compartment_model <- .to_analytical(M)
    from_analytical_model <- .to_analytical(analytical_model)

    expect_equal(from_analytical_model$statefun(times, params), from_compartment_model$statefun(times, params))
    expect_equal(from_analytical_model$freeParams, "kc0")
})

test_that("analytical state function does not require observable-only parameters", {
    M <- compartment_model() |>
        add_compartment("cen", volume = NA_real_) |>
        add_molecule("drug", cmt = "cen", initial = 10, type = "amount") |>
        add_transport("cen", "", const = "kc0", molec = "drug") |>
        add_observable(C = a[drug, cen] / F)
    sol <- .to_analytical(M)
    times <- 0:3

    expect_equal(
        sol$statefun(times, params = list(kc0 = 0.2))[, "a_drug_cen"],
        10 * exp(-0.2 * times),
        tolerance = 1e-6
    )
    expect_equal(
        sol$obsFuncs$C(times, sol$statefun(times, params = list(kc0 = 0.2)), list(F = 2)),
        10 * exp(-0.2 * times) / 2,
        tolerance = 1e-6
    )
})

test_that("analytical export supports first-order reaction systems", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = 1) |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "kAB")
    times <- 0:3
    kAB <- 0.2

    sol <- .to_analytical(M)
    out <- sol$statefun(times, params = list(kAB = kAB))

    expect_equal(out[, "a_A_cyt"], 10 * exp(-kAB * times), tolerance = 1e-6)
    expect_equal(out[, "a_B_cyt"], 10 * (1 - exp(-kAB * times)), tolerance = 1e-6)
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

    sol_aly <- .to_analytical(M)
    sol_num <- .to_ode(M)

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
        tolerance = 1e-5
    )
})
