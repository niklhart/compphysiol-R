
test_that("ODE generation handles first-order reaction with bolus dosing", {

    # PK example with first-order reactions only
    M <- compartment_model() |>
        add_compartment("Central", volume = 10) |>
        add_compartment("Peripheral", volume = 5) |>
        add_molecule(
            name = c("drug", "drug"),
            cmt = c("Central", "Peripheral"),
            initial = c(10, 0),
            type = "amount"
        ) |>
        add_transport("Central", "Peripheral", const = "k12") |>
        add_parameter(k12 = 0.1)

    odeinfo <- to_ode(M)

    # Function generation and correct state names
    expect_true(is.function(odeinfo$odefun))
    expect_equal(odeinfo$stateNames, c("a_drug_Central", "a_drug_Peripheral"))
    expect_equal(odeinfo$dslStateNames, c("a[drug, Central]", "a[drug, Peripheral]"))

    y0   <- odeinfo$y0
    dydt <- odeinfo$odefun(0, y0, list())

    # Size of output
    expect_equal(length(dydt[[1]]), 2)

    # Central decreases, Peripheral increases
    expect_true(dydt[[1]][1] < 0)
    expect_true(dydt[[1]][2] > 0)

})

test_that("ODE generation handles output dimensions", {
    # PK example with first-order reactions only
    M <- compartment_model() |>
        add_compartment("Central") |>
        add_molecule("drug", cmt = "Central", initial = 10[mg], type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke") |>
        add_parameter(ke = 6 [1/h]) |>
        add_dosing(cmt = "Central", amount = 100[mg], time = 1[h])

    odeinfo <- to_ode(M, dimensions = list(mass = "g", time = "min"))

    expect_equal(odeinfo$y0, c(a_drug_Central = 0.01))
    expect_equal(odeinfo$events$data$var, "a_drug_Central")
    expect_equal(odeinfo$events$data$time, 60)
    expect_equal(odeinfo$events$data$value, 0.1)
})

test_that("ODE generation shortens auto-placeholder state names", {
    M <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = 0) |>
        add_transport("Central", "Peripheral", const = "k12") |>
        add_parameter(k12 = 0.1)

    odeinfo <- to_ode(M)

    expect_equal(odeinfo$stateNames, c("a_Central", "a_Peripheral"))
    expect_equal(odeinfo$dslStateNames, c("a[molec, Central]", "a[molec, Peripheral]"))

    M <- compartment_model() |>
        add_molecule("drug", initial = 1, type = "amount")

    odeinfo <- to_ode(M)

    expect_equal(odeinfo$stateNames, "a_drug")
    expect_equal(odeinfo$dslStateNames, "a[drug, cmt]")
})

test_that("ODE observables convert between amount and concentration states", {
    M_amount <- compartment_model() |>
        add_compartment("Central", volume = 10) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_observable(C = c[drug, Central])

    odeinfo <- to_ode(M_amount)
    y <- cbind(time = 0, a_drug_Central = 100)
    expect_equal(odeinfo$obsFuncs$C(0, y, list()), 10)

    M_conc <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = 10, type = "concentration") |>
        add_observable(A = a[drug, Central]) |>
        add_parameter(V = 10)

    odeinfo <- to_ode(M_conc)
    y <- cbind(time = 0, a_drug_Central = 100)
    expect_equal(odeinfo$obsFuncs$A(0, y, list()), 100)
})

test_that("ODE observable functions are unit-free after export", {
    M <- compartment_model() |>
        add_compartment("Central", volume = 10 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_observable(C = c[drug, Central])

    odeinfo <- to_ode(M, dimensions = list(mass = "mg", length = "m"))
    y <- cbind(time = 0, a_drug_Central = 100)
    value <- odeinfo$obsFuncs$C(0, y, list())

    expect_false(inherits(value, "units"))
    expect_equal(value, 10000)
})

test_that("ODE generation evaluates parametrized initial values", {
    M <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(A0 = 100, ke = 0.2)

    odeinfo <- to_ode(M)

    expect_equal(odeinfo$y0, c(a_drug_Central = 100))
    expect_equal(odeinfo$freeParams, character(0))
})

test_that("ODE generation evaluates parametrized initials with unit dimensions", {
    M <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = "C0", type = "concentration") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(C0 = 10 [mg/L], V = 2 [L], ke = 0.2 [1/h])

    odeinfo <- to_ode(M, dimensions = list(mass = "mg", length = "dm", time = "h"))

    expect_equal(odeinfo$y0, c(a_drug_Central = 20))
    expect_equal(odeinfo$freeParams, character(0))
})

test_that("ODE generation reports unresolved parametrized initial values", {
    M <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2)

    expect_error(
        to_ode(M),
        "initial.*A0|A0.*initial"
    )
})


test_that("ODE generation processes equations correctly", {
    # 1-CMT model with redefined elimination rate constant
    M <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 1, type = "amount") |>
        add_transport(from = "Central", to = "", const = "ke_eq") |>
        add_parameter(ke_par = 1) |>
        add_equation(ke_eq = ke_par)

    odeinfo <- to_ode(M)

    expect_equal(odeinfo$freeParams, character(0))
    expect_no_error(odeinfo$odefun(0, odeinfo$y0))

})

test_that("ODE generation includes elementary reactions", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = 2) |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "kAB") |>
        add_parameter(kAB = 2)

    odeinfo <- to_ode(M)
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())[[1]]

    expect_equal(
        setNames(dydt, odeinfo$stateNames),
        c(a_A_cyt = -20, a_B_cyt = 20)
    )
})

test_that("ODE generation supports concentration states for reaction-only models", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule(
            c("A", "B"),
            cmt = "cyt",
            initial = c(10, 0),
            type = "concentration"
        ) |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "kAB") |>
        add_parameter(kAB = 2)

    odeinfo <- to_ode(M)
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())[[1]]

    expect_equal(odeinfo$dslStateNames, c("c[A, cyt]", "c[B, cyt]"))
    expect_equal(odeinfo$stateNames, c("c_A_cyt", "c_B_cyt"))
    expect_equal(
        setNames(dydt, odeinfo$stateNames),
        c(c_A_cyt = -20, c_B_cyt = 20)
    )
})

test_that("ODE generation includes complex reaction rates", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = 1) |>
        add_molecule(
            c("A", "B", "C"),
            cmt = "cyt",
            initial = c(10, 5, 0),
            type = "amount"
        ) |>
        add_reaction(
            input = c("A", "B"),
            output = "C",
            cmt = "cyt",
            rate = "vmax * c[A, cyt] * c[B, cyt] / (Km + c[A, cyt])"
        ) |>
        add_parameter(vmax = 2, Km = 10)

    odeinfo <- to_ode(M)
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())[[1]]

    expect_equal(
        setNames(dydt, odeinfo$stateNames),
        c(a_A_cyt = -5, a_B_cyt = -5, a_C_cyt = 5)
    )
})

test_that("ODE generation includes reaction synthesis and degradation", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = 1) |>
        add_molecule("A", cmt = "cyt", initial = 10, type = "amount") |>
        add_reaction(input = NULL, output = "A", cmt = "cyt", const = "ksyn") |>
        add_reaction(input = "A", output = NULL, cmt = "cyt", const = "kdeg") |>
        add_parameter(ksyn = 3, kdeg = 0.5)

    odeinfo <- to_ode(M)
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())[[1]]

    expect_equal(setNames(dydt, odeinfo$stateNames), c(a_A_cyt = -2))
})

test_that("reaction rates can use equations in ODE export", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = 1) |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "k_eq") |>
        add_equation(k_eq = k1 + k2) |>
        add_parameter(k1 = 1, k2 = 2)

    odeinfo <- to_ode(M)
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())[[1]]

    expect_equal(odeinfo$freeParams, character(0))
    expect_equal(
        setNames(dydt, odeinfo$stateNames),
        c(a_A_cyt = -30, a_B_cyt = 30)
    )
})

test_that("reaction ODEs conserve mass for reversible reactions", {
    times <- seq(0, 10, by = 1)

    M1 <- compartment_model() |>
        add_compartment(c("cyt", "nuc"), volume = 1) |>
        add_molecule("A", cmt = c("cyt", "nuc"), initial = c(10, 5), type = "amount") |>
        add_molecule("B", cmt = c("cyt", "nuc"), initial = c(0, 2), type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = c("cyt", "nuc"), const = "kAB") |>
        add_reaction(input = "B", output = "A", cmt = c("cyt", "nuc"), const = "kBA") |>
        add_parameter(kAB = 0.2, kBA = 0.1)

    odeinfo1 <- to_ode(M1)
    out1 <- deSolve::ode(y = odeinfo1$y0, times = times, func = odeinfo1$odefun)
    total1 <- rowSums(out1[, c("a_A_cyt", "a_A_nuc", "a_B_cyt", "a_B_nuc")])

    expect_equal(total1, rep(total1[[1]], length(total1)), tolerance = 1e-8)

    M2 <- compartment_model() |>
        add_compartment(c("cyt", "nuc"), volume = 1) |>
        add_molecule("A", cmt = c("cyt", "nuc"), initial = c(10, 5), type = "amount") |>
        add_molecule("B", cmt = c("cyt", "nuc"), initial = c(0, 2), type = "amount") |>
        add_reaction(input = c("A", "A"), output = "B", cmt = c("cyt", "nuc"), const = "kAB") |>
        add_reaction(input = "B", output = c("A", "A"), cmt = c("cyt", "nuc"), const = "kBA") |>
        add_parameter(kAB = 0.02, kBA = 0.1)

    odeinfo2 <- to_ode(M2)
    out2 <- deSolve::ode(y = odeinfo2$y0, times = times, func = odeinfo2$odefun)
    total_A <- rowSums(out2[, c("a_A_cyt", "a_A_nuc")])
    total_B <- rowSums(out2[, c("a_B_cyt", "a_B_nuc")])
    total2 <- total_A + 2 * total_B

    expect_equal(total2, rep(total2[[1]], length(total2)), tolerance = 1e-8)
})

test_that("ODE generation supports cross-compartment reaction participants", {
    M <- compartment_model() |>
        add_compartment(c("plasma", "membrane"), volume = c(10, 2)) |>
        add_molecule("L", cmt = "plasma", initial = 100, type = "amount") |>
        add_molecule("R", cmt = "membrane", initial = 20, type = "amount") |>
        add_molecule("LR", cmt = "membrane", initial = 0, type = "amount") |>
        add_reaction(
            input = state(
                molec = c("L", "R"),
                cmt = c("plasma", "membrane")
            ),
            output = state(molec = "LR", cmt = "membrane"),
            scale_cmt = "membrane",
            const = "kon"
        ) |>
        add_parameter(kon = 1)

    expect_silent(odeinfo <- to_ode(M))
    dydt <- odeinfo$odefun(0, odeinfo$y0, list())[[1]]

    expect_equal(odeinfo$stateNames, c("a_L_plasma", "a_R_membrane", "a_LR_membrane"))
    expect_equal(
        setNames(dydt, odeinfo$stateNames),
        c(a_L_plasma = -200, a_R_membrane = -200, a_LR_membrane = 200)
    )
})

test_that("ODE export rejects concentration-only transports without a volume", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule("A", cmt = "cyt", initial = 10, type = "concentration") |>
        add_transport(from = "cyt", to = "", molec = "A", const = "k") |>
        add_parameter(k = 1)

    expect_error(
        to_ode(M),
        "Transports require amount states or a compartment volume"
    )
})

test_that("ODE observables reject amount conversion from concentration states without a volume", {
    M <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule("A", cmt = "cyt", initial = 10, type = "concentration") |>
        add_observable(A_amount = a[A, cyt])

    expect_error(
        suppressWarnings(to_ode(M)),
        "Cannot convert concentration state .* to amount without a compartment volume"
    )
})
