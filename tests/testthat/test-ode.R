
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
    y <- cbind(time = 0, c_drug_Central = 10)
    expect_equal(odeinfo$obsFuncs$A(0, y, list()), 100)
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
    skip("Reaction contributions to ODE generation are not implemented yet.")

    M <- compartment_model() |>
        add_compartment("cyt", volume = 1) |>
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

test_that("ODE generation includes complex reaction rates", {
    skip("Reaction contributions to ODE generation are not implemented yet.")

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
    skip("Reaction contributions to ODE generation are not implemented yet.")

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
    skip("Reaction contributions to ODE generation are not implemented yet.")

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

test_that("ODE generation handles various reaction orders", {

    skip("This test does not work anymore because add_flow vectorization is used differently now. Needs to be adapted.")

    # TODO: To make this work, extend the allowed types of from/to in flows() from character to expression.
    #       Then, quote(2*S) can be used for mass-balance, and parameters could be used as well.

    # Systems biology example with zero- and second-order reactions
    M <- compartment_model() |>
            add_compartment("S", 0) |>
            add_compartment("S2", 1) |>
            add_flow("", "S", rate = "ksyn") |>              # zero-order synthesis
            add_flow("S", "", rate = "kdeg*S") |>            # first-order elimination
            add_flow(c("S","S"), "S2", rate = "kass*S^2") |> # second-order association
            add_flow("S2", c("S","S"), rate = "kdis*S2")     # first-order dissociation

    odeinfo <- to_ode(M, paramValues = list(ksyn = 1, kdeg = 2, kass = 0.1, kdis = 0.5))
    expect_true(is.function(odeinfo$odefun))
    expect_equal(odeinfo$stateNames, c("S", "S2"))

    y0   <- odeinfo$y0
    expect_equal(y0, c(S=0, S2=1))
  
    dydt <- odeinfo$odefun(0, y0, list())
    expect_equal(dydt[[1]], c(2,-0.5))              # FAILS

})
