
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
