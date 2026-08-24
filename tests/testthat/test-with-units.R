test_that("with_units evaluates arithmetic with unit shorthand", {
    amount <- with_units(10 [mg] / 200 [g/mol])
    expected <- units::set_units(10, "mg", mode = "standard") /
        units::set_units(200, "g/mol", mode = "standard")

    expect_equal(amount, expected)
    expect_equal(units::set_units(amount, "mol", mode = "standard"), units::set_units(5e-05, "mol"))
})

test_that("with_units resolves unit variables from the caller environment", {
    MW <- units::set_units(200, "g/mol", mode = "standard")

    amount <- with_units(10 [mg] / MW)

    expect_equal(units::set_units(amount, "mol", mode = "standard"), units::set_units(5e-05, "mol"))
})

test_that("with_units supports conversion and vector arithmetic", {
    duration <- with_units(c(1, 2) [h] + 30 [min])
    expected <- units::set_units(c(1.5, 2.5), "h", mode = "standard")

    expect_equal(duration, expected)
})

test_that("with_units converts existing unit objects", {
    distance <- units::set_units(1000, "m", mode = "standard")

    expect_equal(with_units(distance [km]), units::set_units(1, "km", mode = "standard"))
})

test_that("with_units fails informatively for incompatible arithmetic", {
    expect_error(with_units(1 [L] + 2 [kg]))
})

test_that("with_units works when passed to unit-aware constructors", {
    dose <- dosing(
        time = with_units(30 [min]),
        amount = with_units(10 [mg] / 200 [g/mol]),
        cmt = "Central"
    )
    param <- parameters(CL = with_units(0.5 [L] / 1 [h]))

    expect_equal(dose$time, units::set_units(30, "min", mode = "standard"))
    expect_equal(units::set_units(dose$amount[[1]], "mol", mode = "standard"), units::set_units(5e-05, "mol"))
    expect_equal(param$CL, units::set_units(0.5, "L/h", mode = "standard"))
})

test_that("constructors evaluate nested unit arithmetic", {
    MW <- units::set_units(200, "g/mol", mode = "standard")

    cmt <- compartments("Central", volume = 500 [mL] + 0.5 [L])
    molec <- molecules("drug", initial = 10 [mg] / 2 [L])
    dose <- dosing(
        time = 30 [min] + 0.5 [h],
        amount = 10 [mg] / MW,
        rate = 5 [mg] / 1 [h] / MW,
        cmt = "Central"
    )
    param <- parameters(
        V = 500 [mL] + 0.5 [L],
        CL = 0.5 [L] / 1 [h]
    )

    expect_equal(units::set_units(cmt$volume[[1]], "L", mode = "standard"), units::set_units(1, "L"))
    expect_equal(molec$init[[1]], units::set_units(5, "mg/L", mode = "standard"))
    expect_equal(units::set_units(dose$time, "h", mode = "standard"), units::set_units(1, "h"))
    expect_equal(units::set_units(dose$amount[[1]], "mol", mode = "standard"), units::set_units(5e-05, "mol"))
    expect_equal(units::set_units(dose$rate[[1]], "mol/h", mode = "standard"), units::set_units(2.5e-05, "mol/h"))
    expect_equal(units::set_units(param$V, "L", mode = "standard"), units::set_units(1, "L"))
    expect_equal(param$CL, units::set_units(0.5, "L/h", mode = "standard"))
})
