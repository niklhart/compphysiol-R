test_that("with_units evaluates arithmetic with unit shorthand", {
    amount <- with_units(10 [mg] / 200 [g/mol])
    expected <- units::set_units(10, "mg", mode = "standard") /
        units::set_units(200, "g/mol", mode = "standard")

    expect_equal(amount, expected)
    expect_equal(units::set_units(amount, "mol", mode = "standard"), units::set_units(5e-05, "mol"))
})

test_that("with_units supports conversion and vector arithmetic", {
    duration <- with_units(c(1, 2) [h] + 30 [min])
    expected <- units::set_units(c(1.5, 2.5), "h", mode = "standard")

    expect_equal(duration, expected)
})

test_that("with_units works when passed to unit-aware constructors", {
    dose <- dosing(
        time = with_units(30 [min]),
        amount = with_units(10 [mg] / 200 [g/mol]),
        cmt = "Central"
    )
    param <- parameters(CL = with_units(0.5 [L] / 1 [h]))

    expect_equal(dose$time, units::set_units(30, "min", mode = "standard"))
    expect_equal(units::set_units(dose$amount, "mol", mode = "standard"), units::set_units(5e-05, "mol"))
    expect_equal(param$CL, units::set_units(0.5, "L/h", mode = "standard"))
})
