test_that("Bolus dosing is created correctly", {
    d <- Dosing$new(target = "Central", amount = 100, time = 0)
    expect_true(d$isBolus())
    expect_false(d$isInfusion())
    expect_equal(d$target, "Central")
    expect_equal(d$amount, 100)
    expect_equal(d$time, 0)
    expect_null(d$rate)
    expect_null(d$duration)
})

test_that("Infusion dosing with amount+duration works", {
    d <- Dosing$new(target = "Central", amount = 100, time = 0, duration = 10)
    expect_true(d$isInfusion())
    expect_equal(d$rate, 10)
    expect_equal(d$amount, 100)
    expect_equal(d$duration, 10)
})

test_that("Infusion dosing with amount+rate works", {
    d <- Dosing$new(target = "Central", amount = 100, time = 0, rate = 20)
    expect_true(d$isInfusion())
    expect_equal(d$duration, 5)   # derived
    expect_equal(d$rate, 20)
})

test_that("Infusion dosing with rate+duration works", {
    d <- Dosing$new(target = "Central", time = 0, rate = 10, duration = 5)
    expect_true(d$isInfusion())
    expect_equal(d$amount, 50)    # derived
    expect_equal(d$rate, 10)
    expect_equal(d$duration, 5)
})

test_that("Invalid dosing definitions throw errors", {
    # missing all three
    expect_error(Dosing$new("Central", time = 0))

    # all three specified inconsistently
    expect_error(Dosing$new("Central", time = 0, amount = 100, rate = 20, duration = 20))
})

test_that("Print output for bolus dosing", {
    d1 <- Dosing$new("Central", amount = 50, time = 0)
    expect_snapshot(print(d1))
})

test_that("Print output for infusion dosing", {
    d2 <- Dosing$new("Central", amount = 100, time = 5, duration = 10)
    expect_snapshot(print(d2))
})

