test_that("DosingList() returns a list of Dosing objects", {
    doses <- DosingList("Central", time = c(0, 24, 48), amount = 100)

    expect_type(doses, "list")
    expect_true(all(vapply(doses, inherits, logical(1), "Dosing")))
    expect_equal(length(doses), 3)
})

test_that("DosingList() recycles scalar arguments correctly", {
    doses <- DosingList("Central", time = c(0, 12, 24), amount = 50)
    expect_equal(length(doses), 3)
    expect_equal(vapply(doses, function(d) d$amount, numeric(1)), rep(50, 3))
})

test_that("DosingList() handles vectorized amounts correctly", {
    doses <- DosingList("Central", time = c(0, 24, 48), amount = c(100, 200, 300))
    expect_equal(vapply(doses, function(d) d$amount, numeric(1)), c(100, 200, 300))
    expect_equal(vapply(doses, function(d) d$time, numeric(1)), c(0, 24, 48))
})

test_that("DosingList() supports infusion dosing with derived duration", {
    doses <- DosingList("Central", time = c(0, 24), amount = c(50, 60), rate = 10)
    expect_equal(length(doses), 2)
    expect_true(all(vapply(doses, function(d) d$isInfusion(), logical(1))))
    expect_equal(vapply(doses, function(d) d$duration, numeric(1)), c(5, 6))
})

test_that("DosingList() supports infusion dosing with derived rate", {
    doses <- DosingList("Central", time = c(0, 24), amount = 100, duration = c(5, 10))
    expect_equal(vapply(doses, function(d) d$rate, numeric(1)), c(20, 10))
})

test_that("DosingList() throws error for invalid argument lengths", {
    # time and amount lengths mismatch that cannot be recycled
    expect_error(DosingList("Central", time = c(0, 12, 24), amount = c(100, 200)))
})

test_that("DosingList() output works with CompartmentModel$addDosing()", {

    model <- CompartmentModel$new()
    doses <- DosingList("Central", time = c(0, 24, 48), amount = 100)
    expect_silent(model$addDosing(dose = doses))
    expect_true(length(model$doses) >= 3)
})
