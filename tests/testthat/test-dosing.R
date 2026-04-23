# Scalar dosing tests

test_that("Bolus dosing is created correctly", {
    d <- dosing(time = 0, amount = 100, cmt = "Central", molec = "drug")
    expect_true(is_bolus(d))
    expect_equal(d$time, 0)
    expect_equal(d$amount, 100)
    expect_equal(d$cmt, "Central")
    expect_equal(d$molec, "drug")
    expect_true(is.na(d$rate))
    expect_true(is.na(d$duration))
})

test_that("Infusion dosing with amount+duration works", {
    d <- dosing(amount = 100, time = 0, duration = 10)
    expect_true(is_infusion(d))
    expect_equal(d$rate, 10)
    expect_equal(d$amount, 100)
    expect_equal(d$duration, 10)
})

test_that("Infusion dosing with amount+rate works", {
    d <- dosing(amount = 100, time = 0, rate = 20)
    expect_true(is_infusion(d))
    expect_equal(d$duration, 5)   # derived
    expect_equal(d$rate, 20)
})

test_that("Infusion dosing with rate+duration works", {
    d <- dosing(time = 0, rate = 10, duration = 5)
    expect_true(is_infusion(d))
    expect_equal(d$amount, 50)    # derived
    expect_equal(d$rate, 10)
    expect_equal(d$duration, 5)
})

test_that("Invalid dosing definitions throw errors", {
    # missing all three
    expect_error(dosing(time = 0))

    # all three specified inconsistently
    expect_error(dosing(time = 0, amount = 100, rate = 20, duration = 20))
})

test_that("Print output for bolus dosing", {
    d1 <- dosing(time = 0 [h], amount = 50 [mg], cmt = "Central", molec = "drug")
    expect_snapshot(print(d1))
})

test_that("Print output for infusion dosing", {
    d2 <- dosing(time = 5 [h], amount = 100 [mg], duration = 1 [h])
    expect_snapshot(print(d2))
})

# Vectorized dosing tests

test_that("Vectorized dosing times with recycled amounts work", {
    doses <- dosing( time = c(0, 24, 48), amount = 100)

    expect_s3_class(doses, "Dosing")
    expect_equal(length(doses), 3)
    expect_equal(doses$amount, rep(100, 3))
})

test_that("Vectorized amounts are handled correctly", {
    doses <- dosing(time = c(0, 24, 48), amount = c(100, 200, 300))
    expect_equal(doses$amount, c(100, 200, 300))
    expect_equal(doses$time, c(0, 24, 48))
})

test_that("Vectorized infusion dosing with derived duration is handled correctly", {
    doses <- dosing(time = c(0, 24), amount = c(50, 60), rate = 10)
    expect_equal(length(doses), 2)
    expect_all_true(is_infusion(doses))
    expect_equal(doses$duration, c(5, 6))
})

test_that("Vectorized infusion dosing with derived rate is handled correctly", {
    doses <- dosing(time = c(0, 24), amount = 100, duration = c(5, 10))
    expect_equal(doses$rate, c(20, 10))
})

test_that("Dosing constructor throws error for invalid argument lengths", {
    # time and amount lengths mismatch that cannot be recycled
    expect_error(dosing(time = c(0, 12, 24), amount = c(100, 200)))
})

test_that("Dosing class can be used in add_dosing()", {

    model <- compartment_model()
    doses <- dosing(time = c(0, 24, 48), amount = 100)
    expect_silent(model <- add_dosing(model, dose = doses))
    expect_true(length(model$doses) >= 3)
})

# Concatenation and subsetting tests

test_that("Concatenating dosing objects works", {
    d1 <- dosing(time = 0, amount = 100)
    d2 <- dosing(time = 24, amount = 100)

    doses_combined <- c(d1, d2)
    expect_s3_class(doses_combined, "Dosing")
    expect_equal(length(doses_combined), 2)
    expect_equal(doses_combined$time, c(0, 24))
    expect_equal(doses_combined$amount, c(100, 100))
})

test_that("Subsetting dosing objects works", {
    doses <- dosing(time = c(0, 24, 48), amount = c(100, 200, 300))
    doses_subset <- doses[1:2]

    expect_s3_class(doses_subset, "Dosing")
    expect_equal(length(doses_subset), 2)
    expect_equal(doses_subset$time, c(0, 24))
    expect_equal(doses_subset$amount, c(100, 200))
})

# Dosing with units tests

test_that("Dosing with units is created correctly (single dose)", {

    amt <- units::set_units(100, "mg")
    t <- units::set_units(0, "h")

    d1 <- dosing(time = 0, amount = 100, time_unit = "h", amount_unit = "mg")
    expect_equal(d1$time, t)
    expect_equal(d1$amount, amt)

    d2 <- dosing(time = t, amount = amt)
    expect_equal(d2$time, t)
    expect_equal(d2$amount, amt)

    d3 <- dosing(time = 0 [h], amount = 100 [mg])
    expect_equal(d3$time, t)
    expect_equal(d3$amount, amt)
})

test_that("Dosing with units is created correctly (multiple doses)", {
    
    t12 <- units::set_units(c(0, 24), "h")
    amt1 <- units::set_units(100, "mg")
    amt2 <- units::set_units(200, "mg")
    amt12 <- units::set_units(c(100, 200), "mg")

    d1 <- dosing(time = t12, amount = amt1)
    expect_equal(d1$time, t12)
    expect_equal(d1$amount, rep(amt1, 2))

    d2 <- dosing(time = t12, amount = amt12)
    expect_equal(d2$time, t12)
    expect_equal(d2$amount, amt12)
})
