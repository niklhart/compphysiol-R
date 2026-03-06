# Test for the `Parameters` class

test_that("Parameters class works as expected", {
    p1 <- parameters(name = 'A', value = 2, unit = "m")
    p2 <- parameters(name = 'B', value = 3, unit = "kg")

    # Test names method
    expect_equal(names(p1), c("A"))
    expect_equal(names(p2), c("B"))

    # Test combining parameters
    params <- c(p1, p2)
    expect_equal(names(params), c("A", "B"))
    expect_equal(params["A"], p1)
    expect_equal(params["B"], p2)

    # Test subsetting by index / name
    expect_equal(params[1], p1)
    expect_equal(params["A"], p1)

    # Test print method
    expect_snapshot(print(params))
})

test_that("Non-standard evaluation captures names, values and units correctly", {
    # Numeric parameters with units
    p_SE <- parameters(name = c('A', 'B'), value = c(2, 3), unit = c("", "kg"))
    p_NSE <- parameters(A = 2, B = 3[kg])

    expect_equal(p_SE, p_NSE)

    # Variables with units
    A_m <- units::set_units(1, "m")
    p_SE_var <- parameters(name = 'A', value = A_m)
    p_NSE_var <- parameters(A = A_m)
    expect_equal(p_SE_var, p_NSE_var)
})

test_that("On-the-fly unit conversion works correctly in NSE mode", {
    A_km <- units::set_units(1, "km")
    A_m <- units::set_units(1000, "m")

    p1 <- parameters(A = A_km)
    p2 <- parameters(A = A_m[km])
    expect_equal(p1, p2)
})

test_that("Parameter replacement works correctly", {
    p <- parameters(name = c('A', 'B'), value = c(2, 3), unit = c("", "kg"))
    p["A"] <- parameters(name = 'C', value = 5, unit = "m")
    expect_equal(p[["C"]], units::set_units(5, "m"))
})
