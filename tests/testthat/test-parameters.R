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

test_that("Non-standard evaluation captures parameters correctly", {
  
    p <- parameters(A = 2, B = units::set_units(3,"kg"))
    
    expect_equal(names(p), c("A", "B"))
    expect_equal(units::drop_units(p$A), 2)
    expect_equal(units::drop_units(p$B), 3)
})