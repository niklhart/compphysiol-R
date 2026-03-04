
test_that("unit conversion via .to_dimensions() works as expected", {
    x <- units::set_units(1, "mg/h^2")

    x_si <- x
    units(x_si) <- "kg/s^2"

    expect_equal(compphysiol:::.to_dimensions(x), x_si)
    expect_equal(compphysiol:::.to_dimensions(x, mass = "mg", time = "h"), x)
    expect_equal(compphysiol:::.to_dimensions(x, amount = "mmol", length = "km"), x_si)
})