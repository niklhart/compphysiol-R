
test_that("unit conversion via .to_dimensions() works as expected", {
    x <- units::set_units(1, "mg/h^2")

    x_si <- x
    units(x_si) <- "kg/s^2"

    expect_equal(compphysiol:::.to_dimensions(x), x_si)
    expect_equal(compphysiol:::.to_dimensions(x, mass = "mg", time = "h"), x)
    expect_equal(compphysiol:::.to_dimensions(x, amount = "mmol", length = "km"), x_si)
})

test_that(".c_units returns numeric vectors for bare numeric inputs", {
    expect_equal(compphysiol:::.c_units(1, 2:3), c(1, 2, 3))
    expect_equal(compphysiol:::.c_units(), numeric(0))
})

test_that(".c_units returns units vectors for compatible unit inputs", {
    x <- compphysiol:::.c_units(
        units::set_units(1, "mg", mode = "standard"),
        units::set_units(2, "g", mode = "standard")
    )

    expect_s3_class(x, "units")
    expect_false(inherits(x, "mixed_units"))
    expect_equal(x, units::set_units(c(1, 2000), "mg", mode = "standard"))
})

test_that(".c_units returns mixed_units vectors for mixed unit inputs", {
    x <- compphysiol:::.c_units(
        units::set_units(1, "mg", mode = "standard"),
        units::set_units(2, "mg/h", mode = "standard")
    )

    expect_s3_class(x, "mixed_units")
    expect_equal(x[[1]], units::set_units(1, "mg", mode = "standard"))
    expect_equal(x[[2]], units::set_units(2, "mg/h", mode = "standard"))
})

test_that(".c_units treats bare numeric values as dimensionless in unit-aware inputs", {
    x <- compphysiol:::.c_units(
        1,
        units::set_units(2, "mg", mode = "standard")
    )

    expect_s3_class(x, "mixed_units")
    expect_equal(x[[1]], units::set_units(1, "1", mode = "standard"))
    expect_equal(x[[2]], units::set_units(2, "mg", mode = "standard"))
})

test_that(".c_units rejects non-numeric inputs", {
    expect_error(
        compphysiol:::.c_units(1, "mg"),
        "All arguments must be either numeric or unit-bearing."
    )
})
