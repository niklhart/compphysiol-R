
test_that("unit conversion via .to_dimensions() works as expected", {
    x <- units::set_units(1, "mg/h^2")

    x_si <- x
    units(x_si) <- "kg/s^2"

    expect_equal(compphysiol:::.to_dimensions(x), x_si)
    expect_equal(compphysiol:::.to_dimensions(x, mass = "mg", time = "h"), x)
    expect_equal(compphysiol:::.to_dimensions(x, amount = "mmol", length = "km"), x_si)
})

test_that(".to_dimensions preserves custom base units while converting SI parts", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("modelcell"), add = TRUE)
    install_model_unit("modelcell")

    x <- units::set_units(1, "nmol/modelcell", mode = "standard")
    out <- compphysiol:::.to_dimensions(x)

    expect_equal(out, units::set_units(1e-9, "mol/modelcell", mode = "standard"))
})

test_that(".to_dimensions expands registered derived custom units", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("modelcellperL"), add = TRUE)
    on.exit(units::remove_unit("modelcelltwo"), add = TRUE)
    install_model_unit("modelcelltwo")
    install_model_unit("modelcellperL", "modelcelltwo/L")

    x <- units::set_units(1, "modelcellperL", mode = "standard")
    out <- compphysiol:::.to_dimensions(x)

    expect_true(units::ud_are_convertible(units(out), "modelcelltwo/m^3"))
    expect_equal(
        units::set_units(out, "modelcelltwo/m^3", mode = "standard"),
        units::set_units(1000, "modelcelltwo/m^3", mode = "standard")
    )
})

test_that(".to_dimensions treats convertible derived custom units as the same custom axis", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("twomodelcell"), add = TRUE)
    on.exit(units::remove_unit("modelcellthree"), add = TRUE)
    install_model_unit("modelcellthree")
    install_model_unit("twomodelcell", "2 modelcellthree")

    x <- units::set_units(1, "twomodelcell", mode = "standard")
    out <- compphysiol:::.to_dimensions(x)

    expect_equal(
        units::set_units(out, "modelcellthree", mode = "standard"),
        units::set_units(2, "modelcellthree", mode = "standard")
    )
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
