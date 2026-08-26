test_that("install_model_unit installs and registers base model units", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("celltest"), add = TRUE)

    expect_invisible(install_model_unit("celltest"))

    expect_true(units::ud_are_convertible("celltest", "celltest"))
    expect_equal(
        model_unit_registry(),
        data.frame(symbol = "celltest", def = "", stringsAsFactors = FALSE)
    )
})

test_that("install_model_unit installs and registers derived model units", {
    reset_model_unit_registry()
    on.exit(units::remove_unit("twocelltest"), add = TRUE)
    on.exit(units::remove_unit("celltesttwo"), add = TRUE)
    install_model_unit("celltesttwo")

    expect_invisible(install_model_unit("twocelltest", "2 celltesttwo"))

    expect_true(units::ud_are_convertible("twocelltest", "celltesttwo"))
    expect_equal(
        units::set_units(units::set_units(1, "twocelltest", mode = "standard"), "celltesttwo", mode = "standard"),
        units::set_units(2, "celltesttwo", mode = "standard")
    )
    expect_equal(
        model_unit_registry(),
        data.frame(
            symbol = c("celltesttwo", "twocelltest"),
            def = c("", "2 celltesttwo"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("register_model_unit records definitions without installation", {
    reset_model_unit_registry()

    expect_invisible(register_model_unit("cellperL", "cell/L"))

    expect_equal(
        model_unit_registry(),
        data.frame(symbol = "cellperL", def = "cell/L", stringsAsFactors = FALSE)
    )
})

test_that("model unit registry replaces existing symbols", {
    reset_model_unit_registry()
    register_model_unit("cellcount")

    register_model_unit("cellcount", "2 cell")

    expect_equal(
        model_unit_registry(),
        data.frame(symbol = "cellcount", def = "2 cell", stringsAsFactors = FALSE)
    )
})

test_that("model unit registration treats NULL definitions as base units", {
    reset_model_unit_registry()

    register_model_unit("cellnull", NULL)

    expect_equal(
        model_unit_registry(),
        data.frame(symbol = "cellnull", def = "", stringsAsFactors = FALSE)
    )
})

test_that("model unit registration rejects unsupported definitions and compound symbols", {
    expect_error(register_model_unit("cell", "unitless"), "definition 'unitless'")
    expect_error(register_model_unit("cell/L"), "single unit symbol")
    expect_error(register_model_unit("cell per L"), "single unit symbol")
    expect_error(register_model_unit("cell_per_L"), "single unit symbol")
})
