test_that("Physiology initializes with an empty parameter table", {
    phys <- Physiology$new()
    expect_s3_class(phys$param_table, "data.frame")
    expect_equal(nrow(phys$param_table), 0)
    expect_named(phys$param_table, c("parameter", "context", "value", "unit", "type", "reference", "assumption"))
})

test_that("Physiology initializes correctly with a provided parameter table", {
    params <- data.frame(
        parameter = "BW", context = "scalar", value = 70,
        unit = "kg", type = "scalar", reference = "StdRef", assumption = "",
        stringsAsFactors = FALSE
    )
    phys <- Physiology$new(params)
    expect_equal(nrow(phys$param_table), 1)
    expect_equal(phys$param_table$parameter, "BW")
    expect_equal(phys$param_table$value, 70)
})

test_that("add_scalar() correctly appends scalar parameters", {
    phys <- Physiology$new()
    phys$add_scalar("BW", 70, "kg", "Ref1", "Adult average")
    expect_equal(nrow(phys$param_table), 1)
    expect_equal(phys$param_table$parameter, "BW")
    expect_equal(phys$param_table$context, "scalar")
    expect_equal(phys$param_table$type, "scalar")
    expect_equal(phys$param_table$value, 70)
    expect_equal(phys$param_table$reference, "Ref1")
})

test_that("add_tissue_param() correctly appends tissue parameters", {
    phys <- Physiology$new()
    phys$add_tissue_param("Liver", "V", 1.8, "L", "Ref2", "Typical adult")
    expect_equal(nrow(phys$param_table), 1)
    expect_equal(phys$param_table$parameter, "V")
    expect_equal(phys$param_table$context, "Liver")
    expect_equal(phys$param_table$type, "tissue")
    expect_equal(phys$param_table$value, 1.8)
})

test_that("get() retrieves scalar and tissue parameters", {
    phys <- Physiology$new()
    phys$add_scalar("BW", 70)
    phys$add_tissue_param("Liver", "V", 1.8)
    expect_equal(phys$get("BW"), 70)
    expect_equal(phys$get("V", "Liver"), 1.8)
    expect_true(is.na(phys$get("Nonexistent")))
})

test_that("to_param_list() returns correct named list", {
    phys <- Physiology$new()
    phys$add_scalar("BW", 70)
    phys$add_tissue_param("Liver", "V", 1.8)
    phys$add_tissue_param("Kidney", "V", 0.3)

    param_list <- phys$to_param_list()
    expect_type(param_list, "list")

    # Expect correct naming conventions
    expect_true(all(c("BW", "V[Liver]", "V[Kidney]") %in% names(param_list)))

    # Check values
    expect_equal(param_list$BW, 70)
    expect_equal(param_list[["V[Liver]"]], 1.8)
    expect_equal(param_list[["V[Kidney]"]], 0.3)
})

test_that("add_scalar() and add_tissue_param() are chainable", {
    phys <- Physiology$new()
    res <- phys$add_scalar("BW", 70)$add_tissue_param("Liver", "V", 1.8)
    expect_identical(res, phys)
    expect_equal(nrow(phys$param_table), 2)
})
