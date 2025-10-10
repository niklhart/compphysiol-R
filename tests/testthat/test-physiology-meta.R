test_that("metadata can be added and retrieved", {
    p <- Physiology$new()
    p$add_meta(species = "rat", strain = "Wistar", sex = "female")

    expect_equal(p$get("species"), "rat")
    expect_equal(p$get("strain"), "Wistar")
    expect_equal(p$get("sex"), "female")
})

test_that("add_meta merges new fields correctly", {
    p <- Physiology$new()
    p$add_meta(species = "rat", strain = "Wistar")
    p$add_meta(sex = "male", strain = "Sprague-Dawley") # overwrite strain

    expect_equal(p$get("species"), "rat")
    expect_equal(p$get("strain"), "Sprague-Dawley")
    expect_equal(p$get("sex"), "male")

    expect_named(p$meta, c("species", "strain", "sex"))
})

test_that("add_meta handles empty input safely", {
    p <- Physiology$new()
    expect_silent(p$add_meta())
    expect_length(p$meta, 0)
})

test_that("get returns NA for missing parameters or metadata", {
    p <- Physiology$new()
    p$add_scalar("BW", 250)
    expect_true(is.na(p$get("unknown_param")))
    expect_true(is.na(p$get("unknown_meta")))
})

test_that("unified get retrieves both numeric and categorical fields", {
    p <- Physiology$new()
    p$add_scalar("BW", 300)
    p$add_meta(sex = "female")

    expect_equal(p$get("BW"), 300)
    expect_equal(p$get("sex"), "female")
})

test_that("to_param_list merges metadata and parameters", {
    p <- Physiology$new()
    p$add_scalar("BW", 250)
    p$add_tissue_param("liv", "Vtis", 10)
    p$add_meta(species = "rat", strain = "Wistar", sex = "female")

    out <- p$to_param_list()

    expect_true("BW" %in% names(out))
    expect_true("Vtis[liv]" %in% names(out))
    expect_true("species" %in% names(out))
    expect_equal(out$species, "rat")
    expect_equal(out$BW, 250)
})

test_that("print method displays meta and scalar summaries", {
    p <- Physiology$new()
    p$add_scalar("BW", 250)
    p$add_meta(species = "rat", strain = "Wistar", sex = "female")

    expect_output(print(p), "<Physiology>")
    expect_output(print(p), "Scalars:")
})
