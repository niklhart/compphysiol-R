test_that("metadata can be added and retrieved", {
    p <- physiology() |>
        add_meta(species = "rat", strain = "Wistar", sex = "female")

    expect_equal(parameter(p, "strain"), "Wistar")
})

test_that("add_meta merges new fields correctly", {
    p <- physiology() |>
        add_meta(species = "rat", strain = "Wistar") |> 
        add_meta(sex = "male", strain = "Sprague-Dawley") # overwrite strain

    expect_equal(parameter(p, "strain"), "Sprague-Dawley")
    expect_named(p$meta, c("species", "strain", "sex"))
})

test_that("add_meta handles empty input safely", {
    p <- physiology()
    expect_silent(p <- add_meta(p)) # no new fields
    expect_length(p$meta, 0)
})

test_that("parameter() returns NA for missing parameters or metadata", {
    p <- physiology() |>
        add_scalar("BW", 250)
    expect_true(is.na(parameter(p, "unknown_param")))
    expect_true(is.na(parameter(p, "unknown_meta")))
})

test_that("unified parameter() retrieves both numeric and categorical fields", {
    p <- physiology() |>
        add_scalar("BW", 300) |>
        add_meta(sex = "female")

    expect_equal(parameter(p, "BW"), 300)
    expect_equal(parameter(p, "sex"), "female")
})

test_that("to_param_list merges metadata and parameters", {
    p <- physiology() |>
        add_scalar("BW", 250) |>
        add_tissue_param("liv", "Vtis", 10) |>
        add_meta(species = "rat", strain = "Wistar", sex = "female")

    out <- to_param_list(p)

    expect_true("BW" %in% names(out))
    expect_true("Vtis[liv]" %in% names(out))
    expect_true("species" %in% names(out))
    expect_equal(out$species, "rat")
    expect_equal(out$BW, 250)
})

test_that("print method displays meta and scalar summaries", {
    p <- physiology() |>
        add_scalar("BW", 250) |>
        add_meta(species = "rat", strain = "Wistar", sex = "female")

    expect_snapshot(print(p))
})
