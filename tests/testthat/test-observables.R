# Test the Observables class

test_that("Observables object is created correctly", {
    obs <- observables(c("Cpla", "Cblo"), c("blo/Vblo/BP", "blo/Vblo"))
    expect_s3_class(obs, "Observables")
})

test_that("Length and names methods work for Observables", {
    obs <- observables(c("Cpla", "Cblo", "Cper"), c("cen/Vcen/BP", "cen/Vcen", "per/Vper"))
    expect_equal(length(obs), 3)
    expect_equal(names(obs), c("Cpla", "Cblo", "Cper"))
})

test_that("Print method for Observables works", {
    obs <- observables(c("Cpla", "Cblo"), c("blo/Vblo/BP", "blo/Vblo"))
    expect_snapshot(print(obs))
})

test_that("Subsetting Observables object works", {
    obs <- observables(
        c("Cpla", "Cblo", "Cper"),
        c("blo/Vblo/BP", "blo/Vblo", "per/Vper")
    )
    obs_subset <- obs[1:2]
    expect_equal(length(obs_subset), 2)
    expect_equal(names(obs_subset), c("Cpla", "Cblo"))
    expect_s3_class(obs_subset, "Observables")
})

test_that("Concatenating Observables objects works", {
    obs1 <- observables(c("Cpla", "Cblo"), c("blo/Vblo/BP", "blo/Vblo"))
    obs2 <- observables(c("Cper"), c("per/Vper"))
    combined_obs <- c(obs1, obs2)

    expect_equal(length(combined_obs), 3)
    expect_equal(names(combined_obs), c("Cpla", "Cblo", "Cper"))
    expect_s3_class(combined_obs, "Observables")
})

test_that("Observables with mismatched name and expr lengths throw error", {
    expect_error(observables(
        c("cen", "per"),
        c("blo/Vblo/BP", "blo/Vblo", "per/Vper")
    ))
})

test_that("Empty Observables objects behave as expected", {
    obs <- empty_observable()
    expect_equal(length(obs), 0)

    obs2 <- observables(name = "Cpla", expr = "blo/Vblo/BP")
    expect_equal(obs, obs2[FALSE])
    expect_equal(obs2, c(obs2, obs))
})

test_that("add_observable works in programmatic and interactive paths", {
    # Programmatic path
    obs <- observables(name = "Cblo", expr = "blo/Vblo")
    M1 <- compartment_model() |>
        add_compartment("blo") |>
        add_observable(obs = obs)

    expect_equal(length(M1$observables), 1)
    expect_equal(names(M1$observables), "Cblo")
    expect_equal(M1$observables[[1]], quote(blo/Vblo))

    # Interactive path
    M2 <- compartment_model() |>
        add_compartment("blo") |>
        add_observable(Cblo = blo/Vblo)

    expect_equal(length(M2$observables), 1)
    expect_equal(names(M2$observables), "Cblo")
    expect_equal(M2$observables[[1]], quote(blo/Vblo))
})