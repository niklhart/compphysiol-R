# Test the Observables class

test_that("Observables object is created correctly", {
    obs <- observables(
        name = c("Cblo", "Cpla"),
        expr = c("c[blo]", "c[blo]/BP")
    )
    expect_s3_class(obs, "Observables")
})

test_that("Length and names methods work for Observables", {
    obs <- observables(name = c("Cpla", "Cblo", "Cper"), expr = c("c[blo]/BP", "c[blo]", "c[per]"))
    expect_equal(length(obs), 3)
    expect_equal(names(obs), c("Cpla", "Cblo", "Cper"))
})

test_that("Print method for Observables works", {
    obs <- observables(name = c("Cpla", "Cblo"), expr = c("c[blo]/BP", "c[blo]"))
    expect_snapshot(print(obs))
})

test_that("Subsetting Observables object works", {
    obs <- observables(
        name = c("Cpla", "Cblo", "Cper"),
        expr = c("c[blo]/BP", "c[blo]", "c[per]")
    )
    obs_subset <- obs[1:2]
    expect_equal(length(obs_subset), 2)
    expect_equal(names(obs_subset), c("Cpla", "Cblo"))
    expect_s3_class(obs_subset, "Observables")
})

test_that("Concatenating Observables objects works", {
    obs1 <- observables(name = c("Cpla", "Cblo"), expr = c("c[blo]/BP", "c[blo]"))
    obs2 <- observables(name = "Cper", expr = "c[per]")
    combined_obs <- c(obs1, obs2)

    expect_equal(length(combined_obs), 3)
    expect_equal(names(combined_obs), c("Cpla", "Cblo", "Cper"))
    expect_s3_class(combined_obs, "Observables")
})

test_that("Observables with mismatched name and expr lengths throw error", {
    expect_error(observables(
        name = c("cen", "per"),
        expr = c("c[blo]/BP", "c[blo]", "c[per]")
    ))
})

test_that("Empty Observables objects behave as expected", {
    obs <- observables()
    expect_equal(length(obs), 0)

    obs2 <- observables(name = "Cpla", expr = "c[blo]/BP")
    expect_equal(obs, obs2[FALSE])
    expect_equal(obs2, c(obs2, obs))
})

test_that("add_observable works in programmatic and interactive paths", {
    # Programmatic path 1
    obs <- observables(name = "Cblo", expr = "c[blo]")
    M1 <- compartment_model() |>
        add_compartment("blo") |>
        add_observable(obs = obs)

    expect_equal(length(M1$observables), 1)
    expect_equal(names(M1$observables), "Cblo")
    expect_equal(M1$observables[[1]], quote(c[blo]))

    # Programmatic path 2
    M2 <- compartment_model() |>
        add_compartment("blo") |>
        add_observable(name = "Cblo", expr = "c[blo]")

    expect_equal(length(M2$observables), 1)
    expect_equal(names(M2$observables), "Cblo")
    expect_equal(M2$observables[[1]], quote(c[blo]))

    # Interactive path
    M3 <- compartment_model() |>
        add_compartment("blo") |>
        add_observable(Cblo = c[blo])

    expect_equal(length(M3$observables), 1)
    expect_equal(names(M3$observables), "Cblo")
    expect_equal(M3$observables[[1]], quote(c[blo]))
})

test_that("observables with cmt and molec substitution works", {

    # Substitute cmt only
    obs <- observables(
        "C{cmt}" = "c[{cmt}]",
        cmt = c("blo", "tis")
    )
    expect_length(obs, 2)
    expect_equal(names(obs), c("Cblo", "Ctis"))
    expect_equal(obs[[1]], quote(c[blo]))
    expect_equal(obs[[2]], quote(c[tis]))

    # Substitute molec only
    obs <- observables(
        "C{molec}" = "c[{molec}]",
        molec = c("drug", "metab")
    )
    expect_length(obs, 2)
    expect_equal(names(obs), c("Cdrug", "Cmetab"))
    expect_equal(obs[[1]], quote(c[drug]))
    expect_equal(obs[[2]], quote(c[metab]))

    # Substitute both cmt and molec
    obs <- observables(
        "C{cmt}_{molec}" = "c[{molec},{cmt}]",
        cmt = c("blo", "tis"),
        molec = c("drug", "metab")
    )
    expect_length(obs, 4)
    expect_equal(names(obs), c("Cblo_drug","Cblo_metab", "Ctis_drug", "Ctis_metab"))
    expect_equal(obs[[1]], quote(c[drug, blo]))
    expect_equal(obs[[4]], quote(c[metab, tis]))
})
