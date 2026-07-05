test_that("mergeModels overlays two CompartmentModels with shared molecule-compartment states", {
    abs <- compartment_model() |>
        add_compartment(c("Gut", "Central"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Gut", "Central"), initial = c(100, 0), type = "amount") |>
        add_transport("Gut", "Central", const = "ka", molec = "drug") |>
        add_observable(GutObs = a[drug, Gut]) |>
        add_dosing(cmt = "Gut", molec = "drug", amount = 100, time = 0)

    pk <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(0, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12", molec = "drug") |>
        add_transport("Peripheral", "Central", const = "k21", molec = "drug") |>
        add_transport("Central", "", const = "k10", molec = "drug") |>
        add_observable(CentralObs = a[drug, Central])

    merged <- mergeModels(abs, pk, mode = "overlay")

    expect_setequal(names(merged$compartments), c("Gut", "Central", "Peripheral"))
    expect_equal(
        as.data.frame(merged$transports)[, c("from", "to", "molec")],
        data.frame(
            from = c("Gut", "Central", "Peripheral", "Central"),
            to = c("Central", "Peripheral", "Central", NA_character_),
            molec = rep("drug", 4),
            stringsAsFactors = FALSE
        )
    )
    expect_equal(
        vapply(merged$transports$const, deparse1, character(1)),
        c("ka", "k12", "k21", "k10")
    )
    expect_setequal(names(merged$observables), c("GutObs", "CentralObs"))
    expect_equal(length(merged$doses), 1)
    expect_equal(merged$doses$cmt, "Gut")
    expect_equal(merged$doses$molec, "drug")
})

test_that("mergeModels can merge two distinct drugs with suffixes", {
    skip("Renaming/copy semantics will be redesigned after overlay merge mode.")

    drugA <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(0, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12", molec = "drug") |>
        add_transport("Peripheral", "Central", const = "k21", molec = "drug") |>
        add_transport("Central", "", const = "k10", molec = "drug") |>
        add_observable(CentralObs = a[drug, Central])

    drugB <- drugA

    merged <- mergeModels(drugA, drugB, suffix1 = "A", suffix2 = "B", collision = "error")

    expect_setequal(
        names(merged$compartments),
        c("Central_A", "Peripheral_A", "Central_B", "Peripheral_B")
    )
    expect_setequal(names(merged$observables), c("CentralObs_A", "CentralObs_B"))

    endsWithAB <- function(x) endsWith(x, "_A") | endsWith(x, "_B")
    expect_true(all(endsWithAB(merged$transports$from)))
    expect_true(all(endsWithAB(merged$transports$to) | is.na(merged$transports$to)))
    expect_setequal(merged$molecules$cmt, names(merged$compartments))
})

test_that("mergeModels auto-renames two distinct drugs correctly", {
    skip("Renaming/copy semantics will be redesigned after overlay merge mode.")

    drugA <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(0, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12", molec = "drug") |>
        add_transport("Peripheral", "Central", const = "k21", molec = "drug") |>
        add_transport("Central", "", const = "k10", molec = "drug") |>
        add_observable(CentralObs = a[drug, Central])

    drugB <- drugA

    merged_auto <- mergeModels(drugA, drugB, collision = "auto")
    merged_suffix <- mergeModels(drugA, drugB, suffix1 = "1", suffix2 = "2", collision = "error")

    expect_equal(merged_auto, merged_suffix)
})

test_that("mergeModels respects shared parameters (skip suffixing)", {
    skip("Shared-symbol handling belongs to the later renaming/copy redesign.")

    drugA <- compartment_model() |>
        add_compartment(c("Central", "Liver"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Liver"), initial = c(0, 0), type = "amount") |>
        add_transport(
            "Central",
            "Liver",
            rate = "Q_hepatic / V_liver * (a[drug, Central] - a[drug, Liver] / K_liver)"
        ) |>
        add_transport(
            "Liver",
            "Central",
            rate = "Q_hepatic / V_liver * (a[drug, Liver] / K_liver - a[drug, Central])"
        ) |>
        add_observable(CentralObs = a[drug, Central])

    drugB <- drugA
    shared <- c("Q_hepatic", "V_liver")

    merged <- mergeModels(
        drugA,
        drugB,
        suffix1 = "A",
        suffix2 = "B",
        shared = shared,
        collision = "error"
    )

    expect_setequal(
        names(merged$compartments),
        c("Liver_A", "Central_A", "Liver_B", "Central_B")
    )
    expect_setequal(names(merged$observables), c("CentralObs_A", "CentralObs_B"))

    rate_strings <- vapply(merged$transports$rate, deparse1, character(1))
    expect_true(all(grepl("Q_hepatic", rate_strings)))
    expect_true(all(grepl("V_liver", rate_strings)))
    expect_true(any(grepl("K_liver_A", rate_strings)))
    expect_true(any(grepl("K_liver_B", rate_strings)))
    expect_true(any(grepl("Central_A", rate_strings)))
    expect_true(any(grepl("Central_B", rate_strings)))
})
