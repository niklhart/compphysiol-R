
test_that("mergeModels merges two CompartmentModels without suffixes (collision='merge')", {
    # Absorption module
    abs <- compartment_model() |>
        add_compartment("Gut", 100) |>
        add_compartment("Central") |>
        add_flow("Gut", "Central", const = "ka") |>
        add_observable(GutObs = Gut) |>
        add_dosing(target = "Gut", amount = 100, time = 0)

    # Systemic PK model
    pk <- compartment_model() |>
        add_compartment(c("Central","Peripheral")) |>
        add_flow("Central", "Peripheral", const = "k12") |>
        add_flow("Peripheral", "Central", const = "k21") |>
        add_flow("Central", "", const = "k10") |>
        add_observable(CentralObs = Central)

    # Merge without suffixes, using collision = 'merge'
    merged <- mergeModels(abs, pk, suffix1 = NULL, suffix2 = NULL, collision = "merge")

    # Compartments: Gut + Central + Peripheral
    expect_setequal(names(merged$compartments), c("Gut", "Central", "Peripheral"))

    # Flows: absorption correctly connects to Central in systemic model
    expect_equal(merged$flow$from, c("Gut", "Central", "Peripheral", "Central"))
    expect_equal(merged$flow$to, c("Central", "Peripheral", "Central", NA_character_))
    expect_equal(merged$flow$rate |> unclass(), list(
        quote(ka * Gut),
        quote(k12 * Central),
        quote(k21 * Peripheral),
        quote(k10 * Central)
    ))

    # Observables
    expect_setequal(names(merged$observables), c("GutObs", "CentralObs"))

    # Dosing
    expect_equal(length(merged$doses), 1)
    expect_equal(merged$doses$target, "Gut")
})



test_that("mergeModels can merge two distinct drugs with suffixes", {
    # Drug A
    drugA <- compartment_model() |>
         add_compartment(c("Central", "Peripheral"), 0) |>
         add_flow("Central", "Peripheral", const = "k12") |>
         add_flow("Peripheral", "Central", const = "k21") |>
         add_flow("Central", "", const = "k10") |>
         add_observable(CentralObs = Central)

    # Drug B
    drugB <- compartment_model() |>
         add_compartment(c("Central", "Peripheral"), 0) |>
         add_flow("Central", "Peripheral", const = "k12") |>
         add_flow("Peripheral", "Central", const = "k21") |>
         add_flow("Central", "", const = "k10") |>
         add_observable(CentralObs = Central)

    # Merge with suffixes to keep compartments separate
    merged <- mergeModels(drugA, drugB, suffix1 = "A", suffix2 = "B", collision = "error")

    # Compartments & Observables should have suffixes
    expect_setequal(names(merged$compartments), c("Central_A", "Peripheral_A", "Central_B", "Peripheral_B"))
    expect_setequal(names(merged$observables), c("CentralObs_A", "CentralObs_B"))

    # Reactions should reference suffixed compartments
    endsWithAB <- function(x) endsWith(x, "_A") | endsWith(x, "_B")
    expect_all_true(endsWithAB(merged$flows$from))
    expect_true(all(endsWithAB(merged$flows$to) | is.na(merged$flows$to)))
    rate_names <- Reduce(union, lapply(merged$flows$rate, all.vars))
    expect_all_true(endsWithAB(rate_names))
})

test_that("mergeModels auto-renames two distinct drugs correctly", {

    # Drug A
    drugA <- compartment_model() |>
         add_compartment(c("Central", "Peripheral"), 0) |>
         add_flow("Central", "Peripheral", const = "k12") |>
         add_flow("Peripheral", "Central", const = "k21") |>
         add_flow("Central", "", const = "k10") |>
         add_observable(CentralObs = Central)

    # Drug B
    drugB <- compartment_model() |>
         add_compartment(c("Central", "Peripheral"), 0) |>
         add_flow("Central", "Peripheral", const = "k12") |>
         add_flow("Peripheral", "Central", const = "k21") |>
         add_flow("Central", "", const = "k10") |>
         add_observable(CentralObs = Central)

    # Merge with suffixes to keep compartments separate
    merged_auto   <- mergeModels(drugA, drugB, collision = "auto")
    merged_suffix <- mergeModels(drugA, drugB, suffix1 = "1", suffix2 = "2", collision = "error")

    # Compartments
    expect_equal(merged_auto,merged_suffix)

})



test_that("mergeModels respects shared parameters (skip suffixing)", {
    # Drug A (PBPK compound 1)
    drugA <- compartment_model() |>
         add_compartment("Central", 0) |>
         add_compartment("Liver", 0) |>
         add_flow("Central", "Liver", const = "Q_hepatic/V_liver * (Central - Liver / K_liver)") |>
         add_flow("Liver", "Central", const = "Q_hepatic/V_liver * (Liver / K_liver - Central)") |>
         add_observable(CentralObs = Central)

    # Drug B (PBPK compound 2)
    drugB <- compartment_model() |>
         add_compartment("Central", 0) |>
         add_compartment("Liver", 0) |>
         add_flow("Central", "Liver", const = "Q_hepatic/V_liver * (Central - Liver / K_liver)") |>
         add_flow("Liver", "Central", const = "Q_hepatic/V_liver * (Liver / K_liver - Central)") |>
         add_observable(CentralObs = Central)

    # Shared physiological parameters (should not be suffixed)
    shared <- c("Q_hepatic", "V_liver")

    # Merge models with suffixes for drug-specific parameters only
    merged <- mergeModels(drugA, drugB,
                          suffix1 = "A", suffix2 = "B",
                          shared = shared,
                          collision = "error")

    # Check that state names and observables are suffixed (no collision)
    expect_setequal(names(merged$compartments), c("Liver_A", "Central_A", "Liver_B", "Central_B"))
    expect_setequal(names(merged$observables), c("CentralObs_A", "CentralObs_B"))

    # Check that shared parameters remain unsuffixed
    flow_strings <- vapply(merged$flows$rate, deparse, character(1))
    expect_true(all(grepl("Q_hepatic", flow_strings)))
    expect_true(all(grepl("V_liver", flow_strings)))

    # Check that partition coefficients (drug-specific) were suffixed
    expect_true(any(grepl("K_liver_A", flow_strings)))
    expect_true(any(grepl("K_liver_B", flow_strings)))
})

