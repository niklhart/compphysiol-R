
test_that("mergeModels merges two CompartmentModels without suffixes (collision='merge')", {
    # Absorption module
    abs <- CompartmentModel$
        new()$
        addCompartment("Gut", 100)$
        addCompartment("Central")$
        addReaction("Gut", "Central", "ka * Gut")$
        addObservable("GutObs", "Gut")$
        addDosing(target = "Gut", amount = 100, time = 0)

    # Systemic PK model
    pk <- CompartmentModel$
        new()$
        addCompartment(c("Central","Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addReaction("Peripheral", "Central", const = "k21")$
        addReaction("Central", "", const ="k10")$
        addObservable("CentralObs", "Central")

    # Merge without suffixes, using collision = 'merge'
    merged <- mergeModels(abs, pk, suffix1 = NULL, suffix2 = NULL, collision = "merge")

    # Compartments: Gut + Central + Peripheral
    expect_equal(merged$getStateNames(), c("Gut", "Central", "Peripheral"))

    # Reactions: absorption connects to Central in systemic model
    reaction_strings <- sapply(merged$reactions, function(r) paste(r$from, r$to, deparse(r$rate)))
    expect_true(any(grepl("Gut Central ka \\* Gut", reaction_strings)))
    expect_true(any(grepl("Central Peripheral k12 \\* Central", reaction_strings)))
    expect_true(any(grepl("Peripheral Central k21 \\* Peripheral", reaction_strings)))
    expect_true(any(grepl("Central  k10 \\* Central", reaction_strings)))

    # Observables
    observable_names <- sapply(merged$observables, function(o) o$name)
    expect_true(all(c("GutObs", "CentralObs") %in% observable_names))

    # Dosing
    expect_equal(length(merged$doses), 1)
    expect_equal(merged$doses[[1]]$target, "Gut")
})



test_that("mergeModels can merge two distinct drugs with suffixes", {
    # Drug A
    drugA <- CompartmentModel$
        new()$
        addCompartment(c("Central", "Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addReaction("Peripheral", "Central", const = "k21")$
        addReaction("Central", "", const = "k10")$
        addObservable("CentralObs", "Central")

    # Drug B
    drugB <- CompartmentModel$
        new()$
        addCompartment(c("Central", "Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addReaction("Peripheral", "Central", const = "k21")$
        addReaction("Central", "", const = "k10")$
        addObservable("CentralObs", "Central")

    # Merge with suffixes to keep compartments separate
    merged <- mergeModels(drugA, drugB, suffix1 = "A", suffix2 = "B", collision = "error")

    # Compartments
    expect_equal(
        merged$getStateNames(),
        c("Central_A", "Peripheral_A", "Central_B", "Peripheral_B")
    )

    # Reactions should reference suffixed compartments
    reaction_strings <- sapply(merged$reactions, function(r) paste(r$from, r$to, deparse(r$rate)))
    expect_true(all(grepl("Central_A|Peripheral_A|Central_B|Peripheral_B", reaction_strings)))

    # Observables
    observable_names <- sapply(merged$observables, function(o) o$name)
    expect_true(all(c("CentralObs_A", "CentralObs_B") %in% observable_names))
})

test_that("mergeModels auto-renames two distinct drugs correctly", {

    # Drug A
    drugA <- CompartmentModel$
        new()$
        addCompartment(c("Central", "Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addReaction("Peripheral", "Central", const = "k21")$
        addReaction("Central", "", const = "k10")$
        addObservable("CentralObs", "Central")

    # Drug B
    drugB <- CompartmentModel$
        new()$
        addCompartment(c("Central", "Peripheral"), 0)$
        addReaction("Central", "Peripheral", const = "k12")$
        addReaction("Peripheral", "Central", const = "k21")$
        addReaction("Central", "", const = "k10")$
        addObservable("CentralObs", "Central")

    # Merge with suffixes to keep compartments separate
    merged_auto   <- mergeModels(drugA, drugB, collision = "auto")
    merged_suffix <- mergeModels(drugA, drugB, suffix1 = "1", suffix2 = "2", collision = "error")

    # Compartments
    expect_equal(merged_auto,merged_suffix)

})



test_that("mergeModels respects shared parameters (skip suffixing)", {
    # Drug A (PBPK compound 1)
    drugA <- CompartmentModel$new()$
        addCompartment("Liver", 0)$
        addCompartment("Central", 0)$
        addReaction("Central", "Liver", "Q_hepatic/V_liver * (Central - Liver / K_liver)")$
        addReaction("Liver", "Central", "Q_hepatic/V_liver * (Liver / K_liver - Central)")$
        addObservable("CentralObs", "Central")

    # Drug B (PBPK compound 2)
    drugB <- CompartmentModel$new()$
        addCompartment("Liver", 0)$
        addCompartment("Central", 0)$
        addReaction("Central", "Liver", "Q_hepatic/V_liver * (Central - Liver / K_liver)")$
        addReaction("Liver", "Central", "Q_hepatic/V_liver * (Liver / K_liver - Central)")$
        addObservable("CentralObs", "Central")

    # Shared physiological parameters (should not be suffixed)
    shared <- c("Q_hepatic", "V_liver")

    # Merge models with suffixes for drug-specific parameters only
    merged <- mergeModels(drugA, drugB,
                          suffix1 = "A", suffix2 = "B",
                          shared = shared,
                          collision = "error")

    # Check that state names are suffixed (no collision)
    expect_equal(
        merged$getStateNames(),
        c("Liver_A", "Central_A", "Liver_B", "Central_B")
    )

    # Check that shared parameters remain unsuffixed
    reaction_strings <- sapply(merged$reactions, function(r) deparse(r$rate))
    expect_true(all(grepl("Q_hepatic", reaction_strings)))
    expect_true(all(grepl("V_liver", reaction_strings)))

    # Check that partition coefficients (drug-specific) were suffixed
    expect_true(any(grepl("K_liver_A", reaction_strings)))
    expect_true(any(grepl("K_liver_B", reaction_strings)))

    # Observables have suffixes
    observable_names <- sapply(merged$observables, function(o) o$name)
    expect_true(all(c("CentralObs_A", "CentralObs_B") %in% observable_names))
})

