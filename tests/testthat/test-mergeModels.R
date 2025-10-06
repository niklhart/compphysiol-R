
test_that("mergeModels merges two CompartmentModels without suffixes (collision='merge')", {
    # Absorption module
    abs <- CompartmentModel$new()
    abs$addCompartment("Gut", 100)
    abs$addCompartment("Central")
    abs$addReaction("Gut", "Central", "ka * Gut")
    abs$addObservable("GutObs", "Gut")
    abs$addDosing(Dosing$new("Gut", amount = 100, time = 0))

    # Systemic PK model
    pk <- CompartmentModel$new()
    pk$addCompartment("Central", 0)
    pk$addCompartment("Peripheral", 0)
    pk$addReaction("Central", "Peripheral", "k12 * Central")
    pk$addReaction("Peripheral", "Central", "k21 * Peripheral")
    pk$addReaction("Central", "", "k10 * Central")
    pk$addObservable("CentralObs", "Central")

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
    drugA <- CompartmentModel$new()
    drugA$addCompartment("Central", 0)
    drugA$addCompartment("Peripheral", 0)
    drugA$addReaction("Central", "Peripheral", "k12 * Central")
    drugA$addReaction("Peripheral", "Central", "k21 * Peripheral")
    drugA$addReaction("Central", "", "k10 * Central")
    drugA$addObservable("CentralObs", "Central")

    # Drug B
    drugB <- CompartmentModel$new()
    drugB$addCompartment("Central", 0)
    drugB$addCompartment("Peripheral", 0)
    drugB$addReaction("Central", "Peripheral", "k12 * Central")
    drugB$addReaction("Peripheral", "Central", "k21 * Peripheral")
    drugB$addReaction("Central", "", "k10 * Central")
    drugB$addObservable("CentralObs", "Central")

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
    drugA <- CompartmentModel$new()
    drugA$addCompartment("Central", 0)
    drugA$addCompartment("Peripheral", 0)
    drugA$addReaction("Central", "Peripheral", "k12 * Central")
    drugA$addReaction("Peripheral", "Central", "k21 * Peripheral")
    drugA$addReaction("Central", "", "k10 * Central")
    drugA$addObservable("CentralObs", "Central")

    # Drug B
    drugB <- CompartmentModel$new()
    drugB$addCompartment("Central", 0)
    drugB$addCompartment("Peripheral", 0)
    drugB$addReaction("Central", "Peripheral", "k12 * Central")
    drugB$addReaction("Peripheral", "Central", "k21 * Peripheral")
    drugB$addReaction("Central", "", "k10 * Central")
    drugB$addObservable("CentralObs", "Central")

    # Merge with suffixes to keep compartments separate
    merged_auto   <- mergeModels(drugA, drugB, collision = "auto")
    merged_suffix <- mergeModels(drugA, drugB, suffix1 = "1", suffix2 = "2", collision = "error")

    # Compartments
    expect_equal(merged_auto,merged_suffix)

})

