test_that("multiCompModel creates correct number of compartments", {
    m <- multiCompModel(3, "micro")
    expect_equal(m$getStateNames(), c("C1", "C2", "C3"))
    expect_true(any(sapply(m$observables, function(o) o$name) == "C1Conc"))
})

test_that("multiCompModel micro parametrization reactions are correct", {
    m <- multiCompModel(2, "micro")
    reaction_strings <- sapply(m$reactions, function(r) paste(r$from, r$to, deparse(r$rate)))
    # Elimination reaction
    expect_true(any(grepl("C1  k10 \\* C1", reaction_strings)))
    # Inter-compartment reactions
    expect_true(any(grepl("C1 C2 k12 \\* C1", reaction_strings)))
    expect_true(any(grepl("C2 C1 k21 \\* C2", reaction_strings)))
})

test_that("multiCompModel macro parametrization reactions are correct", {
    m <- multiCompModel(2, "macro")
    reaction_strings <- sapply(m$reactions, function(r) paste(r$from, r$to, deparse(r$rate)))
    # Elimination scaled by volume
    expect_true(any(grepl("C1  CL/V1 \\* C1", reaction_strings)))
    # Inter-compartment flows
    expect_true(any(grepl("C1 C2 Q12/V1 \\* C1", reaction_strings)))
    expect_true(any(grepl("C2 C1 Q12/V2 \\* C2", reaction_strings)))
})
