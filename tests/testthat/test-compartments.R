# Test the Compartments class

test_that("Compartments object is created correctly", {
    comp <- compartments(c("adi", "bon"), c(10, 20))
    expect_s3_class(comp, "Compartments")
})

test_that("Length and names methods work for Compartments", {
    comp <- compartments(c("adi", "bon", "gut"), c(10, 20, 30))
    expect_equal(length(comp), 3)
    expect_equal(names(comp), c("adi", "bon", "gut"))
})

test_that("Print method for Compartments works", {
    comp <- compartments(c("adi", "bon"), c(10, 20))
    expect_snapshot(print(comp))
})

test_that("Subsetting Compartments object works", {
    comp <- compartments(c("adi", "bon", "gut"), c(10, 20, 30))
    comp_subset <- comp[1:2]
    expect_equal(length(comp_subset), 2)
    expect_equal(names(comp_subset), c("adi", "bon"))
})

test_that("Compartments with mismatched name and initial lengths throw error", {
    expect_error(compartments(c("adi", "bon"), c(10, 20, 30)))
    expect_error(compartments(c("adi", "bon", "gut"), c(10, 20)))
})

test_that("Adding empty compartment names works", {
    comp <- compartments(name = character(0))
    expect_equal(length(comp), 0)
})
