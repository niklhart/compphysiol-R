# Test the Compartments class

test_that("Compartments object is created correctly", {
    cmt <- compartments(c("adi", "bon"), volume = c(10, 20))
    expect_s3_class(cmt, "Compartments")
})

test_that("Length and names methods work for Compartments", {
    cmt <- compartments(c("adi", "bon", "gut"), volume = c(10, 20, 30))
    expect_equal(length(cmt), 3)
    expect_equal(names(cmt), c("adi", "bon", "gut"))
})

test_that("Print method for Compartments works", {
    cmt <- compartments(c("adi", "bon"), volume = c(10, 20))
    expect_snapshot(print(cmt))
})

test_that("Subsetting Compartments object works", {
    cmt <- compartments(c("adi", "bon", "gut"), volume = c(10, 20, 30))
    cmt_subset <- cmt[1:2]
    expect_equal(length(cmt_subset), 2)
    expect_equal(names(cmt_subset), c("adi", "bon"))
})

test_that("Compartments with mismatched name and initial lengths throw error", {
    expect_error(compartments(c("adi", "bon"), volume = c(10, 20, 30)))
})

test_that("Zero inputs to compartments() results in valid but empty object", {
    cmt <- compartments()
    expect_equal(length(cmt), 0)
})

test_that("Compartments with units work correctly", {

    val <- units::set_units(1, 'mg')
    cmt <- compartments("test", val)
    expect_equal(cmt$volume[[1]], val)

    cmt <- compartments("test", 1, unit = "mg")
    expect_equal(cmt$volume[[1]], val)

    cmt <- compartments("test", 1 [mg])
    expect_equal(cmt$volume[[1]], val)

})

test_that("Compartments concatenation with units works correctly", {

    cmt1 <- compartments("adi", 10, unit = "mg")
    cmt2 <- compartments("bon", 20, unit = "mg")
    cmt12 <- c(cmt1, cmt2)

    expect_equal(do.call(c,cmt12$volume), units::set_units(c(10, 20), "mg"))
})


test_that("Parametrized volumes work correctly", {

    # Without replacement
    cmt <- compartments("test", volume = "V")
    expect_equal(cmt$volume[[1]], as.name("V"))

    # Without replacement, volume recycling
    cmt <- compartments(c("a", "b"), volume = "V")
    expect_equal(cmt$volume[[1]], as.name("V"))
    expect_equal(cmt$volume[[2]], as.name("V"))

    # With replacement
    cmt <- compartments(c("a", "b"), volume = "V{name}")
    expect_equal(cmt$volume[[1]], as.name("Va"))
    expect_equal(cmt$volume[[2]], as.name("Vb"))
})

