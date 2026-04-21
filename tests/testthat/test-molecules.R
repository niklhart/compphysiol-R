# Test the Molecules class

test_that("Molecules object is created correctly", {
    molec <- molecules("NFkB", cmt = "cytoplasm", initial = 100)
    expect_s3_class(molec, "Molecules")
})

test_that("Length and names methods work for Molecules", {
    molec <- molecules(c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus"), initial = c(100, 50))
    expect_equal(length(molec), 2)
    expect_equal(names(molec), c("NFkB", "IkB"))
})

test_that("Print method for Molecules works", {
    molec <- molecules(c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus"), initial = c(100, 50))
    expect_snapshot(print(molec))
})

test_that("Subsetting Molecules object works", {
    molec <- molecules(c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus"), initial = c(100, 50))
    molec_subset <- molec[1]
    expect_equal(length(molec_subset), 1)
    expect_equal(names(molec_subset), "NFkB")
})

test_that("Molecules with mismatched argument lengths throw error", {
    expect_error(molecules(c("NFkB", "IkB"), initial = c(100, 50, 75)))
    expect_error(molecules(c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus", "extracellular")))
})

test_that("Zero inputs to molecules() results in valid but empty object", {
    molec <- molecules()
    expect_equal(length(molec), 0)
})

test_that("Molecules with units work correctly", {

    val <- units::set_units(1, 'mg')
    molec <- molecules("A", initial = val)
    expect_equal(molec$init[[1]], val)

    molec <- molecules("B", initial = 1, unit = "mg")
    expect_equal(molec$init[[1]], val)

    molec <- molecules("C", initial = 1[mg])
    expect_equal(molec$init[[1]], val)

})

test_that("Molecules concatenation with units works correctly", {

    molec1 <- molecules("A")
    molec2 <- molecules("B")
    expect_equal(names(c(molec1, molec2)), c("A", "B"))
})


test_that("Parametrized initial conditions work correctly", {
    # This test is a placeholder for when we implement parametrized molecules
    skip("Parametrized initial conditions not yet implemented")
})
