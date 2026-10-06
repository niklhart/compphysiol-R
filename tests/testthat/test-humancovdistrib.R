test_that("humancovdistrib returns realized parameter sets", {
    set.seed(42)
    humans <- humancovdistrib(3, "female")

    expect_s3_class(humans, "ParameterSets")
    expect_length(humans, 3)
    expect_null(names(humans))
    expect_true(all(vapply(humans, inherits, logical(1), "Parameters")))
    expect_true(all(vapply(
        humans,
        function(x) identical(names(x), c("species", "type", "sex", "age", "BW", "BH")),
        logical(1)
    )))

    expect_identical(humans[[1]]$species, "human")
    expect_identical(humans[[1]]$type, "Caucasian")
    expect_identical(humans[[1]]$sex, "female")
    expect_equal(humans[[1]]$age, with_units(35 [year]))
    expect_true(units::ud_are_convertible(units::deparse_unit(humans[[1]]$BW), "kg"))
    expect_true(units::ud_are_convertible(units::deparse_unit(humans[[1]]$BH), "m"))
})

test_that("humancovdistrib is reproducible under the R random seed", {
    set.seed(123)
    first <- humancovdistrib(2, "male")
    set.seed(123)
    second <- humancovdistrib(2, "male")

    expect_identical(first, second)
    expect_true(all(vapply(first, function(x) identical(x$sex, "male"), logical(1))))
})

test_that("humancovdistrib validates population size and sex", {
    for (N in list(0, -1, 1.5, c(1, 2), "2", NA_real_)) {
        expect_error(humancovdistrib(N, "female"), "positive integer")
    }
    expect_error(humancovdistrib(1, "unknown"), "male|female")
})
