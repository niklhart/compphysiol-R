test_that("ParameterSets hold named and unnamed Parameters objects", {
    a <- parameters(BW = 65 [kg], sex = "female")
    b <- parameters(BW = 82 [kg], sex = "male")
    population <- parameter_sets(individual_1 = a, b)

    expect_s3_class(population, "ParameterSets")
    expect_length(population, 2)
    expect_named(population, c("individual_1", ""))
    expect_identical(population[[1]], a)
    expect_identical(population[[2]], b)
    expect_s3_class(population[[1]], "Parameters")
    expect_null(population[["missing"]])
    expect_error(population[[3]], "out of bounds")
    expect_error(population[[c(1, 1)]], "single parameter set")
    expect_identical(parameter_sets(a)[[1]], a)
    expect_length(parameter_sets(), 0)
    expect_error(parameter_sets(a, 1), "Parameters")
})

test_that("ParameterSets subsetting preserves the collection", {
    population <- parameter_sets(
        a = parameters(BW = 65 [kg]),
        b = parameters(BW = 82 [kg])
    )

    expect_identical(population[], population)
    expect_s3_class(population["a"], "ParameterSets")
    expect_identical(population["a"][[1]], population[["a"]])
    expect_s3_class(population[integer()], "ParameterSets")
    expect_length(population[integer()], 0)
    expect_error(population[NA_integer_], "Parameters")
    expect_error(population["missing"], "Parameters")
})

test_that("ParameterSets combine collections and reject duplicate names", {
    a <- parameter_sets(a = parameters(BW = 65 [kg]))
    b <- parameter_sets(b = parameters(BW = 82 [kg]))

    expect_identical(c(a, b), parameter_sets(a = a[[1]], b = b[[1]]))
    expect_identical(c(parameter_sets(), a, parameter_sets()), a)
    expect_identical(c(parameter_sets(), parameter_sets()), parameter_sets())
    expect_error(c(a, parameter_sets(a = parameters(BW = 70 [kg]))), "unique")
    expect_error(parameter_sets(a = parameters(), a = parameters()), "unique")
    expect_error(c(a, parameters(BW = 70 [kg])), "ParameterSets")

    unnamed <- c(parameter_sets(parameters(BW = 65)), parameter_sets(parameters(BW = 82)))
    expect_length(unnamed, 2)
    expect_null(names(unnamed))
})

test_that("shared parameter names require compatible units", {
    kg <- parameters(BW = 65 [kg], sex = "female")
    g <- parameters(BW = 82000 [g], sex = "male")

    expect_no_error(parameter_sets(kg, g))
    expect_identical(parameter_sets(kg, g)[[2]], g)
    expect_no_error(parameter_sets(kg, parameters(age = 40 [year])))
    expect_no_error(parameter_sets(parameters(group = 1), parameters(group = "control")))

    for (bad in list(parameters(BW = 82), parameters(BW = 82 [L]))) {
        expect_error(parameter_sets(kg, bad), "parameter 'BW'.*units")
        expect_error(c(parameter_sets(kg), parameter_sets(bad)), "parameter 'BW'.*units")
    }
})

test_that("trusted construction can skip only unit validation", {
    incompatible <- list(parameters(x = 1), parameters(x = 1 [kg]))
    trusted <- .new_parameter_sets(incompatible, check_units = FALSE)

    expect_s3_class(trusted, "ParameterSets")
    expect_s3_class(trusted[1], "ParameterSets")
    expect_error(.new_parameter_sets(incompatible), "parameter 'x'.*units")
    expect_error(.new_parameter_sets(list(parameters(), 1), check_units = FALSE),
                 "Parameters")
    expect_error(
        .new_parameter_sets(list(a = parameters(), a = parameters()), check_units = FALSE),
        "unique"
    )
})

test_that("ParameterSets print a compact summary", {
    population <- parameter_sets(
        individual_1 = parameters(BW = 65 [kg], sex = "female"),
        parameters(BW = 82 [kg], sex = "male")
    )
    output <- capture.output(returned <- print(population))

    expect_identical(returned, population)
    expect_match(output[2], "individual_1: BW = 65 [kg]; sex = female", fixed = TRUE)
    expect_match(output[3], "(2) BW = 82 [kg]; sex = male", fixed = TRUE)
    expect_output(print(parameter_sets()), "Parameter sets: \\(none\\)")
    expect_output(
        print(parameter_sets(empty = parameters())),
        "empty: \\(none\\)"
    )
})
