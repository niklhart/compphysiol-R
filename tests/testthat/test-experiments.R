test_that("Experiments hold named and unnamed experiments without altering them", {
    a <- experiment(start = 0 [h], parameters = parameters(BW = 70 [kg]))
    b <- experiment()
    x <- experiments(first = a, second = b)
    expect_s3_class(x, "Experiments")
    expect_length(x, 2)
    expect_named(x, c("first", "second"))
    expect_identical(x[[1]], a)
    expect_identical(x[["second"]], b)
    expect_null(x[["missing"]])
    expect_error(x[[3]], "out of bounds")
    expect_error(x[[c(1, 1)]], "single experiment")
    expect_identical(experiments(a)[[1]], a)
    expect_length(experiments(), 0)
    expect_error(experiments(a, 1), "Experiment")
})

test_that("Experiments subsetting preserves the collection and requested order", {
    x <- experiments(a = experiment(), b = experiment(start = -1))
    expect_identical(x[], x)
    expect_identical(x[c("b", "a")], experiments(b = x[[2]], a = x[[1]]))
    expect_identical(x[-1], experiments(b = x[[2]]))
    expect_identical(x[c(TRUE, FALSE)], experiments(a = x[[1]]))
    expect_identical(x[c(1, 1)], experiments(a = x[[1]], a = x[[1]]))
    expect_s3_class(x[integer()], "Experiments")
    expect_length(x[integer()], 0)
    expect_error(x["missing"], "Experiment")
    expect_error(x[NA_integer_], "Experiment")
})

test_that("Experiments concatenation retains names, units, and empty collections", {
    a <- experiments(a = experiment(start = 0 [h]))
    b <- experiments(b = experiment(start = -1))
    expect_identical(c(a, b), experiments(a = a[[1]], b = b[[1]]))
    expect_identical(c(experiments(), a, experiments()), a)
    expect_identical(c(experiments(), experiments()), experiments())
    expect_error(c(a, experiment()), "Experiments")
})

test_that("collection printing summarizes while single printing shows details", {
    e <- experiment(
        parameters = parameters(BW = 70 [kg]),
        dosing = dosing(time = 0 [h], amount = 100 [mg], cmt = "Central"),
        measurements = data.frame(time = with_units(c(1, 2) [h]), observable = "C"),
        start = 0 [h]
    )
    x <- experiments(treatment = e, empty = experiment())
    summary <- capture.output(result <- print(x))
    expect_identical(result, x)
    expect_length(summary, 3)
    expect_match(summary[2], "treatment.*0.*h.*1 parameter.*1 dosing event.*2 measurement")
    expect_output(print(experiments()), "Experiments: \\(none\\)")
    details <- capture.output(result <- print(x[[1]]))
    expect_identical(result, e)
    expect_true(any(grepl("BW.*70.*kg", details)))
    expect_true(any(grepl("Bolus:.*100.*mg", details)))
    expect_true(any(grepl("observable", details)))
    expect_true(any(grepl("C", details)))
    expect_output(print(experiment()), "Measurements: \\(none\\)")
})
