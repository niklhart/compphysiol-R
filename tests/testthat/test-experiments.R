test_that("Experiments hold named and unnamed experiments without altering them", {
    a <- experiment(start = 0 [h], parameters = parameters(BW = 70 [kg]))
    b <- experiment(start = 0 [min])
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
    b <- experiments(b = experiment(start = -1 [h]))
    expect_identical(c(a, b), experiments(a = a[[1]], b = b[[1]]))
    expect_identical(c(experiments(), a, experiments()), a)
    expect_identical(c(experiments(), experiments()), experiments())
    expect_error(c(a, experiment()), "Experiments")
})

test_that("collection printing summarizes while single printing shows details", {
    e <- experiment(
        parameters = parameters(BW = 70 [kg]),
        dosing = dosing(time = 0 [h], amount = 100 [mg], cmt = "Central"),
        observations = observation_schedule(c(1, 2) [h], "C"),
        start = 0 [h]
    )
    x <- experiments(treatment = e, empty = experiment(start = 0 [h]))
    summary <- capture.output(result <- print(x))
    expect_identical(result, x)
    expect_length(summary, 3)
    expect_match(summary[2], "treatment.*0.*h.*1 parameter.*1 dosing event.*2 observations scheduled")
    expect_output(print(experiments()), "Experiments: \\(none\\)")
    details <- capture.output(result <- print(x[[1]]))
    expect_identical(result, e)
    expect_true(any(grepl("BW.*70.*kg", details)))
    expect_true(any(grepl("Bolus:.*100.*mg", details)))
    expect_true(any(grepl("observable", details)))
    expect_true(any(grepl("C", details)))
    expect_output(print(experiment()), "Observations: \\(none\\)")
})
test_that("collection time units are consistent even without observations", {
    a <- experiment(start = 0 [h], observations = observation_schedule(60 [min], "C"))
    b <- experiment(start = 0 [s])
    expect_no_error(experiments(a, b))
    expect_error(experiments(a, experiment()), "time.*units")
    expect_error(c(experiments(a), experiments(experiment())), "time.*units")
})

test_that("overlapping experiment parameters require compatible units", {
    a <- experiment(parameters = parameters(BW = 70 [kg], group = "adult"))
    b <- experiment(parameters = parameters(BW = 60000 [g], group = "child"))
    expect_no_error(experiments(a, b))
    expect_no_error(experiments(a, experiment(parameters = parameters(age = 20 [h]))))
    for (p in list(parameters(BW = 70), parameters(BW = 70 [h]))) {
        bad <- experiment(parameters = p)
        expect_error(experiments(a, bad), "parameter 'BW'.*units")
        expect_error(c(experiments(a), experiments(bad)), "parameter 'BW'.*units")
    }
    expect_error(experiments(experiment(), a, experiment(parameters = parameters(BW = 1 [L]))), "BW")
})

test_that("matching dosing targets require compatible amount units", {
    make <- function(amount, molec = "drug", cmt = "Central") {
        experiment(start = 0 [h], dosing = dosing(time = 0 [h], amount = amount,
                                                  molec = molec, cmt = cmt))
    }
    a <- make(with_units(100 [mg]))
    b <- make(with_units(0.1 [g]))
    expect_no_error(experiments(a, b))
    for (amount in list(100, with_units(1 [mol]))) {
        bad <- make(amount)
        expect_error(experiments(a, bad), "dosing target.*drug.*Central.*units")
        expect_error(c(experiments(a), experiments(bad)), "dosing target")
        expect_no_error(experiments(a, make(amount, molec = "other")))
        expect_no_error(experiments(a, make(amount, cmt = "Other")))
    }
    expect_error(experiments(make(1, NULL, NULL), make(with_units(1 [mg]), NULL, NULL)), "dosing target")
    infusion <- experiment(start = 0 [h], dosing = dosing(time = 0 [min],
        rate = 1 [g/h], duration = 60 [min], molec = "drug", cmt = "Central"))
    expect_no_error(experiments(a, infusion))
    expect_identical(experiments(a, b)[[2]], b)
})
