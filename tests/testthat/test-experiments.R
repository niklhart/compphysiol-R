test_that("Experiments hold named and unnamed experiments without altering them", {
    a <- experiment(start = 0 [h], parameters = parameters(BW = 70 [kg]))
    b <- experiment(start = 0 [min])
    x <- experiments(first = a, second = b)
    expect_s3_class(x, "compphysiol_experiments")
    expect_length(x, 2)
    expect_named(x, c("first", "second"))
    expect_identical(x[[1]], a)
    expect_identical(x[["second"]], b)
    expect_null(x[["missing"]])
    expect_error(x[[3]], "out of bounds")
    expect_error(x[[c(1, 1)]], "single experiment")
    expect_identical(experiments(a)[[1]], a)
    expect_length(experiments(), 0)
    expect_error(experiments(a, 1), "experiment")
})

test_that("Experiments subsetting preserves the collection and requested order", {
    x <- experiments(a = experiment(), b = experiment(start = -1))
    expect_identical(x[], x)
    expect_identical(x[c("b", "a")], experiments(b = x[[2]], a = x[[1]]))
    expect_identical(x[-1], experiments(b = x[[2]]))
    expect_identical(x[c(TRUE, FALSE)], experiments(a = x[[1]]))
    expect_error(x[c(1, 1)], "unique")
    expect_s3_class(x[integer()], "compphysiol_experiments")
    expect_length(x[integer()], 0)
    expect_error(x["missing"], "experiment")
    expect_error(x[NA_integer_], "must not contain NA")
})

test_that("experiment collections track component homogeneity", {
    dose <- dosing(time = 0, amount = 100, cmt = "Central")
    a <- experiment(
        parameters = parameters(CL = 1), dosing = dose,
        observations = observation_data(c(1, 2), "C", c(8, 5)), start = 0
    )
    b <- experiment(
        parameters = parameters(CL = 2), dosing = dose,
        observations = observation_data(c(1, 2), "C", c(7, 4)), start = 0
    )
    x <- experiments(a = a, b = b)

    expect_identical(
        .experiments_component_homogeneity(x),
        c(
            start = TRUE, parameter_schema = TRUE, parameters = FALSE,
            dosing = TRUE, observation_plan = TRUE, observations = FALSE
        )
    )
    expect_true(all(.experiments_component_homogeneity(x[1])))

    different_units <- experiments(
        experiment(parameters = parameters(BW = 70 [kg])),
        experiment(parameters = parameters(BW = 70000 [g]))
    )
    expect_false(.experiments_component_homogeneity(different_units)[["parameter_schema"]])
})

test_that("collection combination and replacement maintain homogeneity", {
    a <- experiment(parameters = parameters(CL = 1))
    b <- experiment(parameters = parameters(CL = 2))
    homogeneous <- experiments(a = a, b = a)
    heterogeneous <- experiments(c = a, d = b)

    expect_true(all(.experiments_component_homogeneity(homogeneous)))
    combined <- c(homogeneous, heterogeneous)
    expect_false(.experiments_component_homogeneity(combined)[["parameters"]])
    expect_true(.experiments_component_homogeneity(combined)[["parameter_schema"]])

    heterogeneous[[2]] <- a
    expect_true(all(.experiments_component_homogeneity(heterogeneous)))
    heterogeneous[[2]]$parameters <- parameters(CL = 3)
    expect_false(.experiments_component_homogeneity(heterogeneous)[["parameters"]])

    heterogeneous[2] <- experiments(replacement = a)
    expect_true(all(.experiments_component_homogeneity(heterogeneous)))
    expect_named(heterogeneous, c("c", "d"))

    expect_error(
        heterogeneous[["2"]] <- experiment(),
        "positive whole numbers.*reserved"
    )
})

test_that("Experiment names are optional but nonempty names are unique", {
    expect_error(
        experiments(a = experiment(), a = experiment()),
        "Experiment names must be unique"
    )
    expect_error(
        c(experiments(a = experiment()), experiments(a = experiment())),
        "Experiment names must be unique"
    )

    unnamed <- experiments(experiment(), experiment())
    expect_error(unnamed[c(1, 1)], "unique")
    expect_length(c(unnamed, experiments(experiment())), 3L)
    expect_error(
        experiments(`2` = experiment()),
        "positive whole numbers.*reserved"
    )
})

test_that("observations collects data with stable experiment identifiers", {
    x <- experiments(
        treated = experiment(observations = observation_data(
            c(1, 2) [h], "C", c(8, 5) [mg/L], replicate = c("a", "b")
        )),
        experiment(observations = observation_data(
            60 [min], "C", 7 [mg/L], replicate = "a"
        ))
    )

    out <- observations(x)

    expect_s3_class(out, "compphysiol_observation_data")
    expect_named(out, c("time", "observable", "value", "replicate", "experiment"))
    expect_identical(as.character(out$experiment), c("treated", "treated", "2"))
    expect_identical(levels(out$experiment), c("treated", "2"))
    expect_equal(out$time, with_units(c(1, 2, 1) [h]))
    expect_equal(out$value, with_units(c(8, 5, 7) [mg/L]))
    expect_identical(out$replicate, c("a", "b", "a"))
})

test_that("observations preserves schedules and rejects ambiguous collections", {
    schedules <- experiments(
        experiment(observations = observation_schedule(1, "C")),
        experiment(observations = observation_schedule(2, "C"))
    )
    out <- observations(schedules)
    expect_s3_class(out, "compphysiol_observation_schedule")
    expect_false(inherits(out, "compphysiol_observation_data"))
    expect_identical(levels(out$experiment), c("1", "2"))

    empty <- observations(experiments())
    expect_s3_class(empty, "compphysiol_observation_schedule")
    expect_named(empty, c("time", "observable", "experiment"))

    mixed <- experiments(
        experiment(observations = observation_schedule(1, "C")),
        experiment(observations = observation_data(1, "C", 2))
    )
    expect_error(observations(mixed), "mixture")

    columns <- experiments(
        experiment(observations = observation_schedule(1, "C", note = "a")),
        experiment(observations = observation_schedule(1, "C"))
    )
    expect_error(observations(columns), "identical columns")

    reserved <- experiments(experiment(
        observations = observation_schedule(1, "C", experiment = "source")
    ))
    expect_error(observations(reserved), "reserved")

})

test_that("Experiments subsetting trusts the collection contract", {
    x <- experiments(a = experiment(observations = observation_schedule(1, "A")))
    x[[1]]$observations$observable <- ""

    subset <- x[1]

    expect_s3_class(subset, "compphysiol_experiments")
    expect_identical(subset[[1]], x[[1]])
})

test_that("experiment collection low-level construction trusts canonical input", {
    x <- .new_experiments(list(experiment(), "not an experiment"))
    expect_s3_class(x, "compphysiol_experiments")
})

test_that("Experiments concatenation retains names, units, and empty collections", {
    a <- experiments(a = experiment(start = 0 [h]))
    b <- experiments(b = experiment(start = -1 [h]))
    expect_identical(c(a, b), experiments(a = a[[1]], b = b[[1]]))
    expect_identical(c(experiments(), a, experiments()), a)
    expect_identical(c(experiments(), experiments()), experiments())
    expect_error(c(a, experiment()), "experiments")
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
    expect_length(summary, 4)
    expect_match(summary[2], "shared: start = 0.*h")
    expect_match(summary[3], "treatment.*1 parameter.*1 dosing event.*2 observations scheduled")
    expect_identical(summary[4], "  (2) empty")
    expect_output(print(experiments()), "Experiments: \\(none\\)")
    details <- capture.output(result <- print(x[[1]]))
    expect_identical(result, e)
    expect_true(any(grepl("BW.*70.*kg", details)))
    expect_true(any(grepl("Bolus:.*100.*mg", details)))
    expect_true(any(grepl("observable", details)))
    expect_true(any(grepl("C", details)))
    expect_output(print(experiment()), "Observations: \\(none\\)")
})

test_that("collection printing lifts shared components and omits empty ones", {
    shared <- experiment(
        dosing = dosing(time = 0, amount = 10),
        observations = observation_schedule(c(1, 2), "C")
    )
    unnamed <- experiments(shared, shared)
    output <- capture.output(print(unnamed))
    expect_identical(output[1], " Experiments: 2 unnamed")
    expect_match(output[2], "shared: start = 0; 1 dosing event; 2 observations scheduled")
    expect_length(output, 2L)

    named <- experiments(control = shared, treatment = shared)
    output <- capture.output(print(named))
    expect_length(output, 4L)
    expect_identical(output[3:4], c("  (1) control", "  (2) treatment"))

    singleton <- capture.output(print(experiments(experiment())))
    expect_identical(singleton, c(" Experiments:", "  (1) start = 0"))
})

test_that("collection printing distinguishes shared schemas from shared values", {
    dose <- dosing(time = 0, amount = 10)
    x <- experiments(
        experiment(
            parameters = parameters(CL = 1), dosing = dose,
            observations = observation_data(c(1, 2), "C", c(8, 5))
        ),
        experiment(
            parameters = parameters(CL = 2), dosing = dose,
            observations = observation_data(c(1, 2), "C", c(7, 4))
        )
    )
    output <- capture.output(print(x))
    expect_match(output[2], "parameter schema \\(1 parameter\\)")
    expect_match(output[2], "observation plan \\(2 rows\\)")
    expect_match(output[3], "parameter values vary; observations with data")
    expect_match(output[4], "parameter values vary; observations with data")
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
