test_that("Experiments hold named and unnamed experiments without altering them", {
    a <- experiment(start = 0 [h], parameters = parameters(BW = 70 [kg]))
    b <- experiment(start = 0 [min])
    x <- c(first = a, second = b)
    expect_s3_class(x, "compphysiol_experiments")
    expect_length(x, 2)
    expect_named(x, c("first", "second"))
    expect_identical(x[1], c(first = a))
    expect_identical(x["second"], c(second = b))
    expect_error(x[[1]], "not supported")
    expect_error(x[["missing"]], "not supported")
    expect_length(experiment()[integer()], 0)
    expect_error(c(a, 1), "experiment")
})

test_that("Experiments subsetting preserves the collection and requested order", {
    x <- c(a = experiment(), b = experiment(start = -1))
    expect_identical(x[], x)
    expect_identical(x[c("b", "a")], c(b = x[2], a = x[1]))
    expect_identical(x[-1], c(b = x[2]))
    expect_identical(x[c(TRUE, FALSE)], c(a = x[1]))
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
    x <- c(a = a, b = b)

    expect_identical(
        .experiments_component_homogeneity(x),
        c(
            start = TRUE, parameter_schema = TRUE, parameters = FALSE,
            dosing = TRUE, observation_plan = TRUE, observations = FALSE
        )
    )
    expect_true(all(.experiments_component_homogeneity(x[1])))

    different_units <- c(
        experiment(parameters = parameters(BW = 70 [kg])),
        experiment(parameters = parameters(BW = 70000 [g]))
    )
    expect_false(.experiments_component_homogeneity(different_units)[["parameter_schema"]])
})

test_that("collection combination and replacement maintain homogeneity", {
    a <- experiment(parameters = parameters(CL = 1))
    b <- experiment(parameters = parameters(CL = 2))
    homogeneous <- c(a = a, b = a)
    heterogeneous <- c(c = a, d = b)

    expect_true(all(.experiments_component_homogeneity(homogeneous)))
    combined <- c(homogeneous, heterogeneous)
    expect_false(.experiments_component_homogeneity(combined)[["parameters"]])
    expect_true(.experiments_component_homogeneity(combined)[["parameter_schema"]])

    expect_error(heterogeneous[[2]] <- a, "not supported")
    heterogeneous[2] <- a
    expect_true(all(.experiments_component_homogeneity(heterogeneous)))
    heterogeneous[2] <- experiment(parameters = parameters(CL = 3))
    expect_false(.experiments_component_homogeneity(heterogeneous)[["parameters"]])

    heterogeneous[2] <- c(replacement = a)
    expect_true(all(.experiments_component_homogeneity(heterogeneous)))
    expect_named(heterogeneous, c("c", "d"))

    expect_error(heterogeneous[["2"]] <- experiment(), "not supported")
})

test_that("Experiment names are optional but nonempty names are unique", {
    expect_error(
        c(a = experiment(), a = experiment()),
        "Experiment names must be unique"
    )
    expect_error(
        c(c(a = experiment()), c(a = experiment())),
        "Experiment names must be unique"
    )

    unnamed <- c(experiment(), experiment())
    expect_error(unnamed[c(1, 1)], "unique")
    expect_length(c(unnamed, c(experiment())), 3L)
    expect_error(
        c(`2` = experiment()),
        "positive whole numbers.*reserved"
    )
    expect_error(
        c(experiment(name = "same"), experiment(name = "same")),
        "Experiment names must be unique"
    )
})

test_that("observations collects data with stable experiment identifiers", {
    x <- c(
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
    schedules <- c(
        experiment(observations = observation_schedule(1, "C")),
        experiment(observations = observation_schedule(2, "C"))
    )
    out <- observations(schedules)
    expect_s3_class(out, "compphysiol_observation_schedule")
    expect_false(inherits(out, "compphysiol_observation_data"))
    expect_identical(levels(out$experiment), c("1", "2"))

    empty <- observations(experiment()[integer()])
    expect_s3_class(empty, "compphysiol_observation_schedule")
    expect_named(empty, c("time", "observable", "experiment"))

    mixed <- c(
        experiment(observations = observation_schedule(1, "C")),
        experiment(observations = observation_data(1, "C", 2))
    )
    expect_error(observations(mixed), "mixture")

    columns <- c(
        experiment(observations = observation_schedule(1, "C", note = "a")),
        experiment(observations = observation_schedule(1, "C"))
    )
    expect_error(observations(columns), "identical columns")

    reserved <- c(experiment(
        observations = observation_schedule(1, "C", experiment = "source")
    ))
    expect_error(observations(reserved), "reserved")

})

test_that("Experiments subsetting trusts the collection contract", {
    x <- c(a = experiment(observations = observation_schedule(1, "A")))
    elements <- .experiments_elements(x)
    elements[[1]]$observations$observable <- ""
    x <- .new_experiments(elements, .experiments_component_homogeneity(x))

    subset <- x[1]

    expect_s3_class(subset, "compphysiol_experiments")
    expect_identical(.experiment_entry(subset), .experiment_entry(x))
})

test_that("experiment collection low-level construction trusts canonical input", {
    x <- .new_experiments(list(experiment(), "not an experiment"))
    expect_s3_class(x, "compphysiol_experiments")
})

test_that("Experiments concatenation retains names, units, and empty collections", {
    a <- c(a = experiment(start = 0 [h]))
    b <- c(b = experiment(start = -1 [h]))
    expect_identical(c(a, b), c(a = a, b = b))
    empty <- experiment()[integer()]
    expect_identical(c(empty, a, empty), a)
    expect_identical(c(empty, empty), empty)
    expect_identical(
        c(a, experiment(start = 1 [h])),
        c(a = a, experiment(start = 1 [h]))
    )
})

test_that("individual experiments combine directly with c", {
    a <- experiment(start = 0 [h])
    b <- experiment(start = -1 [h])

    direct <- c(control = a, treatment = b)
    expect_s3_class(direct, "compphysiol_experiments")
    expect_identical(direct, c(control = a, treatment = b))
    expect_identical(c(a, c(treatment = b)), c(a, treatment = b))
    expect_identical(c(c(control = a), b), c(control = a, b))
    expect_error(c(a, 1), "experiment collections")
    expect_error(c(`2` = a, b), "positive whole numbers.*reserved")
    expect_error(c(a, experiment()), "time.*units")

    original <- experiment(name = "original")
    expect_named(c(original), "original")
    expect_named(c(replacement = original), "replacement")
})

test_that("collection printing summarizes while single printing shows details", {
    e <- experiment(
        parameters = parameters(BW = 70 [kg]),
        dosing = dosing(time = 0 [h], amount = 100 [mg], cmt = "Central"),
        observations = observation_schedule(c(1, 2) [h], "C"),
        start = 0 [h]
    )
    x <- c(treatment = e, empty = experiment(start = 0 [h]))
    summary <- capture.output(result <- print(x))
    expect_identical(result, x)
    expect_length(summary, 4)
    expect_match(summary[2], "shared: start = 0.*h")
    expect_match(summary[3], "treatment.*BW = 70.*kg.*1 dosing event.*C\\(t = 1.*h.*\\).*[.][.][.] 1 more observation")
    expect_identical(summary[4], "  (2) empty")
    expect_output(print(experiment()[integer()]), "Experiments: \\(none\\)")
    details <- capture.output(result <- print(x[1]))
    expect_identical(result, x[1])
    expect_true(any(grepl("BW.*70.*kg", details)))
    expect_true(any(grepl("Bolus:.*100.*mg", details)))
    expect_true(any(grepl("observable", details)))
    expect_true(any(grepl("C", details)))
    expect_output(print(experiment()), "Observations: \\(none\\)")
    expect_identical(
        capture.output(print(experiment(name = "treatment")))[1],
        "Experiment: treatment"
    )
})

test_that("collection printing lifts shared components and omits empty ones", {
    shared <- experiment(
        dosing = dosing(time = 0, amount = 10),
        observations = observation_schedule(c(1, 2), "C")
    )
    unnamed <- c(shared, shared)
    output <- capture.output(print(unnamed))
    expect_identical(output[1], " Experiments: 2 unnamed")
    expect_match(output[2], "shared: start = 0; 1 dosing event; 2 observations scheduled")
    expect_length(output, 2L)

    named <- c(control = shared, treatment = shared)
    output <- capture.output(print(named))
    expect_length(output, 4L)
    expect_identical(output[3:4], c("  (1) control", "  (2) treatment"))

    singleton <- capture.output(print(c(experiment())))
    expect_identical(singleton, c(
        "Experiment:", " Start: 0", " Parameters: (none)",
        " Dosing: (none)", " Observations: (none)"
    ))
    expect_identical(
        capture.output(print(c(renamed = experiment(name = "original"))))[1],
        "Experiment: renamed"
    )
})

test_that("collection printing distinguishes shared schemas from shared values", {
    dose <- dosing(time = 0, amount = 10)
    x <- c(
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
    expect_match(output[3], "CL = 1; C\\(t = 1\\) = 8, [.][.][.] 1 more observation")
    expect_match(output[4], "CL = 2; C\\(t = 1\\) = 7, [.][.][.] 1 more observation")
})

test_that("collection printing bounds varying parameter and observation previews", {
    make <- function(offset) experiment(
        parameters = parameters(a = offset, b = offset + 1, c = offset + 2),
        observations = observation_data(c(1, 2, 3), "C", offset + c(1, 2, 3))
    )
    output <- capture.output(print(c(make(0), make(10))))
    expect_match(output[3], "a = 0, b = 1, [.][.][.] 1 more parameter")
    expect_match(output[3], "C\\(t = 1\\) = 1, [.][.][.] 2 more observations")
    expect_match(output[3], "more parameter; C\\(t = 1\\)")
    expect_false(any(grepl("c = 2", output, fixed = TRUE)))
})

test_that("collection time units are consistent even without observations", {
    a <- experiment(start = 0 [h], observations = observation_schedule(60 [min], "C"))
    b <- experiment(start = 0 [s])
    expect_no_error(c(a, b))
    expect_error(c(a, experiment()), "time.*units")
    expect_error(c(c(a), c(experiment())), "time.*units")
})

test_that("overlapping experiment parameters require compatible units", {
    a <- experiment(parameters = parameters(BW = 70 [kg], group = "adult"))
    b <- experiment(parameters = parameters(BW = 60000 [g], group = "child"))
    expect_no_error(c(a, b))
    expect_no_error(c(a, experiment(parameters = parameters(age = 20 [h]))))
    for (p in list(parameters(BW = 70), parameters(BW = 70 [h]))) {
        bad <- experiment(parameters = p)
        expect_error(c(a, bad), "parameter 'BW'.*units")
        expect_error(c(c(a), c(bad)), "parameter 'BW'.*units")
    }
    expect_error(c(experiment(), a, experiment(parameters = parameters(BW = 1 [L]))), "BW")
})

test_that("matching dosing targets require compatible amount units", {
    make <- function(amount, molec = "drug", cmt = "Central") {
        experiment(start = 0 [h], dosing = dosing(time = 0 [h], amount = amount,
                                                  molec = molec, cmt = cmt))
    }
    a <- make(with_units(100 [mg]))
    b <- make(with_units(0.1 [g]))
    expect_no_error(c(a, b))
    for (amount in list(100, with_units(1 [mol]))) {
        bad <- make(amount)
        expect_error(c(a, bad), "dosing target.*drug.*Central.*units")
        expect_error(c(c(a), c(bad)), "dosing target")
        expect_no_error(c(a, make(amount, molec = "other")))
        expect_no_error(c(a, make(amount, cmt = "Other")))
    }
    expect_error(c(make(1, NULL, NULL), make(with_units(1 [mg]), NULL, NULL)), "dosing target")
    infusion <- experiment(start = 0 [h], dosing = dosing(time = 0 [min],
        rate = 1 [g/h], duration = 60 [min], molec = "drug", cmt = "Central"))
    expect_no_error(c(a, infusion))
    expect_identical(c(a, b)[2], b)
})
