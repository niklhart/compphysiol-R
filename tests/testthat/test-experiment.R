test_that("experiments preserve unit-aware inputs and measurement rows", {
    p <- parameters(BW = 70 [kg], group = "adult")
    d <- dosing(time = 0 [min], amount = 100 [mg])
    schedule <- data.frame(
        time = with_units(c(120, 60, 60) [min]),
        observable = c("C", "A", "A")
    )
    x <- experiment(parameters = p, dosing = d, measurements = schedule, start = 0 [h])
    expect_s3_class(x, "Experiment")
    expect_identical(x$parameters, p)
    expect_identical(x$dosing, d)
    expect_identical(x$measurements, schedule)
    expect_equal(x$start, with_units(0 [h]))
    expect_identical(validate_experiment(x), x)
    expect_output(print(x), "Experiment")
})

test_that("empty and unit-free experiments are supported", {
    x <- experiment()
    expect_s3_class(x$parameters, "Parameters")
    expect_s3_class(x$dosing, "Dosing")
    expect_equal(nrow(x$measurements), 0L)
    expect_equal(x$start, 0)
    expect_no_error(experiment(start = -2, measurements = data.frame(time = -1, observable = "C")))
    expect_no_error(experiment(start = 0 [h]))
})

test_that("experiment components and measurement schema are checked", {
    expect_error(experiment(parameters = list(BW = 70)), "Parameters")
    expect_error(experiment(dosing = list()), "Dosing")
    expect_error(experiment(measurements = list()), "data frame")
    expect_error(experiment(measurements = data.frame(time = 1, state = "C")), "observable")
    for (obs in list(NA_character_, "", 1, factor("C"))) {
        expect_error(experiment(measurements = data.frame(time = 1, observable = obs)), "observable")
    }
    for (time in list(NA_real_, Inf, "1")) {
        expect_error(experiment(measurements = data.frame(time = time, observable = "C")), "measurement")
    }
    for (start in list(numeric(), c(0, 1), NA_real_, Inf, "0")) {
        expect_error(experiment(start = start), "start")
    }
})

test_that("experiment schedules require compatible time dimensions", {
    hours <- data.frame(time = with_units(1 [h]), observable = "C")
    expect_error(experiment(measurements = hours), "unit")
    expect_error(experiment(start = 0 [h], measurements = data.frame(time = 1, observable = "C")), "unit")
    expect_error(experiment(start = 0 [kg]), "time units")
    expect_error(experiment(start = 0 [h], measurements = data.frame(time = with_units(1 [kg]), observable = "C")), "time units")
    expect_error(experiment(start = 0 [h], dosing = dosing(time = 0, amount = 1)), "unit")
    expect_error(experiment(dosing = dosing(time = Inf, amount = 1)), "dosing")
    expect_error(experiment(start = 0 [h], dosing = dosing(time = 0 [h], amount = 1 [mg], duration = 1 [kg])), "time units")
    expect_no_error(experiment(start = 0 [h], dosing = dosing(time = 30 [min], amount = 1 [mg], duration = 60 [min])))
})

test_that("measurements and doses cannot precede the experiment start", {
    schedule <- data.frame(time = with_units(30 [min]), observable = "C")
    expect_error(experiment(start = 1 [h], measurements = schedule), "before.*start")
    expect_error(experiment(start = 1 [h], dosing = dosing(time = 30 [min], amount = 1 [mg])), "before.*start")
    expect_no_error(experiment(start = 0.5 [h], measurements = schedule))
})

test_that("model validation accepts only declared observables", {
    model <- compartment_model() |> add_compartment("Central") |>
        add_molecule("drug") |> add_observable(C = c[drug, Central])
    x <- experiment(measurements = data.frame(time = 1, observable = "C"))
    expect_identical(validate_experiment(x, model), x)
    x$measurements$observable <- "a[drug,Central]"
    expect_error(validate_experiment(x, model), "Unknown observable")
    expect_error(validate_experiment(x, list()), "CompartmentModel")
    x$measurements$time <- -1
    expect_error(validate_experiment(x), "before.*start")
})
