test_that("observation schedules validate and preserve rows", {
    schedule <- observation_schedule(
        time = with_units(c(120, 60, 60) [min]),
        observable = c("C", "A", "A"),
        replicate = c(1L, 1L, 2L)
    )
    expect_s3_class(schedule, "ObservationSchedule")
    expect_s3_class(schedule, "data.frame")
    expect_named(schedule, c("time", "observable", "replicate"))
    expect_equal(schedule$observable, c("C", "A", "A"))
    expect_error(observation_schedule(1:2, c("A", "B", "C")), "length")
    expect_error(observation_schedule(1, "A", 2), "named")
    expect_error(observation_schedule(NA_real_, "A"), "finite")
    expect_error(observation_schedule(1, ""), "observable")
    expect_error(observation_schedule(1, factor("A")), "observable")
})

test_that("observation data extend schedules with canonical values", {
    data <- observation_data(
        time = c(1, 2) [h],
        observable = "C",
        value = c(8, 5) [mg/L],
        source = "assay"
    )
    expect_s3_class(data, "ObservationData")
    expect_s3_class(data, "ObservationSchedule")
    expect_named(data, c("time", "observable", "value", "source"))
    schedule <- as_observation_schedule(data)
    expect_s3_class(schedule, "ObservationSchedule")
    expect_false(inherits(schedule, "ObservationData"))
    expect_named(schedule, c("time", "observable", "source"))
    expect_equal(schedule$time, data$time)
    expect_s3_class(data[1, ], "ObservationData")
    expect_s3_class(data[c("time", "observable")], "ObservationSchedule")
    expect_false(inherits(data["time"], "ObservationSchedule"))
    expect_error(observation_data(1, "C", "eight"), "numeric")
    expect_error(as_observation_schedule(data.frame(time = 1, observable = "C")),
                 "ObservationSchedule")
})

test_that("experiments store schedules and data polymorphically as observations", {
    p <- parameters(BW = 70 [kg], group = "adult")
    d <- dosing(time = 0 [min], amount = 100 [mg])
    schedule <- observation_schedule(c(120, 60, 60) [min], c("C", "A", "A"))
    x <- experiment(parameters = p, dosing = d, observations = schedule, start = 0 [h])
    expect_s3_class(x, "Experiment")
    expect_identical(x$parameters, p)
    expect_identical(x$dosing, d)
    expect_identical(x$observations, schedule)
    expect_equal(x$start, with_units(0 [h]))
    expect_identical(validate_experiment(x), x)

    observed <- observation_data(c(1, 2) [h], "C", c(8, 5) [mg/L])
    y <- experiment(observations = observed)
    expect_identical(y$observations, observed)
    expect_s3_class(y$observations, "ObservationData")
    expect_s3_class(y$observations, "ObservationSchedule")
})

test_that("empty and unit-free experiments are supported", {
    x <- experiment()
    expect_s3_class(x$parameters, "Parameters")
    expect_s3_class(x$dosing, "Dosing")
    expect_s3_class(x$observations, "ObservationSchedule")
    expect_equal(nrow(x$observations), 0L)
    expect_equal(x$start, 0)
    expect_no_error(experiment(start = -2, observations = observation_schedule(-1, "C")))
    expect_no_error(experiment(start = 0 [h]))
})

test_that("experiment components are checked", {
    expect_error(experiment(parameters = list(BW = 70)), "Parameters")
    expect_error(experiment(dosing = list()), "Dosing")
    expect_error(experiment(observations = data.frame(time = 1, observable = "C")),
                 "ObservationSchedule")
    expect_error(experiment(observations = data.frame(time = 1, observable = "C", value = 2)),
                 "ObservationSchedule")
    for (start in list(numeric(), c(0, 1), NA_real_, Inf, "0")) {
        expect_error(experiment(start = start), "start")
    }
})

test_that("experiment schedules require compatible time dimensions", {
    hours <- observation_schedule(1 [h], "C")
    expect_error(experiment(start = 0, observations = hours), "unit")
    expect_error(experiment(start = 0 [h], observations = observation_schedule(1, "C")), "unit")
    expect_error(observation_schedule(1 [kg], "C"), "time units")
    expect_error(experiment(start = 0 [kg]), "time units")
    expect_error(experiment(start = 0 [h], dosing = dosing(time = 0, amount = 1)), "unit")
    expect_error(experiment(dosing = dosing(time = Inf, amount = 1)), "dosing")
    expect_error(experiment(start = 0 [h], dosing = dosing(time = 0 [h], amount = 1 [mg], duration = 1 [kg])), "time units")
    expect_no_error(experiment(start = 0 [h], dosing = dosing(time = 30 [min], amount = 1 [mg], duration = 60 [min])))
})

test_that("NULL start infers zero from nonempty schedules, not the first time", {
    schedule <- observation_schedule(c(2, 4) [h], "C")
    expect_equal(experiment(observations = schedule)$start, with_units(0 [h]))
    expect_equal(experiment(observations = schedule, start = NULL)$start, with_units(0 [h]))
    dose <- dosing(time = 30 [min], amount = 10 [mg])
    expect_equal(experiment(dosing = dose)$start, with_units(0 [min]))
    x <- experiment(dosing = dose, observations = schedule)
    expect_identical(x$start, with_units(0 [min]))
    expect_identical(x$observations, schedule)
    expect_identical(x$dosing, dose)
    expect_identical(experiment(start = NULL)$start, 0)
    expect_identical(experiment(observations = observation_schedule(2, "C"))$start, 0)
    empty <- observation_schedule(with_units(numeric(0) [h]), character())
    expect_identical(experiment(observations = empty)$start, 0)
    expect_equal(experiment(observations = schedule, start = 1 [h])$start, with_units(1 [h]))
})

test_that("inferred starts do not hide incompatible or negative schedule times", {
    schedule <- observation_schedule(2 [h], "C")
    expect_error(experiment(dosing = dosing(time = 0, amount = 1), observations = schedule), "unit")
    expect_error(experiment(dosing = dosing(time = 0 [h], amount = 1),
                           observations = observation_schedule(2, "C")), "unit")
    expect_error(experiment(dosing = dosing(time = -1 [h], amount = 1)), "before.*start")
    expect_error(experiment(observations = observation_schedule(-1, "C")), "before.*start")
})

test_that("observations and doses cannot precede the experiment start", {
    schedule <- observation_schedule(30 [min], "C")
    expect_error(experiment(start = 1 [h], observations = schedule), "before.*start")
    expect_error(experiment(start = 1 [h], dosing = dosing(time = 30 [min], amount = 1 [mg])), "before.*start")
    expect_no_error(experiment(start = 0.5 [h], observations = schedule))
})

test_that("model validation accepts only declared observables", {
    model <- compartment_model() |> add_compartment("Central") |>
        add_molecule("drug") |> add_observable(C = c[drug, Central])
    x <- experiment(observations = observation_schedule(1, "C"))
    expect_identical(validate_experiment(x, model), x)
    x$observations$observable <- "a[drug,Central]"
    expect_error(validate_experiment(x, model), "Unknown observable")
    expect_error(validate_experiment(x, list()), "CompartmentModel")
    x$observations$time <- -1
    expect_error(validate_experiment(x), "before.*start")
})

test_that("experiment validation validates the concrete observation subtype", {
    x <- experiment(observations = observation_data(1, "C", 2))
    x$observations$value <- "invalid"
    expect_error(validate_experiment(x), "numeric")
})
