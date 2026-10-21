test_that("object-valued updates require an explicit mode", {
    study <- experiment(parameters = parameters(BW = 70))
    expect_error(update(study, parameters = parameters(BW = 65)),
                 "mode must be specified.*parameters")
    expect_error(update(study, parameters = parameters(BW = 65), mode = "merge"),
                 "replace.*update")
    expect_identical(update(study), study)
})

test_that("parameter replacement and updating have explicit uniform semantics", {
    study <- experiment(parameters = parameters(BW = 70, V = 5))
    replaced <- update(study, parameters = parameters(BW = 65), mode = "replace")
    expect_identical(replaced$parameters, parameters(BW = 65))
    updated <- update(study, parameters = parameters(BW = 65, sex = "female"),
                      mode = "update")
    expect_identical(updated$parameters, parameters(BW = 65, V = 5, sex = "female"))
})

test_that("callbacks transform components without a mode", {
    studies <- experiments(
        experiment(parameters = parameters(BW = 65, V = 5)),
        experiment(parameters = parameters(BW = 80, V = 6))
    )
    out <- update(studies, parameters = function(p) p[setdiff(names(p), "V")])
    expect_identical(out[[1]]$parameters, parameters(BW = 65))
    expect_identical(out[[2]]$parameters, parameters(BW = 80))
    mixed <- update(studies, parameters = function(p) p["BW"],
                    dosing = dosing(), mode = "replace")
    expect_identical(mixed[[1]]$parameters, parameters(BW = 65))
    expect_identical(mixed[[1]]$dosing, dosing())
})

test_that("parameter sets broadcast an experiment and align with collections", {
    protocol <- experiment(parameters = parameters(route = "oral"))
    population <- parameter_sets(
        alice = parameters(BW = 65), bob = parameters(BW = 80)
    )
    expanded <- update(protocol, parameters = population, mode = "update")
    expect_s3_class(expanded, "compphysiol_experiments")
    expect_named(expanded, c("alice", "bob"))
    expect_identical(expanded[[1]]$parameters, parameters(route = "oral", BW = 65))

    reversed <- parameter_sets(
        bob = parameters(BW = 81), alice = parameters(BW = 66)
    )
    aligned <- update(expanded, parameters = reversed, mode = "replace")
    expect_named(aligned, c("alice", "bob"))
    expect_identical(aligned[[1]]$parameters, parameters(BW = 66))
    expect_identical(aligned[[2]]$parameters, parameters(BW = 81))
})

test_that("collection inputs require compatible cardinality and identifiers", {
    studies <- experiments(first = experiment(), second = experiment())
    expect_error(
        update(studies, parameters = parameter_sets(parameters(x = 1)),
               mode = "replace"),
        "match the output length"
    )
    expect_error(
        update(studies, parameters = parameter_sets(
            a = parameters(x = 1), b = parameters(x = 2)
        ), mode = "replace"),
        "same experiment identifiers"
    )
})

test_that("component-specific modes update dosing and replace observations", {
    schedule <- observation_schedule(1 [h], "C")
    studies <- experiments(
        first = experiment(dosing = dosing(time = 0 [h], amount = 10 [mg]),
                           observations = schedule),
        second = experiment(dosing = dosing(time = 0 [h], amount = 20 [mg]),
                            observations = schedule)
    )
    sampled <- observation_data(
        time = c(2, 2) [h], observable = c("C", "C"),
        value = c(5, 7) [mg/L],
        experiment = factor(c("first", "second"), levels = c("first", "second"))
    )
    out <- update(
        studies,
        dosing = dosing(time = 1 [h], amount = 5 [mg]),
        observations = sampled,
        mode = c(dosing = "update", observations = "replace")
    )
    expect_equal(length(out[[1]]$dosing), 2L)
    expect_equal(length(out[[2]]$dosing), 2L)
    expect_identical(out[[1]]$observations$time, with_units(2 [h]))
    expect_equal(as.numeric(out[[2]]$observations$value), 7)
    expect_false("experiment" %in% names(out[[1]]$observations))
})

test_that("bound observations align with unnamed experiments positionally", {
    studies <- experiments(
        experiment(parameters = parameters(V = 5),
                   observations = observation_schedule(1 [h], "C")),
        experiment(parameters = parameters(V = 6),
                   observations = observation_schedule(1 [h], "C"))
    )
    sampled <- observation_data(
        time = c(1, 1) [h], observable = c("C", "C"),
        value = c(4, 7) [mg/L],
        experiment = factor(c("1", "2"), levels = c("1", "2"))
    )
    out <- update(
        studies,
        parameters = parameters(), observations = sampled,
        mode = "replace"
    )

    expect_null(names(out))
    expect_length(out[[1]]$parameters, 0L)
    expect_equal(as.numeric(out[[1]]$observations$value), 4)
    expect_equal(as.numeric(out[[2]]$observations$value), 7)
})

test_that("starts replace atomically and both modes are equivalent", {
    study <- experiment(
        dosing = dosing(time = 0 [h], amount = 1 [mg]),
        observations = observation_schedule(1 [h], "C"), start = 0 [h]
    )
    replacement <- dosing(time = -1 [h], amount = 1 [mg])
    replaced <- update(study, start = -1 [h], dosing = replacement,
                       mode = "replace")
    updated <- update(study, start = -1 [h], dosing = replacement,
                      mode = c(start = "update", dosing = "replace"))
    expect_identical(updated, replaced)
})

test_that("all object-valued components need named modes", {
    study <- experiment()
    expect_error(
        update(study, parameters = parameters(x = 1), dosing = dosing(),
               mode = c(parameters = "update")),
        "missing.*dosing"
    )
    expect_error(
        update(study, parameters = parameters(x = 1),
               mode = c(unknown = "replace", parameters = "replace")),
        "Unknown mode component"
    )
})
