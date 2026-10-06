test_that("extend_protocol creates one experiment per individual", {
    protocol <- experiment(
        parameters = parameters(route = "oral"),
        dosing = dosing(time = 0 [h], amount = 100 [mg], cmt = "Central"),
        observations = observation_schedule(c(1, 2) [h], "C"),
        start = 0 [h]
    )
    population <- parameter_sets(
        alice = parameters(BW = 65 [kg], sex = "female"),
        bob = parameters(BW = 82 [kg], sex = "male")
    )

    result <- extend_protocol(protocol, population)

    expect_s3_class(result, "Experiments")
    expect_length(result, 2)
    expect_named(result, c("alice", "bob"))
    expect_identical(result[[1]]$parameters, parameters(
        route = "oral", BW = 65 [kg], sex = "female"
    ))
    expect_identical(result[[2]]$parameters, parameters(
        route = "oral", BW = 82 [kg], sex = "male"
    ))
    for (individual in result) {
        expect_s3_class(individual, "Experiment")
        expect_identical(individual$dosing, protocol$dosing)
        expect_identical(individual$observations, protocol$observations)
        expect_identical(individual$start, protocol$start)
    }
    expect_identical(protocol$parameters, parameters(route = "oral"))
    expect_identical(population[[1]], parameters(BW = 65 [kg], sex = "female"))
})

test_that("extend_protocol generates names for unnamed individuals", {
    protocol <- experiment()
    unnamed <- parameter_sets(parameters(id = 1), parameters(id = 2))
    mixed <- parameter_sets(control = parameters(id = 1), parameters(id = 2))

    expect_named(
        extend_protocol(protocol, unnamed),
        c("individual_1", "individual_2")
    )
    expect_named(
        extend_protocol(protocol, mixed),
        c("control", "individual_2")
    )
    expect_error(
        extend_protocol(
            protocol,
            parameter_sets(individual_2 = parameters(id = 1), parameters(id = 2))
        ),
        "conflict|unique"
    )
    expect_identical(extend_protocol(protocol, parameter_sets()), experiments())
})

test_that("extend_protocol rejects overlapping parameter names", {
    protocol <- experiment(parameters = parameters(BW = 70 [kg], route = "oral"))
    population <- parameter_sets(person = parameters(BW = 65 [kg], sex = "female"))

    expect_error(
        extend_protocol(protocol, population),
        "Protocol and individual parameters.*person.*BW"
    )
})

test_that("extend_protocol validates its input classes", {
    expect_error(extend_protocol(parameters(), parameter_sets()), "Experiment")
    expect_error(extend_protocol(experiment(), parameters()), "ParameterSets")
})
