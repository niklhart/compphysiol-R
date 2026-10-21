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

    expect_s3_class(result, "compphysiol_experiments")
    expect_length(result, 2)
    expect_named(result, c("alice", "bob"))
    expect_identical(result[[1]]$parameters, parameters(
        route = "oral", BW = 65 [kg], sex = "female"
    ))
    expect_identical(result[[2]]$parameters, parameters(
        route = "oral", BW = 82 [kg], sex = "male"
    ))
    for (individual in result) {
        expect_s3_class(individual, "compphysiol_experiment")
        expect_identical(individual$dosing, protocol$dosing)
        expect_identical(individual$observations, protocol$observations)
        expect_identical(individual$start, protocol$start)
    }
    expect_identical(protocol$parameters, parameters(route = "oral"))
    expect_identical(population[[1]], parameters(BW = 65 [kg], sex = "female"))
    homogeneity <- .experiments_component_homogeneity(result)
    expect_true(all(homogeneity[c("start", "parameter_schema", "dosing", "observation_plan", "observations")]))
    expect_false(homogeneity[["parameters"]])
    expect_identical(
        homogeneity,
        .experiments_compute_component_homogeneity(lapply(result, identity))
    )
})

test_that("extend_protocol propagates heterogeneous parameter schemas", {
    population <- parameter_sets(
        first = parameters(BW = 65 [kg]),
        second = parameters(age = 40 [year])
    )

    result <- extend_protocol(experiment(), population)

    expect_false(.experiments_component_homogeneity(result)[["parameter_schema"]])
})

test_that("extend_protocol preserves named and unnamed individuals", {
    protocol <- experiment()
    unnamed <- parameter_sets(parameters(id = 1), parameters(id = 2))
    mixed <- parameter_sets(control = parameters(id = 1), parameters(id = 2))

    expect_null(names(extend_protocol(protocol, unnamed)))
    expect_named(
        extend_protocol(protocol, mixed),
        c("control", "")
    )
    expect_error(
        parameter_sets(`2` = parameters(id = 1)),
        "positive whole numbers.*reserved"
    )
    expect_identical(extend_protocol(protocol, parameter_sets()), experiments())
})

test_that("extend_protocol lets individual parameters override protocol values", {
    protocol <- experiment(parameters = parameters(BW = 70 [kg], route = "oral"))
    population <- parameter_sets(person = parameters(BW = 65 [kg], sex = "female"))

    result <- extend_protocol(protocol, population)
    expect_identical(
        result[["person"]]$parameters,
        parameters(BW = 65 [kg], route = "oral", sex = "female")
    )
})

test_that("extend_protocol validates its input classes", {
    expect_error(extend_protocol(parameters(), parameter_sets()), "Experiment")
    expect_error(extend_protocol(experiment(), parameters()), "Parameter sets")
})
