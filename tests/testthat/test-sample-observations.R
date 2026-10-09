sample_observation_predictions <- function() {
    observation_data(
        time = c(2, 1, 2) [h],
        observable = c("C", "C", "C"),
        value = c(5, 6, 5) [mg/L],
        replicate = c("a", "b", "c")
    )
}

test_that("sample_observations preserves prediction rows and is reproducible", {
    predictions <- sample_observation_predictions()
    statistics <- statistical_model(
        C = normal(sd = combined("sigma_add", "sigma_prop")),
        CL = lognormal(median = "CL_pop", sdlog = "omega_CL")
    )
    values <- parameters(sigma_add = 0.1 [mg/L], sigma_prop = 0.2)

    set.seed(123)
    first <- sample_observations(predictions, statistics, values)
    set.seed(123)
    second <- sample_observations(predictions, statistics, values)

    expect_s3_class(first, "ObservationData")
    expect_equal(first, second)
    expect_identical(first$time, predictions$time)
    expect_identical(first$observable, predictions$observable)
    expect_identical(first$replicate, predictions$replicate)
    expect_identical(names(first), names(predictions))
    expect_s3_class(first$value, "units")
    expect_false(isTRUE(all.equal(first$value, predictions$value)))
})

test_that("SimulationResult predictions require explicit extraction", {
    predictions <- sample_observation_predictions()
    result <- structure(
        list(states = data.frame(time = 0), observables = predictions),
        class = "SimulationResult"
    )

    statistics <- statistical_model(C = normal(sd = 0 [mg/L]))
    expect_error(
        sample_observations(result, statistics, parameters()),
        "ObservationData.*as_observables_long"
    )

    sampled <- result |>
        as_observables_long() |>
        sample_observations(statistics, parameters())
    expect_identical(sampled, predictions)
})

test_that("explicit observation locations are rejected", {
    predictions <- sample_observation_predictions()
    statistics <- statistical_model(
        C = normal(mean = "assay_mean", sd = "assay_sd")
    )

    expect_error(
        sample_observations(
            predictions,
            statistics,
            parameters(assay_mean = 7 [mg/L], assay_sd = 0 [mg/L])
        ),
        "conditional prediction as location.*C"
    )
})

test_that("sample_observations supports log-normal distributions", {
    predictions <- observation_data(
        time = c(1, 2) [h], observable = "C", value = c(2, 4) [mg/L]
    )
    statistics <- statistical_model(C = lognormal(sdlog = "sigma"))

    set.seed(9)
    sampled <- sample_observations(predictions, statistics, parameters(sigma = 0))

    expect_equal(sampled$value, predictions$value)
    expect_true(all(as.numeric(sampled$value) > 0))
})

test_that("sample_observations preserves mixed observable units", {
    predictions <- observation_data(
        time = c(1, 1) [h],
        observable = c("A", "C"),
        value = units::mixed_units(c(10, 2), c("mg", "mg/L"))
    )
    statistics <- statistical_model(
        A = normal(sd = 0 [mg]),
        C = normal(sd = 0 [mg/L])
    )

    sampled <- sample_observations(predictions, statistics, parameters())

    expect_s3_class(sampled$value, "mixed_units")
    expect_equal(sampled$value[[1]], predictions$value[[1]])
    expect_equal(sampled$value[[2]], predictions$value[[2]])
})

test_that("prediction observables define strict internal sampling targets", {
    predictions <- sample_observation_predictions()
    expect_error(
        sample_observations(
            predictions,
            statistical_model(A = normal(sd = 1 [mg/L])),
            parameters()
        ),
        "missing observation-level distributions.*C"
    )
    expect_error(
        sample_observations(
            predictions,
            statistical_model(C = normal(mean = 5 [mg/L], sd = 1 [mg/L],
                                           level = "individual")),
            parameters()
        ),
        "explicitly individual-level.*C"
    )
})

test_that("sample_observations validates distributions and inputs", {
    predictions <- sample_observation_predictions()
    expect_error(
        sample_observations(predictions, statistical_model(C = normal(sd = "sigma")),
                            parameters()),
        "Missing statistical parameter"
    )
    expect_error(
        sample_observations(predictions, statistical_model(C = normal(sd = 1 [kg])),
                            parameters()),
        "unit|units"
    )
    expect_error(
        sample_observations(
            predictions,
            statistical_model(C = normal(sd = proportional("sigma"))),
            parameters(sigma = 0.1 [kg])
        ),
        "dimensionless"
    )
    expect_error(
        sample_observations(predictions, statistical_model(C = normal(sd = -1 [mg/L])),
                            parameters()),
        "non-negative"
    )
    nonpositive <- predictions
    nonpositive$value[[1]] <- 0 * nonpositive$value[[1]]
    expect_error(
        sample_observations(
            nonpositive,
            statistical_model(C = lognormal(sdlog = 0.1)),
            parameters()
        ),
        "medians must be positive"
    )
    expect_error(
        sample_observations(list(predictions), statistical_model(), parameters()),
        "ObservationData"
    )
    missing_prediction <- predictions
    missing_prediction$value[[1]] <- NA_real_ * missing_prediction$value[[1]]
    expect_error(
        sample_observations(
            missing_prediction,
            statistical_model(C = normal(sd = 1 [mg/L])),
            parameters()
        ),
        "finite numeric predictions"
    )
})

test_that("sample_observations handles empty prediction data", {
    predictions <- observation_data()
    sampled <- sample_observations(predictions, statistical_model(), parameters())

    expect_identical(sampled, predictions)
})
