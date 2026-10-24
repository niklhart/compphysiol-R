test_that("parameter verbs modify models and experiments", {
    model <- compartment_model() |> add_parameter(a = 1)
    expect_equal(model |> overlay_parameter(parameters(a = 2, b = 3)) |> getElement("parameters"), parameters(a = 2, b = 3))
    expect_error(model |> add_parameter(parameters(a = 2)), "already exist|unique")
    expect_equal(model |> replace_parameters(parameters(z = 4)) |> getElement("parameters"), parameters(z = 4))

    study <- experiment(parameters = parameters(a = 1))
    expect_equal(.experiment_entry(overlay_parameter(study, parameters(a = 2)))$parameters, parameters(a = 2))
    expect_equal(.experiment_entry(add_parameter(study, \(p) parameters(b = p$a + 1)))$parameters, parameters(a = 1, b = 2))
    expect_equal(.experiment_entry(replace_parameters(study, \(p) parameters()))$parameters, parameters())
})

test_that("parameter sets expand and align atomically", {
    protocol <- experiment(parameters = parameters(shared = 1))
    population <- parameter_sets(a = parameters(id = 1), b = parameters(id = 2))
    expanded <- overlay_parameter(protocol, population)
    expect_equal(length(expanded), 2)
    expect_identical(names(expanded), c("a", "b"))

    reversed <- parameter_sets(b = parameters(id = 20), a = parameters(id = 10))
    aligned <- replace_parameters(expanded, reversed)
    expect_equal(.experiment_entry(aligned, 1)$parameters$id, 10)
    expect_equal(.experiment_entry(aligned, 2)$parameters$id, 20)
    expect_error(replace_parameters(expanded, parameter_sets(x = parameters(id = 1), y = parameters(id = 2))), "identifiers")
})

test_that("dosing and start verbs reconstruct valid experiments", {
    studies <- c(a = experiment(), b = experiment())
    added <- add_dosing(studies, dosing(time = 1, amount = 2))
    expect_equal(length(.experiment_entry(added)$dosing), 1)
    replaced <- replace_dosing(added, \(d) dosing(time = 2, amount = 3))
    expect_equal(.experiment_entry(replaced)$dosing$time, 2)
    expect_error(replace_start(replaced, 3), "start")
    expect_equal(.experiment_entry(replace_start(replaced, -1))$start, -1)

    unit_study <- experiment(start = 0 [h], dosing = dosing(time = 1 [h], amount = 1))
    expected <- units::set_units(-1, h)
    expect_equal(.experiment_entry(replace_start(unit_study, -1 [h]))$start, expected)
    expect_equal(.experiment_entry(replace_start(unit_study, \(s) s - 1 [h]))$start, expected)
})

test_that("observation verbs preserve and validate correspondence", {
    schedule <- observation_schedule(c(1, 1), "C", replicate = c(1, 2))
    studies <- c(a = experiment(observations = schedule), b = experiment(observations = schedule))
    bound <- observation_data(
        time = c(1, 1, 1, 1), observable = "C", value = 1:4,
        replicate = c(1, 2, 1, 2),
        experiment = factor(c("a", "a", "b", "b"), levels = c("a", "b"))
    )
    attached <- attach_observation_values(studies, bound)
    expect_equal(.experiment_entry(attached, 1)$observations$value, 1:2)
    expect_equal(.experiment_entry(attached, 2)$observations$value, 3:4)
    expect_equal(.experiment_entry(attach_observation_values(studies[1], c(5, 6)))$observations$value, c(5, 6))
    expect_error(attach_observation_values(studies[1], observation_data(c(1, 2), "C", 1:2, replicate = 1:2)), "correspond")

    replacement <- replace_observations(studies, bound)
    expect_s3_class(.experiment_entry(replacement)$observations, "compphysiol_observation_data")
    expect_equal(nrow(.experiment_entry(replace_observations(studies, \(x) x[1, ]))$observations), 1)
})
