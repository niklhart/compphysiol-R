test_that("to_stochastic_model returns a StochasticModel", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12")

    stochastic_model <- to_stochastic_model(model)

    expect_s3_class(stochastic_model, "StochasticModel")
    expect_named(
        stochastic_model,
        c("states", "initials", "stoichiometry", "propensities", "processes",
          "equations", "observables", "parameters", "freeParams")
    )
    expect_s3_class(stochastic_model$states, "data.frame")
    expect_equal(stochastic_model$states$dsl_name, c("a[drug, Central]", "a[drug, Peripheral]"))
    expect_true(is.matrix(stochastic_model$stoichiometry))
    expect_true(all(stochastic_model$stoichiometry == round(stochastic_model$stoichiometry)))
    expect_equal(dim(stochastic_model$stoichiometry), c(nrow(stochastic_model$states), length(stochastic_model$propensities)))
    expect_match(deparse1(stochastic_model$propensities[[1]]), "y\\[1\\]", fixed = FALSE)
    expect_false(grepl("a_drug_Central|y\\[,", deparse1(stochastic_model$propensities[[1]])))
})

test_that("to_stochastic_model accepts a ProcessModel", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12")
    process_model <- to_process_model(model)

    stochastic_model <- to_stochastic_model(process_model)

    expect_s3_class(stochastic_model, "StochasticModel")
    expect_equal(stochastic_model$states, process_model$states)
    expect_equal(stochastic_model$stoichiometry, process_model$stoichiometry)
})

test_that("to_stochastic_model derives first-order transport propensities", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12")

    stochastic_model <- to_stochastic_model(model)

    expect_equal(deparse1(stochastic_model$propensities[[1]]), "k12 * y[1]")
    expect_equal(unname(stochastic_model$stoichiometry[, 1]), c(-1L, 1L))
})

test_that("to_stochastic_model derives elementary reaction propensities", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = "V") |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(input = "A", output = "B", cmt = "cyt", const = "k") |>
        add_parameter(V = 1)

    stochastic_model <- to_stochastic_model(model)

    expect_equal(deparse1(stochastic_model$propensities[[1]]), "k * y[1]")
    expect_equal(unname(stochastic_model$stoichiometry[, 1]), c(-1L, 1L))
})

test_that("to_stochastic_model keeps repeated-input metadata for propensity correction", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = "V") |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(input = c("A", "A"), output = "B", cmt = "cyt", const = "k") |>
        add_parameter(V = 1)

    stochastic_model <- to_stochastic_model(model)

    expect_equal(stochastic_model$processes$input_states[[1]], 1L)
    expect_equal(stochastic_model$processes$input_stoich[[1]], 2)
    expect_equal(unname(stochastic_model$stoichiometry[, 1]), c(-2L, 1L))
})

test_that("to_stochastic_model allows inverse-time units in process parameters", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 10, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_parameter(ke = 0.2 [1/h])

    expect_no_error(to_stochastic_model(model))
})

test_that("stochastic propensity functions compile equations before propensities", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 10, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_equation(ke = base_ke * scale) |>
        add_parameter(base_ke = 0.2, scale = 2)

    stochastic_model <- to_stochastic_model(model)
    propfun <- .stochastic_model_propensity_function(
        stochastic_model,
        parameters = stochastic_model$parameters,
        dimensions = list()
    )
    body_text <- paste(deparse(body(propfun)), collapse = "\n")

    expect_lt(regexpr("ke <-", body_text)[[1]], regexpr("prop <-", body_text)[[1]])
    expect_equal(propfun(10, list()), 4)
})

test_that("to_stochastic_model rejects dosing in the current version", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 10, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 0, amount = 10, cmt = "Central", molec = "drug")

    expect_error(to_stochastic_model(model), "dosing|not implemented", ignore.case = TRUE)
})

test_that("to_stochastic_model rejects concentration states", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule("A", cmt = "cyt", initial = 10, type = "concentration")

    expect_error(to_stochastic_model(model), "concentration|amount|count", ignore.case = TRUE)
})

test_that("to_stochastic_model rejects unit-bearing state initials", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 10 [mg], type = "amount")

    expect_error(to_stochastic_model(model), "dimensionless|count|unit", ignore.case = TRUE)
})

test_that("to_stochastic_model rejects non-count state initials", {
    fractional <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 1.5, type = "amount")
    negative <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = -1, type = "amount")

    expect_error(to_stochastic_model(fractional), "integer|count", ignore.case = TRUE)
    expect_error(to_stochastic_model(negative), "non-negative|count", ignore.case = TRUE)
})

test_that("to_stochastic_model rejects explicit-rate processes in the current version", {
    transport <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport(
            "Central",
            "Peripheral",
            rate = "vmax * a[drug, Central] / (Km + a[drug, Central])"
    )
    reaction <- compartment_model() |>
        add_compartment("cyt", volume = "V") |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(
            input = "A",
            output = "B",
            cmt = "cyt",
            rate = "vmax * a[A, cyt] / (Km + a[A, cyt])"
        ) |>
        add_parameter(V = 1)

    expect_error(to_stochastic_model(transport), "explicit.*rate|rate.*not implemented", ignore.case = TRUE)
    expect_error(to_stochastic_model(reaction), "explicit.*rate|rate.*not implemented", ignore.case = TRUE)
})
