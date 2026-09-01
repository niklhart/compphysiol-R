test_that("to_process_model returns explicit states and process stoichiometry", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12")

    process_model <- to_process_model(model)

    expect_s3_class(process_model, "ProcessModel")
    required <- c(
        "states", "initials", "processes", "stoichiometry", "equations",
        "observables", "parameters", "dosing", "freeParams"
    )
    expect_true(all(required %in% names(process_model)))

    expect_s3_class(process_model$states, "data.frame")
    expect_true("dsl_name" %in% names(process_model$states))
    expect_equal(process_model$states$dsl_name, c("a[drug, Central]", "a[drug, Peripheral]"))

    expect_s3_class(process_model$processes, "data.frame")
    expect_true(all(c("name", "rate") %in% names(process_model$processes)))
    expect_true(is.matrix(process_model$stoichiometry))
    expect_equal(dim(process_model$stoichiometry), c(nrow(process_model$states), nrow(process_model$processes)))
    expect_equal(rownames(process_model$stoichiometry), process_model$states$dsl_name)
    expect_equal(colnames(process_model$stoichiometry), process_model$processes$name)
})

test_that("ProcessModel represents a linear transport by rate and stoichiometry", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12")

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(deparse1(process_model$processes$rate[[1]]), "k12 * y[1]")
    expect_equal(unname(process_model$stoichiometry[, 1]), c(-1, 1))
})

test_that("ProcessModel represents an explicit-rate transport by rate and stoichiometry", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport(
            "Central",
            "Peripheral",
            rate = "vmax * a[drug, Central] / (Km + a[drug, Central])"
        )

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(deparse1(process_model$processes$rate[[1]]), "vmax * y[1]/(Km + y[1])")
    expect_equal(unname(process_model$stoichiometry[, 1]), c(-1, 1))
})

test_that("ProcessModel represents a reaction by participant stoichiometry", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "concentration") |>
        add_reaction(input = c("A", "A"), output = "B", cmt = "cyt", const = "kAA")

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(
        unname(process_model$stoichiometry[c("c[A, cyt]", "c[B, cyt]"), 1]),
        c(-2, 1)
    )
})

test_that("ProcessModel represents an explicit-rate reaction by rate and stoichiometry", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "concentration") |>
        add_reaction(
            input = "A",
            output = "B",
            cmt = "cyt",
            rate = "vmax * c[A, cyt] / (Km + c[A, cyt])"
        )

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(deparse1(process_model$processes$rate[[1]]), "vmax * y[1]/(Km + y[1])")
    expect_equal(
        unname(process_model$stoichiometry[c("c[A, cyt]", "c[B, cyt]"), 1]),
        c(-1, 1)
    )
})

test_that("ProcessModel lowers concentration observables using amount states and volume scaling", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(V = 10)

    process_model <- to_process_model(model)
    obs_text <- deparse1(process_model$observables$C)

    expect_match(obs_text, "y\\[1\\]", fixed = FALSE)
    expect_match(obs_text, "/")
    expect_match(obs_text, "V")
    expect_false(grepl("a_drug_Central|y\\[,", obs_text))
})

test_that("ProcessModel expands infusion depots and flattens dosing", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 0, rate = 10, duration = 2, cmt = "Central", molec = "drug")

    process_model <- to_process_model(model)

    expect_true("a[drug, Depot_drug_Central]" %in% process_model$states$dsl_name)
    expect_true("a[drug, ReleaseRate_drug_Central]" %in% process_model$states$dsl_name)
    expect_s3_class(process_model$dosing, "data.frame")
    expect_named(process_model$dosing, c("state", "time", "value", "operation"), ignore.order = TRUE)
    expect_equal(length(process_model$dosing$state), 3)
    expect_equal(process_model$freeParams, "ke")
})

test_that("ProcessModel tracks free parameters before ODE rhs accumulation", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_equation(C = c[drug, Central]) |>
        add_transport("Central", "", rate = "CL * C") |>
        add_observable(Cobs = c[drug, Central] / F) |>
        add_parameter(V = 10)

    process_model <- to_process_model(model)

    expect_equal(process_model$freeParams, c("A0", "CL", "F"))
})

test_that("OdeModel export from ProcessModel matches direct CompartmentModel lowering", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = c(10, 5)) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12") |>
        add_transport("Peripheral", "Central", const = "k21") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k12 = 0.2, k21 = 0.1)

    direct <- to_ode_model(model)
    split <- to_ode_model(to_process_model(model))

    expect_equal(split, direct)
})
