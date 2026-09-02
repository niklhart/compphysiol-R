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
    expect_named(process_model$processes, c("rate", "const", "input_states", "input_stoich"))
    expect_true(is.matrix(process_model$stoichiometry))
    expect_equal(dim(process_model$stoichiometry), c(nrow(process_model$states), nrow(process_model$processes)))
    expect_equal(rownames(process_model$stoichiometry), process_model$states$dsl_name)
})

test_that("ProcessModel represents a linear transport by rate and stoichiometry", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12")

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(deparse1(process_model$processes$rate[[1]]), "k12 * y[1]")
    expect_equal(deparse1(process_model$processes$const[[1]]), "k12")
    expect_equal(process_model$processes$input_states[[1]], 1L)
    expect_equal(process_model$processes$input_stoich[[1]], 1)
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
    expect_true(is.na(process_model$processes$const[[1]]))
    expect_equal(process_model$processes$input_states[[1]], 1L)
    expect_equal(process_model$processes$input_stoich[[1]], 1)
    expect_equal(unname(process_model$stoichiometry[, 1]), c(-1, 1))
})

test_that("ProcessModel represents a concentration-state reaction by participant stoichiometry", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = NA_real_) |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "concentration") |>
        add_reaction(input = c("A", "A"), output = "B", cmt = "cyt", const = "kAA")

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(deparse1(process_model$processes$const[[1]]), "kAA")
    expect_equal(process_model$processes$input_states[[1]], 1L)
    expect_equal(process_model$processes$input_stoich[[1]], 2)
    expect_equal(
        unname(process_model$stoichiometry[c("c[A, cyt]", "c[B, cyt]"), 1]),
        c(-2, 1)
    )
})

test_that("ProcessModel amount-state reactions use amount-change process rates", {
    model <- compartment_model() |>
        add_compartment("cyt", volume = "V") |>
        add_molecule(c("A", "B"), cmt = "cyt", initial = c(10, 0), type = "amount") |>
        add_reaction(input = c("A", "A"), output = "B", cmt = "cyt", const = "kAA")

    process_model <- to_process_model(model)

    expect_equal(nrow(process_model$processes), 1)
    expect_equal(deparse1(process_model$processes$rate[[1]]), "kAA * y[1] * (y[1]/V)")
    expect_equal(process_model$processes$input_states[[1]], 1L)
    expect_equal(process_model$processes$input_stoich[[1]], 2)
    expect_equal(
        unname(process_model$stoichiometry[c("a[A, cyt]", "a[B, cyt]"), 1]),
        c(-2, 1)
    )
})

test_that("ProcessModel simplifies elementary cross-compartment amount reaction rates", {
    model <- compartment_model() |>
        add_reaction(
            "L[plasma] + R[membrane] -> LR[membrane]",
            scale_cmt = "membrane",
            const = "kon"
        ) |>
        add_compartment(c("plasma", "membrane"), volume = c("Vp", "Vm")) |>
        add_molecule("L", cmt = "plasma", initial = 10, type = "amount") |>
        add_molecule(c("R", "LR"), cmt = "membrane", initial = c(20, 0), type = "amount")

    process_model <- to_process_model(model)

    expect_equal(deparse1(process_model$processes$rate[[1]]), "kon * (y[1]/Vp) * y[2]")
    expect_equal(process_model$processes$input_states[[1]], c(1L, 2L))
    expect_equal(process_model$processes$input_stoich[[1]], c(1, 1))
    expect_equal(
        unname(process_model$stoichiometry[c("a[L, plasma]", "a[R, membrane]", "a[LR, membrane]"), 1]),
        c(-1, -1, 1)
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
    expect_true(is.na(process_model$processes$const[[1]]))
    expect_equal(process_model$processes$input_states[[1]], 1L)
    expect_equal(process_model$processes$input_stoich[[1]], 1)
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

test_that("ProcessModel print method uses DSL state names", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule(c("drug", "metabolite"), cmt = "Central", initial = c("A0", 0), type = "amount") |>
        add_reaction(input = "drug", output = "metabolite", cmt = "Central", const = "kmet") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(V = 10) |>
        add_dosing(time = 0, amount = 100, cmt = "Central", molec = "drug")

    process_model <- to_process_model(model)

    expect_snapshot(print(process_model))
    expect_false(grepl("y\\[", paste(capture.output(print(process_model)), collapse = "\n")))
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

test_that("OdeModel export from ProcessModel reconstructs transport ODEs", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = c(10, 5)) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", molec = "drug", const = "k12") |>
        add_transport("Peripheral", "Central", molec = "drug", const = "k21") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k12 = 0.2, k21 = 0.1)

    direct <- to_ode_model(model)
    split <- to_ode_model(to_process_model(model))

    expect_equal(split, direct)
})

test_that("OdeModel export from ProcessModel uses process rates and stoichiometry", {
    model <- compartment_model() |>
        add_compartment("Central", volume = 10) |>
        add_molecule(c("drug", "metabolite"), cmt = "Central", initial = c(10, 0), type = "amount") |>
        add_reaction(input = "drug", output = "metabolite", cmt = "Central", const = "kmet")

    ode_model <- to_ode_model(to_process_model(model))

    expect_equal(
        vapply(ode_model$rhs, deparse1, character(1)),
        c("-kmet * y[1]", "kmet * y[1]")
    )
})
