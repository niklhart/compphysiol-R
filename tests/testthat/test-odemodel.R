test_that("to_ode_model returns a backend-neutral OdeModel", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(V = 10)

    ode_model <- to_ode_model(model)

    expect_s3_class(ode_model, "OdeModel")
    expect_null(attr(ode_model, "source_model"))
    expect_null(attr(ode_model, "auto_placeholder"))
    expect_named(
        ode_model,
        c("states", "initials", "rhs", "equations", "observables", "parameters", "dosing", "freeParams")
    )
    expect_s3_class(ode_model$equations, "Equations")
    expect_s3_class(ode_model$observables, "Observables")
    expect_s3_class(ode_model$parameters, "Parameters")

    expect_s3_class(ode_model$states, "data.frame")
    expect_named(ode_model$states, c("index", "dsl_name", "output_name", "molec", "cmt", "type"), ignore.order = TRUE)
    expect_equal(ode_model$states$index, 1L)
    expect_equal(ode_model$states$dsl_name, "a[drug, Central]")
    expect_equal(ode_model$states$output_name, "a_drug_Central")
    expect_equal(ode_model$states$molec, "drug")
    expect_equal(ode_model$states$cmt, "Central")
    expect_equal(ode_model$states$type, "amount")

    expect_equal(deparse1(ode_model$initials[[1]]), "A0")
    expect_match(deparse1(ode_model$rhs[[1]]), "y\\[1\\]", fixed = FALSE)
    expect_false(grepl("a_drug_Central|y\\[,", deparse1(ode_model$rhs[[1]])))
    expect_equal(unclass(ode_model$parameters)$V, 10)
    expect_equal(ode_model$freeParams, c("A0", "ke"))
})

test_that("OdeModel stores shortened output names without placeholder attributes", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = 0) |>
        add_transport("Central", "Peripheral", const = "k12")

    ode_model <- to_ode_model(model)

    expect_null(attr(ode_model, "auto_placeholder"))
    expect_equal(ode_model$states$dsl_name, c("a[molec, Central]", "a[molec, Peripheral]"))
    expect_equal(ode_model$states$output_name, c("a_Central", "a_Peripheral"))
    expect_equal(to_deSolve(ode_model, parameters = parameters(k12 = 0.1))$stateNames, ode_model$states$output_name)
})

test_that("OdeModel sink terms from constants avoid redundant product parentheses", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", const = "ke")

    ode_model <- to_ode_model(model)

    expect_equal(deparse1(ode_model$rhs[[1]]), "-ke * y[1]")
})

test_that("OdeModel nonlinear sink terms keep expression parentheses", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport("Central", "", rate = "vmax * a[drug, Central] / (Km + a[drug, Central])")

    ode_model <- to_ode_model(model)

    expect_equal(deparse1(ode_model$rhs[[1]]), "-(vmax * y[1]/(Km + y[1]))")
})

test_that("OdeModel print method uses DSL state names", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_equation(C = c[drug, Central]) |>
        add_observable(Cobs = c[drug, Central]) |>
        add_parameter(V = 10) |>
        add_dosing(time = 0, amount = 100, cmt = "Central", molec = "drug")

    ode_model <- to_ode_model(model)

    expect_snapshot(print(ode_model))
    expect_false(grepl("y\\[", paste(capture.output(print(ode_model)), collapse = "\n")))
})

test_that("OdeModel observables lower DSL states to indexed state references", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(V = 10)

    ode_model <- to_ode_model(model)
    obs_expr <- ode_model$observables$C
    obs_text <- deparse1(obs_expr)

    expect_match(obs_text, "y\\[1\\]", fixed = FALSE)
    expect_match(obs_text, "/")
    expect_match(obs_text, "V")
    expect_false(grepl("a_drug_Central|y\\[,", obs_text))
})

test_that("OdeModel keeps literal unit-bearing initial values as values", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount")

    ode_model <- to_ode_model(model)

    expect_s3_class(ode_model$initials[[1]], "units")
    expect_equal(
        ode_model$initials[[1]],
        units::set_units(100, "mg", mode = "standard")
    )
})

test_that("OdeModel equations can contain lowered state references", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = 100, type = "amount") |>
        add_equation(C = c[drug, Central]) |>
        add_transport("Central", "", rate = "CL * C") |>
        add_parameter(V = 10)

    ode_model <- to_ode_model(model)
    eq_text <- deparse1(ode_model$equations$C)
    rhs_text <- deparse1(ode_model$rhs[[1]])

    expect_match(eq_text, "y\\[1\\]", fixed = FALSE)
    expect_match(eq_text, "/")
    expect_match(eq_text, "V")
    expect_match(rhs_text, "C")
    expect_equal(ode_model$freeParams, "CL")
})

test_that("to_ode_model expands infusion depots before creating ODE states", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 0, rate = 10, duration = 2, cmt = "Central", molec = "drug")

    ode_model <- to_ode_model(model)

    expect_true("a[drug, Depot_drug_Central]" %in% ode_model$states$dsl_name)
    expect_true("a[drug, ReleaseRate_drug_Central]" %in% ode_model$states$dsl_name)
    expect_s3_class(ode_model$dosing, "OdeDosing")
    expect_equal(length(ode_model$dosing$state), 3)
    expect_equal(ode_model$freeParams, "ke")
})

test_that("legacy to_ode output matches OdeModel deSolve export", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = c(10, 5)) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12") |>
        add_transport("Peripheral", "Central", const = "k21") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k12 = 0.2, k21 = 0.1)

    legacy <- to_ode(model)
    exported <- to_deSolve(to_ode_model(model))

    expect_equal(exported$stateNames, legacy$stateNames)
    expect_equal(exported$dslStateNames, legacy$dslStateNames)
    expect_equal(exported$y0, legacy$y0)
    expect_equal(exported$freeParams, legacy$freeParams)
    expect_equal(exported$events, legacy$events)
    expect_equal(
        exported$odefun(0, exported$y0, list())[[1]],
        legacy$odefun(0, legacy$y0, list())[[1]]
    )
})
