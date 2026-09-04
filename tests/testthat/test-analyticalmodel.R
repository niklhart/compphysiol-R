eval_expr_matrix <- function(x, env = list()) {
    stopifnot(is.matrix(x))
    matrix(
        vapply(seq_along(x), function(i) {
            expr <- x[[i]]
            if (is.language(expr)) eval(expr, envir = env) else expr
        }, numeric(1)),
        nrow = nrow(x),
        dimnames = dimnames(x)
    )
}

eval_expr_vector <- function(x, env = list()) {
    unname(vapply(x, function(expr) {
        if (is.language(expr)) eval(expr, envir = env) else expr
    }, numeric(1)))
}

test_that("to_analytical_model returns a backend-neutral AnalyticalModel", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke")

    analytical_model <- to_analytical_model(model)

    expect_s3_class(analytical_model, "AnalyticalModel")
    expect_named(
        analytical_model,
        c("states", "initials", "A", "b", "equations", "observables", "parameters", "freeParams")
    )
    expect_s3_class(analytical_model$states, "data.frame")
    expect_named(analytical_model$states, c("index", "dsl_name", "output_name", "type"), ignore.order = TRUE)
    expect_equal(analytical_model$states$dsl_name, "a[drug, Central]")
    expect_equal(analytical_model$states$output_name, "a_drug_Central")
    expect_equal(deparse1(analytical_model$initials[[1]]), "A0")

    expect_true(is.matrix(analytical_model$A))
    expect_equal(typeof(analytical_model$A), "list")
    expect_equal(dim(analytical_model$A), c(1L, 1L))
    expect_equal(rownames(analytical_model$A), analytical_model$states$dsl_name)
    expect_equal(colnames(analytical_model$A), analytical_model$states$dsl_name)
    expect_false(is.character(analytical_model$A[[1, 1]]))
    expect_equal(deparse1(analytical_model$A[[1, 1]]), "-ke")

    expect_length(analytical_model$b, 1)
    expect_equal(names(analytical_model$b), analytical_model$states$dsl_name)
    expect_equal(eval_expr_vector(analytical_model$b), 0)
    expect_equal(analytical_model$freeParams, c("A0", "ke"))
})

test_that("to_analytical_model accepts a ProcessModel", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "Peripheral", const = "k12") |>
        add_transport("Peripheral", "Central", const = "k21")
    process_model <- to_process_model(model)

    from_compartment_model <- to_analytical_model(model)
    from_process_model <- to_analytical_model(process_model)

    expect_equal(from_process_model, from_compartment_model)
})

test_that("AnalyticalModel represents linear transport systems as A x plus zero b", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport("Central", "", const = "k10") |>
        add_transport("Central", "Peripheral", const = "k12") |>
        add_transport("Peripheral", "Central", const = "k21")

    analytical_model <- to_analytical_model(model)
    values <- list(k10 = 1, k12 = 2, k21 = 3)

    expect_equal(
        eval_expr_matrix(analytical_model$A, values),
        matrix(
            c(-3, 2, 3, -3),
            nrow = 2,
            dimnames = list(analytical_model$states$dsl_name, analytical_model$states$dsl_name)
        )
    )
    expect_equal(eval_expr_vector(analytical_model$b, values), c(0, 0))
})

test_that("AnalyticalModel keeps equations and observables in lowered state form", {
    model <- compartment_model() |>
        add_compartment("Central", volume = "V") |>
        add_molecule("drug", cmt = "Central", initial = "A0", type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_equation(ke = base_ke * scale) |>
        add_observable(C = c[drug, Central] / F) |>
        add_parameter(V = 10)

    analytical_model <- to_analytical_model(model)

    expect_s3_class(analytical_model$equations, "Equations")
    expect_s3_class(analytical_model$observables, "Observables")
    expect_equal(deparse1(analytical_model$equations$ke), "base_ke * scale")
    expect_match(deparse1(analytical_model$observables$C), "y\\[1\\]", fixed = FALSE)
    expect_match(deparse1(analytical_model$observables$C), "/")
    expect_match(deparse1(analytical_model$observables$C), "V")
    expect_false(grepl("a_drug_Central|y\\[,", deparse1(analytical_model$observables$C)))
    expect_equal(analytical_model$freeParams, c("A0", "F", "base_ke", "scale"))
})

test_that("to_analytical_model rejects nonzero source terms in V1", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("", "Central", rate = "kin")

    expect_error(
        to_analytical_model(model),
        "source|b|constant",
        ignore.case = TRUE
    )
})

test_that("to_analytical_model rejects explicit nonlinear process rates", {
    model <- compartment_model() |>
        add_compartment(c("Central", "Peripheral"), volume = NA_real_) |>
        add_molecule("drug", cmt = c("Central", "Peripheral"), initial = c(10, 0), type = "amount") |>
        add_transport(
            "Central",
            "Peripheral",
            rate = "vmax * a[drug, Central] / (Km + a[drug, Central])"
        )

    expect_error(
        to_analytical_model(model),
        "linear|analytical",
        ignore.case = TRUE
    )
})

test_that("to_analytical_model rejects dosing in V1", {
    model <- compartment_model() |>
        add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", cmt = "Central", initial = 0, type = "amount") |>
        add_transport("Central", "", const = "ke") |>
        add_dosing(time = 0, amount = 10, cmt = "Central", molec = "drug")

    expect_error(
        to_analytical_model(model),
        "dosing|not implemented",
        ignore.case = TRUE
    )
})
