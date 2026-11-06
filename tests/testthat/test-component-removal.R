test_that("overlay deletion is local, ordered, unit-aware, and strict", {
    model <- compartment_model() |> add_parameter(k = 1, retained = 2)
    result <- overlay_parameter(model, k = NULL, retained = 3, Vmax = 5 [mg/h], Km = 2 [mg/L])
    expect_identical(names(result$parameters), c("retained", "Vmax", "Km"))
    expect_equal(result$parameters$Vmax, units::set_units(5, mg/h))
    expect_equal(result$parameters$retained, 3)
    expect_error(overlay_parameter(model, absent = NULL, new = 1), "unknown parameters.*absent")
    expect_identical(names(model$parameters), c("k", "retained"))
    expect_length(overlay_parameter(model, k = NULL, retained = NULL)$parameters, 0)
    expect_equal(overlay_parameter(model, param = parameters(k = 4))$parameters$k, 4)
    expect_equal(overlay_parameter(model, name = "k", value = 4)$parameters$k, 4)
    expect_equal(overlay_parameter(model, new = 2, derived = new * 3)$parameters$derived, 6)
    expect_error(overlay_parameter(model, k = NULL, name = "x"), "Cannot use both")
    expect_error(overlay_parameter(model, k = NULL, k = 3), "unique")

    studies <- c(first = experiment(parameters = parameters(k = 1)),
                 second = experiment(parameters = parameters(other = 2)))
    expect_error(overlay_parameter(studies, k = NULL, new = 3), "Experiment 'second'.*unknown.*k")
    result <- overlay_parameter(studies[1], k = NULL, new = 5 [kg])
    expect_equal(.experiment_entry(result)$parameters, parameters(new = 5 [kg]))
    expect_equal(.experiment_entry(studies)$parameters, parameters(k = 1))
})

test_that("component deletion preserves representations and remains local", {
    model <- compartment_model() |>
        add_compartment(name = c("central", "other"), volume = c(1, 2)) |>
        add_molecule(name = c("A", "B"), cmt = "central", initial = 0) |>
        add_transport(from = "central", to = "other", const = 1) |>
        add_reaction(input = "A", output = "B", cmt = "central", const = 1) |>
        add_equation(x = "k", y = "2*k") |>
        add_observable(A = "a[A,central]", B = "a[B,central]") |>
        add_parameter(k = 1, other = 2) |>
        add_dosing(time = c(0, 1), amount = 1)
    fields <- c("compartments", "molecules", "transports", "reactions", "equations", "observables", "parameters", "doses")
    verbs <- list(remove_compartment, remove_molecule, remove_transport, remove_reaction,
                  remove_equation, remove_observable, remove_parameter, remove_dosing)
    for (j in seq_along(fields)) {
        field <- fields[[j]]
        original <- model[[field]]
        direct <- model
        direct[[field]][1] <- NULL
        piped <- verbs[[j]](model, 1)
        expect_identical(direct, piped)
        expect_identical(piped[[field]], original[seq_len(length(original))[-1]])
        empty <- verbs[[j]](model)
        expect_identical(empty[[field]], original[integer()])
        expect_identical(class(empty[[field]]), class(original))
        expect_error(verbs[[j]](model, length(original) + 1), "range")
        expect_error(verbs[[j]](model, NA_integer_), "NA")
        expect_error(verbs[[j]](model, 0.5), "whole numbers")
    }
    expect_identical(remove_parameter(model, "k")$parameters, parameters(other = 2))
    expect_identical(remove_parameter(model, c(TRUE, FALSE))$parameters, parameters(other = 2))
    expect_identical(remove_parameter(model, -1)$parameters, parameters(k = 1))
    expect_identical(remove_parameter(model, integer()), model)
    expect_identical(remove_parameter(model, NULL), model)
    expect_identical(remove_parameter(model, character()), model)
    expect_error(remove_parameter(model, "missing"), "unknown")
    expect_error(remove_reaction(model, "missing"), "by name")
    expect_error(remove_parameter(model, c(-1, 2)), "mix")
    expect_error(remove_parameter(model, c(TRUE, FALSE, TRUE)), "range")
    expect_identical(remove_parameter(model, "k")$equations, model$equations)
    expect_identical(remove_compartment(model, "central")$transports, model$transports)
    model$reactions[] <- NULL
    expect_length(model$reactions, 0)
})

test_that("experiment removal validates each target and retains collection metadata", {
    studies <- c(a = experiment(parameters = parameters(k = 1, other = 2),
                               dosing = dosing(time = c(0, 1), amount = 1)),
                 b = experiment(parameters = parameters(k = 3), dosing = dosing(time = 0, amount = 2)))
    removed <- remove_parameter(studies, "k")
    expect_identical(names(removed), c("a", "b"))
    expect_equal(.experiment_entry(removed)$parameters, parameters(other = 2))
    expect_length(.experiment_entry(removed, 2)$parameters, 0)
    expect_error(remove_parameter(studies, "other"), "Experiment 'b'.*unknown")
    expect_length(.experiment_entry(remove_dosing(studies, 1), 2)$dosing, 0)
    expect_error(remove_dosing(studies, 2), "Experiment 'b'.*range")
    expect_length(.experiment_entry(remove_parameter(studies))$parameters, 0)
    expect_length(.experiment_entry(remove_dosing(studies))$dosing, 0)
    expect_length(remove_parameter(studies[integer()], "k"), 0)
})


test_that("equation and observable replacement remains available during editing", {
    eq <- equations(x = k, y = 2 * k)
    eq[1] <- list(quote(3 * k))
    expect_identical(eq, equations(x = 3 * k, y = 2 * k))
    obs <- observables(x = a[A], y = a[B])
    obs[1] <- list(quote(2 * a[A]))
    expect_identical(obs, observables(x = 2 * a[A], y = a[B]))
    eq[1] <- NULL
    obs[1] <- NULL
    expect_identical(eq, equations(y = 2 * k))
    expect_identical(obs, observables(y = a[B]))
})

test_that("deleting several reactions preserves the remaining participant structure", {
    first <- reactions(input = c("A", "A"), output = "B", cmt = "central", const = 1)
    second <- reactions(input = "B", output = "A", cmt = "central", const = 2)
    third <- reactions(input = "A", output = "B", cmt = "central", const = 3)
    model <- compartment_model() |> add_reaction(c(first, second, third))
    expected <- third
    model$reactions[1:2] <- NULL
    expect_identical(model$reactions, expected)
})
