#' Create a process model representation
#'
#' `to_process_model()` lowers a `CompartmentModel` to a backend-neutral process
#' representation with explicit states, process rates, and process
#' stoichiometry.
#'
#' @param model A `CompartmentModel` object.
#' @returns A `ProcessModel` object.
#' @export
to_process_model <- function(model) {
    UseMethod("to_process_model")
}

#' @export
to_process_model.CompartmentModel <- function(model) {
    model <- model |> wire() |> make_depot()
    .ode_model_check_transport_compartments(model)

    y0_dsl <- initials(model)
    dsl_state_names <- names(y0_dsl)
    state_info <- .ode_model_state_info(model, dsl_state_names)
    name2idx <- setNames(state_info$index, state_info$dsl_name)
    state_volumes <- .ode_model_state_volumes(model)
    eq_names <- names(model$equations)

    lower <- function(expr) {
        .ode_model_lower_expr(
            expr,
            state_names = dsl_state_names,
            eq_names = eq_names,
            name2idx = name2idx,
            state_volumes = state_volumes
        )
    }

    process_list <- list()

    add_process <- function(rate, const, input_states, input_stoich, stoich) {
        list(
            rate = rate,
            const = const,
            input_states = input_states,
            input_stoich = input_stoich,
            stoich = stoich
        )
    }

    for (j in seq_along(model$transports)) {
        from <- model$transports$from[[j]]
        to <- model$transports$to[[j]]
        molec <- model$transports$molec[[j]]
        from_idx <- if (!is.na(from)) .ode_model_transport_state_idx(molec, from, name2idx) else NULL
        to_idx <- if (!is.na(to)) .ode_model_transport_state_idx(molec, to, name2idx) else NULL
        rate <- lower(model$transports$rate[[j]])
        const <- if (is.null(model$transports$const[[j]])) NA else lower(model$transports$const[[j]])
        input_states <- if (!is.na(from)) from_idx else integer(0)
        input_stoich <- if (!is.na(from)) 1 else numeric(0)

        stoich <- numeric(length(dsl_state_names))
        names(stoich) <- dsl_state_names
        if (!is.na(from)) stoich[[from_idx]] <- stoich[[from_idx]] - 1
        if (!is.na(to)) stoich[[to_idx]] <- stoich[[to_idx]] + 1
        process_list <- c(process_list, list(
            add_process(rate, const, input_states, input_stoich, stoich)
        ))
    }

    volume_by_cmt <- setNames(model$compartments$volume, names(model$compartments))
    for (j in seq_along(model$reactions)) {
        scale_cmt <- model$reactions$scale_cmt[[j]]
        vol <- volume_by_cmt[[scale_cmt]]
        rate_expr <- model$reactions$rate[[j]]
        participants <- as.data.frame(model$reactions$participants[[j]])

        stoich <- numeric(length(dsl_state_names))
        names(stoich) <- dsl_state_names
        targets <- lapply(seq_len(nrow(participants)), function(i) {
            .ode_model_reaction_state(
                participants$molec[[i]],
                participants$cmt[[i]],
                name2idx
            )
        })
        target_types <- unique(vapply(targets, `[[`, character(1), "type"))
        if (length(target_types) > 1L) {
            stop(
                "Cannot create a process model for a reaction with mixed amount and concentration states.",
                call. = FALSE
            )
        }

        for (i in seq_len(nrow(participants))) {
            target <- targets[[i]]
            multiplier <- if (identical(participants$role[[i]], "input")) -1 else 1
            stoich[[target$idx]] <- stoich[[target$idx]] + multiplier * participants$stoich[[i]]
        }

        input_participants <- participants[participants$role == "input", , drop = FALSE]
        input_targets <- targets[participants$role == "input"]
        input_states <- vapply(input_targets, `[[`, integer(1), "idx")
        input_stoich <- input_participants$stoich

        if (identical(target_types, "amount")) {
            if (.is_missing_volume(vol)) {
                stop(
                    "Cannot export reaction in compartment '",
                    scale_cmt,
                    "' to amount-state ODEs: reaction rates are ",
                    "concentration-change rates and require a compartment ",
                    "volume to convert them to amount/time.",
                    call. = FALSE
                )
            }
            process_rate <- .process_model_amount_reaction_rate(
                rate_expr = rate_expr,
                type = model$reactions$type[[j]],
                scale_cmt = scale_cmt,
                volume = vol,
                participants = participants,
                state_names = dsl_state_names,
                eq_names = eq_names,
                name2idx = name2idx,
                state_volumes = state_volumes
            )
        } else {
            process_rate <- lower(rate_expr)
        }
        const <- if (is.null(model$reactions$const[[j]])) NA else lower(model$reactions$const[[j]])

        process_list <- c(process_list, list(
            add_process(process_rate, const, input_states, input_stoich, stoich)
        ))
    }

    lowered_equations <- lapply(model$equations, lower) |> structure(class = "Equations")
    lowered_observables <- lapply(model$observables, lower) |> structure(class = "Observables")
    dosing <- .ode_model_dosing(model, name2idx)
    process_rates <- lapply(process_list, `[[`, "rate")
    process_consts <- lapply(process_list, `[[`, "const")
    process_input_states <- lapply(process_list, `[[`, "input_states")
    process_input_stoich <- lapply(process_list, `[[`, "input_stoich")
    stoich_cols <- lapply(process_list, `[[`, "stoich")
    stoichiometry <- .process_model_stoichiometry(stoich_cols, dsl_state_names)
    processes <- structure(
        data.frame(
            rate = I(process_rates),
            const = I(process_consts),
            input_states = I(process_input_states),
            input_stoich = I(process_input_stoich),
            stringsAsFactors = FALSE
        ),
        class = "data.frame"
    )

    structure(
        list(
            states = state_info,
            initials = unname(lapply(y0_dsl, lower)),
            processes = processes,
            stoichiometry = stoichiometry,
            equations = lowered_equations,
            observables = lowered_observables,
            parameters = model$parameters,
            dosing = dosing,
            freeParams = .ode_model_free_params(
                c(unname(lapply(y0_dsl, lower)), process_rates,
                  unclass(lowered_equations), unclass(lowered_observables),
                  unname(as.list(dosing$time)), unname(as.list(dosing$value))),
                eq_names = names(lowered_equations),
                param_names = names(model$parameters)
            )
        ),
        class = "ProcessModel"
    )
}

#' Create an ODE model representation
#'
#' `to_ode_model()` lowers a `CompartmentModel` or `ProcessModel` to a
#' backend-neutral ODE representation with indexed one-dimensional states.
#'
#' @param model A `CompartmentModel` or `ProcessModel` object.
#' @returns An `OdeModel` object.
#' @export
to_ode_model <- function(model) {
    UseMethod("to_ode_model")
}

#' @export
to_ode_model.CompartmentModel <- function(model) {
    model |> to_process_model() |> to_ode_model()
}

#' @export
to_ode_model.ProcessModel <- function(model) {
    rhs <- lapply(seq_len(nrow(model$stoichiometry)), function(i) {
        terms <- lapply(seq_len(ncol(model$stoichiometry)), function(j) {
            coeff <- model$stoichiometry[[i, j]]
            if (identical(coeff, 0) || identical(coeff, 0L)) return(NULL)
            .process_model_ode_term(
                coeff = coeff,
                rate = model$processes$rate[[j]],
                const = model$processes$const[[j]]
            )
        })
        .sum_exprs(Filter(Negate(is.null), terms))
    })
    structure(
        list(
            states = model$states,
            initials = model$initials,
            rhs = rhs,
            equations = model$equations,
            observables = model$observables,
            parameters = model$parameters,
            dosing = model$dosing,
            freeParams = .ode_model_free_params(
                c(model$initials, rhs, unclass(model$equations), unclass(model$observables),
                  unname(as.list(model$dosing$time)), unname(as.list(model$dosing$value))),
                eq_names = names(model$equations),
                param_names = names(model$parameters)
            )
        ),
        class = "OdeModel"
    )
}

#' Create an analytical model representation
#'
#' `to_analytical_model()` lowers a `CompartmentModel` or `ProcessModel` to a
#' backend-neutral linear analytical representation of the form `dx/dt = A x + b`.
#'
#' The first implementation supports homogeneous first-order linear systems.
#' The constant source term `b` is part of the representation, but nonzero
#' entries are rejected for now.
#'
#' @param model A `CompartmentModel` or `ProcessModel` object.
#' @returns An `AnalyticalModel` object.
#' @export
to_analytical_model <- function(model) {
    UseMethod("to_analytical_model")
}

#' @export
to_analytical_model.CompartmentModel <- function(model) {
    model |> to_process_model() |> to_analytical_model()
}

#' @export
to_analytical_model.ProcessModel <- function(model) {
    .analytical_model_check_compatibility(model)

    system <- .analytical_model_linear_system(model)
    .analytical_model_check_zero_b(system$b)

    structure(
        list(
            states = model$states,
            initials = model$initials,
            A = system$A,
            b = system$b,
            equations = model$equations,
            observables = model$observables,
            parameters = model$parameters,
            freeParams = .ode_model_free_params(
                c(model$initials, as.list(system$A), as.list(system$b),
                  unclass(model$equations), unclass(model$observables)),
                eq_names = names(model$equations),
                param_names = names(model$parameters)
            )
        ),
        class = "AnalyticalModel"
    )
}

#' Create a stochastic model representation
#'
#' `to_stochastic_model()` lowers a `CompartmentModel` or `ProcessModel` to a
#' count-based representation suitable for Gillespie SSA simulation.
#'
#' The first implementation supports models whose processes can be rendered as
#' elementary propensities from their `const` and input-state metadata. Explicit
#' `rate = ...` processes and dosing are rejected.
#'
#' @param model A `CompartmentModel` or `ProcessModel` object.
#' @returns A `StochasticModel` object.
#' @export
to_stochastic_model <- function(model) {
    UseMethod("to_stochastic_model")
}

#' @export
to_stochastic_model.CompartmentModel <- function(model) {
    model |> to_process_model() |> to_stochastic_model()
}

#' @export
to_stochastic_model.ProcessModel <- function(model) {
    .stochastic_model_check_compatibility(model)

    propensities <- lapply(seq_len(nrow(model$processes)), function(i) {
        .stochastic_model_propensity(
            const = model$processes$const[[i]],
            input_states = model$processes$input_states[[i]],
            input_stoich = model$processes$input_stoich[[i]]
        )
    })

    structure(
        list(
            states = model$states,
            initials = model$initials,
            stoichiometry = model$stoichiometry,
            propensities = propensities,
            processes = model$processes,
            equations = model$equations,
            observables = model$observables,
            parameters = model$parameters,
            freeParams = .ode_model_free_params(
                c(model$initials, propensities, unclass(model$equations), unclass(model$observables)),
                eq_names = names(model$equations),
                param_names = names(model$parameters)
            )
        ),
        class = "StochasticModel"
    )
}

#' Print method for `ProcessModel` class
#'
#' Pretty-prints a `ProcessModel` object using DSL state names.
#' @param x A `ProcessModel` object.
#' @param ... ignored
#' @returns The `ProcessModel` object (invisibly).
#' @export
print.ProcessModel <- function(x, ...) {
    cat("ProcessModel:\n")

    if (nrow(x$states) > 0) {
        cat(" States:\n")
        cat(sprintf(
            "  (%s) %s, initial = %s\n",
            x$states$index,
            x$states$dsl_name,
            vapply(x$initials, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" States: (none)\n")
    }

    if (nrow(x$processes) > 0) {
        cat(" Process rates:\n")
        cat(sprintf(
            "  (%s) %s\n",
            seq_len(nrow(x$processes)),
            vapply(x$processes$rate, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
        cat(" Stoichiometry:\n")
        print(.process_model_display_stoichiometry(x$stoichiometry), quote = FALSE)
    } else {
        cat(" Process rates: (none)\n")
        cat(" Stoichiometry: (none)\n")
    }

    if (length(x$equations) > 0) {
        cat(" Equations:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$equations),
            names(x$equations),
            vapply(x$equations, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Equations: (none)\n")
    }

    if (length(x$observables) > 0) {
        cat(" Observables:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$observables),
            names(x$observables),
            vapply(x$observables, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Observables: (none)\n")
    }

    if (nrow(x$dosing) > 0) {
        cat(" Dosing:\n")
        cat(sprintf(
            "  (%s) %s %s to %s at %s\n",
            seq_len(nrow(x$dosing)),
            x$dosing$operation,
            vapply(x$dosing$value, .ode_model_format_expr, character(1), model = x),
            x$states$dsl_name[x$dosing$state],
            vapply(x$dosing$time, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Dosing: (none)\n")
    }

    print(x$parameters)

    if (length(x$freeParams) > 0) {
        cat(" Free parameters: ", paste(x$freeParams, collapse = ", "), "\n", sep = "")
    } else {
        cat(" Free parameters: (none)\n")
    }

    invisible(x)
}

#' Print method for `StochasticModel` class
#'
#' Pretty-prints a `StochasticModel` object using DSL state names.
#' @param x A `StochasticModel` object.
#' @param ... ignored
#' @returns The `StochasticModel` object (invisibly).
#' @export
print.StochasticModel <- function(x, ...) {
    cat("StochasticModel:\n")

    if (nrow(x$states) > 0) {
        cat(" States:\n")
        cat(sprintf(
            "  (%s) %s, initial = %s\n",
            x$states$index,
            x$states$dsl_name,
            vapply(x$initials, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" States: (none)\n")
    }

    if (length(x$propensities) > 0) {
        cat(" Propensities:\n")
        cat(sprintf(
            "  (%s) %s\n",
            seq_along(x$propensities),
            vapply(x$propensities, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
        cat(" Stoichiometry:\n")
        print(.process_model_display_stoichiometry(x$stoichiometry), quote = FALSE)
    } else {
        cat(" Propensities: (none)\n")
        cat(" Stoichiometry: (none)\n")
    }

    if (length(x$equations) > 0) {
        cat(" Equations:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$equations),
            names(x$equations),
            vapply(x$equations, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Equations: (none)\n")
    }

    if (length(x$observables) > 0) {
        cat(" Observables:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$observables),
            names(x$observables),
            vapply(x$observables, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Observables: (none)\n")
    }

    print(x$parameters)

    if (length(x$freeParams) > 0) {
        cat(" Free parameters: ", paste(x$freeParams, collapse = ", "), "\n", sep = "")
    } else {
        cat(" Free parameters: (none)\n")
    }

    invisible(x)
}

#' Print method for `AnalyticalModel` class
#'
#' Pretty-prints an `AnalyticalModel` object using state indices in the linear
#' system and DSL state names in the state list.
#' @param x An `AnalyticalModel` object.
#' @param ... ignored
#' @returns The `AnalyticalModel` object (invisibly).
#' @export
print.AnalyticalModel <- function(x, ...) {
    cat("AnalyticalModel:\n")

    if (nrow(x$states) > 0) {
        cat(" States:\n")
        cat(sprintf(
            "  (%s) %s, initial = %s\n",
            x$states$index,
            x$states$dsl_name,
            vapply(x$initials, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" States: (none)\n")
    }

    if (length(x$A) > 0) {
        cat(" A:\n")
        print(.analytical_model_display_A(x), quote = FALSE)
    } else {
        cat(" A: (none)\n")
    }

    if (length(x$b) > 0) {
        cat(" b:\n")
        cat(sprintf(
            "  (%s) %s\n",
            seq_along(x$b),
            vapply(x$b, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" b: (none)\n")
    }

    if (length(x$equations) > 0) {
        cat(" Equations:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$equations),
            names(x$equations),
            vapply(x$equations, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Equations: (none)\n")
    }

    if (length(x$observables) > 0) {
        cat(" Observables:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$observables),
            names(x$observables),
            vapply(x$observables, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Observables: (none)\n")
    }

    print(x$parameters)

    if (length(x$freeParams) > 0) {
        cat(" Free parameters: ", paste(x$freeParams, collapse = ", "), "\n", sep = "")
    } else {
        cat(" Free parameters: (none)\n")
    }

    invisible(x)
}

#' Print method for `OdeModel` class
#'
#' Pretty-prints an `OdeModel` object using DSL state names.
#' @param x An `OdeModel` object.
#' @param ... ignored
#' @returns The `OdeModel` object (invisibly).
#' @export
print.OdeModel <- function(x, ...) {
    cat("OdeModel:\n")

    if (nrow(x$states) > 0) {
        cat(" States:\n")
        cat(sprintf(
            "  (%s) %s, initial = %s\n",
            x$states$index,
            x$states$dsl_name,
            vapply(x$initials, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" States: (none)\n")
    }

    if (length(x$rhs) > 0) {
        cat(" ODEs:\n")
        cat(sprintf(
            "  d/dt %s = %s\n",
            x$states$dsl_name,
            vapply(x$rhs, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" ODEs: (none)\n")
    }

    if (length(x$equations) > 0) {
        cat(" Equations:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$equations),
            names(x$equations),
            vapply(x$equations, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Equations: (none)\n")
    }

    if (length(x$observables) > 0) {
        cat(" Observables:\n")
        cat(sprintf(
            "  (%s) %s = %s\n",
            seq_along(x$observables),
            names(x$observables),
            vapply(x$observables, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Observables: (none)\n")
    }

    if (nrow(x$dosing) > 0) {
        cat(" Dosing:\n")
        cat(sprintf(
            "  (%s) %s %s to %s at %s\n",
            seq_len(nrow(x$dosing)),
            x$dosing$operation,
            vapply(x$dosing$value, .ode_model_format_expr, character(1), model = x),
            x$states$dsl_name[x$dosing$state],
            vapply(x$dosing$time, .ode_model_format_expr, character(1), model = x)
        ), sep = "")
    } else {
        cat(" Dosing: (none)\n")
    }

    print(x$parameters)

    if (length(x$freeParams) > 0) {
        cat(" Free parameters: ", paste(x$freeParams, collapse = ", "), "\n", sep = "")
    } else {
        cat(" Free parameters: (none)\n")
    }

    invisible(x)
}

.to_analytical <- function(model, parameters = list(), dimensions = NULL) {
    if (!inherits(model, "AnalyticalModel")) {
        .check_class(model, "CompartmentModel")
        model <- to_analytical_model(model)
    }
    parameters <- .simulation_parameters_object(parameters)
    output_state_names <- model$states$output_name

    statefun <- function(t, params = list()) {
        runtime_parameters <- .merge_ode_parameters(parameters, .simulation_parameters_object(params))
        system <- .analytical_model_numeric_system(
            model,
            parameters = runtime_parameters,
            dimensions = dimensions
        )
        solver_time <- .simulation_numeric_time(t, dimensions %||% list())
        state_matrix <- .analytical_model_solve_states(
            A = system$A,
            b = system$b,
            y0 = system$y0,
            time = solver_time
        )
        colnames(state_matrix) <- output_state_names
        cbind(time = solver_time, state_matrix)
    }

    obs_funcs <- .analytical_model_observable_functions(model, parameters, dimensions)

    list(
        statefun = statefun,
        stateNames = output_state_names,
        dslStateNames = model$states$dsl_name,
        freeParams = model$freeParams,
        obsFuncs = obs_funcs,
        A = model$A
    )
}

.to_deSolve <- function(model, parameters = list(), dimensions = NULL) {
    .check_class(model, "OdeModel")
    parameters <- .simulation_parameters_object(parameters)
    params <- .merge_ode_parameters(model$parameters, parameters)
    param_values <- .to_dimensions_vec(params, dimensions)
    free_params <- new.env(parent = emptyenv())
    free_params$list <- character()
    eq_names <- names(model$equations)

    subst <- function(expr) {
        .ode_model_substitute_parameters(expr, eq_names, param_values, free_params, dimensions)
    }

    y0 <- .evaluate_initials(
        setNames(model$initials, model$states$dsl_name),
        params,
        allow_unresolved = FALSE
    ) |>
        .to_dimensions_vec(dimensions)

    output_state_names <- model$states$output_name
    y0 <- setNames(unlist(y0), output_state_names)

    lines <- "function(t,y,params) {"
    for (i in seq_along(model$equations)) {
        lines <- c(lines, paste0("    ", names(model$equations)[[i]], " <- ", deparse1(subst(model$equations[[i]]))))
        if (i == length(model$equations)) lines <- c(lines, "")
    }
    lines <- c(lines, paste0("    dydt <- numeric(", length(model$rhs), ")"))
    for (i in seq_along(model$rhs)) {
        lines <- c(lines, paste0("    dydt[", i, "] <- ", deparse1(subst(model$rhs[[i]]))))
    }
    lines <- c(lines, "    list(dydt)", "}")
    odefun <- eval(parse(text = paste(lines, collapse = "\n")))

    obs_funcs <- lapply(model$observables, function(obs) {
        expr <- .ode_model_observable_backend_expr(subst(obs), output_state_names)
        eval(parse(text = paste0("function(t,y,params) unname(", deparse1(expr), ")")))
    })
    names(obs_funcs) <- names(model$observables)

    events <- list(data = data.frame(var = character(), time = numeric(), value = numeric(), method = character()))
    if (length(model$dosing$state) > 0) {
        event_time <- lapply(model$dosing$time, subst) |> .to_dimensions_vec(dimensions)
        event_value <- lapply(model$dosing$value, subst) |> .to_dimensions_vec(dimensions)
        events$data <- data.frame(
            var = output_state_names[model$dosing$state],
            time = unlist(event_time),
            value = unlist(event_value),
            method = model$dosing$operation,
            stringsAsFactors = FALSE
        )
    }

    list(
        odefun = odefun,
        stateNames = output_state_names,
        dslStateNames = model$states$dsl_name,
        obsFuncs = obs_funcs,
        freeParams = sort(unique(free_params$list)),
        y0 = y0,
        events = events
    )
}

.ode_model_check_transport_compartments <- function(model) {
    comp_names <- names(model$compartments)
    transport_comps <- setdiff(
        unique(c(model$transports$from, model$transports$to)),
        NA_character_
    )
    missing <- transport_comps[!(transport_comps %in% comp_names)]
    if (length(missing) > 0) {
        stop(
            "Transport references unknown compartment: ",
            paste(missing, collapse = ", "),
            ". ",
            "Compartment names in this model: ",
            paste(comp_names, collapse = ", "),
            ". ",
            "Did you mean to merge this model with another?"
        )
    }
    invisible(model)
}

.process_model_stoichiometry <- function(stoich_cols, state_names) {
    if (length(stoich_cols) == 0) {
        return(matrix(
            numeric(0),
            nrow = length(state_names),
            ncol = 0,
            dimnames = list(state_names, character())
        ))
    }

    stoichiometry <- do.call(cbind, stoich_cols)
    rownames(stoichiometry) <- state_names
    stoichiometry
}

.process_model_ode_term <- function(coeff, rate, const) {
    magnitude <- abs(coeff)
    term <- rate
    if (!identical(magnitude, 1) && !identical(magnitude, 1L)) {
        term <- .mul(term, magnitude)
    }
    if (coeff < 0) {
        return(.negate_expr(term, simplify_product = .process_model_has_const(const)))
    }
    term
}

.process_model_has_const <- function(x) {
    !(is.atomic(x) && length(x) == 1L && is.na(x))
}

.process_model_display_stoichiometry <- function(stoichiometry) {
    colnames(stoichiometry) <- paste0("(", seq_len(ncol(stoichiometry)), ")")
    stoichiometry
}

.analytical_model_display_A <- function(model) {
    A <- matrix(
        vapply(as.list(model$A), .ode_model_format_expr, character(1), model = model),
        nrow = nrow(model$A),
        dimnames = list(
            paste0("(", seq_len(nrow(model$A)), ")"),
            paste0("(", seq_len(ncol(model$A)), ")")
        )
    )
    A
}

.analytical_model_numeric_system <- function(model, parameters = list(), dimensions = NULL) {
    parameters <- .simulation_parameters_object(parameters)
    merged_parameters <- .merge_ode_parameters(model$parameters, parameters)
    .analytical_model_check_parameters_available(
        model,
        merged_parameters,
        free_params = .analytical_model_system_free_params(model)
    )

    y0 <- .evaluate_initials(
        setNames(model$initials, model$states$dsl_name),
        merged_parameters,
        allow_unresolved = FALSE
    ) |>
        .to_dimensions_vec(dimensions)
    y0 <- unname(unlist(y0))

    system_fun <- .analytical_model_system_function(model, merged_parameters, dimensions)
    system <- system_fun(.simulation_solver_parameters(merged_parameters, dimensions))
    system$y0 <- y0
    system
}

.analytical_model_system_function <- function(model, parameters, dimensions) {
    param_values <- .to_dimensions_vec(parameters, dimensions)
    free_params <- new.env(parent = emptyenv())
    free_params$list <- character()
    eq_names <- names(model$equations)

    subst <- function(expr) {
        .ode_model_substitute_parameters(expr, eq_names, param_values, free_params, dimensions)
    }

    lines <- "function(params) {"
    for (i in seq_along(model$equations)) {
        lines <- c(lines, paste0("    ", names(model$equations)[[i]], " <- ", deparse1(subst(model$equations[[i]]))))
        if (i == length(model$equations)) lines <- c(lines, "")
    }
    lines <- c(lines, paste0("    A <- matrix(0, ", nrow(model$A), ", ", ncol(model$A), ")"))
    for (i in seq_len(nrow(model$A))) {
        for (j in seq_len(ncol(model$A))) {
            lines <- c(lines, paste0("    A[", i, ", ", j, "] <- ", deparse1(subst(model$A[[i, j]]))))
        }
    }
    lines <- c(lines, paste0("    b <- numeric(", length(model$b), ")"))
    for (i in seq_along(model$b)) {
        lines <- c(lines, paste0("    b[", i, "] <- ", deparse1(subst(model$b[[i]]))))
    }
    lines <- c(lines, "    list(A = A, b = b)", "}")

    eval(parse(text = paste(lines, collapse = "\n")))
}

.analytical_model_observable_functions <- function(model, parameters, dimensions) {
    param_values <- .to_dimensions_vec(.merge_ode_parameters(model$parameters, parameters), dimensions)
    free_params <- new.env(parent = emptyenv())
    free_params$list <- character()
    eq_names <- names(model$equations)
    output_state_names <- model$states$output_name

    subst <- function(expr) {
        .ode_model_substitute_parameters(expr, eq_names, param_values, free_params, dimensions)
    }

    obs_funcs <- lapply(model$observables, function(obs) {
        expr <- .ode_model_observable_backend_expr(subst(obs), output_state_names)
        eval(parse(text = paste0("function(t,y,params) unname(", deparse1(expr), ")")))
    })
    names(obs_funcs) <- names(model$observables)
    obs_funcs
}

.analytical_model_solve_states <- function(A, b, y0, time) {
    if (length(y0) == 0L) {
        return(matrix(numeric(0), nrow = length(time), ncol = 0))
    }
    if (any(b != 0)) {
        stop("AnalyticalModel simulation with nonzero b is not implemented yet.", call. = FALSE)
    }

    out <- vapply(
        time,
        function(tt) as.numeric(expm::expm(A * tt) %*% y0),
        numeric(length(y0))
    )
    if (length(y0) == 1L) {
        out <- matrix(out, ncol = 1L)
    } else {
        out <- t(out)
    }
    colnames(out) <- names(y0) %||% character(length(y0))
    out
}

.analytical_model_check_parameters_available <- function(model, parameters, free_params = model$freeParams) {
    missing <- setdiff(free_params, names(parameters))
    if (length(missing) > 0) {
        stop(
            "AnalyticalModel requires missing free parameter(s): ",
            paste(missing, collapse = ", "),
            ".",
            call. = FALSE
        )
    }

    invisible(model)
}

.analytical_model_system_free_params <- function(model) {
    .ode_model_free_params(
        c(model$initials, as.list(model$A), as.list(model$b), unclass(model$equations)),
        eq_names = names(model$equations),
        param_names = names(model$parameters)
    )
}

.analytical_model_check_unit_consistency <- function(model, parameters) {
    context <- .ode_model_unit_context(model, parameters)
    y0 <- context$y0
    env <- context$env

    for (i in seq_len(nrow(model$A))) {
        rhs_value <- tryCatch(
            {
                terms <- list()
                b_value <- .ode_model_eval_expr(model$b[[i]], env, y0)
                if (!.ode_model_is_numeric_zero(b_value)) {
                    terms <- c(terms, list(b_value))
                }
                for (j in seq_len(ncol(model$A))) {
                    coeff <- .ode_model_eval_expr(model$A[[i, j]], env, y0)
                    if (.ode_model_is_numeric_zero(coeff)) next
                    terms <- c(terms, list(coeff * y0[[j]]))
                }
                if (length(terms) == 0L) 0 else Reduce(`+`, terms)
            },
            error = function(e) {
                stop(
                    "Cannot evaluate analytical state '",
                    model$states$dsl_name[[i]],
                    "': ",
                    e$message,
                    call. = FALSE
                )
            }
        )
        .ode_model_check_derivative_units(
            state_value = y0[[i]],
            rhs_value = rhs_value,
            state_name = model$states$dsl_name[[i]]
        )
    }

    invisible(model)
}

.analytical_model_dimension_values <- function(model, parameters) {
    y0 <- tryCatch(
        .ode_model_state_unit_values(model, parameters, allow_unresolved = TRUE),
        error = function(e) list()
    )
    system <- tryCatch(
        .analytical_model_raw_system_values(model, y0, parameters),
        error = function(e) list(A = list(), b = list())
    )

    c(y0, unclass(parameters), as.list(system$A), system$b)
}

.analytical_model_raw_system_values <- function(model, y0, parameters) {
    if (length(y0) != nrow(model$states)) return(list(A = list(), b = list()))
    if (any(vapply(y0, .initial_is_expr, logical(1)))) return(list(A = list(), b = list()))

    env <- list2env(unclass(parameters), parent = baseenv())
    .ode_model_add_equations(model, env, y0)

    A <- lapply(as.list(model$A), function(expr) eval(expr, envir = env))
    b <- lapply(model$b, function(expr) eval(expr, envir = env))
    list(A = A, b = b)
}

.analytical_model_check_compatibility <- function(model) {
    if (nrow(model$dosing) > 0) {
        stop("AnalyticalModel with dosing is not implemented yet.", call. = FALSE)
    }

    invisible(model)
}

.analytical_model_linear_system <- function(model) {
    n_states <- nrow(model$states)
    state_names <- model$states$dsl_name
    A <- matrix(
        vector("list", n_states * n_states),
        nrow = n_states,
        dimnames = list(state_names, state_names)
    )
    A[] <- list(0)
    b <- setNames(vector("list", n_states), state_names)
    b[] <- list(0)

    for (j in seq_len(nrow(model$processes))) {
        rate <- model$processes$rate[[j]]
        const <- model$processes$const[[j]]
        input_states <- model$processes$input_states[[j]]
        input_stoich <- model$processes$input_stoich[[j]]

        if (.analytical_model_is_first_order_process(const, input_states, input_stoich)) {
            source_state <- input_states[[1]]
            for (target_state in seq_len(n_states)) {
                coeff <- model$stoichiometry[[target_state, j]]
                if (.analytical_model_is_zero(coeff)) next
                term <- .analytical_model_scale_expr(const, coeff)
                A[[target_state, source_state]] <- .analytical_model_add_expr(
                    A[[target_state, source_state]],
                    term
                )
            }
            next
        }

        if (.analytical_model_is_constant_process(rate, input_states)) {
            for (target_state in seq_len(n_states)) {
                coeff <- model$stoichiometry[[target_state, j]]
                if (.analytical_model_is_zero(coeff)) next
                term <- .analytical_model_scale_expr(rate, coeff)
                b[[target_state]] <- .analytical_model_add_expr(b[[target_state]], term)
            }
            next
        }

        stop(
            "AnalyticalModel currently supports only linear first-order processes.",
            call. = FALSE
        )
    }

    list(A = A, b = b)
}

.analytical_model_is_first_order_process <- function(const, input_states, input_stoich) {
    .process_model_has_const(const) &&
        length(input_states) == 1L &&
        length(input_stoich) == 1L &&
        identical(as.numeric(input_stoich), 1)
}

.analytical_model_is_constant_process <- function(rate, input_states) {
    length(input_states) == 0L && !.analytical_model_has_state_ref(rate)
}

.analytical_model_check_zero_b <- function(b) {
    nonzero <- !vapply(b, .analytical_model_is_zero, logical(1))
    if (any(nonzero)) {
        stop(
            "The current AnalyticalModel represents b but requires b = 0; ",
            "nonzero source or constant terms are not supported yet.",
            call. = FALSE
        )
    }

    invisible(b)
}

.analytical_model_add_expr <- function(x, y) {
    if (.analytical_model_is_zero(x)) return(y)
    if (.analytical_model_is_zero(y)) return(x)
    call("+", x, y)
}

.analytical_model_scale_expr <- function(expr, coeff) {
    if (.analytical_model_is_zero(coeff)) return(0)
    if (identical(as.numeric(coeff), 1)) return(expr)
    if (identical(as.numeric(coeff), -1)) {
        return(.negate_expr(expr, simplify_product = TRUE))
    }
    .mul(expr, coeff)
}

.analytical_model_is_zero <- function(x) {
    is.numeric(x) && length(x) == 1L && isTRUE(x == 0)
}

.analytical_model_has_state_ref <- function(expr) {
    recurse <- function(e) {
        if (!is.call(e)) return(FALSE)
        if (identical(e[[1]], as.name("[")) && length(e) == 3L && identical(e[[2]], as.name("y"))) {
            return(TRUE)
        }
        any(vapply(as.list(e), recurse, logical(1)))
    }

    recurse(.as_call(expr))
}

.stochastic_model_check_compatibility <- function(model) {
    if (nrow(model$dosing) > 0) {
        stop("SSA simulation with dosing is not implemented yet.", call. = FALSE)
    }

    if (any(model$states$type != "amount")) {
        stop(
            "StochasticModel requires amount/count states; concentration states are not supported.",
            call. = FALSE
        )
    }

    if (length(model$initials) > 0) {
        initials <- .evaluate_initials(
            setNames(model$initials, model$states$dsl_name),
            model$parameters,
            allow_unresolved = TRUE
        )
        .stochastic_model_check_initials(initials, allow_unresolved = TRUE)
    }

    if (length(model$stoichiometry) > 0 &&
        any(model$stoichiometry != round(model$stoichiometry))) {
        stop("StochasticModel requires integer-valued stoichiometry.", call. = FALSE)
    }

    explicit_rate <- vapply(model$processes$const, .stochastic_model_is_explicit_rate, logical(1))
    if (any(explicit_rate)) {
        stop(
            "Explicit rate processes are not implemented for the current StochasticModel.",
            call. = FALSE
        )
    }

    invisible(model)
}

.stochastic_model_check_initials <- function(initials, allow_unresolved = FALSE) {
    for (i in seq_along(initials)) {
        value <- .stochastic_model_normalize_count_initial(initials[[i]])
        if (.initial_is_expr(value)) {
            if (allow_unresolved) next
            stop(
                "Initial value for stochastic state '",
                names(initials)[[i]],
                "' must be a numeric count.",
                call. = FALSE
            )
        }
        if (inherits(value, "units")) {
            stop(
                "Initial value for stochastic state '",
                names(initials)[[i]],
                "' must be a dimensionless count, not a unit-bearing value.",
                call. = FALSE
            )
        }
        if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value)) {
            stop(
                "Initial value for stochastic state '",
                names(initials)[[i]],
                "' must be a finite numeric count.",
                call. = FALSE
            )
        }
        if (value < 0) {
            stop(
                "Initial value for stochastic state '",
                names(initials)[[i]],
                "' must be a non-negative count.",
                call. = FALSE
            )
        }
        if (value != round(value)) {
            stop(
                "Initial value for stochastic state '",
                names(initials)[[i]],
                "' must be an integer count.",
                call. = FALSE
            )
        }
    }

    invisible(initials)
}

.stochastic_model_normalize_count_initial <- function(value) {
    if (!inherits(value, "units")) return(value)

    value <- .expand_registered_model_units(value)
    unit_obj <- units(value)
    if (length(unit_obj$numerator) == 0L && length(unit_obj$denominator) == 0L) {
        return(units::set_units(value, NULL))
    }

    value
}

.stochastic_model_is_explicit_rate <- function(const) {
    is.atomic(const) && length(const) == 1L && is.na(const)
}

.stochastic_model_propensity <- function(const, input_states, input_stoich) {
    term <- .as_call(const)

    if (length(input_states) == 0) return(term)

    for (i in seq_along(input_states)) {
        factor <- .stochastic_model_input_factor(
            idx = input_states[[i]],
            stoich = input_stoich[[i]]
        )
        term <- .stochastic_model_mul(term, factor)
    }

    term
}

.stochastic_model_input_factor <- function(idx, stoich) {
    stoich <- as.integer(stoich)
    idx <- as.numeric(idx)
    if (identical(stoich, 1L)) return(bquote(y[.(idx)]))

    bquote(.ssa_falling(y[.(idx)], .(stoich)) / .(factorial(stoich)))
}

.stochastic_model_mul <- function(x, y) {
    if (is.numeric(x) && length(x) == 1L && identical(x, 0)) return(0)
    if (is.numeric(y) && length(y) == 1L && identical(y, 0)) return(0)
    if (is.numeric(x) && length(x) == 1L && identical(x, 1)) return(y)
    if (is.numeric(y) && length(y) == 1L && identical(y, 1)) return(x)
    call("*", x, y)
}

.process_model_amount_reaction_rate <- function(
    rate_expr,
    type,
    scale_cmt,
    volume,
    participants,
    state_names,
    eq_names,
    name2idx,
    state_volumes
) {
    if (identical(type, "elementary")) {
        scale_state <- .process_model_scale_concentration_state(participants, scale_cmt, state_names)
        if (!is.null(scale_state)) {
            return(.ode_model_lower_expr(
                rate_expr,
                state_names = state_names,
                eq_names = eq_names,
                name2idx = name2idx,
                state_volumes = state_volumes,
                concentration_as_amount_once = scale_state
            ))
        }
    }

    .ode_model_lower_expr(
        .mul(rate_expr, .as_call(volume)),
        state_names = state_names,
        eq_names = eq_names,
        name2idx = name2idx,
        state_volumes = state_volumes
    )
}

.process_model_scale_concentration_state <- function(participants, scale_cmt, state_names) {
    if (is.na(scale_cmt)) return(NULL)
    input <- participants[
        participants$role == "input" &
            !is.na(participants$cmt) &
            participants$cmt == scale_cmt,
        ,
        drop = FALSE
    ]
    if (nrow(input) == 0) return(NULL)

    for (i in seq_len(nrow(input))) {
        conc_state <- .dsl_make_state(input$molec[[i]], input$cmt[[i]], prefix = "c")
        amount_state <- .dsl_make_state(input$molec[[i]], input$cmt[[i]], prefix = "a")
        if (amount_state %in% state_names) return(conc_state)
    }

    NULL
}

.ode_model_observable_backend_expr <- function(expr, output_state_names) {
    recurse <- function(e) {
        if (is.call(e)) {
            if (identical(e[[1]], as.name("[")) && length(e) == 3 && identical(e[[2]], as.name("y"))) {
                idx <- e[[3]]
                if (is.numeric(idx) && length(idx) == 1L) {
                    return(bquote(y[.obs_idx(t, y), .(output_state_names[[idx]])]))
                }
            }
            return(as.call(lapply(as.list(e), recurse)))
        }
        e
    }

    recurse(expr)
}

.ode_model_format_expr <- function(expr, model) {
    if (!is.character(expr) && !is.expression(expr) && !is.language(expr)) {
        return(.ode_model_format_atom(expr))
    }
    .ode_model_deparse_with_unit_placeholders(.ode_model_display_expr(expr, model))
}

.ode_model_display_expr <- function(expr, model) {
    recurse <- function(e) {
        if (is.call(e)) {
            if (identical(e[[1]], as.name("[")) && length(e) == 3 && identical(e[[2]], as.name("y"))) {
                idx <- e[[3]]
                if (is.numeric(idx) && length(idx) == 1L) {
                    return(.ode_model_dsl_state_call(model$states$dsl_name[[idx]]))
                }
            }
            return(as.call(lapply(as.list(e), recurse)))
        }
        e
    }

    recurse(.as_call(expr))
}

.ode_model_deparse_with_unit_placeholders <- function(expr) {
    unit_values <- list()
    prefix <- .ode_model_unit_placeholder_prefix(expr)

    recurse <- function(e) {
        if (inherits(e, "units")) {
            placeholder <- paste0(prefix, length(unit_values) + 1L)
            unit_values[[placeholder]] <<- .ode_model_format_atom(e)
            return(as.name(placeholder))
        }
        if (is.call(e)) return(as.call(lapply(as.list(e), recurse)))
        e
    }

    out <- deparse1(recurse(expr))
    for (placeholder in names(unit_values)) {
        out <- gsub(placeholder, unit_values[[placeholder]], out, fixed = TRUE)
    }
    out
}

.ode_model_unit_placeholder_prefix <- function(expr) {
    vars <- all.vars(expr)
    prefix <- ".compphysiol_unit_display_"
    while (any(startsWith(vars, prefix))) {
        prefix <- paste0(prefix, "x")
    }
    prefix
}

.ode_model_format_atom <- function(expr) {
    paste(format(expr), collapse = ", ")
}

.ode_model_dsl_state_call <- function(state_name) {
    parsed <- .dsl_parse_state(state_name)
    call(
        "[",
        as.name(parsed$prefix),
        as.name(parsed$molec),
        as.name(parsed$cmt)
    )
}

.ode_model_state_info <- function(model, dsl_state_names) {
    parsed <- lapply(dsl_state_names, .dsl_parse_state)
    auto_placeholder <- model$metadata$auto_placeholder %||% list()
    data.frame(
        index = seq_along(dsl_state_names),
        dsl_name = dsl_state_names,
        output_name = .dsl_state_to_name(
            dsl_state_names,
            omit_molec = isTRUE(auto_placeholder$molec),
            omit_cmt = isTRUE(auto_placeholder$cmt)
        ),
        type = ifelse(vapply(parsed, `[[`, character(1), "prefix") == "a", "amount", "concentration"),
        stringsAsFactors = FALSE
    )
}

.ode_model_state_volumes <- function(model) {
    volume_by_cmt <- setNames(model$compartments$volume, names(model$compartments))
    volumes <- list()
    for (i in seq_along(model$molecules)) {
        molec <- model$molecules$name[[i]]
        cmt <- model$molecules$cmt[[i]]
        vol <- volume_by_cmt[[cmt]]
        volumes[[.dsl_make_state(molec = molec, cmt = cmt, prefix = "a")]] <- vol
        volumes[[.dsl_make_state(molec = molec, cmt = cmt, prefix = "c")]] <- vol
    }
    volumes
}

.ode_model_lower_expr <- function(
    expr,
    state_names,
    eq_names,
    name2idx,
    state_volumes,
    concentration_as_amount_once = NULL
) {
    if (!is.character(expr) && !is.expression(expr) && !is.language(expr)) {
        return(expr)
    }
    expr <- .as_call(expr)
    concentration_as_amount_used <- FALSE

    lower <- function(e) {
        state_ref <- function(nm) bquote(y[.(as.numeric(name2idx[[nm]]))])

        if (is.symbol(e)) {
            nm <- as.character(e)
            if (nm %in% state_names) return(state_ref(nm))
            return(e)
        }

        if (is.call(e)) {
            if (.dsl_is_special(e) && length(e) >= 4) {
                prefix <- as.character(e[[2]])
                molec <- as.character(e[[3]])
                cmt <- as.character(e[[4]])
                nm <- .dsl_make_state(molec = molec, cmt = cmt, prefix = prefix)
                if (nm %in% state_names) return(state_ref(nm))

                if (prefix == "c") {
                    amount_nm <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "a")
                    if (
                        !is.null(concentration_as_amount_once) &&
                            identical(nm, concentration_as_amount_once) &&
                            !concentration_as_amount_used &&
                            amount_nm %in% state_names
                    ) {
                        concentration_as_amount_used <<- TRUE
                        return(state_ref(amount_nm))
                    }
                    if (amount_nm %in% state_names) {
                        if (.is_missing_volume(state_volumes[[amount_nm]])) {
                            stop("Cannot convert amount state '", amount_nm, "' to concentration without a compartment volume.", call. = FALSE)
                        }
                        return(bquote(.(state_ref(amount_nm)) / .(lower(state_volumes[[amount_nm]]))))
                    }
                }
                if (prefix == "a") {
                    conc_nm <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "c")
                    if (conc_nm %in% state_names) {
                        if (.is_missing_volume(state_volumes[[conc_nm]])) {
                            stop("Cannot convert concentration state '", conc_nm, "' to amount without a compartment volume.", call. = FALSE)
                        }
                        return(bquote(.(state_ref(conc_nm)) * .(lower(state_volumes[[conc_nm]]))))
                    }
                }
            }
            return(as.call(lapply(as.list(e), lower)))
        }

        e
    }

    lower(expr)
}

.ode_model_substitute_parameters <- function(expr, eq_names, param_values, free_params, dimensions) {
    reserved <- c("t", "y", "params", "pi", "Inf", "NaN", "TRUE", "FALSE", "NULL")

    recurse <- function(e) {
        if (is.symbol(e)) {
            nm <- as.character(e)
            if (nm %in% eq_names) return(e)
            if (nm %in% names(param_values)) {
                val <- param_values[[nm]]
                if (is.numeric(val) && length(val) == 1L) return(val)
                free_params$list <- union(free_params$list, nm)
                return(bquote(params[[.(nm)]]))
            }
            if (nm %in% reserved || exists(nm, mode = "function", inherits = TRUE)) return(e)
            free_params$list <- union(free_params$list, nm)
            return(bquote(params[[.(nm)]]))
        }
        if (is.call(e)) return(as.call(lapply(as.list(e), recurse)))
        if (inherits(e, "units")) {
            return(as.numeric(.to_dimensions_value(e, dimensions)))
        }
        e
    }

    recurse(.as_call(expr))
}

.ode_model_check_unit_consistency <- function(model, parameters) {
    context <- .ode_model_unit_context(model, parameters)
    y0 <- context$y0
    env <- context$env

    for (i in seq_along(model$rhs)) {
        rhs_value <- tryCatch(
            .ode_model_eval_expr(model$rhs[[i]], env, y0),
            error = function(e) {
                stop(
                    "Cannot evaluate right-hand side for ODE state '",
                    model$states$dsl_name[[i]],
                    "': ",
                    e$message,
                    call. = FALSE
                )
            }
        )
        .ode_model_check_derivative_units(
            state_value = y0[[i]],
            rhs_value = rhs_value,
            state_name = model$states$dsl_name[[i]]
        )
    }

    for (i in seq_along(model$dosing$state)) {
        state_idx <- model$dosing$state[[i]]
        dose_value <- tryCatch(
            .ode_model_eval_expr(model$dosing$value[[i]], env, y0),
            error = function(e) {
                stop(
                    "Cannot evaluate dosing event for ODE state '",
                    model$states$dsl_name[[state_idx]],
                    "': ",
                    e$message,
                    call. = FALSE
                )
            }
        )
        .ode_model_check_same_units(
            expected = y0[[state_idx]],
            value = dose_value,
            state_name = model$states$dsl_name[[state_idx]],
            what = "dosing event"
        )
    }

    invisible(model)
}

.ode_model_unit_context <- function(model, parameters) {
    y0 <- .ode_model_state_unit_values(model, parameters, allow_unresolved = FALSE)
    env <- list2env(unclass(parameters), parent = baseenv())
    .ode_model_add_equations(model, env, y0)
    list(y0 = y0, env = env)
}

.ode_model_state_unit_values <- function(model, parameters, allow_unresolved = TRUE) {
    .evaluate_initials(
        setNames(model$initials, model$states$dsl_name),
        parameters,
        allow_unresolved = allow_unresolved
    )
}

.ode_model_observable_unit_values <- function(model, parameters) {
    if (length(model$observables) == 0) return(list())
    context <- .ode_model_unit_context(model, parameters)
    lapply(model$observables, .ode_model_eval_expr, env = context$env, y_values = context$y0)
}

.ode_model_dimension_values <- function(model, parameters) {
    y0 <- tryCatch(
        .ode_model_state_unit_values(model, parameters, allow_unresolved = TRUE),
        error = function(e) list()
    )
    dosing_time <- lapply(model$dosing$time, .ode_model_eval_expr, env = list2env(unclass(parameters), parent = baseenv()), y_values = y0)
    dosing_value <- lapply(model$dosing$value, .ode_model_eval_expr, env = list2env(unclass(parameters), parent = baseenv()), y_values = y0)

    c(y0, unclass(parameters), dosing_time, dosing_value)
}

.ode_model_add_equations <- function(model, env, y_values) {
    pending <- seq_along(model$equations)
    while (length(pending) > 0) {
        resolved <- logical(length(pending))
        for (k in seq_along(pending)) {
            i <- pending[[k]]
            expr <- .ode_model_inline_state_values(model$equations[[i]], y_values)
            vars <- all.vars(expr)
            if (!all(vars %in% names(env))) next

            value <- tryCatch(
                eval(expr, envir = env),
                error = function(e) {
                    stop(
                        "In ODE equation '",
                        names(model$equations)[[i]],
                        "', unit inconsistency in expression: ",
                        e$message,
                        call. = FALSE
                    )
                }
            )
            assign(names(model$equations)[[i]], value, envir = env)
            resolved[[k]] <- TRUE
        }
        if (!any(resolved)) break
        pending <- pending[!resolved]
    }

    invisible(env)
}

.ode_model_eval_expr <- function(expr, env, y_values) {
    expr <- .ode_model_inline_state_values(expr, y_values)
    if (is.language(expr)) eval(expr, envir = env) else expr
}

.ode_model_inline_state_values <- function(expr, y_values) {
    recurse <- function(e) {
        if (is.call(e)) {
            if (identical(e[[1]], as.name("[")) && length(e) == 3 && identical(e[[2]], as.name("y"))) {
                idx <- e[[3]]
                if (is.numeric(idx) && length(idx) == 1L) return(y_values[[idx]])
            }
            return(as.call(lapply(as.list(e), recurse)))
        }
        e
    }

    recurse(expr)
}

.ode_model_check_derivative_units <- function(state_value, rhs_value, state_name) {
    if (.ode_model_is_numeric_zero(rhs_value)) return(invisible(NULL))

    state_has_units <- inherits(state_value, "units")
    rhs_has_units <- inherits(rhs_value, "units")
    if (!state_has_units && !rhs_has_units) return(invisible(NULL))
    if (state_has_units != rhs_has_units) {
        stop(
            "In ODE state '",
            state_name,
            "', unit inconsistency in right-hand side: state and derivative units are not both unit-aware.",
            call. = FALSE
        )
    }

    expected <- state_value / units::set_units(1, "h", mode = "standard")
    if (!units::ud_are_convertible(units(rhs_value), units(expected))) {
        stop(
            "In ODE state '",
            state_name,
            "', unit inconsistency in right-hand side: expected units compatible with ",
            units(expected),
            ", got ",
            units(rhs_value),
            ".",
            call. = FALSE
        )
    }

    invisible(NULL)
}

.ode_model_check_same_units <- function(expected, value, state_name, what) {
    if (.ode_model_is_numeric_zero(value)) return(invisible(NULL))

    expected_has_units <- inherits(expected, "units")
    value_has_units <- inherits(value, "units")
    if (!expected_has_units && !value_has_units) return(invisible(NULL))
    if (expected_has_units != value_has_units || !units::ud_are_convertible(units(value), units(expected))) {
        stop(
            "In ODE state '",
            state_name,
            "', unit inconsistency in ",
            what,
            ": expected units compatible with ",
            if (expected_has_units) units(expected) else "unitless",
            ", got ",
            if (value_has_units) units(value) else "unitless",
            ".",
            call. = FALSE
        )
    }

    invisible(NULL)
}

.ode_model_is_numeric_zero <- function(x) {
    is.numeric(x) && length(x) == 1L && isTRUE(as.numeric(x) == 0)
}

.ode_model_free_params <- function(exprs, eq_names, param_names) {
    vars <- unique(unlist(lapply(exprs, .dsl_all_vars), use.names = FALSE))
    reserved <- c("t", "y", "params", "pi", "Inf", "NaN", "TRUE", "FALSE", "NULL")
    sort(setdiff(vars, c(eq_names, param_names, reserved)))
}

.ode_model_transport_state_idx <- function(molec, cmt, name2idx) {
    state <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "a")
    idx <- if (state %in% names(name2idx)) name2idx[[state]] else NULL
    if (!is.null(idx)) return(idx)

    conc_state <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "c")
    if (conc_state %in% names(name2idx)) {
        stop(
            "Transports require amount states or a compartment volume; state '",
            conc_state,
            "' is a concentration state and cannot be used for transport.",
            call. = FALSE
        )
    }

    stop("Transport references unknown state: ", state, ". Did you define the corresponding molecule in this compartment?", call. = FALSE)
}

.ode_model_reaction_state <- function(molec, cmt, name2idx) {
    amount_state <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "a")
    amount_idx <- if (amount_state %in% names(name2idx)) name2idx[[amount_state]] else NULL
    if (!is.null(amount_idx)) return(list(name = amount_state, idx = amount_idx, type = "amount"))

    conc_state <- .dsl_make_state(molec = molec, cmt = cmt, prefix = "c")
    conc_idx <- if (conc_state %in% names(name2idx)) name2idx[[conc_state]] else NULL
    if (!is.null(conc_idx)) return(list(name = conc_state, idx = conc_idx, type = "concentration"))

    stop("Reaction references unknown state: ", amount_state, ". Did you define the corresponding molecule in this compartment?", call. = FALSE)
}

.ode_model_dosing <- function(model, name2idx) {
    events <- .dosing_to_events(model)$data
    if (nrow(events) == 0) {
        return(structure(
            data.frame(state = integer(), time = I(list()), value = I(list()), operation = character()),
            class = c("OdeDosing", "data.frame")
        ))
    }

    idx <- unname(name2idx[events$var])
    if (any(is.na(idx))) stop("Some dosing events do not map to generated ODE states.", call. = FALSE)
    structure(
        data.frame(
            state = as.integer(idx),
            time = I(as.list(events$time)),
            value = I(as.list(events$value)),
            operation = events$method,
            stringsAsFactors = FALSE
        ),
        class = c("OdeDosing", "data.frame")
    )
}

.sum_exprs <- function(exprs) {
    if (length(exprs) == 0) return(0)
    Reduce(function(a, b) bquote(.(a) + .(b)), exprs)
}

.negate_expr <- function(expr, simplify_product = FALSE) {
    if (is.numeric(expr) && length(expr) == 1L) return(-expr)
    if (simplify_product && is.call(expr) && identical(expr[[1]], as.name("*")) && length(expr) == 3L) {
        return(call("*", .negate_expr(expr[[2]]), expr[[3]]))
    }
    call("-", expr)
}

.dsl_parse_state <- function(x) {
    prefix <- substr(x, 1, 1)
    open <- regexpr("[", x, fixed = TRUE)[[1]]
    comma <- regexpr(",", x, fixed = TRUE)[[1]]
    close <- regexpr("]", x, fixed = TRUE)[[1]]
    if (
        !(prefix %in% c("a", "c")) ||
        open != 2 ||
        comma < 4 ||
        close != nchar(x) ||
        comma > close
    ) {
        stop("Invalid DSL state name: ", x, call. = FALSE)
    }
    list(
        prefix = prefix,
        molec = trimws(substr(x, open + 1, comma - 1)),
        cmt = trimws(substr(x, comma + 1, close - 1))
    )
}

.merge_ode_parameters <- function(defaults, overrides) {
    if (length(overrides) > 0 && (is.null(names(overrides)) || any(names(overrides) == ""))) {
        stop("Argument 'parameters' must be named.", call. = FALSE)
    }
    params <- defaults
    for (nm in names(overrides)) {
        params[nm] <- overrides[nm]
    }
    params
}
