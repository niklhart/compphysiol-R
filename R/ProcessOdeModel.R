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
    .check_class(model, "CompartmentModel")

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
    .check_class(model, "CompartmentModel")

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

    rhs <- vector("list", length(dsl_state_names))
    for (j in seq_along(model$transports)) {
        from <- model$transports$from[[j]]
        to <- model$transports$to[[j]]
        molec <- model$transports$molec[[j]]
        from_idx <- if (!is.na(from)) .ode_model_transport_state_idx(molec, from, name2idx) else NULL
        to_idx <- if (!is.na(to)) .ode_model_transport_state_idx(molec, to, name2idx) else NULL
        rate <- lower(model$transports$rate[[j]])

        if (!is.na(from)) {
            rhs[[from_idx]] <- c(rhs[[from_idx]], list(
                .negate_expr(rate, simplify_product = identical(model$transports$type[[j]], "linear"))
            ))
        }
        if (!is.na(to)) rhs[[to_idx]] <- c(rhs[[to_idx]], list(rate))
    }

    volume_by_cmt <- setNames(model$compartments$volume, names(model$compartments))
    for (j in seq_along(model$reactions)) {
        scale_cmt <- model$reactions$scale_cmt[[j]]
        vol <- volume_by_cmt[[scale_cmt]]
        rate_expr <- model$reactions$rate[[j]]
        participants <- as.data.frame(model$reactions$participants[[j]])

        for (i in seq_len(nrow(participants))) {
            target <- .ode_model_reaction_state(
                participants$molec[[i]],
                participants$cmt[[i]],
                name2idx
            )
            term <- rate_expr
            stoich <- participants$stoich[[i]]
            if (!identical(stoich, 1) && !identical(stoich, 1L)) {
                term <- .mul(term, stoich)
            }
            if (identical(target$type, "amount")) {
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
                term <- .mul(term, .as_call(vol))
            }
            term <- lower(term)
            if (identical(participants$role[[i]], "input")) {
                term <- .negate_expr(term, simplify_product = identical(model$reactions$type[[j]], "elementary"))
            }
            rhs[[target$idx]] <- c(rhs[[target$idx]], list(term))
        }
    }

    rhs <- lapply(rhs, .sum_exprs)
    lowered_equations <- lapply(model$equations, lower) |> structure(class = "Equations")
    lowered_observables <- lapply(model$observables, lower) |> structure(class = "Observables")
    dosing <- .ode_model_dosing(model, name2idx)

    ode_model <- structure(
        list(
            states = state_info,
            initials = unname(lapply(y0_dsl, lower)),
            rhs = rhs,
            equations = lowered_equations,
            observables = lowered_observables,
            parameters = model$parameters,
            dosing = dosing,
            freeParams = .ode_model_free_params(
                c(unname(lapply(y0_dsl, lower)), rhs, unclass(lowered_equations), unclass(lowered_observables),
                  unname(as.list(dosing$time)), unname(as.list(dosing$value))),
                eq_names = names(lowered_equations),
                param_names = names(model$parameters)
            )
        ),
        class = "OdeModel"
    )

    ode_model
}

#' @export
to_ode_model.ProcessModel <- function(model) {
    .check_class(model, "ProcessModel")

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

#' Export an ODE model to deSolve format
#'
#' @param model An `OdeModel` object.
#' @param parameters Runtime parameter overrides.
#' @param dimensions Named list of unit dimensions.
#' @returns A list suitable for [deSolve::ode()].
#' @export
to_deSolve <- function(model, parameters = list(), dimensions = NULL) {
    UseMethod("to_deSolve")
}

#' Print method for `OdeModel` class
#'
#' Pretty-prints an `OdeModel` object using DSL state names.
#' @param x An `OdeModel` object.
#' @param ... ignored
#' @returns The `OdeModel` object (invisibly).
#' @export
print.OdeModel <- function(x, ...) {
    .check_class(x, "OdeModel")

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

#' @export
to_deSolve.OdeModel <- function(model, parameters = list(), dimensions = NULL) {
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
    funs <- vars[vapply(vars, exists, logical(1), mode = "function", inherits = TRUE)]
    sort(setdiff(vars, c(eq_names, param_names, reserved, funs)))
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
