#' Create an ODE model representation
#'
#' `to_ode_model()` lowers a `CompartmentModel` to a backend-neutral ODE
#' representation with indexed one-dimensional states.
#'
#' @param model A `CompartmentModel` object.
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

        if (!is.na(from)) rhs[[from_idx]] <- c(rhs[[from_idx]], list(.negate_expr(rate)))
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
            if (identical(participants$role[[i]], "input")) term <- .negate_expr(term)
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

    attr(ode_model, "auto_placeholder") <- model$metadata$auto_placeholder %||% list()
    ode_model
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

    auto_placeholder <- attr(model, "auto_placeholder") %||% list()
    output_state_names <- .dsl_state_to_name(
        model$states$dsl_name,
        omit_molec = isTRUE(auto_placeholder$molec),
        omit_cmt = isTRUE(auto_placeholder$cmt)
    )
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

.ode_model_state_info <- function(model, dsl_state_names) {
    parsed <- lapply(dsl_state_names, .dsl_parse_state)
    data.frame(
        index = seq_along(dsl_state_names),
        dsl_name = dsl_state_names,
        molec = vapply(parsed, `[[`, character(1), "molec"),
        cmt = vapply(parsed, `[[`, character(1), "cmt"),
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

.ode_model_lower_expr <- function(expr, state_names, eq_names, name2idx, state_volumes) {
    if (!is.character(expr) && !is.expression(expr) && !is.language(expr)) {
        return(expr)
    }
    expr <- .as_call(expr)

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

.negate_expr <- function(expr) bquote(-(.(expr)))

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
