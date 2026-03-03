#' Create a new CompartmentModel object.
#' @returns A new `CompartmentModel` object.
#' @export
compartment_model <- function() {

    structure(
        list(
            compartments = compartments(),
            flows = empty_flow(),
            equations = empty_equation(),
            observables = empty_observable(),
            doses = empty_dosing(),
            infusionEvents = data.frame(
                var = character(),
                time = numeric(),
                value = numeric(),
                method = character(),
                stringsAsFactors = FALSE
            )
        ),
        class = "CompartmentModel"
    )
}

#' Print method for CompartmentModel class
#' Pretty-prints a `CompartmentModel` object.
#' @param x A `CompartmentModel` object.
#' @param ... ignored
#' @returns The `CompartmentModel` object (invisibly).
#' @export
print.CompartmentModel <- function(x, ...) {
    cat("<CompartmentModel>\n")

    print(x$compartments)
    print(x$flows)
    print(x$equations)
    print(x$observables)

    # dosing (boluses + infusions unified) 
    # 
    # TODO: Leverage the print method of the dosing object instead of re-implementing this here. 
    #       This requires to rethink the interface: model$doses currently contains only partial dosing information 
    #       (bolus dosing and bolus into infusion bag), while changes of infusion rate are stored in model$infusionEvents
    #       and handled separately in the toODE method.
    # 
    # print(.dosing_to_events(x)$data) # -> ugly

    events <- .dosing_to_events(x)$data
    if (nrow(events) > 0) {
        cat(" Dosing events:\n")
        for (i in seq_len(nrow(events))) {
            ev <- events[i, ]
            cat(
                "   - at t =",
                ev$time,
                ":",
                ev$method,
                ev$value,
                "\u2192",
                ev$var,
                "\n"
            )
        }
    } else {
        cat(" Dosing: (none)\n")
    }

    invisible(x)
}

# ------------------------------------ `add_*` functions for CompartmentModel composition in a pipeline ------------------------------------

#' Add one or several compartments to a `CompartmentModel` object.
#'
#' @param model A `CompartmentModel` object.
#' @param name Name of the compartment(s)
#' @param initial Initial amount(s) (default 0)
#' @param unit Optional units for the initial amount(s) (e.g., `"mg"`, the default `NULL` means unitless)
#'   If provided, must be either a single unit applied to all compartments or a vector of units matching the length of `name`.
#' @param state Optional state name(s) for the compartment(s)  (character scalar or vector, default = `"A" + name`).
#' @param comp A `Compartments` object. Constructed from the other inputs if not provided.
#' @export
add_compartment <- function(
    model,
    name,
    initial = 0,
    unit = NULL,
    state = paste0("A", name, recycle0 = TRUE),
    comp = compartments(name, initial, unit, state)
) {
    .check_class(model, "CompartmentModel")
    .check_class(comp, "Compartments")
    model$compartments <- c(model$compartments, comp)
    model
}

#' Add one or several flows to a `CompartmentModel` object.
#'
#' @param model A `CompartmentModel` object.
#' @param from Source compartment
#' @param to Target compartment
#' @param ... Unused, enforces `rate` / `const` / `flow` to be specified as named arguments only, not positional
#' @param rate Optional flow rate expression as character
#' @param const Optional rate constant name (for linear flows)
#' @param flow A `Flows` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' model <- compartment_model() |>
#'     add_flow(from = "A", to = "B", const = "k1")
#' @export
add_flow <- function(model, from, to, ..., rate = NULL, const = NULL, flow = flows(from, to, rate = rate, const = const)) {
    .check_class(model, "CompartmentModel")
    .check_class(flow, "Flows")
    model$flows <- c(model$flows, flow)
    return(model)
}


#' Add an observable to a `CompartmentModel` object.
#'
#' @param model A `CompartmentModel` object.
#' @param ... Name-expression pairs for observables, e.g. `Cblo = blo / Vblo`
#' @param obs An `Observables` object.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(Cblo = blo/Vblo)
#' ## Programmatic path with Observables object
#' obs <- observables(name = "Cblo", expr = "blo/Vblo")
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(obs = obs)
#' @export
add_observable <- function(model, ..., obs = NULL) {
    .check_class(model, "CompartmentModel")
    if (!is.null(obs)) {
        # programmatic path
        .check_class(obs, "Observables")
    } else {
        # interactive path
        dots <- as.list(substitute(list(...)))[-1]
        obs <- observables(
            name = names(dots),
            expr = unname(dots)
        )
    }
    model$observables <- c(model$observables, obs)
    return(model)
}

#' Add one or several equations to a `CompartmentModel` object.
#' @param model A `CompartmentModel` object.
#' @param ... Name-expression pairs for equations, e.g. `co = Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski`
#' @param eq An `Equations` object.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_equation(co = Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski)
#' ## Programmatic path with Equations object
#' eq <- equations(name = "co", expr = "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski")
#' compartment_model() |>
#'     add_equation(eq = eq)
#' @export
add_equation <- function(model, ..., eq = NULL) {
    .check_class(model, "CompartmentModel")
    if (!is.null(eq)) {
        # programmatic path
        .check_class(eq, "Equations")
    } else {
        # interactive path
        dots <- as.list(substitute(list(...)))[-1]
        eq <- equations(
            name = names(dots),
            expr = unname(dots)
        )
    }
    model$equations <- c(model$equations, eq)
    return(model)
}

#' Add one or several parameters to a `CompartmentModel` object.
#' Parameters can be added interactively as name-value pairs (potentially with units), or programmatically as a `Parameters` object.
#' @param model A `CompartmentModel` object.
#' @param ... Parameter values as name-value pairs, where values can optionally have units (e.g., `A = 2 [m]`).
#' @param name Optional parameter names (if not using named arguments).
#' @param value Optional parameter values (if not using named arguments).
#' @param unit Optional parameter units (if not using named arguments).
#' @param param A `Parameters` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @export
add_parameter <- function(
    model, ..., name = NULL, value = NULL, unit = NULL, 
    param = parameters(..., name = name, value = value, unit = unit)
) {
    .check_class(model, "CompartmentModel")
    .check_class(param, "Parameters")
    model$parameters <- c(model$parameters, param)
    return(model)
}

#' Add one or several dosing events (bolus or infusion).
#'
#' This function allows the user to specify dosing events for a compartment model.
#' Dosing events can be either bolus (instantaneous) or infusion (continuous over time).
#'
#' The function handles the necessary modifications to the model structure for infusion dosing,
#' such as adding infusion bag and rate compartments, and creating the appropriate events for
#' starting and stopping the infusion.
#'
#' TODO: Currently there is an infinite loop caused by the pipeline of `add_dosing` calling `add_dosing`
#' when processing infusion dosing. This is a temporary issue that will be resolved in the next iteration
#' of the code refactor, where the internal handling of infusion dosing will be separated from the user-facing
#' `add_dosing` function.
#'
#' @param model A `CompartmentModel` object.
#' @param target Name of the target compartment(s) for the dose(s)
#' @param time Time of the dosing event(s)
#' @param amount Amount(s) to be dosed (for bolus or infusion)
#' @param ... Unused, enforces `rate` / `duration` / `dose` to be specified as named arguments only, not positional
#' @param rate Infusion rate (for infusion)
#' @param duration Infusion duration (for infusion)
#' @param dose A `Dosing` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' model <- compartment_model() |>
#'     add_dosing(target = "ven", time = 0, amount = 100, duration = 5)
#' @export
add_dosing <- function(
    model,
    target,
    time,
    amount = NULL,
    ...,
    rate = NULL,
    duration = NULL,
    dose = dosing(
        target = target,
        time = time,
        amount = amount,
        rate = rate,
        duration = duration
    )
) {
    .check_class(model, "CompartmentModel")
    .check_class(dose, "Dosing")

    # Separate bolus and infusion dosing for different handling
    bolus <- dose[is_bolus(dose)]
    infus <- dose[is_infusion(dose)]

    # Bolus dosing is simply appended to the models dosing list
    model$doses <- c(model$doses, bolus)

    # Early return if no infusion dosing
    if (length(infus) == 0) {
        return(model)
    }

    # --------------------------------------------------------------------------------------------------
    # Infusion dosing requires more complex handling: we need to add infusion bag and rate compartments,
    # convert the infusion dosing into bolus-to-bag + infusion rate events, and add flows from the bag
    # to the target compartment with rate equal to the infusion rate.
    # --------------------------------------------------------------------------------------------------

    # Convert infusion dosing into bolus-to-bag + infusion rate events, and add to model
    bag_names <- paste0("InfusionBag_", infus$target)
    rate_names <- paste0("InfusionRate_", infus$target)
    comp_names <- names(model$compartments)
    new_bag_names <- setdiff(bag_names, comp_names)
    new_rate_names <- setdiff(rate_names, comp_names)

    # Helper function allowing to update the model in a single pipeline
    add_infusion_events <- function(model, var, time, value, method) {
        new_events <- data.frame(
            var = var,
            time = time,
            value = value,
            method = method,
            stringsAsFactors = FALSE
        )
        model$infusionEvents <- rbind(model$infusionEvents, new_events)
        return(model)
    }

    # Return the updated model
    model |>
        add_compartment(new_bag_names, 0) |>
        add_compartment(new_rate_names, 0) |>
        add_dosing(
            target = bag_names,
            time = infus$time,
            amount = infus$rate * infus$duration
        ) |>
        add_flow(
            from = bag_names,
            to = infus$target,
            rate = rate_names
        ) |>
        add_infusion_events(
            var = rate_names,
            time = infus$time,
            value = infus$rate,
            method = "add"
        ) |>
        add_infusion_events(
            var = rate_names,
            time = infus$time + infus$duration,
            value = -infus$rate,
            method = "add"
        )
}

# ------------------------------------ `to_*` functions for CompartmentModel exporting ------------------------------------

#' Generate analytical solution function from a linear `CompartmentModel` object with a single bolus dose at time 0.
#' @param model A `CompartmentModel` object.
#' @param paramValues Named list of parameter values to inline in ODEs.
#' @returns A length 2 list named `state` (a function) and `observable`
#' (a list of functions, possibly empty). `state(t,param)` calculates the ODE
#' solution at time `t` for free parameters `param`, while `observable[[i]](t,param)`
#' calculates the `i`-th observable defined in the CompartmentModel.
#' @examples
#' M <- multiCompModel(ncomp = 2, type = "micro")
#' sol <- to_analytical(M, paramValues = list(k10 = 0.05))  # fix one param
#'
#' # Evaluate ODE state at t = 5 with free params
#' sol$statefun(5, params = list(k12 = 0.2, k21 = 0.1))
#' @export
to_analytical <- function(model, paramValues = list()) {

    .check_class(model, "CompartmentModel")

    stateNames <- names(model$compartments)
    nStates <- length(stateNames)
    name2idx <- setNames(seq_along(stateNames), stateNames)

    # Initialize symbolic system matrix
    A <- matrix("0", nStates, nStates)
    rownames(A) <- stateNames
    colnames(A) <- stateNames

    # Track free parameters
    freeParams <- new.env(parent = emptyenv())
    freeParams$list <- character()

    # ---- Process each flow ----
    if (any(model$flows$type == "nonlinear")) {
        stop("Model contains nonlinear flows: cannot compute analytical solution.")
    }

    for (i in seq_along(model$flows)) {
        coef_ast <- model$flows$const[[i]]
        coef_str <- paste(deparse(coef_ast, width.cutoff = 500), collapse = " ")

        # Inline paramValues numerically
        if (length(paramValues) > 0) {
            for (nm in names(paramValues)) {
                coef_str <- gsub(
                    paste0("\\b", nm, "\\b"),
                    as.character(paramValues[[nm]]),
                    coef_str
                )
            }
        }

        # Convert remaining symbols to params[["name"]] and collect freeParams
        coef_symbols <- all.vars(parse(text = coef_str))
        coef_symbols <- setdiff(coef_symbols, names(paramValues))
        coef_symbols <- setdiff(coef_symbols, stateNames)
        freeParams$list <- c(freeParams$list, coef_symbols)
        for (s in coef_symbols) {
            coef_str <- gsub(
                paste0("\\b", s, "\\b"),
                paste0('params[["', s, '"]]'),
                coef_str
            )
        }

        from <- model$flows$from[[i]]
        to <- model$flows$to[[i]]

        from_idx <- name2idx[[from]]
        to_idx <- if (!is.na(to)) name2idx[[to]] else NA

        # Diagonal contribution
        if (A[from_idx, from_idx] == "0") {
            A[from_idx, from_idx] <- paste0("-", "(", coef_str, ")")
        } else {
            A[from_idx, from_idx] <- paste0(
                A[from_idx, from_idx],
                "-(",
                coef_str,
                ")"
            )
        }

        # Off-diagonal contribution
        if (!is.na(to_idx) && to_idx != from_idx) {
            if (A[to_idx, from_idx] == "0") {
                A[to_idx, from_idx] <- paste0("+(", coef_str, ")")
            } else {
                A[to_idx, from_idx] <- paste0(
                    A[to_idx, from_idx],
                    "+(",
                    coef_str,
                    ")"
                )
            }
        }
    }

    # Replace empty entries with "0"
    A[A == ""] <- "0"

    # ---- Construct vectorized statefun using matrix exponential ----
    statefun <- function(t, params = list()) {
        eval_env <- as.list(params)
        A_eval <- matrix(0, nStates, nStates)
        for (i in seq_len(nStates)) {
            for (j in seq_len(nStates)) {
                A_eval[i, j] <- eval(
                    parse(text = A[i, j]),
                    envir = eval_env
                )
            }
        }
        x0 <- initials(model$compartments)
        res <- as.matrix(vapply(
            t,
            function(tt) expm::expm(A_eval * tt) %*% x0,
            x0
        ))
        if (length(x0) > 1) {
            res <- t(res)
        }
        colnames(res) <- stateNames
        # Prepend t as the first column, as deSolve does
        cbind(time = t, res)
    }

    # Observables (same substitution logic)
    obsFuncs <- lapply(model$observables, function(o) {
        expr_lang <- substitute_expr(
            o,
            stateNames,
            name2idx,
            paramValues = paramValues,
            freeParamsEnv = freeParams,
            obsFunc = TRUE
        )
        expr_str <- paste(
            deparse(expr_lang, width.cutoff = 500),
            collapse = " "
        )
        eval(parse(text = paste0("function(t,y,params) ", expr_str)))
    })
    names(obsFuncs) <- names(model$observables)

    # ---- Output ----
    list(
        statefun = statefun,
        stateNames = stateNames,
        freeParams = sort(unique(freeParams$list)),
        obsFuncs = obsFuncs,
        A = A
    )
}

#' Generate ODE function, initial values, observables, and free parameters from a `CompartmentModel` object.
#' @param model A `CompartmentModel` object.
#' @param paramValues Named list of parameter values to inline in ODEs.
#' @returns A list with elements `odefun` (function), `y0` (named numeric vector), `obsFuncs` (list of functions), 
#' and `freeParams` (character vector).
#' @examples
#' M <- multiCompModel(ncomp = 2, type = "micro")
#' odeinfo <- to_ode(M, paramValues = list(k10 = 0.05))
#' @export
to_ode <- function(model, paramValues = list()) {
    stateNames <- names(model$compartments)
    name2idx <- setNames(seq_along(stateNames), stateNames)

    # ---- Validation: check that all flows point to known compartments ----
    check_comp <- function(nm) {
        if (!is.null(nm) && any(!(nm %in% stateNames))) {
            missing <- nm[!(nm %in% stateNames)]
            stop(
                "Flow references unknown compartment: ",
                paste(missing, collapse = ", "),
                ". ",
                "Compartment names in this model: ",
                paste(stateNames, collapse = ", "),
                ". ",
                "Did you mean to merge this model with another?"
            )
        }
    }

    flow_comps <- setdiff(unique(c(model$flows$from, model$flows$to)), NA_character_)
    lapply(flow_comps, check_comp)

    # Environment container for free parameters
    freeParams <- new.env(parent = emptyenv())
    freeParams$list <- character()

    makeFun <- function(expr, obsFunc = FALSE) {
        substitute_expr(
            expr,
            stateNames,
            name2idx,
            paramValues = paramValues,
            freeParamsEnv = freeParams,
            obsFunc = obsFunc
        )
    }

    # Collect RHS terms for ODEs
    rhs <- vector("list", length(stateNames))
    for (j in seq_along(model$flows)) {
        expr <- makeFun(model$flows$rate[[j]])
        expr_str <- deparse(expr, width.cutoff = 500) |>
            paste(collapse = " ")
        if (!is.na(model$flows$from[[j]])) {
            for (from in model$flows$from[[j]]) {
                idx <- name2idx[[from]]
                rhs[[idx]] <- c(rhs[[idx]], paste0("-(", expr_str, ")"))
            }
        }
        if (!is.na(model$flows$to[[j]])) {
            for (to in model$flows$to[[j]]) {
                idx <- name2idx[[to]]
                rhs[[idx]] <- c(rhs[[idx]], paste0("+(", expr_str, ")"))
            }
        }
    }

    # Build ODE function body (explicit, human-readable)
    lines <- c(
        "function(t,y,params) {",
        paste0("    dydt <- numeric(", length(stateNames), ")")
    )
    for (i in seq_along(stateNames)) {
        if (length(rhs[[i]]) == 0) {
            lines <- c(lines, paste0("    dydt[", i, "] <- 0"))
        } else {
            lines <- c(
                lines,
                paste0(
                    "    dydt[",
                    i,
                    "] <- ",
                    paste(rhs[[i]], collapse = " ")
                )
            )
        }
    }
    lines <- c(lines, "    list(dydt)", "}")
    odefun <- eval(parse(text = paste(lines, collapse = "\n")))

    # Observables (same substitution logic)
    obsFuncs <- lapply(model$observables, function(o) {
        expr_lang <- makeFun(o, obsFunc = TRUE)
        expr_str <- paste(
            deparse(expr_lang, width.cutoff = 500),
            collapse = " "
        )
        eval(parse(text = paste0("function(t,y,params) ", expr_str)))
    })
    names(obsFuncs) <- names(model$observables)

    # Output list
    list(
        odefun = odefun,
        stateNames = stateNames,
        obsFuncs = obsFuncs,
        freeParams = sort(unique(freeParams$list)),
        y0 = initials(model$compartments, named = TRUE),
        events = .dosing_to_events(model)
    )
}

# ------------------------------------ Non-exported helper functions for CompartmentModel processing ------------------------------------

#' Linearity check for CompartmentModel object.
#' 
#' Checks if all flows in the model are linear with respect to the state variables.
#' 
#' @param model A `CompartmentModel` object.
#' @return `TRUE` if all flows are linear, `FALSE` otherwise.
#' @noRd
.is_linear <- function(model) all(model$flows$type == "linear")

#' Generate events data.frame for `deSolve` from stored dosing.
#'
#' This unifies bolus and infusion dosing into a single events data.frame, which is then used in
#' the `toODE` method to generate the final events list for `deSolve`.
#' @param model A `CompartmentModel` object.
#' @returns A list with a single element `data`, which is a data.frame with columns `var`, `time`, `value`, and `method` (add or replace).
.dosing_to_events <- function(model) {
    .check_class(model, "CompartmentModel")

    events <- data.frame(
        var = character(),
        time = numeric(),
        value = numeric(),
        method = character(),
        stringsAsFactors = FALSE
    )

    # boluses
    if (length(model$doses) > 0) {
        doses <- model$doses
        events <- rbind(
            events,
            data.frame(
                var = doses$target,
                time = doses$time,
                value = doses$amount,
                method = "add",
                stringsAsFactors = FALSE
            )
        )
    }

    # infusion rate events
    if (length(model$infusionEvents) > 0) {
        events <- rbind(events, model$infusionEvents)
    }

    # sort by time
    events <- events[order(events$time), ]
    list(data = events)
}

