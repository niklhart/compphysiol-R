#' Create a new CompartmentModel object.
#' @returns A new `CompartmentModel` object.
#' @export
compartment_model <- function() {

    structure(
        list(
            compartments = compartments(),
            flows = flows(),
            equations = equations(),
            observables = observables(),
            parameters = parameters(),
            doses = dosing(),
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
    print(x$parameters)

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
                format(ev$time),
                ":",
                ev$method,
                format(ev$value),
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
#' @param model A `CompartmentModel` object.
#' @inheritParams compartments
#' @param comp A `Compartments` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @export
add_compartment <- function(
    model,
    name = character(0),
    initial = 0,
    unit = NULL,
    state = paste0("A", name, recycle0 = TRUE),
    comp
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    comp <- .forward_or_use(
        object_arg_name = "comp",
        constructor_name = "compartments",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(comp, "Compartments")

    model$compartments <- c(model$compartments, comp)
    model
}

#' Add one or several flows to a `CompartmentModel` object.
#'
#' @param model A `CompartmentModel` object.
#' @inheritParams flows
#' @param flow A `Flows` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' model <- compartment_model() |>
#'     add_flow(from = "A", to = "B", const = "k1")
#' @export
add_flow <- function(
    model,
    from,
    to,
    ...,
    rate = NULL,
    const = NULL,
    flow
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    flow <- .forward_or_use(
        object_arg_name = "flow",
        constructor_name = "flows",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(flow, "Flows")

    model$flows <- c(model$flows, flow)
    return(model)
}

#' Add an observable to a `CompartmentModel` object.
#'
#' @inherit observables description details
#' 
#' @param model A `CompartmentModel` object.
#' @inheritParams observables
#' @param obs An `Observables` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(Cblo = Ablo / Vblo)
#' ## Programmatic path with name and expression vectors
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(name = "Cblo", expr = "Ablo/Vblo")
#' ## Programmatic path with Observables object
#' obs <- observables(name = "Cblo", expr = "Ablo/Vblo")
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(obs = obs)
#' @export
add_observable <- function(model, ..., name = character(0), expr = character(0), obs) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    obs <- .forward_or_use(
        object_arg_name = "obs",
        constructor_name = "observables",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(obs, "Observables")

    model$observables <- c(model$observables, obs)
    return(model)
}

#' Add one or several equations to a `CompartmentModel` object.
#' 
#' @inherit equations description details
#' 
#' @param model A `CompartmentModel` object.
#' @inheritParams equations
#' @param eq An `Equations` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_equation(co = Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski)
#' ## Programmatic path with name and expression vectors
#' compartment_model() |>
#'     add_equation(name = "co", expr = "Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski")
#' ## Programmatic path with Equations object
#' eq <- equations(name = "co", expr = "Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski")
#' compartment_model() |>
#'     add_equation(eq = eq)
#' @export
add_equation <- function(model, ..., name = character(0), expr = character(0), eq) {
    .check_class(model, "CompartmentModel")
    
    call <- match.call()

    eq <- .forward_or_use(
        object_arg_name = "eq",
        constructor_name = "equations",
        call = call,
        parent_env = parent.frame()
    )
    
    .check_class(eq, "Equations")

    model$equations <- c(model$equations, eq)
    return(model)
}

#' Add one or several parameters to a `CompartmentModel` object.
#' Parameters can be added interactively as name-value pairs (potentially with units), or programmatically as a `Parameters` object.
#' @param model A `CompartmentModel` object.
#' @inheritParams parameters
#' @param param A `Parameters` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @export
add_parameter <- function(
    model,
    ...,
    name = NULL,
    value = NULL,
    unit = NULL,
    param
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    param <- .forward_or_use(
        object_arg_name = "param",
        constructor_name = "parameters",
        call = call,
        parent_env = parent.frame()
    )

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
#' @param model A `CompartmentModel` object.
#' @inheritParams dosing
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
    time_unit = NULL,
    amount_unit = NULL,
    ...,
    rate = NULL,
    duration = NULL,
    dose
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    dose <- .forward_or_use(
        object_arg_name = "dose",
        constructor_name = "dosing",
        call = call,
        parent_env = parent.frame()
    )

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

#' Generate ODE function, initial values, observables, and free parameters from a `CompartmentModel` object
#' @param model A `CompartmentModel` object
#' @param dimensions Named list of unit dimensions used for inlining parameters in ODEs (default: SI units)
#' @returns A list with elements `odefun` (function), `y0` (named numeric vector), `obsFuncs` (list of functions),
#' and `freeParams` (character vector).
#' @examples
#' M <- multiCompModel(ncomp = 2, type = "micro", unit = "mg") |>
#'    add_parameter(k10 = 0.05 [1/h])  # fix one param
#' odeinfo <- to_ode(M, dimensions = list(time = "h"))
#' @export
to_ode <- function(
    model,
    dimensions = NULL
) {
    compNames <- names(model$compartments)
    stateNames <- names(model$compartments) # TODO: should become states(model$compartments) once the distinction is finalized
    name2idx <- setNames(seq_along(stateNames), stateNames)

    paramValues <- lapply(model$parameters, function(p) do.call(.to_dimensions, c(list(p), dimensions))) # TODO: look up dimensions in global dimensions list instead of passing as argument?
    paramValues <- vapply(paramValues, function(x) units::set_units(x,NULL), numeric(1))

    # ---- Validation: check that all flows point to known compartments ----
    check_comp <- function(nm) {
        if (!is.null(nm) && any(!(nm %in% compNames))) {
            missing <- nm[!(nm %in% compNames)]
            stop(
                "Flow references unknown compartment: ",
                paste(missing, collapse = ", "),
                ". ",
                "Compartment names in this model: ",
                paste(compNames, collapse = ", "),
                ". ",
                "Did you mean to merge this model with another?"
            )
        }
    }

    flow_comps <- setdiff(
        unique(c(model$flows$from, model$flows$to)),
        NA_character_
    )
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

    # Initial values in output units
    # y0 <- initials(model$compartments, named = TRUE) # OLD (no unit handling)
    y0 <- model$compartments$initial |>
        lapply(function(x) do.call(.to_dimensions, c(list(x), dimensions))) |>
        vapply(function(x) units::set_units(x, NULL), numeric(1)) |> 
        setNames(stateNames)

    # Output list
    list(
        odefun = odefun,
        stateNames = stateNames,
        obsFuncs = obsFuncs,
        freeParams = sort(unique(freeParams$list)),
        y0 = y0,
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

#' Unit consistency check for CompartmentModel object.
#' 
#' Checks that all flows, observables, and equations in the model are dimensionally consistent with respect to the units of the compartments, parameters and dosing.
#' 
#' @param model A `CompartmentModel` object.
#' @return `TRUE` if all units are consistent, otherwise an error is raised.
#' @noRd
.check_unit_consistency <- function(model) {
    .check_class(model, "CompartmentModel")

    # Available variables for unit checking: compartment initials and parameters
    varlist <-  c(
        setNames(model$compartments$initial, nm = states(model$compartments)), 
        unclass(model$parameters)
    )
    varnames <- names(varlist)

    # Check dosing units against compartment units
    for (i in seq_along(model$doses)) {
        amt <- model$doses$amount[[i]]
        tar <- model$doses$target[[i]]
        tar_value <- initials(model$compartments[tar])
        isunit_amt <- inherits(amt, "units")
        isunit_tar <- inherits(tar_value, "units")
        if (isunit_amt || isunit_tar) {
            if (!(isunit_amt && isunit_tar) ) stop(
                sprintf("In dosing (%s), inconsistent units for dosing amount and target compartment '%s': one has units while the other does not.", i, tar)
            )
            if (!units::ud_are_convertible(units(amt), units(tar_value))) stop(
                sprintf(
                    "In dosing (%s), inconsistent units for dosing amount and target compartment '%s': %s vs. %s",
                    i, tar, units(amt), units(tar_value)
                )
            )
        }
    }

    # Check if flow rates are unit consistent
    parnames <- names(model$parameters)
    for (i in seq_along(model$flows)) {
        rate <- model$flows$rate[[i]]
        from <- model$flows$from[[i]]
        to <- model$flows$to[[i]]
        type <- model$flows$type[[i]]

        # Check that 'from' and 'to' compartments have compatible units (if both are defined)
        from_val <- if (!is.na(from)) initials(model$compartments[from]) else NULL
        to_val <- if (!is.na(to)) initials(model$compartments[to]) else NULL
        if (!is.null(from_val) &&  !is.null(to_val)) {

            isunit_from <- inherits(from_val, "units")
            isunit_to <- inherits(to_val, "units")
            if (!isunit_from && !isunit_to) next
            if (isunit_from != isunit_to) stop(
                sprintf("In flow (%d), inconsistent units for 'from' and 'to' compartments: one has units while the other does not.", i)
            )
            units::ud_are_convertible(units(from_val), units(to_val)) || stop(
                sprintf(
                    "In flow (%d), inconsistent units for 'from' and 'to' compartments: %s vs. %s",
                    i, units(from_val), units(to_val)
                )
            )
        }

        if (type == "linear") {

            # Check that all parameters in the rate constant are defined in the model, warn if not (we cannot check units in this case)
            const <- model$flows$const[[i]]
            if (!all(all.vars(const) %in% parnames)) {
                warning("Cannot check units for flow (",i,"): some parameter(s) in the rate constant are not defined in the model.")
                next
            }
            # Evaluate the rate constant with the parameter values to check that it has units of 1/time (if parameters have units)
            const_val <- tryCatch(
                eval(const, envir = as.list(model$parameters)), 
                error = function(e) {
                    stop(sprintf("In flow (%s), unit inconsistency in rate constant expression: %s", i, e$message))
                }
            )
            if (!inherits(const_val, "units")) next
            units::ud_are_convertible(units(const_val), "1/h") || stop(
                sprintf("In flow (%d), unit of rate constant (%s) must be type 1/Time.", i, units(const_val))
            )
        } else { # type == "nonlinear"

            # Check that all parameters in the rate expression are defined in the model, warn if not (we cannot check units in this case)
            rate <- model$flows$rate[[i]]
            if (!all(all.vars(rate) %in% varnames)) {
                warning("Cannot check units for flow (", i, "): some parameter(s) in the rate expression are not defined in the model.")
                next
            }
            # Evaluate the rate expression with all variables to check that it has units of [cmt unit]/time
            rate_val <- tryCatch(
                eval(rate, envir = varlist),
                error = function(e) {
                    stop(sprintf("In flow (%s), unit inconsistency in rate expression: %s", i, e$message))
                }
            )
            if (!inherits(rate_val, "units")) next
            one_h <- units::set_units(1, "h")
            expected <- c(from_val, to_val) / one_h
            units::ud_are_convertible(units(rate_val), units(expected)) || stop(
                sprintf(
                    "In flow (%d), unit inconsistency of rate expression (%s) must be compatible with unit of compartment per time (%s)",
                    i, units(rate_val), units(expected)
                )
            )
        }
    }

    # Check that observables definitions are valid in terms of compartment and parameter units (if units are involved)
    obsnames <- names(model$observables)
    for (i in seq_along(model$observables)) {
        obs_expr <- model$observables[[i]]
        if (!all(all.vars(obs_expr) %in% varnames)) {
            warning("Cannot check units for observable '", obsnames[[i]], "': some parameter(s) in the observable are not defined in the model.")
            next
        }
        # Evaluate the observable with the compartment initial amounts and parameter values to check that it can be evaluated without unit errors
        tryCatch(
            eval(obs_expr, envir = varlist), 
            error = function(e) {
                stop(sprintf("In observable '%s', unit inconsistency in expression: %s", obsnames[[i]], e$message))
            }
        )
    }

    # Check that equation definitions are valid in terms of compartment and parameter units (if units are involved)
    eqnames <- names(model$equations)
    for (i in seq_along(model$equations)) {
        eq_expr <- model$equations[[i]]
        if (!all(all.vars(eq_expr) %in% varnames)) {
            warning("Cannot check units for equation '", eqnames[[i]], "': some parameter(s) in the equation are not defined in the model.")
            next
        }
        # Evaluate the equation with the compartment initial amounts and parameter values to check that it can be evaluated without unit errors
        tryCatch(
            eval(eq_expr, envir = varlist), 
            error = function(e) {
                stop(sprintf("In equation '%s', unit inconsistency in expression: %s", eqnames[[i]], e$message))
            }
        )
    }
}
