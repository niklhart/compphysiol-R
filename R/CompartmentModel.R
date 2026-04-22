#' Create a new `CompartmentModel` object.
#' 
#' A compartment model consists of compartments, flows between compartments, equations defining auxiliary variables, 
#' observables defined as functions of the states and parameters, parameters, and dosing events. 
#' This function initializes an empty model that can then be built up by adding these components using the `add_*` functions.
#' 
#' @returns A new `CompartmentModel` object.
#' @seealso [add_compartment()], [add_molecule()], [add_transport()], [add_reaction()], [add_equation()], 
#'   [add_observable()], [add_parameter()], [add_dosing()] for building up a `CompartmentModel` object, 
#'   and [to_ode()], [to_analytical()] for exporting a `CompartmentModel` to ODE or analytical solution format.
#' @export
compartment_model <- function() {

    structure(
        list(
            compartments = compartments(),
            molecules = molecules(),
            transports = transports(),
            reactions = reactions(),
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

#' Print method for `CompartmentModel` class
#' 
#' Pretty-prints a `CompartmentModel` object.
#' @param x A `CompartmentModel` object.
#' @param ... ignored
#' @returns The `CompartmentModel` object (invisibly).
#' @export
print.CompartmentModel <- function(x, ...) {
    cat("CompartmentModel:\n")

    print(x$compartments)
    print(x$molecules)
    print(x$transports)
    print(x$reactions)
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

# ------------------------------------ `to_*` functions for CompartmentModel exporting ------------------------------------

#' Generate analytical solution function from a linear `CompartmentModel` object, possibly with a single bolus dose at time 0.
#' 
#' For linear compartment models, the system of ODEs can be solved analytically using matrix exponentials. 
#' This function generates a state function that evaluates this analytical solution at given time points and parameter values.
#' 
#' @param model A `CompartmentModel` object.
#' @param paramValues Named list of parameter values to inline in ODEs.
#' @returns A length 2 list named `state` (a function) and `observable`
#' (a list of functions, possibly empty). `state(t,param)` calculates the ODE
#' solution at time `t` for free parameters `param`, while `observable[[i]](t,param)`
#' calculates the `i`-th observable defined in the CompartmentModel.
#' @examples
#' M <- multiCompModel(ncomp = 2, type = "micro")
#' sol <- to_analytical(M, paramValues = list(kc0 = 0.05))  # fix one param
#'
#' # Evaluate ODE state at t = 5 with free params
#' sol$statefun(5, params = list(kcp = 0.2, kpc = 0.1))
#' @export
to_analytical <- function(model, paramValues = list()) {

    .check_class(model, "CompartmentModel")

    stateNames <- names(model$compartments)
    nStates <- length(stateNames)
    name2idx <- setNames(seq_along(stateNames), stateNames)

    eqNames <- names(model$equations)
    
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
            eqNames,
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
#' 
#' This function converts a `CompartmentModel` object into a format suitable for numerical ODE solvers, 
#' such as those in the `deSolve` package.
#' 
#' If the model specification uses units, the `dimensions` argument can be used to specify the unit dimensions
#' for time, mass, length, amount, etc., which will be used to inline the parameters in the ODEs with consistent units.
#' The dimensions default to SI base units and will be appended to the output list for reference, 
#' but the user can specify custom dimensions (e.g., time in hours instead of seconds) if desired.
#' 
#' Any variable that is neither defined as a parameter nor as a state variable will be treated as a free parameter 
#' and included in the `freeParams` output. These parameters need to be passed to the ODE solver as a vector.
#' They can be used for simulation or estimation purposes.
#' 
#' @param model A `CompartmentModel` object
#' @param dimensions Named list of unit dimensions used for inlining parameters in ODEs (default: SI units)
#' @param backend Character scalar specifying the ODE solver backend (default: "deSolve") for which the output should be optimized. 
#'   This argument is currently ignored, but may be extended in the future.
#' @returns A list with elements `odefun` (function), `y0` (named numeric vector), `obsFuncs` (list of functions),
#' `freeParams` (character vector) and `dimensions` (named list).
#' @examples
#' M <- multiCompModel(ncomp = 2, type = "micro", unit = "mg") |>
#'    add_parameter(k10 = 0.05 [1/h]) |> # fix one param
#'    add_dosing(target = "cen", time = 0 [h], amount = 100 [mg])
#' odeinfo <- to_ode(M, dimensions = list(time = "h"))
#' @export
to_ode <- function(
    model,
    dimensions = NULL,
    backend = "deSolve"
) {
    compNames <- names(model$compartments)
    stateNames <- names(model$compartments) # TODO: should become states(model$compartments) once the distinction is finalized
    eqNames <- names(model$equations)
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
            eqNames,
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
        from <- model$flows$from[[j]]
        to <- model$flows$to[[j]]
        if (!is.na(from)) {
            idx <- name2idx[[from]]
            rhs[[idx]] <- c(rhs[[idx]], paste0("-(", expr_str, ")"))
        }
        if (!is.na(to)) {
            idx <- name2idx[[to]]
            rhs[[idx]] <- c(rhs[[idx]], paste0("+(", expr_str, ")"))
        }
    }

    # Build ODE function body (explicit, human-readable)
    lines <- "function(t,y,params) {"
    for (i in seq_along(model$equations)) {
        eq_rhs <- model$equations[[i]]
        eq_nm <- names(model$equations)[i]
        eq_expr <- makeFun(eq_rhs)
        eq_str <- paste(deparse(eq_expr, width.cutoff = 500), collapse = " ")
        lines <- c(lines, paste0("    ", eq_nm, " <- ", eq_str))
        if (i == length(model$equations)) lines <- c(lines, "")
    }
    lines <- c(lines, paste0("    dydt <- numeric(", length(stateNames), ")"))
    for (i in seq_along(stateNames)) {
        if (length(rhs[[i]]) == 0) {
            lines <- c(lines, paste0("    dydt[", i, "] <- 0"))
        } else {
            lines <- c(lines, paste0("    dydt[", i, "] <- ", paste(rhs[[i]], collapse = " ")))
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
    y0 <- model$compartments$initial |>
        lapply(function(x) do.call(.to_dimensions, c(list(x), dimensions))) |>
        vapply(function(x) units::set_units(x, NULL), numeric(1)) |> 
        setNames(stateNames)

    # Dosing events table in output units
    events <- .dosing_to_events(model)
    if (inherits(events$data$value, "units")) {
        events$data$value <- events$data$value |>
            lapply(function(x) do.call(.to_dimensions, c(list(x), dimensions))) |>
            vapply(function(x) units::set_units(x, NULL), numeric(1))
    }
    if (inherits(events$data$time, "units")) {
         events$data$time <- events$data$time |>
            lapply(function(x) do.call(.to_dimensions, c(list(x), dimensions))) |>
            vapply(function(x) units::set_units(x, NULL), numeric(1))
    }

    # Output list
    list(
        odefun = odefun,
        stateNames = stateNames,
        obsFuncs = obsFuncs,
        freeParams = sort(unique(freeParams$list)),
        y0 = y0,
        events = events
    )
}

# ------------------------------------ Non-exported helper functions for CompartmentModel processing ------------------------------------

#' Linearity check for CompartmentModel object.
#' 
#' Checks if all transports in the model are linear with respect to the state variables.
#' 
#' @param model A `CompartmentModel` object.
#' @return `TRUE` if all transports are linear, `FALSE` otherwise.
#' @noRd
.is_linear <- function(model) all(model$transports$type == "linear")

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
