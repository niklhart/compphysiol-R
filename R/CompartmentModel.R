# New CompartmentModel class, implemented using S3. To be fully migrated from the old R6 version.

#' Create a new CompartmentModel object.
#' @return A new `CompartmentModel` object.
#' @export
compartment_model <- function() {

    structure(
        list(
            compartments = compartments(name = NULL, initial = NULL),
            flows = list(),
            equations = list(),
            observables = list(),
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
#' @return The `CompartmentModel` object (invisibly).
#' @export
print.CompartmentModel <- function(x, ...) {
    cat("<CompartmentModel>\n")

    # compartments
    print(x$compartments)

    # flows
    if (length(x$flows) > 0) {
        cat(" Flows:\n")
        for (f in x$flows) {
            from <- if (is.null(f$from)) {
                "\u2205"
            } else {
                paste(f$from, collapse = "+")
            }
            to <- if (is.null(f$to)) {
                "\u2205"
            } else {
                paste(f$to, collapse = "+")
            }
            cat("   -", from, "\u2192", to, ":", deparse(f$rate), "\n")
        }
    } else {
        cat(" Flows: (none)\n")
    }

    # observables
    if (length(x$observables) > 0) {
        cat(" Observables:\n")
        for (o in x$observables) {
            cat("   -", o$name, "=", deparse(o$expr), "\n")
        }
    } else {
        cat(" Observables: (none)\n")
    }

    # dosing (boluses + infusions unified)
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


#' Add one or several compartments to a `CompartmentModel` object.
#' 
#' @param model A `CompartmentModel` object.
#' @param name Name of the compartment(s)
#' @param initial Initial amount(s) (default 0)
#' @param comp A `Compartments` object. Constructed from the other inputs if not provided.
#' @export
add_compartment <- function(model, name, initial = 0, comp = compartments(name, initial)) {
    .check_class(model, "CompartmentModel")
    model$compartments <- c(model$compartments, comp)
    model
}

#' Get initial states of a `CompartmentModel` as a named or unnamed vector.
#' @param model A `CompartmentModel` object.
#' @param named A boolean, should the initial states be named (default: `TRUE`)?
#' @returns A named or unnamed numeric vector of initial states, in the same order as `compartment_names(model)`.
#' @export
initials <- function(model, named = TRUE) {
    .check_class(model, "CompartmentModel")
    y0 <- model$compartments$initial
    if (named) {
        setNames(y0, nm = compartment_names(model))
    } else {
        y0
    }
}

#' Get compartment names of a `CompartmentModel`.
#' @param model A `CompartmentModel` object.
#' @returns A character vector of compartment names.
#' @export
compartment_names <- function(model) {
    .check_class(model, "CompartmentModel")
    names(model$compartments)
}


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
    if (!is.null(model$doses)) {
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
    if (!is.null(model$infusionEvents)) {
        events <- rbind(events, model$infusionEvents)
    }

    # sort by time
    events <- events[order(events$time), ]
    list(data = events)
}

#' Add one or several flows to a `CompartmentModel` object.
#'
#' @param from Source compartment
#' @param to Target compartment
#' @param rate Optional flow rate expression as character
#' @param const Optional rate constant name (for linear flows)
#' @param flow A Reaction object or list of Reaction objects. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' model <- compartment_model() |>
#'     add_flow(from = "A", to = "B", const = "k1")
#' @export
add_flow <- function(model, from, to, rate = NULL, const = NULL, flow = NULL) {
    .check_class(model, "CompartmentModel")

    # Early return if flow(s) are provided directly (programmatic path)
    if (!is.null(flow)) {
        flow <- .wrap_into_list(flow)
        model$flows <- c(model$flows, flow)
        return(model)
    }

    # Input lengths
    nFrom <- length(from)
    nTo <- length(to)
    nRate <- length(rate)
    nConst <- length(const)

    # Check that all inputs are either NULL, scalar or vector of the same length
    nMax <- max(nFrom, nTo)
    if (!all(c(nFrom, nTo, nRate, nConst) %in% c(0, 1, nMax))) {
        stop(
            "All inputs must be either NULL, scalar, or vector of the same length."
        )
    }

    # Check that if rate is provided, const is not provided and vice versa
    if (!xor(is.null(rate), is.null(const))) {
        stop("Exactly one of 'rate' or 'const' must be provided.")
    }
    type <- if (!is.null(rate)) "rate" else "const"

    from <- rep(from, nMax / nFrom)
    to <- rep(to, nMax / nTo)

    # If rate/const is scalar and one of from/to is vector, apply special substitution rule in rate/const
    replace_pattern <- function(x) {
        Map(
            function(f_, t_) {
                x |>
                    gsub(pattern = "_from", replacement = f_) |>
                    gsub(pattern = "_to", replacement = t_)
            },
            f_ = from,
            t_ = to,
            USE.NAMES = FALSE
        )
    }

    # Vectorized construction of flows
    flow <- switch(
        type,
        rate = Map(
            Reaction$new,
            from = from,
            to = to,
            rate = if (nRate == 1 && nMax > 1) replace_pattern(rate) else rate,
            USE.NAMES = FALSE
        ),
        const = Map(
            Reaction$new,
            from = from,
            to = to,
            const = if (nConst == 1 && nMax > 1) replace_pattern(const) else const,
            USE.NAMES = FALSE
        )
    )

    model$flows <- c(model$flows, flow)
    return(model)
}

#' Add an observable to a `CompartmentModel` object.
#'
#' @param model A `CompartmentModel` object.
#' @param name Name of the observable
#' @param expr Expression (character or function)
#' @param obs An Observable object or list of Observable objects. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' model <- compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(name = "Cblo", expr = "blo/Vblo")
#' @export
add_observable <- function(model, name, expr, obs = ObservableList(name, expr)) {
    .check_class(model, "CompartmentModel")
    obs <- .wrap_into_list(obs)
    model$observables <- c(model$observables, obs)
    return(model)
}

#' Add one or several equations to a `CompartmentModel` object.
#' @param eq An Equation object or list of Equation objects. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' model <- compartment_model() |>
#'     add_equation("co = Qadi+Qbon+Qgut+Qhea+Qkid+Qliv+Qmus+Qski+Qspl")
#' @export
add_equation <- function(model, eq) {
    .check_class(model, "CompartmentModel")
    eq <- .wrap_into_list(eq)
    model$equations <- c(model$equations, eq)
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
    comp_names <- compartment_names(model)
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

#' Linearity check for CompartmentModel object.
#' 
#' Checks if all flows in the model are linear with respect to the state variables.
#' 
#' @param model A `CompartmentModel` object.
#' @return `TRUE` if all flows are linear, `FALSE` otherwise.
#' @noRd
.is_linear <- function(model) {
    stateNames <- compartment_names(model)
    all(vapply(
        model$reactions,
        function(r) r$isLinear(stateNames),
        logical(1)
    ))
}


#' Generate analytical solution function from a linear `CompartmentModel` object with a single bolus dose at time 0.
#' @param model A `CompartmentModel` object.
#' @param paramValues Named list of parameter values to inline in ODEs.
#' @returns A length 2 list named `state` (a function) and `observable`
#' (a list of functions, possibly empty). `state(t,param)` calculates the ODE
#' solution at time `t` for free parameters `param`, while `observable[[i]](t,param)`
#' calculates the `i`-th observable defined in the CompartmentModel.
#' @examples
#' M <- multiCompModel(ncomp = 2, type = "micro")
#' sol <- M$toAnalytical(paramValues = list(k10 = 0.05))  # fix one param
#'
#' # Evaluate ODE state at t = 5 with free params
#' sol$statefun(5, params = list(k12 = 0.2, k21 = 0.1))
#' @export
to_analytical <- function(model, paramValues = list()) {

    .check_class(model, "CompartmentModel")

    stateNames <- compartment_names(model)
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
    for (f in model$flows) {
        if (!f$isLinear(stateNames)) {
            stop(
                "Flow is nonlinear: cannot compute analytical solution."
            )
        }

        coef_str <- f$rateConstant(stateNames)

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

        from_idx <- name2idx[[f$from]]
        to_idx <- if (!is.null(f$to) && f$to != "") {
            name2idx[[f$to]]
        } else {
            NA
        }

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
        x0 <- initials(model)
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
            o$expr,
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
    names(obsFuncs) <- vapply(model$observables, function(o) o$name, "")

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
#' M <- multi_comp_model(ncomp = 2, type = "micro")
#' odeinfo <- to_ode(M, paramValues = list(k10 = 0.05))
#' 
to_ode <- function(model, paramValues = list()) {
    stateNames <- compartment_names(model)
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
    for (f in model$flows) {
        check_comp(f$from)
        check_comp(f$to)
    }

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
        f <- model$flows[[j]]
        expr <- makeFun(f$rate)
        expr_str <- deparse(expr, width.cutoff = 500) |>
            paste(collapse = " ")
        if (!is.null(f$from)) {
            for (from in f$from) {
                idx <- name2idx[[from]]
                rhs[[idx]] <- c(rhs[[idx]], paste0("-(", expr_str, ")"))
            }
        }
        if (!is.null(f$to)) {
            for (to in f$to) {
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
        expr_lang <- makeFun(o$expr, obsFunc = TRUE)
        expr_str <- paste(
            deparse(expr_lang, width.cutoff = 500),
            collapse = " "
        )
        eval(parse(text = paste0("function(t,y,params) ", expr_str)))
    })
    names(obsFuncs) <- vapply(model$observables, function(o) o$name, "")

    # Output list
    list(
        odefun = odefun,
        stateNames = stateNames,
        obsFuncs = obsFuncs,
        freeParams = sort(unique(freeParams$list)),
        y0 = initials(model),
        events = .dosing_to_events(model)
    )
}




# Old CompartmentModel class, implemented using R6, for reference and testing purposes. Will be removed in the future.

#' CompartmentModel class
#' @description
#' Represents a compartmental model.
#' @details
#' `CompartmentModel` is an R6 class that encapsulates the structure of a compartmental model,
#' including its compartments, reactions, observables, and dosing events.
#' @export
CompartmentModel <- R6::R6Class(
    "CompartmentModel",
    public = list(
        #' @field compartments A list of `Compartment` objects
        compartments = list(),

        #' @field reactions A list of `Reaction` objects
        reactions = list(),

        #' @field equations A list of expressions representing aggregate parameters as functions of model parameters
        equations = list(),

        #' @field observables A list of `Observable` objects
        observables = list(),

        #' @field doses List of dosing objects. Includes bolus doses into model
        #'   compartments, as well as bolus-to-bag doses used to implement infusions.
        doses = NULL,

        #' @field infusionEvents Data frame of infusion rate adjustment events
        #'   (start and stop of zero-order input). Generated automatically when
        #'   infusions are defined.
        infusionEvents = NULL,

        #' @description
        #' Initialize a new `CompartmentModel` object.
        initialize = function() {
            self$compartments <- list()
            self$reactions <- list()
            self$observables <- list()
            self$doses <- list()
            self$infusionEvents <- data.frame(
                var = character(),
                time = numeric(),
                value = numeric(),
                method = character(),
                stringsAsFactors = FALSE
            )
        },

        #' Print method for CompartmentModel class
        #'
        #' Pretty-prints a `CompartmentModel` object.
        #'
        #' @param ... ignored
        #' @return The `CompartmentModel` object (invisibly).
        #' @examples
        #' M <- multiCompModel()
        #' print(M)
        #' @export
        print = function(...) {
            cat("<CompartmentModel>\n")

            # compartments
            if (length(self$compartments) > 0) {
                cat(" Compartments:\n")
                for (c in self$compartments) {
                    cat("   -", c$name, "(initial =", c$initial, ")\n")
                }
            } else {
                cat(" Compartments: (none)\n")
            }

            # reactions
            if (length(self$reactions) > 0) {
                cat(" Reactions:\n")
                for (r in self$reactions) {
                    from <- if (is.null(r$from)) {
                        "\u2205"
                    } else {
                        paste(r$from, collapse = "+")
                    }
                    to <- if (is.null(r$to)) {
                        "\u2205"
                    } else {
                        paste(r$to, collapse = "+")
                    }
                    cat("   -", from, "\u2192", to, ":", deparse(r$rate), "\n")
                }
            } else {
                cat(" Reactions: (none)\n")
            }

            # observables
            if (length(self$observables) > 0) {
                cat(" Observables:\n")
                for (o in self$observables) {
                    cat("   -", o$name, "=", deparse(o$expr), "\n")
                }
            } else {
                cat(" Observables: (none)\n")
            }

            # dosing (boluses + infusions unified)
            events <- private$dosing_to_events()$data
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

            invisible(self)
        },

        #' @description
        #' Get initial states for simulation as a named vector.
        #' @param named A boolean, should the initial states be named?
        getInitialState = function(named = TRUE) {
            y0 <- sapply(self$compartments, function(c) c$initial)
            if (named) {
                setNames(y0, nm = self$getStateNames())
            } else {
                y0
            }
        },

        #' @description
        #' Get compartment names.
        getStateNames = function() {
            sapply(self$compartments, function(c) c$name)
        },

        #' @description
        #' Add one or several compartments to the model.
        #' @param name Name of the compartment(s)
        #' @param initial Initial amount(s) (default 0)
        #' @param comp A Compartment object or list of Compartment objects. Constructed from the other inputs if not provided.
        addCompartment = function(
            name,
            initial = 0,
            comp = CompartmentList(name, initial)
        ) {
            comp <- .wrap_into_list(comp)
            self$compartments <- c(self$compartments, comp)
            invisible(self)
        },

        #' @description
        #' Add one or several reactions to the model.
        #' Currently, only the `reaction` argument can be vectorized.
        #' The other inputs can only be used to construct a scalar reaction.
        #' @param from Source compartment
        #' @param to Target compartment
        #' @param rate Rate expression as character
        #' @param const Optional rate constant name (for linear reactions)
        #' @param reaction A Reaction object or list of Reaction objects. Constructed from the other inputs if not provided.
        addReaction = function(
            from,
            to,
            rate = NULL,
            const = NULL,
            reaction = Reaction$new(from, to, rate, const)
        ) {
            reaction <- .wrap_into_list(reaction)
            self$reactions <- c(self$reactions, reaction)
            invisible(self)
        },

        #' @description
        #' Add one or several reactions to the model, test version (vectorized + substitution). The `rate` argument of `addReaction`
        #' is not supported currently.
        #' @param from Source compartment
        #' @param to Target compartment
        #' @param const Optional rate constant name (for linear reactions)
        #' @param reaction A Reaction object or list of Reaction objects. Constructed from the other inputs if not provided.
        addReaction2 = function(from, to, const) {
            # either all inputs are vectorized or one of from/to is vectorized and const is scalar
            nFrom <- length(from)
            nTo <- length(to)
            nConst <- length(const)

            # Check that all inputs are either scalar or vector of the same length
            nMax <- max(nFrom, nTo, nConst)
            if (!all(c(nFrom, nTo, nConst) %in% c(1, nMax))) {
                stop(
                    "All inputs must be either scalar or vector of the same length. ",
                    "Lengths: from = ",
                    nFrom,
                    ", to = ",
                    nTo,
                    ", const = ",
                    nConst
                )
            }

            from <- rep(from, nMax / nFrom)
            to <- rep(to, nMax / nTo)

            # If const is scalar and one of from/to is vector, apply special substitution rule in const
            if (nConst == 1 && nMax > 1) {
                const <- Map(
                    function(f_, t_) const |> 
                        gsub(pattern = "_from", replacement = f_) |> 
                        gsub(pattern = "_to", replacement = t_),
                    f_ = from,
                    t_ = to,
                    USE.NAMES = FALSE
                )
            }

            r <- Map(
                Reaction$new,
                from = from,
                to = to,
                const = const,
                USE.NAMES = FALSE
            )

            self$reactions <- c(self$reactions, r)
            return(invisible(self))
        },

        #' @description
        #' Add an observable.
        #' @param name Name of the observable
        #' @param expr Expression (character or function)
        #' @param obs An Observable object or list of Observable objects. Constructed from the other inputs if not provided.
        addObservable = function(name, expr, obs = ObservableList(name, expr)) {
            obs <- .wrap_into_list(obs)
            self$observables <- c(self$observables, obs)
            invisible(self)
        },

        #' @description
        #' Add one or several observables.
        #' @param ... Name and expression pairs for observables, e.g. `addObservable(C1Conc = C1 / V1)`
        #' @param obs An Observable object or list of Observable objects. Constructed from the other inputs if not provided.
        addObservable2 = function(..., obs = NULL) {
            if (!is.null(obs)) {
                # programmatic path
                obs <- .wrap_into_list(obs)
            } else {
                # interactive DSL path
                dots <- as.list(substitute(list(...)))[-1]
                obs <- ObservableList(
                    name = names(dots),
                    expr = unname(dots)
                )
            }
            self$observables <- c(self$observables, obs)
            invisible(self)
        },

        #' @description
        #' Add one or several equations.
        #' @param eq An Equation object or list of Equation objects. Constructed from the other inputs if not provided.
        addEquation = function(eq) {
            eq <- .wrap_into_list(eq)
            self$equations <- c(self$equations, eq)
            invisible(self)
        },

        #' @description
        #' Add one or several dosing events (bolus or infusion).
        #' @param target Name of the target compartment(s) for the dose(s)
        #' @param time Time of the dosing event(s)
        #' @param amount Amount(s) to be dosed (for bolus) or infusion rate (for infusion)
        #' @param rate Infusion rate (for infusion)
        #' @param duration Infusion duration (for infusion)
        #' @param dose A Dosing object or a list of Dosing objects. Constructed from the other inputs if not provided.
        addDosing = function(
            target,
            time,
            amount = NULL,
            rate = NULL,
            duration = NULL,
            dose = DosingList(
                target = target,
                time = time,
                amount = amount,
                rate = rate,
                duration = duration
            )
        ) {
            # --- handle lists of Dosing objects ---
            if (is.list(dose)) {
                for (d in dose) {
                    self$addDosing(dose = d) # recursion on single elements
                }
                return(invisible(self))
            }

            # --- Logic for single Dosing object ---
            stopifnot(inherits(dose, "Dosing"))
            if (dose$isBolus()) {
                # simple bolus, store in doses list
                if (is.null(self$doses)) {
                    self$doses <- list()
                }
                self$doses[[length(self$doses) + 1]] <- dose
            } else if (dose$isInfusion()) {
                # ensure bag and rate compartments exist for target
                bagName <- paste0("InfusionBag_", dose$target)
                rateName <- paste0("InfusionRate_", dose$target)
                if (!(bagName %in% names(self$compartments))) {
                    self$addCompartment(bagName, 0)
                }
                if (!(rateName %in% names(self$compartments))) {
                    self$addCompartment(rateName, 0)
                }

                # add zero-order reaction from bag to  target, rate = InfusionRate compartment
                self$addReaction(bagName, dose$target, paste0(rateName))

                # create bolus into the bag
                totalAmount <- dose$rate * dose$duration
                bolusToBag <- Dosing$new(
                    bagName,
                    amount = totalAmount,
                    time = dose$time
                )

                # store both bolus-to-bag and infusion start/end events
                if (is.null(self$doses)) {
                    self$doses <- list()
                }
                self$doses[[length(self$doses) + 1]] <- bolusToBag

                # infusion rate modification events
                if (is.null(self$infusionEvents)) {
                    self$infusionEvents <- data.frame(
                        var = character(),
                        time = numeric(),
                        value = numeric(),
                        method = character(),
                        stringsAsFactors = FALSE
                    )
                }
                # start
                self$infusionEvents <- rbind(
                    self$infusionEvents,
                    data.frame(
                        var = rateName,
                        time = dose$time,
                        value = dose$rate,
                        method = "add",
                        stringsAsFactors = FALSE
                    )
                )
                # end
                self$infusionEvents <- rbind(
                    self$infusionEvents,
                    data.frame(
                        var = rateName,
                        time = dose$time + dose$duration,
                        value = -dose$rate,
                        method = "add",
                        stringsAsFactors = FALSE
                    )
                )
            } else {
                stop(
                    "Invalid dosing: either bolus or infusion with rate+duration"
                )
            }

            invisible(self)
        },

        #' @description
        #' Linearity check for CompartmentModel object.
        #' @return `TRUE` if all reactions are linear, `FALSE` otherwise.
        isLinear = function() {
            stateNames <- self$getStateNames()
            all(vapply(
                self$reactions,
                function(r) r$isLinear(stateNames),
                logical(1)
            ))
        },

        #' @description
        #' Generate analytical solution function from a CompartmentModel object with
        #' linear reactions, up to 3 compartments and a single bolus dose at time 0.
        #' @param paramValues Named list of parameter values to inline in ODEs.
        #' @return A length 2 list named `state` (a function) and `observable`
        #' (a list of functions, possibly empty). `state(t,param)` calculates the ODE
        #' solution at time `t` for free parameters `param`, while `observable[[i]](t,param)`
        #' calculates the `i`-th observable defined in the CompartmentModel.
        #' @examples
        #' M <- multiCompModel(ncomp = 2, type = "micro")
        #' sol <- M$toAnalytical(paramValues = list(k10 = 0.05))  # fix one param
        #'
        #' # Evaluate ODE state at t = 5 with free params
        #' sol$statefun(5, params = list(k12 = 0.2, k21 = 0.1))
        toAnalytical = function(paramValues = list()) {
            stateNames <- self$getStateNames()
            nStates <- length(stateNames)
            name2idx <- setNames(seq_along(stateNames), stateNames)

            # Initialize symbolic system matrix
            A <- matrix("0", nStates, nStates)
            rownames(A) <- stateNames
            colnames(A) <- stateNames

            # Track free parameters
            freeParams <- new.env(parent = emptyenv())
            freeParams$list <- character()

            # ---- Process each reaction ----
            for (r in self$reactions) {
                if (!r$isLinear(stateNames)) {
                    stop(
                        "Reaction is nonlinear: cannot compute analytical solution."
                    )
                }

                coef_str <- r$rateConstant(stateNames)

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

                from_idx <- name2idx[[r$from]]
                to_idx <- if (!is.null(r$to) && r$to != "") {
                    name2idx[[r$to]]
                } else {
                    NA
                }

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
                x0 <- self$getInitialState()
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
            obsFuncs <- lapply(self$observables, function(o) {
                expr_lang <- substitute_expr(
                    o$expr,
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
            names(obsFuncs) <- vapply(self$observables, function(o) o$name, "")

            # ---- Output ----
            list(
                statefun = statefun,
                stateNames = stateNames,
                freeParams = sort(unique(freeParams$list)),
                obsFuncs = obsFuncs,
                A = A
            )
        },

        #' @description
        #' Generate ODE function, initial values, observables, and free parameters.
        #' @param paramValues Named list of parameter values to inline in ODEs.
        toODE = function(paramValues = list()) {
            stateNames <- self$getStateNames()
            name2idx <- setNames(seq_along(stateNames), stateNames)

            # ---- Validation: check that all reactions point to known compartments ----
            check_comp <- function(nm) {
                if (!is.null(nm) && any(!(nm %in% stateNames))) {
                    missing <- nm[!(nm %in% stateNames)]
                    stop(
                        "Reaction references unknown compartment: ",
                        paste(missing, collapse = ", "),
                        ". ",
                        "Compartment names in this model: ",
                        paste(stateNames, collapse = ", "),
                        ". ",
                        "Did you mean to merge this model with another?"
                    )
                }
            }
            for (r in self$reactions) {
                check_comp(r$from)
                check_comp(r$to)
            }

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
            for (j in seq_along(self$reactions)) {
                r <- self$reactions[[j]]
                expr <- makeFun(r$rate)
                expr_str <- deparse(expr, width.cutoff = 500) |>
                    paste(collapse = " ")
                if (!is.null(r$from)) {
                    for (from in r$from) {
                        idx <- name2idx[[from]]
                        rhs[[idx]] <- c(rhs[[idx]], paste0("-(", expr_str, ")"))
                    }
                }
                if (!is.null(r$to)) {
                    for (to in r$to) {
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
            obsFuncs <- lapply(self$observables, function(o) {
                expr_lang <- makeFun(o$expr, obsFunc = TRUE)
                expr_str <- paste(
                    deparse(expr_lang, width.cutoff = 500),
                    collapse = " "
                )
                eval(parse(text = paste0("function(t,y,params) ", expr_str)))
            })
            names(obsFuncs) <- vapply(self$observables, function(o) o$name, "")

            # Output list
            list(
                odefun = odefun,
                stateNames = stateNames,
                obsFuncs = obsFuncs,
                freeParams = sort(unique(freeParams$list)),
                y0 = self$getInitialState(),
                events = private$dosing_to_events()
            )
        }
    ),
    # ---- start of private methods ----
    private = list(
        # Generate events data.frame for `deSolve` from stored dosing.
        dosing_to_events = function() {
            events <- data.frame(
                var = character(),
                time = numeric(),
                value = numeric(),
                method = character(),
                stringsAsFactors = FALSE
            )

            # boluses
            if (!is.null(self$doses)) {
                for (d in self$doses) {
                    events <- rbind(
                        events,
                        data.frame(
                            var = d$target,
                            time = d$time,
                            value = d$amount,
                            method = "add",
                            stringsAsFactors = FALSE
                        )
                    )
                }
            }

            # infusion rate events
            if (!is.null(self$infusionEvents)) {
                events <- rbind(events, self$infusionEvents)
            }

            # sort by time
            events <- events[order(events$time), ]
            list(data = events)
        }
    )
)
