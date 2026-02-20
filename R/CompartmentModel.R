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
        addCompartment = function(name, initial = 0, comp = CompartmentList(name, initial)) {
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
