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
            doses = dosing()
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
    print(x$doses)

    invisible(x)
}

#' Wire wildcard molecules and / or compartments into a compartment model
#' 
#' The `wire()` function allows users to replace wildcard molecule and compartment names 
#' in transport and reaction definitions with actual names from the model. 
#' A fully wired model is safe from any potentially unintended side effects when extending the model.
#' `wire()` is also called before ODE generation to replace any remaining wildcards with the full set of molecule or compartment names.
#' 
#' If molecules / compartments have been defined in the model, then any transport or reaction that references the wildcard name 
#' "<all molec>" / "<all cmt>" will be updated to reference all defined molecules / compartments, respectively 
#' (note that this may increase the number of molecules, transports and reactions in the model).
#' If no compartments or molecules have been defined, then a single dummy name ("molec" or "cmt") will be used 
#' to replace the wildcard, with a message printed to the console to inform the user. 
#' In addition, rate expressions and observables that include statements of the form `[molec]` or `[cmt]` will be 
#' expanded to include the implicit molecule and compartment names (i.e., into the form `[molec,cmt]`).
#' 
#' @param model A `CompartmentModel` object
#' @param what What to wire: `"molec"` for molecules, `"cmt"` for compartments, or both (the default)
#' @examples
#' # wire "drug" into transport rate expressions, replacing the <all molec> wildcard
#' M <- compartment_model() |>
#'   add_compartment(name = c("cen", "per")) |>
#'   add_molecule(name = "drug") |>
#'   add_transport(from = "cen", to = "per", const = "kcp") |> 
#'   add_transport(from = "per", to = "cen", const = "kpc") |> 
#'   wire(what = "molec")
#' 
#' # wire "cyt" and "nuc" into molecule definitions and reaction rate expressions, replacing the <all cmt> wildcard
#' M <- compartment_model() |>
#'   add_compartment(name = c("cyt", "nuc")) |>
#'   add_molecule(name = c("A","B")) |>
#'   add_reaction(input = "A", output = "B", const = "kAB") |>
#'   wire(what = "cmt")
#' @export
wire <- function(model, what = c("molec", "cmt")) {
        .check_class(model, "CompartmentModel")
    
        what <- match.arg(what, several.ok = TRUE)

        if (length(what) == 0) return(model)

        if ("molec" %in% what) {

            # if no molecules defined, wire to a single dummy molecule "molec" and print message
            if (length(model$molecules) == 0) {
                message(
                    "No molecules defined in model: wiring '<all molec>' to a single dummy molecule 'molec'."
                )
                model$molecules <- molecules(name = "molec")
            }
            molec_names <- names(model$molecules)
            nmolec <- length(model$molecules)

            # process all transports
            model$transports <- model$transports |>
                as.list() |>
                lapply(function(tr) {
                    if (is.na(tr$molec)) {
                        tr$molec <- molec_names
                        tr$rate <- I(lapply(molec_names, function(molec) {
                            .add_expr_index(tr$rate, pos = 1, val = molec)
                        }))
                        tr$const <- I(rep(list(tr$const), nmolec))
                    }
                    return(tr)
                }) |>
                lapply(do.call, what = "data.frame") |>
                lapply(structure, class = "Transports") |>
                do.call(what = "c")  %||% transports()
        }
    
        if ("cmt" %in% what) {

            # if no compartments defined, wire to a single dummy compartment "cmt" and print message
            if (length(model$compartments) == 0) {
                message("No compartments defined in model: wiring '<all cmt>' to a single dummy compartment 'cmt'.")
                model$compartments <- compartments(name = "cmt", volume = NA_real_)
            }
            cmt_names <- names(model$compartments)
            ncmt <- length(model$compartments)

            # process all molecules
            model$molecules <- model$molecules |>
                as.list() |>
                lapply(function(m) {
                    if (is.na(m$cmt)) {
                        m$cmt <- cmt_names
                        m$init <- I(rep(m$init, ncmt))
                    }
                    return(m)
                }) |>
                lapply(do.call, what = "data.frame") |>
                lapply(structure, class = "Molecules") |>
                do.call(what = "c")

            # process all reactions
            model$reactions <- model$reactions |>
                as.list() |>
                lapply(function(m) {
                    if (is.na(m$cmt)) {
                        m$cmt <- cmt_names
                        m$input <- I(rep(m$input, ncmt))
                        m$output <- I(rep(m$output, ncmt))
                        m$rate <- I(lapply(cmt_names, function(cmt) {
                            .add_expr_index(m$rate[[1]], pos = 2, val = cmt)
                        }))
                        m$const <- I(rep(m$const, ncmt))}
                    return(m)
                }) |>
                lapply(do.call, what = "data.frame") |>
                lapply(structure, class = "Reactions") |>
                do.call(what = "c") %||% reactions()
        }
    
        model
}

#' Extract the initial states (with names) from a `CompartmentModel` object
#'
#  The type of the returned object depends on the presence of units in the initial conditions:
#
#  - if all compartment initial conditions are numeric without units, a numeric vector is returned
#  - if all compartment initial conditions have consistent units, the returned vector will be of class `units`,
#  - if the compartment initial conditions have mixed units,  the returned vector will be of class `mixed_units`.
#' @param model A `CompartmentModel` object
#' @param type The type of initials to extract: the default `"a[] or c[]"` means amounts are attempted first, 
#'   but concentrations given if amounts cannot be calculated. `"c[] or a[]"` does the opposite, 
#'   while `"a[] only"` and `"c[] only"` will return an error if the respective type cannot be calculated.
#'   If all compartments have volumes (fixed or parametrized), then both types of initials can be calculated 
#'   and the `type` argument only determines which one is returned. For compartments that have no defined volume, 
#'   only the type of initial specified in the molecule definition can be calculated.
#' @returns A named numeric vector of state initial values, where the names are in the format
#'   `"a[molec,cmt]"` for amount states or `"c[molec,cmt]"` for concentration states.
#' @export
initials <- function(model, type = c("a[] or c[]", "c[] or a[]", "a[] only", "c[] only")) {
    # input checking
    .check_class(model, "CompartmentModel")
    type <- match.arg(type)

    # molec / cmt names
    molec_nm <- names(model$molecules)
    molec_cmt <- model$molecules$cmt
    if (any(is.na(molec_cmt))) stop("Cannot extract initials: some molecules have undefined compartments. Please wire the model first to resolve any wildcards.")

    # auxiliary quantity: compartment volume per state
    state_vol <- model$compartments$volume[match(
        molec_cmt,
        names(model$compartments)
    )]
    value_vol <- vapply(
        state_vol,
        function(v) is.numeric(v),
        FUN.VALUE = logical(1)
    )
    state_vol[!value_vol] <- list(NA_real_) # if volume is not numeric, set to NA for normalization     # TODO: query model$parameters for the respective value instead.

    # Allow mixed units in initial conditions if required by temporarily setting allow_mixed = TRUE
    oldopt <- units::units_options(allow_mixed = TRUE)
    on.exit(units::units_options(oldopt), add = TRUE)

    # flatten list of volumes/initial values into a vector
    vol <- do.call(what = c, args = state_vol) %||% numeric(0)
    init <- do.call(what = c, args = model$molecules$init) %||% numeric(0)

    molec_type <- model$molecules$type
    switch(
        type,
        "a[] or c[]" = {
            in_amount <- molec_type == "amount"
            out_amount <- in_amount | !is.na(vol)
            to_convert <- out_amount & !in_amount
            init <- Map(function(conv, x, v) if (conv) x * v else x, conv = to_convert, x = init, v = vol) |>
                do.call(what = c)
            prefix <- ifelse(out_amount, "a", "c")
            name <- paste0(prefix,"[", molec_nm, ",", molec_cmt, "]")
        },
        "c[] or a[]" = {
            in_conc <- molec_type == "concentration"
            out_conc <- in_conc | !is.na(vol)
            to_convert <- out_conc & !in_conc
            init <- Map(function(conv, x, v) if (conv) x / v else x, conv = to_convert, x = init, v = vol) |>
                do.call(what = c)
            prefix <- ifelse(out_conc, "c", "a")
            name <- paste0(prefix,"[", molec_nm, ",", molec_cmt, "]")
        },
        "a[] only" = {
            needs_convert <- molec_type == "concentration"
            cannot_convert <- is.na(vol) & needs_convert
            if (any(cannot_convert)) stop("Cannot extract amount initials for molecules #", paste(which(cannot_convert), collapse = ", "),".")
            init <- Map(function(conv, x, v) if (conv) x * v else x, conv = needs_convert, x = init, v = vol) |>
                do.call(what = c)
            name <- paste0("a[", molec_nm, ",", molec_cmt, "]")
        },
        "c[] only" = {
            needs_convert <- molec_type == "amount"
            cannot_convert <- is.na(vol) & needs_convert
            if (any(cannot_convert)) stop("Cannot extract concentration initials for molecules #", paste(which(cannot_convert), collapse = ", "),".")
            init <- Map(function(conv, x, v) if (conv) x / v else x, conv = needs_convert, x = init, v = vol) |>
                do.call(what = c)
            name <- paste0("c[", molec_nm, ",", molec_cmt, "]")
        }
    )

    setNames(init, nm = name)
}

#' Make auxiliary structures in a `CompartmentModel` object for handling continuous inputs
#' 
#' This function adds auxiliary compartments and events to the model to handle continuous inputs:
#' - `Depot_{molec}_{cmt}` and `ReleaseRate_{molec}_{cmt}` compartments 
#'   (both with `NA` volumes and for each molecule / target compartment)
#' - transports from the depot to its target compartment, at rate `a[ReleaseRate_{molec}_{cmt}]`
#' - 3 (instantaneous) dosing events per infusion (fill depot at infusion start, 
#'   increase release rate at infusion start, decrease release rate at infusion end)
#' @param model A `CompartmentModel` object
#' @returns An updated `CompartmentModel` object with the auxiliary compartments and events added.
make_depot <- function(model) {

    dose <- model$doses
    infus <- dose[is_infusion(dose)]

    # Early return if no infusion is defined
    if (length(infus) == 0) return(model)

    model$doses <- dose[!is_infusion(dose)]

    # --------------------------------------------------------------------------------------------------
    # Infusion dosing requires more complex handling: we need to add infusion bag and rate compartments,
    # convert the infusion dosing into bolus-to-bag + infusion rate events, and add flows from the bag
    # to the target compartment with rate equal to the infusion rate.
    # --------------------------------------------------------------------------------------------------

    # Convert continuous dosing into instantaneous-to-depot + release rate events, and add to model
    bag_names <- paste("Depot", infus$molec, infus$cmt, sep = "_")
    rate_names <- paste("ReleaseRate", infus$molec, infus$cmt, sep = "_")
    comp_names <- names(model$compartments)
    new_bag_cmt_names <- setdiff(bag_names, comp_names)
    new_rate_cmt_names <- setdiff(rate_names, comp_names)

    duplicates <- duplicated(bag_names)    # same for bag/rate names since they are generated from the same set of molecule/cmt names
    unique_bag_names <- bag_names[!duplicates]

    # Return the updated model (TODO: remove infusion dosing events!)
    model |>
        add_compartment(new_bag_cmt_names, volume = NA_real_) |>
        add_compartment(new_rate_cmt_names, volume = NA_real_) |>
        add_transport(
            from = bag_names[!duplicates],
            to = infus$cmt[!duplicates],
            rate = paste0("a[", rate_names[!duplicates], "]"),
            molec = infus$molec[!duplicates]
        ) |>
        add_dosing(
            time = infus$time,
            amount = infus$rate * infus$duration,
            cmt = bag_names,
            molec = infus$molec
        ) |>
        add_dosing(
            time = infus$time,
            amount = infus$rate,
            cmt = rate_names,
            molec = infus$molec
        ) |>
        add_dosing(
            time = infus$time + infus$duration,
            amount = -infus$rate,
            cmt = rate_names,
            molec = infus$molec
        ) 
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
                var = paste0("a[", doses$molec, ",", doses$cmt, "]"),
                time = doses$time,
                value = doses$amount,
                method = "add",
                stringsAsFactors = FALSE
            )
        )
    }

    # infusion rate events (TODO: merge with commented code below)
    if (length(model$infusionEvents) > 0) {
        events <- rbind(events, model$infusionEvents)
    }

    # # Helper function allowing to update the model in a single pipeline
    # add_infusion_events <- function(model, var, time, value, method) {
    #     new_events <- data.frame(
    #         var = var,
    #         time = time,
    #         value = value,
    #         method = method,
    #         stringsAsFactors = FALSE
    #     )
    #     model$infusionEvents <- rbind(model$infusionEvents, new_events)
    #     return(model)
    # }


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

    # Available variables for unit checking: compartment volumes, initial amounts/concentrations, and parameters
    varlist <-  c(
        setNames(model$compartments$volume, nm = states(model$compartments)), 
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
