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
#' sol$state(5, params = list(k12 = 0.2, k21 = 0.1))
CompartmentModel$set("public", "toAnalytical", function(paramValues = list()) {
    stateNames <- self$getStateNames()
    nStates <- length(stateNames)
    name2idx <- setNames(seq_along(stateNames), stateNames)

    # Initialize symbolic system matrix
    A <- matrix("0", nStates, nStates)
    rownames(A) <- stateNames
    colnames(A) <- stateNames

    # Track free parameters
    freeParams <- character()

    # ---- Process each reaction ----
    for (r in self$reactions) {
        if (!r$isLinear(stateNames)) {
            stop("Reaction is nonlinear: cannot compute analytical solution.")
        }

        coef_str <- r$rateConstant(stateNames)

        # Inline paramValues numerically
        if (length(paramValues) > 0) {
            for (nm in names(paramValues)) {
                coef_str <- gsub(paste0("\\b", nm, "\\b"), as.character(paramValues[[nm]]), coef_str)
            }
        }

        # Convert remaining symbols to params[["name"]] and collect freeParams
        coef_symbols <- all.vars(parse(text = coef_str))
        coef_symbols <- setdiff(coef_symbols, names(paramValues))
        coef_symbols <- setdiff(coef_symbols, stateNames)
        freeParams <- c(freeParams, coef_symbols)
        for (s in coef_symbols) {
            coef_str <- gsub(paste0("\\b", s, "\\b"), paste0('params[["', s, '"]]'), coef_str)
        }

        from_idx <- name2idx[[r$from]]
        to_idx   <- if (!is.null(r$to) && r$to != "") name2idx[[r$to]] else NA

        # Diagonal contribution
        if (A[from_idx, from_idx] == "0") {
            A[from_idx, from_idx] <- paste0("-", "(", coef_str, ")")
        } else {
            A[from_idx, from_idx] <- paste0(A[from_idx, from_idx], "-(", coef_str, ")")
        }

        # Off-diagonal contribution
        if (!is.na(to_idx) && to_idx != from_idx) {
            if (A[to_idx, from_idx] == "0") {
                A[to_idx, from_idx] <- paste0("+(", coef_str, ")")
            } else {
                A[to_idx, from_idx] <- paste0(A[to_idx, from_idx], "+(", coef_str, ")")
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
                A_eval[i,j] <- eval(parse(text = A[i,j]), envir = eval_env)
            }
        }
        x0 <- self$getInitialState()
        res <- as.matrix(vapply(t, function(tt) expm::expm(A_eval * tt) %*% x0, x0))
        if (length(x0)>1) res <- t(res)
        colnames(res) <- stateNames
        # Prepend t as the first column, as deSolve does
        cbind(time = t, res)
    }

    # ---- Output ----
    list(
        statefun   = statefun,
        stateNames = stateNames,
        freeParams = sort(unique(freeParams)),
        A          = A
    )
})


