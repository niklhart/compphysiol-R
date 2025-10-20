#' Reaction Class
#'
#' Represents a model reaction.
#'
#' @export
Reaction <- R6::R6Class("Reaction",
    public = list(
        
        #' @field from Source compartment name (character scalar, or NULL for source)
        from = NULL,
        
        #' @field to Target compartment name (character scalar, or NULL for sink)
        to   = NULL,
        
        #' @field rate Rate expression (as an R call)
        rate = NULL,

        #' @description
        #' Initialize a new `Reaction` object.
        #' @param from Source compartment name (character scalar, or NULL for source)
        #' @param to Target compartment name (character scalar, or NULL for sink)
        #' @param rate Rate expression (character or function)
        #' @return A new `Reaction` object
        initialize = function(from, to, rate) {
            self$from <- if (.is_emptychar(from)) NULL else 
                            if (all(nzchar(from))) from else 
                            stop("'from' must be a non-empty character vector or NULL")
            self$to   <- if (.is_emptychar(to))   NULL else 
                            if (all(nzchar(to))) to else 
                            stop("'to' must be a non-empty character vector or NULL")
            self$rate <- .as_call(rate)
        },

        #' @description
        #' Print the reaction
        #' @param ... Additional arguments (not used)
        #' @return The `Reaction` object (invisible)
        print = function(...) {
            from <- if (is.null(self$from)) "\u2205" else paste(self$from, collapse = "+")
            to   <- if (is.null(self$to))   "\u2205" else paste(self$to, collapse = "+")

            cat(sprintf("Reaction: %s \u2192 %s, rate = %s\n",
                        from, to, deparse(self$rate)))
            invisible(self)
        },

        #' @description
        #' Check if the reaction is linear with respect to its source compartment.
        #' A reaction is linear if its rate expression is of the form `k * X`,
        #' where `k` is a constant (not depending on any compartment) and `X`
        #' is the source compartment.
        #' @param stateNames Character vector of valid compartment names
        #' @return `TRUE` if the reaction is linear, `FALSE` otherwise
        isLinear = function(stateNames) {
            src_state <- self$from
          
            # Early return if source state is not scalar
            if (length(src_state) != 1) return (FALSE)

            # Recursive function to check linearity
            isLinearRec <- function(e, mult_context = TRUE) {
                # Base case: symbol
                if (is.symbol(e)) {
                    nm <- as.character(e)
                    if (nm %in% stateNames) {
                        return(list(states = nm, ok = mult_context && nm == src_state))
                    } else {
                        return(list(states = character(0), ok = TRUE))
                    }
                }

                # If not a call, nothing to do
                if (!is.call(e))
                    return(list(states = character(0), ok = TRUE))

                fn <- as.character(e[[1]])
                args <- as.list(e[-1])

                # Parentheses are transparent — just recurse into inner expression
                if (fn == "(")
                    return(isLinearRec(args[[1]], mult_context))

                # Multiplicative operators keep the multiplicative context for subterms
                # Additive or nonlinear operators reset it
                next_context <- mult_context && fn %in% c("*", "/")

                res <- lapply(args, function(a) isLinearRec(a, mult_context = next_context))
                states <- unique(unlist(lapply(res, `[[`, "states")))
                ok <- all(vapply(res, `[[`, logical(1), "ok"))

                # Addition, subtraction, power or nonlinear calls destroy linearity if state appears
                if ((fn %in% c("+", "-", "^")) && any(states == src_state))
                    ok <- FALSE
                if (!(fn %in% c("+", "-", "*", "/", "(")) && any(states == src_state))
                    ok <- FALSE

                list(states = states, ok = ok)
            }

            res <- isLinearRec(e = self$rate)
            length(res$states) == 1 && res$states == src_state && res$ok
        },

        #' @description
        #' Extract the rate constant from a linear reaction.
        #' If the reaction is not linear, returns `NA`.
        #' @param stateNames Character vector of valid compartment names
        #' @return Character string representing the rate constant expression, or `NA`
        #' if the reaction is not linear.
        rateConstant = function(stateNames) {
            if (!self$isLinear(stateNames)) return(NA_character_)

            expr <- self$rate
            src_state <- self$from

            # Recursive function to find the coefficient
            extractCoef <- function(e) {
                # If the node is the source state symbol --> replace with 1
                if (is.symbol(e) && as.character(e) == src_state) return(1)

                # If it is a call, recurse into it, taking special care of "*"
                if (is.call(e)) {
                    if (e[[1]] == as.symbol("*")) {
                        args <- as.list(e[-1])
                        # Check if one of the arguments is the source state
                        is_state <- vapply(args, function(a) is.symbol(a) && as.character(a) == src_state, logical(1))
                        if (sum(is_state) == 1) {
                            # Coefficient is the other argument
                            return(args[!is_state][[1]])
                        }
                    }

                    # Recurse on all arguments
                    new_args <- lapply(as.list(e[-1]), extractCoef)

                    # Rebuild the call with the same function
                    return(as.call(c(e[[1]], new_args)))
                }

                # Otherwise, leave as is
                e
            }

            coef_ast <- extractCoef(expr)
            paste(deparse(coef_ast, width.cutoff = 500), collapse = " ")
        }
    )
)


