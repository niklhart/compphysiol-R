#' Reaction Class
#'
#' Represents a model reaction.
#'
#' @export
Reaction <- R6::R6Class("Reaction",
    public = list(
        from = NULL,
        to   = NULL,
        rate = NULL,   # stored as an expression
        initialize = function(from, to, rate) {
            self$from <- from
            self$to <- to
            self$rate <- private$as_expression(rate)
        },
        print = function(...) {
            cat(sprintf("Reaction: %s -> %s, rate = %s\n",
                        self$from, self$to,
                        deparse(self$rate)))
        }
    ),
    private = list(
        as_expression = function(rate) {
            if (is.character(rate)) {
                # parse string into an expression
                parsed <- parse(text = rate)
                if (length(parsed) != 1L)
                    stop("Rate must be a single expression.")
                parsed[[1]]
            } else if (is.expression(rate)) {
                rate[[1]]
            } else if (is.language(rate)) { # quote() returns a call
                rate
            } else {
                stop("Rate must be a character, expression, or quoted call.")
            }
        }
    )
)



Reaction$set("public", "isLinear", function(stateNames) {
    expr <- self$rate

    # Recursively collect state symbols in the expression
    collectStates <- function(e) {
        if (is.symbol(e)) return(as.character(e))
        if (is.call(e)) return(unlist(lapply(as.list(e[-1]), collectStates)))
        character(0)
    }

    states_in_expr <- intersect(collectStates(expr), stateNames)
    # Linear if exactly one state appears and it is r$from
    length(states_in_expr) == 1 && states_in_expr == self$from
})

Reaction$set("public", "rateConstant", function(stateNames) {
    if (!self$isLinear(stateNames)) return(NA_character_)

    expr <- self$rate
    src_state <- self$from

    # Recursive function to find the coefficient
    extractCoef <- function(e) {
        # If the node is the source state symbol → replace with 1
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
})
