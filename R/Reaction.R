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
