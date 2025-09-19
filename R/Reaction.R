#' Reaction Class
#'
#' Represents a model reaction.
#'
#' @export
Reaction <- R6::R6Class("Reaction",
                    public = list(
                        from = NULL,
                        to = NULL,
                        rate = NULL,   # string or function(t, y, params)
                        name = NULL,
                        initialize = function(from, to, rate, name = "") {
                            self$from <- from
                            self$to <- to
                            self$rate <- rate
                            self$name <- name
                        }
                    )
)
