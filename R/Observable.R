#' Observable Class
#'
#' Represents a model observable
#'
#' @export
Observable <- R6::R6Class("Observable",                  
    public = list(
        #' @field name Name of the observable
        name = NULL,
        #' @field expr Expression (as an R call)
        expr = NULL,
        #' @description
        #' Initialize a new `Observable` object.
        #' @param name Name of the observable
        #' @param expr Expression (character or function)
        #' @return A new `Observable` object
        initialize = function(name, expr) {
            self$name <- name
            self$expr <- .as_call(expr)
        },
        #' @description
        #' Print the observable
        #' @param ... Additional arguments (not used)
        #' @return The `Observable` object (invisible)
        print = function(...) {
            cat(sprintf("Observable: %s = %s\n",
                        self$name, deparse(self$expr)))
            invisible(self)
        }
    )
)
