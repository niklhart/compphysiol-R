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

#' Construct one or more Observable objects
#'
#' `ObservableList()` is a wrapper function to construct multiple `Observable` objects at once.
#' It accepts vectorized arguments of identical length.
#'
#' @param name Observable name(s), character scalar or vector
#' @param expr Observable expression(s), (lists of) expressions or functions, or character scalar or vector
#' @return A list of `Observable` objects.
#' @examples
#' ObservableList(c("Cpla","Cblo"), expr = c("Central/Vcentral/BP", "Central/Vcentral"))
#'
#' @export
ObservableList <- function(name, expr) {
    if (!is.vector(expr)) expr <- list(expr)
    if (length(name) != length(expr)) stop("Arguments 'name' and 'expr' must have the same length.")
    
    Map(Observable$new, name, expr, USE.NAMES = FALSE)
}