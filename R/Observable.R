#' Observable Class
#'
#' Represents a model observable
#'
#' @export
Observable <- R6::R6Class("Observable",
                      public = list(
                          name = NULL,
                          expr = NULL,   # string or function(t, y, params)
                          initialize = function(name, expr) {
                              self$name <- name
                              self$expr <- .as_call(expr)
                          },
                          print = function(...) {
                              cat(sprintf("Observable: %s = %s\n",
                                          self$name, deparse(self$expr)))
                          }
                      )
)
