#' Compartment Class
#'
#' Represents a model compartment with a name and an initial amount.
#'
#' @export
Compartment <- R6::R6Class("Compartment",
                           public = list(
                               name = NULL,
                               initial = 0,
                               initialize = function(name, initial = 0) {
                                   self$name <- name
                                   self$initial <- initial
                               }
                           )
)
