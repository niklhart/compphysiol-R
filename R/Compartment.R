#' Compartment Class
#'
#' @description
#' Represents a model compartment with a name and an initial amount.
#'
#' @examples
#' cpt <- Compartment$new("Heart", 10)
#' cpt$name
#' cpt$initial
#'
#' @export
Compartment <- R6::R6Class("Compartment",
                           public = list(

                               #' @field name Character string giving the name of the compartment.
                               name = NULL,

                               #' @field initial Numeric value, the initial amount in the compartment.
                               initial = 0,

                               #' @description
                               #' Create a new compartment object.
                               #' @param name Name of the compartment (character)
                               #' @param initial Initial amount (numeric, default = 0)
                               #' @return A new `Compartment` object
                               initialize = function(name, initial = 0) {
                                   self$name <- name
                                   self$initial <- initial
                               },

                               #' @description
                               #' Print a compartment object to the console.
                               #' @param ... Additional arguments (not used)
                               #' @return The `Compartment` object (invisible)
                               print = function(...) {
                                   cat(sprintf("Compartment: %s (initial = %s)\n",
                                               self$name, self$initial))
                                   invisible(self)
                               }
                           )
)
