#' `CompartmentModel` class
#'
#' Represents a compartmental model.
#'
#' @export
CompartmentModel <- R6::R6Class(
    "CompartmentModel",
    public = list(

        #' @field compartments A list of `Compartment` objects
        compartments = list(),

        #' @field reactions A list of `Reaction` objects
        reactions = list(),

        #' @field observables A list of `Observable` objects
        observables = list(),

        #' @field doses List of dosing objects. Includes bolus doses into model
        #'   compartments, as well as bolus-to-bag doses used to implement infusions.
        doses = NULL,

        #' @field infusionEvents Data frame of infusion rate adjustment events
        #'   (start and stop of zero-order input). Generated automatically when
        #'   infusions are defined.
        infusionEvents = NULL,

        #' @description
        #' Initialize a new `CompartmentModel` object.
        initialize = function() {
            self$compartments <- list()
            self$reactions <- list()
            self$observables <- list()
            self$doses <- list()
            self$infusionEvents <- data.frame(
                var = character(),
                time = numeric(),
                value = numeric(),
                method = character(),
                stringsAsFactors = FALSE
            )
        }
    )
)
