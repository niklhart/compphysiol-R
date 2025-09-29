#' `CompartmentModel` class
#'
#' Represents a compartmental model.
#'
#' @export
CompartmentModel <- R6::R6Class(
    "CompartmentModel",
    public = list(
        compartments = list(),
        reactions = list(),
        observables = list(),
        doses = NULL,
        infusionEvents = NULL,

        #' @description
        #' Initialize a new CompartmentModel.
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
