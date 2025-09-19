#' Dosing Class
#'
#' Represents dosing into a CompartmentModel
#'
#' @export
Dosing <- R6::R6Class("Dosing",
                      public = list(
                          target = NULL,
                          amount = NULL,    # total amount for bolus or preloaded bag
                          time = NULL,      # start time
                          rate = NULL,      # infusion rate
                          duration = NULL,  # infusion duration

                          initialize = function(target, amount = NULL, time, rate = NULL, duration = NULL) {
                              self$target <- target
                              self$amount <- amount
                              self$time <- time
                              self$rate <- rate
                              self$duration <- duration
                          },

                          isBolus = function() {
                              is.null(self$rate) && is.null(self$duration)
                          },

                          isInfusion = function() {
                              !is.null(self$rate) && !is.null(self$duration)
                          }
                      )
)
