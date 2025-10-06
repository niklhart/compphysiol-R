#' Dosing specification
#'
#' Represents a dosing event for a \code{CompartmentModel}. A dose can be
#' specified either as a bolus (instantaneous input) or as an infusion
#' (continuous input over time).
#'
#' ## Bolus
#' Provide only \code{amount}. The dose is given at \code{time}.
#'
#' ## Infusion
#' Provide any two of \code{amount}, \code{rate}, and \code{duration}.
#' The missing quantity is derived automatically:
#' \deqn{amount = rate * duration}
#' \deqn{rate = amount / duration}
#' \deqn{duration = amount / rate}
#'
#' @examples
#' # Bolus of 100 at t=0
#' Dosing$new("Central", time = 0, amount = 100)
#'
#' # Infusion examples (all equivalent)
#' Dosing$new("Central", time = 0, rate = 10, duration = 5)
#' Dosing$new("Central", time = 0, amount = 50, duration = 5)
#' Dosing$new("Central", time = 0, amount = 50, rate = 10)
#'
#' @export
Dosing <- R6::R6Class("Dosing",
                      public = list(
                          target = NULL,
                          amount = NULL,    # total amount for bolus or preloaded bag
                          time = NULL,      # start time
                          rate = NULL,      # infusion rate
                          duration = NULL,  # infusion duration

                          initialize = function(target, time, amount = NULL, rate = NULL, duration = NULL) {
                              stopifnot(!is.null(target), is.character(target), nzchar(target))
                              stopifnot(!is.null(time), is.numeric(time), length(time) == 1, time >= 0)

                              self$target <- target
                              self$time <- time

                              # bolus: only amount is given
                              if (!is.null(amount) && is.null(rate) && is.null(duration)) {
                                  stopifnot(is.numeric(amount), length(amount) == 1, amount >= 0)

                                  self$amount <- amount
                                  self$rate <- NULL
                                  self$duration <- NULL
                                  return(invisible(self))
                              }

                              # infusion: need exactly two of the three (amount, rate, duration)
                              nset <- sum(!sapply(list(amount, rate, duration), is.null))
                              if (nset != 2) {
                                  stop("Infusion dosing requires exactly two of: amount, rate, duration.")
                              }

                              if (is.null(amount)) {
                                  amount <- rate * duration
                              } else if (is.null(rate)) {
                                  rate <- amount / duration
                              } else if (is.null(duration)) {
                                  duration <- amount / rate
                              }

                              stopifnot(length(amount) == 1,   amount >= 0,
                                        length(rate) == 1,     rate >= 0,
                                        length(duration) == 1, duration > 0)

                              self$amount <- amount
                              self$rate <- rate
                              self$duration <- duration
                              invisible(self)

                          },

                          isBolus = function() {
                              is.null(self$rate) && is.null(self$duration)
                          },

                          isInfusion = function() {
                              !self$isBolus()
                          }
                      )
)


Dosing$set("public", "print", function(...) {
    if (self$isBolus()) {
        cat("<Dosing> Bolus:",
            self$amount, "→", self$target,
            "at t =", self$time, "\n")
    } else if (self$isInfusion()) {
        cat("<Dosing> Infusion:",
            self$amount, "→", self$target,
            "from t =", self$time,
            "to t =", self$time + self$duration,
            "(rate =", self$rate, ")\n")
    } else {
        cat("<Dosing> (invalid)\n")
    }
    invisible(self)
})
