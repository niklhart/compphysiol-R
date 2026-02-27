# S3 redesign of Dosing class


#' Dosing specification
#' Represents a dosing event for a `CompartmentModel`. A dose can be
#' specified either as a bolus (instantaneous input) or as an infusion (continuous input over time).
#' ## Bolus
#' Provide only `amount`. The dose is given at `time`.
#' ## Infusion
#' Provide any two of `amount`, `rate`, and `duration`. The missing quantity is derived automatically:
#' \deqn{amount = rate * duration}
#' \deqn{rate = amount / duration}
#' \deqn{duration = amount / rate}
#' @param target Target compartment name (character scalar)
#' @param time Dosing time(s) (numeric scalar or vector)
#' @param amount Amount(s) of dose (non-negative numeric scalar or vector, optional)
#' @param rate Infusion rate (non-negative numeric scalar or vector, optional)
#' @param duration Infusion duration (positive numeric scalar or vector, optional)
#' @return A `Dosing` object
#' @examples
#' # Bolus of 100 at t=0
#' dosing("Central", time = 0, amount = 100)
#' # Infusion examples (all equivalent)
#' dosing("Central", time = 0, rate = 10, duration = 5)
#' dosing("Central", time = 0, amount = 50, duration = 5)
#' dosing("Central", time = 0, amount = 50, rate = 10)
#' # Vectorized dosing examples
#' dosing("Central", time = c(0, 24, 48), amount = 100)
#' dosing("Central", time = c(0, 24), amount = c(50, 60), rate = 10)
#' @export
dosing <- function(target, time, amount = NULL, rate = NULL, duration = NULL) {
    
    # Validate inputs
    stopifnot(is.character(target))
    stopifnot(is.numeric(time))
    if (!is.null(amount)) stopifnot(is.numeric(amount), amount >= 0)
    if (!is.null(rate)) stopifnot(is.numeric(rate), rate >= 0)
    if (!is.null(duration)) stopifnot(is.numeric(duration), duration > 0)

    # Early return if dosing time is empty (no dosing events)
    if (length(time) == 0) {
        return(empty_dosing())
    }

    # More strict checks on argument lengths than data.frame recycling rules, to avoid silent bugs from unintended recycling.
    arg_lengths <- c(
        target = length(target),
        time = length(time),
        amount = if (!is.null(amount)) length(amount) else NA_integer_,
        rate = if (!is.null(rate)) length(rate) else NA_integer_,
        duration = if (!is.null(duration)) length(duration) else NA_integer_
    )
    valid_lengths <- arg_lengths[!is.na(arg_lengths)]
    non_scalar <- valid_lengths[valid_lengths > 1]
    if (length(unique(non_scalar)) > 1) {
        stop("Incompatible argument lengths: all non-scalar dosing arguments must have the same length.")
    }

    # Case 1: bolus dosing -- early return
    if (!is.null(amount) && is.null(rate) && is.null(duration)) {
        # Bolus dosing
        return(structure(
            data.frame(
                target = target,
                time = time,
                amount = amount,
                rate = NA_real_,
                duration = NA_real_
            ),
            class = "Dosing"
        ))
    }

    # Case 2: infusion dosing (need exactly two out of amount, rate, duration)
    nset <- sum(!sapply(list(amount, rate, duration), is.null))
    if (nset != 2) stop("Infusion dosing requires exactly two of: amount, rate, duration.")

    if (is.null(amount)) {
        amount <- rate * duration
    } else if (is.null(rate)) {
        rate <- amount / duration
    } else if (is.null(duration)) {
        duration <- amount / rate
    }

    structure(
        data.frame(
            target = target,
            time = time,
            amount = amount,
            rate = rate,
            duration = duration
        ),
        class = "Dosing"
    )
}

#' Create an empty `Dosing` object
#' @return An empty `Dosing` object with zero rows
#' @export
empty_dosing <- function() {
    structure(
        data.frame(
            target = character(0),
            time = numeric(0),
            amount = numeric(0),
            rate = numeric(0),
            duration = numeric(0)
        ),
        class = "Dosing"
    )
}

#' Check which dosing events are boluses.
#' @param dose A `Dosing` object
#' @return A logical vector in which the i-th entry is `TRUE` if the i-th dosing is a bolus and `FALSE` otherwise
#' @export
is_bolus <- function(dose) {
    .check_class(dose, "Dosing")
    is.na(dose$rate) & is.na(dose$duration)
}

#' Check which dosing events are infusions.
#' @param dose A `Dosing` object
#' @return A logical vector in which the i-th entry is `TRUE` if the i-th dosing is an infusion and `FALSE` otherwise
#' @export
is_infusion <- function(dose) {
    .check_class(dose, "Dosing")
    !is_bolus(dose)
}

#' Length method for Dosing class
#' Returns the number of dosing events in a `Dosing` object.
#' @param x A `Dosing` object
#' @return The number of dosing events (number of rows in the data frame)
#' @export
length.Dosing <- function(x) {
    length(x$target)
}

#' Print method for Dosing class
#' Pretty-prints a `Dosing` object.
#' @param x A `Dosing` object
#' @param ... ignored
#' @return The `Dosing` object (invisibly)
#' @export
print.Dosing <- function(x, ...) {

    n <- length(x)
    if (n > 0) {

        # assemble string to be printed, differentiating bolus vs infusion events
        dosing_strings <- character(length = n)
        dosing_strings[is_bolus(x)] <- paste0(
            "   - Bolus: ",
            format(x$amount[is_bolus(x)]),
            " \u2192 ",
            x$target[is_bolus(x)],
            " at t = ",
            format(x$time[is_bolus(x)])
        )
        dosing_strings[is_infusion(x)] <- paste0(
            "   - Infusion: ",
            format(x$amount[is_infusion(x)]),
            " \u2192 ",
            x$target[is_infusion(x)],
            " from t = ",
            format(x$time[is_infusion(x)]),
            " to t = ",
            format(x$time[is_infusion(x)] + x$duration[is_infusion(x)]),
            " (rate = ",
            format(x$rate[is_infusion(x)]),
            ")"
        )
        cat(" Dosing:\n")
        cat(dosing_strings, sep = "\n")
    } else {
        cat(" Dosing: (none)\n")
    }
    invisible(x)
}

#' Subset method for `Dosing` class
#' Allows subsetting a `Dosing` object while preserving its class.
#' @param x A `Dosing` object
#' @param i Row indices to subset
#' @param ... ignored
#' @return A subsetted `Dosing` object
#' @export
`[.Dosing` <- function(x, i, ...) {
    x_subset <- as.data.frame(x)[i, , drop = FALSE]
    structure(x_subset, class = "Dosing")
}

#' Combine multiple `Dosing` objects into one
#' Combines multiple `Dosing` objects by row-binding their data.
#' @param ... One or more `Dosing` objects to combine.
#' @return A single `Dosing` object containing all dosing events from the inputs.
#' @export
c.Dosing <- function(...) {
    objs <- list(...)
    if (!all(sapply(objs, function(o) inherits(o, "Dosing")))) {
        stop("All inputs must be Dosing objects.")
    }

    # Combine the data frames by row-binding
    combined_df <- do.call(rbind, lapply(objs, as.data.frame))
    structure(combined_df, class = "Dosing")
}

#' Convert a Dosing object to a data frame
#' 
#' This method allows a `Dosing` object to be treated as a data frame for operations 
#' like subsetting and combining, while preserving the `Dosing` class.
#' 
#' @param x A `Dosing` object
#' @param ... ignored
#' @return A data frame representation of the `Dosing` object
#' @export
as.data.frame.Dosing <- function(x, ...) {
    class(x) <- "data.frame"
    x
}


# Old R6 implementation of Dosing class, now replaced by S3 redesign

#' Dosing specification
#'
#' Represents a dosing event for a `CompartmentModel`. A dose can be
#' specified either as a bolus (instantaneous input) or as an infusion
#' (continuous input over time).
#'
#' ## Bolus
#' Provide only `amount`. The dose is given at `time`.
#'
#' ## Infusion
#' Provide any two of `amount`, `rate`, and `duration`.
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
        #' @field target Target compartment name (character scalar)
        target = NULL,
        #' @field amount Total amount for bolus or preloaded bag (numeric scalar, non-negative)
        amount = NULL,
        #' @field time Time of dose (numeric scalar, non-negative)
        time = NULL,
        #' @field rate Infusion rate (numeric scalar, non-negative, NULL for bolus)
        rate = NULL,
        #' @field duration Infusion duration (numeric scalar, positive, NULL for bolus)
        duration = NULL,

        #' @description
        #' Initialize a new `Dosing` object.
        #' @param target Target compartment name (character scalar)
        #' @param time Time of dose (numeric scalar, non-negative)
        #' @param amount Amount of dose (numeric scalar, non-negative, optional)
        #' @param rate Infusion rate (numeric scalar, non-negative, optional)
        #' @param duration Infusion duration (numeric scalar, positive, optional)
        #' @return A new `Dosing` object
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

            stopifnot(
                length(amount) == 1,   amount >= 0,
                length(rate) == 1,     rate >= 0,
                length(duration) == 1, duration > 0
            )

            self$amount <- amount
            self$rate <- rate
            self$duration <- duration
            invisible(self)

        },

        #' @description
        #' Check if the dosing event is a bolus.
        #' @return `TRUE` if the dosing is a bolus, `FALSE` otherwise
        isBolus = function() {
            is.null(self$rate) && is.null(self$duration)
        },
        
        #' @description
        #' Check if the dosing event is an infusion.
        #' @return `TRUE` if the dosing is an infusion, `FALSE` otherwise.
        isInfusion = function() {
            !self$isBolus()
        },

        #' @description
        #' Print a summary of the dosing event.
        #' @param ... Additional arguments (ignored)
        #' @return The `Dosing` object (invisible)
        print = function(...) {
            if (self$isBolus()) {
                cat("<Dosing> Bolus:",
                    format(self$amount), "\u2192", self$target,
                    "at t =", format(self$time), "\n")
            } else if (self$isInfusion()) {
                cat("<Dosing> Infusion:",
                    format(self$amount), "\u2192", self$target,
                    "from t =", format(self$time),
                    "to t =", format(self$time + self$duration),
                    "(rate =", format(self$rate), ")\n")
            } else {
                cat("<Dosing> (invalid)\n")
            }
            invisible(self)
        }
    )
)

#' Construct one or more Dosing objects
#'
#' `DosingList()` is a wrapper function to construct multiple `Dosing` objects at once.
#' It accepts vectorized arguments of compatible length (`NULL`, scalar, or same-length vectors).
#'
#' @param target Target compartment name (character scalar)
#' @param time Time(s) of dose(s) (numeric vector, non-negative)
#' @param amount Amount(s) of dose(s) (numeric vector, non-negative, optional)
#' @param rate Infusion rate(s) (numeric vector, non-negative, optional)
#' @param duration Infusion duration(s) (numeric vector, positive, optional)
#' @return A list of `Dosing` objects.
#' @examples
#' DosingList("Central", time = c(0, 24, 48), amount = 100)
#' DosingList("Central", time = 0, amount = 50)
#'
#' @export
DosingList <- function(target, time, amount = NULL, rate = NULL, duration = NULL) {
    stopifnot(!is.null(time), is.numeric(time), all(time >= 0))

    arg_lengths <- c(
        time     = length(time),
        amount   = if (!is.null(amount)) length(amount) else NA_integer_,
        rate     = if (!is.null(rate)) length(rate) else NA_integer_,
        duration = if (!is.null(duration)) length(duration) else NA_integer_
    )
    valid_lengths <- arg_lengths[!is.na(arg_lengths)]

    # --- Enforce scalar-or-same-length rule ---
    non_scalar <- valid_lengths[valid_lengths > 1]
    if (length(non_scalar) > 1 && length(unique(non_scalar)) > 1) {
        stop(
            "Incompatible argument lengths: all non-scalar dosing arguments must have the same length, ",
            "or be scalar for recycling."
        )
    }

    # Determine number of dosing events
    n <- max(valid_lengths, na.rm = TRUE)

    # Helper for safe recycling
    recycle <- function(x) {
        if (is.null(x)) return(NULL)
        if (length(x) == 1) rep(x, n) else x
    }

    time     <- recycle(time)
    amount   <- recycle(amount)
    rate     <- recycle(rate)
    duration <- recycle(duration)

    # Create list of Dosing objects
    lapply(seq_len(n), function(i) {
        Dosing$new(
            target   = target,
            time     = time[i],
            amount   = if (!is.null(amount)) amount[i],
            rate     = if (!is.null(rate)) rate[i],
            duration = if (!is.null(duration)) duration[i]
        )
    })

}
