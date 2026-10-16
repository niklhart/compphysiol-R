#' Create a `compphysiol_dosing` object
#'
#' Represents a dosing event for a `compphysiol_compartment_model`. A dose can be specified either as a bolus (instantaneous input)
#' or as an infusion (continuous input over time).
#'
#' ## Bolus
#' Provide `cmt`, `time`, and `amount`.
#' ## Infusion
#' In addition to `cmt` and `time`, provide any two of `amount`, `rate`, and `duration`. 
#' The missing quantity is derived automatically:
#' \deqn{amount = rate * duration}
#' \deqn{rate = amount / duration}
#' \deqn{duration = amount / rate}
#' The `rate` and `duration` argument must be specified as named arguments, not positional, to avoid any ambiguity.
#'
#' @param time compphysiol_dosing time(s) (numeric scalar or vector)
#' @param amount Amount(s) of dose (non-negative numeric scalar or vector, optional)
#' @param time_unit Optional unit for time (character scalar, e.g., "h", or `NULL`, the default)
#' @param amount_unit Optional unit for amount (character scalar, e.g., "mg", or `NULL`, the default)
#' @param molec The name of the molecule to be dosed (character scalar). 
#'   If only a single molecule is present in the model, this can be omitted.
#' @param cmt Compartment name (character scalar). If only a single compartment is present in the model, 
#'   this can be omitted.
#' @param ... Unused, enforces `rate` and `duration` for infusion dosing to be specified 
#'   as named arguments only, not positional
#' @param rate Infusion rate (non-negative numeric scalar or vector, optional)
#' @param duration Infusion duration (positive numeric scalar or vector, optional)
#' @return A `compphysiol_dosing` object
#' @examples
#' # Bolus of 100 at t=0
#' dosing(time = 0, amount = 100, cmt = "Central")
#' # Infusion examples (all equivalent)
#' dosing(time = 0, rate = 10, duration = 5, cmt = "Central")
#' dosing(time = 0, amount = 50, duration = 5, cmt = "Central")
#' dosing(time = 0, amount = 50, rate = 10, cmt = "Central")
#' # Vectorized dosing examples
#' dosing(time = c(0, 24, 48), amount = 100, cmt = "Central")
#' dosing(time = c(0, 24), amount = c(50, 60), rate = 10, cmt = "Central")
#' @export
dosing <- function(
    time = numeric(0),
    amount = NULL,
    time_unit = NULL,
    amount_unit = NULL,
    molec = NULL,
    cmt = NULL,
    ...,
    rate = NULL,
    duration = NULL
) {
    # Process NSE arguments for time, amount, rate, and duration, which might include units via `value[unit]`
    time <- .process_nse_arg(expr = substitute(time), envir = parent.frame(n = 1))
    amount <- .process_nse_arg(expr = substitute(amount), envir = parent.frame(n = 1))
    rate <- .process_nse_arg(expr = substitute(rate), envir = parent.frame(n = 1))
    duration <- .process_nse_arg(expr = substitute(duration), envir = parent.frame(n = 1))

    # Set units if specified via time_unit and amount_unit arguments
    if (!is.null(time_unit)) time <- units::set_units(time, time_unit, mode = "standard")
    if (!is.null(amount_unit)) amount <- units::set_units(amount, amount_unit, mode = "standard")

    # Early return for empty dosing
    ndose <- length(time)
    if (ndose == 0) {
        return(.validate_dosing(.new_dosing(
            .dosing_data_frame(
                time = numeric(0),
                amount = numeric(0),
                rate = numeric(0),
                duration = numeric(0),
                molec = character(0),
                cmt = character(0)
            )
        )))
    }

    # Validate inputs (using sign to support variables with/without units)
    stopifnot(is.numeric(time))
    if (!is.null(amount)) stopifnot(.dosing_is_numeric_input(amount))
    if (!is.null(rate)) stopifnot(.dosing_is_numeric_input(rate))
    if (!is.null(duration)) stopifnot(is.numeric(duration), sign(duration) == 1)

    # More strict checks on argument lengths than data.frame recycling rules, to avoid silent bugs from unintended recycling.
    stopifnot(
        "amount must be NULL, scalar or match length of time" = length(amount) %in% c(0, 1, ndose),
        "rate must be NULL, scalar or match length of time" = length(rate) %in% c(0, 1, ndose),
        "duration must be NULL, scalar or match length of time" = length(duration) %in% c(0, 1, ndose),
        "molec must be NULL, scalar or match length of time" = length(molec) %in% c(0, 1, ndose),
        "cmt must be NULL, scalar or match length of time" = length(cmt) %in% c(0, 1, ndose)
    )

    # Variables with units are not automatically recycled by data.frame -> do it here
    if (!is.null(amount)) amount <- rep(amount, length.out = ndose)
    if (!is.null(rate)) rate <- rep(rate, length.out = ndose)
    if (!is.null(duration)) duration <- rep(duration, length.out = ndose)

    # Uniformize molec and cmt to NA if NULL, to avoid issues with data.frame handling of NULL vs NA
    molec <- molec %||% NA_character_
    cmt <- cmt %||% NA_character_

    # Case 1: bolus dosing -- early return
    if (!is.null(amount) && is.null(rate) && is.null(duration)) {
        return(.validate_dosing(.new_dosing(
            .dosing_data_frame(
                time = time,
                amount = amount,
                rate = rep(NA_real_, ndose),
                duration = rep(NA_real_, ndose),
                molec = molec,
                cmt = cmt
            )
        )))
    }

    # Case 2: infusion dosing (need exactly two out of amount, rate, duration)
    nset <- sum(!sapply(list(amount, rate, duration), is.null))
    if (nset > 2) stop("Only two of amount, rate, and duration can be specified.")
    if (nset < 2) stop("Incomplete set of dosing arguments.")

    if (is.null(amount)) {
        amount <- rate * duration
    } else if (is.null(rate)) {
        rate <- amount / duration
    } else if (is.null(duration)) {
        duration <- amount / rate
    }

    x <- .new_dosing(
        .dosing_data_frame(
            time = time,
            amount = amount,
            rate = rate,
            duration = duration,
            molec = molec,
            cmt = cmt
        )
    )
    .validate_dosing(x)
}

.new_dosing <- function(x) {
    structure(x, class = .compphysiol_class("compphysiol_dosing"))
}

.validate_dosing <- function(x) {
    required <- c("time", "amount", "rate", "duration", "molec", "cmt")
    if (!is.list(unclass(x)) || !all(required %in% names(unclass(x)))) {
        stop("Dosing must contain the canonical dosing columns.", call. = FALSE)
    }
    if (!is.numeric(x$time) || any(!is.finite(x$time)) ||
        !is.list(x$amount) || !is.list(x$rate) || !is.numeric(x$duration) ||
        !is.character(x$molec) || !is.character(x$cmt)) {
        stop("Dosing columns do not satisfy the dosing representation.", call. = FALSE)
    }
    bolus <- .dosing_is_na_col(x$rate) & is.na(x$duration)
    infusion <- !bolus
    if (any(vapply(x$amount, function(value) !is.numeric(value), logical(1))) ||
        any(infusion & (!is.finite(x$duration) | as.numeric(x$duration) <= 0)) ||
        any(vapply(x$rate, function(value) !is.numeric(value), logical(1)))) {
        stop("Dosing amounts and rates must be numeric and infusion durations positive.", call. = FALSE)
    }
    x
}

.dosing_data_frame <- function(time, amount, rate, duration, molec, cmt) {
    data.frame(
        time = time,
        amount = I(.dosing_list_col(amount)),
        rate = I(.dosing_list_col(rate)),
        duration = duration,
        molec = molec,
        cmt = cmt
    )
}

.dosing_is_numeric_input <- function(x) {
    is.numeric(x) || (is.list(x) && all(vapply(x, is.numeric, logical(1))))
}

.dosing_list_col <- function(x) {
    lapply(seq_along(x), function(i) x[[i]])
}

.dosing_format_col <- function(x) {
    vapply(x, format, character(1))
}

.dosing_is_na_col <- function(x) {
    vapply(x, function(value) all(is.na(value)), logical(1))
}

#' Add one or several dosing events (bolus or infusion) to a `compphysiol_compartment_model` object.
#'
#' This function allows the user to add dosing events to a compartment model.
#' compphysiol_dosing events can be either bolus (instantaneous) or infusion (continuous over time).
#'
#' Unlike the other `add_*` functions, `add_dosing()` does not simply append the new dosing events 
#' to the model, but also handles the necessary modifications to the model structure for infusion dosing,
#' i.e., adding infusion bag and rate compartments, and creating the appropriate events for
#' starting and stopping the infusion.
#'
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams dosing
#' @param dose A `compphysiol_dosing` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @examples
#' model <- compartment_model() |>
#'     add_dosing(time = 0, amount = 100, duration = 5, cmt = "cen")
#' @export
add_dosing <- function(
    model,
    time = numeric(0),
    amount = NULL,
    time_unit = NULL,
    amount_unit = NULL,
    molec = NULL,
    cmt = NULL,
    ...,
    rate = NULL,
    duration = NULL,
    dose
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    dose <- .forward_or_use(
        object_arg_name = "dose",
        constructor_name = "dosing",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(dose, "compphysiol_dosing")

    model$doses <- c(model$doses, dose)
    model
}

#' Check which dosing events are boluses.
#' @param dose A `compphysiol_dosing` object
#' @return A logical vector in which the i-th entry is `TRUE` if the i-th dosing is a bolus and `FALSE` otherwise
#' @export
is_bolus <- function(dose) {
    .check_class(dose, "compphysiol_dosing")
    .dosing_is_na_col(dose$rate) & is.na(dose$duration)
}

#' Check which dosing events are infusions.
#' @param dose A `compphysiol_dosing` object
#' @return A logical vector in which the i-th entry is `TRUE` if the i-th dosing is an infusion and `FALSE` otherwise
#' @export
is_infusion <- function(dose) {
    .check_class(dose, "compphysiol_dosing")
    !is_bolus(dose)
}

#' Length of a `compphysiol_dosing` object
#' 
#' Returns the number of dosing events in a `compphysiol_dosing` object.
#' 
#' @param x A `compphysiol_dosing` object
#' @return The number of dosing events (number of rows in the data frame)
#' @export
length.compphysiol_dosing <- function(x) {
    nrow(as.data.frame(x))
}

#' Print a `compphysiol_dosing` object
#' 
#' Pretty-prints a `compphysiol_dosing` object.
#' 
#' @param x A `compphysiol_dosing` object
#' @param ... ignored
#' @return The `compphysiol_dosing` object (invisibly)
#' @export
print.compphysiol_dosing <- function(x, ...) {

    n <- length(x)
    if (n > 0) {

        bol <- is_bolus(x)
        inf <- is_infusion(x)

        target <- ifelse(
            is.na(x$molec), 
            yes = ifelse(
                is.na(x$cmt), 
                yes = "", 
                no = paste0(" in ", x$cmt)
            ),
            no = ifelse(
                is.na(x$cmt), 
                yes = paste0(" of ", x$molec), 
                no = paste0(" of ", x$molec, " in ", x$cmt)
            )
    )

        # assemble string to be printed, differentiating bolus vs infusion events
        dosing_strings <- character(length = n)
        if (any(bol)) {
            dosing_strings[bol] <- paste0(
                "Bolus: ",
                .dosing_format_col(x$amount[bol]),
#                " \u2192 ",
                target[bol],
                " at t = ",
                format(x$time[bol])
            )
        }
        if (any(inf)) {
            dosing_strings[inf] <- paste0(
                "Infusion: ",
                .dosing_format_col(x$amount[inf]),
 #               " \u2192 ",
                target[inf],
                " from t = ",
                format(x$time[inf]),
                " to t = ",
                format(x$time[inf] + x$duration[inf]),
                " (rate = ",
                .dosing_format_col(x$rate[inf]),
                ")"
            )
        }
        cat(" Dosing:\n")
        cat(sprintf('  (%s) %s\n', seq_len(n), dosing_strings), sep = "")
    } else {
        cat(" Dosing: (none)\n")
    }
    invisible(x)
}

#' Subset a `compphysiol_dosing` object
#' 
#' Allows subsetting a `compphysiol_dosing` object while preserving its class.
#' 
#' @param x A `compphysiol_dosing` object
#' @param i Row indices to subset
#' @param ... ignored
#' @return A subsetted `compphysiol_dosing` object
#' @export
`[.compphysiol_dosing` <- function(x, i, ...) {
    x_subset <- as.data.frame(x)[i, , drop = FALSE]
    .new_dosing(x_subset)
}

#' Combine multiple `compphysiol_dosing` objects into one
#' 
#' Combines multiple `compphysiol_dosing` objects by row-binding their data.
#' The resulting `compphysiol_dosing` object will contain all dosing events from the inputs, sorted by time.
#' 
#' @param ... One or more `compphysiol_dosing` objects to combine.
#' @return A single `compphysiol_dosing` object containing all dosing events from the inputs, sorted by time.
#' @export
c.compphysiol_dosing <- function(...) {
    objs <- list(...)
    if (!all(sapply(objs, function(o) inherits(o, "compphysiol_dosing")))) {
        stop("All inputs must be dosing objects.")
    }

    # Combine the data frames by row-binding
    combined_df <- do.call(rbind, lapply(objs, as.data.frame))

    # sort by time to ensure correct order of dosing events
    combined_df <- combined_df[order(combined_df$time), ]

    # output as object of class Dosing
    combined <- .new_dosing(combined_df)
    .validate_dosing(combined)
}

#' Convert a `compphysiol_dosing` object to a data frame
#' 
#' This method allows a `compphysiol_dosing` object to be treated as a data frame for operations 
#' like subsetting and combining, while preserving the `compphysiol_dosing` class.
#' 
#' @param x A `compphysiol_dosing` object
#' @param ... ignored
#' @return A data frame representation of the `compphysiol_dosing` object
#' @export
as.data.frame.compphysiol_dosing <- function(x, ...) {
    class(x) <- "data.frame"
    x
}
