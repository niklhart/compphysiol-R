#' Dosing specification
#'
#' Represents a dosing event for a `CompartmentModel`. A dose can be specified either as a bolus (instantaneous input) 
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
#' @param time Dosing time(s) (numeric scalar or vector)
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
#' @return A `Dosing` object
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
        return(structure(
            data.frame(
                time = numeric(0),
                amount = numeric(0),
                rate = numeric(0),
                duration = numeric(0),
                molec = character(0),
                cmt = character(0)
            ),
            class = "Dosing"
        ))
    }

    # Validate inputs (using sign to support variables with/without units)
    stopifnot(is.numeric(time))
    if (!is.null(amount)) stopifnot(is.numeric(amount), sign(amount) >= 0)
    if (!is.null(rate)) stopifnot(is.numeric(rate), sign(rate) >= 0)
    if (!is.null(duration)) stopifnot(is.numeric(duration), sign(duration) == 1)

    # More strict checks on argument lengths than data.frame recycling rules, to avoid silent bugs from unintended recycling.
    stopifnot(
        "amount must be NULL, scalar or match length of time" = length(amount) %in% c(0, 1, ndose),
        "rate must be NULL, scalar or match length of time" = length(rate) %in% c(0, 1, ndose),
        "duration must be NULL, scalar or match length of time" = length(duration) %in% c(0, 1, ndose),
        "molec must be NULL or scalar" = length(molec) %in% c(0, 1),
        "cmt must be NULL or scalar" = length(cmt) %in% c(0, 1)
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
        return(structure(
            data.frame(
                time = time,
                amount = amount,
                rate = NA_real_,
                duration = NA_real_,
                molec = molec,
                cmt = cmt
            ),
            class = "Dosing"
        ))
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

    structure(
        data.frame(
            time = time,
            amount = amount,
            rate = rate,
            duration = duration,
            molec = molec,
            cmt = cmt
        ),
        class = "Dosing"
    )
}

#' Add one or several dosing events (bolus or infusion).
#'
#' This function allows the user to add dosing events to a compartment model.
#' Dosing events can be either bolus (instantaneous) or infusion (continuous over time).
#'
#' Unlike the other `add_*` functions, `add_dosing()` does not simply append the new dosing events 
#' to the model, but also handles the necessary modifications to the model structure for infusion dosing,
#' i.e., adding infusion bag and rate compartments, and creating the appropriate events for
#' starting and stopping the infusion.
#'
#' @param model A `CompartmentModel` object.
#' @inheritParams dosing
#' @param dose A `Dosing` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
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
    .check_class(model, "CompartmentModel")

    call <- match.call()

    dose <- .forward_or_use(
        object_arg_name = "dose",
        constructor_name = "dosing",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(dose, "Dosing")

    # Separate bolus and infusion dosing for different handling
    bolus <- dose[is_bolus(dose)]
    infus <- dose[is_infusion(dose)]

    # Bolus dosing is simply appended to the models dosing list
    model$doses <- c(model$doses, bolus)

    # Early return if no infusion dosing
    if (length(infus) == 0) {
        return(model)
    }

    # --------------------------------------------------------------------------------------------------
    # Infusion dosing requires more complex handling: we need to add infusion bag and rate compartments,
    # convert the infusion dosing into bolus-to-bag + infusion rate events, and add flows from the bag
    # to the target compartment with rate equal to the infusion rate.
    # --------------------------------------------------------------------------------------------------

    # Convert infusion dosing into bolus-to-bag + infusion rate events, and add to model
    bag_names <- paste0("InfusionBag_", infus$target)
    rate_names <- paste0("InfusionRate_", infus$target)
    comp_names <- names(model$compartments)
    new_bag_names <- setdiff(bag_names, comp_names)
    new_rate_names <- setdiff(rate_names, comp_names)

    # Helper function allowing to update the model in a single pipeline
    add_infusion_events <- function(model, var, time, value, method) {
        new_events <- data.frame(
            var = var,
            time = time,
            value = value,
            method = method,
            stringsAsFactors = FALSE
        )
        model$infusionEvents <- rbind(model$infusionEvents, new_events)
        return(model)
    }

    # Return the updated model
    model |>
        add_compartment(new_bag_names, 0) |>
        add_compartment(new_rate_names, 0) |>
        add_dosing(
            time = infus$time,
            amount = infus$rate * infus$duration,
            cmt = bag_names
        ) |>
        add_transport(
            from = bag_names,
            to = infus$target,
            rate = rate_names
        ) |>
        add_infusion_events(
            var = rate_names,
            time = infus$time,
            value = infus$rate,
            method = "add"
        ) |>
        add_infusion_events(
            var = rate_names,
            time = infus$time + infus$duration,
            value = -infus$rate,
            method = "add"
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
#' 
#' Returns the number of dosing events in a `Dosing` object.
#' 
#' @param x A `Dosing` object
#' @return The number of dosing events (number of rows in the data frame)
#' @export
length.Dosing <- function(x) {
    length(x$time)
}

#' Print method for Dosing class
#' 
#' Pretty-prints a `Dosing` object.
#' 
#' @param x A `Dosing` object
#' @param ... ignored
#' @return The `Dosing` object (invisibly)
#' @export
print.Dosing <- function(x, ...) {

    n <- length(x)
    if (n > 0) {

        bol <- is_bolus(x)
        inf <- is_infusion(x)

        target <- ifelse(
            is.na(x$molec), 
            yes = paste0(" in ", x$cmt), 
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
                format(x$amount[bol]),
#                " \u2192 ",
                target[bol],
                " at t = ",
                format(x$time[bol])
            )
        }
        if (any(inf)) {
            dosing_strings[inf] <- paste0(
                "Infusion: ",
                format(x$amount[inf]),
 #               " \u2192 ",
                target[inf],
                " from t = ",
                format(x$time[inf]),
                " to t = ",
                format(x$time[inf] + x$duration[inf]),
                " (rate = ",
                format(x$rate[inf]),
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

#' Subset method for `Dosing` class
#' 
#' Allows subsetting a `Dosing` object while preserving its class.
#' 
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
#' 
#' Combines multiple `Dosing` objects by row-binding their data.
#' 
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
