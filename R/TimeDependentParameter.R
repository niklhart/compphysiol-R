#' Construct a time-dependent parameter value
#'
#' `interpolate()` describes a known continuous parameter whose value changes
#' over model time. The resulting object is supplied as a value to
#' [parameters()]. Model expressions continue to refer to the parameter by its
#' bare name; the current value is selected at the solver's current time.
#'
#' Times are sorted together with their values. Evaluation before the first
#' scheduled time is an error, while the final value is carried forward after
#' the last scheduled time.
#'
#' @param time Unique finite numeric times, optionally with compatible units.
#' @param value Finite numeric values of the same length as `time`, optionally
#'   with compatible units.
#' @param method Interpolation method: `"linear"` or `"locf"` (last
#'   observation carried forward).
#' @returns A structured time-dependent parameter value for use in
#'   [parameters()].
#' @export
interpolate <- function(time, value, method = c("linear", "locf")) {
    method <- match.arg(method)
    if (!is.numeric(time) || !is.null(dim(time)) || !length(time)) {
        stop("'time' must be a nonempty numeric vector.", call. = FALSE)
    }
    if (!is.numeric(value) || !is.null(dim(value))) {
        stop("'value' must be a numeric vector.", call. = FALSE)
    }
    if (length(time) != length(value)) {
        stop("'time' and 'value' must have the same length.", call. = FALSE)
    }
    if (anyNA(time) || any(!is.finite(as.numeric(time)))) {
        stop("'time' must contain only finite values.", call. = FALSE)
    }
    if (anyNA(value) || any(!is.finite(as.numeric(value)))) {
        stop("'value' must contain only finite values.", call. = FALSE)
    }
    if (anyDuplicated(as.numeric(time))) {
        stop("'time' values must be unique.", call. = FALSE)
    }

    order <- order(as.numeric(time))
    structure(
        list(time = time[order], value = value[order], method = method),
        class = .compphysiol_class("compphysiol_interpolation")
    )
}

.is_interpolation <- function(x) inherits(x, "compphysiol_interpolation")

#' @export
format.compphysiol_interpolation <- function(x, ...) {
    paste0("<", x$method, " interpolation, ", length(x$time), " points>")
}

#' @export
print.compphysiol_interpolation <- function(x, ...) {
    cat(format(x), "\n", sep = "")
    invisible(x)
}

.interpolation_value <- function(x, time) {
    if (!.is_interpolation(x)) stop("Expected an interpolation parameter value.", call. = FALSE)
    profile_time <- x$time
    if (inherits(profile_time, "units") != inherits(time, "units")) {
        stop("Interpolation time and model time must both be unit-free or carry compatible units.", call. = FALSE)
    }
    if (inherits(profile_time, "units")) {
        time <- tryCatch(
            units::set_units(time, units::deparse_unit(profile_time), mode = "standard"),
            error = function(e) stop("Interpolation time and model time have incompatible units.", call. = FALSE)
        )
    }
    at <- as.numeric(time)
    scheduled <- as.numeric(profile_time)
    if (at < scheduled[[1]]) {
        stop("Cannot evaluate a time-dependent parameter before its first scheduled time.", call. = FALSE)
    }
    if (identical(x$method, "locf") || length(scheduled) == 1L) {
        return(x$value[[max(which(scheduled <= at))]])
    }
    if (at >= scheduled[[length(scheduled)]]) return(x$value[[length(scheduled)]])
    right <- which(scheduled > at)[[1]]
    left <- right - 1L
    fraction <- (at - scheduled[[left]]) / (scheduled[[right]] - scheduled[[left]])
    x$value[[left]] + fraction * (x$value[[right]] - x$value[[left]])
}

.parameters_at_time <- function(parameters, time) {
    out <- lapply(unclass(parameters), function(x) {
        if (.is_interpolation(x)) .interpolation_value(x, time) else x
    })
    .new_parameters(out)
}

.parameter_representative_values <- function(parameters) {
    out <- lapply(unclass(parameters), function(x) {
        if (.is_interpolation(x)) x$value[[1]] else x
    })
    .new_parameters(out)
}

.parameter_dimension_values <- function(parameters) {
    unlist(lapply(unclass(parameters), function(x) {
        if (.is_interpolation(x)) list(x$time, x$value) else list(x)
    }), recursive = FALSE)
}

.interpolation_expression <- function(x, dimensions) {
    time <- as.numeric(.to_dimensions_value(x$time, dimensions))
    value <- as.numeric(.to_dimensions_value(x$value, dimensions))
    if (length(time) == 1L) return(value[[1]])

    if (identical(x$method, "locf")) {
        expr <- value[[1]]
        for (i in 2:length(time)) {
            expr <- call("ifelse", call(">=", quote(t), time[[i]]), value[[i]], expr)
        }
        return(expr)
    }

    expr <- value[[1]]
    for (i in seq_len(length(time) - 1L)) {
        elapsed <- call(
            "min",
            call("max", call("-", quote(t), time[[i]]), 0),
            time[[i + 1L]] - time[[i]]
        )
        slope <- (value[[i + 1L]] - value[[i]]) / (time[[i + 1L]] - time[[i]])
        expr <- call("+", expr, call("*", slope, elapsed))
    }
    expr
}

.substitute_interpolations <- function(expr, parameters, dimensions) {
    recurse <- function(x) {
        if (is.symbol(x)) {
            nm <- as.character(x)
            value <- parameters[[nm]]
            if (!is.null(value) && .is_interpolation(value)) {
                return(.interpolation_expression(value, dimensions))
            }
            return(x)
        }
        if (is.call(x)) return(as.call(lapply(as.list(x), recurse)))
        x
    }
    recurse(.as_call(expr))
}

.validate_interpolation_start <- function(parameters, time) {
    for (nm in names(parameters)) {
        value <- parameters[[nm]]
        if (!.is_interpolation(value)) next
        tryCatch(
            .interpolation_value(value, time),
            error = function(e) stop(
                "Cannot evaluate time-dependent parameter '", nm,
                "' at the simulation start: ", e$message,
                call. = FALSE
            )
        )
    }
    invisible(NULL)
}
