#' Describe observable evaluation points
#'
#' An observation schedule identifies the times and named model observables to
#' evaluate. It contains no observed or predicted values. Row order, duplicate
#' rows, and additional columns are preserved.
#'
#' @param time Finite numeric observation times, optionally with time units.
#' @param observable Nonempty observable names. A scalar is recycled over
#'   `time`.
#' @param ... Additional columns with one value per schedule row.
#' @returns An `ObservationSchedule` data frame.
#' @examples
#' observation_schedule(
#'     time = c(1, 2, 4) [h],
#'     observable = "C"
#' )
#' @export
observation_schedule <- function(time = numeric(), observable = character(), ...) {
    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    dots <- list(...)
    .observation_check_dots(dots)
    n <- max(c(length(time), length(observable), lengths(dots)), 0L)
    time <- .observation_recycle(time, n, "time")
    observable <- .observation_recycle(observable, n, "observable")
    dots <- lapply(names(dots), function(nm) .observation_recycle(dots[[nm]], n, nm)) |>
        setNames(names(dots))
    x <- do.call(data.frame, c(
        list(time = time, observable = observable), dots,
        list(check.names = FALSE, stringsAsFactors = FALSE)
    ))
    .new_observation_schedule(x)
}

.observation_recycle <- function(x, n, label) {
    if (!n) return(x[FALSE])
    if (length(x) == n) return(x)
    if (length(x) == 1L) return(rep(x, n))
    stop("Observation ", label, " must have length 1 or match the number of rows.", call. = FALSE)
}

.observation_check_dots <- function(x) {
    if (length(x) && (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))))) {
        stop("Additional observation columns must be named.", call. = FALSE)
    }
    invisible(NULL)
}

.new_observation_schedule <- function(x) {
    if (!is.data.frame(x) || anyDuplicated(names(x)) ||
        !all(c("time", "observable") %in% names(x))) {
        stop("An ObservationSchedule must be a data frame with unique time and observable columns.",
             call. = FALSE)
    }
    .experiment_check_time(x$time, "schedule time")
    if (!is.character(x$observable) || anyNA(x$observable) ||
        any(!nzchar(trimws(x$observable)))) {
        stop("Observation schedule observable names must be nonempty character values.", call. = FALSE)
    }
    class(x) <- unique(c("ObservationSchedule", class(x)))
    x
}

#' Store observed or predicted observable values
#'
#' Observation data use the same rows as an [observation_schedule()] and add a
#' canonical `value` column. The values may be measurements, model predictions,
#' synthetic data, or other quantities. Row order, duplicate rows, and
#' additional columns are preserved.
#'
#' @inheritParams observation_schedule
#' @param value Numeric observable values, including `units` or `mixed_units`
#'   values. A scalar is recycled over the rows.
#' @returns An `ObservationData` data frame. It also inherits from
#'   `ObservationSchedule`.
#' @examples
#' observation_data(
#'     time = c(1, 2, 4) [h],
#'     observable = "C",
#'     value = c(8, 6, 3) [mg/L]
#' )
#' @export
observation_data <- function(time = numeric(), observable = character(), value = numeric(), ...) {
    time <- .process_nse_arg(substitute(time), envir = parent.frame())
    value <- .process_nse_arg(substitute(value), envir = parent.frame())
    dots <- list(...)
    .observation_check_dots(dots)
    n <- max(c(length(time), length(observable), length(value), lengths(dots)), 0L)
    time <- .observation_recycle(time, n, "time")
    observable <- .observation_recycle(observable, n, "observable")
    value <- .observation_recycle(value, n, "value")
    dots <- lapply(names(dots), function(nm) .observation_recycle(dots[[nm]], n, nm)) |>
        setNames(names(dots))
    x <- do.call(data.frame, c(
        list(time = time, observable = observable, value = value), dots,
        list(check.names = FALSE, stringsAsFactors = FALSE)
    ))
    .new_observation_data(x)
}

.new_observation_data <- function(x) {
    if (!is.data.frame(x) || anyDuplicated(names(x)) ||
        !all(c("time", "observable", "value") %in% names(x))) {
        stop("ObservationData must be a data frame with unique time, observable, and value columns.",
             call. = FALSE)
    }
    .new_observation_schedule(x)
    value <- x$value
    if ((!is.numeric(value) && !inherits(value, "mixed_units")) || !is.null(dim(value))) {
        stop("Observation data values must be numeric, units, or mixed_units values.", call. = FALSE)
    }
    class(x) <- unique(c("ObservationData", "ObservationSchedule", class(x)))
    x
}

#' Subset observation schedules and data
#'
#' Row subsets retain their class. Column subsets retain `ObservationData` when
#' all three canonical columns remain, retain `ObservationSchedule` when `time`
#' and `observable` remain, and otherwise return an ordinary data frame or
#' vector according to data-frame subsetting rules.
#'
#' @param x An `ObservationSchedule` or `ObservationData`.
#' @param i,j Row and column indices.
#' @param drop Whether to simplify the result.
#' @param ... Unused.
#' @returns A validated observation object when its required columns remain.
#' @export
`[.ObservationSchedule` <- function(x, i, j, ..., drop = FALSE) {
    out <- NextMethod("[")
    if (!is.data.frame(out)) return(out)
    class(out) <- "data.frame"
    if (all(c("time", "observable", "value") %in% names(out))) {
        return(.new_observation_data(out))
    }
    if (all(c("time", "observable") %in% names(out))) {
        return(.new_observation_schedule(out))
    }
    out
}

#' Extract the schedule from observation data
#'
#' @param x An `ObservationSchedule` or `ObservationData` object.
#' @returns An `ObservationSchedule` without the `value` column.
#' @export
as_observation_schedule <- function(x) {
    if (!inherits(x, "ObservationSchedule")) {
        stop("x must be an ObservationSchedule or ObservationData object.", call. = FALSE)
    }
    out <- x[, setdiff(names(x), "value"), drop = FALSE]
    class(out) <- "data.frame"
    .new_observation_schedule(out)
}
