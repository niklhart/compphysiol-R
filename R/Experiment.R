#' Describe an experiment
#'
#' An experiment stores known parameters (including covariates), a fixed dosing
#' schedule, and the times and observables to measure. It does not store a model
#' or measured values. Simulation and estimation integration is not yet provided.
#'
#' Measurement rows retain their order, duplicates, additional columns, and
#' units. Times need not be sorted. All nonempty schedules must use the same
#' unit mode as `start`: either unit-free, or units convertible to time.
#' Compatible units need not be identical. Doses and measurements cannot precede
#' `start`. Empty schedules do not impose a time unit.
#'
#' @param parameters A [Parameters] object containing known experimental values.
#'   `NULL` creates an empty parameter collection.
#' @param dosing A [Dosing] object. `NULL` creates an empty dosing schedule.
#' @param measurements A data frame with numeric `time` and nonempty character
#'   `observable` columns. Use [with_units()] to construct unit-bearing times.
#' @param start Initial time, a finite numeric scalar, optionally with units.
#'   Supports unit shorthand such as `0 [h]`. Defaults to unit-free zero;
#'   supply units explicitly for a unit-aware experiment.
#' @returns An `Experiment` object.
#' @seealso [validate_experiment()]
#' @examples
#' e <- experiment(
#'     parameters = parameters(BW = 70 [kg]),
#'     dosing = dosing(time = 0 [h], amount = 100 [mg]),
#'     measurements = data.frame(
#'         time = with_units(c(1, 2, 4) [h]),
#'         observable = "C"
#'     ),
#'     start = 0 [h]
#' )
#' e
#' @export
experiment <- function(
    parameters = NULL,
    dosing = NULL,
    measurements = data.frame(time = numeric(0), observable = character(0)),
    start = 0
) {
    start <- .process_nse_arg(substitute(start), envir = parent.frame())
    if (is.null(parameters)) parameters <- parameters()
    if (is.null(dosing)) dosing <- dosing()
    x <- structure(
        list(parameters = parameters, dosing = dosing, measurements = measurements, start = start),
        class = "Experiment"
    )
    validate_experiment(x)
    x
}

#' Validate an experiment
#'
#' Checks the experiment's components and time compatibility. When a model is
#' supplied, every measurement must reference a declared observable; state names
#' are not accepted unless explicitly declared as observable names. This does
#' not wire dosing targets, resolve model parameters, or check model equations.
#'
#' @param x An `Experiment` object.
#' @param model Optional `CompartmentModel` against which to check observable names.
#' @returns `x`, invisibly, after successful validation.
#' @export
validate_experiment <- function(x, model = NULL) {
    .check_class(x, "Experiment")
    .check_class(x$parameters, "Parameters")
    .check_class(x$dosing, "Dosing")
    if (length(x$start) != 1L) stop("Experiment start must be a scalar.", call. = FALSE)
    .experiment_check_time(x$start, "start")

    schedule <- x$measurements
    if (!is.data.frame(schedule) || anyDuplicated(names(schedule)) ||
        !all(c("time", "observable") %in% names(schedule))) {
        stop("Experiment measurements must be a data frame with time and observable columns.", call. = FALSE)
    }
    if (!is.character(schedule$observable) || anyNA(schedule$observable) ||
        any(!nzchar(trimws(schedule$observable)))) {
        stop("Experiment measurement observable names must be nonempty character values.", call. = FALSE)
    }
    .experiment_check_time(schedule$time, "measurement time", x$start)
    .experiment_check_time(x$dosing$time, "dosing time", x$start)
    infusion <- is_infusion(x$dosing)
    if (any(infusion)) {
        duration <- x$dosing$duration[infusion]
        .experiment_check_time(duration, "infusion duration", x$start, check_start = FALSE)
        if (any(as.numeric(duration) <= 0)) {
            stop("Experiment infusion duration must be positive.", call. = FALSE)
        }
    }
    if (!is.null(model)) {
        .check_class(model, "CompartmentModel")
        unknown <- setdiff(schedule$observable, names(model$observables))
        if (length(unknown)) {
            stop("Unknown observable(s) in experiment measurements: ",
                 paste(unknown, collapse = ", "), ".", call. = FALSE)
        }
    }
    invisible(x)
}

.experiment_check_time <- function(value, label, start = NULL, check_start = TRUE) {
    if (!is.numeric(value) || !is.null(dim(value)) || any(!is.finite(value))) {
        stop("Experiment ", label, " must contain finite numeric values.", call. = FALSE)
    }
    if (!length(value)) return(invisible(NULL))
    has_units <- inherits(value, "units")
    if (has_units && !units::ud_are_convertible(units::deparse_unit(value), "s")) {
        stop("Experiment ", label, " must have time units.", call. = FALSE)
    }
    if (!is.null(start)) {
        if (has_units != inherits(start, "units")) {
            stop("Experiment ", label, " and start must both have time units or both be unit-free.", call. = FALSE)
        }
        if (check_start && any(value < start)) {
            stop("Experiment ", label, " cannot be before the experiment start.", call. = FALSE)
        }
    }
    invisible(NULL)
}

#' Print an experiment
#' @param x An `Experiment` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.Experiment <- function(x, ...) {
    cat("Experiment:\n")
    cat(" Start: ", format(x$start), "\n", sep = "")
    cat(" Parameters: ", length(x$parameters), "\n", sep = "")
    cat(" Dosing events: ", length(x$dosing), "\n", sep = "")
    cat(" Measurements: ", nrow(x$measurements), "\n", sep = "")
    invisible(x)
}
