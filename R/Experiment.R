#' Describe an experiment
#'
#' An experiment stores known parameters (including covariates), a fixed dosing
#' schedule, and either an observation schedule or observation data. It does not
#' store a model or an observation error model. Pass it to [simulate()] through
#' the `experiment` argument for ODE simulation. Estimation integration is not
#' yet provided.
#'
#' Observation rows retain their order, duplicates, additional columns, and
#' units. When `data` is supplied, its schedule is stored in `schedule` as well.
#' Supply either `schedule` or `data`, not both. Times need not be sorted. All
#' nonempty schedules must use the same unit mode as `start`: either unit-free,
#' or units convertible to time.
#' Compatible units need not be identical. Doses and observations cannot precede
#' `start`. Empty schedules do not impose a time unit.
#'
#' @param parameters A [Parameters][parameters()] object containing known experimental values.
#'   `NULL` creates an empty parameter collection.
#' @param dosing A [Dosing][dosing()] object. `NULL` creates an empty dosing schedule.
#' @param schedule An [ObservationSchedule][observation_schedule()] describing
#'   which observables to evaluate. `NULL` creates an empty schedule.
#' @param data Optional [ObservationData][observation_data()] containing observed
#'   or predicted values. Its schedule is derived automatically. Cannot be
#'   combined with `schedule`.
#' @param start Initial time, a finite numeric scalar, optionally with units.
#'   Supports unit shorthand such as `0 [h]`. The default `NULL` means zero in
#'   the units of the first nonempty schedule (dosing, then observations), or
#'   unit-free zero if both schedules are empty. It does not mean the earliest
#'   scheduled time. Incompatible schedules still produce an error.
#' @returns An `Experiment` object.
#' @seealso [validate_experiment()]
#' @examples
#' e <- experiment(
#'     parameters = parameters(BW = 70 [kg]),
#'     dosing = dosing(time = 0 [h], amount = 100 [mg]),
#'     schedule = observation_schedule(c(1, 2, 4) [h], "C")
#' )
#' e
#' @export
experiment <- function(
    parameters = NULL,
    dosing = NULL,
    schedule = NULL,
    data = NULL,
    start = NULL
) {
    start <- .process_nse_arg(substitute(start), envir = parent.frame())
    if (is.null(parameters)) parameters <- parameters()
    if (is.null(dosing)) dosing <- dosing()
    if (!is.null(schedule) && !is.null(data)) {
        stop("Supply either schedule or data, not both.", call. = FALSE)
    }
    if (!is.null(data)) {
        .check_class(data, "ObservationData")
        schedule <- as_observation_schedule(data)
    }
    if (is.null(schedule)) schedule <- observation_schedule()
    .check_class(schedule, "ObservationSchedule")
    if (is.null(start)) start <- .experiment_default_start(dosing, schedule)
    x <- structure(
        list(parameters = parameters, dosing = dosing, schedule = schedule, data = data, start = start),
        class = "Experiment"
    )
    validate_experiment(x)
    x
}

.experiment_default_start <- function(dosing, schedule) {
    .check_class(dosing, "Dosing")
    time <- dosing$time
    label <- "dosing time"
    if (!length(time)) {
        time <- schedule$time
        label <- "observation time"
    }
    if (!length(time)) return(0)
    .experiment_check_time(time, label)
    if (inherits(time, "units")) {
        return(units::set_units(0, units::deparse_unit(time), mode = "standard"))
    }
    0
}

#' Validate an experiment
#'
#' Checks the experiment's components and time compatibility. When a model is
#' supplied, every scheduled row must reference a declared observable; state names
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
    .check_class(x$schedule, "ObservationSchedule")
    if (!is.null(x$data)) {
        .check_class(x$data, "ObservationData")
        if (!identical(x$schedule, as_observation_schedule(x$data))) {
            stop("Experiment schedule must match the schedule derived from its data.", call. = FALSE)
        }
    }
    if (length(x$start) != 1L) stop("Experiment start must be a scalar.", call. = FALSE)
    .experiment_check_time(x$start, "start")

    schedule <- x$schedule
    .new_observation_schedule(schedule)
    .experiment_check_time(schedule$time, "observation time", x$start)
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
            stop("Unknown observable(s) in experiment schedule: ",
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
    print(x$parameters)
    print(x$dosing)
    if (nrow(x$schedule)) {
        cat(if (is.null(x$data)) " Schedule:\n" else " Observation data:\n")
        print(if (is.null(x$data)) x$schedule else x$data, row.names = FALSE)
    } else {
        cat(" Schedule: (none)\n")
    }
    invisible(x)
}
