#' Describe an experiment
#'
#' One experiment represents one independent individual simulation or
#' estimation unit. It stores known parameters (including covariates), a fixed
#' dosing schedule, and observations represented by either an observation
#' schedule or observation data. Derived covariate relationships belong in
#' model equations rather than in separate subject metadata. An experiment does
#' not store a model or an observation error model. Repeated occasions and state
#' resets are outside the current contract.
#'
#' Pass an experiment to [simulate()] through the `experiment` argument for ODE
#' simulation, or include observation data when using it for estimation.
#'
#' Observation rows retain their order, duplicates, additional columns, and
#' units. `ObservationData` inherits from `ObservationSchedule`, so simulation
#' can use either representation while estimation can require data values.
#' Times need not be sorted. All nonempty observations must use the same unit
#' mode as `start`: either unit-free,
#' or units convertible to time.
#' Compatible units need not be identical. Doses and observations cannot precede
#' `start`. Empty observations do not impose a time unit.
#'
#' @param parameters A [Parameters][parameters()] object containing known
#'   individual and experimental values, including covariates. `NULL` creates
#'   an empty parameter collection.
#' @param dosing A [Dosing][dosing()] object. `NULL` creates an empty dosing schedule.
#' @param observations An [ObservationSchedule][observation_schedule()] or
#'   [ObservationData][observation_data()]. `NULL` creates an empty schedule.
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
#'     observations = observation_schedule(c(1, 2, 4) [h], "C")
#' )
#' e
#' @export
experiment <- function(
    parameters = NULL,
    dosing = NULL,
    observations = NULL,
    start = NULL
) {
    start <- .process_nse_arg(substitute(start), envir = parent.frame())
    if (is.null(parameters)) parameters <- parameters()
    if (is.null(dosing)) dosing <- dosing()
    if (is.null(observations)) observations <- observation_schedule()
    .check_class(observations, "ObservationSchedule")
    if (is.null(start)) start <- .experiment_default_start(dosing, observations)
    x <- structure(
        list(parameters = parameters, dosing = dosing, observations = observations, start = start),
        class = "Experiment"
    )
    validate_experiment(x)
    x
}

.experiment_default_start <- function(dosing, observations) {
    .check_class(dosing, "Dosing")
    time <- dosing$time
    label <- "dosing time"
    if (!length(time)) {
        time <- observations$time
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
    .check_class(x$observations, "ObservationSchedule")
    if (length(x$start) != 1L) stop("Experiment start must be a scalar.", call. = FALSE)
    .experiment_check_time(x$start, "start")

    observations <- x$observations
    if (inherits(observations, "ObservationData")) {
        .new_observation_data(observations)
    } else {
        .new_observation_schedule(observations)
    }
    .experiment_check_time(observations$time, "observation time", x$start)
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
        unknown <- setdiff(observations$observable, names(model$observables))
        if (length(unknown)) {
            stop("Unknown observable(s) in experiment observations: ",
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
    if (nrow(x$observations)) {
        cat(if (inherits(x$observations, "ObservationData")) {
            " Observation data:\n"
        } else {
            " Observation schedule:\n"
        })
        print(x$observations, row.names = FALSE)
    } else {
        cat(" Observations: (none)\n")
    }
    invisible(x)
}
