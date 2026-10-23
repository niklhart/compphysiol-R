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
#' Experiments are collections at every length. Use `c()` to combine them.
#' Existing names are preserved unless an input is given an outer argument name
#' in `c()`; that outer name replaces the name stored on the length-one input.
#' Positive whole-number names are reserved for transient positional identifiers.
#'
#' Pass an experiment to [simulate()] through the `experiment` argument for ODE
#' simulation, or include observation data when using it for estimation.
#'
#' Observation rows retain their order, duplicates, additional columns, and
#' units. `compphysiol_observation_data` inherits from `compphysiol_observation_schedule`, so simulation
#' can use either representation while estimation can require data values.
#' Times need not be sorted. All nonempty observations must use the same unit
#' mode as `start`: either unit-free,
#' or units convertible to time.
#' Compatible units need not be identical. Doses and observations cannot precede
#' `start`. Empty observations do not impose a time unit.
#'
#' @param parameters A [compphysiol_parameters][parameters()] object containing known
#'   individual and experimental values, including covariates. `NULL` creates
#'   an empty parameter collection.
#' @param dosing A [compphysiol_dosing][dosing()] object. `NULL` creates an empty dosing schedule.
#' @param observations An [compphysiol_observation_schedule][observation_schedule()] or
#'   [compphysiol_observation_data][observation_data()]. `NULL` creates an empty schedule.
#' @param start Initial time, a finite numeric scalar, optionally with units.
#'   Supports unit shorthand such as `0 [h]`. The default `NULL` means zero in
#'   the units of the first nonempty schedule (dosing, then observations), or
#'   unit-free zero if both schedules are empty. It does not mean the earliest
#'   scheduled time. Incompatible schedules still produce an error.
#' @param name Optional experiment identifier. It must be a nonempty character
#'   scalar and cannot be a positive whole number, which is reserved for
#'   transient positional identifiers.
#' @returns `experiment()` returns a length-one `compphysiol_experiments`
#'   object. `c()` returns the combined `compphysiol_experiments` collection.
#' @examples
#' e <- experiment(
#'     parameters = parameters(BW = 70 [kg]),
#'     dosing = dosing(time = 0 [h], amount = 100 [mg]),
#'     observations = observation_schedule(c(1, 2, 4) [h], "C")
#' )
#' e
#' c(control = e, treatment = experiment())
#' @export
experiment <- function(
    parameters = NULL,
    dosing = NULL,
    observations = NULL,
    start = NULL,
    name = NULL
) {
    start <- .process_nse_arg(substitute(start), envir = parent.frame())
    if (is.null(parameters)) parameters <- parameters()
    if (is.null(dosing)) dosing <- dosing()
    if (is.null(observations)) observations <- observation_schedule()
    .check_class(parameters, "compphysiol_parameters")
    .check_class(dosing, "compphysiol_dosing")
    .check_class(observations, "compphysiol_observation_schedule")
    if (is.null(start)) start <- .experiment_default_start(dosing, observations)
    if (length(start) != 1L) stop("Experiment start must be a scalar.", call. = FALSE)
    .experiment_check_time(start, "start")
    .experiment_check_time(observations$time, "observation time", start)
    .experiment_check_time(dosing$time, "dosing time", start)
    infusion <- is_infusion(dosing)
    if (any(infusion)) {
        infusion_duration <- dosing$duration[infusion]
        .experiment_check_time(infusion_duration, "infusion duration", start, check_start = FALSE)
        if (any(as.numeric(infusion_duration) <= 0)) {
            stop("Experiment infusion duration must be positive.", call. = FALSE)
        }
    }
    entry <- .new_experiment(list(
        parameters = parameters, dosing = dosing, observations = observations, start = start
    ))
    entries <- if (is.null(name)) list(entry) else {
        if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
            stop("Experiment name must be NULL or one nonempty character value.", call. = FALSE)
        }
        stats::setNames(list(entry), name)
    }
    .check_optional_unique_names(entries, "experiment")
    .collection_identifiers(entries, "Experiment")
    .new_experiments(entries, .experiments_compute_component_homogeneity(entries))
}

.new_experiment <- function(x) {
    x
}

.experiment_default_start <- function(dosing, observations) {
    time <- dosing$time
    if (!length(time)) {
        time <- observations$time
    }
    if (!length(time)) return(0)
    if (inherits(time, "units")) {
        return(units::set_units(0, units::deparse_unit(time), mode = "standard"))
    }
    0
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

.print_experiment_entry <- function(x, name = NULL) {
    named <- !is.null(name) && length(name) == 1L && !is.na(name) && nzchar(name)
    cat("Experiment:", if (named) paste0(" ", name), "\n", sep = "")
    cat(" Start: ", format(x$start), "\n", sep = "")
    print(x$parameters)
    print(x$dosing)
    if (nrow(x$observations)) {
        cat(if (inherits(x$observations, "compphysiol_observation_data")) {
            " Observation data:\n"
        } else {
            " Observation schedule:\n"
        })
        print(x$observations, row.names = FALSE)
    } else {
        cat(" Observations: (none)\n")
    }
    invisible(NULL)
}
