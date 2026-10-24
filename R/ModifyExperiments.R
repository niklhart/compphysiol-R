# Component-specific experiment modification ---------------------------------

.rebuild_experiment <- function(entry, component, value) {
    entry[[component]] <- value
    .experiment_entry(experiment(
        parameters = entry$parameters,
        dosing = entry$dosing,
        observations = entry$observations,
        start = entry$start
    ))
}

.modify_experiments_scalar <- function(object, component, input, combine) {
    .check_class(object, "compphysiol_experiments")
    entries <- .experiments_elements(object)
    out <- lapply(entries, function(entry) {
        incoming <- if (is.function(input)) input(entry[[component]]) else input
        value <- combine(entry[[component]], incoming)
        .rebuild_experiment(entry, component, value)
    })
    names(out) <- names(entries)
    .experiments_validate_result(out)
}

.align_parameter_sets <- function(object, sets) {
    entries <- .experiments_elements(object)
    incoming <- .parameter_sets_elements(sets)
    if (length(entries) == 1L) {
        entries <- rep(entries, length(incoming))
        names(entries) <- names(incoming)
        return(list(entries = entries, incoming = incoming))
    }
    if (length(entries) != length(incoming)) {
        stop("Parameter sets must match the number of experiments.", call. = FALSE)
    }
    entry_names <- names(entries)
    set_names <- names(incoming)
    wholly_unnamed <- (is.null(entry_names) || all(!nzchar(entry_names))) &&
        (is.null(set_names) || all(!nzchar(set_names)))
    if (!wholly_unnamed) {
        entry_ids <- .collection_identifiers(entries, "Experiment")
        set_ids <- .collection_identifiers(incoming, "Parameter set")
        positions <- match(entry_ids, set_ids)
        if (anyNA(positions) || anyDuplicated(positions)) {
            stop("Parameter sets must have the same experiment identifiers.", call. = FALSE)
        }
        incoming <- incoming[positions]
    }
    list(entries = entries, incoming = incoming)
}

.modify_experiment_parameters <- function(object, param, verb) {
    if (is.function(param) || inherits(param, "compphysiol_parameters")) {
        return(.modify_experiments_scalar(object, "parameters", param, verb))
    }
    .check_class(param, "compphysiol_parameter_sets")
    aligned <- .align_parameter_sets(object, param)
    out <- Map(function(entry, incoming) {
        .rebuild_experiment(entry, "parameters", verb(entry$parameters, incoming))
    }, aligned$entries, aligned$incoming)
    names(out) <- names(aligned$entries)
    .experiments_validate_result(out)
}

#' @name modify_parameters
#' @export
add_parameter.compphysiol_experiments <- function(
    object, param, ..., name = NULL, value = NULL, unit = NULL
) {
    param <- .parameter_input(match.call(), parent.frame())
    .modify_experiment_parameters(object, param, function(current, incoming) {
        .check_class(incoming, "compphysiol_parameters")
        conflicts <- intersect(names(current), names(incoming))
        if (length(conflicts)) stop("Parameters already exist: ", paste(conflicts, collapse = ", "), ".", call. = FALSE)
        c(current, incoming)
    })
}

#' @rdname modify_parameters
#' @export
overlay_parameter.compphysiol_experiments <- function(
    object, param, ..., name = NULL, value = NULL, unit = NULL
) {
    param <- .parameter_input(match.call(), parent.frame())
    .modify_experiment_parameters(object, param, .parameters_overlay)
}

#' @rdname modify_parameters
#' @export
replace_parameters.compphysiol_experiments <- function(
    object, param, ..., name = NULL, value = NULL, unit = NULL
) {
    param <- .parameter_input(match.call(), parent.frame())
    .modify_experiment_parameters(object, param, function(current, incoming) {
        .check_class(incoming, "compphysiol_parameters")
        incoming
    })
}

.dosing_input <- function(call, envir) {
    .forward_or_use("dose", "dosing", call, envir)
}

#' @rdname modify_dosing
#' @export
add_dosing.compphysiol_experiments <- function(
    object, dose, ..., time = numeric(0), amount = NULL, time_unit = NULL,
    amount_unit = NULL, molec = NULL, cmt = NULL, rate = NULL, duration = NULL
) {
    dose <- .dosing_input(match.call(), parent.frame())
    .modify_experiments_scalar(object, "dosing", dose, function(current, incoming) {
        .check_class(incoming, "compphysiol_dosing")
        c(current, incoming)
    })
}

#' @rdname modify_dosing
#' @export
replace_dosing <- function(
    object, dose, ..., time = numeric(0), amount = NULL, time_unit = NULL,
    amount_unit = NULL, molec = NULL, cmt = NULL, rate = NULL, duration = NULL
) UseMethod("replace_dosing")

#' @rdname modify_dosing
#' @export
replace_dosing.compphysiol_experiments <- function(
    object, dose, ..., time = numeric(0), amount = NULL, time_unit = NULL,
    amount_unit = NULL, molec = NULL, cmt = NULL, rate = NULL, duration = NULL
) {
    dose <- .dosing_input(match.call(), parent.frame())
    .modify_experiments_scalar(object, "dosing", dose, function(current, incoming) {
        .check_class(incoming, "compphysiol_dosing")
        incoming
    })
}

.split_bound_observations <- function(x) {
    if (!inherits(x, "compphysiol_observation_schedule") || !"experiment" %in% names(x)) return(NULL)
    ids <- as.character(x$experiment)
    if (anyNA(ids) || any(!nzchar(ids))) stop("Observation experiment identifiers must be non-missing and nonempty.", call. = FALSE)
    levels <- if (is.factor(x$experiment)) levels(x$experiment) else unique(ids)
    values <- lapply(levels, function(id) {
        out <- as.data.frame(x[ids == id, , drop = FALSE])
        out$experiment <- NULL
        rownames(out) <- NULL
        if (inherits(x, "compphysiol_observation_data")) .new_observation_data(out) else .new_observation_schedule(out)
    })
    names(values) <- levels
    values
}

.align_bound_observations <- function(object, bound) {
    entries <- .experiments_elements(object)
    ids <- .collection_identifiers(entries, "Experiment")
    positions <- match(ids, names(bound))
    if (length(bound) != length(entries) || anyNA(positions) || anyDuplicated(positions)) {
        stop("Bound observations must have the same experiment identifiers.", call. = FALSE)
    }
    bound[positions]
}

#' Modify experiment observations
#'
#' `replace_observations()` replaces the complete observation component with a
#' schedule or observation-data object. `attach_observation_values()` combines
#' each experiment's existing schedule with corresponding values to produce
#' observation data. Scalar objects broadcast across experiments; bound objects
#' carrying an `experiment` column split and align by identifier.
#'
#' Observation callbacks receive each experiment's current observations.
#' Attachment callbacks may return a value vector or corresponding observation
#' data. Attachment preserves schedule rows, duplicates, units, and additional
#' metadata, and validates rowwise correspondence.
#'
#' @param object A `compphysiol_experiments` object.
#' @param observations A schedule, observation data, bound observation object,
#'   or callback.
#' @param values A value vector, corresponding observation data, or callback.
#' @returns A `compphysiol_experiments` object.
#' @seealso [observation_schedule()] and [observation_data()] for construction;
#'   [observations()] for collecting observations; [modify_experiments] for
#'   other modifiable experiment components.
#' @name modify_experiment_observations
#' @export
replace_observations <- function(object, observations) UseMethod("replace_observations")

#' @rdname modify_experiment_observations
#' @export
replace_observations.compphysiol_experiments <- function(object, observations) {
    bound <- if (is.function(observations)) NULL else .split_bound_observations(observations)
    if (is.null(bound)) {
        return(.modify_experiments_scalar(object, "observations", observations, function(current, incoming) {
            .check_class(incoming, "compphysiol_observation_schedule")
            incoming
        }))
    }
    aligned <- .align_bound_observations(object, bound)
    entries <- .experiments_elements(object)
    out <- Map(function(entry, incoming) .rebuild_experiment(entry, "observations", incoming), entries, aligned)
    names(out) <- names(entries)
    .experiments_validate_result(out)
}

.attach_values <- function(schedule, values) {
    if (inherits(schedule, "compphysiol_observation_data")) schedule <- as_observation_schedule(schedule)
    if (inherits(values, "compphysiol_observation_data")) {
        shared <- intersect(names(schedule), setdiff(names(values), c("value", "experiment")))
        if (nrow(values) != nrow(schedule) || !all(vapply(shared, function(nm) identical(schedule[[nm]], values[[nm]]), logical(1)))) {
            stop("Observation data do not correspond rowwise to the schedule.", call. = FALSE)
        }
        values <- values$value
    }
    if ((!is.numeric(values) && !inherits(values, "mixed_units")) || !is.null(dim(values)) || length(values) != nrow(schedule)) {
        stop("Observation values must be a numeric vector matching the schedule rows.", call. = FALSE)
    }
    out <- as.data.frame(schedule)
    out$value <- values
    .observation_check_table(out, require_value = TRUE)
    .new_observation_data(out)
}

#' @rdname modify_experiment_observations
#' @export
attach_observation_values <- function(object, values) UseMethod("attach_observation_values")

#' @rdname modify_experiment_observations
#' @export
attach_observation_values.compphysiol_experiments <- function(object, values) {
    bound <- if (is.function(values)) NULL else .split_bound_observations(values)
    aligned <- if (is.null(bound)) NULL else .align_bound_observations(object, bound)
    entries <- .experiments_elements(object)
    out <- lapply(seq_along(entries), function(i) {
        schedule <- entries[[i]]$observations
        incoming <- if (is.function(values)) values(schedule) else if (!is.null(aligned)) aligned[[i]] else values
        .rebuild_experiment(entries[[i]], "observations", .attach_values(schedule, incoming))
    })
    names(out) <- names(entries)
    .experiments_validate_result(out)
}

#' Replace experiment start times
#' @param object A `compphysiol_experiments` object.
#' @param start A finite numeric scalar, optionally with units, or a callback.
#' @returns A `compphysiol_experiments` object.
#' @seealso [experiment()] for setting the initial start and
#'   [modify_experiments] for other modifiable experiment components.
#' @export
replace_start <- function(object, start) {
    UseMethod("replace_start")
}

#' @rdname replace_start
#' @export
replace_start.compphysiol_experiments <- function(object, start) {
    generic_call <- match.call(replace_start, call = sys.call(-1L))
    start <- .process_nse_arg(generic_call$start, parent.frame(2L))
    .modify_experiments_scalar(object, "start", start, function(current, incoming) {
        if (!is.numeric(incoming) || length(incoming) != 1L || !is.finite(incoming)) stop("start must be a finite numeric scalar.", call. = FALSE)
        incoming
    })
}
