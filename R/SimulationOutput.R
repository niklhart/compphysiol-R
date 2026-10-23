#' Extract long-format observable predictions
#'
#' Observable output has `time`, `observable`, and `value` columns, plus `rep`
#' for stochastic replicates and `experiment` for experiment-collection
#' simulation. Values are numeric for unit-free output, a `units` vector when
#' all output units are identical, and `mixed_units` otherwise.
#' Experiment schedule order and duplicate observation rows are retained.
#' Both helpers accept an observable table directly, allowing filtering before
#' reshaping without modifying the simulation result.
#' @param x A `compphysiol_simulation_result`, `compphysiol_observation_data`, or a long-format observable
#'   data frame with `time`, `observable`, and `value` columns, and optionally
#'   `rep`.
#' @returns A `compphysiol_observation_data` object containing observable predictions.
#' @export
as_observables_long <- function(x) {
    if (is.data.frame(x)) {
        .observation_check_table(x, require_value = TRUE)
        return(.new_observation_data(x))
    }
    .check_class(x, "compphysiol_simulation_result")
    if (!is.null(x$observables)) return(x$observables)
    out <- x$states[
        FALSE,
        intersect(c("experiment", "time", "rep"), names(x$states)),
        drop = FALSE
    ]
    out$observable <- character()
    out$value <- numeric()
    .new_observation_data(out)
}

#' Extract wide-format observable predictions
#'
#' Each observable becomes a numeric or ordinary `units` column. Missing
#' time-observable combinations are filled with typed `NA`s. Time and optional
#' replicate identify rows. Duplicate keys produce an error rather than implicit
#' aggregation. For collection simulation, `experiment` is part of the key.
#' Units within each observable must be convertible. Observable names must not
#' collide with the identifier columns.
#' @inheritParams as_observables_long
#' @returns A data frame with one column per observable. Rows follow first
#'   occurrence of each experiment/time/replicate key in the long output. With
#'   no observables, a result input returns the state's identifier columns; an
#'   empty table input returns its empty identifier columns. Additional table
#'   columns are ignored.
#' @examples
#' predictions <- data.frame(time = c(0, 1, 0, 1),
#'                           observable = c("A", "A", "B", "B"), value = 1:4)
#' predictions |>
#'     subset(observable == "A") |>
#'     as_observables_wide()
#' @export
as_observables_wide <- function(x) {
    long <- as_observables_long(x)
    ids <- intersect(c("experiment", "time", "rep"), names(long))
    if (!nrow(long)) {
        if (inherits(x, "compphysiol_simulation_result")) return(x$states[, ids, drop = FALSE])
        out <- long[, ids, drop = FALSE]
        class(out) <- "data.frame"
        return(out)
    }
    if (anyDuplicated(long[c(ids, "observable")])) {
        stop("Cannot widen duplicate time-observable keys; distinguish replicates before widening.", call. = FALSE)
    }
    obs <- unique(long$observable)
    if (any(obs %in% ids)) stop("Observable names collide with output identifier columns.", call. = FALSE)
    keys <- long[ids]
    class(keys) <- "data.frame"
    out <- unique(keys)
    rownames(out) <- NULL
    for (nm in obs) {
        rows <- which(long$observable == nm)
        value <- long$value[rows]
        if (inherits(value, "mixed_units")) value <- units::as_units(value)
        key_columns <- intersect(c("experiment", "rep"), ids)
        input_keys <- cbind(long[rows, key_columns, drop = FALSE],
                            time = as.numeric(long$time[rows]))
        output_keys <- cbind(out[key_columns], time = as.numeric(out$time))
        idx <- match(
            do.call(paste, c(input_keys, sep = "\r")),
            do.call(paste, c(output_keys, sep = "\r"))
        )
        column <- value[rep(NA_integer_, nrow(out))]
        column[idx] <- value
        out[[nm]] <- column
    }
    out
}

.simulation_pack_observables <- function(schedule, groups, rows_by_observable = NULL) {
    labels <- vapply(groups, function(x) if (inherits(x, "units")) units::deparse_unit(x) else "", character(1))
    value <- numeric(nrow(schedule))
    row_units <- rep("1", nrow(schedule))
    if (is.null(rows_by_observable)) {
        rows_by_observable <- lapply(names(groups), function(nm) {
            which(schedule$observable == nm)
        })
        names(rows_by_observable) <- names(groups)
    }
    for (nm in names(groups)) {
        idx <- rows_by_observable[[nm]]
        value[idx] <- as.numeric(groups[[nm]])
        if (nzchar(labels[[nm]])) row_units[idx] <- labels[[nm]]
    }
    if (length(unique(labels)) == 1L && nzchar(labels[1])) {
        value <- units::set_units(value, labels[1], mode = "standard")
    } else if (length(unique(labels)) > 1L) {
        value <- units::mixed_units(value, row_units)
    }
    schedule$value <- value
    .new_observation_data(schedule)
}

.simulation_observables <- function(
    solver_output, time, model, odeinfo, solver_time, dimensions,
    parameters = model$parameters, observation_plan = NULL
) {
    if (!length(odeinfo$obsFuncs)) return(NULL)
    schedule <- attr(model, "observation_schedule")
    if (is.null(schedule)) {
        obs <- names(odeinfo$obsFuncs)
        schedule <- data.frame(time = rep(time, times = length(obs)),
                               observable = rep(obs, each = length(time)))
    }
    observation_plan <- observation_plan %||% .simulation_prepare_observation_plan(
        schedule, solver_output[, "time"], dimensions
    )
    groups <- vector("list", length(observation_plan$observable_names))
    names(groups) <- observation_plan$observable_names
    for (evaluation in observation_plan$evaluations) {
        info <- odeinfo
        observable_names <- observation_plan$observable_names[evaluation$observable_indices]
        info$obsFuncs <- info$obsFuncs[observable_names]
        values <- .simulation_observable_columns(
            solver_output[evaluation$solver_indices, , drop = FALSE],
            time[evaluation$solver_indices], model, info,
            solver_output[evaluation$solver_indices, "time"], dimensions, parameters
        )
        for (observable_index in evaluation$observable_indices) {
            nm <- observation_plan$observable_names[[observable_index]]
            groups[[observable_index]] <- values[[nm]][
                observation_plan$rematch[[observable_index]]
            ]
        }
    }
    .simulation_pack_observables(
        schedule, groups, rows_by_observable = observation_plan$rows_by_observable
    )
}

.simulation_prepare_observation_plan <- function(schedule, solver_time, dimensions) {
    observable_names <- unique(schedule$observable)
    rows_by_observable <- lapply(observable_names, function(nm) {
        which(schedule$observable == nm)
    })
    names(rows_by_observable) <- observable_names
    solver_indices <- vector("list", length(observable_names))
    rematch <- vector("list", length(observable_names))
    for (i in seq_along(observable_names)) {
        rows <- rows_by_observable[[i]]
        requested <- .simulation_numeric_time(schedule$time[rows], dimensions)
        idx <- match(requested, solver_time)
        if (anyNA(idx)) stop("Solver output is missing requested observation times.", call. = FALSE)
        solver_indices[[i]] <- sort(unique(idx))
        rematch[[i]] <- match(idx, solver_indices[[i]])
    }
    evaluations <- list()
    for (i in seq_along(observable_names)) {
        matching <- which(vapply(evaluations, function(x) {
            identical(x$solver_indices, solver_indices[[i]])
        }, logical(1)))
        if (length(matching)) {
            j <- matching[[1]]
            evaluations[[j]]$observable_indices <- c(
                evaluations[[j]]$observable_indices, i
            )
        } else {
            evaluations[[length(evaluations) + 1L]] <- list(
                observable_indices = i,
                solver_indices = solver_indices[[i]]
            )
        }
    }
    list(
        observable_names = observable_names,
        rows_by_observable = rows_by_observable,
        rematch = rematch,
        evaluations = evaluations
    )
}
