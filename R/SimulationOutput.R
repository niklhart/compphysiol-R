#' Extract long-format observable predictions
#'
#' Observable output has `time`, `observable`, and `value` columns, plus `rep`
#' for stochastic replicates. Values are numeric for unit-free output, a `units`
#' vector when all output units are identical, and `mixed_units` otherwise.
#' Experiment schedule order and duplicate measurement rows are retained.
#' Both helpers accept an observable table directly, allowing filtering before
#' reshaping without modifying the simulation result.
#' @param x A `SimulationResult` or a long-format observable data frame with
#'   `time`, `observable`, and `value` columns, and optionally `rep`.
#' @returns A data frame of observable predictions.
#' @export
as_observables_long <- function(x) {
    if (is.data.frame(x)) {
        if (anyDuplicated(names(x)) || !all(c("time", "observable", "value") %in% names(x))) {
            stop("Observable tables require time, observable, and value columns with unique names.", call. = FALSE)
        }
        return(x)
    }
    .check_class(x, "SimulationResult")
    if (!is.null(x$observables)) return(x$observables)
    out <- x$states[FALSE, intersect(c("time", "rep"), names(x$states)), drop = FALSE]
    out$observable <- character()
    out$value <- numeric()
    out
}

#' Extract wide-format observable predictions
#'
#' Each observable becomes a numeric or ordinary `units` column. Missing
#' time-observable combinations are filled with typed `NA`s. Time and optional
#' replicate identify rows. Duplicate keys produce an error rather than implicit
#' aggregation. Units within each observable must be convertible. Observable
#' names must not collide with the identifier columns.
#' @inheritParams as_observables_long
#' @returns A data frame with one column per observable. Rows follow first
#'   occurrence of each time/replicate key in the long output. With no observables,
#'   a result input returns the state's time and replicate columns; an empty
#'   table input returns its empty identifier columns. Additional table columns
#'   are ignored.
#' @examples
#' predictions <- data.frame(time = c(0, 1, 0, 1),
#'                           observable = c("A", "A", "B", "B"), value = 1:4)
#' predictions |>
#'     subset(observable == "A") |>
#'     as_observables_wide()
#' @export
as_observables_wide <- function(x) {
    long <- as_observables_long(x)
    ids <- intersect(c("time", "rep"), names(long))
    if (!nrow(long)) {
        if (inherits(x, "SimulationResult")) return(x$states[, ids, drop = FALSE])
        return(long[, ids, drop = FALSE])
    }
    if (anyDuplicated(long[c(ids, "observable")])) {
        stop("Cannot widen duplicate time-observable keys; distinguish replicates before widening.", call. = FALSE)
    }
    obs <- unique(long$observable)
    if (any(obs %in% ids)) stop("Observable names collide with output identifier columns.", call. = FALSE)
    keys <- long[ids]
    out <- unique(keys)
    rownames(out) <- NULL
    for (nm in obs) {
        rows <- which(long$observable == nm)
        value <- long$value[rows]
        if (inherits(value, "mixed_units")) value <- units::as_units(value)
        idx <- match(as.numeric(long$time[rows]), as.numeric(out$time))
        if ("rep" %in% ids) {
            idx <- match(paste(long$rep[rows], as.numeric(long$time[rows])),
                         paste(out$rep, as.numeric(out$time)))
        }
        column <- value[rep(NA_integer_, nrow(out))]
        column[idx] <- value
        out[[nm]] <- column
    }
    out
}

.simulation_pack_observables <- function(schedule, groups) {
    labels <- vapply(groups, function(x) if (inherits(x, "units")) units::deparse_unit(x) else "", character(1))
    value <- numeric(nrow(schedule))
    row_units <- rep("1", nrow(schedule))
    for (nm in names(groups)) {
        idx <- which(schedule$observable == nm)
        value[idx] <- as.numeric(groups[[nm]])
        if (nzchar(labels[[nm]])) row_units[idx] <- labels[[nm]]
    }
    if (length(unique(labels)) == 1L && nzchar(labels[1])) {
        value <- units::set_units(value, labels[1], mode = "standard")
    } else if (length(unique(labels)) > 1L) {
        value <- units::mixed_units(value, row_units)
    }
    schedule$value <- value
    schedule
}

.simulation_observables <- function(solver_output, time, model, odeinfo, solver_time, dimensions, parameters = model$parameters) {
    if (!length(odeinfo$obsFuncs)) return(NULL)
    schedule <- attr(model, "measurement_schedule")
    if (is.null(schedule)) {
        wide <- .simulation_observable_columns(solver_output, time, model, odeinfo,
                                               solver_time, dimensions, parameters)
        obs <- names(odeinfo$obsFuncs)
        schedule <- data.frame(time = rep(time, times = length(obs)),
                               observable = rep(obs, each = length(time)))
        return(.simulation_pack_observables(schedule, wide[obs]))
    }
    groups <- lapply(unique(schedule$observable), function(nm) {
        rows <- which(schedule$observable == nm)
        requested <- .simulation_numeric_time(schedule$time[rows], dimensions)
        idx <- match(requested, solver_output[, "time"])
        if (anyNA(idx)) stop("Solver output is missing requested measurement times.", call. = FALSE)
        unique_idx <- unique(idx)
        info <- odeinfo
        info$obsFuncs <- info$obsFuncs[nm]
        values <- .simulation_observable_columns(
            solver_output[unique_idx, , drop = FALSE], time[unique_idx], model, info,
            solver_output[unique_idx, "time"], dimensions, parameters
        )[[nm]]
        values[match(idx, unique_idx)]
    })
    names(groups) <- unique(schedule$observable)
    .simulation_pack_observables(schedule[c("time", "observable")], groups)
}
