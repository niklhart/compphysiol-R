#' Collect experiments
#'
#' Creates a list-like collection of experiments. Optional names, order, and
#' duplicates are preserved. Each element retains its own parameters and units.
#' Use `[` to obtain a collection and `[[` to extract a single experiment.
#'
#' All experiments must use the same time unit mode (unit-free or unit-bearing).
#' Parameters with the same name and dose amounts for the same molecule and
#' compartment must have compatible units, or all be unit-free. Convertible
#' units are accepted without changing the stored values. Infusions are compared
#' through their total amounts. Omitted dosing targets match other identically
#' omitted targets; resolving them against explicit targets requires a model
#' and is deferred to model preparation.
#'
#' @param ... `Experiment` objects, optionally named. For a list of experiments,
#'   use `do.call(experiments, x)`.
#' @returns An `Experiments` object.
#' @examples
#' x <- experiments(control = experiment(), treatment = experiment())
#' x
#' x["control"]
#' x[["treatment"]]
#' c(x, experiments(repeat_run = experiment()))
#' @export
experiments <- function(...) {
    .new_experiments(list(...))
}

.new_experiments <- function(x) {
    if (!all(vapply(x, inherits, logical(1), "Experiment"))) {
        stop("All elements must be Experiment objects.", call. = FALSE)
    }
    invisible(lapply(x, validate_experiment))
    .experiments_check_units(x)
    structure(x, class = "Experiments")
}

.experiments_check_units <- function(x) {
    parameter_values <- list()
    targets <- list()
    amounts <- list()
    for (i in seq_along(x)) {
        e <- x[[i]]
        context <- paste0(" in experiment ", i)
        .experiments_check_compatible_units(x[[1]]$start, e$start,
                                            paste0("time", context))
        for (nm in names(e$parameters)) {
            value <- e$parameters[[nm]]
            if (nm %in% names(parameter_values)) {
                .experiments_check_compatible_units(parameter_values[[nm]], value,
                    paste0("parameter '", nm, "'", context))
            } else {
                parameter_values[nm] <- list(value)
            }
        }
        for (j in seq_along(e$dosing$time)) {
            target <- c(e$dosing$molec[j], e$dosing$cmt[j])
            match <- which(vapply(targets, identical, logical(1), target))
            value <- e$dosing$amount[[j]]
            if (length(match)) {
                label <- ifelse(is.na(target), "<unspecified>", target)
                .experiments_check_compatible_units(amounts[[match]], value,
                    paste0("dosing target '", label[1], " in ", label[2], "'", context))
            } else {
                targets[[length(targets) + 1L]] <- target
                amounts[length(amounts) + 1L] <- list(value)
            }
        }
    }
    invisible(NULL)
}

.experiments_check_compatible_units <- function(reference, value, label) {
    reference_has_units <- inherits(reference, "units")
    value_has_units <- inherits(value, "units")
    compatible <- identical(reference_has_units, value_has_units)
    if (compatible && reference_has_units) {
        compatible <- units::ud_are_convertible(
            units::deparse_unit(reference), units::deparse_unit(value)
        )
    }
    if (!compatible) {
        stop("Inconsistent units for ", label,
             ": values must both be unit-free or have convertible units.", call. = FALSE)
    }
    invisible(NULL)
}

#' Subset a collection of experiments
#'
#' Missing or unknown indices that would introduce missing elements are rejected.
#' @param x An `Experiments` object.
#' @param i Numeric, logical, or character indices.
#' @param ... Unused.
#' @returns An `Experiments` object.
#' @export
`[.Experiments` <- function(x, i, ...) {
    .new_experiments(unclass(x)[i])
}

#' Extract an experiment
#' @param x An `Experiments` object.
#' @param i Index or name of a single experiment.
#' @param ... Unused.
#' @param exact Whether character indices must match exactly, as for lists.
#' @returns An `Experiment` object; unknown names return `NULL`, as for lists.
#' @export
`[[.Experiments` <- function(x, i, ..., exact = TRUE) {
    if (length(i) != 1L) stop("Select a single experiment with [[.", call. = FALSE)
    unclass(x)[[i, exact = exact]]
}

#' Combine collections of experiments
#' @param ... `Experiments` objects to combine.
#' @returns An `Experiments` object, preserving order and names.
#' @export
c.Experiments <- function(...) {
    objects <- list(...)
    if (!all(vapply(objects, inherits, logical(1), "Experiments"))) {
        stop("All arguments must be Experiments objects.", call. = FALSE)
    }
    combined <- do.call(c, unname(lapply(objects, unclass)))
    .new_experiments(combined %||% list())
}

#' Print a collection of experiments
#' @param x An `Experiments` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.Experiments <- function(x, ...) {
    if (!length(x)) {
        cat(" Experiments: (none)\n")
        return(invisible(x))
    }
    cat(" Experiments:\n")
    labels <- names(x)
    for (i in seq_along(x)) {
        e <- x[[i]]
        label <- if (!is.null(labels) && !is.na(labels[i]) && nzchar(labels[i])) {
            paste0(labels[i], ": ")
        } else ""
        cat(sprintf(
            "  (%s) %sstart = %s; %s parameters; %s dosing events; %s observations%s\n",
            i, label, format(e$start), length(e$parameters), length(e$dosing),
            nrow(e$schedule), if (is.null(e$data)) " scheduled" else " with data"
        ))
    }
    invisible(x)
}
