#' Collect experiments
#'
#' Creates a list-like collection of independent individual simulation or
#' estimation units. Element names are individual identifiers; unnamed inputs
#' remain supported, while nonempty names must be unique. Order is preserved.
#' Each element
#' retains its own parameters and units. Use `[` to obtain a collection and
#' `[[` to extract a single experiment.
#'
#' All experiments must use the same time unit mode (unit-free or unit-bearing).
#' compphysiol_parameters with the same name and dose amounts for the same molecule and
#' compartment must have compatible units, or all be unit-free. Convertible
#' units are accepted without changing the stored values. Infusions are compared
#' through their total amounts. Omitted dosing targets match other identically
#' omitted targets; resolving them against explicit targets requires a model
#' and is deferred to model preparation.
#'
#' @param ... `compphysiol_experiment` objects, optionally named with their individual
#'   identifiers. For a list of experiments, use `do.call(experiments, x)`.
#' @returns A `compphysiol_experiments` object.
#' @examples
#' x <- experiments(control = experiment(), treatment = experiment())
#' x
#' x["control"]
#' x[["treatment"]]
#' c(x, experiments(repeat_run = experiment()))
#' @export
experiments <- function(...) {
    x <- list(...)
    .experiments_check_elements(x)
    .check_optional_unique_names(x, "experiment")
    .experiments_check_units(x)
    .new_experiments(x)
}

.new_experiments <- function(x) {
    structure(x, class = .compphysiol_class(
        "compphysiol_experiments", base = "list"
    ))
}

.experiments_check_elements <- function(x) {
    if (!all(vapply(x, inherits, logical(1), "compphysiol_experiment"))) {
        stop("All elements must be experiment objects.", call. = FALSE)
    }
    invisible(NULL)
}

.experiments_check_units <- function(x) {
    parameter_values <- list()
    targets <- list()
    amounts <- list()
    for (i in seq_along(x)) {
        e <- x[[i]]
        context <- paste0(" in experiment ", i)
        .check_compatible_units(x[[1]]$start, e$start, paste0("time", context))
        for (nm in names(e$parameters)) {
            value <- e$parameters[[nm]]
            if (nm %in% names(parameter_values)) {
                .check_compatible_units(parameter_values[[nm]], value,
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
                .check_compatible_units(amounts[[match]], value,
                    paste0("dosing target '", label[1], " in ", label[2], "'", context))
            } else {
                targets[[length(targets) + 1L]] <- target
                amounts[length(amounts) + 1L] <- list(value)
            }
        }
    }
    invisible(NULL)
}

.check_compatible_units <- function(reference, value, label) {
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
#' @param x A `compphysiol_experiments` object.
#' @param i Numeric, logical, or character indices.
#' @param ... Unused.
#' @returns A `compphysiol_experiments` object.
#' @export
`[.compphysiol_experiments` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .check_unique_subset_index(i, "Experiment")
    out <- unclass(x)[i]
    if (any(vapply(out, is.null, logical(1)))) {
        stop("Cannot subset experiments with missing or unknown indices.", call. = FALSE)
    }
    .new_experiments(out)
}

#' Extract an experiment
#' @param x A `compphysiol_experiments` object.
#' @param i Index or name of a single experiment.
#' @param ... Unused.
#' @param exact Whether character indices must match exactly, as for lists.
#' @returns A `compphysiol_experiment` object; unknown names return `NULL`, as for lists.
#' @export
`[[.compphysiol_experiments` <- function(x, i, ..., exact = TRUE) {
    if (length(i) != 1L) stop("Select a single experiment with [[.", call. = FALSE)
    unclass(x)[[i, exact = exact]]
}

#' Combine collections of experiments
#' @param ... `compphysiol_experiments` objects to combine.
#' @returns A `compphysiol_experiments` object, preserving order and names.
#' @export
c.compphysiol_experiments <- function(...) {
    objects <- list(...)
    if (!all(vapply(objects, inherits, logical(1), "compphysiol_experiments"))) {
        stop("All arguments must be experiments objects.", call. = FALSE)
    }
    combined <- do.call(c, unname(lapply(objects, unclass)))
    combined <- combined %||% list()
    .check_optional_unique_names(combined, "experiment")
    .experiments_check_units(combined)
    .new_experiments(combined)
}

#' Print a collection of experiments
#' @param x A `compphysiol_experiments` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.compphysiol_experiments <- function(x, ...) {
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
            nrow(e$observations), if (inherits(e$observations, "compphysiol_observation_data")) {
                " with data"
            } else {
                " scheduled"
            }
        ))
    }
    invisible(x)
}
