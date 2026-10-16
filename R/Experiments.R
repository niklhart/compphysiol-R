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
    .new_experiments(x, .experiments_compute_component_homogeneity(x))
}

.new_experiments <- function(x, component_homogeneity = NULL) {
    structure(
        x,
        component_homogeneity = component_homogeneity,
        class = .compphysiol_class("compphysiol_experiments", base = "list")
    )
}

.experiments_elements <- function(x) {
    out <- unclass(x)
    attr(out, "component_homogeneity") <- NULL
    out
}

.experiments_component_names <- function() {
    c(
        "start", "parameter_schema", "parameters", "dosing",
        "observation_plan", "observations"
    )
}

.experiments_component_homogeneity <- function(x) {
    out <- attr(x, "component_homogeneity", exact = TRUE)
    if (is.null(out)) {
        out <- .experiments_compute_component_homogeneity(.experiments_elements(x))
    }
    out
}

.experiments_compute_component_homogeneity <- function(
    x, categories = .experiments_component_names()
) {
    out <- stats::setNames(rep(TRUE, length(categories)), categories)
    if (length(x) < 2L) return(out)

    reference <- x[[1]]
    for (category in categories) {
        for (i in seq.int(2L, length(x))) {
            if (!.experiments_components_identical(reference, x[[i]], category)) {
                out[[category]] <- FALSE
                break
            }
        }
    }
    out
}

.experiments_components_identical <- function(x, y, category) {
    switch(
        category,
        start = identical(x$start, y$start),
        parameter_schema = .parameters_schema_identical(x$parameters, y$parameters),
        parameters = identical(x$parameters, y$parameters),
        dosing = identical(x$dosing, y$dosing),
        observation_plan = identical(x$observations$time, y$observations$time) &&
            identical(x$observations$observable, y$observations$observable),
        observations = identical(x$observations, y$observations),
        stop("Unknown experiment component category: ", category, ".", call. = FALSE)
    )
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
    out <- .experiments_elements(x)[i]
    if (any(vapply(out, is.null, logical(1)))) {
        stop("Cannot subset experiments with missing or unknown indices.", call. = FALSE)
    }
    .new_experiments(out, .experiments_compute_component_homogeneity(out))
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
    .experiments_elements(x)[[i, exact = exact]]
}

#' Replace experiments in a collection
#'
#' Replacement re-establishes collection validity and refreshes component
#' homogeneity metadata.
#' @param x A `compphysiol_experiments` collection.
#' @param i Replacement index.
#' @param ... Unused.
#' @param value For `[[<-`, a `compphysiol_experiment` or `NULL`; for `[<-`, a
#'   `compphysiol_experiments` collection.
#' @returns The modified `compphysiol_experiments` collection.
#' @export
`[[<-.compphysiol_experiments` <- function(x, i, ..., value) {
    if (!is.null(value)) .check_class(value, "compphysiol_experiment")
    out <- .experiments_elements(x)
    out[[i]] <- value
    .experiments_validate_result(out)
}

#' @rdname sub-subset-.compphysiol_experiments
#' @export
`[<-.compphysiol_experiments` <- function(x, i, ..., value) {
    .check_class(value, "compphysiol_experiments")
    out <- .experiments_elements(x)
    out[i] <- .experiments_elements(value)
    .experiments_validate_result(out)
}

.experiments_validate_result <- function(x) {
    .experiments_check_elements(x)
    .check_optional_unique_names(x, "experiment")
    .experiments_check_units(x)
    .new_experiments(x, .experiments_compute_component_homogeneity(x))
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
    combined <- do.call(c, unname(lapply(objects, .experiments_elements)))
    combined <- combined %||% list()
    .check_optional_unique_names(combined, "experiment")
    .experiments_check_units(combined)
    .new_experiments(combined, .experiments_combine_component_homogeneity(objects))
}

.experiments_combine_component_homogeneity <- function(objects) {
    nonempty <- objects[lengths(objects) > 0L]
    categories <- .experiments_component_names()
    out <- stats::setNames(rep(TRUE, length(categories)), categories)
    if (length(nonempty) < 1L) return(out)

    metadata <- lapply(nonempty, .experiments_component_homogeneity)
    reference <- nonempty[[1]][[1]]
    for (category in categories) {
        if (!all(vapply(metadata, `[[`, logical(1), category))) {
            out[[category]] <- FALSE
            next
        }
        if (length(nonempty) > 1L) {
            for (i in seq.int(2L, length(nonempty))) {
                if (!.experiments_components_identical(reference, nonempty[[i]][[1]], category)) {
                    out[[category]] <- FALSE
                    break
                }
            }
        }
    }
    out
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
