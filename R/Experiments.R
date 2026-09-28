#' Collect experiments
#'
#' Creates a list-like collection of experiments. Optional names, order, and
#' duplicates are preserved. Each element retains its own parameters and units.
#' Use `[` to obtain a collection and `[[` to extract a single experiment.
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
    structure(x, class = "Experiments")
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
            "  (%s) %sstart = %s; %s parameters; %s dosing events; %s measurements\n",
            i, label, format(e$start), length(e$parameters), length(e$dosing),
            nrow(e$measurements)
        ))
    }
    invisible(x)
}
