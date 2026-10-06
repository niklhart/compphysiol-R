#' Collect realized parameter sets
#'
#' Creates a list-like collection in which each element is a complete,
#' realized [Parameters][parameters()] object. Elements may be named or unnamed.
#' `ParameterSets` records values only: it does not describe a population model
#' or how the parameter sets were generated.
#'
#' Parameters shared across elements must both be unit-free or have convertible
#' units. Compatible values are retained as supplied and are not converted.
#' Numeric and categorical covariates are ordinary parameters.
#'
#' @param ... [Parameters][parameters()] objects, optionally named. For a list
#'   of parameter sets, use `do.call(parameter_sets, x)`.
#' @returns A `ParameterSets` object.
#' @examples
#' population <- parameter_sets(
#'     individual_1 = parameters(BW = 65 [kg], sex = "female"),
#'     individual_2 = parameters(BW = 82 [kg], sex = "male")
#' )
#' population
#' population["individual_1"]
#' population[["individual_2"]]
#' @export
parameter_sets <- function(...) {
    .new_parameter_sets(list(...))
}

.new_parameter_sets <- function(x) {
    if (!all(vapply(x, inherits, logical(1), "Parameters"))) {
        stop("All elements must be Parameters objects.", call. = FALSE)
    }
    labels <- names(x)
    named <- !is.null(labels) & !is.na(labels) & nzchar(labels)
    if (anyDuplicated(labels[named])) {
        stop("Parameter set names must be unique.", call. = FALSE)
    }
    .parameter_sets_check_units(x)
    structure(x, class = c("ParameterSets", "list"))
}

.parameter_sets_check_units <- function(x) {
    shared <- list()
    for (i in seq_along(x)) {
        for (nm in names(x[[i]])) {
            value <- x[[i]][[nm]]
            if (nm %in% names(shared)) {
                .check_compatible_units(
                    shared[[nm]], value,
                    paste0("parameter '", nm, "' in parameter set ", i)
                )
            } else {
                shared[nm] <- list(value)
            }
        }
    }
    invisible(NULL)
}

#' Subset realized parameter sets
#'
#' @param x A `ParameterSets` object.
#' @param i Numeric, logical, or character indices.
#' @param ... Unused.
#' @returns A `ParameterSets` object.
#' @export
`[.ParameterSets` <- function(x, i, ...) {
    .new_parameter_sets(unclass(x)[i])
}

#' Extract one realized parameter set
#'
#' @param x A `ParameterSets` object.
#' @param i Index or name of one parameter set.
#' @param ... Unused.
#' @param exact Whether character indices must match exactly, as for lists.
#' @returns A `Parameters` object; an unknown name returns `NULL`, as for lists.
#' @export
`[[.ParameterSets` <- function(x, i, ..., exact = TRUE) {
    if (length(i) != 1L) stop("Select a single parameter set with [[.", call. = FALSE)
    unclass(x)[[i, exact = exact]]
}

#' Combine realized parameter-set collections
#'
#' @param ... `ParameterSets` objects to combine.
#' @returns A `ParameterSets` object, preserving order and names.
#' @export
c.ParameterSets <- function(...) {
    objects <- list(...)
    if (!all(vapply(objects, inherits, logical(1), "ParameterSets"))) {
        stop("All arguments must be ParameterSets objects.", call. = FALSE)
    }
    combined <- do.call(c, unname(lapply(objects, unclass)))
    .new_parameter_sets(combined %||% list())
}

#' Print realized parameter sets
#'
#' @param x A `ParameterSets` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.ParameterSets <- function(x, ...) {
    if (!length(x)) {
        cat(" Parameter sets: (none)\n")
        return(invisible(x))
    }
    cat(" Parameter sets:\n")
    labels <- names(x)
    for (i in seq_along(x)) {
        label <- if (!is.null(labels) && !is.na(labels[i]) && nzchar(labels[i])) {
            paste0(labels[i], ": ")
        } else {
            ""
        }
        values <- if (length(x[[i]])) {
            vapply(seq_along(x[[i]]), function(j) {
                value <- paste(format(x[[i]][[j]]), collapse = ", ")
                paste0(names(x[[i]])[[j]], " = ", value)
            }, character(1)) |>
                paste(collapse = "; ")
        } else {
            "(none)"
        }
        cat(sprintf("  (%s) %s%s\n", i, label, values))
    }
    invisible(x)
}
