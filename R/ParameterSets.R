#' Collect realized parameter sets
#'
#' Creates a list-like collection in which each element is a complete,
#' realized [compphysiol_parameters][parameters()] object. Elements may be named or unnamed.
#' `compphysiol_parameter_sets` records values only: it does not describe a population model
#' or how the parameter sets were generated.
#'
#' compphysiol_parameters shared across elements must both be unit-free or have convertible
#' units. Compatible values are retained as supplied and are not converted.
#' Numeric and categorical covariates are ordinary parameters.
#'
#' @param ... [compphysiol_parameters][parameters()] objects, optionally named. For a list
#'   of parameter sets, use `do.call(parameter_sets, x)`.
#' @returns A `compphysiol_parameter_sets` object.
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
    x <- list(...)
    .parameter_sets_check_elements(x)
    .parameter_sets_check_names(x)
    .parameter_sets_check_units(x)
    .new_parameter_sets(x)
}

.new_parameter_sets <- function(x) {
    structure(x, class = .compphysiol_class(
        "compphysiol_parameter_sets", base = "list"
    ))
}

.parameter_sets_check_elements <- function(x) {
    if (!all(vapply(x, inherits, logical(1), "compphysiol_parameters"))) {
        stop("All elements must be parameters objects.", call. = FALSE)
    }
    invisible(NULL)
}

.parameter_sets_check_names <- function(x) {
    .check_optional_unique_names(x, "parameter set")
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
#' @param x A `compphysiol_parameter_sets` object.
#' @param i Numeric, logical, or character indices.
#' @param ... Unused.
#' @returns A `compphysiol_parameter_sets` object.
#' @export
`[.compphysiol_parameter_sets` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .check_unique_subset_index(i, "Parameter set")
    out <- unclass(x)[i]
    if (any(vapply(out, is.null, logical(1)))) {
        stop("Cannot subset parameters with missing or unknown indices.", call. = FALSE)
    }
    .new_parameter_sets(out)
}

#' Extract one realized parameter set
#'
#' @param x A `compphysiol_parameter_sets` object.
#' @param i Index or name of one parameter set.
#' @param ... Unused.
#' @param exact Whether character indices must match exactly, as for lists.
#' @returns A `compphysiol_parameters` object; an unknown name returns `NULL`, as for lists.
#' @export
`[[.compphysiol_parameter_sets` <- function(x, i, ..., exact = TRUE) {
    if (length(i) != 1L) stop("Select a single parameter set with [[.", call. = FALSE)
    unclass(x)[[i, exact = exact]]
}

#' Combine realized parameter-set collections
#'
#' @param ... `compphysiol_parameter_sets` objects to combine.
#' @returns A `compphysiol_parameter_sets` object, preserving order and names.
#' @export
c.compphysiol_parameter_sets <- function(...) {
    objects <- list(...)
    if (!all(vapply(objects, inherits, logical(1), "compphysiol_parameter_sets"))) {
        stop("All arguments must be parameter sets objects.", call. = FALSE)
    }
    combined <- do.call(c, unname(lapply(objects, unclass)))
    combined <- combined %||% list()
    .parameter_sets_check_names(combined)
    .parameter_sets_check_units(combined)
    .new_parameter_sets(combined)
}

#' Print realized parameter sets
#'
#' @param x A `compphysiol_parameter_sets` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.compphysiol_parameter_sets <- function(x, ...) {
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
