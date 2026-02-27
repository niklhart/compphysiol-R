#' `Compartments` Class
#' 
#' Represents one or several model compartments with names and initial amounts.
#' @param name Compartment name(s), character scalar or vector
#' @param initial Initial amounts for the compartments (numeric scalar or vector, default = 0)
#' @return A `Compartments` object
#' @examples
#' compartments(c("adi","bon"), 10)
#' @export
compartments <- function(name, initial = 0) {

    if (length(initial) == 1) {
        initial <- rep(initial, length(name))
    }
    if (length(name) != length(initial)) {
        stop("Arguments 'name' and 'initial' must have the same length.")
    }

    structure(
        list(name = name, initial = initial),
        class = "Compartments"
    )
}

#' Length method for `Compartments` class
#' Returns the number of compartments in a `Compartments` object.
#' @param x A `Compartments` object
#' @return The number of compartments (length of the `name` vector)
#' @export
length.Compartments <- function(x) {
    length(x$name)
}

#' Names method for `Compartments` class
#' Returns the names of compartments in a `Compartments` object.
#' @param x A `Compartments` object
#' @return A character vector of compartment names
#' @export
names.Compartments <- function(x) {
    x$name
}

#' Print method for `Compartments` class
#' Pretty-prints a `Compartments` object.
#' @param x A `Compartments` object.
#' @param ... ignored
#' @return The `Compartments` object (invisibly).
#' @export
print.Compartments <- function(x, ...) {

    if (length(x) > 0) {
        cat(" Compartments:\n")
        cat(
            sprintf("  (%s) %s (initial = %s)\n", seq_along(x), x$name, format(x$initial)),
            sep = ""
        )
    } else {
        cat(" Compartments: (none)\n")
    }
    invisible(x)
}

#' Combine multiple `Compartments` objects into one
#' Combines multiple `Compartments` objects by concatenating their names and initial values.
#' @param ... One or more `Compartments` objects to combine.
#' @param recursive ignored
#' @return A single `Compartments` object containing all compartments from the inputs.
#' @export
c.Compartments <- function(..., recursive = FALSE) {
    objs <- list(...)

    # ensure all inputs are Compartments
    if (!all(vapply(objs, inherits, logical(1), "Compartments"))) {
        stop("All inputs must be Compartments objects.")
    }

    name <- unlist(lapply(objs, `[[`, "name"))
    initial <- unlist(lapply(objs, `[[`, "initial"))

    if (anyDuplicated(name)) {
        stop("Duplicate compartment names are not allowed.")
    }

    compartments(name = name, initial = initial)
}

#' Subset method for `Compartments` class
#' 
#' Allows subsetting a `Compartments` object by index or name, 
#' returning a new `Compartments` object with the selected compartments.
#' @param x A `Compartments` object
#' @param i Index or name(s) of compartments to select
#' @param ... ignored
#' @return A new `Compartments` object containing only the selected compartments
#' @export
`[.Compartments` <- function(x, i, ...) {
    # Allow character indexing via names
    if (is.character(i)) {
        i <- match(i, x$name)
        if (any(is.na(i))) {
            stop("Unknown compartment name.")
        }
    }

    compartments(
        name = x$name[i],
        initial = x$initial[i]
    )
}

#' Create an empty `Compartments` object
#' @return An empty `Compartments` object
#' @export
empty_compartment <- function() compartments(name = character(0), initial = numeric(0))
