# Parameters class. Parameters are obtained from `Physiology` objects or empirically specified, and can be passed to `CompartmentModel` objects.

#' `Parameters` class.
#'
#' Parameters can be specified in two ways:
#' 1. As name-value pairs using non-standard evaluation (where each value can optionally have an associated unit).
#' 2. Via the `name`, `value`, and `unit` arguments (where `name` is a character vector of parameter names, 
#' `value` is a numeric vector of parameter values, and `unit` is an optional character vector of units).
#'
#' @param ... Parameter values, either as named arguments or as a list of named parameters.
#' @param name Optional parameter names (if not using named arguments).
#' @param value Optional parameter values (if not using named arguments).
#' @param unit Optional parameter units (if not using named arguments).
#' @returns A `Parameters` object containing the specified parameters.
#' @export
parameters <- function(..., name = NULL, value = NULL, unit = NULL) {
    # Non-standard evaluation to capture parameter names and values
    args <- list(...)
    if (length(args) > 0) {
        if (!is.null(name) || !is.null(value) || !is.null(unit)) {
            stop("Cannot use both '...' and 'name'/'value/unit' arguments.")
        }
        name <- names(args)
        value <- unlist(args, use.names = FALSE)
        unit <- vapply(args, function(x) if (inherits(x, c("units", "mixed_units"))) units::deparse_unit(x) else "", character(1))
    }

    if (length(name) != length(value)) stop("All parameters must be named.")
    names(value) <- name
    if (!is.null(unit)) value <- units::mixed_units(value, unit)
    structure(
        value,
        class = c("Parameters", "mixed_units", "list")
    )
}

#' Names method for `Parameters` objects
#' @param x A `Parameters` object
#' @returns The names of the parameters
#' @export
names.Parameters <- function(x) names(unclass(x))

#' Subsetting method for `Parameters` objects
#' @param x A `Parameters` object
#' @param i Indices or names of the parameters to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `Parameters` object
#' @export
`[.Parameters` <- function(x, i, ...) {
    structure(
        unclass(x)[i],
        class = c("Parameters", "mixed_units", "list")
    )
}

#' Combine multiple `Parameters` objects
#' @param ... `Parameters` objects to combine
#' @returns A combined `Parameters` object
#' @export
c.Parameters <- function(...) {
    params_list <- list(...)
    if (!all(sapply(params_list, inherits, "Parameters"))) {
        stop("All arguments must be of class 'Parameters'")
    }
    params_list |>
        lapply(FUN = unclass) |>
        do.call(what = "c") |>
        structure(class = c("Parameters", "mixed_units", "list"))
}

#' Print method for `Parameters` objects
#' @param x An `Parameters` object
#' @param ... Additional arguments (not used)
#' @returns The `Parameters` object (invisible)
#' @export
print.Parameters <- function(x, ...) {
    if (length(x) > 0) {
        cat(" Parameters:\n")
        cat(
            sprintf(
                "  (%s) %s = %s\n",
                seq_along(x),
                names(x),
                format(x)
            ),
            sep = ""
        )
    } else {
        cat(" Parameters: (none)\n")
    }
    invisible(x)
}
