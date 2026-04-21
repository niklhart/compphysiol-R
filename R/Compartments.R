#' `Compartments` Class
#'
#' Compartments represent model compartments with names and fixed or parametrized volumes.
#'
#' @param name Compartment name(s), character scalar or vector
#' @param volume Volume(s) (numeric or character) for each compartment.
#'   The default `V{name}` uses parametrized volumes, with `{name}` replaced by the compartment name(s).
#'   Parametrized volumes have to be provided via the `parameters` argument of `compartment_model()`, e.g., `parameters(Vadi = 5 [L])`.
#'   Fixed numeric volumes can also be provided, e.g., `volume = 5 [L]` or `volume = 5, unit = "L"`.
#'   Can be a single value (applied to all compartments) or a vector of values corresponding to each compartment.
#' @param unit Unit for the compartment volume(s) (optional, default: `NULL`). If specified,
#'   it can be a single unit (applied to all volumes) or a vector of units corresponding to each volume.
#'   The `unit` argument is ignored (with a warning) if the `volume` argument is not numeric.
#' @return A `Compartments` object
#' @examples
#' compartments(c("adi","bon"))
#' compartments('adi', volume = "Vadi")
#' compartments('adi', volume = 5 [L])
#' compartments('adi', volume = 5, unit = "L")
#' @export
compartments <- function(name = character(0), volume = "V{name}", unit = NULL) {
    # Process NSE argument for volume, which might include units via `value[unit]`
    volume <- .process_nse_arg(
        expr = substitute(volume),
        envir = parent.frame(n = 1)
    )

    if (anyDuplicated(name)) stop("Duplicate compartment names are not allowed.")
    

    if (is.numeric(volume)) {

        volume <- as.list(volume)
        if (length(volume) == 1) volume <- rep(volume, length(name))
        if (length(name) != length(volume)) stop("Arguments 'name' and 'volume' must have the same length.")
        
        if (!is.null(unit)) {
            if (length(unit) == 1) unit <- rep(unit, length(name))
            if (length(unit) != length(name)) {
                stop("Argument 'unit' must be NULL, a scalar, or have the same length as 'name'.")
            }

            volume <- Map(
                function(v, u) {
                    if (u != "") {
                        units::set_units(v, u, mode = "standard")
                    } else {
                        v
                    }
                },
                volume,
                unit
            )
        }

    } else {

        # logic for character volumes (parametrized)
        if (!is.null(unit)) warning("Argument 'unit' is ignored when 'volume' is not numeric.")
        if (!is.character(volume)) stop("Argument 'volume' must be numeric or character.")

        # If volume is a character string, apply special substitution rule
        if (length(volume) == 1) {
            volume <- lapply(name, function(nm) .as_call(gsub(pattern = "{name}", replacement = nm, x = volume, fixed = TRUE)))
        }
        if (length(name) != length(volume)) {
            stop("Arguments 'name' and 'volume' must have the same length.")
        }

    }

    structure(
        data.frame(name = name, volume = I(volume)),
        class = "Compartments"
    )
}

#' Convert a `Compartments` object to a data frame
#'
#' @param x A `Compartments` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `Compartments` object
#' @export
as.data.frame.Compartments <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Length method for `Compartments` class
#' 
#' Returns the number of compartments in a `Compartments` object.
#' @param x A `Compartments` object
#' @return The number of compartments
#' @export
length.Compartments <- function(x) {
    nrow(as.data.frame(x))
}

#' Names method for `Compartments` class
#' 
#' Returns the names of compartments in a `Compartments` object.
#' @param x A `Compartments` object
#' @return A character vector of compartment names
#' @export
names.Compartments <- function(x) x$name


#' Print method for `Compartments` class
#' 
#' Pretty-prints a `Compartments` object.
#' 
#' @param x A `Compartments` object.
#' @param ... ignored
#' @return The `Compartments` object (invisibly).
#' @export
print.Compartments <- function(x, ...) {

    vol_str <- vapply(x$volume, format, character(1))

    if (length(x) > 0) {
        cat(" Compartments:\n")
        cat(
            sprintf("  (%s) %s (volume = %s)\n", seq_along(x), x$name, vol_str),
            sep = ""
        )
    } else {
        cat(" Compartments: (none)\n")
    }
    invisible(x)
}

#' Combine multiple `Compartments` objects into one
#'
#' @param ... Multiple `Compartments` objects to combine
#' @return A combined `Compartments` object
#' @export
c.Compartments <- function(...) .combine_df_like(...)

#' Subset a `Compartments` object
#'
#' @param x A `Compartments` object
#' @param i Index or name(s) of compartments to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `Compartments` object
#' @export
`[.Compartments` <- function(x, i, ...) .subset_df_like(x, i, byname = TRUE)

#' Extraction method for `Compartments` class
#'
#' This method is intentionally not implemented to prevent direct element access,
#' which could lead to confusion given the internal data frame-like structure of `Compartments` objects.
#' Instead, users should use subsetting with `[` and a scalar name/index to access specific compartments.
#'
#' @param x A `Compartments` object
#' @param i Row index to access
#' @param ... Additional arguments (not used)
#' @return Nothing (errors)
#' @export
`[[.Compartments` <- function(x, i, ...) .extract_df_like(x, i)

