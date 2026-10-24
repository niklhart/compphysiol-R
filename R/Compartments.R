#' Create a `compphysiol_compartments` object
#'
#' compphysiol_compartments represent model compartments with names and fixed or parametrized volumes.
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
#' @return A `compphysiol_compartments` object
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

        if (length(volume) == 1) {
            volume <- rep(volume, length(name))
        }
        if (length(name) != length(volume)) {
            stop("Arguments 'name' and 'volume' must have the same length.")
        }
        volume <- unname(Map(
            function(vol, nm) {
                .as_call(gsub(pattern = "{name}", replacement = nm, x = vol, fixed = TRUE))
            },
            volume,
            name
        ))

    }

    .new_compartments(data.frame(name = name, volume = I(volume)))
}

.new_compartments <- function(x) {
    structure(x, class = .compphysiol_class("compphysiol_compartments"))
}

#' Add one or several compartments to a `compphysiol_compartment_model` object.
#'
#' @inherit compartments description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams compartments
#' @param comp A `compphysiol_compartments` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @examples
#' # Several compartments added in a single call
#' compartment_model() |>
#'     add_compartment(name = c("A","B"))
#'
#' # Different ways of adding compartments
#' M1 <- compartment_model() |>
#'     add_compartment(name = "A", volume = 10 [L])
#' M2 <- compartment_model() |>
#'     add_compartment(name = "A", volume = 10, unit = "L")
#' identical(M1, M2)
#' @export
add_compartment <- function(
    model,
    comp,
    ...,
    name = character(0),
    volume = "V{name}",
    unit = NULL
) {
    UseMethod("add_compartment")
}

#' @rdname add_compartment
#' @export
add_compartment.compphysiol_compartment_model <- function(
    model,
    comp,
    ...,
    name = character(0),
    volume = "V{name}",
    unit = NULL
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    comp <- .forward_or_use(
        object_arg_name = "comp",
        constructor_name = "compartments",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(comp, "compphysiol_compartments")

    model$compartments <- c(model$compartments, comp)
    model
}

#' Convert a `compphysiol_compartments` object to a data frame
#'
#' @param x A `compphysiol_compartments` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `compphysiol_compartments` object
#' @export
as.data.frame.compphysiol_compartments <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Length of a `compphysiol_compartments` object
#' 
#' Returns the number of compartments in a `compphysiol_compartments` object.
#' @param x A `compphysiol_compartments` object
#' @return The number of compartments
#' @export
length.compphysiol_compartments <- function(x) {
    nrow(as.data.frame(x))
}

#' Names of compartments in a `compphysiol_compartments` object
#' 
#' Returns the names of compartments in a `compphysiol_compartments` object.
#' @param x A `compphysiol_compartments` object
#' @return A character vector of compartment names
#' @export
names.compphysiol_compartments <- function(x) x$name


#' Print a `compphysiol_compartments` object
#' 
#' Pretty-prints a `compphysiol_compartments` object.
#' 
#' @param x A `compphysiol_compartments` object.
#' @param ... ignored
#' @return The `compphysiol_compartments` object (invisibly).
#' @export
print.compphysiol_compartments <- function(x, ...) {

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

#' Combine multiple `compphysiol_compartments` objects into one
#'
#' @param ... Multiple `compphysiol_compartments` objects to combine
#' @return A combined `compphysiol_compartments` object
#' @export
c.compphysiol_compartments <- function(...) {
    x <- .combine_df_like(...)
    if (anyDuplicated(x$name)) stop("Duplicate compartment names are not allowed.")
    .new_compartments(x)
}

#' Subset a `compphysiol_compartments` object
#'
#' @param x A `compphysiol_compartments` object
#' @param i Index or name(s) of compartments to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `compphysiol_compartments` object
#' @export
`[.compphysiol_compartments` <- function(x, i, ...) {
    .new_compartments(.subset_df_like(x, i, byname = TRUE, unique = TRUE))
}

#' Extraction method for `compphysiol_compartments` class
#'
#' This method is intentionally not implemented to prevent direct element access,
#' which could lead to confusion given the internal data frame-like structure of `compphysiol_compartments` objects.
#' Instead, users should use subsetting with `[` and a scalar name/index to access specific compartments.
#'
#' @param x A `compphysiol_compartments` object
#' @param i Row index to access
#' @param ... Additional arguments (not used)
#' @return Nothing (errors)
#' @export
`[[.compphysiol_compartments` <- function(x, i, ...) .extract_df_like(x, i)
