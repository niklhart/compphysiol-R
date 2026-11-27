#' Create a `compphysiol_compartments` object
#'
#' compphysiol_compartments represent model compartments with names and fixed or parametrized volumes.
#'
#' @param name Compartment name(s), character scalar or vector. Names beginning
#'   with `Depot_` or `ReleaseRate_` are reserved for internal infusion
#'   auxiliaries.
#' @param volume Volume(s) (numeric or character) for each compartment.
#'   The default `V{name}` uses parametrized volumes, with `{name}` replaced by the compartment name(s).
#'   Parametrized volumes have to be provided via the `parameters` argument of `compartment_model()`, e.g., `parameters(Vadi = 5 [L])`.
#'   Fixed numeric volumes can also be provided, e.g., `volume = 5 [L]` or `volume = 5, unit = "L"`.
#'   Can be a single value (applied to all compartments) or a vector of values corresponding to each compartment.
#' @param unit Unit for the compartment volume(s) (optional, default: `NULL`). If specified,
#'   it can be a single unit (applied to all volumes) or a vector of units corresponding to each volume.
#'   The `unit` argument is ignored (with a warning) if the `volume` argument is not numeric.
#' @param x For methods, a `compphysiol_compartments` object.
#' @param i For subsetting methods, indices or names of the compartments.
#' @param ... For `c()`, `compphysiol_compartments` objects to combine; unused by
#'   the other methods.
#' @return A `compphysiol_compartments` object
#' @examples
#' compartments(c("adi","bon"))
#' compartments('adi', volume = "Vadi")
#' compartments('adi', volume = 5 [L])
#' compartments('adi', volume = 5, unit = "L")
#' @seealso [add_compartment()] for adding compartments to a model and
#'   [modify_compartment_model] for the complete component map.
#' @seealso [inspect] for printing, structure inspection, and component access.
#' @export
compartments <- function(name = character(0), volume = "V{name}", unit = NULL) {
    # Process NSE argument for volume, which might include units via `value[unit]`
    volume <- .process_nse_arg(
        expr = substitute(volume),
        envir = parent.frame(n = 1)
    )

    if (anyDuplicated(name)) stop("Duplicate compartment names are not allowed.")
    reserved <- vapply(.infusion_auxiliary_compartment_prefixes(), function(prefix) {
        any(startsWith(name, prefix))
    }, logical(1))
    if (any(reserved)) {
        stop(
            "Compartment names beginning with ",
            paste0("'", .infusion_auxiliary_compartment_prefixes()[reserved], "'", collapse = " or "),
            " are reserved for internal infusion auxiliaries.",
            call. = FALSE
        )
    }
    

    if (is.logical(volume) && length(volume) > 0L && all(is.na(volume))) {
        volume <- as.numeric(volume)
    }

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

.infusion_auxiliary_compartment_prefixes <- function() {
    c("Depot_", "ReleaseRate_")
}

#' Add one or several compartments to a `compphysiol_compartment_model` object
#'
#' @inherit compartments description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams compartments
#' @param comp A `compphysiol_compartments` object. Constructed from the other inputs if not provided.
#' @param ... Unused. Its position requires the constructor arguments that
#'   follow it to be supplied by name rather than position.
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
#' @seealso [compartments()] for construction and [modify_compartment_model]
#'   for other model components.
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

#' @describeIn compartments Convert to a data frame.
#' @export
as.data.frame.compphysiol_compartments <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' @describeIn compartments Number of compartments.
#' @export
length.compphysiol_compartments <- function(x) {
    nrow(as.data.frame(x))
}

#' @describeIn compartments Names of the compartments.
#' @export
names.compphysiol_compartments <- function(x) x$name


#' @describeIn compartments Print a summary; returns `x` invisibly.
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

#' @describeIn compartments Combine several objects into one.
#' @export
c.compphysiol_compartments <- function(...) {
    x <- .combine_df_like(...)
    if (anyDuplicated(x$name)) stop("Duplicate compartment names are not allowed.")
    .new_compartments(x)
}

#' @describeIn compartments Subset by index or name.
#' @export
`[.compphysiol_compartments` <- function(x, i, ...) {
    .new_compartments(.subset_df_like(x, i, byname = TRUE, unique = TRUE))
}

#' @describeIn compartments Not supported; always errors. Use `[` with a scalar name or index instead.
#' @export
`[[.compphysiol_compartments` <- function(x, i, ...) .extract_df_like(x, i)
