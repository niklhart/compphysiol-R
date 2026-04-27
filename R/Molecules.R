#' `Molecules` class
#' Represents the molecules in a PBPK model.
#'
#' @param name Molecule name(s), character scalar or vector
#' @param cmt Compartment name(s), character scalar or vector
#' @param initial Initial concentration(s)/amount(s) of the molecule (numeric, possibly as `values[unit]`). The dimension of the 
#' `initial` argument needs to be compatible with the `molec` and `cmt` arguments (to be clarified).
#' @param unit Unit for the initial conditions (character scalar or `NULL`, the default), to be used if units are not specified via `initial`. 
#' @param type Type of initial condition, either `"concentration"` (the default) or `"amount"`.
#' @return A `Molecules` object containing the compartment and initial conditioninformation for each molecule.
#' @examples 
#' # No compartment specified: molecules are assumed to be present in all compartments
#' molecules(name = c("NFkB", "IkB"), unit = "mol/L")
#' # Compartment specified: each molecule is associated with a specific compartment
#' molecules(name = c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus"), initial = c(100, 50) [mol/L])
#' @export
molecules <- function(name = character(0), cmt = NULL, initial = 0, unit = NULL, type = c("concentration","amount")) {
    
    type <- match.arg(type)

    # Process NSE argument for initial, which might include units via `value[unit]` or as a separate `unit` argument
    initial <- .process_nse_arg(
        expr = substitute(initial),
        envir = parent.frame(n = 1)
    )


    # Determine output length
    nOut <- max(length(name), length(cmt), length(initial), length(unit))

    # Initial is a list-column --> manual recycing if needed
    initial <- as.list(initial)
    if (length(initial) == 1) initial <- rep(initial, nOut)
    
    # Unit provided as a separate argument --> combine into initial
    if (!is.null(unit)) {
        if (length(unit) == 1) unit <- rep(unit, nOut)
        if (length(unit) != nOut) stop("Arguments 'initial' and 'unit' have incompatible lengths.")

        initial <- Map(
            function(init, u) if (u != "") units::set_units(init, u, mode = "standard") else init,
            initial, unit
        )
    }

    # Early return for empty molecules
    if (identical(name, character(0))) {
        return(
            structure(
                data.frame(
                    name = character(),
                    cmt = character(),
                    init = numeric(),
                    type = character(),
                    stringsAsFactors = FALSE
                ),
                class = "Molecules"
            )
        )
    }

    # Construct the Molecules object as a data frame with class "Molecules"
    molec <- data.frame(
        name = name,
        cmt = cmt %||% NA_character_,
        init = I(initial),
        type = type,
        stringsAsFactors = FALSE
    )
    structure(molec, class = "Molecules")
}

#' Add one or several molecules to a `CompartmentModel` object.
#' 
#' @inherit molecules description details
#' @param model A `CompartmentModel` object.
#' @inheritParams molecules
#' @param molec A `Molecules` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @export
#' @seealso [molecules()]
add_molecule <- function(
    model, 
    name = character(0), 
    cmt = NULL, 
    initial = 0, 
    unit = NULL, 
    type = c("concentration","amount"), 
    molec = NULL
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    molec <- .forward_or_use(
        object_arg_name = "molec",
        constructor_name = "molecules",
        call = call,
        parent_env = parent.frame()
    )

    model$molecules <- c(model$molecules, molec)

    model
}

#' Combine multiple `Molecules` objects into one
#'
#' @param ... Multiple `Molecules` objects to combine
#' @return A combined `Molecules` object
#' @export
c.Molecules <- function(...) .combine_df_like(...)

#' Subset a `Molecules` object
#'
#' @param x A `Molecules` object
#' @param i Index or name(s) of molecules to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `Molecules` object
#' @export
`[.Molecules` <- function(x, i, ...) .subset_df_like(x, i, byname = TRUE)

#' Extraction method for `Molecules` class
#'
#' This method is intentionally not implemented to prevent direct element access,
#' which could lead to confusion given the internal data frame-like structure of `Molecules` objects.
#' Instead, users should use subsetting with `[` and a scalar name/index to access specific molecules.
#'
#' @param x A `Molecules` object
#' @param i Row index to access
#' @param ... Additional arguments (not used)
#' @return Nothing (errors)
#' @export
`[[.Molecules` <- function(x, i, ...) .extract_df_like(x, i)

#' Convert a `Molecules` object to a data frame
#'
#' @param x A `Molecules` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `Molecules` object
#' @export
as.data.frame.Molecules <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Print method for `Molecules` class
#' 
#' Pretty-prints a `Molecules` object.
#' 
#' @param x A `Molecules` object
#' @param ... Additional arguments (not used)
#' @return The `Molecules` object (invisible)
#' @export
print.Molecules <- function(x, ...) {

    cmt <- ifelse(is.na(x$cmt), "<all cmt>", x$cmt)
    init_str <- vapply(x$init, format, character(1))

    if (length(x) > 0) {
        cat(" Molecules:\n")
        cat(
            sprintf(
                "  (%i) %s in %s, initial %s = %s\n",
                seq_along(x),
                x$name,
                cmt,
                x$type,
                init_str
            ),
            sep = ""
        )
    } else {
        cat(" Molecules: (none)\n")
    }

    invisible(x)
}

#' Names of molecules in a `Molecules` object
#'
#' @param x A `Molecules` object
#' @param ... Additional arguments (not used)
#' @return A character vector of molecule names
#' @export
names.Molecules <- function(x, ...) {
    x$name
}

#' Length of a `Molecules` object
#'
#' @param x A `Molecules` object
#' @param ... Additional arguments (not used)
#' @return The number of molecules in the object
#' @export
length.Molecules <- function(x, ...) {
    nrow(as.data.frame(x))
}

#' Convert a `Molecules` object to a list of lists, where each inner list represents a molecule with its properties
#' 
#' @param x A `Molecules` object
#' @param ... Additional arguments (not used)
#' @return A list of lists, where each inner list represents a molecule with its properties
#' @export
as.list.Molecules <- function(x, ...) .listify_df_like(x)

# ---- Code snippet for retrieving initial conditions from when they were defined in Compartments objects, for potential later use ----
#  
# #' Initial conditions of a `Compartments` object
# #' 
# #' Returns the initial amounts of compartments in a `Compartments` object. 
# #' 
# #' The type of the returned object depends on the presence of units in the initial conditions: 
# #' 
# #' - if all compartment initial conditions are numeric without units, a numeric vector is returned
# #' - if all compartment initial conditions have consistent units, the returned vector will be of class `units`, 
# #' - if the compartment initial conditions have mixed units,  the returned vector will be of class `mixed_units`.
# #' 
# #' @param comp A `Compartments` object
# #' @param named If `TRUE`, returns a named list of initial amounts; if `FALSE` (default), it is unnamed
# #' @return A vector of initial amounts for each compartment (see Details)
# #' @export
# initials <- function(comp, named = FALSE) {
#     .check_class(comp, "Compartments")
#     y0 <- comp$initial

#     # Allow mixed units in initial conditions if required by temporarily setting allow_mixed = TRUE
#     oldopt <- units::units_options(allow_mixed = TRUE)
#     on.exit(units::units_options(oldopt), add = TRUE)

#     # flatten list of initial values into a vector
#     y0 <- do.call(what = c, args = y0) %||% numeric(0)

#     return(if (named) setNames(y0, nm = names(comp)) else y0)
# } 
