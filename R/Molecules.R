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
#' # No compartment specified: all molecules are assumed to be in the same compartment
#' molecules(name = c("NFkB", "IkB"), initial = c(100, 50))
#' # Compartment specified: each molecule is associated with a specific compartment
#' molecules(name = c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus"), initial = c(100, 50))
#' @export
molecules <- function(name = character(0), cmt = NULL, initial = 0, unit = NULL, type = c("concentration","amount")) {
    
    type <- match.arg(type)

    # Process NSE argument for initial, which might include units via `value[unit]` or as a separate `unit` argument
    initial <- .process_nse_arg(
        expr = substitute(initial),
        envir = parent.frame(n = 1)
    )

    # Initial is a list-column --> manual recycing if needed
    initial <- as.list(initial)
    if (length(initial) == 1) initial <- rep(initial, length(name))
    if (length(name) != length(initial)) {
        stop("Arguments 'name' and 'initial' must have compatible lengths.")
    }
    # Unit provided as a separate argument --> combine into initial
    if (!is.null(unit)) {
        if (length(unit) == 1) unit <- rep(unit, length(name))
        if (length(unit) != length(name)) {
            stop("Argument 'unit' must be NULL, a scalar, or have the same length as 'name'.")
        }

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
        cmt = cmt %||% rep(NA_character_, length(name)),
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

#' Print a `Molecules` object
#'
#' @param x A `Molecules` object
#' @param ... Additional arguments (not used)
#' @return The `Molecules` object (invisible)
#' @export
print.Molecules <- function(x, ...) {

    cmt <- vapply(
        x$cmt, 
        function(c) if (is.na(c)) "<all cmt>" else paste0("[", c, "]", collapse = ","), 
        character(1)
    )
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