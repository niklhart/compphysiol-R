#' Create a `compphysiol_molecules` object
#'
#' compphysiol_molecules represent the modelled substances whose amounts or concentrations
#' can vary across compartments. A molecule may be a drug, metabolite, protein,
#' receptor, complex, biomarker, or other substance in the model.
#'
#' A molecule can be defined for a specific compartment through `cmt`, or for
#' all compartments by leaving `cmt` unspecified. The latter creates a compact
#' wildcard definition that is resolved by [wire()] before ODE generation.
#'
#' @param name Molecule name(s), character scalar or vector
#' @param cmt Compartment name(s), character scalar or vector. If omitted,
#'   the molecule is assumed to be present in all compartments.
#' @param initial Initial concentration(s) or amount(s) of the molecule
#'   (numeric, possibly as `values[unit]`, or character for parametrized
#'   initials). Character values are converted to expressions; `{molec}` and
#'   `{cmt}` placeholders are replaced by the generated molecule and
#'   compartment names. Scalar values are recycled across the generated molecule
#'   definitions; otherwise the length must be compatible with `name`, `cmt`,
#'   and `unit`.
#' @param unit Unit for the initial conditions (character scalar or `NULL`, the default), to be used if units are not specified via `initial`. 
#' @param type Type of initial condition, either `"concentration"` (the default) or `"amount"`.
#'   Compartments with time-dependent volumes are represented using amount
#'   states. A concentration initial is converted using the volume at the first
#'   simulation time.
#' @param x For methods, a `compphysiol_molecules` object.
#' @param i For subsetting methods, indices or names of the molecules.
#' @param ... For `c()`, `compphysiol_molecules` objects to combine; unused by the
#'   other methods.
#' @return A `compphysiol_molecules` object containing the compartment and initial condition
#'   information for each molecule.
#' @examples 
#' # No compartment specified: molecules are assumed to be present in all compartments
#' molecules(name = c("NFkB", "IkB"), unit = "mol/L")
#' # Compartment specified: each molecule is associated with a specific compartment
#' molecules(name = c("NFkB", "IkB"), cmt = c("cytoplasm", "nucleus"), initial = c(100, 50) [mol/L])
#' @seealso [add_molecule()] for adding molecules to a model and
#'   [modify_compartment_model] for the complete component map.
#' @seealso [inspect] for printing, structure inspection, and component access.
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
    if (is.numeric(initial) || inherits(initial, "units") || is.list(initial)) {
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
    } else {
        if (!is.null(unit)) warning("Argument 'unit' is ignored when 'initial' is not numeric.")
        if (!is.character(initial)) stop("Argument 'initial' must be numeric or character.")

        if (length(initial) == 1) initial <- rep(initial, nOut)
        if (length(initial) != nOut) stop("Arguments 'initial' and 'unit' have incompatible lengths.")

        name_for_initial <- if (length(name) == 1) rep(name, nOut) else name
        cmt_for_initial <- cmt %||% rep(NA_character_, nOut)
        if (length(cmt_for_initial) == 1) cmt_for_initial <- rep(cmt_for_initial, nOut)

        initial <- unname(Map(
            function(init, molec, cmt) {
                init <- gsub(pattern = "{molec}", replacement = molec, x = init, fixed = TRUE)
                init <- gsub(pattern = "{cmt}", replacement = cmt, x = init, fixed = TRUE)
                .as_call(init)
            },
            initial,
            name_for_initial,
            cmt_for_initial
        ))
    }

    # Early return for empty molecules
    if (identical(name, character(0))) {
        return(.new_molecules(
                data.frame(
                    name = character(),
                    cmt = character(),
                    init = numeric(),
                    type = character(),
                    stringsAsFactors = FALSE
                )
        ))
    }

    # Construct the molecules object as a data frame with its package class.
    x <- data.frame(
        name = name,
        cmt = cmt %||% NA_character_,
        init = I(initial),
        type = type,
        stringsAsFactors = FALSE
    )
    .check_unique_molecule_states(x)
    .new_molecules(x)
}

.new_molecules <- function(x) {
    structure(x, class = .compphysiol_class("compphysiol_molecules"))
}

.check_unique_molecule_states <- function(x) {
    key <- paste(x$name, ifelse(is.na(x$cmt), "<all cmt>", x$cmt), sep = "\r")
    if (anyDuplicated(key)) {
        stop("Molecule-compartment states must be unique.", call. = FALSE)
    }
    invisible(NULL)
}

#' Add one or several molecules to a `compphysiol_compartment_model` object.
#' 
#' @inherit molecules description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams molecules
#' @param molec A `compphysiol_molecules` object. Constructed from the other inputs if not provided.
#' @param ... Unused. Its position requires the constructor arguments that
#'   follow it to be supplied by name rather than position.
#' @returns The modified `compphysiol_compartment_model` object.
#' @examples
#' # A drug with an initial amount in one compartment
#' compartment_model() |>
#'     add_compartment(name = c("Central", "Peripheral"), volume = 1 [L]) |>
#'     add_molecule(name = "drug", cmt = "Central", initial = 100 [mg], type = "amount")
#'
#' # Molecules present in all compartments; the wildcard is resolved by wire()
#' compartment_model() |>
#'     add_compartment(name = c("cyt", "nuc"), volume = 1 [L]) |>
#'     add_molecule(name = c("A", "B"), initial = 1 [nmol/L])
#' @seealso [molecules()] for construction and [modify_compartment_model] for
#'   other model components.
#' @export
add_molecule <- function(
    model,
    molec,
    ...,
    name = character(0),
    cmt = NULL,
    initial = 0,
    unit = NULL,
    type = c("concentration", "amount")
) {
    UseMethod("add_molecule")
}

#' @rdname add_molecule
#' @export
add_molecule.compphysiol_compartment_model <- function(
    model,
    molec,
    ...,
    name = character(0),
    cmt = NULL,
    initial = 0,
    unit = NULL,
    type = c("concentration", "amount")
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    molec <- .forward_or_use(
        object_arg_name = "molec",
        constructor_name = "molecules",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(molec, "compphysiol_molecules")

    model$molecules <- c(model$molecules, molec)

    model
}

#' @describeIn molecules Combine several objects into one.
#' @export
c.compphysiol_molecules <- function(...) {
    out <- .combine_df_like(...)
    .check_unique_molecule_states(out)
    .new_molecules(out)
}

#' @describeIn molecules Subset by index or name.
#' @export
`[.compphysiol_molecules` <- function(x, i, ...) {
    .new_molecules(.subset_df_like(x, i, byname = TRUE, unique = TRUE))
}

#' @describeIn molecules Not supported; always errors. Use `[` with a scalar name or index instead.
#' @export
`[[.compphysiol_molecules` <- function(x, i, ...) .extract_df_like(x, i)

#' @describeIn molecules Convert to a data frame.
#' @export
as.data.frame.compphysiol_molecules <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' @describeIn molecules Print a summary; returns `x` invisibly.
#' @export
print.compphysiol_molecules <- function(x, ...) {

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

#' @describeIn molecules Names of the molecules.
#' @export
names.compphysiol_molecules <- function(x, ...) {
    x$name
}

#' @describeIn molecules Number of molecules.
#' @export
length.compphysiol_molecules <- function(x, ...) {
    nrow(as.data.frame(x))
}

#' @describeIn molecules Convert to a list of lists, one per molecule, holding its properties.
#' @export
as.list.compphysiol_molecules <- function(x, ...) .listify_df_like(x)

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
#     .check_class(comp, "compphysiol_compartments")
#     y0 <- comp$initial

#     # Allow mixed units in initial conditions if required by temporarily setting allow_mixed = TRUE
#     oldopt <- units::units_options(allow_mixed = TRUE)
#     on.exit(units::units_options(oldopt), add = TRUE)

#     # flatten list of initial values into a vector
#     y0 <- do.call(what = c, args = y0) %||% numeric(0)

#     return(if (named) setNames(y0, nm = names(comp)) else y0)
# } 
