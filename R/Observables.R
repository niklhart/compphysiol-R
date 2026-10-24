
#' Create a `compphysiol_observables` object
#' 
#' compphysiol_observables are quantities of interest that are computed from parameters
#' and/or concentrations or amounts of molecule-in-compartment pairs. They are
#' not integrated as independent variables in the ODE system.
#'
#' In the DSL, `a[molecule, compartment]` and `c[molecule, compartment]`
#' refer to the amount or concentration of a molecule in a compartment. This
#' semantic reference is independent of whether the corresponding ODE state is
#' represented internally as an amount or as a concentration.
#' 
#' compphysiol_observables and equations are very similar, but there are two key differences:
#' 
#' 1. compphysiol_observables are evaluated from simulation output, while equations are evaluated inside model expressions.
#' 2. compphysiol_equations can be used in the definitions of transports, while observables cannot.
#' 
#' The `cmt` and `molec` arguments allow for convenient generation of common observables like concentrations and amounts.
#' They substitute `{cmt}` and `{molec}` in the names and expressions of the observables with the provided values.
#' If both `cmt` and `molec` are provided, the substitution is performed for both, and all possible combinations of `cmt` 
#' and `molec` are generated for the names and expressions of the observables.
#' 
#' @param ... Name-expression pairs defining the observables. The expressions can be provided as character strings or as R calls. 
#' @param name Name of the observable(s), character scalar or vector
#' @param expr Expression(s) for the observable(s), as character scalar/vector or as R call / lists of R calls.
#'   Expressions may contain parameters that are added to the model's parameters list, as well as amount or concentration references to molecule-in-compartment pairs.
#' @param cmt Optional character vector specifying the compartment(s) associated with the observable(s). 
#'  Used for automatic generation of common observables like concentrations and amounts.
#' @param molec Optional character vector specifying the molecule(s) associated with the observable(s). 
#'  Used for automatic generation of common observables like concentrations and amounts.
#' @returns A `compphysiol_observables` object
#' @examples
#' # Interactive path with name-expression pairs
#' observables(
#'     Cblo = c[cen],
#'     Cpla = c[cen]/BP
#' )
#' # Programmatic path with name and expression vectors
#' observables(
#'     name = c("Cblo", "Cpla"), 
#'     expr = c("c[cen]", "c[cen]/BP")
#' )
#' # Replacement of {cmt} in names and expressions
#' observables(
#'     "C{cmt}" = "c[{cmt}]", 
#'     cmt = c("adi","bon")
#' ) 
#' @seealso [add_observable()] for adding observables to a model and
#'   [modify_compartment_model] for the complete component map.
#' @export
observables <- function(..., name = character(0), expr = character(0), cmt = NULL, molec = NULL) {

    dots <- as.list(substitute(list(...)))[-1]
    if (length(dots) > 0) {
        if (length(name) > 0 || length(expr) > 0) {
            stop("Cannot use both '...' and 'name'/'expr' arguments.")
        }
        name <- names(dots)
        expr <- unname(dots)
    }

    if (!is.vector(expr)) expr <- list(expr)
    if (length(name) != length(expr)) stop("Arguments 'name' and 'expr' must have the same length.")

    # Abstract substitution logic 
    validate_pattern <- function(nm, ex, pattern) {
        ext_pattern <- paste0("\\{", pattern, "\\}")
        idx_name <- grep(ext_pattern, nm)
        idx_expr <- grep(ext_pattern, ex)
        if (!identical(idx_name, idx_expr)) {
            stop(
                sprintf("The special expression '{%s}' must be used in pairs in both names and expressions for replacement.", pattern),
                call. = FALSE
            )
        }
        invisible(NULL)
    }
    replace_pattern <- function(where, pattern, values) {
        ext_pattern <- paste0("\\{", pattern, "\\}")
        idx <- grep(ext_pattern, where)
        where <- as.list(where)
        where[idx] <- lapply(where[idx], function(x) {
            lapply(values, function(c_) gsub(ext_pattern, c_, x))
        })
        unlist(where)
    }

    # Substitution of {cmt} and {molec} in names and expressions if provided
    if (!is.null(cmt)) {
        validate_pattern(name, expr, "cmt")
        name <- replace_pattern(name, "cmt", cmt)
        expr <- replace_pattern(expr, "cmt", cmt)
    }
    if (!is.null(molec)) {
        validate_pattern(name, expr, "molec")
        name <- replace_pattern(name, "molec", molec)
        expr <- replace_pattern(expr, "molec", molec)
    }

    # internal structure of class compphysiol_observables: a named list of expressions
    names(expr) <- name
    expr <- lapply(expr, .as_call)

    .check_named_values(expr, "observable")
    .new_observables(expr)
}

.new_observables <- function(x = list()) {
    structure(x, class = .compphysiol_class("compphysiol_observables"))
}

#' Add one or several observables to a `compphysiol_compartment_model` object.
#'
#' @inherit observables description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams observables
#' @param obs A `compphysiol_observables` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_compartment(name = "blo") |>
#'     add_observable(Cblo = Ablo / Vblo)
#' ## Programmatic path with name and expression vectors
#' compartment_model() |>
#'     add_compartment(name = "blo") |>
#'     add_observable(name = "Cblo", expr = "Ablo/Vblo")
#' ## Programmatic path with compphysiol_observables object
#' obs <- observables(name = "Cblo", expr = "Ablo/Vblo")
#' compartment_model() |>
#'     add_compartment(name = "blo") |>
#'     add_observable(obs)
#' @seealso [observables()] for construction and [modify_compartment_model] for
#'   other model components.
#' @export
add_observable <- function(
    model,
    obs,
    ...,
    name = character(0),
    expr = character(0),
    cmt = NULL,
    molec = NULL
) {
    UseMethod("add_observable")
}

#' @rdname add_observable
#' @export
add_observable.compphysiol_compartment_model <- function(
    model,
    obs,
    ...,
    name = character(0),
    expr = character(0),
    cmt = NULL,
    molec = NULL
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    obs <- .forward_or_use(
        object_arg_name = "obs",
        constructor_name = "observables",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(obs, "compphysiol_observables")

    model$observables <- c(model$observables, obs)
    return(model)
}


#' Names of observables in a `compphysiol_observables` object
#' @param x A `compphysiol_observables` object
#' @returns The names of the observables
#' @export
names.compphysiol_observables <- function(x) names(unclass(x))

#' Print a `compphysiol_observables` object
#' @param x A `compphysiol_observables` object
#' @param ... Additional arguments (not used)
#' @returns The `compphysiol_observables` object (invisible)
#' @export
print.compphysiol_observables <- function(x, ...) {

    if (length(x) > 0) {
        cat(" Observables:\n")
        cat(sprintf(
                "  (%s) %s = %s\n",
                seq_along(x),
                names(x),
                vapply(x, deparse, character(1))
            ), 
            sep = ""
        )
    } else {
        cat(" Observables: (none)\n")
    }
    invisible(x)
}

#' Subset a `compphysiol_observables` object
#' @param x A `compphysiol_observables` object
#' @param i Indices or names of the observables to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `compphysiol_observables` object
#' @export
`[.compphysiol_observables` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .check_unique_subset_index(i, "Observable")
    .new_observables(unclass(x)[i])
}

#' Combine multiple `compphysiol_observables` objects into one
#' @param ... `compphysiol_observables` objects to combine
#' @returns A combined `compphysiol_observables` object
#' @export
c.compphysiol_observables <- function(...) {
    obs_list <- list(...)
    if (!all(sapply(obs_list, inherits, "compphysiol_observables"))) {
        stop("All arguments must be of class 'observables'")
    }
    x <- obs_list |>
         lapply(FUN = unclass) |> 
         do.call(what = "c")
    .check_unique_names(names(x), "observable")
    .new_observables(x)
}
