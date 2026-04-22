
#' Create an `Observables` object
#' 
#' Observables are quantities of interest that are calculated from the state variables 
#' (compartments) and parameters, but are not part of the ODE system. 
#' 
#' Observables and equations are very similar, but there are two key differences:
#' 
#' 1. For each observable (but not for equations), `to_ode()` returns a function to calculate it based on the ODE output.
#' 2. Equations can be used in the definitions of flows, while observables cannot.
#' 
#' The `cmt` and `molec` arguments allow for convenient generation of common observables like concentrations and amounts.
#' They substitute `{cmt}` and `{molec}` in the names and expressions of the observables with the provided values.
#' If both `cmt` and `molec` are provided, the substitution is performed for both, and all possible combinations of `cmt` 
#' and `molec` are generated for the names and expressions of the observables.
#' 
#' @param ... Name-expression pairs defining the observables. The expressions can be provided as character strings or as R calls. 
#' @param name Name of the observable(s), character scalar or vector
#' @param expr Expression(s) for the observable(s), as character scalar/vector or as R call / lists of R calls.
#'   Expressions may contain parameters that are added to the model's parameters list, as well as compartment states (not compartment names!)
#' @param cmt Optional character vector specifying the compartment(s) associated with the observable(s). 
#'  Used for automatic generation of common observables like concentrations and amounts.
#' @param molec Optional character vector specifying the molecule(s) associated with the observable(s). 
#'  Used for automatic generation of common observables like concentrations and amounts.
#' @returns An `Observables` object
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

    # internal structure of class Observables: a named list of expressions
    names(expr) <- name
    expr <- lapply(expr, .as_call)

    structure(
        expr,
        class = "Observables"
    )
}

#' Add an observable to a `CompartmentModel` object.
#'
#' @inherit observables description details
#' @param model A `CompartmentModel` object.
#' @inheritParams observables
#' @param obs An `Observables` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(Cblo = Ablo / Vblo)
#' ## Programmatic path with name and expression vectors
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(name = "Cblo", expr = "Ablo/Vblo")
#' ## Programmatic path with Observables object
#' obs <- observables(name = "Cblo", expr = "Ablo/Vblo")
#' compartment_model() |>
#'     add_compartment("blo") |>
#'     add_observable(obs = obs)
#' @export
add_observable <- function(
    model,
    ...,
    name = character(0),
    expr = character(0),
    cmt = NULL,
    molec = NULL,
    obs
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    obs <- .forward_or_use(
        object_arg_name = "obs",
        constructor_name = "observables",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(obs, "Observables")

    model$observables <- c(model$observables, obs)
    return(model)
}


#' Names method for `Observables` objects
#' @param x An `Observables` object
#' @returns The names of the observables
#' @export
names.Observables <- function(x) names(unclass(x))

#' Print method for `Observables` objects
#' @param x An `Observables` object
#' @param ... Additional arguments (not used)
#' @returns The `Observables` object (invisible)
#' @export
print.Observables <- function(x, ...) {

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

#' Subsetting method for `Observables` objects
#' @param x An `Observables` object
#' @param i Indices or names of the observables to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `Observables` object
#' @export
`[.Observables` <- function(x, i, ...) {
    structure(
        unclass(x)[i],
        class = "Observables"
    )
}

#' Combine multiple `Observables` objects
#' @param ... `Observables` objects to combine
#' @returns A combined `Observables` object
#' @export
c.Observables <- function(...) {
    obs_list <- list(...)
    if (!all(sapply(obs_list, inherits, "Observables"))) {
        stop("All arguments must be of class 'Observables'")
    }
    obs_list |> 
         lapply(FUN = unclass) |> 
         do.call(what = "c") |>
         structure(class = "Observables")
}
