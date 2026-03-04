
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
#' @param ... Name-expression pairs defining the observables. The expressions can be provided as character strings or as R calls. 
#' @param name Name of the observable(s), character scalar or vector
#' @param expr Expression(s) for the observable(s), as character scalar/vector or as R call / lists of R calls.
#'   Expressions may contain parameters that are added to the model's parameters list, as well as compartment states (not compartment names!)
#' @returns An `Observables` object
#' @examples
#' ## Interactive path with name-expression pairs
#' observables(
#'     Cpla = Acen/Vcen/BP,
#'     Cblo = Acen/Vcen
#' )
#' ## Programmatic path with name and expression vectors
#' observables(
#'     name = c("Cpla","Cblo"), 
#'     expr = c("Acen/Vcen/BP", "Acen/Vcen")
#' )
#' @export
observables <- function(..., name = character(0), expr = character(0)) {

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

    names(expr) <- name
    expr <- lapply(expr, .as_call)

    structure(
        expr,
        class = "Observables"
    )
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
