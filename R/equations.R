#' Create an `Equations` object
#' 
#' Equations allow to define algebraic relationships between model variables,
#' which can be used in flows and observables. The main purpose of equations is to 
#' define physiologically meaningful aggregate parameters (such as cardiac output) 
#' that are composed of more basic parameters (such as blood flows to individual organs).
#' 
#' Observables and equations are very similar, but there are two key differences:
#' 
#' 1. For each observable (but not for equations), `to_ode()` returns a function to calculate it based on the ODE output.
#' 2. Equations can be used in the definitions of flows, while observables cannot.
#' 
#' @param ... Name-expression pairs defining the equations. The expressions can be provided as character strings or as R calls. 
#' @param name Name of the equation(s), character scalar or vector
#' @param expr Expression(s) for the equation(s), as character scalar/vector or as R call / lists of R calls.
#'   Expressions may contain parameters that are added to the model's parameters list, as well as compartment states (not compartment names!)
#' @returns An `Equations` object
#' @examples
#' ## Interactive path with name-expression pairs
#' equations(co = Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski)
#' ## Programmatic path with name and expression vectors
#' equations(name = "co", expr = "Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski")
#' @export
equations <- function(..., name = character(0), expr = character(0)) {

    dotsx <- substitute(list(...))
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
        class = "Equations"
    )
}

#' Names method for `Equations` objects
#' @param x An `Equations` object
#' @returns The names of the equations
#' @export
names.Equations <- function(x) names(unclass(x))

#' Print method for `Equations` objects
#' @param x An `Equations` object
#' @param ... Additional arguments (not used)
#' @returns The `Equations` object (invisible)
#' @export
print.Equations <- function(x, ...) {

    if (length(x) > 0) {
        cat(" Equations:\n")
        cat(sprintf(
                "  (%s) %s = %s\n",
                seq_along(x),
                names(x),
                vapply(x, deparse, character(1))
            ), 
            sep = ""
        )
    } else {
        cat(" Equations: (none)\n")
    }
    invisible(x)
}

#' Subsetting method for `Equations` objects
#' @param x An `Equations` object
#' @param i Indices or names of the equations to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `Equations` object
#' @export
`[.Equations` <- function(x, i, ...) {
    structure(
        unclass(x)[i],
        class = "Equations"
    )
}

#' Combine multiple `Equations` objects
#' @param ... `Equations` objects to combine
#' @returns A combined `Equations` object
#' @export
c.Equations <- function(...) {
    eq_list <- list(...)
    if (!all(sapply(eq_list, inherits, "Equations"))) {
        stop("All arguments must be of class 'Equations'")
    }
    eq_list |> 
         lapply(FUN = unclass) |> 
         do.call(what = "c") |>
         structure(class = "Equations")
}
