#' Create an `Equations` object
#' @param name Name of the equation(s), character scalar or vector
#' @param expr Expression (as an R call)
#' @returns An `Equations` object
#' @examples
#' equations(
#'     name = "co", 
#'     expr = "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski"
#' )
#' @export
equations <- function(name = character(0), expr = character(0)) {

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
