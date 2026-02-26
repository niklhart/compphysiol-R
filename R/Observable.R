# S3 redesign of Observable class

#' Create an `Observables` object
#' @param name Name of the observable(s), character scalar or vector
#' @param expr Expression (as an R call)
#' @returns An `Observables` object
#' @examples
#' observables(
#'     name = c("Cpla","Cblo"), 
#'     expr = c("Central/Vcentral/BP", "Central/Vcentral")
#' )
#' @export
observables <- function(name, expr) {
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

#' Create an empty `Observables` object
#' @returns An empty `Observables` object
#' @export
empty_observable <- function() observables(name = character(0), expr = character(0))

# Old R6 implementation of Observable class, now replaced by S3 redesign

#' Observable Class
#'
#' Represents a model observable
#'
#' @export
Observable <- R6::R6Class("Observable",                  
    public = list(
        #' @field name Name of the observable
        name = NULL,
        #' @field expr Expression (as an R call)
        expr = NULL,
        #' @description
        #' Initialize a new `Observable` object.
        #' @param name Name of the observable
        #' @param expr Expression (character or function)
        #' @return A new `Observable` object
        initialize = function(name, expr) {
            self$name <- name
            self$expr <- .as_call(expr)
        },
        #' @description
        #' Print the observable
        #' @param ... Additional arguments (not used)
        #' @return The `Observable` object (invisible)
        print = function(...) {
            cat(sprintf("Observable: %s = %s\n",
                        self$name, deparse(self$expr)))
            invisible(self)
        }
    )
)

#' Construct one or more Observable objects
#'
#' `ObservableList()` is a wrapper function to construct multiple `Observable` objects at once.
#' It accepts vectorized arguments of identical length.
#'
#' @param name Observable name(s), character scalar or vector
#' @param expr Observable expression(s), (lists of) expressions or functions, or character scalar or vector
#' @return A list of `Observable` objects.
#' @examples
#' ObservableList(c("Cpla","Cblo"), expr = c("Central/Vcentral/BP", "Central/Vcentral"))
#'
#' @export
ObservableList <- function(name, expr) {
    if (!is.vector(expr)) expr <- list(expr)
    if (length(name) != length(expr)) stop("Arguments 'name' and 'expr' must have the same length.")
    
    Map(Observable$new, name, expr, USE.NAMES = FALSE)
}