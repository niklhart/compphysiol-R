# Functions defining the domain-specific language (DSL) used in parameter and rate expressions


#' Subsetting expressions of the form `a[molec,cmt]` and `c[molec,cmt]`
#' 
#' @param x A `compphysiol_dsl` object
#' @param molec A molecule name (symbol)
#' @param cmt A compartment name (symbol)
#' @returns The corresponding value `a[molec,cmt]` or `c[molec,cmt]` retrieved
#'   from the evaluation environment.
#' @export
`[.compphysiol_dsl` <- function(x, molec, cmt) {

    molec <- as.character(substitute(molec))
    cmt <- as.character(substitute(cmt))

    key <- .make_state(molec = molec, cmt = cmt, prefix = x$type)

    get(key, envir = x$env, inherits = TRUE)
}

#' Evaluate an expression in a wrapper environment understanding `a[molec,cmt]` and `c[molec,cmt]`
#'
#' @param expr An expression to evaluate, potentially using the special syntax `a[molec,cmt]` or `c[molec,cmt]`
#' @param envir An environment or list
#' @returns The result of evaluating `expr` in a wrapper environment
#'   that allows subsetting with `a[molec,cmt]` and `c[molec,cmt]` to retrieve values from `envir`.
#' @noRd
.dsl_eval <- function(expr, envir) {

    dsl_env <- new.env(parent = envir)

    dsl_env$c <- structure(
        list(env = envir, type = "c"),
        class = "compphysiol_dsl"
    )
    dsl_env$a <- structure(
        list(env = envir, type = "a"),
        class = "compphysiol_dsl"
    )

    eval(expr, envir = dsl_env)
}

#' Variant of `all_vars()` that counts `a[molec,cmt]` and `c[molec,cmt]` as variables named `a[molec,cmt]` and `c[molec,cmt]`.
#' @param expr An expression
#' @returns A character vector of variables in the input, counting `a[molec,cmt]` and `c[molec,cmt]` as variables.
#' @noRd
.dsl_all_vars <- function(expr) {

    is_special <- function(e) {
        is.call(e) &&
            e[[1]] == quote(`[`) &&
            length(e) > 2 &&
            (e[[2]] == as.name("a") || e[[2]] == as.name("c"))
    }
    env <- new.env(parent = emptyenv())
    env$list <- character()

    collect_vars_and_specials <- function(e) {
        if (is.call(e)) {
            if (is_special(e)) {
                env$list <- c(env$list, deparse1(e))
            } else {
                lapply(as.list(e)[-1], collect_vars_and_specials)
            }

        } else if (is.symbol(e)) {
            env$list <- c(env$list, as.character(e))
        }
    }
    collect_vars_and_specials(expr)
    return(unique(env$list))

}

#' Utility function to create a state variable name based on molecule, compartment, and type (amount or concentration)
#' @param molec The name(s) of the molecule, character scalar or vector
#' @param cmt The name(s) of the compartment, character scalar or vector
#' @param type The type of the state variable, either "amount" (default) or "concentration". Must be of length 1.
#' @param prefix Optional prefix(es) to use instead of "a" for amount and "c" for concentration 
#'   (character scalar or vector of same length as `molec` and `cmt`). If `NULL` (default), the type argument is used 
#'   to determine a global prefix: "a" for amount or "c" for concentration.
#' @returns A string representing the state variable name, in the format `"a[molec, cmt]"` for amount 
#'   or `"c[molec, cmt]"` for concentration
#' @noRd
.make_state <- function(molec, cmt, type = c("amount","concentration"), prefix = NULL) {
    prefix <- prefix %||% switch(match.arg(type), amount = "a", concentration = "c")
    paste0(prefix, "[", molec, ", ", cmt, "]")
}
