# expression handling

#' Substitute states and parameters in an expression
#'
#' @param expr character string or R expression
#' @param stateNames character vector of state names
#' @param name2idx named integer mapping from stateNames -> index
#' @param paramValues named list of parameter values (scalars may be inlined)
#' @param freeParamsEnv Environment containing names of free parameters, will be
#'     updated by reference.
#' @param obsFunc logical, `TRUE` if in observable (matrix indexing)
#'
#' @return An R expression with substitutions applied
substitute_expr <- function(expr, stateNames, name2idx,
                            paramValues = list(),
                            freeParamsEnv = NULL,
                            obsFunc = FALSE) {
    if (is.character(expr)) expr <- parse(text = expr)[[1L]]
    if (is.expression(expr)) expr <- expr[[1L]]

    reserved <- c("t", "y", "params", "pi", "Inf", "NaN",
                  "TRUE", "FALSE", "NULL")

    substitute_symbols <- function(e) {
        if (is.symbol(e)) {
            nm <- as.character(e)

            # 1) state variable
            if (nm %in% stateNames) {
                idx <- name2idx[[nm]]
                if (obsFunc) return(bquote(y[, .(idx)]))
                else return(bquote(y[.(idx)]))
            }

            # 2) paramValues
            if (nm %in% names(paramValues)) {
                val <- paramValues[[nm]]
                if (is.numeric(val) && length(val) == 1L) {
                    return(val)
                } else {
                    if (!is.null(freeParamsEnv)) {
                        freeParamsEnv$list <- union(freeParamsEnv$list, nm)
                    }
                    return(bquote(params[[.(nm)]]))
                }
            }

            # 3) reserved names or builtin functions/operators
            if (nm %in% reserved || exists(nm, mode = "function", inherits = TRUE)) {
                return(e)
            }

            # 4) treat as free parameter
            if (!is.null(freeParamsEnv)) {
                freeParamsEnv$list <- union(freeParamsEnv$list, nm)
            }
            return(bquote(params[[.(nm)]]))
        }

        # recursive substitution for calls
        if (is.call(e)) {
            elems <- lapply(as.list(e), substitute_symbols)
            return(as.call(elems))
        }

        # leave numbers etc. untouched
        e
    }

    substitute_symbols(expr)
}
