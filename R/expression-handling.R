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
    expr <- .as_call(expr)

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


#' Recursively replace symbols in an expression
#'
#' This function traverses the abstract syntax tree (AST) of a quoted R
#' expression and replaces all occurrences of symbols listed in \code{old}
#' with the corresponding ones in \code{new}.
#'
#' @param expr A quoted R expression (e.g. from \code{quote()} or \code{substitute()}).
#' @param old A character vector of symbol names to replace.
#' @param new A character vector of replacement symbol names, of the same length as \code{old}.
#'
#' @return A new expression with symbols replaced.
#' @examples
#' expr <- quote(a * b / c + a)
#' .replace_symbols(expr, c("a", "b"), c("A", "B"))
#' # Returns: quote(A * B / c + A)
#'
#' @noRd
.replace_symbols <- function(expr, old, new) {
    # Convert old/new to a named lookup
    map <- setNames(new, old)

    # Recursive helper function
    recurse <- function(e) {
        if (is.symbol(e)) {
            # If the symbol matches one in 'old', replace it
            name <- as.character(e)
            if (name %in% old) {
                as.symbol(map[[name]])
            } else {
                e
            }
        } else if (is.call(e)) {
            # If it's a call, recursively apply to each argument
            as.call(lapply(e, recurse))
        } else if (is.pairlist(e)) {
            # Handle function arguments or pairlists
            as.pairlist(lapply(e, recurse))
        } else {
            # constants (numbers, strings, etc.) — leave untouched
            e
        }
    }

    recurse(expr)
}




#' Recursively append a suffix to variable symbols in an expression
#'
#' This function traverses the abstract syntax tree (AST) of a quoted R
#' expression and appends a suffix to all variable symbols, *except*:
#' \itemize{
#'   \item function names in calls (e.g. `f(x)` keeps `f` unchanged),
#'   \item symbols listed in \code{skip}.
#' }
#'
#' @param expr A quoted R expression (e.g., from \code{quote()}).
#' @param suffix A character string to append to variable names.
#' @param skip A character vector of symbol names that should not be suffixed.
#'
#' @return A new expression with the suffix appended to variable symbols.
#' @examples
#' expr <- quote(a * b + f(c, g(d)))
#' .suffix_symbols(expr, "_s")
#' # Returns: quote(a_s * b_s + f(c_s, g(d_s)))
#'
#' # Skip specific variables
#' .suffix_symbols(expr, "_s", skip = c("c", "d"))
#' # Returns: quote(a_s * b_s + f(c, g(d)))
#'
#' @noRd
.suffix_symbols <- function(expr, suffix, skip = character()) {

    recurse <- function(e) {
        if (is.symbol(e)) {
            name <- as.character(e)
            if (name %in% skip) {
                e  # leave skipped symbols unchanged
            } else {
                as.symbol(paste0(name, suffix))
            }

        } else if (is.call(e)) {
            # Preserve function name (the first element)
            fn <- e[[1]]
            args <- as.list(e)[-1]
            new_args <- lapply(args, recurse)
            as.call(c(list(fn), new_args))

        } else if (is.pairlist(e)) {
            as.pairlist(lapply(e, recurse))

        } else {
            e  # constants etc.
        }
    }

    recurse(expr)
}
