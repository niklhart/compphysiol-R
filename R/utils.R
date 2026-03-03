
#' Helper function to uniformize input into an call
#'
#' @param input A character string, expression, numeric scalar, or call
#' @returns A call representation of the input
#' @noRd
.as_call <- function(input) {
    if (is.character(input)) {
        str2lang(input)
    } else if (is.expression(input)) {
        input[[1]]
    } else if (is.language(input) || is.null(input)) { # quote() returns a call
        input
    } else if (is.numeric(input) && length(input) == 1L) {
        as.call(list(as.name("("), input))
    } else {
        stop("Input must be a character, expression, numeric scalar, or quoted call.")
    }
}

#' Helper function to check for empty character input
#' @param x Input to check
#' @returns `TRUE` if `x` is `NULL`, of length zero, or a single empty string
#' @noRd
.is_emptychar <- function(x) {
    is.null(x) || length(x) == 0L || (length(x) == 1 && x == "")
}

#' Helper function to wrap objects into a list if they are not already lists
#' @param x Input object
#' @returns A list containing `x` if `x` is not already a list
#' @noRd
.wrap_into_list <- function(x) {
    if (is.list(x)) x else list(x)
}


#' Helper function to check if an object inherits from a specified class
#' 
#' Throws an error if `x` does not inherit from `class`
#' 
#' @param x Object to check
#' @param class Expected class name (string)
#' @returns The function returns the input invisibly if the check passes, otherwise it throws an error
#' @noRd
.check_class <- function(x, class) {
    if (!inherits(x, class)) {
        stop("Expected object of class '", class, "', got '", class(x)[1], "'.")
    }
    invisible(x)
}

#' Process a quoted call `expr` of the form `value` or `value[unit]` into a `units` object or numeric value
#' @param expr Quoted call to process
#' @returns A `units` object if a unit is specified, otherwise a numeric value
#' @noRd
.process_nse_arg <- function(expr, envir = parent.frame(n = 1)) {
    if (is.call(expr) && length(expr) == 3 && expr[[1]] == quote(`[`)) {
        val <- eval(expr[[2]], envir = envir)
        unit <- paste(deparse(expr[[3]]), collapse = "")
        if (inherits(val, 'units') && !units::ud_are_convertible(units(val), unit)) {
            stop(sprintf("Value %s cannot be converted to specified unit '%s'.", val, unit))
        }
        res <- units::set_units(val, unit, mode = "standard")
    } else {
        res <- eval(expr, envir = envir)
    }
    res
}


#' Helper function to forward the NSE logic from pipable add_x methods to the underlying constructors
#' @param object_arg_name Name of the argument in the constructor that takes the object being added to 
#'   (e.g. "comp" for `add_compartment()`, "dose" for `add_dosing()`)
#' @param constructor_name Name of the constructor function to call (e.g. "compartments" for `add_compartment()`, 
#'   "dosing" for `add_dosing()`)
#' @param call The matched call from the add_x method
#' @param parent_env The parent environment to evaluate the call in (usually `parent.frame()`)
#' @returns The object created by the constructor, which will then be added to the model by the add_x method
#' @noRd
.forward_or_use <- function(
    object_arg_name, # string, e.g. "comp"
    constructor_name, # string, e.g. "compartments"
    call,
    parent_env
) {
    if (!is.null(call[[object_arg_name]])) {
        # object explicitly supplied
        return(eval(call[[object_arg_name]], parent_env))
    }

    # Rewrite call to constructor
    call[[1]] <- as.name(constructor_name)
    call$model <- NULL

    eval(call, parent_env)
}