
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

#' Convert a variable with or without units to a specified set of dimensions.
#' 
#' This function is used to create a consistent internal representation of variables.
#' 
#' @param var A numeric variable, with or without units
#' @param length The unit to convert length dimensions to (default "m")
#' @param mass The unit to convert mass dimensions to (default "kg")
#' @param time The unit to convert time dimensions to (default "s")
#' @param amount The unit to convert amount of substance dimensions to (default "mol")
#' @param current The unit to convert electric current dimensions to (default "A")
#' @param temperature The unit to convert temperature dimensions to (default "K")
#' @param intensity The unit to convert luminous intensity dimensions to (default "cd")
#' @returns The variable converted to the specified dimensions if it has units, otherwise returned unchanged
#' @noRd
.to_dimensions <- function(var, length = "m", mass = "kg", time = "s", amount = "mol", current = "A", temperature = "K", intensity = "cd") {
    inherits(var, "units") || return(var)

    var_si <- units::convert_to_base(var)

    si_units <- c("m", "kg", "s", "mol", "A", "K", "cd")
    output_units <- c(length, mass, time, amount, current, temperature, intensity)

    is_convertible <- units::ud_are_convertible(si_units, output_units)
    all(is_convertible) || stop(
        sprintf(
            "The following SI dimensions cannot be converted to the specified output dimensions: %s",
            paste(si_units[!is_convertible], collapse = ", ")
        )
    )
    
    map <- setNames(output_units, nm = si_units)
    units(var_si)$numerator <- unname(map[units(var_si)$numerator])
    units(var_si)$denominator <- unname(map[units(var_si)$denominator])
    var_si
}

#' Apply `.to_dimensions()` to a vector of variables (TODO: improve naming and documentation)
#' 
#' @param vars A vector of variables, each with or without units
#' @param dimensions A list of dimension specifications to pass to `.to_dimensions()`
#' @returns A numeric vector of the variables converted to the specified dimensions 
#'   if they have units, otherwise returned unchanged
#' @noRd
.to_dimensions_vec <- function(vars, dimensions) {
    if (!inherits(vars, "units")) return(vars)
    vars |>
        lapply(function(x) do.call(.to_dimensions, c(list(x), dimensions))) |>
        vapply(function(x) units::set_units(x, NULL), numeric(1))
}

#' Substitute equations in transports
#' 
#' This function takes a `Transports` object and an `Equations` object, and substitutes 
#' the equations into the rate expressions of the transports.
#' 
#' @param trans A `Transports` object
#' @param eqs An `Equations` object
#' @returns A `Transports` object with the equations substituted into the rate expressions
#' @noRd
.subst_eq <- function(trans, eqs) {
    lapply(trans, function(tr) {
        tr$rate <- do.call("substitute", list(tr$rate, env = unclass(eqs)))
        if (!is.null(tr$const)) {
            tr$const <- do.call("substitute", list(tr$const, env = unclass(eqs)))
        }
        tr
    }) |> do.call(what = "c")
}




#' Subsetting logic for data frame-like classes (named or unnamed)
#' 
#' This function is used to implement the `[` method for classes like `Molecules`, `Compartments`, etc. 
#' It applies the subset to the underlying data frame and then reconstructs the object with the same class.
#' 
#' @param x The object to subset (e.g. a `Molecules` object)
#' @param i The row indices to subset
#' @param byname If `TRUE`, allows subsetting by name; if `FALSE` (default), only allows numeric indices
#' @param ... Additional arguments (not used)
#' @returns A subsetted object of the same class (e.g. a `Molecules` object)
#' @noRd
.subset_df_like <- function(x, i, byname = FALSE, ...) {

    # Handle character indexing via names
    if (is.character(i)) {
        if (!byname) stop(sprintf("Subsetting by name is not allowed for class '%s'.", class(x)[1]))
        i <- match(i, names(x))
        if (any(is.na(i))) stop("Unknown name.")
    }

    df_subset <- as.data.frame(x)[i, , drop = FALSE]
    rownames(df_subset) <- NULL
    
    structure(df_subset, class = class(x))
}

#' Combine multiple data frame-like objects (e.g. `Molecules`, `Compartments`) into one
#'
#' @param ... Multiple objects of the same class to combine
#' @returns A combined object of the same class
#' @noRd
.combine_df_like <- function(...) {
    objs <- list(...)
    class1 <- class(objs[[1]])
    if (!all(sapply(objs, function(o) inherits(o, class1)))) {
        stop(sprintf("All inputs must be of class '%s'.", class1[1]))
    }

    # Combine the data frames by row-binding
    combined_df <- do.call(rbind, lapply(objs, as.data.frame))
    structure(combined_df, class = class(objs[[1]]))
}

#' Extraction method for data frame-like class
#' 
#' These methods are intentionally not implemented to prevent direct element access, 
#' which could lead to confusion given their internal data frame-like structure. 
#' Instead, users should use subsetting with `[` and a scalar index.
#' @param x The object to extract from
#' @param i Row index to access
#' @param ... Additional arguments (not used)
#' @returns Nothing (errors)
#' @noRd
.extract_df_like <- function(x, i, ...) {

    stop(sprintf(
        "Direct element access is not supported for class '%s'. Use subsetting with [ and a scalar index instead.",
        class(x)[1]
    ))
}


#' Convert a data frame-like object to a list of lists, where each inner list represents a row of the data frame
#' @param x The data frame-like object to convert
#' @returns A list of lists, where each inner list represents a row of the data frame
#' @noRd
.listify_df_like <- function(x) {
    x <- as.data.frame(x)
    lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
}

#' Replace all occurrences of `[x]` in a call by `[x,val]` or `[val,x]`
#' @param expr A call
#' @param pos The position to insert the index in (1 for first argument, 2 for second)
#' @param val The value to insert as the index
#' @returns A call with the indices replaced
#' @noRd
.add_expr_index <- function(expr, pos, val) {

    # recursive substitution for calls
    if (is.call(expr)) {
        if (expr[[1]] == quote(`[`) && length(expr) == 3) {
            return(as.call(append(as.list(expr), values = list(as.name(val)), after = pos+1)))
        }
        elems <- lapply(as.list(expr), .add_expr_index, pos = pos, val = val)
        return(as.call(elems))
    }

    # leave numbers etc. untouched
    expr
}

