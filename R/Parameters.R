# Parameters class. Parameters are obtained from `Physiology` objects or empirically specified, and can be passed to `CompartmentModel` objects.

#' `compphysiol_parameters` class.
#'
#' The `compphysiol_parameters` class represents a collection of named parameters, which can optionally have associated units.
#' 
#' compphysiol_parameters can be specified in three different ways:
#' 
#' 1. As name-value pairs, where values can be categorical, numeric or `units` objects, e.g., `parameters(A = 2, B = units::set_units(3, "m"))`.
#' 2. As name-value pairs specifying the unit in square brackets after the value (e.g., `A = 2 [m]`). 
#'    This allows for a more concise syntax when specifying parameters with units.
#' 3. Via the `name`, `value`, and `unit` arguments (where `name` is a character vector of parameter names, 
#' `value` is a numeric vector of parameter values, and `unit` is an optional character vector of units).
#'
#' @param ... Parameter values as name-value pairs, where values can optionally have units (e.g., `A = 2 [m]`).
#'   Values are evaluated from left to right and may refer to parameters
#'   defined earlier in the same call.
#' @param name Optional parameter names (if not using named arguments).
#' @param value Optional parameter values (if not using named arguments).
#' @param unit Optional parameter units (if not using named arguments).
#' @returns A `compphysiol_parameters` object containing the specified parameters.
#' @examples
#' # Specification using name/value/unit vectors (programmatic use)
#' parameters(name = c("A", "B"), value = c(2, 3), unit = c("", "m"))
#' # Shorthand notation for interactive use
#' parameters(A = 2, B = 3[m])
#' @export
parameters <- function(..., name = NULL, value = NULL, unit = NULL) {

    # Non-standard evaluation to capture parameter names and values
    args <- eval(substitute(alist(...)))

    if (length(args) > 0) {
        if (!is.null(name) || !is.null(value) || !is.null(unit)) {
            stop("Cannot use both '...' and 'name'/'value/unit' arguments.")
        }
        .check_parameter_names(names(args))
        value <- .process_parameter_args(args, envir = parent.frame())

    } else if (!is.null(name)) {
        if (length(name) != length(value)) stop("All parameters must be named.")
        .check_parameter_names(name)
        if (!(length(unit) %in% c(0,1,length(value)))) stop("'unit' must be NULL, scalar or match length of 'value'.")
        if (length(unit) == 1) unit <- rep(unit, length(value))
        value <- if (!is.null(unit)) {
                Map(function(v, u) if (u != "") units::set_units(v, u, mode = "standard") else v, value, unit)
            } else {
                list(value)
            } 
        value <- setNames(value, nm = name)
    }

    x <- .new_parameters(value %||% list())
    .validate_parameters(x)
}

.new_parameters <- function(x = list()) {
    structure(x, class = .class_parameters())
}

.validate_parameters <- function(x) {
    if (!is.list(unclass(x))) stop("Parameters must use a list representation.", call. = FALSE)
    if (length(x)) .check_parameter_names(names(x))
    x
}

.process_parameter_args <- function(args, envir) {
    param_names <- names(args)
    eval_env <- new.env(parent = envir)
    value <- vector("list", length(args))
    names(value) <- param_names

    for (i in seq_along(args)) {
        future_names <- if (i < length(args)) param_names[(i + 1):length(args)] else character()
        future_refs <- intersect(.expr_symbols(args[[i]]), future_names)
        if (length(future_refs) > 0) {
            stop(
                sprintf(
                    "Parameter '%s' refers to later parameter%s: %s.",
                    param_names[[i]],
                    if (length(future_refs) == 1) "" else "s",
                    paste(future_refs, collapse = ", ")
                ),
                call. = FALSE
            )
        }

        value[[i]] <- .process_nse_arg(args[[i]], envir = eval_env)
        assign(param_names[[i]], value[[i]], envir = eval_env)
    }

    value
}

.expr_symbols <- function(expr) {
    if (is.symbol(expr)) {
        return(as.character(expr))
    }
    if (!is.call(expr)) {
        return(character())
    }

    expr <- as.list(expr)
    if (identical(expr[[1]], quote(`[`)) && length(expr) == 3) {
        return(.expr_symbols(expr[[2]]))
    }

    unique(unlist(lapply(expr[-1], .expr_symbols), use.names = FALSE))
}

.check_parameter_names <- function(name) {
    if (is.null(name) || any(name == "")) {
        stop("All parameters must be named.", call. = FALSE)
    }
    if (anyDuplicated(name)) {
        stop("Parameter names must be unique.", call. = FALSE)
    }

    invisible(NULL)
}

#' Add one or several parameters to a `compphysiol_compartment_model` object.
#' 
#' @inherit parameters description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams parameters
#' @param param A `compphysiol_parameters` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @export
add_parameter <- function(
    model,
    ...,
    name = NULL,
    value = NULL,
    unit = NULL,
    param
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    param <- .forward_or_use(
        object_arg_name = "param",
        constructor_name = "parameters",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(param, "compphysiol_parameters")

    model$parameters <- c(model$parameters, param)
    return(model)
}


#' Names of parameters in a `compphysiol_parameters` object
#' @param x A `compphysiol_parameters` object
#' @returns The names of the parameters
#' @export
names.compphysiol_parameters <- function(x) names(unclass(x))

#' Subset a `compphysiol_parameters` object
#' @param x A `compphysiol_parameters` object
#' @param i Indices or names of the parameters to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `compphysiol_parameters` object
#' @export
`[.compphysiol_parameters` <- function(x, i, ...) {
    .new_parameters(unclass(x)[i])
}

#' Replace parts of a `compphysiol_parameters` object
#' @param x A `compphysiol_parameters` object
#' @param i Indices or names of the parameters to replace
#' @param value Another `compphysiol_parameters` object with the new values to insert
#' @returns The modified `compphysiol_parameters` object
#' @export
`[<-.compphysiol_parameters` <- function(x, i, value) {
    x <- unclass(x)
    x[i] <- unclass(value)
    if (is.character(i)) i <- match(i, names(x))
    names(x)[i] <- names(value)
    x <- .new_parameters(x)
    .validate_parameters(x)
}

#' Combine multiple `compphysiol_parameters` objects into one
#' @param ... `compphysiol_parameters` objects to combine
#' @returns A combined `compphysiol_parameters` object
#' @export
c.compphysiol_parameters <- function(...) {
    params_list <- list(...)
    if (!all(sapply(params_list, inherits, "compphysiol_parameters"))) {
        stop("All arguments must be of class 'parameters'")
    }
    params <- params_list |>
        lapply(FUN = unclass) |>
        do.call(what = "c")
    .check_parameter_names(names(params))
    params <- .new_parameters(params)
    .validate_parameters(params)
}

#' Print a `compphysiol_parameters` object
#' @param x A `compphysiol_parameters` object
#' @param ... Additional arguments (not used)
#' @returns The `compphysiol_parameters` object (invisible)
#' @export
print.compphysiol_parameters <- function(x, ...) {
    if (length(x) > 0) {
        cat(" Parameters:\n")
        cat(
            sprintf(
                "  (%s) %s = %s\n",
                seq_along(x),
                names(x),
                vapply(x, format, "")
            ),
            sep = ""
        )
    } else {
        cat(" Parameters: (none)\n")
    }
    invisible(x)
}

#' Unlist a `compphysiol_parameters` object (experimental).
#' 
#' compphysiol_parameters may be scalars or vectors, but for some purposes it may be desirable to unlist them into a flat `compphysiol_parameters` object.
#' 
#' @param x A `compphysiol_parameters` object
#' @param ... Additional arguments (not used)
#' @returns An unlisted `compphysiol_parameters` object
#' @method unlist compphysiol_parameters
#' @export
unlist.compphysiol_parameters <- function(x, ...) {
    unlisted <- unlist(unclass(x), recursive = FALSE, use.names = TRUE)
    x <- .new_parameters(as.list(unlisted))
    .validate_parameters(x)
}
