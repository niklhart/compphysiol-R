# Parameters class. Parameters are obtained from `Physiology` objects or empirically specified, and can be passed to `CompartmentModel` objects.

#' `Parameters` class.
#'
#' The `Parameters` class represents a collection of named parameters, which can optionally have associated units.
#' 
#' Parameters can be specified in three different ways:
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
#' @returns A `Parameters` object containing the specified parameters.
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

    structure(
        value %||% list(),
        class = c("Parameters", "list")
    )
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

#' Add one or several parameters to a `CompartmentModel` object.
#' 
#' @inherit parameters description details
#' @param model A `CompartmentModel` object.
#' @inheritParams parameters
#' @param param A `Parameters` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @export
add_parameter <- function(
    model,
    ...,
    name = NULL,
    value = NULL,
    unit = NULL,
    param
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    param <- .forward_or_use(
        object_arg_name = "param",
        constructor_name = "parameters",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(param, "Parameters")

    model$parameters <- c(model$parameters, param)
    return(model)
}


#' Names of parameters in a `Parameters` object
#' @param x A `Parameters` object
#' @returns The names of the parameters
#' @export
names.Parameters <- function(x) names(unclass(x))

#' Subset a `Parameters` object
#' @param x A `Parameters` object
#' @param i Indices or names of the parameters to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `Parameters` object
#' @export
`[.Parameters` <- function(x, i, ...) {
    structure(
        unclass(x)[i],
        class = c("Parameters", "list")
    )
}

#' Replace parts of a `Parameters` object
#' @param x A `Parameters` object
#' @param i Indices or names of the parameters to replace
#' @param value Another `Parameters` object with the new values to insert
#' @returns The modified `Parameters` object
#' @export
`[<-.Parameters` <- function(x, i, value) {
    x <- unclass(x)
    x[i] <- unclass(value)
    if (is.character(i)) i <- match(i, names(x))
    names(x)[i] <- names(value)
    structure(
        x,
        class = c("Parameters", "list")
    )
}

#' Combine multiple `Parameters` objects into one
#' @param ... `Parameters` objects to combine
#' @returns A combined `Parameters` object
#' @export
c.Parameters <- function(...) {
    params_list <- list(...)
    if (!all(sapply(params_list, inherits, "Parameters"))) {
        stop("All arguments must be of class 'Parameters'")
    }
    params <- params_list |>
        lapply(FUN = unclass) |>
        do.call(what = "c")
    .check_parameter_names(names(params))
    params |>
        structure(class = c("Parameters", "list"))
}

#' Print a `Parameters` object
#' @param x An `Parameters` object
#' @param ... Additional arguments (not used)
#' @returns The `Parameters` object (invisible)
#' @export
print.Parameters <- function(x, ...) {
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

#' Unlist a `Parameters` object (experimental).
#' 
#' Parameters may be scalars or vectors, but for some purposes it may be desirable to unlist them into a flat `Parameters` object.
#' 
#' @param x A `Parameters` object
#' @param ... Additional arguments (not used)
#' @returns An unlisted `Parameters` object
#' @method unlist Parameters
#' @export
unlist.Parameters <- function(x, ...) {
    unlisted <- unlist(unclass(x), recursive = FALSE, use.names = TRUE)
    structure(
        as.list(unlisted),
        class = c("Parameters", "list")
    )
}
