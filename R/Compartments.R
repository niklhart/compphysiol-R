#' `Compartments` Class
#'
#' Represents one or several model compartments with names and initial amounts.
#' @param name Compartment name(s), character scalar or vector
#' @param initial Initial amounts for the compartments (numeric scalar or vector, default = `0`)
#' @param unit Units for the initial amounts (character scalar or vector, default = `NULL`).
#'   If provided, must be either a single unit applied to all compartments or a vector of units matching the length of `name`.
#' @param state State names for the compartments (character scalar or vector, default = `"A" + name`).
#' @return A `Compartments` object
#' @examples
#' compartments(c("adi","bon"), initial = 10)
#' compartments('adi', unit = "mg")
#' compartments('adi', initial = 10 [mg])
#' @export
compartments <- function(
    name = character(0),
    initial = 0,
    unit = NULL,
    state = paste0("A", name, recycle0 = TRUE)
) {
    initial <- .process_nse_arg(
        expr = substitute(initial),
        envir = parent.frame(n = 1)
    )
    initial <- as.list(initial)
    if (length(initial) == 1) initial <- rep(initial, length(name))
    if (any(length(name) != c(length(initial), length(state)))) {
        stop("Arguments 'name', 'initial' and 'state' must have the same length.")
    }
    if (anyDuplicated(name)) stop("Duplicate compartment names are not allowed.")
    if (anyDuplicated(state)) stop("Duplicate state names are not allowed.")

    if (!is.null(unit)) {
        if (length(unit) == 1) unit <- rep(unit, length(name))
        if (length(unit) != length(name)) {
            stop("Argument 'unit' must be NULL, a scalar, or have the same length as 'name'.")
        }

        initial <- Map(
            function(init, u) if (u != "") units::set_units(init, u, mode = "standard") else init,
            initial, unit
        )
    }

    structure(
        list(name = name, initial = initial, state = state),
        class = "Compartments"
    )
}

#' Length method for `Compartments` class
#' Returns the number of compartments in a `Compartments` object.
#' @param x A `Compartments` object
#' @return The number of compartments (length of the `name` vector)
#' @export
length.Compartments <- function(x) {
    length(x$name)
}

#' Names method for `Compartments` class
#' Returns the names of compartments in a `Compartments` object.
#' @param x A `Compartments` object
#' @return A character vector of compartment names
#' @export
names.Compartments <- function(x) x$name

#' States method for `Compartments` class
#' Returns the states of compartments in a `Compartments` object.
#' @param comp A `Compartments` object
#' @return A character vector of compartment states
#' @export
states <- function(comp) {
    .check_class(comp, "Compartments")
    return(comp$state)
}

#' Initial conditions of a `Compartments` object
#' Returns the initial amounts of compartments in a `Compartments` object. The type of the returned object depends 
#' on the presence of units in the initial conditions: 
#' 
#' - if all compartment initial conditions are numeric without units, a numeric vector is returned
#' - if all compartment initial conditions have consistent units, the returned vector will be of class `units`, 
#' - if the compartment initial conditions have mixed units,  the returned vector will be of class `mixed_units`.
#' 
#' @param comp A `Compartments` object
#' @param named If `TRUE`, returns a named list of initial amounts; if `FALSE` (default), it is unnamed
#' @return A vector of initial amounts for each compartment
#' @export
initials <- function(comp, named = FALSE) {
    .check_class(comp, "Compartments")
    y0 <- comp$initial

    # Allow mixed units in initial conditions if required by temporarily setting allow_mixed = TRUE
    oldopt <- units::units_options(allow_mixed = TRUE)
    on.exit(units::units_options(oldopt), add = TRUE)

    # flatten list of initial values into a vector
    y0 <- do.call(what = c, args = y0) %||% numeric(0)

    return(if (named) setNames(y0, nm = names(comp)) else y0)
} 
#' Print method for `Compartments` class
#' Pretty-prints a `Compartments` object.
#' @param x A `Compartments` object.
#' @param ... ignored
#' @return The `Compartments` object (invisibly).
#' @export
print.Compartments <- function(x, ...) {

    init_str <- vapply(x$initial, format, character(1))
    if (length(x) > 0) {
        cat(" Compartments:\n")
        cat(
            sprintf("  (%s) %s (state = %s, initial = %s)\n", seq_along(x), x$name, x$state, init_str),
            sep = ""
        )
    } else {
        cat(" Compartments: (none)\n")
    }
    invisible(x)
}

#' Combine multiple `Compartments` objects into one
#' Combines multiple `Compartments` objects by concatenating their names and initial values.
#' @param ... One or more `Compartments` objects to combine.
#' @return A single `Compartments` object containing all compartments from the inputs.
#' @export
c.Compartments <- function(...) {
    objs <- list(...)

    # ensure all inputs are Compartments
    if (!all(vapply(objs, inherits, logical(1), "Compartments"))) {
        stop("All inputs must be Compartments objects.")
    }

    name <- unlist(lapply(objs, `[[`, "name"))
    state <- unlist(lapply(objs, `[[`, "state"))

    if (anyDuplicated(name)) stop("Duplicate compartment names are not allowed.")
    if (anyDuplicated(state)) stop("Duplicate compartment states are not allowed.")
    
    initial <- do.call(c, lapply(objs, `[[`, "initial"))

    structure(
        list(name = name, initial = initial, state = state),
        class = "Compartments"
    )
}

#' Subset method for `Compartments` class
#' 
#' Allows subsetting a `Compartments` object by index or name, 
#' returning a new `Compartments` object with the selected compartments.
#' @param x A `Compartments` object
#' @param i Index or name(s) of compartments to select
#' @param ... ignored
#' @return A new `Compartments` object containing only the selected compartments
#' @export
`[.Compartments` <- function(x, i, ...) {
    # Allow character indexing via names
    if (is.character(i)) {
        i <- match(i, x$name)
        if (any(is.na(i))) {
            stop("Unknown compartment name.")
        }
    }

    structure(
        list(name = x$name[i], initial = x$initial[i], state = x$state[i]),
        class = "Compartments"
    )
}
