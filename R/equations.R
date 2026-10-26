#' Create a `compphysiol_equations` object
#' 
#' compphysiol_equations allow to define algebraic relationships between model variables,
#' which can be used in transports and observables. The main purpose of equations is to 
#' define physiologically meaningful aggregate parameters (such as cardiac output) 
#' that are composed of more basic parameters (such as blood flows to individual organs).
#' The symbol `t` is reserved for current model time. Equations that refer to
#' `t`, directly or through another equation, are time dependent.
#' 
#' compphysiol_observables and equations are very similar, but there are two key differences:
#' 
#' 1. compphysiol_observables are evaluated from simulation output, while equations are evaluated inside model expressions.
#' 2. compphysiol_equations can be used in the definitions of transports, while observables cannot.
#' 
#' @param ... Name-expression pairs defining the equations. The expressions can be provided as character strings or as R calls. 
#' @param name Name of the equation(s), character scalar or vector
#' @param expr Expression(s) for the equation(s), as character scalar/vector or as R call / lists of R calls.
#'   Expressions may contain parameters that are added to the model's parameters list, as well as compartment states (not compartment names!)
#' @returns A `compphysiol_equations` object
#' @examples
#' ## Interactive path with name-expression pairs
#' equations(co = Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski)
#' ## Programmatic path with name and expression vectors
#' equations(name = "co", expr = "Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski")
#' @seealso [add_equation()] for adding equations to a model and
#'   [modify_compartment_model] for the complete component map.
#' @export
equations <- function(..., name = character(0), expr = character(0)) {

    dotsx <- substitute(list(...))
    dots <- as.list(substitute(list(...)))[-1]

    if (length(dots) > 0) {
        if (length(name) > 0 || length(expr) > 0) {
            stop("Cannot use both '...' and 'name'/'expr' arguments.")
        }
        name <- names(dots)
        expr <- unname(dots)
    }

    if (!is.vector(expr)) expr <- list(expr)
    if (length(name) != length(expr)) stop("Arguments 'name' and 'expr' must have the same length.")

    names(expr) <- name
    expr <- lapply(expr, .as_call)

    .check_named_values(expr, "equation")
    if ("t" %in% name) {
        stop("Equation name 't' is reserved for the current model time.", call. = FALSE)
    }
    .new_equations(expr)
}

.new_equations <- function(x = list()) {
    structure(x, class = .compphysiol_class("compphysiol_equations"))
}

.equation_time_dependence <- function(equations) {
    eq_names <- names(equations)
    if (!length(eq_names)) return(setNames(logical(), character()))

    dependencies <- lapply(equations, function(expr) {
        intersect(.dsl_all_vars(expr), eq_names)
    })
    time_dependent <- vapply(
        equations,
        function(expr) "t" %in% .dsl_all_vars(expr),
        logical(1)
    )

    repeat {
        updated <- time_dependent | vapply(dependencies, function(x) {
            any(time_dependent[x])
        }, logical(1))
        if (identical(updated, time_dependent)) break
        time_dependent <- updated
    }

    setNames(time_dependent, eq_names)
}

.time_dependent_equation_names <- function(equations) {
    names(Filter(isTRUE, as.list(.equation_time_dependence(equations))))
}

.equation_unit_time_value <- function(parameters) {
    uses_time_units <- any(vapply(
        unclass(parameters),
        function(x) inherits(x, "units") && .has_time_dimension(x),
        logical(1)
    ))
    if (uses_time_units) units::set_units(1, "s", mode = "standard") else 1
}

.model_time_dependent_volume_compartments <- function(model) {
    time_dependent <- .time_dependent_equation_names(model$equations)
    if (!length(time_dependent)) return(character())

    volume_vars <- lapply(model$compartments$volume, function(volume) {
        if (.is_missing_volume(volume)) return(character())
        .dsl_all_vars(.as_call(volume))
    })
    names(model$compartments)[vapply(
        volume_vars,
        function(x) any(x %in% time_dependent) || "t" %in% x,
        logical(1)
    )]
}

#' Add one or several equations to a `compphysiol_compartment_model` object.
#' 
#' @inherit equations description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams equations
#' @param eq A `compphysiol_equations` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @examples
#' ## Interactive path with name-expression pairs
#' compartment_model() |>
#'     add_equation(co = Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski)
#' ## Programmatic path with name and expression vectors
#' compartment_model() |>
#'     add_equation(name = "co", expr = "Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski")
#' ## Programmatic path with compphysiol_equations object
#' eq <- equations(name = "co", expr = "Qadi + Qbon + Qhea + Qkid + Qliv + Qmus + Qski")
#' compartment_model() |>
#'     add_equation(eq)
#' @seealso [equations()] for construction and [modify_compartment_model] for
#'   other model components.
#' @export
add_equation <- function(model, eq, ..., name = character(0), expr = character(0)) {
    UseMethod("add_equation")
}

#' @rdname add_equation
#' @export
add_equation.compphysiol_compartment_model <- function(
    model, eq, ..., name = character(0), expr = character(0)
) {
    .check_class(model, "compphysiol_compartment_model")
    
    call <- match.call()

    eq <- .forward_or_use(
        object_arg_name = "eq",
        constructor_name = "equations",
        call = call,
        parent_env = parent.frame()
    )
    
    .check_class(eq, "compphysiol_equations")

    model$equations <- c(model$equations, eq)
    return(model)
}

#' Names of equations in a `compphysiol_equations` object
#' @param x A `compphysiol_equations` object
#' @returns The names of the equations
#' @export
names.compphysiol_equations <- function(x) names(unclass(x))

#' Print a `compphysiol_equations` object
#' @param x A `compphysiol_equations` object
#' @param ... Additional arguments (not used)
#' @returns The `compphysiol_equations` object (invisible)
#' @export
print.compphysiol_equations <- function(x, ...) {

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

#' Subset a `compphysiol_equations` object
#' @param x A `compphysiol_equations` object
#' @param i Indices or names of the equations to subset
#' @param ... Additional arguments (not used)
#' @returns A subset of the `compphysiol_equations` object
#' @export
`[.compphysiol_equations` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .check_unique_subset_index(i, "Equation")
    .new_equations(unclass(x)[i])
}

#' Combine multiple `compphysiol_equations` objects
#' @param ... `compphysiol_equations` objects to combine
#' @returns A combined `compphysiol_equations` object
#' @export
c.compphysiol_equations <- function(...) {
    eq_list <- list(...)
    if (!all(sapply(eq_list, inherits, "compphysiol_equations"))) {
        stop("All arguments must be of class 'equations'")
    }
    x <- eq_list |>
         lapply(FUN = unclass) |> 
         do.call(what = "c")
    .check_unique_names(names(x), "equation")
    .new_equations(x)
}
