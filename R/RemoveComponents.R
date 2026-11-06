#' Remove selected model or experiment components
#'
#' Removal is local: references in other components are retained and checked
#' when the model is prepared for use. Selection follows component indexing:
#' positions or logical indices, and names for compartments, molecules,
#' equations, observables, and parameters. Unknown names, NA indices,
#' fractional indices, and out-of-range positions are rejected. Negative
#' indices select the complement, as with `[`. Logical indices recycle up to
#' the component length but cannot be longer than it.
#'
#' All removal verbs support compartment models. `remove_parameter()` and
#' `remove_dosing()` also apply the selection to each experiment in a collection.
#' Removing all entries retains an empty component of the same class.
#'
#' @param object A compartment model, or an experiment collection for parameter
#'   and dosing removal.
#' @param i Component indices to remove. Omit to remove all entries.
#' @returns The modified object.
#' @examples
#' model <- compartment_model() |> add_parameter(k = 1, other = 2)
#' model |> remove_parameter("k")
#' model$parameters["k"] <- NULL
#' @name remove_components
NULL

.removal_positions <- function(x, i) {
    if (is.null(i)) return(integer())
    n <- length(x)
    .check_subset_index(i)
    if (is.character(i)) {
        named_classes <- c("compphysiol_compartments", "compphysiol_molecules",
                           "compphysiol_equations", "compphysiol_observables",
                           "compphysiol_parameters")
        if (!any(vapply(named_classes, function(cls) inherits(x, cls), logical(1)))) {
            stop("This component does not support selection by name.", call. = FALSE)
        }
        positions <- match(i, names(x))
        if (anyNA(positions)) stop("Cannot remove unknown component names.", call. = FALSE)
        return(unique(positions))
    }
    if (is.logical(i)) {
        if (length(i) > n) stop("Removal indices are out of range.", call. = FALSE)
    } else if (is.numeric(i)) {
        if (any(!is.finite(i)) || any(i != trunc(i)) || any(abs(i) > n)) {
            stop("Removal indices must be whole numbers within the component range.", call. = FALSE)
        }
        if (any(i < 0) && any(i > 0)) {
            stop("Cannot mix positive and negative removal indices.", call. = FALSE)
        }
    } else {
        stop("Removal indices must be numeric, logical, or character.", call. = FALSE)
    }
    unique(seq_len(n)[i])
}

.remove_component_entries <- function(x, i) {
    if (missing(i)) i <- seq_len(length(x))
    positions <- .removal_positions(x, i)
    x[setdiff(seq_len(length(x)), positions)]
}

.remove_model_component <- function(object, component, i) {
    object[[component]] <- .remove_component_entries(object[[component]], i)
    object
}

.remove_experiment_component <- function(object, component, i) {
    .modify_experiments_scalar(object, component, function(current) {
        .remove_component_entries(current, i)
    }, function(current, incoming) incoming)
}

#' @rdname remove_components
#' @export
remove_compartment <- function(object, i) UseMethod("remove_compartment")

#' @rdname remove_components
#' @export
remove_compartment.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$compartments))
    .remove_model_component(object, "compartments", i)
}

#' @rdname remove_components
#' @export
remove_molecule <- function(object, i) UseMethod("remove_molecule")

#' @rdname remove_components
#' @export
remove_molecule.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$molecules))
    .remove_model_component(object, "molecules", i)
}

#' @rdname remove_components
#' @export
remove_transport <- function(object, i) UseMethod("remove_transport")

#' @rdname remove_components
#' @export
remove_transport.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$transports))
    .remove_model_component(object, "transports", i)
}

#' @rdname remove_components
#' @export
remove_reaction <- function(object, i) UseMethod("remove_reaction")

#' @rdname remove_components
#' @export
remove_reaction.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$reactions))
    .remove_model_component(object, "reactions", i)
}

#' @rdname remove_components
#' @export
remove_equation <- function(object, i) UseMethod("remove_equation")

#' @rdname remove_components
#' @export
remove_equation.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$equations))
    .remove_model_component(object, "equations", i)
}

#' @rdname remove_components
#' @export
remove_observable <- function(object, i) UseMethod("remove_observable")

#' @rdname remove_components
#' @export
remove_observable.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$observables))
    .remove_model_component(object, "observables", i)
}

#' @rdname remove_components
#' @export
remove_parameter <- function(object, i) UseMethod("remove_parameter")

#' @rdname remove_components
#' @export
remove_parameter.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$parameters))
    .remove_model_component(object, "parameters", i)
}

#' @rdname remove_components
#' @export
remove_dosing <- function(object, i) UseMethod("remove_dosing")

#' @rdname remove_components
#' @export
remove_dosing.compphysiol_compartment_model <- function(object, i) {
    if (missing(i)) i <- seq_len(length(object$doses))
    .remove_model_component(object, "doses", i)
}

#' @rdname remove_components
#' @export
remove_parameter.compphysiol_experiments <- function(object, i) {
    if (missing(i)) {
        return(.modify_experiments_scalar(object, "parameters",
            function(current) current[integer()], function(current, incoming) incoming))
    }
    .remove_experiment_component(object, "parameters", i)
}

#' @rdname remove_components
#' @export
remove_dosing.compphysiol_experiments <- function(object, i) {
    if (missing(i)) {
        return(.modify_experiments_scalar(object, "dosing",
            function(current) current[integer()], function(current, incoming) incoming))
    }
    .remove_experiment_component(object, "dosing", i)
}

#' Delete component entries with NULL replacement
#'
#' `x[i] <- NULL` removes component entries, preserving their class and internal
#' structure. Index rules are the same as [remove_components]. Equation and
#' observable components also retain ordinary list replacement for non-NULL
#' values. Other methods on this page support deletion only.
#'
#' @param x A compartment, molecule, transport, reaction, equation, observable,
#'   or dosing component.
#' @param i Entries to remove; omit to remove all entries.
#' @param ... Unused.
#' @param value `NULL`, or non-NULL list replacement for equations and observables.
#' @returns The component with selected entries removed.
#' @name delete_component_entries
NULL

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_compartments` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        stop("Component replacement supports NULL deletion only.", call. = FALSE)
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_molecules` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        stop("Component replacement supports NULL deletion only.", call. = FALSE)
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_transports` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        stop("Component replacement supports NULL deletion only.", call. = FALSE)
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_reactions` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        stop("Component replacement supports NULL deletion only.", call. = FALSE)
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_equations` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        out <- unclass(NextMethod("[<-"))
        .check_named_values(out, "equation")
        return(.new_equations(out))
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_observables` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        out <- unclass(NextMethod("[<-"))
        .check_named_values(out, "observable")
        return(.new_observables(out))
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

#' @rdname delete_component_entries
#' @export
`[<-.compphysiol_dosing` <- function(x, i, ..., value) {
    if (!is.null(value)) {
        stop("Component replacement supports NULL deletion only.", call. = FALSE)
    }
    if (length(list(...))) stop("Select component entries with one index.", call. = FALSE)
    .remove_component_entries(x, i)
}

