#' Inspect model components and experiment collections
#'
#' Print an object at the console for a domain-specific description. Use
#' `str(x)` for its stored structure, including its actual class, units,
#' expressions, and attributes. Standard `str()` arguments such as `max.level`
#' and `list.len` control the amount of detail.
#'
#' Subsetting with `x[i]` retains the component or collection class. For
#' experiments, `x[1]` prints the full details of the first experiment.
#'
#' Compartments, molecules, reactions, states, and transports block direct `[[` access.
#' Use `as.data.frame(x)` for ordinary row and column access, for example
#' `as.data.frame(x)[["name"]]`. Molecules, reactions, and transports also support
#' `as.list(x)`, returning a list of entries rather than columns.
#'
#' Experiments block direct `[[` access too. Use `as.list(x)[[1]]` to inspect
#' the first entry's `parameters`, `dosing`, `observations`, and `start`.
#' Parameter sets and dosing sets support direct `x[[1]]` access.
#'
#' @name inspect
#' @param object A package component or experiment collection.
#' @param ... Arguments passed to [utils::str()].
#' @returns Structure inspection returns `NULL`, invisibly.
#' @examples
#' x <- compartments(c("central", "peripheral"))
#' str(x)
#' as.data.frame(x)[["name"]]
#' e <- experiment(parameters = parameters(BW = 70 [kg]))
#' e[1]
#' str(e)
#' as.list(e)[[1]]$parameters
NULL

# Print the class before inspecting storage directly. Calling str.default()
# on unclassed storage avoids its heuristic probes through public [[ methods.
.str_components <- function(object, ...) {
    classes <- class(object)
    cat(if (length(classes) == 1L) "Class" else "Classes",
        " '", paste(classes, collapse = "', '"), "': ", sep = "")
    str_default <- getS3method("str", "default", envir = asNamespace("utils"))
    str_default(unclass(object), ...)
}

#' @rdname inspect
#' @export
str.compphysiol_compartments <- function(object, ...) {
    .str_components(object, ...)
}

#' @rdname inspect
#' @export
str.compphysiol_molecules <- function(object, ...) {
    .str_components(object, ...)
}

#' @rdname inspect
#' @export
str.compphysiol_reactions <- function(object, ...) {
    .str_components(object, ...)
}

#' @rdname inspect
#' @export
str.compphysiol_states <- function(object, ...) {
    .str_components(object, ...)
}

#' @rdname inspect
#' @export
str.compphysiol_transports <- function(object, ...) {
    .str_components(object, ...)
}
