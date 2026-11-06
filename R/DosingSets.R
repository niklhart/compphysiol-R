#' Collect alternative dosing schedules
#'
#' Each element is a complete [dosing()] schedule, including an empty schedule
#' for an untreated experiment. Names are optional and must be unique when
#' supplied. Positive whole-number names are reserved for positional identifiers.
#' Schedules retain their units; compatibility with an experiment is checked
#' when they are applied. Use [replace_dosing()] or [add_dosing()] to expand a
#' scalar experiment or align schedules with an experiment collection.
#'
#' @param ... Dosing objects for construction; dosing-set collections for `c()`.
#' @param x A dosing-set collection.
#' @param i Numeric, logical, or character indices.
#' @param value A dosing-set collection for `[<-`, a dosing object or `NULL`
#'   for `[[<-`, or replacement names for `names<-`.
#' @param exact Whether extraction matches names exactly.
#' @returns A `compphysiol_dosing_sets` collection, except `[[` extracts one
#'   dosing object (or `NULL` for an unknown name), `names()` returns names,
#'   `length()` returns the schedule count, and printing returns `x` invisibly.
#' @examples
#' regimens <- dosing_sets(
#'     control = dosing(),
#'     treated = dosing(time = 0 [h], amount = 100 [mg])
#' )
#' experiment() |> replace_dosing(regimens)
#' @export
dosing_sets <- function(...) {
    .dosing_sets_validate_result(list(...))
}

.new_dosing_sets <- function(x) {
    structure(x, class = .compphysiol_class("compphysiol_dosing_sets", base = "list"))
}

.dosing_sets_elements <- function(x) unclass(x)

.dosing_sets_validate_result <- function(x) {
    if (!all(vapply(x, inherits, logical(1), "compphysiol_dosing"))) {
        stop("All elements must be dosing objects.", call. = FALSE)
    }
    .check_optional_unique_names(x, "dosing set")
    .collection_identifiers(x, "Dosing set")
    .new_dosing_sets(x)
}

#' @rdname dosing_sets
#' @export
length.compphysiol_dosing_sets <- function(x) length(.dosing_sets_elements(x))

#' @rdname dosing_sets
#' @export
names.compphysiol_dosing_sets <- function(x) names(.dosing_sets_elements(x))

#' @rdname dosing_sets
#' @export
`names<-.compphysiol_dosing_sets` <- function(x, value) {
    out <- .dosing_sets_elements(x)
    names(out) <- value
    .dosing_sets_validate_result(out)
}

#' @rdname dosing_sets
#' @export
`[.compphysiol_dosing_sets` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .check_unique_subset_index(i, "Dosing set")
    out <- .dosing_sets_elements(x)[i]
    if (any(vapply(out, is.null, logical(1)))) {
        stop("Cannot subset dosing sets with missing or unknown indices.", call. = FALSE)
    }
    .new_dosing_sets(out)
}

#' @rdname dosing_sets
#' @export
`[[.compphysiol_dosing_sets` <- function(x, i, ..., exact = TRUE) {
    if (length(i) != 1L) stop("Select a single dosing set with [[.", call. = FALSE)
    .dosing_sets_elements(x)[[i, exact = exact]]
}

#' @rdname dosing_sets
#' @export
`[<-.compphysiol_dosing_sets` <- function(x, i, ..., value) {
    .check_class(value, "compphysiol_dosing_sets")
    out <- .dosing_sets_elements(x)
    out[i] <- .dosing_sets_elements(value)
    .dosing_sets_validate_result(out)
}

#' @rdname dosing_sets
#' @export
`[[<-.compphysiol_dosing_sets` <- function(x, i, ..., value) {
    if (!is.null(value)) .check_class(value, "compphysiol_dosing")
    out <- .dosing_sets_elements(x)
    out[[i]] <- value
    .dosing_sets_validate_result(out)
}

#' @rdname dosing_sets
#' @export
c.compphysiol_dosing_sets <- function(...) {
    objects <- list(...)
    if (!all(vapply(objects, inherits, logical(1), "compphysiol_dosing_sets"))) {
        stop("All arguments must be dosing-set collections.", call. = FALSE)
    }
    .dosing_sets_validate_result(do.call(c, unname(lapply(objects, .dosing_sets_elements))) %||% list())
}

#' @rdname dosing_sets
#' @export
print.compphysiol_dosing_sets <- function(x, ...) {
    cat(" Dosing sets:", if (!length(x)) " (none)", "\n", sep = "")
    labels <- names(x)
    for (i in seq_along(x)) {
        label <- if (!is.null(labels) && nzchar(labels[[i]])) paste0(labels[[i]], ": ") else ""
        cat("  (", i, ") ", label, length(x[[i]]), " dosing events\n", sep = "")
    }
    invisible(x)
}
