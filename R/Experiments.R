#' Collect experiments
#'
#' Creates a list-like collection of independent individual simulation or
#' estimation units. Element names are individual identifiers; unnamed inputs
#' remain supported, while nonempty names must be unique. Positive whole-number
#' names are reserved for transient identifiers of unnamed experiments. Order is preserved.
#' Each element
#' retains its own parameters and units. Use `[` to obtain a collection and
#' `[[` to extract a single experiment.
#'
#' All experiments must use the same time unit mode (unit-free or unit-bearing).
#' compphysiol_parameters with the same name and dose amounts for the same molecule and
#' compartment must have compatible units, or all be unit-free. Convertible
#' units are accepted without changing the stored values. Infusions are compared
#' through their total amounts. Omitted dosing targets match other identically
#' omitted targets; resolving them against explicit targets requires a model
#' and is deferred to model preparation.
#'
#' @param ... `compphysiol_experiment` objects, optionally named with their individual
#'   identifiers. For a list of experiments, use `do.call(experiments, x)`.
#' @returns A `compphysiol_experiments` object.
#' @examples
#' x <- experiments(control = experiment(), treatment = experiment())
#' x
#' x["control"]
#' x[["treatment"]]
#' c(control = experiment(), treatment = experiment())
#' c(x, experiments(repeat_run = experiment()))
#' @export
experiments <- function(...) {
    x <- list(...)
    .experiments_check_elements(x)
    .check_optional_unique_names(x, "experiment")
    .collection_identifiers(x, "Experiment")
    .experiments_check_units(x)
    .new_experiments(x, .experiments_compute_component_homogeneity(x))
}

.new_experiments <- function(x, component_homogeneity = NULL) {
    structure(
        x,
        component_homogeneity = component_homogeneity,
        class = .compphysiol_class("compphysiol_experiments", base = "list")
    )
}

.experiments_elements <- function(x) {
    out <- unclass(x)
    attr(out, "component_homogeneity") <- NULL
    out
}

.experiments_component_names <- function() {
    c(
        "start", "parameter_schema", "parameters", "dosing",
        "observation_plan", "observations"
    )
}

.experiments_component_homogeneity <- function(x) {
    out <- attr(x, "component_homogeneity", exact = TRUE)
    if (is.null(out)) {
        out <- .experiments_compute_component_homogeneity(.experiments_elements(x))
    }
    out
}

.experiments_compute_component_homogeneity <- function(
    x, categories = .experiments_component_names()
) {
    out <- stats::setNames(rep(TRUE, length(categories)), categories)
    if (length(x) < 2L) return(out)

    reference <- x[[1]]
    for (category in categories) {
        for (i in seq.int(2L, length(x))) {
            if (!.experiments_components_identical(reference, x[[i]], category)) {
                out[[category]] <- FALSE
                break
            }
        }
    }
    out
}

.experiments_components_identical <- function(x, y, category) {
    switch(
        category,
        start = identical(x$start, y$start),
        parameter_schema = .parameters_schema_identical(x$parameters, y$parameters),
        parameters = identical(x$parameters, y$parameters),
        dosing = identical(x$dosing, y$dosing),
        observation_plan = identical(x$observations$time, y$observations$time) &&
            identical(x$observations$observable, y$observations$observable),
        observations = identical(x$observations, y$observations),
        stop("Unknown experiment component category: ", category, ".", call. = FALSE)
    )
}

.experiments_check_elements <- function(x) {
    if (!all(vapply(x, inherits, logical(1), "compphysiol_experiment"))) {
        stop("All elements must be experiment objects.", call. = FALSE)
    }
    invisible(NULL)
}

.experiments_check_units <- function(x) {
    parameter_values <- list()
    targets <- list()
    amounts <- list()
    for (i in seq_along(x)) {
        e <- x[[i]]
        context <- paste0(" in experiment ", i)
        .check_compatible_units(x[[1]]$start, e$start, paste0("time", context))
        for (nm in names(e$parameters)) {
            value <- e$parameters[[nm]]
            if (nm %in% names(parameter_values)) {
                .check_compatible_units(parameter_values[[nm]], value,
                    paste0("parameter '", nm, "'", context))
            } else {
                parameter_values[nm] <- list(value)
            }
        }
        for (j in seq_along(e$dosing$time)) {
            target <- c(e$dosing$molec[j], e$dosing$cmt[j])
            match <- which(vapply(targets, identical, logical(1), target))
            value <- e$dosing$amount[[j]]
            if (length(match)) {
                label <- ifelse(is.na(target), "<unspecified>", target)
                .check_compatible_units(amounts[[match]], value,
                    paste0("dosing target '", label[1], " in ", label[2], "'", context))
            } else {
                targets[[length(targets) + 1L]] <- target
                amounts[length(amounts) + 1L] <- list(value)
            }
        }
    }
    invisible(NULL)
}

.check_compatible_units <- function(reference, value, label) {
    reference_has_units <- inherits(reference, "units")
    value_has_units <- inherits(value, "units")
    compatible <- identical(reference_has_units, value_has_units)
    if (compatible && reference_has_units) {
        compatible <- units::ud_are_convertible(
            units::deparse_unit(reference), units::deparse_unit(value)
        )
    }
    if (!compatible) {
        stop("Inconsistent units for ", label,
             ": values must both be unit-free or have convertible units.", call. = FALSE)
    }
    invisible(NULL)
}

#' Subset a collection of experiments
#'
#' Missing or unknown indices that would introduce missing elements are rejected.
#' @param x A `compphysiol_experiments` object.
#' @param i Numeric, logical, or character indices.
#' @param ... Unused.
#' @returns A `compphysiol_experiments` object.
#' @export
`[.compphysiol_experiments` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .check_unique_subset_index(i, "Experiment")
    out <- .experiments_elements(x)[i]
    if (any(vapply(out, is.null, logical(1)))) {
        stop("Cannot subset experiments with missing or unknown indices.", call. = FALSE)
    }
    .new_experiments(out, .experiments_compute_component_homogeneity(out))
}

#' Extract an experiment
#' @param x A `compphysiol_experiments` object.
#' @param i Index or name of a single experiment.
#' @param ... Unused.
#' @param exact Whether character indices must match exactly, as for lists.
#' @returns A `compphysiol_experiment` object; unknown names return `NULL`, as for lists.
#' @export
`[[.compphysiol_experiments` <- function(x, i, ..., exact = TRUE) {
    if (length(i) != 1L) stop("Select a single experiment with [[.", call. = FALSE)
    .experiments_elements(x)[[i, exact = exact]]
}

#' Replace experiments in a collection
#'
#' Replacement re-establishes collection validity and refreshes component
#' homogeneity metadata.
#' @param x A `compphysiol_experiments` collection.
#' @param i Replacement index.
#' @param ... Unused.
#' @param value For `[[<-`, a `compphysiol_experiment` or `NULL`; for `[<-`, a
#'   `compphysiol_experiments` collection.
#' @returns The modified `compphysiol_experiments` collection.
#' @export
`[[<-.compphysiol_experiments` <- function(x, i, ..., value) {
    if (!is.null(value)) .check_class(value, "compphysiol_experiment")
    out <- .experiments_elements(x)
    out[[i]] <- value
    .experiments_validate_result(out)
}

#' @rdname sub-subset-.compphysiol_experiments
#' @export
`[<-.compphysiol_experiments` <- function(x, i, ..., value) {
    .check_class(value, "compphysiol_experiments")
    out <- .experiments_elements(x)
    out[i] <- .experiments_elements(value)
    .experiments_validate_result(out)
}

.experiments_validate_result <- function(x) {
    .experiments_check_elements(x)
    .check_optional_unique_names(x, "experiment")
    .collection_identifiers(x, "Experiment")
    .experiments_check_units(x)
    .new_experiments(x, .experiments_compute_component_homogeneity(x))
}

#' Combine experiments
#'
#' Individual `compphysiol_experiment` objects and existing
#' `compphysiol_experiments` collections can be combined. The result is checked
#' as a new collection, preserving element order and names.
#'
#' @param ... `compphysiol_experiment` or `compphysiol_experiments` objects.
#' @param recursive Unused.
#' @returns A `compphysiol_experiments` object, preserving order and names.
#' @name combine_experiments
NULL

#' @rdname combine_experiments
#' @export
c.compphysiol_experiment <- function(..., recursive = FALSE) {
    .combine_experiments(list(...))
}

#' @rdname combine_experiments
#' @export
c.compphysiol_experiments <- function(..., recursive = FALSE) {
    objects <- list(...)
    .combine_experiments(objects)
}

.combine_experiments <- function(objects) {
    object_names <- names(objects)
    if (is.null(object_names)) object_names <- rep("", length(objects))
    parts <- lapply(seq_along(objects), function(i) {
        object <- objects[[i]]
        if (inherits(object, "compphysiol_experiment")) {
            return(setNames(list(object), object_names[[i]]))
        }
        if (inherits(object, "compphysiol_experiments")) {
            return(.experiments_elements(object))
        }
        stop(
            "c() can only combine experiment and experiments objects.",
            call. = FALSE
        )
    })
    combined <- do.call(c, unname(parts))
    if (is.null(combined)) combined <- list()
    .experiments_validate_result(combined)
}

#' Collect observations from experiments
#'
#' Row-binds the observation tables from an experiment collection and adds an
#' `experiment` factor identifying their source. Explicit experiment names are
#' used as identifiers; unnamed experiments use their one-based collection
#' positions (`"1"`, `"2"`, and so on). Factor levels preserve collection
#' order.
#'
#' Every experiment must contain the same kind of observations: either all
#' observation data or all observation schedules. Observation tables must have
#' identical columns, and `experiment` is reserved for the added identifier.
#'
#' @param x A `compphysiol_experiments` collection.
#' @returns A `compphysiol_observation_data` or
#'   `compphysiol_observation_schedule` object with an `experiment` column.
#' @examples
#' studies <- experiments(
#'     treated = experiment(observations = observation_data(1, "C", 2)),
#'     experiment(observations = observation_data(1, "C", 3))
#' )
#' observations(studies)
#' @export
observations <- function(x) {
    .check_class(x, "compphysiol_experiments")
    identifiers <- .collection_identifiers(x, "Experiment")
    if (!length(x)) {
        out <- observation_schedule()
        out$experiment <- factor(character())
        return(.new_observation_schedule(out))
    }

    tables <- lapply(x, `[[`, "observations")
    data <- vapply(tables, inherits, logical(1), "compphysiol_observation_data")
    if (!all(data) && any(data)) {
        stop(
            "Cannot collect a mixture of observation schedules and observation data.",
            call. = FALSE
        )
    }
    if (any(vapply(tables, function(table) "experiment" %in% names(table), logical(1)))) {
        stop(
            "Observation column 'experiment' is reserved for collection identifiers.",
            call. = FALSE
        )
    }
    columns <- lapply(tables, names)
    if (!all(vapply(columns, identical, logical(1), columns[[1]]))) {
        stop("Observation tables must have identical columns to be collected.", call. = FALSE)
    }
    for (i in seq_along(tables)) {
        tables[[i]]$experiment <- factor(
            rep(identifiers[[i]], nrow(tables[[i]])), levels = identifiers
        )
    }
    out <- do.call(rbind, lapply(tables, as.data.frame))
    rownames(out) <- NULL
    if (all(data)) .new_observation_data(out) else .new_observation_schedule(out)
}

#' Print a collection of experiments
#' @param x A `compphysiol_experiments` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.compphysiol_experiments <- function(x, ...) {
    if (!length(x)) {
        cat(" Experiments: (none)\n")
        return(invisible(x))
    }
    homogeneity <- .experiments_component_homogeneity(x)
    labels <- names(x)
    if (is.null(labels)) labels <- rep("", length(x))
    named <- !is.na(labels) & nzchar(labels)
    all_shared <- length(x) > 1L && all(homogeneity[c(
        "start", "parameters", "dosing", "observations"
    )])
    collapsed <- all_shared && !any(named)
    cat(
        " Experiments:",
        if (collapsed) paste0(" ", length(x), " unnamed") else "",
        "\n", sep = ""
    )

    if (length(x) > 1L) {
        shared <- .experiments_print_shared(x[[1]], homogeneity)
        if (length(shared)) cat("  shared: ", paste(shared, collapse = "; "), "\n", sep = "")
        if (collapsed) return(invisible(x))
    }

    for (i in seq_along(x)) {
        e <- x[[i]]
        label <- if (named[[i]]) labels[[i]] else ""
        details <- .experiments_print_individual(
            e, if (length(x) > 1L) homogeneity else NULL
        )
        text <- c(label, details)
        text <- text[nzchar(text)]
        cat("  (", i, ")", if (length(text)) paste0(" ", paste(text, collapse = "; ")) else "",
            "\n", sep = "")
    }
    invisible(x)
}

.experiments_print_shared <- function(experiment, homogeneity) {
    out <- character()
    if (homogeneity[["start"]]) {
        out <- c(out, paste0("start = ", format(experiment$start)))
    }
    if (homogeneity[["parameters"]] && length(experiment$parameters)) {
        out <- c(out, .experiments_print_count(length(experiment$parameters), "parameter"))
    } else if (homogeneity[["parameter_schema"]] && length(experiment$parameters)) {
        out <- c(out, paste0(
            "parameter schema (",
            .experiments_print_count(length(experiment$parameters), "parameter"), ")"
        ))
    }
    if (homogeneity[["dosing"]] && length(experiment$dosing)) {
        out <- c(out, .experiments_print_count(length(experiment$dosing), "dosing event"))
    }
    observations <- nrow(experiment$observations)
    if (homogeneity[["observations"]] && observations) {
        out <- c(out, .experiments_print_observations(experiment$observations, count = TRUE))
    } else if (homogeneity[["observation_plan"]] && observations) {
        out <- c(out, paste0(
            "observation plan (", .experiments_print_count(observations, "row"), ")"
        ))
    }
    out
}

.experiments_print_individual <- function(experiment, homogeneity = NULL) {
    shared <- function(component) !is.null(homogeneity) && homogeneity[[component]]
    out <- character()
    if (!shared("start")) out <- c(out, paste0("start = ", format(experiment$start)))
    if (!shared("parameters") && length(experiment$parameters)) {
        out <- c(out, .experiments_print_parameter_preview(experiment$parameters))
    }
    if (!shared("dosing") && length(experiment$dosing)) {
        out <- c(out, .experiments_print_count(length(experiment$dosing), "dosing event"))
    }
    if (!shared("observations") && nrow(experiment$observations)) {
        out <- c(out, .experiments_print_observation_preview(experiment$observations))
    }
    out
}

.experiments_print_parameter_preview <- function(x, max = 2L) {
    shown <- seq_len(min(length(x), max))
    out <- vapply(shown, function(i) {
        paste0(names(x)[[i]], " = ", .experiments_print_value(x[[i]]))
    }, character(1))
    omitted <- length(x) - length(shown)
    if (omitted) {
        out <- c(out, paste0("... ", .experiments_print_count(omitted, "more parameter")))
    }
    paste(out, collapse = ", ")
}

.experiments_print_observation_preview <- function(x) {
    key <- paste0(
        x$observable[[1]], "(t = ", .experiments_print_value(x$time[[1]]), ")"
    )
    out <- if (inherits(x, "compphysiol_observation_data")) {
        paste0(key, " = ", .experiments_print_value(x$value[[1]]))
    } else key
    omitted <- nrow(x) - 1L
    if (omitted) {
        out <- c(out, paste0("... ", .experiments_print_count(omitted, "more observation")))
    }
    paste(out, collapse = ", ")
}

.experiments_print_value <- function(x, max_chars = 40L) {
    out <- paste(format(x, trim = TRUE), collapse = ", ")
    if (nchar(out, type = "width") > max_chars) {
        out <- paste0(substr(out, 1L, max_chars - 3L), "...")
    }
    out
}

.experiments_print_count <- function(n, label) {
    paste(n, paste0(label, if (n == 1L) "" else "s"))
}

.experiments_print_observations <- function(x, count) {
    kind <- if (inherits(x, "compphysiol_observation_data")) "with data" else "scheduled"
    if (count) {
        paste(.experiments_print_count(nrow(x), "observation"), kind)
    } else {
        paste("observations", kind)
    }
}
