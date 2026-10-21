#' Update experiments
#'
#' Replaces or updates experiment components, with scalar components broadcast
#' across collections and collection-valued components aligned by experiment.
#' A function supplied for a component is called on the current component and
#' must return its replacement; callbacks do not require a `mode`.
#'
#' `mode = "replace"` installs complete incoming components. `mode = "update"`
#' overlays parameters by name, appends dosing events or observation rows, and
#' assigns `start`. Incoming parameter values take precedence on name conflicts.
#'
#' A single mode applies to every object-valued component. A named mode vector
#' can select behavior by component; every object-valued component must be
#' covered. The default `NULL` is accepted only when all supplied components are
#' callbacks (or no components are supplied).
#'
#' @param object A [compphysiol_experiment][experiment()] or
#'   [compphysiol_experiments][experiments()] object.
#' @param parameters A [compphysiol_parameters][parameters()] object,
#'   [compphysiol_parameter_sets][parameter_sets()] collection, `NULL`, or a
#'   callback receiving the current parameters.
#' @param dosing A [compphysiol_dosing][dosing()] object, a list of dosing
#'   objects, `NULL`, or a callback receiving the current dosing schedule.
#' @param observations An observation schedule or data object, a bound object
#'   with an `experiment` column, a list of observation objects, `NULL`, or a
#'   callback receiving the current observations.
#' @param start A finite numeric scalar, a list of such scalars, `NULL`, or a
#'   callback receiving the current start. Unit shorthand is supported.
#' @param ... Unused.
#' @param mode `NULL`, or `"replace"`/`"update"` as an unnamed scalar or a
#'   named character vector selecting a mode for each object-valued component.
#' @returns An experiment when `object` and all supplied components are scalar;
#'   otherwise an experiment collection.
#' @examples
#' protocol <- experiment(parameters = parameters(route = "oral"))
#' population <- parameter_sets(
#'     first = parameters(BW = 65 [kg]),
#'     second = parameters(BW = 80 [kg])
#' )
#' update(protocol, parameters = population, mode = "update")
#'
#' studies <- experiments(
#'     experiment(parameters = parameters(BW = 65 [kg], V = 5 [L])),
#'     experiment(parameters = parameters(BW = 80 [kg], V = 6 [L]))
#' )
#' update(studies, parameters = function(p) p[setdiff(names(p), "V")])
#' @method update compphysiol_experiment
#' @export
update.compphysiol_experiment <- function(
    object, parameters, dosing, observations, start, ..., mode = NULL
) {
    .update_experiments(
        object,
        parameters = if (missing(parameters)) quote(expr = ) else substitute(parameters),
        dosing = if (missing(dosing)) quote(expr = ) else substitute(dosing),
        observations = if (missing(observations)) quote(expr = ) else substitute(observations),
        start = if (missing(start)) quote(expr = ) else substitute(start),
        mode = mode,
        envir = parent.frame()
    )
}

#' @rdname update.compphysiol_experiment
#' @method update compphysiol_experiments
#' @export
update.compphysiol_experiments <- function(
    object, parameters, dosing, observations, start, ..., mode = NULL
) {
    .update_experiments(
        object,
        parameters = if (missing(parameters)) quote(expr = ) else substitute(parameters),
        dosing = if (missing(dosing)) quote(expr = ) else substitute(dosing),
        observations = if (missing(observations)) quote(expr = ) else substitute(observations),
        start = if (missing(start)) quote(expr = ) else substitute(start),
        mode = mode,
        envir = parent.frame()
    )
}

.update_experiments <- function(
    object, parameters, dosing, observations, start, mode, envir
) {
    .check_class(object, c("compphysiol_experiment", "compphysiol_experiments"))
    expressions <- list(
        parameters = parameters, dosing = dosing,
        observations = observations, start = start
    )
    supplied <- !vapply(expressions, .update_missing_expression, logical(1))
    if (!any(supplied)) {
        .update_experiments_check_mode(mode, character(), character())
        return(object)
    }

    inputs <- lapply(names(expressions)[supplied], function(component) {
        is_inline_function <- is.call(expressions[[component]]) &&
            identical(expressions[[component]][[1L]], quote(`function`))
        value <- if (component == "start" && !is_inline_function) {
            .process_nse_arg(expressions[[component]], envir = envir)
        } else {
            eval(expressions[[component]], envir = envir)
        }
        .update_experiments_input(value, component)
    })
    names(inputs) <- names(expressions)[supplied]
    callbacks <- names(inputs)[vapply(inputs, `[[`, logical(1), "callback")]
    object_components <- setdiff(names(inputs), callbacks)
    modes <- .update_experiments_check_mode(mode, object_components, callbacks)

    object_is_collection <- inherits(object, "compphysiol_experiments")
    base <- if (object_is_collection) .experiments_elements(object) else list(object)
    output_names <- if (object_is_collection) names(base) else NULL
    target_ids <- if (object_is_collection) {
        .collection_identifiers(base, "Experiment")
    } else {
        NULL
    }

    collection_inputs <- inputs[vapply(inputs, `[[`, logical(1), "collection")]
    if (!object_is_collection && length(collection_inputs)) {
        driver <- collection_inputs[[1L]]
        target_ids <- driver$identifiers
        output_names <- driver$names
        base <- rep(base, length(driver$values))
    }
    if (is.null(target_ids)) target_ids <- "1"

    for (component in names(collection_inputs)) {
        input <- collection_inputs[[component]]
        if (length(input$values) != length(base)) {
            stop("Collection-valued '", component, "' must match the output length.",
                 call. = FALSE)
        }
        unnamed <- is.null(input$names) || all(is.na(input$names) | !nzchar(input$names))
        if (unnamed) {
            inputs[[component]]$values <- input$values
        } else {
            positions <- match(target_ids, input$identifiers)
            if (anyNA(positions) || anyDuplicated(positions)) {
                stop("Collection-valued '", component,
                     "' must have the same experiment identifiers.", call. = FALSE)
            }
            inputs[[component]]$values <- input$values[positions]
        }
    }

    out <- vector("list", length(base))
    for (i in seq_along(base)) {
        current <- base[[i]]
        values <- list(
            parameters = current$parameters,
            dosing = current$dosing,
            observations = current$observations,
            start = current$start
        )
        for (component in names(inputs)) {
            input <- inputs[[component]]
            if (input$callback) {
                values[[component]] <- input$value(values[[component]])
            } else {
                incoming <- if (input$collection) input$values[[i]] else input$value
                values[[component]] <- .update_experiment_component(
                    values[[component]], incoming, component, modes[[component]]
                )
            }
        }
        out[[i]] <- experiment(
            parameters = values$parameters,
            dosing = values$dosing,
            observations = values$observations,
            start = values$start
        )
    }

    if (!object_is_collection && !length(collection_inputs)) return(out[[1L]])
    names(out) <- output_names
    .experiments_validate_result(out)
}

.update_missing_expression <- function(x) {
    is.symbol(x) && identical(as.character(x), "")
}

.update_experiments_input <- function(value, component) {
    if (is.function(value)) {
        return(list(callback = TRUE, collection = FALSE, value = value))
    }
    if (is.null(value)) {
        value <- switch(component,
            parameters = parameters(),
            dosing = dosing(),
            observations = observation_schedule(),
            start = NULL
        )
    }

    scalar_class <- switch(component,
        parameters = "compphysiol_parameters",
        dosing = "compphysiol_dosing",
        observations = "compphysiol_observation_schedule",
        start = NULL
    )
    if (!is.null(scalar_class) && inherits(value, scalar_class)) {
        if (component == "observations" && "experiment" %in% names(value)) {
            return(.update_experiments_observation_input(value))
        }
        return(list(callback = FALSE, collection = FALSE, value = value))
    }
    if (component == "start" && (is.null(value) ||
        (is.numeric(value) && length(value) == 1L && is.finite(value)))) {
        return(list(callback = FALSE, collection = FALSE, value = value))
    }
    if (component == "parameters" && inherits(value, "compphysiol_parameter_sets")) {
        elements <- .parameter_sets_elements(value)
        return(.update_experiments_collection_input(elements, "Parameter set"))
    }
    if (is.list(value) && component %in% c("dosing", "observations", "start")) {
        expected <- switch(component,
            dosing = "compphysiol_dosing",
            observations = "compphysiol_observation_schedule",
            start = NULL
        )
        valid <- if (component == "start") {
            vapply(value, function(x) is.null(x) ||
                (is.numeric(x) && length(x) == 1L && is.finite(x)), logical(1))
        } else {
            vapply(value, inherits, logical(1), expected)
        }
        if (all(valid)) return(.update_experiments_collection_input(value, component))
    }
    stop("Invalid '", component, "' update input.", call. = FALSE)
}

.update_experiments_collection_input <- function(values, label) {
    list(
        callback = FALSE,
        collection = TRUE,
        values = values,
        names = names(values),
        identifiers = .collection_identifiers(values, label)
    )
}

.update_experiments_observation_input <- function(value) {
    identifiers <- if (is.factor(value$experiment)) {
        levels(value$experiment)
    } else {
        unique(as.character(value$experiment))
    }
    if (anyNA(value$experiment) || any(!nzchar(as.character(value$experiment)))) {
        stop("Observation experiment identifiers must be non-missing and nonempty.",
             call. = FALSE)
    }
    tables <- lapply(identifiers, function(identifier) {
        out <- value[as.character(value$experiment) == identifier, , drop = FALSE]
        out$experiment <- NULL
        rownames(out) <- NULL
        if (inherits(value, "compphysiol_observation_data")) {
            .new_observation_data(out)
        } else {
            .new_observation_schedule(out)
        }
    })
    raw_names <- identifiers
    positional <- identifiers == as.character(seq_along(identifiers))
    raw_names[positional] <- ""
    names(tables) <- raw_names
    input <- .update_experiments_collection_input(tables, "Observation")
    input$identifiers <- identifiers
    input
}

.update_experiments_check_mode <- function(mode, object_components, callbacks) {
    allowed_components <- c("parameters", "dosing", "observations", "start")
    if (is.null(mode)) {
        if (length(object_components)) {
            stop("mode must be specified for object-valued updates: ",
                 paste(object_components, collapse = ", "), ".", call. = FALSE)
        }
        return(stats::setNames(character(), character()))
    }
    if (!is.character(mode) || !length(mode) || anyNA(mode) ||
        any(!mode %in% c("replace", "update"))) {
        stop("mode must contain only 'replace' or 'update'.", call. = FALSE)
    }
    mode_names <- names(mode)
    if (length(mode) == 1L && (is.null(mode_names) || !nzchar(mode_names))) {
        return(stats::setNames(rep(mode, length(object_components)), object_components))
    }
    if (is.null(mode_names) || anyNA(mode_names) || any(!nzchar(mode_names)) ||
        anyDuplicated(mode_names)) {
        stop("A non-scalar mode must be uniquely named by component.", call. = FALSE)
    }
    unknown <- setdiff(mode_names, allowed_components)
    if (length(unknown)) {
        stop("Unknown mode component(s): ", paste(unknown, collapse = ", "), ".",
             call. = FALSE)
    }
    missing <- setdiff(object_components, mode_names)
    if (length(missing)) {
        stop("mode is missing for object-valued update(s): ",
             paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    mode[object_components]
}

.update_experiment_component <- function(current, incoming, component, mode) {
    if (identical(mode, "replace") || identical(component, "start")) return(incoming)
    switch(component,
        parameters = .update_parameters_overlay(current, incoming),
        dosing = c(current, incoming),
        observations = .update_observations_append(current, incoming),
        stop("Unknown experiment component: ", component, ".", call. = FALSE)
    )
}

.update_parameters_overlay <- function(current, incoming) {
    .check_class(incoming, "compphysiol_parameters")
    out <- unclass(current)
    for (name in names(incoming)) out[[name]] <- incoming[[name]]
    .new_parameters(out)
}

.update_observations_append <- function(current, incoming) {
    .check_class(incoming, "compphysiol_observation_schedule")
    if (!nrow(current)) return(incoming)
    if (!nrow(incoming)) return(current)
    current_data <- inherits(current, "compphysiol_observation_data")
    incoming_data <- inherits(incoming, "compphysiol_observation_data")
    if (!identical(current_data, incoming_data)) {
        stop("Cannot update observations by combining a schedule with data.",
             call. = FALSE)
    }
    if (!identical(names(current), names(incoming))) {
        stop("Observation columns must be identical when mode = 'update'.",
             call. = FALSE)
    }
    out <- rbind(as.data.frame(current), as.data.frame(incoming))
    rownames(out) <- NULL
    if (current_data) .new_observation_data(out) else .new_observation_schedule(out)
}
