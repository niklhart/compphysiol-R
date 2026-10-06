.statistical_parameter_name <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
        stop(label, " must name a statistical parameter.", call. = FALSE)
    }
    x
}

.statistical_level <- function(level) {
    if (is.null(level)) return(NULL)
    if (!is.character(level) || length(level) != 1L || is.na(level) ||
        !level %in% c("individual", "observation")) {
        stop("level must be NULL, 'individual', or 'observation'.", call. = FALSE)
    }
    level
}

#' Standard-deviation specifications
#'
#' These constructors describe how the standard deviation of a normal
#' distribution depends on its resolved mean. A proportional standard deviation
#' is `coefficient * abs(mean)`. The combined convention is initially
#' `constant + proportional * abs(mean)`.
#'
#' @param coefficient Name of a dimensionless statistical parameter.
#' @param constant Name of a statistical parameter with the target's units.
#' @param proportional Name of a dimensionless statistical parameter.
#' @returns A standard-deviation specification for use in [normal()].
#' @name sd_specifications
NULL

#' @rdname sd_specifications
#' @export
proportional <- function(coefficient) {
    coefficient <- .statistical_parameter_name(coefficient, "coefficient")
    structure(list(coefficient = coefficient), class = "ProportionalSD")
}

#' @rdname sd_specifications
#' @export
combined <- function(constant, proportional) {
    constant <- .statistical_parameter_name(constant, "constant")
    proportional <- .statistical_parameter_name(proportional, "proportional")
    if (identical(constant, proportional)) {
        stop("Combined constant and proportional parameters must be distinct.", call. = FALSE)
    }
    structure(
        list(constant = constant, proportional = proportional),
        class = "CombinedSD"
    )
}

#' Statistical distribution specifications
#'
#' `normal()` uses a mean and a standard-deviation specification. A bare name in
#' `sd` denotes a constant standard deviation; [proportional()] and [combined()]
#' describe mean-dependent standard deviations.
#'
#' `lognormal()` uses the median (equivalently, the geometric mean) as its
#' location. It intentionally does not accept arithmetic-mean or raw log-mean
#' parameterizations. `sdlog` names a dimensionless log-scale standard
#' deviation.
#'
#' Missing locations are resolved by [validate_statistical_model()]: an
#' observable uses its model prediction, while a structural parameter requires
#' an explicit location.
#'
#' @param mean Name of the normal location parameter, or `NULL` for an
#'   observable prediction default.
#' @param sd Name of a constant standard-deviation parameter, or a specification
#'   from [proportional()] or [combined()].
#' @param median Name of the log-normal median parameter, or `NULL` for an
#'   observable prediction default.
#' @param sdlog Name of a dimensionless log-scale standard-deviation parameter.
#' @param level `NULL`, `"individual"`, or `"observation"`. `NULL` is inferred
#'   from the target when validated against a dynamic model.
#' @returns A `StatisticalDistribution` object.
#' @name statistical_distributions
NULL

#' @rdname statistical_distributions
#' @export
normal <- function(mean = NULL, sd, level = NULL) {
    if (!is.null(mean)) mean <- .statistical_parameter_name(mean, "mean")
    if (missing(sd)) stop("sd must be supplied.", call. = FALSE)
    if (is.character(sd)) {
        sd <- .statistical_parameter_name(sd, "sd")
    } else if (!inherits(sd, c("ProportionalSD", "CombinedSD"))) {
        stop("sd must name a statistical parameter or use proportional() or combined().",
             call. = FALSE)
    }
    structure(
        list(mean = mean, sd = sd, level = .statistical_level(level)),
        class = c("NormalDistribution", "StatisticalDistribution")
    )
}

#' @rdname statistical_distributions
#' @export
lognormal <- function(median = NULL, sdlog, level = NULL) {
    if (!is.null(median)) median <- .statistical_parameter_name(median, "median")
    if (missing(sdlog)) stop("sdlog must be supplied.", call. = FALSE)
    sdlog <- .statistical_parameter_name(sdlog, "sdlog")
    structure(
        list(median = median, sdlog = sdlog, level = .statistical_level(level)),
        class = c("LognormalDistribution", "StatisticalDistribution")
    )
}

#' Construct a statistical model
#'
#' The left-hand-side name identifies the random quantity. Targets are resolved
#' as unresolved structural parameters or model observables when the statistical
#' model is passed to [validate_statistical_model()].
#'
#' @param ... Named distribution specifications created by [normal()] or
#'   [lognormal()].
#' @returns A `StatisticalModel` object.
#' @examples
#' model <- statistical_model(
#'     CL = normal(mean = "CL_pop", sd = proportional("omega_CL")),
#'     C = lognormal(sdlog = "sigma")
#' )
#' @export
statistical_model <- function(...) {
    x <- list(...)
    nm <- names(x)
    if (length(x) && (is.null(nm) || anyNA(nm) || any(!nzchar(nm)))) {
        stop("Statistical distributions must be named by their random quantity.", call. = FALSE)
    }
    if (anyDuplicated(nm)) {
        stop("Statistical-model target names must be unique.", call. = FALSE)
    }
    if (!all(vapply(x, inherits, logical(1), "StatisticalDistribution"))) {
        stop("Every statistical-model entry must be a StatisticalDistribution object.",
             call. = FALSE)
    }
    structure(x, class = c("StatisticalModel", "list"))
}

#' Subset a statistical model
#'
#' @param x A `StatisticalModel` object.
#' @param i Indices or names of random quantities to retain.
#' @param ... Unused.
#' @returns A `StatisticalModel` object.
#' @export
`[.StatisticalModel` <- function(x, i, ...) {
    structure(unclass(x)[i], class = c("StatisticalModel", "list"))
}

#' Validate and resolve a statistical model
#'
#' Missing levels are inferred from the dynamic model. Unresolved/free
#' structural parameters resolve to `"individual"`; observables resolve to
#' `"observation"`. A target matching both requires an explicit level, and an
#' unknown target is rejected. Fixed structural parameters cannot be
#' individual-level targets.
#'
#' Missing locations resolve to the dynamic-model prediction only for
#' observation-level distributions. Individual-level distributions require an
#' explicit `mean` or `median`.
#'
#' If `parameters` is supplied, referenced statistical parameters are checked
#' for existence, scalar numeric values, and compatible units. The values must
#' also supply any dynamic-model free parameters not represented by
#' individual-level distributions so observable and structural target units can
#' be evaluated.
#'
#' @param x A [StatisticalModel][statistical_model()].
#' @param model A `CompartmentModel`, `ProcessModel`, `OdeModel`, or
#'   `CompiledOdeModel`.
#' @param parameters Optional [Parameters][parameters()] object containing
#'   realized statistical-parameter values and any otherwise unresolved dynamic
#'   parameters needed for unit evaluation.
#' @returns A resolved `StatisticalModel` object with inferred levels and
#'   observation-prediction locations.
#' @export
validate_statistical_model <- function(x, model, parameters = NULL) {
    .check_class(x, "StatisticalModel")
    ode_model <- .statistical_ode_model(model)
    structural <- unique(c(names(ode_model$parameters), ode_model$freeParams))
    free <- ode_model$freeParams
    observables <- names(ode_model$observables)

    resolved <- lapply(seq_along(x), function(i) {
        target <- names(x)[[i]]
        distribution <- x[[i]]
        is_structural <- target %in% structural
        is_observable <- target %in% observables
        if (!is_structural && !is_observable) {
            stop("Unknown statistical-model target: ", target, ".", call. = FALSE)
        }
        level <- distribution$level
        if (is.null(level)) {
            if (is_structural && is_observable) {
                stop("Statistical-model target '", target,
                     "' is both a structural parameter and an observable; supply level explicitly.",
                     call. = FALSE)
            }
            level <- if (is_structural) "individual" else "observation"
        }
        if (identical(level, "individual") && !is_structural) {
            stop("Individual-level target '", target,
                 "' is not a structural parameter.", call. = FALSE)
        }
        if (identical(level, "observation") && !is_observable) {
            stop("Observation-level target '", target, "' is not an observable.", call. = FALSE)
        }
        if (identical(level, "individual") && !target %in% free) {
            stop("Individual-level target '", target,
                 "' must be an unresolved/free structural parameter.", call. = FALSE)
        }
        distribution$level <- level
        location <- if (inherits(distribution, "NormalDistribution")) "mean" else "median"
        if (is.null(distribution[[location]])) {
            if (identical(level, "individual")) {
                stop("Individual-level ", class(distribution)[[1]], " target '", target,
                     "' requires an explicit ", location, ".", call. = FALSE)
            }
            distribution[[location]] <- structure(list(), class = "PredictionLocation")
        }
        distribution
    })
    names(resolved) <- names(x)
    resolved <- structure(resolved, class = c("StatisticalModel", "list"))

    if (!is.null(parameters)) {
        .check_class(parameters, "Parameters")
        .statistical_validate_units(resolved, ode_model, parameters)
    }
    resolved
}

.statistical_ode_model <- function(model) {
    if (inherits(model, "CompiledOdeModel")) return(model$ode_model)
    if (inherits(model, "OdeModel")) return(model)
    if (inherits(model, "ProcessModel") || inherits(model, "CompartmentModel")) {
        return(to_ode_model(model))
    }
    stop("Unsupported dynamic model for statistical validation: ",
         class(model)[[1]] %||% typeof(model), ".", call. = FALSE)
}

.statistical_parameter_value <- function(parameters, name) {
    value <- parameters[[name]]
    if (is.null(value)) stop("Missing statistical parameter: ", name, ".", call. = FALSE)
    if (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L ||
        is.na(value) || !is.finite(value)) {
        stop("Statistical parameter '", name, "' must be a finite numeric scalar.",
             call. = FALSE)
    }
    value
}

.statistical_require_dimensionless <- function(value, name) {
    if (inherits(value, "units")) {
        tryCatch(
            units::set_units(value, "1", mode = "standard"),
            error = function(e) stop("Statistical parameter '", name,
                "' must be dimensionless.", call. = FALSE)
        )
    }
    invisible(NULL)
}

.statistical_require_target_units <- function(value, target_value, name, target) {
    tryCatch(
        .check_compatible_units(target_value, value,
            paste0("statistical parameter '", name, "' for target '", target, "'")),
        error = function(e) stop(conditionMessage(e), call. = FALSE)
    )
}

.statistical_validate_units <- function(x, model, parameters) {
    runtime <- unclass(parameters)
    for (target in names(x)) {
        distribution <- x[[target]]
        if (!identical(distribution$level, "individual")) next
        location_name <- if (inherits(distribution, "NormalDistribution")) {
            distribution$mean
        } else {
            distribution$median
        }
        runtime[[target]] <- .statistical_parameter_value(parameters, location_name)
    }
    runtime <- structure(runtime, class = c("Parameters", "list"))
    missing <- setdiff(model$freeParams, names(runtime))
    if (length(missing)) {
        stop("Cannot validate statistical-model units; missing dynamic-model free parameter(s): ",
             paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    merged <- .merge_ode_parameters(model$parameters, runtime)
    .ode_model_check_unit_consistency(model, merged)
    observable_values <- .ode_model_observable_unit_values(model, merged)
    names(observable_values) <- names(model$observables)

    for (target in names(x)) {
        distribution <- x[[target]]
        target_value <- if (identical(distribution$level, "individual")) {
            runtime[[target]]
        } else {
            observable_values[[target]]
        }
        if (inherits(distribution, "NormalDistribution")) {
            if (!inherits(distribution$mean, "PredictionLocation")) {
                mean_value <- .statistical_parameter_value(parameters, distribution$mean)
                .statistical_require_target_units(mean_value, target_value,
                                                  distribution$mean, target)
            }
            if (is.character(distribution$sd)) {
                sd_value <- .statistical_parameter_value(parameters, distribution$sd)
                .statistical_require_target_units(sd_value, target_value,
                                                  distribution$sd, target)
            } else if (inherits(distribution$sd, "ProportionalSD")) {
                name <- distribution$sd$coefficient
                .statistical_require_dimensionless(
                    .statistical_parameter_value(parameters, name), name
                )
            } else {
                constant <- distribution$sd$constant
                proportional_name <- distribution$sd$proportional
                .statistical_require_target_units(
                    .statistical_parameter_value(parameters, constant), target_value,
                    constant, target
                )
                .statistical_require_dimensionless(
                    .statistical_parameter_value(parameters, proportional_name),
                    proportional_name
                )
            }
        } else {
            if (!inherits(distribution$median, "PredictionLocation")) {
                median_value <- .statistical_parameter_value(parameters, distribution$median)
                .statistical_require_target_units(median_value, target_value,
                                                  distribution$median, target)
            }
            .statistical_require_dimensionless(
                .statistical_parameter_value(parameters, distribution$sdlog),
                distribution$sdlog
            )
        }
    }
    invisible(NULL)
}

#' Print a statistical model
#'
#' @param x A `StatisticalModel` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.StatisticalModel <- function(x, ...) {
    if (!length(x)) {
        cat(" Statistical model: (none)\n")
        return(invisible(x))
    }
    cat(" Statistical model:\n")
    for (i in seq_along(x)) {
        d <- x[[i]]
        family <- if (inherits(d, "NormalDistribution")) "normal" else "lognormal"
        location <- if (inherits(d, "NormalDistribution")) d$mean else d$median
        location <- if (inherits(location, "PredictionLocation")) "prediction" else {
            location %||% "<unresolved>"
        }
        level <- d$level %||% "<unresolved>"
        cat(sprintf("  (%s) %s: %s; location = %s; level = %s\n",
                    i, names(x)[[i]], family, location, level))
    }
    invisible(x)
}
