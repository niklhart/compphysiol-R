.statistical_value_spec <- function(x, label) {
    is_name <- is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
    is_value <- is.numeric(x) && is.null(dim(x)) && length(x) == 1L &&
        !is.na(x) && is.finite(x)
    if (!is_name && !is_value) {
        stop(label, " must be a statistical-parameter name or a finite numeric scalar.",
             call. = FALSE)
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
#' @param coefficient Name or fixed scalar value of a dimensionless coefficient.
#' @param constant Name or fixed scalar value of a component with the target's
#'   units.
#' @param proportional Name or fixed scalar value of a dimensionless component.
#' @returns A standard-deviation specification for use in [normal()].
#' @name sd_specifications
NULL

#' @rdname sd_specifications
#' @export
proportional <- function(coefficient) {
    coefficient <- .process_nse_arg(substitute(coefficient), envir = parent.frame())
    coefficient <- .statistical_value_spec(coefficient, "coefficient")
    structure(list(coefficient = coefficient), class = "ProportionalSD")
}

#' @rdname sd_specifications
#' @export
combined <- function(constant, proportional) {
    constant <- .process_nse_arg(substitute(constant), envir = parent.frame())
    proportional <- .process_nse_arg(substitute(proportional), envir = parent.frame())
    constant <- .statistical_value_spec(constant, "constant")
    proportional <- .statistical_value_spec(proportional, "proportional")
    if (is.character(constant) && identical(constant, proportional)) {
        stop("Combined constant and proportional parameters must be distinct.", call. = FALSE)
    }
    structure(
        list(constant = constant, proportional = proportional),
        class = "CombinedSD"
    )
}

#' Statistical distribution specifications
#'
#' `normal()` uses a mean and a standard-deviation specification. A bare name or
#' fixed scalar in `sd` denotes a constant standard deviation; [proportional()]
#' and [combined()] describe mean-dependent standard deviations. Locations and
#' scale components may likewise be statistical-parameter names or fixed scalar
#' values, including unit-bearing values where appropriate.
#'
#' `lognormal()` uses the median (equivalently, the geometric mean) as its
#' location. It intentionally does not accept arithmetic-mean or raw log-mean
#' parameterizations. `sdlog` is a dimensionless log-scale standard deviation.
#'
#' Missing locations are resolved internally when the statistical model is
#' checked against a dynamic model: an observable uses its model prediction,
#' while a structural parameter requires an explicit location.
#'
#' @param mean Name or fixed scalar value of the normal location, or `NULL` for
#'   an observable prediction default.
#' @param sd Name or fixed scalar value of a constant standard deviation, or a
#'   specification from [proportional()] or [combined()].
#' @param median Name or fixed scalar value of the log-normal median, or `NULL`
#'   for an observable prediction default.
#' @param sdlog Name or fixed dimensionless scalar value of the log-scale
#'   standard deviation.
#' @param level `NULL`, `"individual"`, or `"observation"`. `NULL` is inferred
#'   from the target when validated against a dynamic model.
#' @returns A `StatisticalDistribution` object.
#' @name statistical_distributions
NULL

#' @rdname statistical_distributions
#' @export
normal <- function(mean = NULL, sd, level = NULL) {
    mean <- .process_nse_arg(substitute(mean), envir = parent.frame())
    if (!is.null(mean)) mean <- .statistical_value_spec(mean, "mean")
    if (missing(sd)) stop("sd must be supplied.", call. = FALSE)
    sd <- .process_nse_arg(substitute(sd), envir = parent.frame())
    if (!inherits(sd, c("ProportionalSD", "CombinedSD"))) {
        sd <- .statistical_value_spec(sd, "sd")
    }
    structure(
        list(mean = mean, sd = sd, level = .statistical_level(level)),
        class = c("NormalDistribution", "StatisticalDistribution")
    )
}

#' @rdname statistical_distributions
#' @export
lognormal <- function(median = NULL, sdlog, level = NULL) {
    median <- .process_nse_arg(substitute(median), envir = parent.frame())
    if (!is.null(median)) median <- .statistical_value_spec(median, "median")
    if (missing(sdlog)) stop("sdlog must be supplied.", call. = FALSE)
    sdlog <- .process_nse_arg(substitute(sdlog), envir = parent.frame())
    sdlog <- .statistical_value_spec(sdlog, "sdlog")
    structure(
        list(median = median, sdlog = sdlog, level = .statistical_level(level)),
        class = c("LognormalDistribution", "StatisticalDistribution")
    )
}

#' Specify correlations between individual parameters
#'
#' Each argument declares one pairwise correlation as
#' `list(first_target, second_target, correlation)`. The first two entries name
#' random quantities declared in the enclosing [statistical_model()]. The third
#' entry is either a fixed dimensionless correlation or the name of a
#' statistical parameter. Unspecified pairs are uncorrelated.
#'
#' Correlations apply to the latent normal variables underlying individual-level
#' normal and log-normal distributions. Normal distributions using
#' [proportional()] or [combined()] standard deviations are not supported in a
#' correlated block.
#'
#' @param ... Unnamed lists of the form
#'   `list(first_target, second_target, correlation)`.
#' @returns A correlation specification for use in [statistical_model()].
#' @examples
#' model <- statistical_model(
#'     CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
#'     V = lognormal(median = "V_pop", sdlog = "omega_V"),
#'     correlated(list("CL", "V", "rho_CL_V"))
#' )
#' @export
correlated <- function(...) {
    pairs <- list(...)
    nm <- names(pairs)
    if (length(pairs) && !is.null(nm) && any(nzchar(nm))) {
        stop("Arguments to correlated() must be unnamed correlation triplets.",
             call. = FALSE)
    }
    normalized <- lapply(seq_along(pairs), function(i) {
        pair <- pairs[[i]]
        if (!is.list(pair) || length(pair) != 3L || !is.null(names(pair))) {
            stop("Each correlation must be an unnamed list(first_target, ",
                 "second_target, correlation).", call. = FALSE)
        }
        endpoints <- pair[1:2]
        valid_endpoints <- vapply(endpoints, function(x) {
            is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
        }, logical(1))
        if (!all(valid_endpoints)) {
            stop("The first two entries of each correlation must be nonempty target names.",
                 call. = FALSE)
        }
        if (identical(endpoints[[1]], endpoints[[2]])) {
            stop("A correlation must refer to two distinct targets.", call. = FALSE)
        }
        value <- .statistical_value_spec(pair[[3]], "correlation")
        if (is.numeric(value)) {
            .statistical_require_dimensionless(value, "fixed correlation")
            numeric_value <- as.numeric(value)
            if (numeric_value < -1 || numeric_value > 1) {
                stop("Fixed correlations must be between -1 and 1.", call. = FALSE)
            }
        }
        list(first = endpoints[[1]], second = endpoints[[2]], value = value)
    })
    keys <- vapply(normalized, function(pair) {
        paste(sort(c(pair$first, pair$second)), collapse = "\r")
    }, character(1))
    if (anyDuplicated(keys)) {
        stop("Each unordered target pair may be correlated only once.", call. = FALSE)
    }
    structure(normalized, class = c("Correlated", "list"))
}

#' Construct a statistical model
#'
#' The left-hand-side name identifies the random quantity. Targets are resolved
#' as unresolved structural parameters or model observables when the statistical
#' model is resolved against a dynamic model by downstream statistical workflows.
#'
#' @param ... Named distribution specifications created by [normal()] or
#'   [lognormal()], plus optional unnamed specifications created by
#'   `correlated()`.
#' @returns A `StatisticalModel` object.
#' @examples
#' model <- statistical_model(
#'     CL = normal(mean = "CL_pop", sd = proportional("omega_CL")),
#'     C = lognormal(sdlog = "sigma")
#' )
#' @export
statistical_model <- function(...) {
    entries <- list(...)
    nm <- names(entries)
    if (is.null(nm)) nm <- rep("", length(entries))
    nm[is.na(nm)] <- ""
    is_correlated <- vapply(entries, inherits, logical(1), "Correlated")
    if (any(is_correlated & nzchar(nm))) {
        stop("correlated() specifications must be unnamed in statistical_model().",
             call. = FALSE)
    }
    if (any(!is_correlated & (is.na(nm) | !nzchar(nm)))) {
        stop("Statistical distributions must be named by their random quantity.", call. = FALSE)
    }
    x <- entries[!is_correlated]
    names(x) <- nm[!is_correlated]
    if (anyDuplicated(names(x))) {
        stop("Statistical-model target names must be unique.", call. = FALSE)
    }
    if (!all(vapply(x, inherits, logical(1), "StatisticalDistribution"))) {
        stop("Every statistical-model entry must be a StatisticalDistribution object.",
             call. = FALSE)
    }
    correlations <- unlist(entries[is_correlated], recursive = FALSE)
    if (length(correlations)) {
        keys <- vapply(correlations, function(pair) {
            paste(sort(c(pair$first, pair$second)), collapse = "\r")
        }, character(1))
        if (anyDuplicated(keys)) {
            stop("Each unordered target pair may be correlated only once.", call. = FALSE)
        }
        endpoints <- unique(unlist(lapply(correlations, function(pair) {
            c(pair$first, pair$second)
        }), use.names = FALSE))
        unknown <- setdiff(endpoints, names(x))
        if (length(unknown)) {
            stop("Unknown correlated statistical-model target(s): ",
                 paste(unknown, collapse = ", "), ".", call. = FALSE)
        }
        explicit_observation <- endpoints[vapply(x[endpoints], function(distribution) {
            identical(distribution$level, "observation")
        }, logical(1))]
        if (length(explicit_observation)) {
            stop("Correlations are supported only between individual-level parameter ",
                 "distributions; observation-level target(s): ",
                 paste(explicit_observation, collapse = ", "), ".", call. = FALSE)
        }
    }
    .new_statistical_model(x, correlations)
}

.new_statistical_model <- function(x, correlations = list()) {
    structure(x, correlations = correlations, class = c("StatisticalModel", "list"))
}

.statistical_correlations <- function(x) {
    attr(x, "correlations", exact = TRUE) %||% list()
}

#' Subset a statistical model
#'
#' @param x A `StatisticalModel` object.
#' @param i Indices or names of random quantities to retain.
#' @param ... Unused.
#' @returns A `StatisticalModel` object.
#' @export
`[.StatisticalModel` <- function(x, i, ...) {
    distributions <- unclass(x)
    attr(distributions, "correlations") <- NULL
    selected <- distributions[i]
    retained <- names(selected)
    correlations <- Filter(function(pair) {
        pair$first %in% retained && pair$second %in% retained
    }, .statistical_correlations(x))
    .new_statistical_model(selected, correlations)
}

.resolve_statistical_model <- function(x, model, parameters = NULL) {
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
    resolved <- .new_statistical_model(resolved, .statistical_correlations(x))
    .statistical_validate_correlated_distributions(resolved)

    if (!is.null(parameters)) {
        .check_class(parameters, "Parameters")
        .statistical_validate_units(resolved, ode_model, parameters)
    }
    resolved
}

.statistical_validate_correlated_distributions <- function(x) {
    correlations <- .statistical_correlations(x)
    if (!length(correlations)) return(invisible(NULL))
    targets <- unique(unlist(lapply(correlations, function(pair) {
        c(pair$first, pair$second)
    }), use.names = FALSE))
    observation <- targets[vapply(x[targets], function(distribution) {
        identical(distribution$level, "observation")
    }, logical(1))]
    if (length(observation)) {
        stop("Correlations are supported only between individual-level parameter ",
             "distributions; observation-level target(s): ",
             paste(observation, collapse = ", "), ".", call. = FALSE)
    }
    unsupported <- targets[vapply(x[targets], function(distribution) {
        inherits(distribution, "NormalDistribution") &&
            inherits(distribution$sd, c("ProportionalSD", "CombinedSD"))
    }, logical(1))]
    if (length(unsupported)) {
        stop("Correlated normal distributions require a constant standard deviation; ",
             "proportional() and combined() are unsupported for: ",
             paste(unsupported, collapse = ", "), ".", call. = FALSE)
    }
    invisible(NULL)
}

.statistical_correlation_matrix <- function(x, parameters = NULL, targets = NULL) {
    correlations <- .statistical_correlations(x)
    if (is.null(targets)) {
        targets <- names(x)[names(x) %in% unique(unlist(lapply(correlations, function(pair) {
            c(pair$first, pair$second)
        }), use.names = FALSE))]
    }
    matrix <- diag(1, length(targets), length(targets))
    dimnames(matrix) <- list(targets, targets)
    if (!length(targets)) return(matrix)
    for (pair in correlations) {
        if (!all(c(pair$first, pair$second) %in% targets)) next
        value <- pair$value
        if (!is.null(parameters)) {
            value <- .statistical_value(value, parameters, "correlation")
            .statistical_require_dimensionless(
                value, .statistical_spec_label(pair$value, "fixed correlation")
            )
            value <- as.numeric(value)
            if (value < -1 || value > 1) {
                stop("Correlations must be between -1 and 1.", call. = FALSE)
            }
        }
        matrix[pair$first, pair$second] <- value
        matrix[pair$second, pair$first] <- value
    }
    matrix
}

.statistical_validate_correlation_matrix <- function(matrix) {
    if (!length(matrix)) return(invisible(NULL))
    eigenvalues <- eigen(matrix, symmetric = TRUE, only.values = TRUE)$values
    tolerance <- sqrt(.Machine$double.eps) * max(1, nrow(matrix))
    if (min(eigenvalues) < -tolerance) {
        stop("The resolved correlation matrix must be positive semidefinite.", call. = FALSE)
    }
    invisible(NULL)
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

.statistical_value <- function(spec, parameters, label) {
    if (!is.character(spec)) return(spec)
    value <- parameters[[spec]]
    if (is.null(value)) stop("Missing statistical parameter: ", spec, ".", call. = FALSE)
    if (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L ||
        is.na(value) || !is.finite(value)) {
        stop("Statistical parameter '", spec, "' must be a finite numeric scalar.",
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
        location <- if (inherits(distribution, "NormalDistribution")) {
            distribution$mean
        } else {
            distribution$median
        }
        runtime[[target]] <- .statistical_value(location, parameters, "location")
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
                mean_value <- .statistical_value(distribution$mean, parameters, "mean")
                .statistical_require_target_units(mean_value, target_value,
                    .statistical_spec_label(distribution$mean, "fixed mean"), target)
            }
            if (!inherits(distribution$sd, c("ProportionalSD", "CombinedSD"))) {
                sd_value <- .statistical_value(distribution$sd, parameters, "sd")
                .statistical_require_target_units(sd_value, target_value,
                    .statistical_spec_label(distribution$sd, "fixed sd"), target)
            } else if (inherits(distribution$sd, "ProportionalSD")) {
                spec <- distribution$sd$coefficient
                .statistical_require_dimensionless(
                    .statistical_value(spec, parameters, "coefficient"),
                    .statistical_spec_label(spec, "fixed coefficient")
                )
            } else {
                constant <- distribution$sd$constant
                proportional_spec <- distribution$sd$proportional
                .statistical_require_target_units(
                    .statistical_value(constant, parameters, "constant"), target_value,
                    .statistical_spec_label(constant, "fixed constant"), target
                )
                .statistical_require_dimensionless(
                    .statistical_value(proportional_spec, parameters, "proportional"),
                    .statistical_spec_label(proportional_spec, "fixed proportional")
                )
            }
        } else {
            if (!inherits(distribution$median, "PredictionLocation")) {
                median_value <- .statistical_value(distribution$median, parameters, "median")
                .statistical_require_target_units(median_value, target_value,
                    .statistical_spec_label(distribution$median, "fixed median"), target)
            }
            .statistical_require_dimensionless(
                .statistical_value(distribution$sdlog, parameters, "sdlog"),
                .statistical_spec_label(distribution$sdlog, "fixed sdlog")
            )
        }
    }
    correlation_matrix <- .statistical_correlation_matrix(x, parameters)
    .statistical_validate_correlation_matrix(correlation_matrix)
    invisible(NULL)
}

.statistical_spec_label <- function(spec, fixed) {
    if (is.character(spec)) spec else fixed
}

.statistical_format_value_spec <- function(x) {
    paste(format(x), collapse = ", ")
}

.statistical_format_sd <- function(x) {
    if (inherits(x, "ProportionalSD")) {
        return(paste0("proportional(",
                      .statistical_format_value_spec(x$coefficient), ")"))
    }
    if (inherits(x, "CombinedSD")) {
        return(paste0(
            "combined(constant = ", .statistical_format_value_spec(x$constant),
            ", proportional = ", .statistical_format_value_spec(x$proportional), ")"
        ))
    }
    .statistical_format_value_spec(x)
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
            if (is.null(location)) "<unresolved>" else paste(format(location), collapse = ", ")
        }
        scale_name <- if (inherits(d, "NormalDistribution")) "sd" else "sdlog"
        scale <- if (inherits(d, "NormalDistribution")) {
            .statistical_format_sd(d$sd)
        } else {
            .statistical_format_value_spec(d$sdlog)
        }
        level <- d$level %||% "<unresolved>"
        cat(sprintf("  (%s) %s: %s; location = %s; %s = %s; level = %s\n",
                    i, names(x)[[i]], family, location, scale_name, scale, level))
    }
    correlations <- .statistical_correlations(x)
    if (length(correlations)) {
        targets <- names(x)[names(x) %in% unique(unlist(lapply(correlations, function(pair) {
            c(pair$first, pair$second)
        }), use.names = FALSE))]
        matrix <- matrix("0", length(targets), length(targets),
                         dimnames = list(targets, targets))
        diag(matrix) <- "1"
        for (pair in correlations) {
            value <- .statistical_format_value_spec(pair$value)
            matrix[pair$first, pair$second] <- value
            matrix[pair$second, pair$first] <- value
        }
        all_normal <- all(vapply(x[targets], inherits, logical(1), "NormalDistribution"))
        heading <- if (all_normal) " Correlations:\n" else {
            " Correlations (latent normal scale):\n"
        }
        cat(heading)
        output <- utils::capture.output(print(noquote(matrix)))
        cat(paste0("  ", output, collapse = "\n"), "\n", sep = "")
    }
    invisible(x)
}
