#' Sample a realized population
#'
#' Samples structural parameters from the individual-level distributions in a
#' [StatisticalModel][statistical_model()], respecting dependencies declared by
#' [correlated()]. Observation-level entries are ignored. The result contains
#' only the sampled targets; population parameters used as distribution
#' locations, scales, or correlations are not copied into the individual
#' parameter sets.
#'
#' With `targets = NULL`, every statistical-model entry must have an explicit or
#' previously resolved `level`, and all individual-level entries are sampled.
#' Supplying `targets` explicitly selects exactly those entries and resolves an
#' unspecified level as individual for this operation. An entry already marked
#' as observation-level cannot be selected.
#'
#' @param statistics A `StatisticalModel` with resolved or explicit levels.
#' @param parameters A [Parameters][parameters()] object containing population
#'   parameters referenced by the individual-level distributions and their
#'   correlations.
#' @param n Positive whole number of individuals to sample.
#' @param targets Optional character vector of statistical-model targets to
#'   sample. `NULL` samples all resolved individual-level entries. Explicit
#'   targets may have unresolved levels but must not be observation-level.
#' @returns A named `ParameterSets` collection with names `individual_1`,
#'   `individual_2`, and so on.
#' @examples
#' statistics <- statistical_model(
#'     CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
#'     C = normal(sd = "sigma")
#' )
#' population_parameters <- parameters(
#'     CL_pop = 1 [L/h], omega_CL = 0.2, sigma = 1 [mg/L]
#' )
#' population <- sample_population(
#'     statistics, population_parameters, n = 3, targets = "CL"
#' )
#' population
#' @export
sample_population <- function(statistics, parameters, n, targets = NULL) {
    .check_class(statistics, "StatisticalModel")
    .check_class(parameters, "Parameters")
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
        n < 1 || n > .Machine$integer.max || n != floor(n)) {
        stop("n must be a positive whole number.", call. = FALSE)
    }
    n <- as.integer(n)

    if (is.null(targets)) {
        unresolved <- names(statistics)[vapply(
            statistics, function(x) is.null(x$level), logical(1)
        )]
        if (length(unresolved)) {
            stop("sample_population() requires explicit or resolved levels when targets is NULL; ",
                 "supply targets for unresolved entries: ",
                 paste(unresolved, collapse = ", "), ".", call. = FALSE)
        }
        targets <- names(statistics)[vapply(
            statistics, function(x) identical(x$level, "individual"), logical(1)
        )]
    } else {
        if (!is.character(targets) || anyNA(targets) || any(!nzchar(targets))) {
            stop("targets must be NULL or a character vector of target names.", call. = FALSE)
        }
        if (anyDuplicated(targets)) {
            stop("targets must not contain duplicate names.", call. = FALSE)
        }
        unknown <- setdiff(targets, names(statistics))
        if (length(unknown)) {
            stop("Unknown statistical-model target(s): ",
                 paste(unknown, collapse = ", "), ".", call. = FALSE)
        }
        observation <- targets[vapply(statistics[targets], function(x) {
            identical(x$level, "observation")
        }, logical(1))]
        if (length(observation)) {
            stop("Observation-level target(s) cannot be sampled as individual parameters: ",
                 paste(observation, collapse = ", "), ".", call. = FALSE)
        }
    }
    individual <- statistics[targets]
    .statistical_validate_correlated_distributions(individual)
    correlation_matrix <- .statistical_correlation_matrix(
        individual, parameters = parameters, targets = names(individual)
    )
    .statistical_validate_correlation_matrix(correlation_matrix)
    latent <- .sample_correlated_standard_normals(correlation_matrix, n)
    sampled <- lapply(seq_along(individual), function(j) {
        .sample_individual_distribution(
            individual[[j]], parameters = parameters, n = n,
            latent = latent[, j]
        )
    })
    names(sampled) <- names(individual)

    people <- lapply(seq_len(n), function(i) {
        values <- lapply(sampled, `[[`, i)
        structure(values, class = c("Parameters", "list"))
    })
    names(people) <- paste0("individual_", seq_len(n))
    .new_parameter_sets(people, check_units = FALSE)
}

.sample_correlated_standard_normals <- function(correlation, n) {
    if (!ncol(correlation)) return(matrix(numeric(), nrow = n, ncol = 0L))
    decomposition <- eigen(correlation, symmetric = TRUE)
    root <- decomposition$vectors %*%
        diag(sqrt(pmax(decomposition$values, 0)), nrow = ncol(correlation))
    matrix(stats::rnorm(n * ncol(correlation)), nrow = n) %*% t(root)
}

.sample_individual_distribution <- function(distribution, parameters, n, latent = NULL) {
    if (is.null(latent)) latent <- stats::rnorm(n)
    if (inherits(distribution, "NormalDistribution")) {
        mean <- .sampling_value(distribution$mean, parameters, "mean")
        sd <- .sampling_normal_sd(distribution$sd, mean, parameters)
        if (sd < 0) stop("Normal standard deviations must be non-negative.", call. = FALSE)
        draws <- as.numeric(mean) + sd * latent
        return(.sampling_restore_units(draws, mean))
    }
    if (inherits(distribution, "LognormalDistribution")) {
        median <- .sampling_value(distribution$median, parameters, "median")
        sdlog <- .sampling_dimensionless(
            distribution$sdlog, parameters, "sdlog"
        )
        if (as.numeric(median) <= 0) {
            stop("Log-normal medians must be positive.", call. = FALSE)
        }
        if (sdlog < 0) {
            stop("Log-normal sdlog values must be non-negative.", call. = FALSE)
        }
        draws <- exp(log(as.numeric(median)) + sdlog * latent)
        return(.sampling_restore_units(draws, median))
    }
    stop("Unsupported individual-level statistical distribution.", call. = FALSE)
}

.sampling_normal_sd <- function(spec, mean, parameters) {
    if (inherits(spec, "ProportionalSD")) {
        coefficient <- .sampling_dimensionless(
            spec$coefficient, parameters, "proportional coefficient"
        )
        return(coefficient * abs(as.numeric(mean)))
    }
    if (inherits(spec, "CombinedSD")) {
        constant <- .sampling_target_scale(
            spec$constant, mean, parameters, "constant"
        )
        proportional <- .sampling_dimensionless(
            spec$proportional, parameters, "proportional coefficient"
        )
        return(constant + proportional * abs(as.numeric(mean)))
    }
    .sampling_target_scale(spec, mean, parameters, "sd")
}

.sampling_value <- function(spec, parameters, label) {
    if (inherits(spec, "PredictionLocation") || is.null(spec)) {
        stop("Individual-level distributions require an explicit location.", call. = FALSE)
    }
    .statistical_value(spec, parameters, label)
}

.sampling_target_scale <- function(spec, target, parameters, label) {
    value <- .sampling_value(spec, parameters, label)
    tryCatch(
        .check_compatible_units(target, value, label),
        error = function(e) stop(conditionMessage(e), call. = FALSE)
    )
    if (inherits(target, "units")) {
        value <- units::set_units(
            value, units::deparse_unit(target), mode = "standard"
        )
    }
    as.numeric(value)
}

.sampling_dimensionless <- function(spec, parameters, label) {
    value <- .sampling_value(spec, parameters, label)
    if (inherits(value, "units")) {
        value <- tryCatch(
            units::set_units(value, "1", mode = "standard"),
            error = function(e) stop(label, " must be dimensionless.", call. = FALSE)
        )
    }
    as.numeric(value)
}

.sampling_restore_units <- function(x, template) {
    if (!inherits(template, "units")) return(x)
    units::set_units(x, units::deparse_unit(template), mode = "standard")
}
