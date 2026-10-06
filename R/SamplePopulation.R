#' Sample a realized population
#'
#' Samples structural parameters independently from the individual-level
#' distributions in a [StatisticalModel][statistical_model()]. Observation-level
#' entries are ignored. The result contains only the sampled targets; population
#' parameters used as distribution locations or scales are not copied into the
#' individual parameter sets.
#'
#' With `targets = NULL`, every statistical-model entry must have an explicit or
#' previously resolved `level`, and all individual-level entries are sampled.
#' Supplying `targets` explicitly selects exactly those entries and resolves an
#' unspecified level as individual for this operation. An entry already marked
#' as observation-level cannot be selected.
#'
#' @param statistics A `StatisticalModel` with resolved or explicit levels.
#' @param parameters A [Parameters][parameters()] object containing population
#'   parameters referenced by the individual-level distributions.
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
    sampled <- lapply(individual, .sample_individual_distribution,
                      parameters = parameters, n = n)

    people <- lapply(seq_len(n), function(i) {
        values <- lapply(sampled, `[[`, i)
        structure(values, class = c("Parameters", "list"))
    })
    names(people) <- paste0("individual_", seq_len(n))
    .new_parameter_sets(people, check_units = FALSE)
}

.sample_individual_distribution <- function(distribution, parameters, n) {
    if (inherits(distribution, "NormalDistribution")) {
        mean <- .sample_population_value(distribution$mean, parameters, "mean")
        sd <- .sample_population_normal_sd(distribution$sd, mean, parameters)
        if (sd < 0) stop("Normal standard deviations must be non-negative.", call. = FALSE)
        draws <- stats::rnorm(n, mean = as.numeric(mean), sd = sd)
        return(.sample_population_restore_units(draws, mean))
    }
    if (inherits(distribution, "LognormalDistribution")) {
        median <- .sample_population_value(distribution$median, parameters, "median")
        sdlog <- .sample_population_dimensionless(
            distribution$sdlog, parameters, "sdlog"
        )
        if (as.numeric(median) <= 0) {
            stop("Log-normal medians must be positive.", call. = FALSE)
        }
        if (sdlog < 0) {
            stop("Log-normal sdlog values must be non-negative.", call. = FALSE)
        }
        draws <- stats::rlnorm(n, meanlog = log(as.numeric(median)), sdlog = sdlog)
        return(.sample_population_restore_units(draws, median))
    }
    stop("Unsupported individual-level statistical distribution.", call. = FALSE)
}

.sample_population_normal_sd <- function(spec, mean, parameters) {
    if (inherits(spec, "ProportionalSD")) {
        coefficient <- .sample_population_dimensionless(
            spec$coefficient, parameters, "proportional coefficient"
        )
        return(coefficient * abs(as.numeric(mean)))
    }
    if (inherits(spec, "CombinedSD")) {
        constant <- .sample_population_target_scale(
            spec$constant, mean, parameters, "constant"
        )
        proportional <- .sample_population_dimensionless(
            spec$proportional, parameters, "proportional coefficient"
        )
        return(constant + proportional * abs(as.numeric(mean)))
    }
    .sample_population_target_scale(spec, mean, parameters, "sd")
}

.sample_population_value <- function(spec, parameters, label) {
    if (inherits(spec, "PredictionLocation") || is.null(spec)) {
        stop("Individual-level distributions require an explicit location.", call. = FALSE)
    }
    .statistical_value(spec, parameters, label)
}

.sample_population_target_scale <- function(spec, target, parameters, label) {
    value <- .sample_population_value(spec, parameters, label)
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

.sample_population_dimensionless <- function(spec, parameters, label) {
    value <- .sample_population_value(spec, parameters, label)
    if (inherits(value, "units")) {
        value <- tryCatch(
            units::set_units(value, "1", mode = "standard"),
            error = function(e) stop(label, " must be dimensionless.", call. = FALSE)
        )
    }
    as.numeric(value)
}

.sample_population_restore_units <- function(x, template) {
    if (!inherits(template, "units")) return(x)
    units::set_units(x, units::deparse_unit(template), mode = "standard")
}
