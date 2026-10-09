#' Sample observations conditional on model predictions
#'
#' Replaces each conditional prediction with one independent draw from the
#' corresponding observation-level distribution. Rows, ordering, duplicate
#' rows, units, and additional columns are preserved. Individual-level
#' statistical entries are ignored unless they conflict with a represented
#' observable.
#'
#' The observables represented in `x` act as the sampling targets. A matching
#' statistical entry with an unresolved level is treated as observation-level
#' for this operation. An entry explicitly marked individual-level is rejected.
#'
#' @param x An `ObservationData` object containing conditional predictions, or
#'   one `SimulationResult` whose `observables` component contains them.
#' @param statistics A [StatisticalModel][statistical_model()] containing a
#'   distribution for every observable represented in `x`.
#' @param parameters A [Parameters][parameters()] object containing referenced
#'   observation-distribution parameters.
#' @param seed Optional numeric random seed.
#' @returns An `ObservationData` object whose `value` column contains sampled
#'   observations.
#' @examples
#' predictions <- observation_data(
#'     time = c(1, 2) [h], observable = "C", value = c(5, 3) [mg/L]
#' )
#' statistics <- statistical_model(
#'     C = normal(sd = combined("sigma_add", "sigma_prop"))
#' )
#' sample_observations(
#'     predictions,
#'     statistics,
#'     parameters(sigma_add = 0.1 [mg/L], sigma_prop = 0.2),
#'     seed = 123
#' )
#' @export
sample_observations <- function(x, statistics, parameters, seed = NULL) {
    if (inherits(x, "SimulationResult")) {
        if (is.null(x$observables)) {
            stop("SimulationResult does not contain observable predictions.", call. = FALSE)
        }
        x <- x$observables
    }
    if (!inherits(x, "ObservationData")) {
        stop("x must be one ObservationData or SimulationResult object.", call. = FALSE)
    }
    x <- .new_observation_data(x)
    .check_class(statistics, "StatisticalModel")
    .check_class(parameters, "Parameters")
    .sample_observations_seed(seed)

    predictions <- lapply(seq_len(nrow(x)), function(i) x$value[[i]])
    invalid <- vapply(predictions, function(value) {
        !is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value)
    }, logical(1))
    if (any(invalid)) {
        stop("ObservationData must contain finite numeric predictions in every row.",
             call. = FALSE)
    }

    targets <- unique(x$observable)
    missing <- setdiff(targets, names(statistics))
    if (length(missing)) {
        stop("Statistical model is missing observation-level distributions for: ",
             paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    selected <- statistics[targets]
    individual <- targets[vapply(selected, function(distribution) {
        identical(distribution$level, "individual")
    }, logical(1))]
    if (length(individual)) {
        stop("Prediction observable(s) are explicitly individual-level in the statistical model: ",
             paste(individual, collapse = ", "), ".", call. = FALSE)
    }

    if (!is.null(seed)) set.seed(seed)
    sampled <- lapply(seq_len(nrow(x)), function(i) {
        distribution <- statistics[[x$observable[[i]]]]
        .sample_observation_distribution(distribution, predictions[[i]], parameters)
    })
    x$value <- do.call(.c_units, sampled)
    .new_observation_data(x)
}

.sample_observations_seed <- function(seed) {
    if (is.null(seed)) return(invisible(NULL))
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed) ||
        seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
        stop("seed must be NULL or a non-negative whole number.", call. = FALSE)
    }
    invisible(NULL)
}

.sample_observation_distribution <- function(distribution, prediction, parameters) {
    location_spec <- if (inherits(distribution, "NormalDistribution")) {
        distribution$mean
    } else if (inherits(distribution, "LognormalDistribution")) {
        distribution$median
    } else {
        stop("Unsupported observation-level statistical distribution.", call. = FALSE)
    }
    location <- if (is.null(location_spec) || inherits(location_spec, "PredictionLocation")) {
        prediction
    } else {
        .sampling_value(location_spec, parameters, "location")
    }
    location <- .sample_observation_align_location(location, prediction)

    if (inherits(distribution, "NormalDistribution")) {
        sd <- .sampling_normal_sd(distribution$sd, location, parameters)
        if (!is.finite(sd) || sd < 0) {
            stop("Normal standard deviations must be finite and non-negative.", call. = FALSE)
        }
        draw <- stats::rnorm(1L, mean = as.numeric(location), sd = sd)
    } else {
        sdlog <- .sampling_dimensionless(distribution$sdlog, parameters, "sdlog")
        if (!is.finite(sdlog) || sdlog < 0) {
            stop("Log-normal sdlog values must be finite and non-negative.", call. = FALSE)
        }
        if (as.numeric(location) <= 0) {
            stop("Log-normal conditional medians must be positive.", call. = FALSE)
        }
        draw <- stats::rlnorm(1L, meanlog = log(as.numeric(location)), sdlog = sdlog)
    }
    .sampling_restore_units(draw, prediction)
}

.sample_observation_align_location <- function(location, prediction) {
    tryCatch(
        .check_compatible_units(prediction, location, "observation location"),
        error = function(e) stop(conditionMessage(e), call. = FALSE)
    )
    if (inherits(prediction, "units")) {
        location <- units::set_units(
            location, units::deparse_unit(prediction), mode = "standard"
        )
    }
    location
}
