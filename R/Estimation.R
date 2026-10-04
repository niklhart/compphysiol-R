#' Describe an observation error model
#'
#' Observation models map model observables to compositional residual-error
#' specifications. Observation values always come from the canonical `value`
#' column of [ObservationData][observation_data()].
#'
#' @param ... Named residual-error objects created by [additive_error()] and
#'   related constructors, one per model observable.
#' @returns An `ObservationModel` object.
#' @export
observation_model <- function(...) {
    x <- list(...)
    nm <- names(x)
    if (length(x) && (is.null(nm) || anyNA(nm) || any(!nzchar(nm)))) {
        stop("Observation error models must be named by observable.", call. = FALSE)
    }
    if (anyDuplicated(nm)) {
        stop("Observation model names must be unique; duplicated observable names are not allowed.",
             call. = FALSE)
    }
    if (!all(vapply(x, inherits, logical(1), "ObservationError"))) {
        stop("Every observation model entry must be an ObservationError object.", call. = FALSE)
    }
    structure(x, class = c("ObservationModel", "list"))
}

.observation_error_parameter <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
        stop(label, " must name a statistical parameter.", call. = FALSE)
    }
    x
}

#' Construct residual-error specifications
#'
#' @param sigma Name of the residual standard-deviation parameter.
#' @param additive Name of the additive standard-deviation parameter.
#' @param proportional Name of the proportional standard-deviation parameter.
#' @returns An `ObservationError` object of the corresponding subclass.
#' @name observation_errors
NULL

#' @rdname observation_errors
#' @export
additive_error <- function(sigma = "sigma") {
    sigma <- .observation_error_parameter(sigma, "sigma")
    structure(list(sigma = sigma), class = c("AdditiveError", "ObservationError"))
}

#' @rdname observation_errors
#' @export
proportional_error <- function(sigma = "sigma") {
    sigma <- .observation_error_parameter(sigma, "sigma")
    structure(list(sigma = sigma), class = c("ProportionalError", "ObservationError"))
}

#' @rdname observation_errors
#' @export
combined_error <- function(additive = "sigma_add", proportional = "sigma_prop") {
    additive <- .observation_error_parameter(additive, "additive")
    proportional <- .observation_error_parameter(proportional, "proportional")
    if (identical(additive, proportional)) {
        stop("Combined-error additive and proportional parameters must be distinct.", call. = FALSE)
    }
    structure(
        list(additive = additive, proportional = proportional),
        class = c("CombinedError", "ObservationError")
    )
}

#' @rdname observation_errors
#' @export
lognormal_error <- function(sigma = "sigma") {
    sigma <- .observation_error_parameter(sigma, "sigma")
    structure(list(sigma = sigma), class = c("LognormalError", "ObservationError"))
}

#' Describe one parameter to estimate
#'
#' @param initial Finite numeric scalar initial estimate, optionally with units.
#' @param lower,upper Optional scalar bounds. When `NULL`, identity transforms
#'   use `-Inf` or `Inf`, while log transforms use zero or `Inf`. Inferred bounds
#'   inherit the units of `initial`. Logit transforms use zero and one.
#'   Explicit finite bounds must use units compatible with `initial`.
#' @param transform Internal optimizer transformation: `"identity"`, `"log"`,
#'   or `"logit"`. Log transforms require a positive initial value. Logit
#'   transforms require finite lower and upper bounds.
#' @returns A `ParameterEstimate` object.
#' @export
parameter_estimate <- function(
    initial,
    lower = NULL,
    upper = NULL,
    transform = c("identity", "log", "logit")
) {
    initial <- .process_nse_arg(substitute(initial), envir = parent.frame())
    lower <- .process_nse_arg(substitute(lower), envir = parent.frame())
    upper <- .process_nse_arg(substitute(upper), envir = parent.frame())
    transform <- tryCatch(
        match.arg(transform),
        error = function(e) stop("Unknown parameter transform; use identity, log, or logit.",
                                 call. = FALSE)
    )
    .estimation_scalar(initial, "initial", finite = TRUE)
    if (is.null(lower)) {
        lower <- .estimation_default_bound(if (transform %in% c("log", "logit")) 0 else -Inf,
                                           initial)
    }
    if (is.null(upper)) {
        upper <- .estimation_default_bound(if (identical(transform, "logit")) 1 else Inf,
                                           initial)
    }
    if (identical(transform, "log") && as.numeric(initial) <= 0) {
        stop("A log-transformed parameter must have a positive initial value.", call. = FALSE)
    }
    .estimation_scalar(lower, "lower", finite = FALSE)
    .estimation_scalar(upper, "upper", finite = FALSE)

    lower_numeric <- .estimation_bound_numeric(lower, initial, "lower")
    upper_numeric <- .estimation_bound_numeric(upper, initial, "upper")
    initial_numeric <- as.numeric(initial)
    if (lower_numeric > upper_numeric) {
        stop("Parameter lower bound cannot exceed its upper bound.", call. = FALSE)
    }
    if (initial_numeric < lower_numeric || initial_numeric > upper_numeric) {
        stop("Parameter initial value must lie within its bounds.", call. = FALSE)
    }
    if (identical(transform, "logit")) {
        if (!is.finite(lower_numeric) || !is.finite(upper_numeric) ||
            initial_numeric <= lower_numeric || initial_numeric >= upper_numeric) {
            stop("A logit-transformed parameter requires finite bounds and an initial value strictly between them.",
                 call. = FALSE)
        }
    }
    structure(
        list(initial = initial, lower = lower, upper = upper, transform = transform),
        class = "ParameterEstimate"
    )
}

.estimation_default_bound <- function(value, initial) {
    if (!inherits(initial, "units")) return(value)
    units::set_units(value, units::deparse_unit(initial), mode = "standard")
}

#' Print an estimated-parameter specification
#'
#' @param x A `ParameterEstimate` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.ParameterEstimate <- function(x, ...) {
    unit <- if (inherits(x$initial, "units")) units::deparse_unit(x$initial) else NULL
    values <- c(
        initial = as.numeric(x$initial),
        lower = .estimation_bound_numeric(x$lower, x$initial, "lower"),
        upper = .estimation_bound_numeric(x$upper, x$initial, "upper")
    )
    cat("ParameterEstimate:\n")
    cat(" initial: ", format(values[["initial"]], trim = TRUE), "\n", sep = "")
    cat(" bounds: [", format(values[["lower"]], trim = TRUE), ", ",
        format(values[["upper"]], trim = TRUE), "]\n", sep = "")
    cat(" transform: ", x$transform, "\n", sep = "")
    if (!is.null(unit)) cat(" unit: ", unit, "\n", sep = "")
    invisible(x)
}

.estimation_scalar <- function(x, label, finite) {
    if (!is.numeric(x) || !is.null(dim(x)) || length(x) != 1L || is.na(x) ||
        (finite && !is.finite(x))) {
        stop("Parameter ", label, " must be a ", if (finite) "finite " else "",
             "numeric scalar.", call. = FALSE)
    }
    invisible(NULL)
}

.estimation_bound_numeric <- function(bound, initial, label) {
    if (!is.finite(bound)) return(as.numeric(bound))
    bound_units <- inherits(bound, "units")
    initial_units <- inherits(initial, "units")
    if (bound_units != initial_units) {
        stop("Finite parameter ", label, " bound and initial value must use compatible units.",
             call. = FALSE)
    }
    if (bound_units) {
        bound <- tryCatch(
            units::set_units(bound, units::deparse_unit(initial), mode = "standard"),
            error = function(e) stop("Parameter ", label,
                " bound and initial value must use compatible units.", call. = FALSE)
        )
    }
    as.numeric(bound)
}

#' Combine parameter-estimation specifications
#'
#' Named [parameter_estimate()] objects and existing `ParameterEstimates`
#' collections can be combined with `c()`. Names identify the model or
#' observation-model parameters being estimated.
#'
#' @param ... Named `ParameterEstimate` or `ParameterEstimates` objects.
#' @param recursive Unused.
#' @returns A `ParameterEstimates` collection.
#' @name parameter_estimates
NULL

#' @rdname parameter_estimates
#' @export
c.ParameterEstimate <- function(..., recursive = FALSE) {
    .combine_parameter_estimates(list(...))
}

#' @rdname parameter_estimates
#' @export
c.ParameterEstimates <- function(..., recursive = FALSE) {
    .combine_parameter_estimates(list(...))
}

.combine_parameter_estimates <- function(x) {
    labels <- names(x)
    if (is.null(labels)) labels <- rep("", length(x))
    out <- list()
    for (i in seq_along(x)) {
        item <- x[[i]]
        label <- labels[[i]]
        if (inherits(item, "ParameterEstimate")) {
            if (is.na(label) || !nzchar(label)) {
                stop("Every parameter estimate must be named.", call. = FALSE)
            }
            part <- setNames(list(item), label)
        } else if (inherits(item, "ParameterEstimates")) {
            part <- unclass(item)
            if (!is.na(label) && nzchar(label)) {
                names(part) <- paste(label, names(part), sep = ".")
            }
        } else {
            stop("c() can only combine ParameterEstimate and ParameterEstimates objects.",
                 call. = FALSE)
        }
        out <- append(out, part)
    }
    .new_parameter_estimates(out)
}

.new_parameter_estimates <- function(x = list()) {
    nm <- names(x)
    if (length(x) && (is.null(nm) || anyNA(nm) || any(!nzchar(nm)))) {
        stop("Every parameter estimate must be named.", call. = FALSE)
    }
    if (anyDuplicated(nm)) {
        stop("Parameter estimate names must be unique; duplicated names are not allowed.",
             call. = FALSE)
    }
    if (!all(vapply(x, inherits, logical(1), "ParameterEstimate"))) {
        stop("Every parameter estimate must be a ParameterEstimate object.", call. = FALSE)
    }
    structure(x, class = c("ParameterEstimates", "list"))
}

#' Subset parameter-estimation specifications
#'
#' @param x A `ParameterEstimates` collection.
#' @param i Indices or names of estimates to retain.
#' @param ... Unused.
#' @returns A `ParameterEstimates` collection.
#' @export
`[.ParameterEstimates` <- function(x, i, ...) {
    if (missing(i)) return(x)
    .new_parameter_estimates(unclass(x)[i])
}

#' Print parameter-estimation specifications
#'
#' @param x A `ParameterEstimates` collection.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.ParameterEstimates <- function(x, ...) {
    if (!length(x)) {
        cat(" Parameter estimates: (none)\n")
        return(invisible(x))
    }
    cat(" Parameter estimates:\n")
    for (i in seq_along(x)) {
        estimate <- x[[i]]
        values <- c(
            initial = as.numeric(estimate$initial),
            lower = .estimation_bound_numeric(estimate$lower, estimate$initial, "lower"),
            upper = .estimation_bound_numeric(estimate$upper, estimate$initial, "upper")
        )
        unit <- if (inherits(estimate$initial, "units")) {
            as.character(units(estimate$initial))
        } else {
            "1"
        }
        cat(sprintf(
            "  (%s) %s: initial = %s, bounds = [%s, %s], transform = %s, unit [%s]\n",
            i, names(x)[[i]], format(values[["initial"]], trim = TRUE),
            format(values[["lower"]], trim = TRUE),
            format(values[["upper"]], trim = TRUE), estimate$transform, unit
        ))
    }
    invisible(x)
}

#' Configure the stats optim estimation backend
#'
#' @param method Optimization method passed to [stats::optim()].
#' @param control Control list passed to [stats::optim()].
#' @returns An `OptimBackend` object.
#' @export
optim_backend <- function(
    method = "L-BFGS-B",
    control = list()
) {
    methods <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")
    if (!is.character(method) || length(method) != 1L || is.na(method) ||
        !method %in% methods) {
        stop("Unknown optim method.", call. = FALSE)
    }
    if (!is.list(control)) stop("optim control must be a list.", call. = FALSE)
    structure(
        list(method = method, control = control),
        class = c("OptimBackend", "EstimationBackend")
    )
}

#' Create a backend-neutral estimation problem
#'
#' @param model A `CompartmentModel`, `ProcessModel`, `OdeModel`, or
#'   `CompiledOdeModel`.
#' @param experiments An [Experiment][experiment()] or
#'   [Experiments][experiments()] collection containing `ObservationData`.
#' @param parameters Named [parameter_estimate()] objects combined with `c()`
#'   into a `ParameterEstimates` collection.
#' @param observation An observation model from [observation_model()].
#' @returns An `EstimationProblem` object.
#' @export
estimation_problem <- function(model, experiments, parameters, observation) {
    supported <- c("CompartmentModel", "ProcessModel", "OdeModel", "CompiledOdeModel")
    model_class <- supported[vapply(supported, inherits, logical(1), x = model)]
    if (!length(model_class)) {
        supplied <- class(model)[1] %||% typeof(model)
        stop(supplied, " is not supported for estimation. Supported models are ",
             paste(supported, collapse = ", "), ".", call. = FALSE)
    }
    .check_class(parameters, "ParameterEstimates")
    .check_class(observation, "ObservationModel")
    if (inherits(experiments, "Experiment")) experiments <- .new_experiments(list(experiments))
    .check_class(experiments, "Experiments")
    if (!length(experiments)) stop("Estimation requires at least one experiment.", call. = FALSE)

    model_observables <- .estimation_model_observables(model)
    estimated_names <- names(parameters)
    observed_names <- character()
    for (i in seq_along(experiments)) {
        e <- experiments[[i]]
        validate_experiment(e)
        if (!inherits(e$observations, "ObservationData")) {
            stop("Estimation requires ObservationData in every experiment.", call. = FALSE)
        }
        if (!nrow(e$observations)) stop("Estimation requires nonempty observation data.", call. = FALSE)
        if (all(is.na(e$observations$value))) {
            stop("Estimation requires at least one non-missing observation value in every experiment.",
                 call. = FALSE)
        }
        unknown <- setdiff(e$observations$observable, model_observables)
        if (length(unknown)) {
            stop("Unknown observable(s) in estimation data: ", paste(unknown, collapse = ", "), ".",
                 call. = FALSE)
        }
        observed_names <- union(observed_names, e$observations$observable)
        overlap <- intersect(names(e$parameters), estimated_names)
        if (length(overlap)) {
            stop("Known experiment parameter(s) cannot also be estimated: ",
                 paste(overlap, collapse = ", "), ".", call. = FALSE)
        }
    }
    missing_error <- setdiff(observed_names, names(observation))
    if (length(missing_error)) {
        stop("Observation model is missing observable(s): ",
             paste(missing_error, collapse = ", "), ".", call. = FALSE)
    }
    error_parameters <- unique(unlist(lapply(observation[observed_names], unclass), use.names = FALSE))
    missing_parameters <- setdiff(error_parameters, estimated_names)
    if (length(missing_parameters)) {
        stop("Observation model parameter(s) are not declared for estimation: ",
             paste(missing_parameters, collapse = ", "), ".", call. = FALSE)
    }
    structure(
        list(model = model, experiments = experiments, parameters = parameters,
             observation = observation),
        class = "EstimationProblem"
    )
}

#' Print an estimation problem
#'
#' @param x An `EstimationProblem` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.EstimationProblem <- function(x, ...) {
    observations <- sum(vapply(
        unclass(x$experiments), function(e) nrow(e$observations), integer(1)
    ))
    cat("EstimationProblem:\n")
    cat(" model: ", class(x$model)[[1]], "\n", sep = "")
    cat(" experiments: ", length(x$experiments), "\n", sep = "")
    cat(" observations: ", observations, "\n", sep = "")
    cat(" estimated parameters: ", .estimation_format_names(names(x$parameters)), "\n", sep = "")
    cat(" observation models: ", .estimation_format_names(names(x$observation)), "\n", sep = "")
    invisible(x)
}

.estimation_format_names <- function(x) {
    if (!length(x)) return("(none)")
    paste(x, collapse = ", ")
}

.estimation_model_observables <- function(model) {
    if (inherits(model, "CompiledOdeModel")) model <- model$ode_model
    names(model$observables)
}

#' Estimate model parameters
#'
#' @param object An `EstimationProblem`.
#' @param ... Additional method arguments.
#' @export
estimate <- function(object, ...) UseMethod("estimate")

#' @export
estimate.default <- function(object, ...) {
    stop("estimate() requires an EstimationProblem object; no method exists for class ",
         paste(class(object), collapse = "/"), ".", call. = FALSE)
}

#' @param backend An estimation backend, currently [optim_backend()].
#' @param dimensions Optional solver-facing unit dimensions passed to simulation.
#' @rdname estimate
#' @export
estimate.EstimationProblem <- function(
    object,
    backend = optim_backend(),
    dimensions = NULL,
    ...
) {
    .check_class(backend, "EstimationBackend")
    if (!inherits(backend, "OptimBackend")) {
        stop("Unsupported estimation backend: ", class(backend)[1], ".", call. = FALSE)
    }
    .estimate_optim(object, backend, dimensions = dimensions, ...)
}

.estimate_optim <- function(problem, backend, dimensions = NULL, ...) {
    specs <- problem$parameters
    coordinates <- .estimation_coordinates(specs)
    simulation_model <- if (inherits(problem$model, "ProcessModel")) {
        to_ode_model(problem$model)
    } else {
        problem$model
    }
    model_parameters <- .estimation_model_parameter_names(problem$model)

    evaluate <- function(par, diagnostics = FALSE) {
        values <- .estimation_decode(par, specs)
        runtime <- values[intersect(names(values), model_parameters)]
        runtime <- structure(runtime, class = c("Parameters", "list"))
        estimation_experiments <- lapply(unclass(problem$experiments), function(e) {
            e$parameters <- .merge_ode_parameters(e$parameters, runtime)
            e
        })
        names(estimation_experiments) <- names(problem$experiments)
        estimation_experiments <- .new_experiments(estimation_experiments)
        simulated <- simulate(
            simulation_model,
            experiment = estimation_experiments,
            dimensions = dimensions,
            ...
        )
        .estimation_likelihood(
            problem$experiments,
            simulated,
            problem$observation,
            values,
            diagnostics = diagnostics
        )
    }

    initial <- tryCatch(evaluate(coordinates$initial), error = identity)
    if (inherits(initial, "error")) {
        stop("Cannot evaluate the estimation problem at its initial values: ",
             conditionMessage(initial), call. = FALSE)
    }
    if (!is.finite(initial)) {
        stop("The estimation objective is not finite at its initial values.", call. = FALSE)
    }
    objective <- function(par) {
        if (!.estimation_coordinates_within_bounds(par, coordinates)) return(1e100)
        value <- tryCatch(evaluate(par), error = function(e) Inf)
        if (!is.finite(value)) 1e100 else value
    }
    optim_args <- list(
        par = coordinates$initial,
        fn = objective,
        method = backend$method,
        control = backend$control
    )
    if (backend$method %in% c("L-BFGS-B", "Brent")) {
        optim_args$lower <- coordinates$lower
        optim_args$upper <- coordinates$upper
    }
    raw <- do.call(stats::optim, optim_args)
    diagnostics <- evaluate(raw$par, diagnostics = TRUE)
    coefficients <- .estimation_decode(raw$par, specs)
    coefficients <- structure(coefficients, class = c("Parameters", "list"))
    structure(
        list(
            coefficients = coefficients,
            neg_log_lik = unname(raw$value),
            convergence = list(code = raw$convergence, message = raw$message %||% NULL),
            predictions = diagnostics$predictions,
            residuals = diagnostics$residuals,
            backend = backend,
            backend_result = raw,
            problem = problem
        ),
        class = "EstimationResult"
    )
}

#' Print an estimation result
#'
#' @param x An `EstimationResult` object.
#' @param ... Unused.
#' @returns `x`, invisibly.
#' @export
print.EstimationResult <- function(x, ...) {
    cat("EstimationResult:\n")
    cat(" Coefficients:\n")
    if (length(x$coefficients)) {
        cat(sprintf(
            "  %s = %s\n",
            names(x$coefficients),
            vapply(x$coefficients, format, character(1))
        ), sep = "")
    } else {
        cat("  (none)\n")
    }
    cat(" negative log-likelihood: ", format(x$neg_log_lik, digits = 7), "\n", sep = "")
    cat(" convergence code: ", x$convergence$code, "\n", sep = "")
    if (!is.null(x$convergence$message)) {
        cat(" convergence message: ", x$convergence$message, "\n", sep = "")
    }
    backend <- class(x$backend)[[1]]
    if (inherits(x$backend, "OptimBackend")) {
        backend <- paste0(backend, " (", x$backend$method, ")")
    }
    cat(" backend: ", backend, "\n", sep = "")
    invisible(x)
}

.estimation_coordinates_within_bounds <- function(par, coordinates) {
    all(par >= coordinates$lower & par <= coordinates$upper)
}

.estimation_model_parameter_names <- function(model) {
    if (inherits(model, "CompiledOdeModel")) return(model$parameterNames)
    if (inherits(model, "CompartmentModel")) model <- to_ode_model(model)
    unique(c(names(model$parameters), model$freeParams))
}

.estimation_coordinates <- function(specs) {
    decoded <- lapply(specs, function(spec) {
        initial <- as.numeric(spec$initial)
        lower <- .estimation_bound_numeric(spec$lower, spec$initial, "lower")
        upper <- .estimation_bound_numeric(spec$upper, spec$initial, "upper")
        switch(spec$transform,
            identity = c(initial = initial, lower = lower, upper = upper),
            log = c(initial = log(initial), lower = if (lower <= 0) -Inf else log(lower),
                    upper = log(upper)),
            logit = {
                scale <- upper - lower
                p <- (initial - lower) / scale
                c(initial = stats::qlogis(p), lower = -Inf, upper = Inf)
            }
        )
    })
    list(
        initial = setNames(vapply(decoded, `[[`, numeric(1), "initial"), names(specs)),
        lower = setNames(vapply(decoded, `[[`, numeric(1), "lower"), names(specs)),
        upper = setNames(vapply(decoded, `[[`, numeric(1), "upper"), names(specs))
    )
}

.estimation_decode <- function(par, specs) {
    out <- lapply(seq_along(specs), function(i) {
        spec <- specs[[i]]
        lower <- .estimation_bound_numeric(spec$lower, spec$initial, "lower")
        upper <- .estimation_bound_numeric(spec$upper, spec$initial, "upper")
        value <- switch(spec$transform,
            identity = par[[i]],
            log = exp(par[[i]]),
            logit = lower + (upper - lower) * stats::plogis(par[[i]])
        )
        if (inherits(spec$initial, "units")) {
            units::set_units(value, units::deparse_unit(spec$initial), mode = "standard")
        } else {
            value
        }
    })
    setNames(out, names(specs))
}

.estimation_likelihood <- function(experiments, simulated, observation, parameters,
                                   diagnostics = FALSE) {
    if (!inherits(simulated, "list") || inherits(simulated, "SimulationResult")) {
        simulated <- list(simulated)
    }
    objective <- 0
    prediction_rows <- list()
    residual_rows <- list()
    experiment_names <- names(experiments)
    for (i in seq_along(experiments)) {
        observed <- experiments[[i]]$observations
        predicted <- simulated[[i]]$observables
        if (nrow(observed) != nrow(predicted) ||
            !identical(observed$observable, predicted$observable)) {
            stop("Simulation predictions do not match the observation rows.", call. = FALSE)
        }
        label <- if (!is.null(experiment_names) && nzchar(experiment_names[[i]])) {
            experiment_names[[i]]
        } else {
            as.character(i)
        }
        for (j in seq_len(nrow(observed))) {
            obs <- observed$value[[j]]
            pred <- predicted$value[[j]]
            if (diagnostics) {
                aligned <- .estimation_align_values(obs, pred, "observed value")
                prediction_rows[[length(prediction_rows) + 1L]] <- list(
                    experiment = label, time = observed$time[[j]],
                    observable = observed$observable[[j]],
                    value = .estimation_restore_unit(aligned$predicted, aligned$unit)
                )
                residual_rows[[length(residual_rows) + 1L]] <- list(
                    experiment = label, time = observed$time[[j]],
                    observable = observed$observable[[j]],
                    value = .estimation_restore_unit(
                        aligned$observed - aligned$predicted, aligned$unit
                    )
                )
            }
            if (length(obs) != 1L || is.na(obs)) next
            error <- observation[[observed$observable[[j]]]]
            contribution <- .estimation_error_nll(obs, pred, error, parameters)
            objective <- objective + contribution$nll
        }
    }
    if (!diagnostics) return(objective)
    list(
        objective = objective,
        predictions = .estimation_bind_diagnostic_rows(prediction_rows),
        residuals = .estimation_bind_diagnostic_rows(residual_rows)
    )
}

.estimation_restore_unit <- function(value, unit) {
    if (!nzchar(unit)) return(value)
    units::set_units(value, unit, mode = "standard")
}

.estimation_bind_diagnostic_rows <- function(rows) {
    if (!length(rows)) return(observation_data())
    time <- .estimation_combine_times(lapply(rows, `[[`, "time"))
    value <- .estimation_combine_values(lapply(rows, `[[`, "value"))
    observation_data(
        time = time,
        observable = vapply(rows, `[[`, character(1), "observable"),
        value = value,
        experiment = vapply(rows, `[[`, character(1), "experiment")
    )
}

.estimation_combine_times <- function(values) {
    has_units <- vapply(values, inherits, logical(1), "units")
    if (!any(has_units)) return(vapply(values, as.numeric, numeric(1)))
    if (!all(has_units)) {
        stop("Estimation diagnostic times must consistently use time units.", call. = FALSE)
    }
    target <- units::deparse_unit(values[[1]])
    numeric <- vapply(values, function(x) {
        as.numeric(units::set_units(x, target, mode = "standard"))
    }, numeric(1))
    units::set_units(numeric, target, mode = "standard")
}

.estimation_combine_values <- function(values) {
    labels <- vapply(values, function(x) {
        if (inherits(x, "units")) units::deparse_unit(x) else ""
    }, character(1))
    numeric <- vapply(values, as.numeric, numeric(1))
    unique_labels <- unique(labels)
    if (identical(unique_labels, "")) return(numeric)
    if (length(unique_labels) == 1L) {
        return(units::set_units(numeric, unique_labels, mode = "standard"))
    }
    units::mixed_units(numeric, ifelse(nzchar(labels), labels, "1"))
}

.estimation_error_nll <- function(observed, predicted, error, parameters) {
    aligned <- .estimation_align_values(observed, predicted, "observed value")
    obs <- aligned$observed
    pred <- aligned$predicted
    unit <- aligned$unit
    if (inherits(error, "AdditiveError")) {
        sd <- .estimation_scale_parameter(parameters[[error$sigma]], predicted, error$sigma)
        nll <- .estimation_normal_nll(obs, pred, sd)
    } else if (inherits(error, "ProportionalError")) {
        prop <- .estimation_dimensionless_parameter(parameters[[error$sigma]], error$sigma)
        sd <- prop * abs(pred)
        nll <- .estimation_normal_nll(obs, pred, sd)
    } else if (inherits(error, "CombinedError")) {
        add <- .estimation_scale_parameter(parameters[[error$additive]], predicted, error$additive)
        prop <- .estimation_dimensionless_parameter(
            parameters[[error$proportional]], error$proportional
        )
        sd <- add + prop * abs(pred)
        nll <- .estimation_normal_nll(obs, pred, sd)
    } else if (inherits(error, "LognormalError")) {
        sigma <- .estimation_dimensionless_parameter(parameters[[error$sigma]], error$sigma)
        if (obs <= 0 || pred <= 0 || sigma <= 0) return(list(nll = Inf))
        nll <- log(obs) + log(sigma) + 0.5 * log(2 * pi) +
            0.5 * ((log(obs) - log(pred)) / sigma)^2
    } else {
        stop("Unsupported observation error model.", call. = FALSE)
    }
    list(nll = nll, predicted = pred, residual = obs - pred, unit = unit)
}

.estimation_align_values <- function(observed, predicted, label) {
    obs_units <- inherits(observed, "units")
    pred_units <- inherits(predicted, "units")
    if (obs_units != pred_units) {
        stop(label, " and prediction must both be unit-free or have compatible units.", call. = FALSE)
    }
    unit <- ""
    if (pred_units) {
        unit <- units::deparse_unit(predicted)
        observed <- tryCatch(
            units::set_units(observed, unit, mode = "standard"),
            error = function(e) stop(label, " and prediction have incompatible units.", call. = FALSE)
        )
    }
    list(observed = as.numeric(observed), predicted = as.numeric(predicted), unit = unit)
}

.estimation_scale_parameter <- function(x, predicted, name) {
    if (is.null(x)) stop("Missing observation error parameter: ", name, ".", call. = FALSE)
    x_units <- inherits(x, "units")
    pred_units <- inherits(predicted, "units")
    if (x_units != pred_units) {
        stop("Observation error parameter '", name,
             "' must have the same unit mode as its observable.", call. = FALSE)
    }
    if (pred_units) {
        x <- tryCatch(
            units::set_units(x, units::deparse_unit(predicted), mode = "standard"),
            error = function(e) stop("Observation error parameter '", name,
                "' has incompatible units.", call. = FALSE)
        )
    }
    as.numeric(x)
}

.estimation_dimensionless_parameter <- function(x, name) {
    if (is.null(x)) stop("Missing observation error parameter: ", name, ".", call. = FALSE)
    if (inherits(x, "units")) {
        x <- tryCatch(
            units::set_units(x, "1", mode = "standard"),
            error = function(e) stop("Observation error parameter '", name,
                "' must be dimensionless.", call. = FALSE)
        )
    }
    as.numeric(x)
}

.estimation_normal_nll <- function(observed, predicted, sd) {
    if (!is.finite(sd) || sd <= 0) return(Inf)
    log(sd) + 0.5 * log(2 * pi) + 0.5 * ((observed - predicted) / sd)^2
}

#' Extract estimated coefficients
#' @param object An `EstimationResult`.
#' @param ... Unused.
#' @returns Estimated coefficients as a `Parameters` object.
#' @export
coef.EstimationResult <- function(object, ...) object$coefficients

#' Extract fitted observable values
#' @param object An `EstimationResult`.
#' @param ... Unused.
#' @returns Unit-aware `ObservationData` of fitted values in estimation-row order.
#' @export
fitted.EstimationResult <- function(object, ...) object$predictions

#' Extract estimation residuals
#' @param object An `EstimationResult`.
#' @param ... Unused.
#' @returns Unit-aware `ObservationData` of observed-minus-fitted residuals.
#' @export
residuals.EstimationResult <- function(object, ...) object$residuals
