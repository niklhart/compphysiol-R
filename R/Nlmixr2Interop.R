.nlmixr2_named_control <- function(x) {
    if (!is.list(x) || (length(x) && (is.null(names(x)) ||
        anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))))) {
        stop("Likelihood control must be a uniquely named list.", call. = FALSE)
    }
    x
}

.nlmixr2_signature <- function(ui) {
    fields <- c("ntheta", "neta1", "neta2", "name", "lower", "upper", "fix")
    parameters <- ui$iniDf[, fields, drop = FALSE]
    parameters$fixed_value <- ifelse(parameters$fix, ui$iniDf$est, NA_real_)
    list(model = ui$model, parameters = parameters)
}

.nlmixr2_saem_scope <- function(problem) {
    individual <- .nlmixr2_individual_targets(problem$statistics)
    if (!length(individual)) stop("SAEM requires individual random effects.", call. = FALSE)
    spreads <- unlist(lapply(problem$statistics[individual], function(d) {
        value <- .nlmixr2_scale_name(d)
        if (is.character(value)) value else character()
    }), use.names = FALSE)
    normal_locations <- character()
    bounded_locations <- list()
    for (target in individual) {
        d <- problem$statistics[[target]]
        if (inherits(d,"compphysiol_normal_distribution") && is.character(d$mean))
            normal_locations <- c(normal_locations,d$mean)
        if (inherits(d, "compphysiol_logitnormal_distribution") && is.character(d$median))
            bounded_locations[[d$median]] <- d
    }
    for (name in names(problem$parameters)) {
        spec <- problem$parameters[[name]]
        if (name %in% spreads) {
            if (as.numeric(spec$lower) != 0 || !is.infinite(as.numeric(spec$upper)))
                stop("SAEM does not support finite variability constraints.", call. = FALSE)
            next
        }
        if (name %in% .nlmixr2_correlation_parameters(problem)) next
        if (name %in% normal_locations) {
            if (spec$transform != "identity" || !is.infinite(as.numeric(spec$lower)) ||
                !is.infinite(as.numeric(spec$upper)))
                stop("SAEM normal locations require an unbounded identity domain.", call. = FALSE)
            next
        }
        if (name %in% names(bounded_locations)) {
            d <- bounded_locations[[name]]
            if (spec$transform != "logit" ||
                !isTRUE(all.equal(spec$lower, d$lower)) || !isTRUE(all.equal(spec$upper, d$upper)))
                stop("SAEM logitnormal medians require a matching logit parameter domain.", call. = FALSE)
            next
        }
        residual <- name %in% .nlmixr2_residual_parameters(problem)
        natural <- if (residual) as.numeric(spec$lower) == 0 && is.infinite(as.numeric(spec$upper)) else
            spec$transform == "log" && as.numeric(spec$lower) == 0 && is.infinite(as.numeric(spec$upper))
        if (!natural) stop("SAEM does not support this parameter transformation or finite constraint: '",
                           name, "'. Use a verified natural parameter domain.", call. = FALSE)
    }
    for (target in .nlmixr2_observation_targets(problem$statistics)) {
        scale <- .nlmixr2_scale_name(problem$statistics[[target]])
        if (!is.character(scale) || length(scale) != 1L ||
            inherits(scale, c("compphysiol_proportional_sd", "compphysiol_combined_sd")))
            stop("SAEM currently requires a named, directly estimated residual SD.", call. = FALSE)
    }
}

.nlmixr2_residual_parameters <- function(problem) {
    unique(unlist(lapply(problem$statistics[.nlmixr2_observation_targets(problem$statistics)], function(x) {
        s <- .nlmixr2_scale_name(x)
        if (is.character(s) && !inherits(s, c("compphysiol_proportional_sd", "compphysiol_combined_sd"))) s else character()
    }), use.names = FALSE))
}

#' Export an estimation problem for native nlmixr2 execution
#'
#' The S3 export retains native inputs and the scientific and unit mappings needed
#' for guarded import. Ordinary base R serialization is supported within a session
#' environment; cross-version compatibility is not guaranteed.
#' The `control` field contains required native control defaults. Normal SAEM
#' execution requires `handleUninformativeEtas = FALSE`; import verifies it.
#' Fixed nonzero correlations and singular covariance blocks are rejected.
#' Unspecified correlations remain fixed zeros, not free initial values.
#' @param problem An estimation problem.
#' @param method Target fitting method, `"focei"` or `"saem"`.
#' @param dimensions Solver unit dimensions.
#' @return A `compphysiol_nlmixr2_export` with `model`, `source`, and `data`.
#' @template nlmixr2-saem-example
#' @export
to_nlmixr2 <- function(problem, method = c("focei", "saem"), dimensions = NULL) {
    .check_class(problem, "compphysiol_estimation_problem")
    method <- match.arg(method)
    decoding <- problem
    if (method == "saem") {
        .nlmixr2_saem_scope(problem)
        for (name in .nlmixr2_residual_parameters(problem))
            decoding$parameters[[name]]$transform <- "identity"
    }
    translated <- .nlmixr2_prepare(decoding, dimensions)
    if (method == "saem") {
        # Direct residual coordinates allow native SAEM residual recognition.
        for (name in .nlmixr2_residual_parameters(problem)) {
            helper <- .nlmixr2_parameter_id(translated$executable, name)
            translated$source <- gsub(paste0("add(", helper, ")"),
                paste0("add(", translated$theta_ids[[name]], ")"), translated$source, fixed = TRUE)
            translated$source <- gsub(paste0("lnorm(", helper, ")"),
                paste0("lnorm(", translated$theta_ids[[name]], ")"), translated$source, fixed = TRUE)
        }
        translated$model <- eval(parse(text = translated$source))
    }
    ui <- nlmixr2est::nlmixr2(translated$model)
    .new_nlmixr2_export(c(translated, list(problem = problem, decoding_problem = decoding,
        method = method, control = if (method == "saem" &&
            any(vapply(problem$statistics, inherits, logical(1), "compphysiol_normal_distribution") &
                vapply(problem$statistics, function(d) identical(d$level,"individual"),logical(1))))
                    list(handleUninformativeEtas = FALSE) else list(),
        signature = .nlmixr2_signature(ui))))
}

.nlmixr2_compatible <- function(fit, exported) {
    if (!inherits(fit, "nlmixr2FitCore")) stop("Expected a native nlmixr2 fit.", call. = FALSE)
    if (!isTRUE(all.equal(.nlmixr2_signature(fit$finalUi), exported$signature, tolerance = 1e-8)))
        stop("Native model or parameter signature is incompatible with the export.", call. = FALSE)
    actual <- fit$origData
    if (!is.data.frame(actual) || !isTRUE(all.equal(actual, exported$data, check.attributes = FALSE)))
        stop("Native data or observation mappings are incompatible with the export.", call. = FALSE)
    if (!identical(names(actual), names(exported$data)))
        stop("Native data columns are incompatible with the export.", call. = FALSE)
    if (!all(is.finite(fit$theta)) || !all(is.finite(fit$omega)))
        stop("Native population parameters must be finite.", call. = FALSE)
}

.nlmixr2_evaluate <- function(fit, method, control = list()) {
    control <- .nlmixr2_named_control(control)
    fixed <- if (method == "impmap") list(nIter = 0L) else list(maxOuterIterations = 0L)
    if (any(names(control) %in% c(names(fixed), "calcTables", "covMethod", "etaMat")))
        stop("Likelihood controls cannot change evaluation-only execution or required tables.", call. = FALSE)
    constructor <- if (method == "impmap") nlmixr2est::impmapControl else nlmixr2est::foceiControl
    evaluated <- nlmixr2est::nlmixr2(fit, fit$origData, est = method,
        control = do.call(constructor, c(list(print = 0L, covMethod = ""), fixed,
                                        control[setdiff(names(control), "print")])) )
    if (!isTRUE(all.equal(fit$theta, evaluated$theta)) || !isTRUE(all.equal(fit$omega, evaluated$omega)))
        stop("Likelihood evaluation changed population parameters.", call. = FALSE)
    evaluated
}

#' Import a compatible native nlmixr2 fit
#'
#' Auto reports FOCEi likelihood for plain FOCEi fits, no likelihood for plain
#' SAEM fits, and requires an explicit choice after IMPMAP. Quadrature is not
#' supported. Explicit evaluation preserves population estimates. Missing
#' covariance is allowed; valid prediction tables are required.
#' @param fit Native fit.
#' @param exported Object returned by [to_nlmixr2()].
#' @param likelihood `"auto"`, `"focei"`, or `"impmap"`.
#' @param reference_fit Preceding native fit, required for importing an IMPMAP stage.
#' @param likelihood_control Controls for an explicitly requested evaluation.
#' @return An estimation result with likelihood method and estimation provenance.
#' @template nlmixr2-saem-example
#' @export
from_nlmixr2 <- function(fit, exported, likelihood = c("auto", "focei", "impmap"),
                         reference_fit = NULL, likelihood_control = list()) {
    .check_class(exported, "compphysiol_nlmixr2_export")
    likelihood <- match.arg(likelihood)
    likelihood_control <- .nlmixr2_named_control(likelihood_control)
    fit <- unserialize(serialize(fit, NULL))
    .nlmixr2_compatible(fit, exported)
    env <- fit$env
    cached <- get0("logLik", env, inherits = FALSE)
    if (length(cached) == 1L && is.finite(as.numeric(cached))) {
        label <- get0("ofvType", env, inherits = FALSE)
        table <- get0("objDf", env, inherits = FALSE)
        row <- if (is.character(label) && length(label) == 1L)
            which(tolower(rownames(table)) == tolower(label)) else integer()
        if (length(row) != 1L || !isTRUE(all.equal(as.numeric(cached),
            table[["Log-likelihood"]][row])))
            stop("Inconsistent native likelihood metadata.", call. = FALSE)
    }
    method <- fit$est
    if (!method %in% c("focei", "saem", "impmap")) stop("Unsupported native estimation method.", call. = FALSE)
    sampling <- identical(method, "impmap") || "IMPMAP" %in% toupper(rownames(fit$objDf))
    if (sampling && likelihood == "auto")
        stop("Ambiguous likelihood after IMPMAP: choose 'focei' or 'impmap'.", call. = FALSE)
    if (sampling) {
        if (identical(fit$est, "impmap") && !isTRUE(fit$impEonly))
            stop("IMPMAP import requires an evaluation-only stage.", call. = FALSE)
        if (is.null(reference_fit)) stop("IMPMAP import requires reference_fit.", call. = FALSE)
        .nlmixr2_compatible(reference_fit, exported)
        if (!reference_fit$est %in% c("focei", "saem"))
            stop("reference_fit must be a preceding FOCEi or SAEM estimate.", call. = FALSE)
        if (!isTRUE(all.equal(reference_fit$theta, fit$theta)) || !isTRUE(all.equal(reference_fit$omega, fit$omega)))
            stop("Evaluation population parameters differ from reference_fit.", call. = FALSE)
        method <- reference_fit$est
    }
    control_fit <- if (sampling) reference_fit else fit
    if (method == "saem" && length(exported$control) &&
        !identical(control_fit$saemControl$handleUninformativeEtas, FALSE))
        stop("Normal SAEM import requires handleUninformativeEtas = FALSE; use exported$control.", call. = FALSE)
    if (method == "saem" && exported$method != "saem")
        stop("SAEM requires an export prepared for SAEM.", call. = FALSE)
    tab <- tryCatch(as.data.frame(fit), error = function(e) NULL)
    if (is.null(tab) || !all(c("ID", "TIME", "DV", "IPRED") %in% names(tab)))
        stop("Import requires native prediction tables.", call. = FALSE)
    obs <- exported$data[exported$data$EVID == 0L, ]
    if (nrow(tab) != nrow(obs) || !isTRUE(all.equal(as.numeric(tab$ID), as.numeric(obs$ID))) ||
        !isTRUE(all.equal(tab$TIME, obs$TIME)) || !isTRUE(all.equal(tab$DV, obs$DV)))
        stop("Native prediction rows do not match exported observations.", call. = FALSE)
    selected <- if (likelihood == "auto") if (method == "saem") NA_character_ else "focei" else likelihood
    nll <- NA_real_
    evaluated <- fit
    if (!is.na(selected)) {
        if (likelihood != "auto") evaluated <- .nlmixr2_evaluate(fit, selected, likelihood_control)
        if (selected == "focei") {
            ll <- as.numeric(stats::logLik(evaluated))
            label <- nlmixr2est::getOfvType(evaluated)
            row <- which(tolower(rownames(evaluated$objDf)) == tolower(label))
            if (!(tolower(label) == "focei" || (tolower(label) == "pop" &&
                !length(.nlmixr2_individual_targets(exported$problem$statistics)))) ||
                length(row) != 1L || !is.finite(ll) ||
                !isTRUE(all.equal(ll, evaluated$objDf[["Log-likelihood"]][row])))
                stop("Inconsistent or unsupported native likelihood metadata.", call. = FALSE)
            nll <- -ll
        } else {
            nll <- .nlmixr2_sampling_nll(evaluated, exported)
        }
        if (length(nll) != 1L || !is.finite(nll)) stop("Likelihood evaluation is not finite.", call. = FALSE)
        nll <- nll + exported$log_likelihood_unit_jacobian
    }
    # Rebuild SAEM tables in public native coordinates without invoking quadrature.
    predictions <- if (method == "saem") .nlmixr2_evaluate(fit, "focei") else evaluated
    result <- .nlmixr2_result(exported$decoding_problem, nlmixr2_engine(method),
                              predictions, exported, likelihood_value = nll)
    uncertainty <- .nlmixr2_uncertainty(exported$decoding_problem, fit, exported, result$coefficients)
    result$standard_errors <- uncertainty$standard_errors
    result$estimate_correlations <- uncertainty$correlations
    result$problem <- exported$problem
    result$engine_result <- fit
    result$likelihood_result <- if (!is.na(selected)) evaluated else NULL
    result$estimation_method <- method
    result$likelihood_method <- selected
    result$likelihood_native_label <- if (!is.na(selected) && selected == "focei")
        nlmixr2est::getOfvType(evaluated) else selected
    result$likelihood_reason <- if (is.na(selected)) "SAEM requires explicit likelihood evaluation." else NULL
    result
}

.nlmixr2_sampling_nll <- function(fit, exported) {
    obs <- exported$data[exported$data$EVID == 0L, ]
    targets <- exported$observation[obs$DVID]
    if (anyNA(targets)) stop("Unknown endpoint in sampling likelihood.", call. = FALSE)
    lognormal <- vapply(targets, function(n) inherits(exported$problem$statistics[[n]],
        "compphysiol_lognormal_distribution"), logical(1))
    observed <- !is.na(obs$DV)
    fit$impObj / 2 + sum(observed) * log(2*pi) / 2 +
        sum(log(obs$DV[observed & lognormal]))
}

.new_nlmixr2_export <- function(x) structure(x, class = "compphysiol_nlmixr2_export")
