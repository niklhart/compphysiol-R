# Covariance entries are native coordinates; public correlations are derived.
.nlmixr2_correlation_parameters <- function(problem) {
    pairs <- .statistical_correlations(problem$statistics)
    vapply(Filter(function(p) is.character(p$value), pairs), `[[`, character(1), "value")
}

.nlmixr2_omega <- function(problem, dimensions, eta_ids) {
    targets <- names(eta_ids)
    if (!length(eta_ids)) return(list(source = character(), correlations = list(),
        covariance = matrix(numeric(),0,0), free = matrix(logical(),0,0)))
    sd <- vapply(targets, function(target) {
        value <- .nlmixr2_scale_name(problem$statistics[[target]])
        if (is.character(value)) value <- problem$parameters[[value]]$initial
        .nlmixr2_solver_value(value, dimensions)
    }, numeric(1))
    if (any(!is.finite(sd) | sd <= 0)) stop("nlmixr2 requires positive individual spreads.", call. = FALSE)
    matrix <- diag(sd^2, length(sd))
    dimnames(matrix) <- list(targets, targets)
    free <- matrix(FALSE, length(sd), length(sd), dimnames = list(targets, targets))
    diag(free) <- vapply(targets, function(t) is.character(.nlmixr2_scale_name(problem$statistics[[t]])), logical(1))
    adjacency <- diag(TRUE, length(sd))
    correlation_map <- list()
    for (pair in .statistical_correlations(problem$statistics)) {
        value <- pair$value
        estimated <- is.character(value)
        if (estimated) {
            spec <- problem$parameters[[value]]
            if (as.numeric(spec$lower) != -1 || as.numeric(spec$upper) != 1)
                stop("Estimated correlations require the full (-1, 1) domain.", call. = FALSE)
            rho <- as.numeric(spec$initial)
            correlation_map[[value]] <- c(pair$first, pair$second)
        } else {
            rho <- as.numeric(value)
            if (rho != 0) stop("Fixed nonzero correlations are unsupported for nlmixr2 estimation.", call. = FALSE)
        }
        i <- match(pair$first, targets); j <- match(pair$second, targets)
        matrix[i,j] <- matrix[j,i] <- rho * sd[i] * sd[j]
        free[i,j] <- free[j,i] <- estimated
        adjacency[i,j] <- adjacency[j,i] <- estimated
    }
    if (length(sd) && inherits(try(chol(matrix), silent = TRUE), "try-error"))
        stop("nlmixr2 estimation requires a positive-definite random-effect covariance.", call. = FALSE)
    remaining <- seq_along(sd); source <- character()
    while (length(remaining)) {
        group <- remaining[1]
        repeat {
            next_group <- which(rowSums(adjacency[,group,drop=FALSE]) > 0)
            if (setequal(group, next_group)) break
            group <- next_group
        }
        remaining <- setdiff(remaining, group)
        entries <- character()
        for (i in seq_along(group)) for (j in seq_len(i)) {
            value <- .nlmixr2_number(matrix[group[i],group[j]])
            if (!free[group[i],group[j]]) value <- paste0("fix(",value,")")
            entries <- c(entries, value)
        }
        source <- c(source, paste0(paste(eta_ids[group],collapse=" + "), " ~ ",
            if(length(entries)==1L) entries else paste0("c(",paste(entries,collapse=", "),")")))
    }
    list(source = source, correlations = correlation_map, covariance = matrix, free = free)
}

.nlmixr2_correlated_uncertainty <- function(problem, covariance, theta, omega, translated, coefficients) {
    specs <- problem$parameters
    missing <- .estimation_public_uncertainty(NULL, numeric(), specs)
    if (!is.matrix(covariance) || is.null(rownames(covariance))) return(missing)
    # Native full-covariance names are verified through the available matrix;
    # unavailable omega coordinates leave the corresponding public SE missing.
    gradients <- list()
    for (name in names(specs)) {
        spec <- specs[[name]]
        scale <- .nlmixr2_public_unit_scale(spec, translated$dimensions)
        target <- names(translated$omega_parameters)[which(translated$omega_parameters == name)]
        if (length(target)) {
            eta <- translated$eta_ids[[target[1]]]
            gradients[[name]] <- setNames(scale / (2*sqrt(omega[eta,eta])), paste0("om.",eta))
        } else if (name %in% names(translated$correlation_parameters)) {
            pair <- unname(translated$eta_ids[translated$correlation_parameters[[name]]])
            v1 <- omega[pair[1],pair[1]]; v2 <- omega[pair[2],pair[2]]
            rho <- as.numeric(coefficients[[name]])
            # nlmixr2est .foceiOmegaCovNames uses cov.<eta1>.<eta2>.
            candidates <- c(paste0("cov.",pair[1],".",pair[2]), paste0("cov.",pair[2],".",pair[1]))
            off <- intersect(candidates, rownames(covariance))
            if (length(off) != 1L) next
            diagonal <- setNames(c(-rho/(2*v1),-rho/(2*v2)),paste0("om.",pair))
            targets <- translated$correlation_parameters[[name]]
            diagonal <- diagonal[!is.na(translated$omega_parameters[targets])]
            gradients[[name]] <- c(diagonal,setNames(1/sqrt(v1*v2),off))
        } else {
            id <- translated$theta_ids[[name]]
            derivative <- switch(spec$transform, identity=scale, log=as.numeric(coefficients[[name]]),
                logit={b <- .nlmixr2_theta_bounds(spec,translated$dimensions)
                    p <- stats::plogis(theta[[id]])
                    scale*(b['upper']-b['lower'])*p*(1-p)})
            gradients[[name]] <- setNames(as.numeric(derivative),id)
        }
    }
    gradients <- Filter(function(g) all(names(g) %in% rownames(covariance)) &&
        all(names(g) %in% colnames(covariance)) && all(is.finite(g)), gradients)
    if (!length(gradients)) return(missing)
    coordinates <- unique(unlist(lapply(gradients,names)))
    jacobian <- matrix(0,length(gradients),length(coordinates),dimnames=list(names(gradients),coordinates))
    for (name in names(gradients)) jacobian[name,names(gradients[[name]])] <- gradients[[name]]
    public <- jacobian %*% covariance[coordinates,coordinates,drop=FALSE] %*% t(jacobian)
    .estimation_public_uncertainty(public,setNames(rep(1,nrow(public)),rownames(public)),specs)
}
