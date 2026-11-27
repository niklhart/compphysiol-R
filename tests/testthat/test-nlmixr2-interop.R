test_that("native export/import likelihoods agree with analytic marginal densities", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    for (family in c("normal","lognormal")) {
        x <- interop_fixture(family)
        fit <- from_nlmixr2(x$fit,x$export)
        expect_equal(fit$neg_log_lik, independent_interop_nll(family), tolerance = 1e-4)
        expect_identical(fit$likelihood_method,"focei")
        sampled <- from_nlmixr2(x$fit,x$export,likelihood="impmap",
                               likelihood_control=list(isample=1000L,impSeed=42L))
        expect_equal(sampled$neg_log_lik, independent_interop_nll(family), tolerance=1e-3)
        expect_identical(sampled$likelihood_method,"impmap")
        expect_true(all(is.na(vapply(fit$standard_errors, as.numeric, numeric(1)))))
        native <- sampled$likelihood_result
        wrong_reference <- unserialize(serialize(x$fit,NULL))
        assign("omega", x$fit$omega*2, wrong_reference$env)
        expect_error(from_nlmixr2(native,x$export,likelihood="impmap",
                                 reference_fit=wrong_reference),"parameters")
        expect_error(from_nlmixr2(native,x$export),"Ambiguous")
        expect_error(from_nlmixr2(native,x$export,likelihood="impmap"),"reference_fit")
        imported <- from_nlmixr2(native,x$export,likelihood="impmap",reference_fit=x$fit,
                                likelihood_control=list(isample=1000L))
        expect_equal(coef(imported),coef(fit))
    }
})

test_that("import rejects incompatible native edits without mutating shared fits", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    x <- interop_fixture()
    edited <- unserialize(serialize(x$fit,NULL))
    env <- edited$env
    data <- get("origData",env)
    data$DV[1] <- 999
    assign("origData",data,env)
    expect_error(from_nlmixr2(edited,x$export),"data")
    expect_equal(x$fit$origData$DV[1],9)
    edited <- unserialize(serialize(x$fit,NULL))
    assign("ofvType","gauss3_1.6",edited$env)
    expect_error(from_nlmixr2(edited,x$export),"metadata")
    expect_error(from_nlmixr2(x$fit,x$export,likelihood="impmap",
                             likelihood_control=list(nIter=1)),"evaluation-only")
})

test_that("SAEM rejects unverified bounds and supports absent or explicit likelihood", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    # Reuse the established mixed-effects fixture definition without executing tests.
    problem <- nlmixr2_test_problem()
    expect_error(to_nlmixr2(problem,method="saem"),"constraint")
    problem$parameters <- c(CL_pop=parameter_spec(.2,transform="log"),
        omega_CL=parameter_spec(.2,transform="log"),sigma=parameter_spec(2,transform="log"))
    engine <- nlmixr2_engine("saem",control=list(seed=99L,nBurn=30L,nEm=50L,print=0L,covMethod=""))
    fit <- suppressMessages(estimate(problem,engine))
    expect_true(is.na(fit$neg_log_lik))
    expect_true(is.na(fit$likelihood_method))
    expect_match(fit$likelihood_reason,"explicit")
    expect_identical(fit$engine_result$env$ofvType,"foce")
    expect_true(is.na(get("objective",fit$engine_result$env,inherits=FALSE)))
    export <- to_nlmixr2(problem,method="saem")
    explicit <- suppressMessages(from_nlmixr2(fit$engine_result,export,likelihood="impmap",
        likelihood_control=list(isample=1000L)))
    expect_identical(explicit$estimation_method,"saem")
    expect_identical(explicit$likelihood_method,"impmap")
    expect_equal(coef(explicit),coef(fit))
    expect_true(is.finite(explicit$neg_log_lik))
})

test_that("sampling normalization counts responses rather than events and missing records", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    x <- interop_fixture()
    exported <- x$export
    exported$data$DV[3] <- NA_real_
    event <- exported$data[1,,drop=FALSE]
    event$TIME <- 0
    event$EVID <- 2L
    event$DV <- NA_real_
    exported$data <- rbind(event,exported$data)
    evaluated <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"impmap",
        control=nlmixr2est::impmapControl(nIter=0,isample=1000L,print=0,covMethod="")))
    target <- sum(vapply(list(c(9,10.5),c(8.5,9.5,10)),function(y) {
        n <- length(y); v <- diag(4,n)+matrix(1,n,n); r <- y-10
        as.numeric((n*log(2*pi)+determinant(v,logarithm=TRUE)$modulus+t(r)%*%solve(v,r))/2)
    },numeric(1)))
    expect_equal(.nlmixr2_sampling_nll(evaluated,exported),target,tolerance=1e-4)
})
