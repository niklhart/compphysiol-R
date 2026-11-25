extension_problem <- function(logit = FALSE, rho = "rho") {
    model <- compartment_model() |>
        add_compartment(name="Central",volume=1) |>
        add_molecule(name="drug",cmt="Central",initial=1,type="amount") |>
        add_transport(from="Central",to=NULL,molec="drug",rate="A * a[drug, Central]") |>
        add_observable(C=A+B)
    studies <- c(one=experiment(observations=observation_data(time=c(0,.5,1),observable="C",value=c(3,3.2,3.6))),
                 two=experiment(observations=observation_data(time=c(0,.5,1),observable="C",value=c(2.8,3.1,3.4))))
    specs <- c(ma=parameter_spec(if(logit) .4 else 3,
        transform=if(logit) "logit" else "identity"),mb=parameter_spec(.5,transform=if(logit) "log" else "identity"),
        sa=parameter_spec(.3,transform="log"),sb=parameter_spec(.2,transform="log"),sigma=parameter_spec(.1,transform="log"))
    if(is.character(rho)) specs <- c(specs,rho=parameter_spec(.4,lower=-1,upper=1,transform="logit"))
    estimation_problem(model,studies,specs,statistical_model(
        A=if(logit) logitnormal(median="ma",sdlogit="sa") else normal(mean="ma",sd="sa"),
        B=if(logit) lognormal(median="mb",sdlog="sb") else normal(mean="mb",sd="sb"), C=normal(sd="sigma"), correlated(list("A","B",rho))))
}

test_that("correlated normal export/import agrees with a multivariate Gaussian density", {
    skip_if_not_installed("nlmixr2est")
    problem <- extension_problem()
    exported <- to_nlmixr2(problem)
    native <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"focei",
        control=nlmixr2est::foceiControl(maxOuterIterations=0,print=0,covMethod="")))
    result <- from_nlmixr2(native,exported)
    expect_equal(as.numeric(coef(result)$rho),.4,tolerance=1e-6)
    reference <- sum(vapply(list(c(3,3.2,3.6),c(2.8,3.1,3.4)),function(y) {
        X <- matrix(1,3,2); omega <- matrix(c(.09,.024,.024,.04),2)
        V <- X%*%omega%*%t(X)+diag(.01,3); r <- y-drop(X%*%c(3,.5))
        as.numeric((3*log(2*pi)+determinant(V,logarithm=TRUE)$modulus+t(r)%*%solve(V,r))/2)
    },numeric(1)))
    expect_equal(result$neg_log_lik,reference,tolerance=1e-4)
    sampled <- from_nlmixr2(native,exported,likelihood="impmap",likelihood_control=list(isample=1000L))
    expect_equal(sampled$neg_log_lik,reference,tolerance=.001)
    expect_error(to_nlmixr2(extension_problem(rho=.4)),"Fixed nonzero")
    expect_identical(to_nlmixr2(problem,method="saem")$control,
                     list(handleUninformativeEtas=FALSE))
})

test_that("logit-normal individuals translate to bounded latent-normal expressions", {
    skip_if_not_installed("nlmixr2est")
    exported <- to_nlmixr2(extension_problem(logit=TRUE))
    expect_match(exported$source,"cp_latent_")
    native <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"focei",
        control=nlmixr2est::foceiControl(maxOuterIterations=0,print=0,covMethod="")))
    result <- from_nlmixr2(native,exported)
    expect_equal(as.numeric(coef(result)$ma),.4,tolerance=1e-6)
    expect_equal(as.numeric(coef(result)$sa),.3,tolerance=1e-6)
    expect_equal(as.numeric(coef(result)$rho),.4,tolerance=1e-6)
    expect_true(is.finite(result$neg_log_lik))
})


test_that("SAEM wraps correlated lognormal and bounded logit-normal individuals", {
    skip_if_not_installed("nlmixr2est")
    fit <- suppressMessages(estimate(extension_problem(logit=TRUE),nlmixr2_engine("saem",
        control=list(nBurn=30L,nEm=50L,print=0L,covMethod=""),likelihood="impmap",
        likelihood_control=list(isample=500L))))
    expect_true(is.finite(fit$neg_log_lik))
    expect_true(as.numeric(coef(fit)$ma)>0 && as.numeric(coef(fit)$ma)<1)
    expect_true(abs(as.numeric(coef(fit)$rho))<1)
    expect_identical(fit$likelihood_method,"impmap")
})

test_that("correlation uncertainty uses covariance and both marginal variances", {
    p <- extension_problem()
    names <- c("om.e1","om.e2","cov.e1.e2")
    covariance <- diag(c(.0001,.0001,.00001))
    dimnames(covariance) <- list(names,names)
    omega <- matrix(c(.09,.024,.024,.04),2,dimnames=list(c("e1","e2"),c("e1","e2")))
    translated <- list(dimensions=list(),omega_parameters=c(A="sa",B="sb"),
        eta_ids=c(A="e1",B="e2"),correlation_parameters=list(rho=c("A","B")),
        theta_ids=c(ma="t1",mb="t2",sigma="t3"))
    coefs <- list(ma=3,mb=.5,sa=.3,sb=.2,sigma=.1,rho=.4)
    result <- .nlmixr2_correlated_uncertainty(p,covariance,c(t1=3,t2=.5,t3=log(.1)),omega,translated,coefs)
    f <- function(x) x[3]/sqrt(x[1]*x[2])
    x <- c(.09,.04,.024); h <- 1e-6
    gradient <- vapply(1:3,function(i){a<-b<-x;a[i]<-a[i]+h;b[i]<-b[i]-h;(f(a)-f(b))/(2*h)},numeric(1))
    expected <- sqrt(drop(t(gradient)%*%covariance%*%gradient))
    expect_equal(as.numeric(result$standard_errors$rho),expected,tolerance=1e-6)
    expect_equal(as.numeric(result$standard_errors$sa),.01/(2*.3))
})

test_that("zero constraints and fixed marginal spreads are represented explicitly", {
    p <- extension_problem(rho=0)
    # Add a connected three-effect block with an unspecified (A,D) pair.
    p$model <- add_observable(p$model, Extra=D)
    p <- estimation_problem(p$model,p$experiments,c(p$parameters,md=parameter_spec(2),sd=parameter_spec(.4),
        rab=parameter_spec(.2,lower=-1,upper=1),rbd=parameter_spec(.2,lower=-1,upper=1)),
        statistical_model(A=normal(mean="ma",sd="sa"),B=normal(mean="mb",sd=.2),D=normal(mean="md",sd="sd"),
            C=normal(sd="sigma"),correlated(list("A","B","rab"),list("B","D","rbd"))))
    omega <- .nlmixr2_omega(p,list(),c(A="e1",B="e2",D="e3"))
    expect_match(omega$source,"fix(0e+00)",fixed=TRUE)
    expect_match(omega$source,"fix(4.0000000000000008e-02)",fixed=TRUE)
    p$parameters$rab$initial <- 1
    expect_error(.nlmixr2_omega(p,list(),c(A="e1",B="e2",D="e3")),"positive-definite")
})


test_that("normal SAEM disables the native uninformative-eta heuristic", {
    skip_if_not_installed("nlmixr2est")
    p <- extension_problem()
    # Avoid testing identifiability of two intercepts: use one normal effect.
    p$model$observables <- observables(C=A)
    p$experiments <- c(one=experiment(observations=observation_data(time=c(0,.5,1),observable="C",value=c(3,3.2,3.6))),
        two=experiment(observations=observation_data(time=c(0,.5,1),observable="C",value=c(1,1.1,1.2))))
    p <- estimation_problem(p$model,p$experiments,p$parameters[c("ma","sa","sigma")],
        statistical_model(A=normal(mean="ma",sd="sa"),C=normal(sd="sigma")))
    fit <- suppressMessages(estimate(p,nlmixr2_engine("saem",
        control=list(nBurn=40,nEm=60,print=0,covMethod=""))))
    expect_true(is.na(fit$neg_log_lik))
    expect_gt(as.numeric(coef(fit)$sa),.01)
    expect_identical(fit$engine_result$saemControl$handleUninformativeEtas,FALSE)
    exported <- to_nlmixr2(p,method="saem")
    evaluated <- .nlmixr2_evaluate(fit$engine_result,"impmap",list(isample=500L))
    imported <- from_nlmixr2(evaluated,exported,likelihood="focei",reference_fit=fit$engine_result)
    expect_equal(coef(imported),coef(fit))
    expect_error(estimate(p,nlmixr2_engine("saem",control=list(handleUninformativeEtas=TRUE))),
                 "handleUninformativeEtas")
})

test_that("logit-normal sampling density agrees with independent latent integration", {
    skip_if_not_installed("nlmixr2est")
    p <- extension_problem(logit=TRUE,rho=0)
    p$model$observables <- observables(C=A)
    studies <- c(one=experiment(observations=observation_data(time=c(0,.5,1),observable="C",value=c(.35,.4,.5))),
        two=experiment(observations=observation_data(time=c(0,.5,1),observable="C",value=c(.3,.35,.4))))
    p <- estimation_problem(p$model,studies,p$parameters[c("ma","sa","sigma")],
        statistical_model(A=logitnormal(median="ma",sdlogit="sa"),C=normal(sd="sigma")))
    exported <- to_nlmixr2(p)
    native <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"focei",
        control=nlmixr2est::foceiControl(maxOuterIterations=0,print=0,covMethod="")))
    imported <- from_nlmixr2(native,exported,likelihood="impmap",likelihood_control=list(isample=3000L))
    expected <- sum(vapply(list(c(.35,.4,.5),c(.3,.35,.4)),function(y) {
        density <- Vectorize(function(eta) exp(sum(dnorm(y,plogis(qlogis(.4)+eta),.1,log=TRUE))+
                                                  dnorm(eta,sd=.3,log=TRUE)))
        -log(integrate(density,-Inf,Inf,rel.tol=1e-9)$value)
    },numeric(1)))
    expect_equal(imported$neg_log_lik,expected,tolerance=.001)
})

test_that("covariances use converted normal spreads and dimensionless correlations", {
    p <- list(statistics=statistical_model(A=normal(sd=2 [mg]),B=normal(sd=3 [L]),
        correlated(list("A","B","rho"))),parameters=c(rho=parameter_spec(.2,lower=-1,upper=1)))
    omega <- .nlmixr2_omega(p,list(mass="g",length="dm"),c(A="e1",B="e2"))
    expect_equal(unname(omega$covariance),matrix(c(.002^2,.2*.002*3,.2*.002*3,9),2))
    expect_false(omega$free[1,1])
    expect_true(omega$free[1,2])
})

test_that("fixed normal spreads round-trip with an estimated correlation", {
    skip_if_not_installed("nlmixr2est")
    p <- extension_problem()
    p <- estimation_problem(p$model,p$experiments,p$parameters[c("ma","mb","sigma","rho")],
        statistical_model(A=normal(mean="ma",sd=.3),B=normal(mean="mb",sd=.2),
                          C=normal(sd="sigma"),correlated(list("A","B","rho"))))
    exported <- to_nlmixr2(p)
    fit <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"focei",
        control=nlmixr2est::foceiControl(maxOuterIterations=0,print=0,covMethod="")))
    result <- from_nlmixr2(fit,exported)
    expect_equal(as.numeric(coef(result)$rho),.4,tolerance=1e-6)
    expect_false(any(c("sa","sb") %in% names(coef(result))))
    altered <- exported
    altered$signature$parameters$fixed_value[4] <- 100
    expect_error(from_nlmixr2(fit,altered),"signature")
})
