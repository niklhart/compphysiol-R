# Run after explore.R, from the repository root.
devtools::load_all(quiet=TRUE)
options(width=110)
x <- readRDS('/tmp/estimation-api-fits.rds')
sink('doc/estimation-api/units-likelihood-output.txt', split=TRUE)
# Independent integration for the simple initial-amount model.
f <- x$impmap
th <- f$theta; omega <- sqrt(f$omega[1,1]); dat <- x$translated$data
ll <- vapply(split(dat,dat$ID),function(d) {
    joint <- function(eta) sum(dnorm(d$DV,100*exp(-exp(th[1]+eta)*d$TIME),exp(th[2]),log=TRUE))+dnorm(eta,0,omega,log=TRUE)
    mode <- optimize(function(e) -joint(e),c(-5,5),tol=1e-12)$minimum
    h <- 1e-5; curvature <- -(joint(mode+h)-2*joint(mode)+joint(mode-h))/h^2
    scale <- 1/sqrt(curvature)
    jointMode <- joint(mode)
    z <- integrate(Vectorize(function(z) exp(joint(mode+scale*z)-jointMode)), -Inf, Inf,rel.tol=1e-8)
    jointMode+log(scale)+log(z$value)
},numeric(1))
cat('Direct marginal NLL:',-sum(ll),'\n')
cat('IMPMAP raw objective:',f$impObj,'nobs accessor:',f$nobs,'observation rows:',nrow(dat),'\n')
print(c(using_accessor=(f$impObj+f$nobs*log(2*pi))/2,
        using_rows=(f$impObj+nrow(dat)*log(2*pi))/2, default=-as.numeric(logLik(f))))
cat('IMPMAP E-only and iteration markers:\n'); print(f$impEonly); print(f$impIter)
# Unit-aware mixed-effects fixture, retaining natural log-coordinate domains.
eval(parse('tests/testthat/test-estimation-engine-conformance.R')[[2]])
p <- unit_aware_fixed_effects_conformance_problem()
p$parameters <- c(CL_pop=parameter_spec(0.3 [L/h], transform='log'),
                   omega_CL=parameter_spec(0.2, transform='log'),
                   sigma=parameter_spec(1 [mg/L],transform='log'))
p <- estimation_problem(p$model,p$experiments,p$parameters,
    statistical_model(CL=lognormal(median='CL_pop',sdlog='omega_CL'),C=normal(sd='sigma')))
tr <- .nlmixr2_prepare(p,dimensions=list(time='min',mass='ug',length='dm'))
print(tr$dimensions); print(tr$data); cat('likelihood Jacobian:',tr$log_likelihood_unit_jacobian,'\n')
u <- nlmixr2est::nlmixr2(tr$model,tr$data,'focei',control=nlmixr2est::foceiControl(print=0,maxOuterIterations=100,covMethod='r',compress=FALSE))
r <- .nlmixr2_result(p,nlmixr2_engine(),u,tr)
print(r$coefficients); print(r$standard_errors); print(r$population_predictions)
print(c(rawNLL=-as.numeric(logLik(u)),publicNLL=r$neg_log_lik,difference=r$neg_log_lik+as.numeric(logLik(u)),expected=8*log(0.001)))
print(vcov(u)); print(r$estimate_correlations)
sink()
