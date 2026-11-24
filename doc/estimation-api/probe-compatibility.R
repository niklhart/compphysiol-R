# Run after explore.R, from repository root.
devtools::load_all(quiet=TRUE)
options(width=110)
x <- readRDS('/tmp/estimation-api-fits.rds')
problem <- x$problem; translated <- x$translated
sink('doc/estimation-api/compatibility-output.txt', split=TRUE)
run <- function(label, expr) {
    cat('\nRUN:',label,'\n')
    tryCatch(withCallingHandlers(force(expr), warning=function(w) {
        cat('WARNING:',conditionMessage(w),'\n'); invokeRestart('muffleWarning')
    }), error=function(e) {cat('ERROR:',conditionMessage(e),'\n'); NULL})
}
summary_fit <- function(fit, tr=translated, pr=problem) {
    if(is.null(fit)) return()
    print(fit$theta); print(fit$omega); print(fit$objDf)
    cat('original data matches export:', identical(nlme::getData(fit),tr$data),'\n')
    cat('iniUi iniDf:\n'); print(fit$iniUi$iniDf)
    cat('finalUi iniDf:\n'); print(fit$finalUi$iniDf)
    cat('iniUi model:\n'); print(fit$iniUi$model)
    cat('table columns:',names(fit),'\n')
    r <- run('existing conversion', .nlmixr2_result(pr,nlmixr2_engine(),fit,tr))
    if(!is.null(r)) {print(r$coefficients); print(r$standard_errors); print(r$neg_log_lik)}
}
ctl <- nlmixr2est::foceiControl(print=0L,maxOuterIterations=0L,covMethod='',compress=FALSE)
for (variant in c('start','structure','data','endpoint','rows','no_tables')) {
    src <- translated$source; dat <- translated$data
    if(variant=='start') src <- sub('-1.6094379124341003e+00','-1.5',src,fixed=TRUE)
    if(variant=='structure') src <- sub('cp_obs_C = (','cp_obs_C = 2 * (',src,fixed=TRUE)
    if(variant=='data') dat$DV[1] <- dat$DV[1]+1
    if(variant=='endpoint') dat$DVID[1] <- 2L
    if(variant=='rows') dat <- dat[c(3,1,2,6,4,5),]
    control <- ctl
    if(variant=='no_tables') control$calcTables <- FALSE
    fit <- run(variant,nlmixr2est::nlmixr2(eval(parse(text=src)),dat,'focei',control=control))
    summary_fit(fit)
}
# SAEM parameterization checks: remove finite transformed bounds, then inline
# expressions to give the backend an explicit exp(theta + eta) location.
pr <- problem
pr$parameters <- c(CL_pop=parameter_spec(0.2,transform='log'),
                   omega_CL=parameter_spec(0.2,transform='log'),
                   sigma=parameter_spec(2,transform='log'))
tr <- .nlmixr2_prepare(pr)
for (variant in c('unbounded','inlined')) {
    src <- tr$source
    if(variant=='inlined') {
        src <- sub('cp_stat_CL_pop * exp(cp_eta_1)','exp(cp_fix_1 + cp_eta_1)',src,fixed=TRUE)
        src <- sub('add(cp_stat_sigma)','add(exp(cp_fix_2))',src,fixed=TRUE)
    }
    fit <- run(paste('SAEM',variant),nlmixr2est::nlmixr2(eval(parse(text=src)),tr$data,'saem',
        control=nlmixr2est::saemControl(seed=99,nBurn=100,nEm=200,print=0,covMethod='',compress=FALSE)))
    summary_fit(fit,tr,pr)
    if(!is.null(fit)) {print(as.data.frame(fit)[,intersect(c('PRED','IPRED','cp_stat_CL_pop','cp_par_CL','cp_stat_sigma'),names(fit))]); print(logLik(fit))}
}
sink()
