# Run from the repository root: Rscript doc/estimation-api/explore.R
# Native fits are transient; the compact report is written alongside this script.
devtools::load_all(quiet = TRUE)
options(width = 110)
out <- file('doc/estimation-api/exploration-output.txt', 'wt')
sink(out, split = TRUE)
cat('Revision:', system('git rev-parse HEAD', intern = TRUE), '\n')
print(sessionInfo())
for (p in c('nlmixr2est','rxode2','nlmixr2data','units'))
    cat(p, as.character(packageVersion(p)), '\n')
eval(parse('tests/testthat/test-nlmixr2-estimation.R')[[1]])
problem <- nlmixr2_test_problem()
translated <- .nlmixr2_prepare(problem)
cat('\nEXPORTED SOURCE\n', translated$source, '\n')
print(translated$data)
run <- function(label, expr) {
    cat('\nRUN:', label, '\n')
    tryCatch(withCallingHandlers(force(expr), warning = function(w) {
        cat('WARNING:', conditionMessage(w), '\n'); invokeRestart('muffleWarning')
    }), error = function(e) {cat('ERROR:', conditionMessage(e), '\n'); NULL})
}
field <- function(x, name) tryCatch(do.call(`$`, list(x, name)), error = function(e) conditionMessage(e))
report <- function(fit) {
    if (is.null(fit)) return(invisible(NULL))
    cat('class:', class(fit), '\ncolumns:', names(fit), '\n')
    for (n in c('est', 'ofvType', 'theta', 'omega', 'objDf', 'impObj', 'nobs', 'covMethod')) {
        cat(n, ':\n'); print(field(fit,n))
    }
    print(logLik(fit)); print(tryCatch(vcov(fit), error = conditionMessage))
    cat('environment fields:\n'); print(sort(ls(fit$env, all.names=TRUE)))
    for (n in c('iniUi','finalUi','ui')) {
        ui <- field(fit,n)
        cat(n, 'class:', class(ui), '\n')
        if (!is.character(ui)) {
            cat('iniDf:\n'); print(ui$iniDf)
            cat('model expressions:\n'); print(ui$model)
        }
    }
    cat('getData:\n'); print(nlme::getData(fit))
    cat('origData:\n'); print(field(fit,'origData'))
    cat('table:\n'); print(as.data.frame(fit))
    cat('existing result conversion:\n')
    imported <- run('convert', .nlmixr2_result(problem, nlmixr2_engine(), fit, translated))
    if (!is.null(imported)) print(imported[c('coefficients','standard_errors','neg_log_lik')])
}
f <- run('FOCEi', nlmixr2est::nlmixr2(translated$model, translated$data, 'focei',
    control=nlmixr2est::foceiControl(print=0L, maxOuterIterations=30L, compress=FALSE)))
report(f)
s <- run('SAEM', nlmixr2est::nlmixr2(translated$model, translated$data, 'saem',
    control=nlmixr2est::saemControl(seed=99L, nBurn=100L, nEm=200L, print=0L,
                                  covMethod='', compress=FALSE)))
report(s)
if (!is.null(s)) {
    before <- list(theta=s$theta, omega=s$omega)
    i <- run('SAEM -> IMPMAP nIter=0', nlmixr2est::nlmixr2(s, nlme::getData(s), 'impmap',
        control=nlmixr2est::impmapControl(print=0L, nIter=0L, isample=1000L,
            impSeed=42L, covMethod='', compress=FALSE)))
    report(i)
    if (!is.null(i)) {
        cat('population equality after evaluation:\n')
        print(all.equal(before$theta, i$theta)); print(all.equal(before$omega, i$omega))
        cat('raw impObj plus normalizing constant:\n')
        print(c(impObj=i$impObj, normalizedNLL=(i$impObj+i$nobs*log(2*pi))/2))
    }
    z <- unserialize(serialize(s,NULL))
    invisible(run('setOfv(SAEM, impmap)', nlmixr2est::setOfv(z,'impmap')))
    cat('setOfv objective table and selected logLik:\n'); print(z$objDf); print(logLik(z))
    print(nlmixr2est::getOfvType(z))
    print(all.equal(before$theta,z$theta)); print(all.equal(before$omega,z$omega))
}
saveRDS(list(problem=problem, translated=translated, focei=f, saem=s,
             impmap=if(exists('i')) i else NULL), '/tmp/estimation-api-fits.rds')
sink(); close(out)
