# Run from the repository root. All fits are fresh; no /tmp artifacts required.
# This script investigates selection metadata, not likelihood normalization.
devtools::load_all(quiet = TRUE)
options(width = 110)
main <- function() {
    report <- 'doc/estimation-api/likelihood-selection-output.txt'
    sink(report, split = TRUE)
    on.exit(sink(), add = TRUE)
    cat('Investigation date: 2026-11-25\nRevision:',
        system('git rev-parse HEAD', intern = TRUE), '\n')
    for (p in c('nlmixr2est', 'rxode2', 'compphysiol'))
        cat(p, as.character(packageVersion(p)), '\n')
    cat(R.version.string, '\n')
    attempt <- function(label, expr) {
        cat('\nCALL:', label, '\n')
        tryCatch(withCallingHandlers(force(expr),
            warning = function(w) {cat('WARNING:', conditionMessage(w), '\n'); invokeRestart('muffleWarning')},
            message = function(m) {
                if (grepl('quadrature|likelihood|changed to focei', conditionMessage(m), ignore.case=TRUE))
                    cat('MESSAGE:', conditionMessage(m), '\n')
                invokeRestart('muffleMessage')}),
            error = function(e) {cat('ERROR:', conditionMessage(e), '\n'); NULL})
    }
    copy <- function(x) unserialize(serialize(x, NULL))
    raw <- function(f, key) {
        e <- f$env
        if (exists(key, e, inherits = FALSE)) get(key, e, inherits = FALSE) else NULL
    }
    snapshot <- function(f, label) {
        cat('\nSNAPSHOT:', label, '\n')
        # Direct environment reads avoid forcing the likelihood through accessors.
        for (n in c('est','ofvType','objective','logLik','objDf','impObj','impEonly','impIter')) {
            cat(n, ':\n'); print(raw(f,n))
        }
    }
    identify <- function(f, ll) {
        type <- nlmixr2est::getOfvType(f)
        tab <- f$objDf
        row <- if (is.character(type) && length(type) == 1L)
            which(tolower(rownames(tab)) == tolower(type)) else integer()
        ok <- length(row) == 1L && length(ll) == 1L && is.finite(as.numeric(ll)) &&
            is.finite(tab[['Log-likelihood']][row]) &&
            isTRUE(all.equal(as.numeric(ll), tab[['Log-likelihood']][row], tolerance=1e-10))
        cat('Candidate identification: type=', type, '; selected-row/value agreement=', ok, '\n', sep='')
        invisible(ok)
    }
    exercise <- function(f, label) {
        z <- copy(f)
        snapshot(z, paste(label, 'before logLik'))
        first <- attempt('logLik first', logLik(z))
        snapshot(z, paste(label, 'after first logLik'))
        second <- attempt('logLik second', logLik(z))
        cat('Repeated value identical:', identical(first, second), '\n')
        cat('Accessor selected method:', nlmixr2est::getOfvType(z), '\n')
        identify(z, second)
        z
    }
    eval(parse('tests/testthat/test-nlmixr2-estimation.R')[[1]])
    tr <- .nlmixr2_prepare(nlmixr2_test_problem())
    f <- attempt('fresh FOCEi', nlmixr2est::nlmixr2(tr$model, tr$data, 'focei',
        control=nlmixr2est::foceiControl(print=0L,maxOuterIterations=30L,compress=FALSE)))
    s <- attempt('fresh SAEM', nlmixr2est::nlmixr2(tr$model, tr$data, 'saem',
        control=nlmixr2est::saemControl(seed=99L,nBurn=100L,nEm=200L,print=0L,covMethod='',compress=FALSE)))
    stopifnot(!is.null(f), !is.null(s))
    checked <- list(focei=exercise(f,'FOCEi'), saem=exercise(s,'SAEM'))
    for (n in names(checked)) {
        origin <- copy(checked[[n]])
        before <- list(theta=origin$theta, omega=origin$omega)
        z <- attempt(paste(n,'-> IMPMAP E-only'), nlmixr2est::nlmixr2(origin,
            nlme::getData(origin),'impmap',control=nlmixr2est::impmapControl(
                nIter=0L,isample=1000L,impSeed=42L,print=0L,covMethod='',compress=FALSE)))
        stopifnot(!is.null(z))
        cat('Theta preserved:',isTRUE(all.equal(before$theta,z$theta)),
            'Omega preserved:',isTRUE(all.equal(before$omega,z$omega)), '\n')
        checked[[paste0(n,'_impmap')]] <- exercise(z,paste(n,'-> IMPMAP'))
    }
    # Native explicit selection, including the previously failing SAEM route.
    for (n in names(checked)) {
        z <- copy(checked[[n]])
        for (target in c('impmap','focei')) {
            snapshot(z,paste(n,'before setOfv',target))
            invisible(attempt(paste('setOfv',n,target),nlmixr2est::setOfv(z,target)))
            snapshot(z,paste(n,'after setOfv',target))
            ll <- attempt('logLik after selection',logLik(z))
            identify(z,ll)
        }
    }
    # Native quadrature selection on an independent copy, then switch back.
    z <- copy(checked$saem)
    for (target in c('laplace1.6','gauss5_2','gauss3_1.6')) {
        invisible(attempt(paste('SAEM setOfv',target),nlmixr2est::setOfv(z,target)))
        snapshot(z,paste('SAEM selected',target))
        identify(z,attempt('logLik after quadrature selection',logLik(z)))
    }
    # Check supported controls at fit creation, rather than editing fit internals.
    for (eager in c(FALSE, TRUE)) {
        label <- paste('SAEM one-node configured; logLik control', eager)
        z <- attempt(label, nlmixr2est::nlmixr2(tr$model,tr$data,'saem',
            control=nlmixr2est::saemControl(seed=99L,nBurn=100L,nEm=200L,
                print=0L,covMethod='',compress=FALSE,logLik=eager,
                nnodesGq=1L,nsdGq=2)))
        if (!is.null(z)) invisible(exercise(z,label))
    }
    cat('\nSOURCE EVIDENCE (installed version)\n')
    ns <- asNamespace('nlmixr2est')
    for (n in c('logLik.nlmixr2FitCore','logLik.nlmixr2FitData','nmObjGet',
                '.nmObjEnsureObjective','getOfvType','setOfv','.setOfvAdd',
                '.setOfvImp','.setSaemExtra','.saemGetLikName')) {
        cat('\nFUNCTION:',n,'\n')
        fun <- get(n, ns)
        cat(paste(deparse(fun),collapse='\n'),'\n')
    }
}
main()
