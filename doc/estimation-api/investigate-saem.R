# Run from repository root. Isolate one change at a time in the exported fixture.
devtools::load_all(quiet=TRUE)
main <- function() {
    sink('doc/estimation-api/saem-isolation-output.txt',split=TRUE)
    on.exit(sink())
    options(width=110)
    cat('Date: 2026-11-25\nRevision:',system('git rev-parse HEAD',intern=TRUE),'\n')
    for(p in c('nlmixr2est','rxode2')) cat(p,as.character(packageVersion(p)),'\n')
    run <- function(label,expr) {
        cat('\nRUN:',label,'\n')
        tryCatch(withCallingHandlers(force(expr),warning=function(w){cat('WARNING:',conditionMessage(w),'\n');invokeRestart('muffleWarning')},message=function(m)invokeRestart('muffleMessage')),
            error=function(e){cat('ERROR:',conditionMessage(e),'\n');NULL})
    }
    eval(parse('tests/testthat/test-nlmixr2-estimation.R')[[1]])
    tr <- .nlmixr2_prepare(nlmixr2_test_problem())
    results <- list()
    for(variant in c('original','boundedTransform_FALSE','inline_clearance_only','direct_residual_only','direct_residual_inline_clearance','direct_residual_unbounded_clearance','direct_residual_boundedTransform_FALSE','direct_residual_tight_bounds','direct_residual_tight_bounds_transform_FALSE')) {
        src <- tr$source
        direct <- grepl('direct_residual',variant)
        if(direct) {
            # Public sigma remains an SD, but native sigma now uses direct coordinates.
            src <- sub('cp_fix_2 <- c([^\n]+)', 'cp_fix_2 <- c(0.001, 2, Inf)',src)
            src <- sub('cp_stat_sigma = exp(cp_fix_2);','cp_stat_sigma = cp_fix_2;',src,fixed=TRUE)
            src <- sub('add(cp_stat_sigma)','add(cp_fix_2)',src,fixed=TRUE)
        }
        if(variant %in% c('inline_clearance_only','direct_residual_inline_clearance'))
            src <- sub('cp_stat_CL_pop * exp(cp_eta_1)','exp(cp_fix_1 + cp_eta_1)',src,fixed=TRUE)
        if(variant=='direct_residual_unbounded_clearance')
            src <- sub('cp_fix_1 <- c([^\n]+)','cp_fix_1 <- -1.6094379124341003',src)
        if(grepl('tight_bounds',variant))
            src <- sub('cp_fix_1 <- c([^\n]+)',sprintf('cp_fix_1 <- c(%.17g, %.17g, %.17g)',log(.18),log(.2),log(.21)),src)
        cat('\nVARIANT SOURCE:',variant,'\n',src,'\n')
        ctl <- nlmixr2est::saemControl(seed=99,nBurn=100,nEm=200,print=0,covMethod='',compress=FALSE,
                    boundedTransform=!grepl('FALSE',variant))
        fit <- run(paste(variant,'SAEM'),nlmixr2est::nlmixr2(eval(parse(text=src)),tr$data,'saem',control=ctl))
        if(is.null(fit)) next
        print(fit$theta);print(fit$omega)
        cat('initial metadata:\n');print(fit$iniUi$iniDf)
        tab <- as.data.frame(fit)
        sigma <- if(direct)unname(fit$theta[2]) else exp(unname(fit$theta[2]))
        cl <- exp(unname(fit$theta[1]))
        expected <- 100*exp(-cl*tab$TIME)
        conditional <- 100*exp(-cl*exp(tab$cp_eta_1)*tab$TIME)
        row <- data.frame(variant,CL_pop=cl,sigma=sigma,omega=fit$omega[1,1],
            max_population_error=max(abs(tab$PRED-expected)),
            max_individual_error=max(abs(tab$IPRED-conditional)),
            max_sigma_helper_error=if('cp_stat_sigma'%in%names(tab))max(abs(tab$cp_stat_sigma-sigma)) else NA_real_)
        print(row)
        print(tab[,intersect(c('ID','TIME','PRED','IPRED','cp_stat_CL_pop','cp_stat_sigma','cp_par_CL'),names(tab))])
        # Independently reevaluate the final native model, preserving population values.
        fresh <- run(paste(variant,'FOCEi reevaluation'),nlmixr2est::nlmixr2(fit,nlme::getData(fit),'focei',
            control=nlmixr2est::foceiControl(print=0,maxOuterIterations=0,covMethod='',compress=FALSE)))
        if(!is.null(fresh)) {
            cat('reevaluation preserves theta:',isTRUE(all.equal(fit$theta,fresh$theta)),
                'omega:',isTRUE(all.equal(fit$omega,fresh$omega)),'\n')
            ft <- as.data.frame(fresh)
            print(c(fresh_population_error=max(abs(ft$PRED-100*exp(-cl*ft$TIME))),
                    fresh_sigma_error=max(abs(ft$cp_stat_sigma-sigma))))
        }
        # Direct marginal likelihood for this exact analytic ODE solution.
        dat <- tr$data
        exact <- sum(vapply(split(dat,dat$ID),function(d) {
            joint <- function(e) sum(dnorm(d$DV,100*exp(-cl*exp(e)*d$TIME),sigma,log=TRUE))+dnorm(e,0,sqrt(fit$omega[1,1]),log=TRUE)
            mode <- optimize(function(e)-joint(e),c(-5,5),tol=1e-12)$minimum
            h <- 1e-5
            scale <- 1/sqrt(-(joint(mode+h)-2*joint(mode)+joint(mode-h))/h^2)
            j <- joint(mode)
            z <- integrate(Vectorize(function(t)exp(joint(mode+scale*t)-j)),-Inf,Inf,rel.tol=1e-8)
            -j-log(scale*z$value)
        },numeric(1)))
        cat('Independent marginal NLL:',exact,'\n')
        if(!is.null(fresh)) cat('Fresh FOCEi NLL:',-as.numeric(logLik(fresh)),'\n')
        if(variant=='original') {
            lower <- log(.001)
            cat('Double-bound-transform prediction for CL:',exp(lower+exp(fit$theta[1])),
                'table value:',tab$cp_stat_CL_pop[1],'\n')
        }
        if(variant=='direct_residual_unbounded_clearance') for(type in c('gauss15_3','gauss61_5')) {
            invisible(run(paste('quadrature refinement',type),{
                nlmixr2est::setOfv(fit,type);print(logLik(fit));print(fit$objDf)
            }))
        }
        # Materialization can fail; retain the observed error rather than stop exploration.
        invisible(run(paste(variant,'SAEM logLik'),{print(logLik(fit));print(fit$objDf)}))
        results[[variant]] <- row
    }
    cat('\nSUMMARY\n');print(do.call(rbind,results),row.names=FALSE)
}
reference <- function() {
    sink('doc/estimation-api/saem-reference-output.txt',split=TRUE)
    on.exit(sink())
    cat('Canonical native Gaussian random-intercept reference; nlmixr2est',
        as.character(packageVersion('nlmixr2est')), '\n')
    set.seed(2511)
    data <- data.frame(ID=rep(1:12,each=5),TIME=rep(0:4,12))
    data$DV <- 10+rep(rnorm(12,sd=1),each=5)+rnorm(60,sd=.5)
    model <- function() {
        ini({mu <- 9; sigma <- 1; eta ~ 1})
        model({pred = mu+eta; pred ~ add(sigma)})
    }
    fit <- suppressMessages(nlmixr2est::nlmixr2(model,data,'saem',
        control=nlmixr2est::saemControl(seed=99,nBurn=200,nEm=400,print=0,
            covMethod='',compress=FALSE)))
    print(fit$theta);print(fit$omega)
    target <- sum(vapply(split(data,data$ID),function(d) {
        n <- nrow(d); v <- diag(unname(fit$theta['sigma'])^2,n)+matrix(fit$omega[1,1],n,n)
        r <- d$DV-unname(fit$theta['mu'])
        as.numeric((n*log(2*pi)+determinant(v,logarithm=TRUE)$modulus+t(r)%*%solve(v,r))/2)
    },numeric(1)))
    cat('Independent analytic marginal NLL:',target,'\n')
    for(type in c('auto','gauss15_3','gauss25_5','focei')) {
        cat('Requested likelihood:',type,'\n')
        suppressMessages(if(type!='auto')nlmixr2est::setOfv(fit,type))
        print(logLik(fit));print(fit$objDf)
    }
}
if('--reference-only' %in% commandArgs(trailingOnly=TRUE)) reference() else main()
