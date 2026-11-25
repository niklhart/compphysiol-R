# Run from repository root: Rscript doc/estimation-api/investigate-extensions.R
# Small native representation probes; these do not extend production support.
main <- function() {
    fixed_zero_only <- '--fixed-zero-only' %in% commandArgs(trailingOnly=TRUE)
    sink(if(fixed_zero_only) 'doc/estimation-api/extensions-fixed-zero-output.txt' else
        'doc/estimation-api/extensions-output.txt', split=TRUE)
    on.exit(sink())
    options(width=110)
    cat('Date: 2026-11-25\nRevision:',system('git rev-parse HEAD',intern=TRUE),'\n')
    for(p in c('nlmixr2est','rxode2')) cat(p,as.character(packageVersion(p)),'\n')
    run <- function(label,expr) {
        cat('\nRUN:',label,'\n')
        tryCatch(withCallingHandlers(force(expr),
            warning=function(w){cat('WARNING:',conditionMessage(w),'\n');invokeRestart('muffleWarning')},
            message=function(m)invokeRestart('muffleMessage')),
            error=function(e){cat('ERROR:',conditionMessage(e),'\n');NULL})
    }
    set.seed(251125)
    id <- rep(1:8,each=4); time <- rep(c(0,.5,1,2),8)
    latent <- rep(rnorm(8,sd=.3),each=4)
    fits <- list()
    families <- if(fixed_zero_only) character() else c('normal','lognormal','logitnormal','logitnormal_split')
    for(family in families) {
        basefamily <- sub('_split','',family,fixed=TRUE)
        location <- switch(basefamily, normal=3,lognormal=log(3),logitnormal=log(.5))
        expression <- switch(basefamily,normal='location + eta',lognormal='exp(location + eta)',
                             logitnormal='2 + 3 * expit(location + eta)')
        prediction <- switch(basefamily,normal=location+latent,lognormal=exp(location+latent),
                             logitnormal=2+3*plogis(location+latent))
        data <- data.frame(ID=id,TIME=time,DV=prediction+rnorm(length(id),sd=.1))
        src <- sprintf('function(){ini({location <- %.17g; sigma <- .1; eta ~ .09}); model({individual = %s; pred = individual; pred ~ add(sigma)})}',location,expression)
        if(family=='logitnormal_split') src <- sub('individual = 2 + 3 * expit(location + eta)',
            'latent = location + eta; individual = 2 + 3 * expit(latent)',src,fixed=TRUE)
        model <- eval(parse(text=src))
        cat('\nSOURCE:',family,'\n',src,'\n')
        ui <- run(paste(family,'parse'),nlmixr2est::nlmixr2(model))
        if(!is.null(ui)) {print(ui$iniDf);print(ui$muRefDataFrame)}
        for(method in c('focei','saem')) {
            control <- if(method=='saem') nlmixr2est::saemControl(seed=99,nBurn=40,nEm=60,
                print=0,covMethod='',compress=FALSE) else
                nlmixr2est::foceiControl(print=0,maxOuterIterations=0,covMethod='',compress=FALSE)
            fit <- run(paste(family,method),nlmixr2est::nlmixr2(model,data,method,control=control))
            if(is.null(fit)) next
            cat('theta:\n');print(fit$theta);cat('omega:\n');print(fit$omega)
            cat('initial parameter metadata:\n');print(fit$iniUi$iniDf)
            cat('final parameter metadata:\n');print(fit$finalUi$iniDf)
            cat('final model identical to input:',identical(fit$finalUi$model,ui$model),'\n')
            tab <- as.data.frame(fit)
            pop <- switch(basefamily,normal=fit$theta['location'],lognormal=exp(fit$theta['location']),
                          logitnormal=2+3*plogis(fit$theta['location']))
            conditional <- switch(basefamily,normal=fit$theta['location']+tab$eta,
                lognormal=exp(fit$theta['location']+tab$eta),
                logitnormal=2+3*plogis(fit$theta['location']+tab$eta))
            cat('PRED/IPRED errors:',max(abs(tab$PRED-pop)),max(abs(tab$IPRED-conditional)),'\n')
            cat('marginal median:',as.numeric(pop),'\n')
            evaluated <- run(paste(family,method,'IMPMAP'),nlmixr2est::nlmixr2(fit,fit$origData,'impmap',
                control=nlmixr2est::impmapControl(nIter=0,isample=500,impSeed=42,print=0,covMethod='')))
            if(!is.null(evaluated)) cat('evaluation preserves theta/omega:',
                isTRUE(all.equal(fit$theta,evaluated$theta)),isTRUE(all.equal(fit$omega,evaluated$omega)),'\n')
            fits[[paste(family,method)]] <- fit
        }
    }
    block <- c(free='eta1 + eta2 ~ c(.25, .05, .04)',
               fixed_block='eta1 + eta2 ~ fix(.25, .05, .04)',
               fixed_entry='eta1 + eta2 ~ c(.25, fix(.05), .04)',
               fixed_diagonal='eta1 + eta2 ~ c(fix(.25), .05, fix(.04))',
               zero_pattern='eta1 + eta2 + eta3 ~ c(.25,.05,.04,0,.01,.09)',
               zero_fixed='eta1 + eta2 + eta3 ~ c(.25,.05,.04,fix(0),.01,.09)',
               singular='eta1 + eta2 ~ c(.25,.1,.04)')
    # Match the full-run RNG stream when running only the fixed-zero supplement.
    if(fixed_zero_only) invisible(rnorm(length(id)*4))
    u <- matrix(rnorm(16),8,2)%*%chol(matrix(c(.25,.05,.05,.04),2))
    data <- data.frame(ID=id,TIME=time,DV=exp(log(3)+u[id,1])+exp(log(.5)+u[id,2])*time+rnorm(length(id),sd=.1))
    if(fixed_zero_only) block <- block['zero_fixed']
    for(name in names(block)) {
        model_expr <- if(grepl('^zero_',name))
            'intercept = exp(location + eta1); slope = exp(beta + eta2); quadratic = exp(gamma + eta3); pred = intercept + slope * TIME + quadratic * TIME * TIME' else
            'intercept = exp(location + eta1); slope = exp(beta + eta2); pred = intercept + slope * TIME'
        src <- paste0('function(){ini({location <- 1.0986122886681098; beta <- -0.6931471805599453; sigma <- .1;',
                      if(grepl('^zero_',name)) 'gamma <- -2.302585092994046;' else '',block[[name]],
                      '}); model({',model_expr,'; pred ~ add(sigma)})}')
        cat('\nBLOCK SOURCE:',name,'\n',src,'\n')
        model <- eval(parse(text=src))
        ui <- run(paste(name,'parse'),nlmixr2est::nlmixr2(model))
        if(is.null(ui)) next
        print(ui$iniDf)
        if(name=='singular') next
        fit <- run(paste(name,'SAEM'),nlmixr2est::nlmixr2(model,data,'saem',
            control=nlmixr2est::saemControl(seed=99,nBurn=40,nEm=60,print=0,covMethod='',compress=FALSE)))
        if(is.null(fit)) next
        print(fit$theta); print(fit$omega); print(cov2cor(fit$omega))
        cat('final parameter metadata:\n'); print(fit$finalUi$iniDf)
        cat('final model identical to input:',identical(fit$finalUi$model,ui$model),'\n')
        ev <- run(paste(name,'IMPMAP'),nlmixr2est::nlmixr2(fit,fit$origData,'impmap',
            control=nlmixr2est::impmapControl(nIter=0,isample=500,print=0,covMethod='')))
        if(!is.null(ev)) cat('evaluation preserves theta/omega:',
            isTRUE(all.equal(fit$theta,ev$theta)),isTRUE(all.equal(fit$omega,ev$omega)),'\n')
    }
}
main()
