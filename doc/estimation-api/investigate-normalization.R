# Run from repository root. Independent likelihoods use analytic Gaussian integrals.
devtools::load_all(quiet=TRUE)
main <- function() {
    sink('doc/estimation-api/normalization-output.txt',split=TRUE)
    on.exit(sink())
    options(width=110)
    cat('Date: 2026-11-25\nRevision:',system('git rev-parse HEAD',intern=TRUE),'\n')
    for(p in c('nlmixr2est','rxode2')) cat(p,as.character(packageVersion(p)),'\n')
    run <- function(label,expr) {
        cat('\nRUN:',label,'\n')
        tryCatch(withCallingHandlers(force(expr),warning=function(w){cat('WARNING:',conditionMessage(w),'\n');invokeRestart('muffleWarning')},message=function(m)invokeRestart('muffleMessage')),
            error=function(e){cat('ERROR:',conditionMessage(e),'\n');NULL})
    }
    # Random intercept: exact Gaussian marginal with covariance omega + sigma^2 I.
    exact <- function(y,loc,sd,omega) {
        n <- length(y); v <- diag(sd^2,n)+matrix(omega,n,n)
        r <- y-loc
        as.numeric((n*log(2*pi)+determinant(v,logarithm=TRUE)$modulus+t(r)%*%solve(v,r))/2)
    }
    results <- list()
    for(family in c('add','prop','lnorm')) for(layout in c('positive_time','zero_time','event','missing','two_endpoints','scaled')) {
        data <- data.frame(ID=rep(1:2,each=3),TIME=rep(c(1,2,4),2),DV=c(9,10.5,11,8.5,9.5,10),EVID=0L)
        if(layout=='zero_time') data$TIME <- data$TIME-1
        if(layout=='event') data <- rbind(data.frame(ID=1:2,TIME=0,DV=NA_real_,EVID=2L),data)
        if(layout=='missing') data$DV[3] <- NA_real_
        data$DVID <- 1L
        if(layout=='two_endpoints') data$DVID <- rep(c(1L,2L,1L),2)
        loc <- if(family=='lnorm') log(10) else 10
        sd <- if(family=='prop') .2 else if(family=='lnorm') .2 else 2
        variance <- if(family=='lnorm') .04 else 1
        if(layout=='scaled') {
            data$DV <- data$DV*1000
            if(family=='lnorm') loc <- loc+log(1000) else {
                loc <- loc*1000; variance <- variance*1000^2
                if(family=='add') sd <- sd*1000
            }
        }
        prediction <- if(family=='lnorm') 'exp(mu + eta)' else 'mu + eta'
        error <- paste0(family,'(sigma)')
        # Fixed declared values, but no fix flags: outer iterations are zero.
        src <- sprintf('function(){ini({mu <- %.17g; sigma <- %.17g; eta ~ %.17g}); model({pred = %s; pred ~ %s})}',loc,sd,variance,prediction,error)
        if(layout=='two_endpoints') {
            src <- sub('eta ~', sprintf('sigma2 <- %.17g; eta ~',sd),src,fixed=TRUE)
            src <- sub('pred ~',paste0('pred2 = pred; pred2 ~ ',family,'(sigma2); pred ~'),src,fixed=TRUE)
        }
        model <- eval(parse(text=src))
        label <- paste(family,layout)
        f <- run(paste(label,'FOCEi'),nlmixr2est::nlmixr2(model,data,'focei',control=nlmixr2est::foceiControl(print=0,maxOuterIterations=0,covMethod='',compress=FALSE)))
        if(is.null(f)) next
        i <- run(paste(label,'IMPMAP'),nlmixr2est::nlmixr2(f,nlme::getData(f),'impmap',control=nlmixr2est::impmapControl(print=0,nIter=0,isample=10000,impSeed=42,covMethod='',compress=FALSE)))
        if(is.null(i)) next
        obs <- data[data$EVID==0 & !is.na(data$DV),]
        # Proportional SD depends on eta: independent one-dimensional integration.
        target <- sum(vapply(split(obs,obs$ID),function(d) {
            if(family=='prop') {
                joint <- function(e) sum(dnorm(d$DV,loc+e,abs(loc+e)*sd,log=TRUE))+dnorm(e,0,sqrt(variance),log=TRUE)
                mode <- optimize(function(e)-joint(e),c(-5,5)*sqrt(variance))$minimum
                h <- sqrt(variance)*1e-4
                scale <- 1/sqrt(-(joint(mode+h)-2*joint(mode)+joint(mode-h))/h^2)
                j <- joint(mode)
                z <- integrate(Vectorize(function(t)exp(joint(mode+scale*t)-j)),-Inf,Inf,rel.tol=1e-9)
                return(-j-log(scale*z$value))
            }
            y <- if(family=='lnorm') log(d$DV) else d$DV
            exact(y,loc,sd,variance)+if(family=='lnorm') sum(log(d$DV)) else 0
        },numeric(1)))
        row <- data.frame(family,layout,n_obs=nrow(obs),native_nobs=i$nobs,
            exact_NLL=target,focei_NLL=-as.numeric(logLik(i)),impObj=i$impObj,
            imp_NLL_observation_constant=(i$impObj+nrow(obs)*log(2*pi))/2,
            imp_NLL_native_count=(i$impObj+i$nobs*log(2*pi))/2,
            lognormal_Jacobian=if(family=='lnorm')sum(log(obs$DV)) else 0)
        row$corrected_imp_NLL <- row$imp_NLL_observation_constant+row$lognormal_Jacobian
        row$corrected_minus_exact <- row$corrected_imp_NLL-target
        print(row)
        cat('impLi:');print(i$impLi)
        results[[label]] <- row
    }
    # Mixed normal/lognormal endpoints: a joint Gaussian integral after log transform.
    data <- data.frame(ID=rep(1:2,each=3),TIME=rep(c(1,2,4),2),
        DV=c(9,2.8,11,8.5,2.5,10),EVID=0L,DVID=rep(c(1L,2L,1L),2))
    model <- function() {
        ini({mu <- 10; sigma <- 2; sdlog <- .2; eta ~ 1})
        model({pred = mu+eta; pred ~ add(sigma)
               predlog = exp((mu+eta)/10); predlog ~ lnorm(sdlog)})
    }
    f <- run('mixed endpoints FOCEi',nlmixr2est::nlmixr2(model,data,'focei',
        control=nlmixr2est::foceiControl(print=0,maxOuterIterations=0,covMethod='',compress=FALSE)))
    if(!is.null(f)) {
        i <- run('mixed endpoints IMPMAP',nlmixr2est::nlmixr2(f,nlme::getData(f),'impmap',
            control=nlmixr2est::impmapControl(print=0,nIter=0,isample=10000,impSeed=42,covMethod='',compress=FALSE)))
        if(!is.null(i)) {
            target <- sum(vapply(split(data,data$ID),function(d) {
                ln <- d$DVID==2L; y <- d$DV; y[ln] <- log(y[ln])
                load <- ifelse(ln,.1,1); r <- y-10*load
                v <- outer(load,load)+diag(ifelse(ln,.2,2)^2)
                as.numeric((length(y)*log(2*pi)+determinant(v,logarithm=TRUE)$modulus+t(r)%*%solve(v,r))/2)+sum(log(d$DV[ln]))
            },numeric(1)))
            corrected <- (i$impObj+6*log(2*pi))/2+sum(log(data$DV[data$DVID==2L]))
            cat('Mixed endpoints exact NLL:',target,'corrected sampling NLL:',corrected,
                'difference:',corrected-target,'native nobs:',i$nobs,'\n')
        }
    }
    cat('\nSUMMARY\n'); print(do.call(rbind,results),row.names=FALSE)
}
main()
