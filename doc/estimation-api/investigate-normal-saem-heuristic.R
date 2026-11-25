# Run from repository root. Isolate the native heuristic behind normal-IIV collapse.
main <- function() {
    sink('doc/estimation-api/normal-saem-heuristic-output.txt',split=TRUE)
    on.exit(sink())
    cat('nlmixr2est',as.character(packageVersion('nlmixr2est')),'\n')
    set.seed(7)
    data <- data.frame(ID=rep(1:8,each=4),TIME=rep(0:3,8))
    data$DV <- 3+rep(rnorm(8,sd=.4),each=4)+rnorm(32,sd=.1)
    model <- function() {
        ini({location <- 3; sigma <- .1; eta ~ .04})
        model({pred = location+eta; pred ~ add(sigma)})
    }
    for(enabled in c(TRUE,FALSE)) {
        cat('\nhandleUninformativeEtas:',enabled,'\n')
        fit <- suppressMessages(nlmixr2est::nlmixr2(model,data,'saem',
            control=nlmixr2est::saemControl(seed=99,nBurn=40,nEm=60,print=0,covMethod='',
                                           handleUninformativeEtas=enabled)))
        print(fit$theta);print(fit$omega)
    }
}
main()
