# Run after explore.R. Capture implementation evidence for the installed version.
library(nlmixr2est)
x <- readRDS('/tmp/estimation-api-fits.rds')
sink('doc/estimation-api/native-metadata-output.txt')
for (n in c('focei','saem','impmap')) {
    f <- x[[n]]
    cat('\n',n,'\n')
    cat('finalUi model identical to FOCEi:',identical(f$finalUi$model,x$focei$finalUi$model),'\n')
    cat('\nfinalUi parameter signature:\n')
    print(f$finalUi$iniDf[,c('ntheta','neta1','neta2','name','lower','upper','fix')])
    cat('row map:\n'); print(f$env$.rownum)
    cat('origData identical to export:',identical(f$origData,x$translated$data),'\n')
}
for (n in c('.impmapFamilyFit','.setOfvImp','.impmapRecomputeObjf','setOfv')) {
    cat('\nINSTALLED FUNCTION:',n,'\n')
    print(get(n,asNamespace('nlmixr2est')))
}
sink()
