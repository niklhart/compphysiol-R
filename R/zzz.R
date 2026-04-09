
.onLoad <- function(libname, pkgname) {

    # Define hidden `.physiologies` object in the package namespace
    ns <- asNamespace(pkgname)
    ns$.physiologies <- lapply(.physiologydb, function(cfg) {
        P <- physiology(params = cfg$param)
        P <- do.call(add_meta, c(list(phys = P), cfg$meta))
        P
    })

}



#' @importFrom stats rlnorm rnorm setNames
#' @importFrom utils head tail
#' @importFrom deSolve ode
NULL