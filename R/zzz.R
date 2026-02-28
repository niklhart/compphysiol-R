
.onLoad <- function(libname, pkgname) {

    # Define hidden `.physiologies` object in the package namespace
    ns <- asNamespace(pkgname)
    ns$.physiologies <- lapply(.physiology_data, function(cfg) {
        P <- physiology(params = cfg$param)
        P <- do.call(add_meta, c(list(phys = P), cfg$meta))
        P
    })

}



#' @importFrom stats rlnorm rnorm setNames
#' @importFrom utils head tail
#' @importFrom R6 R6Class
#' @importFrom deSolve ode
NULL