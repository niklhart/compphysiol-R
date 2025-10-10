
.onLoad <- function(libname, pkgname) {

    # Define hidden `.physiologies` object in the package namespace
    ns <- asNamespace(pkgname)
    ns$.physiologies <- lapply(.physiology_data, function(cfg) {
        P <- Physiology$new(params = cfg$param)
        do.call(P$add_meta, cfg$meta)
        P
    })

}
