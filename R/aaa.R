# global options
# for now, just two dummy options that I can further adapt.
.the <- new.env(parent = emptyenv())
.the$options_meta <- list(
    verbose = list(
        default = TRUE,
        validate = function(x) is.logical(x) && length(x) == 1,
        description = "Whether to print detailed progress messages."
    ),
    solver = list(
        default = "rk45",
        validate = function(x) is.character(x) && length(x) == 1,
        description = "ODE solver to use for simulations."
    )
)
.the$options <- lapply(.the$options_meta, function(x) x$default)


#' Get package options
#'
#' @param name Option name, or NULL for all options
#' @export
getopt_compphysiol <- function(name = NULL) {
    if (is.null(name)) {
        return(.the$options)
    }
    .the$options[[name]]
}

#' Reset package options to the defaults
#' @export
resetopt_compphysiol <- function() {
    .the$options <- lapply(.the$options_meta, `[[`, "default")
    invisible(NULL)
}

#' Set package options
#'
#' @param ... Named arguments to update package options
#' @export
setopt_compphysiol <- function(...) {
    dots <- list(...)
    if (length(dots) == 0) return(invisible(NULL))
    for (nm in names(dots)) {
        meta  <- .the$options_meta[[nm]]
        value <- dots[[nm]]
        if (is.null(meta)) stop("Unknown option: ", nm)
        if (!meta$validate(value)) stop("Invalid value for ", nm)
        .the$options[[nm]] <- value
    }
    invisible(NULL)
}

#' Display package options
#' @export
dispopt_compphysiol <- function() {
    df <- data.frame(
        default = sapply(.the$options_meta, function(x) x$default),
        description = sapply(.the$options_meta, function(x) x$description),
        stringsAsFactors = FALSE
    )
    df
}



