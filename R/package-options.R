#' # package options
#'
#' options::define_option(
#'     "atol",
#'     default = 1,
#'     desc = "Absolute tolerance, a dummy option used for testing.",
#'     option_fn = function(value) if(!is.numeric(value)) stop("Option must be numeric.") else value
#' )
#'
#'
#'
#' #' @eval options::as_roxygen_docs()
#' NULL
