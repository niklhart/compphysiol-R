#' Generic functions for accessing database parameters
#' @param x The object to access
#' @param name The name of the parameter to access
#' @param ... Additional arguments to be passed to specific methods
#' @returns A `compphysiol_parameters` object containing the queried data.
#' @noRd
param <- function(x, name, ...) UseMethod("param")
