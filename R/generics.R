#' Generic functions for accessing database parameters
#' @param x The object to access
#' @param name The name of the parameter to access
#' @param ... Additional arguments to be passed to specific methods
#' @returns A `Parameters` object containing the queried data.
#' @export
param <- function(x, name, ...) UseMethod("param")
