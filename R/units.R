#' Helper function for printing variables of class `units`
#' @param x Object of class `units`
#' @return Character string representation of `x` with units
#' @export
as.character.units <- function(x) format(x)
