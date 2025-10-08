
#' Helper function to format a character string, expression or call into an call
#'
#' @param input A character string, expression or call
#' @returns A call representation of the input
#' @noRd
.as_call <- function(input) {
    if (is.character(input)) {
        # parse string into an expression
        parsed <- parse(text = input)
        if (length(parsed) != 1L)
            stop("Input must be a single expression.")
        parsed[[1]]
    } else if (is.expression(input)) {
        input[[1]]
    } else if (is.language(input)) { # quote() returns a call
        input
    } else {
        stop("Input must be a character, expression, or quoted call.")
    }
}
