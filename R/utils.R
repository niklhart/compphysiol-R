
#' Helper function to uniformize input into an call
#'
#' @param input A character string, expression, numeric scalar, or call
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
    } else if (is.numeric(input) && length(input) == 1L) {
        as.call(list(as.name("("), input))
    } else {
        stop("Input must be a character, expression, numeric scalar, or quoted call.")
    }
}

#' Helper function to check for empty character input
#' @param x Input to check
#' @returns `TRUE` if `x` is `NULL`, of length zero, or a single empty string
#' @noRd
.is_emptychar <- function(x) {
    is.null(x) || length(x) == 0L || (length(x) == 1 && x == "")
}