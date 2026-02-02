#' Simplify symbolic expressions using the specified method
#' @param expr A named list of quoted expressions
#' @param method A string specifying the simplification method: "none" or "Ryacas"
#' @returns A named list of simplified expressions
#' @noRd
.simplify <- function(expr, method) {

    switch(method,
        none = expr,
        Ryacas = .simplify_ryacas(expr),
        stop("Unknown simplification method: ", method)
    )

}


#' Simplify a symbolic expression using Ryacas
#' @param expr A named list of quoted expressions
#' @returns A named list of simplified expressions
#' @noRd
.simplify_ryacas <- function(expr) {

    if (!requireNamespace("Ryacas", quietly = TRUE)) {
        stop("Package 'Ryacas' is required for expression simplification. Please install it first.")
    }

    simplify_expr_ryacas <- function(expr) {
        # Convert quoted R expression to Ryacas object
        y <- Ryacas::ysym(expr)
        # Simplify using Ryacas
        ys <- Ryacas::Simplify(y)
        # Convert back to R expression
        as.expression(ys)[[1]]  # returns a single quoted expression
    }
    lapply(expr, simplify_expr_ryacas)
}



#' Simplify a symbolic expression by cancelling double negatives (currently unused)
#' 
#' @param expr A quoted call
#' @returns A simplified expression
#' @noRd
.simplify_expr <- function(expr) {

    is_unary_minus <- function(e) {
        is.call(e) && e[[1]] == as.name("-") && length(e) == 2
    }
    is_div <- function(e) {
        is.call(e) && e[[1]] == as.name("/") && length(e) == 3
    }

    if (is.call(expr)) {
        if (is_unary_minus(expr) && is_unary_minus(expr[[2]])) {
            return(.simplify_expr(expr[[2]][[2]]))
        } else if (is_div(expr) && is_unary_minus(expr[[3]])) {
            if (is_unary_minus(expr[[2]])) {
                return(.simplify_expr(.div(expr[[2]][[2]], expr[[3]][[2]])))
            } else {
                return(.simplify_expr(call("/", expr[[2]], expr[[3]][[2]])))
            }
        } else {
            for (i in seq_along(expr)) {
                expr[[i]] <- .simplify_expr(expr[[i]])
            }
        }
    }
    expr

}
