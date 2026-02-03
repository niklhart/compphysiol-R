#' Simplify symbolic expressions using the specified method
#' @param expr A named list of quoted expressions
#' @param method A string specifying the simplification method: "none" or "Ryacas"
#' @returns A named list of simplified expressions
#' @noRd
.simplify <- function(expr, method) {

    switch(method,
        none = expr,
        Ryacas = .simplify_ryacas(expr)
    )

}


#' Simplify a symbolic expression using Ryacas
#' @param expr A quoted expression or a named list of quoted expressions
#' @returns A simplified expression or named list of simplified expressions
#' @noRd
.simplify_ryacas <- function(expr) {

    if (!requireNamespace("Ryacas", quietly = TRUE)) {
        stop(
        "Package 'Ryacas' is required for expression simplification.\n",
        "Please install it via install.packages('Ryacas').",
        call. = FALSE
        )
    }

    simplify_one <- function(e) {
        txt <- paste(deparse(e), collapse = "") |> 
            .encode_symbol()
        y <- Ryacas::ysym(txt)
        ys <- Ryacas::simplify(y)
        parse(text = as.character(ys))[[1]] |> 
            .decode_expr()
    }

    if (is.list(expr)) {
        lapply(expr, simplify_one)
    } else {
        simplify_one(expr)
    }
}


.encode_symbol <- function(x) {
    gsub("_", "zzz", x, fixed = TRUE)
}

.decode_symbol <- function(x) {
    gsub("zzz", "_", x, fixed = TRUE)
}

.encode_expr <- function(expr) {
    if (is.symbol(expr)) {
        as.symbol(.encode_symbol(as.character(expr)))
    } else if (is.call(expr)) {
        as.call(lapply(expr, .encode_expr))
    } else {
        expr
    }
}

.decode_expr <- function(expr) {
    if (is.symbol(expr)) {
        as.symbol(.decode_symbol(as.character(expr)))
    } else if (is.call(expr)) {
        as.call(lapply(expr, .decode_expr))
    } else {
        expr
    }
}
