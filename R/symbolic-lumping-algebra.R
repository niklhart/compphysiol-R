# Linear algebra for symbolic lumping of ODE systems

#' Symbolic lumping of a linear `CompartmentModel` based on its graph structure
#' @param flows A `Flows` object
#' @param scc A character vector, the strongly connected component (SCC) to be lumped
#' @returns A list with entries `internal`, `incoming`, `outgoing`, `elimination`, each a list of `Reaction` objects
.classify_flows <- function(flows, scc) {
    .check_class(flows, "Flows")
    list(
        internal = flows$from %in% scc & !is.na(flows$to) & flows$to %in% scc,
        incoming = !(flows$from %in% scc) & flows$to %in% scc,
        elimination = flows$from %in% scc & is.na(flows$to),
        outgoing = flows$from %in% scc & !is.na(flows$to) & !(flows$to %in% scc)
    ) |> 
        lapply(function(idx) flows[idx])
}

#' Solve a linear system of equations Ax+b=0 with symbolic coefficients
#' @param A A square list matrix of expressions
#' @param b A list of right-hand side expressions
#' @returns A named list of solution expressions
#' @export
.solve_linear_expr <- function(A, b) {
    switch(
        nrow(A),
        .solve_linear_expr_1x1(A, b),
        .solve_linear_expr_2x2(A, b),
        stop("Symbolic solve only implemented for 1x1 and 2x2 systems")
    )
}

# Helper functions to build symbolic expressions
.neg <- function(x) if (x == 0) 0 else call("-", x)
.add <- function(x, y) if (x == 0) y else if (y == 0) x else call("+", x, y)
.sub <- function(x, y) if (y == 0) x else if (x == 0) .neg(y) else call("-", x, y)
.mul <- function(x, y) if (x == 0 || y == 0) 0 else call("*", x, y)
.div <- function(x, y) if (x == 0) 0 else call("/", x, y)

#' Solve a 1x1 linear system of equations Ax+b=0 with symbolic coefficients
#' @param A A 1x1 list matrix of coefficients
#' @param b A list of right-hand side expressions
#' @returns A named list of solution expressions
#' @noRd
.solve_linear_expr_1x1 <- function(A, b) {

    a <- A[[1, 1]]
    rhs <- .neg(b[[1]])
    setNames(list(.div(rhs, a)), rownames(A))
}

#' Solve a 2x2 linear system of equations Ax+b=0 with symbolic coefficients
#' @param A A 2x2 list matrix of coefficients
#' @param b A list of right-hand side expressions
#' @returns A named list of solution expressions
#' @noRd
.solve_linear_expr_2x2 <- function(A, b) {

    a11 <- A[[1, 1]]
    a12 <- A[[1, 2]]
    a21 <- A[[2, 1]]
    a22 <- A[[2, 2]]
    b1 <- b[[1]]
    b2 <- b[[2]]

    det <- .sub(.mul(a11, a22), .mul(a12, a21))

    x <- .div(
        .sub(.mul(a12, b2), .mul(b1, a22)),
        det
    )

    y <- .div(
        .sub(.mul(b1, a21), .mul(a11, b2)),
        det
    )

    setNames(list(x, y), rownames(A))
}


#' Assemble the lumping equations for a strongly connected component (SCC)
#' @param scc A character vector, the strongly connected component (SCC) to be lumped
#' @param flows A `Flows` object
#' @returns A list with entries `A`, `b`
#' @export
.assemble_linear_expr <- function(scc, flows) {
    cls <- .classify_flows(flows, scc)

    n <- length(scc)
    idx <- setNames(seq_len(n), scc)

    ## initialize A and b as expressions
    A <- matrix(
        vector("list", n * n),
        nrow = n,
        dimnames = list(scc, scc)
    )
    A[,] <- list(quote(0))

    b <- vector("list", n)
    names(b) <- scc
    b[] <- list(quote(0))

    ## internal + elimination + outgoing → A
    for (fl in c(cls$internal, cls$elimination, cls$outgoing)) {
        i <- idx[fl$from]

        ## A[i,i] <- A[i,i] - k
        A[[i, i]] <- .sub(A[[i, i]], fl$const)

        ## internal transfer: j <- i
        if (!is.null(fl$to) && fl$to %in% scc) {
            j <- idx[fl$to]
            A[[j, i]] <- .add(A[[j, i]], fl$const)
        }
    }

    ## incoming → b
    for (fl in cls$incoming) {
        j <- idx[fl$to]
        b[[j]] <- .add(b[[j]], fl$rate)
    }

    ## return symbolic linear system
    list(A = A, b = b)
}

