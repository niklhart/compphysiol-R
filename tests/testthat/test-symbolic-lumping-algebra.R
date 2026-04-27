# Testing the linear algebra part of symbolic lumping

test_that("classification of flows works", {
    # Model definition
    trans <- c(
        transports(from = c("A","B","C","B"), to = c("B","C","B","D"), const = "k{from}{to}"),
        transports(from = "C", to = NULL, const = "kC0")
    )

    scc <- c("B", "C")

    cls <- .classify_transports(trans, scc)

    expect_equal(length(cls$internal), 2)
    expect_equal(length(cls$incoming), 1)
    expect_equal(length(cls$outgoing), 1)
    expect_equal(length(cls$elimination), 1)
})

test_that("1x1 symbolic solve works for class B in A->B->0 model", {

    A <- matrix(list(quote(-kB0)), 1, 1, dimnames = list("B", "B"))
    b <- list(quote(kAB * A))
    res <- .solve_linear_expr(A, b)

    f <- function(A, kAB, kB0) eval(res$B)

    # Expected solution: B = (kAB * A) / kB0
    # Numerical test
    expect_equal(f(1, 2, 4), 0.5)
    expect_equal(f(3, 2, 4), 1.5)

})

test_that("2x2 symbolic solve works for classes B and C in A->B<->C->0 model", {
    A <- matrix(
        list(
            quote(-kBC), quote(kCB),
            quote(kBC),  quote(-kC0 - kCB)
        ),
        2,
        2,
        byrow = TRUE,
        dimnames = list(c("B", "C"), c("B", "C"))
    )

    b <- list(
        quote(kAB * A), 
        quote(0)
    )

    res <- .solve_linear_expr(A, b)

    f <- function(A, kAB, kBC, kCB, kC0) c(eval(res$B), eval(res$C))

    # Expected solution:
    # B = (kAB * A * (kC0 + kCB)) / ( kBC * (kC0 + kCB) + kBC * kCB )
    # C = (kAB * A * kBC) / ( kBC * (kC0 + kCB) + kBC * kCB )
    # Numerical tests
    expect_equal(f(A=1, kAB=1, kBC=1, kCB=1, kC0=1), c(2, 1))

})

test_that("assemble_linear_expr builds correct 1x1 system", {

    trans <- c(
        transports(from = "A", to = "B", const = "kAB"),
        transports(from = "B", to = NULL, const = "kB0")
    )

    scc <- "B"

    sys <- .assemble_linear_expr(scc, trans)

    expect_equal(
        sys$A,
        matrix(list(quote(-kB0)), 1, 1, dimnames = list("B","B"))
    )

    expect_equal(
        sys$b,
        list(B = quote(kAB * a[A]))
    )
})

test_that("assemble_linear_expr builds correct 2x2 system", {

    trans <- c(
        transports(from = c("A","B","C"), to = c("B","C","B"), const = "k{from}{to}"),
        transports(from = "C", to = NULL, const = "kC0")
    )

    scc <- c("B", "C")

    sys <- .assemble_linear_expr(scc, trans)

    A_expected <- matrix(
        list(
        quote(-kBC), quote(kCB),
        quote(kBC), quote(-kCB - kC0)
        ),
        2,
        2,
        byrow = TRUE,
        dimnames = list(c("B","C"), c("B","C"))
    )

    b_expected <- list(
        B = quote(kAB * a[A]),
        C = quote(0)
    )

    expect_equal(sys$A, A_expected)
    expect_equal(sys$b, b_expected)
})
