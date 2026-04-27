# Test file for symbolic lumping traversal

test_that("reference state is preserved and downstream states are expressed w.r.t. it", {
    ## --- build model ---
    M <- compartment_model() |>
        add_compartment(c("A", "B")) |>
        add_transport("A", "B", const = "kAB") |>
        add_transport("B", "", const = "kB0")

    refstate <- "A"

    ## --- lumping ---
    res <- get_lumping_conditions(M, refstate = refstate)

    ## --- structural checks ---
    expect_true(is.symbol(res$A))
    expect_equal(res$A, quote(A))

    expect_true(is.language(res$B))

    ## --- numerical correctness ---
    f <- function(A, kAB, kB0) eval(res$B)       # TODO: replace `a[A]` by `A` to avoid this error
    expect_equal(f(2, 4, 8), 1)
})

test_that("full symbolic assembly works for A->B->C->D->0", {
    # Model definition
    M <- compartment_model() |>
        add_compartment(c("A", "B", "C", "D")) |>
        add_transport(
            from = c("A", "B", "C"),
            to = c("B", "C", "D"),
            const = "k{from}{to}"
        ) |>
        add_transport("D", "", const = "kD0")

    ## --- lumping ---
    res <- get_lumping_conditions(M, refstate = "A")

    ## --- check for complete substitution ---
    expect_true(is.symbol(res$A))
    expect_equal(res$A, quote(A))
    expect_false("B" %in% all.vars(res$C))
    expect_false(any(c("B", "C") %in% all.vars(res$D)))
})
