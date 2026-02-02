# Test file for symbolic lumping traversal

test_that("reference state is preserved and downstream states are expressed w.r.t. it", {
    ## --- build model ---
    M <- CompartmentModel$new()

    M$addCompartment("A", 0)
    M$addCompartment("B", 0)

    M$addReaction("A", "B", const = "kAB")
    M$addReaction("B", NULL, const = "kB0")

    refstate <- "A"

    ## --- lumping ---
    res <- get_lumping_conditions(M, refstate = refstate)

    ## --- structural checks ---
    expect_true(is.symbol(res$A))
    expect_equal(res$A, quote(A))

    expect_true(is.language(res$B))

    ## --- numerical correctness ---
    f <- function(A, kAB, kB0) eval(res$B)
    expect_equal(f(2, 4, 8), 1)
})

test_that("full symbolic assembly works for A->B->C->0", {
    # Model definition
    M <- CompartmentModel$new()

    M$addCompartment("A", 0)
    M$addCompartment("B", 0)
    M$addCompartment("C", 0)
    M$addCompartment("D", 0)

    M$addReaction("A", "B", const = "kAB")
    M$addReaction("B", "C", const = "kBC")
    M$addReaction("C", "D", const = "kCD")
    M$addReaction("D", "", const = "kD0")

    ## --- lumping ---
    res <- get_lumping_conditions(M, refstate = "A")

    ## --- check for complete substitution ---
    expect_true(is.symbol(res$A))
    expect_equal(res$A, quote(A))
    expect_false("B" %in% all.vars(res$C))
    expect_false(any(c("B", "C") %in% all.vars(res$D)))
})