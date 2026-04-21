# Test for reaction-related functions

test_that("Reactions are created correctly", {
    # Elementary reaction
    r1 <- reactions(input = c("A","B"), output = "C", const = "k1")
    expect_equal(length(r1), 1)
    expect_equal(r1$input, I(list(c("A","B"))))
    expect_equal(r1$output, I(list("C")))
    expect_equal(r1$const[[1]], quote(k1))
    expect_equal(r1$rate[[1]], quote(k1 * c[A] * c[B]))
    expect_equal(r1$type, "elementary")

    # Complex reaction
    r2 <- reactions(input = "A", output = "B", rate = "k1 * c[A]*c[B]/(c[B]+K)")
    expect_equal(length(r2), 1)
    expect_equal(r2$input, I(list("A")))
    expect_equal(r2$output, I(list("B")))
    expect_equal(r2$rate[[1]], quote(k1 * c[A] * c[B] / (c[B] + K)))
    expect_equal(r2$const[[1]], NULL)
    expect_equal(r2$type, "complex")
})

test_that("Empty reactions are handled correctly", {
    r1 <- reactions()
    r2 <- reactions(input = "A", output = "B", const = "k")

    expect_equal(length(r1), 0)
    expect_equal(r2, c(r2, r1))
})

test_that("Multiple reactions can be combined and subsetted", {
    r1 <- reactions(input = "A", output = "B", const = "kAB")
    r2 <- reactions(input = "B", output = "A", rate = "kBA*c[B]")

    r12 <- c(r1, r2)
    r1s <- r12[1]

    expect_equal(length(r12), 2)
    expect_equal(r12$input, I(list("A", "B")))
    expect_equal(r12$output, I(list("B", "A")))
    expect_equal(r1s,r1)
})

test_that("Vectorized reaction creation with compartment substitution works correctly", {

    # Elementary reactions with substitution
    r1 <- reactions(input = "A", output = "B", cmt = c("a","b"), const = "k{cmt}")

    expect_equal(length(r1), 2)
    expect_equal(r1$const[[1]], quote(ka))
    expect_equal(r1$const[[2]], quote(kb))

    # Complex reaction with substitution
    r2 <- reactions(input = "A", output = "B", cmt = c("a","b"), rate = "k{cmt}*c[A]")

    expect_equal(length(r2), 2)
    expect_equal(r2$rate[[1]], quote(ka * c[A]))
    expect_equal(r2$rate[[2]], quote(kb * c[A]))
})

test_that("Reaction printing works correctly", {
    # 1 input -> 1 output, no cmt specified
    r1 <- reactions(input = c("A", "B"), output = "C", const = "k")
    expect_snapshot(print(r1))

    # input -> sink, 1 nonlinear flow, no cmt specified
    r2 <- reactions(input = "A", output = NULL, rate = "k1 * c[A] / (c[A] + K)")
    expect_snapshot(print(r2))
})

test_that("Reactions can be added to compartment models", {
    model <- compartment_model() |>
        add_reaction(input = "A", output = "B", const = "k1") |>
        add_reaction(input = "B", output = "C", rate = "k2 * c[B]")

    expect_equal(length(model$reactions), 2)
    expect_equal(model$reactions$input, I(list("A", "B")))
    expect_equal(model$reactions$output, I(list("B", "C")))
    expect_equal(model$reactions$const[[1]], quote(k1))
    expect_equal(model$reactions$const[[2]], NULL)
    expect_equal(model$reactions$rate[[1]], quote(k1 * c[A]))
    expect_equal(model$reactions$rate[[2]], quote(k2 * c[B]))
})
