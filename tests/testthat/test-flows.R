# Test for flow-related functions

test_that("Flows are created correctly", {
    # Linear flow
    f1 <- flows(from = "A", to = "B", const = "k1")
    expect_equal(length(f1), 1)
    expect_equal(f1$from, "A")
    expect_equal(f1$to, "B")
    expect_equal(f1$const[[1]], quote(k1))
    expect_equal(f1$rate[[1]], quote(k1*A))
    expect_equal(f1$type, "linear")

    # Nonlinear flow
    f2 <- flows(from = "A", to = "B", rate = "k1 * A*B/(B+K)")
    expect_equal(length(f2), 1)
    expect_equal(f2$from, "A")
    expect_equal(f2$to, "B")
    expect_equal(f2$rate[[1]], quote(k1 * A * B/(B + K)))
    expect_equal(f2$const[[1]], NULL)
    expect_equal(f2$type, "nonlinear")
})

test_that("Empty flows are handled correctly", {
    f1 <- flows()
    f2 <- flows(from = "A", to = "B", const = "k")

    expect_equal(length(f1), 0)
    expect_equal(f2, c(f2, f1))
})

test_that("Multiple flows can be combined", {
    f1 <- flows(from = "A", to = "B", const = "kAB")
    f2 <- flows(from = "B", to = "A", rate = "kBA*B")

    f12 <- c(f1, f2)

    expect_equal(length(f12), 2)
    expect_equal(f12$from, c("A", "B"))
    expect_equal(f12$to, c("B", "A"))

})

test_that("Flow subsetting works correctly", {
    f12 <- flows(from = c("A", "B"), to = c("B", "A"), const = c("kAB", "kBA"))
    f1 <- f12[1]

    expect_equal(length(f1), 1)
    expect_equal(f1$from, "A")
    expect_equal(f1$to, "B")
    expect_equal(f1$const[[1]], quote(kAB))
    expect_equal(f1$rate[[1]], quote(kAB*A))
})

test_that("Vectorized flow creation with substitution works correctly", {
    # Linear flows with substitution
    f <- flows(from = c("A", "B"), to = c("C", "D"), const = "k_from_to")

    expect_equal(length(f), 2)
    expect_equal(f$const[[1]], quote(kAC))
    expect_equal(f$const[[2]], quote(kBD))

    # Nonlinear flows with substitution
    f <- flows(from = c("A", "B"), to = c("C", "D"), rate = "k_from_to * _from")

    expect_equal(length(f), 2)
    expect_equal(f$rate[[1]], quote(kAC * A))
    expect_equal(f$rate[[2]], quote(kBD * B))
})

test_that("Flow printing works correctly", {
    # from -> to, 2 linear flows
    f1 <- flows(from = c("A", "B"), to = c("C", "D"), const = c("k1", "k2"))
    expect_snapshot(print(f1))

    # from -> sink, 1 nonlinear flow
    f2 <- flows(from = "A", to = NULL, rate = "k1 * A / (A + K)")
    expect_snapshot(print(f2))
})

test_that("Adding flows to CompartmentModels works", {
    model <- compartment_model() |>
        add_flow(from = "A", to = "B", const = "k1") |>
        add_flow(from = "B", to = "C", rate = "k2 * B")

    expect_equal(length(model$flows), 2)
    expect_equal(model$flows$from, c("A", "B"))
    expect_equal(model$flows$to, c("B", "C"))
    expect_equal(model$flows$const[[1]], quote(k1))
    expect_equal(model$flows$const[[2]], NULL)
    expect_equal(model$flows$rate[[1]], quote(k1 * A))
    expect_equal(model$flows$rate[[2]], quote(k2 * B))
})