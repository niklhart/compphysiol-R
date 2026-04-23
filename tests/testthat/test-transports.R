# Test for transport-related functions

test_that("Transports are created correctly", {
    # Linear transport
    t1 <- transports(from = "A", to = "B", const = "k1")
    expect_equal(length(t1), 1)
    expect_equal(t1$from, "A")
    expect_equal(t1$to, "B")
    expect_equal(t1$const[[1]], quote(k1))
    expect_equal(t1$rate[[1]], quote(k1*a[A]))
    expect_equal(t1$type, "linear")

    # Nonlinear transport
    t2 <- transports(from = "A", to = "B", rate = "k1 * a[A]*a[B]/(a[B]+K)")
    expect_equal(length(t2), 1)
    expect_equal(t2$from, "A")
    expect_equal(t2$to, "B")
    expect_equal(t2$rate[[1]], quote(k1 * a[A] * a[B]/(a[B] + K)))
    expect_equal(t2$const[[1]], NULL)
    expect_equal(t2$type, "nonlinear")
})

test_that("Empty transports are handled correctly", {
    t1 <- transports()
    t2 <- transports(from = "A", to = "B", const = "k")

    expect_equal(length(t1), 0)
    expect_equal(t2, c(t2, t1))
})

test_that("Multiple transports can be combined", {
    t1 <- transports(from = "A", to = "B", const = "kAB")
    t2 <- transports(from = "B", to = "A", rate = "kBA*a[A]")

    t12 <- c(t1, t2)

    expect_equal(length(t12), 2)
    expect_equal(t12$from, c("A", "B"))
    expect_equal(t12$to, c("B", "A"))

})

test_that("Transport subsetting works correctly", {
    t12 <- transports(from = c("A", "B"), to = c("B", "A"), const = c("kAB", "kBA"))
    t1 <- t12[1]

    expect_equal(length(t1), 1)
    expect_equal(t1$from, "A")
    expect_equal(t1$to, "B")
    expect_equal(t1$const[[1]], quote(kAB))
    expect_equal(t1$rate[[1]], quote(kAB*a[A]))
})

test_that("Vectorized transport creation with substitution works correctly", {
    # Linear transports with substitution
    t <- transports(from = c("A", "B"), to = c("C", "D"), const = "k{from}{to}")

    expect_equal(length(t), 2)
    expect_equal(t$const[[1]], quote(kAC))
    expect_equal(t$const[[2]], quote(kBD))

    # Nonlinear transports with substitution
    t2 <- transports(from = c("A", "B"), to = c("C", "D"), rate = "k{from}{to} * a[{from}]")

    expect_equal(length(t2), 2)
    expect_equal(t2$rate[[1]], quote(kAC * a[A]))
    expect_equal(t2$rate[[2]], quote(kBD * a[B]))
})

test_that("Transport printing works correctly", {
    # from -> to, 2 linear transports, molec as wildcard
    t1 <- transports(from = c("cen", "per"), to = c("per", "cen"), const = c("k1", "k2"))
    expect_snapshot(print(t1))

    # from -> sink, 1 nonlinear transport, specific molec
    t2 <- transports(from = "cen", to = NULL, molec = "drug", rate = "k1 * a[cen] / (a[cen] + K)")
    expect_snapshot(print(t2))
})

test_that("Adding transports to CompartmentModels works", {
    model <- compartment_model() |>
        add_transport(from = "A", to = "B", const = "k1") |>
        add_transport(from = "B", to = "C", rate = "k2 * a[C]")

    expect_equal(length(model$transports), 2)
    expect_equal(model$transports$from, c("A", "B"))
    expect_equal(model$transports$to, c("B", "C"))
    expect_equal(model$transports$const[[1]], quote(k1))
    expect_equal(model$transports$const[[2]], NULL)
    expect_equal(model$transports$rate[[1]], quote(k1 * a[A]))
    expect_equal(model$transports$rate[[2]], quote(k2 * a[C]))
})
