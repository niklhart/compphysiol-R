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
    expect_s3_class(t1, "compphysiol_transports")
    expect_error(t12[[1]], "as.data.frame.*inspect")
    expect_error(t12[["from"]], "as.data.frame.*inspect")
})

test_that("transport list conversion returns ordinary unwrapped component lists", {
    linear <- transports("A", "B", molec = "drug", const = 2 [1/h])
    nonlinear <- transports("B", NULL, molec = "drug", rate = "k * a[drug,B]")
    source <- transports(NULL, "A", rate = "input")
    x <- c(linear, nonlinear, source)
    entries <- as.list(x)
    expect_identical(class(entries), "list")
    expect_length(entries, 3L)
    for (i in seq_along(entries)) {
        entry <- entries[[i]]
        expect_false(is.object(entry))
        expect_named(entry, c("from", "to", "molec", "rate", "const", "type", "const_basis"))
        expect_identical(entry$from, x$from[i])
        expect_identical(entry$to, x$to[i])
        expect_identical(entry$molec, x$molec[i])
        expect_identical(entry$rate, x$rate[[i]])
        expect_identical(entry$const, x$const[[i]])
        expect_identical(entry$type, x$type[i])
    }
    expect_identical(as.list(linear), entries[1])
    expect_identical(as.list(transports()), list())
    expect_null(entries[[2]]$const)
    expect_named(entries[[2]], c("from", "to", "molec", "rate", "const", "type", "const_basis"))
})

test_that("wiring ordinary transport lists preserves constants and expands wildcards", {
    model <- compartment_model() |>
        add_compartment(name = c("A", "B")) |>
        add_molecule(name = c("drug", "metabolite"), type = "amount") |>
        add_transport(from = "A", to = "B", const = 2 [1/h]) |>
        add_transport(from = "B", to = NULL, rate = "k * a[B]") |>
        add_transport(from = NULL, to = "A", molec = "drug", rate = "input")
    wired <- wire(model)
    expect_s3_class(wired$transports, "compphysiol_transports")
    expect_identical(wired$transports$molec,
                     c("drug", "metabolite", "drug", "metabolite", "drug"))
    expect_identical(wired$transports$const[[1]], model$transports$const[[1]])
    expect_identical(wired$transports$const[[2]], model$transports$const[[1]])
    expect_null(wired$transports$const[[3]])
    expect_null(wired$transports$const[[4]])
    expect_identical(wired$transports$rate[[3]], quote(k * a[drug, B]))
    expect_identical(wired$transports$rate[[4]], quote(k * a[metabolite, B]))
    expect_identical(wired$transports$rate[[5]], quote(input))
    expect_identical(wired$transports$to[3:4], rep(NA_character_, 2))
    expect_identical(wired$transports$from[5], NA_character_)
    empty <- wire(compartment_model() |> add_compartment(name = "A") |> add_molecule(name = "drug"))
    expect_identical(empty$transports, transports())
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

test_that("Transport const rejects state references", {
    expect_error(
        transports(from = "A", to = "B", const = "k * a[A]"),
        "const.*state references.*rate"
    )
    expect_error(
        transports(from = "A", to = "B", const = quote(k * c[A])),
        "const.*state references.*rate"
    )

    expect_no_error(
        transports(from = "A", to = "B", rate = "k * a[A]")
    )
})

test_that("Transport const supports explicit unit values", {
    t <- transports(from = "A", to = "B", const = 1 [1/h])

    expect_s3_class(t$const[[1]], "units")
    expect_equal(units::set_units(t$const[[1]], "1/h", mode = "standard"), units::set_units(1, "1/h", mode = "standard"))
    expect_equal(
        t$rate[[1]],
        bquote(.(units::set_units(1, "1/h", mode = "standard")) * a[A])
    )

    model <- compartment_model() |>
        add_transport(from = "A", to = "B", const = 2 [1/h])

    expect_equal(units::set_units(model$transports$const[[1]], "1/h", mode = "standard"), units::set_units(2, "1/h", mode = "standard"))
})

test_that("Transport printing renders unit-bearing constants", {
    t <- transports(from = "A", to = "B", const = 1 [1/h])

    expect_output(print(t), "rate = 1 \\[1/h\\] \\* a\\[A\\]")
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

test_that("concentration-based constants preserve linearity and collection metadata", {
    tr <- transports("A", "B", const = "Q_{molec}",
                     molec = c("drug", "met"), const_basis = "concentration")
    expect_equal(tr$type, rep("linear", 2))
    expect_equal(tr$rate[[1]], quote(Q_drug * c[drug, A]))
    expect_equal(tr$rate[[2]], quote(Q_met * c[met, A]))
    expect_equal(c(tr[1], tr[2]), tr)
    expect_equal(as.list(tr)[[1]]$const_basis, "concentration")
    expect_error(transports("A", "B", const = "Q", const_basis = "invalid"), "arg")
    expect_error(transports("A", "B", rate = "r", const_basis = "amount"), "only.*const")
    model <- compartment_model() |>
        add_compartment(name = c("A", "B"), volume = c(2, 4)) |>
        add_molecule(name = c("drug", "met"), type = "amount") |>
        add_transport(from = "A", to = "B", const = "Q", const_basis = "concentration")
    wired <- wire(model)
    expect_equal(wired$transports$const_basis, rep("concentration", 2))
    expect_equal(wired$transports$rate[[2]], quote(Q * c[met, A]))
})

test_that("passive exchange conserves amount and equilibrates at equal concentrations", {
    model <- compartment_model() |>
        add_compartment(name = c("pla", "tis"), volume = c("Vp", "Vt")) |>
        add_molecule(name = "parent", initial = c(12, 0),
                     cmt = c("pla", "tis"), type = "amount") |>
        add_parameter(Vp = 2, Vt = 4, Q = 3) |>
        add_passive_exchange(c("pla", "tis"), molec = "parent", clearance = "Q")
    expect_equal(model$transports$from, c("pla", "tis"))
    expect_equal(model$transports$type, rep("linear", 2))
    process <- to_process_model(model)
    expect_equal(eval(process$processes$const[[1]], list(Q = 3, Vp = 2)), 1.5)
    expect_equal(eval(process$processes$const[[2]], list(Q = 3, Vt = 4)), 0.75)
    reference <- model
    reference$transports <- transports(c("pla", "tis"), c("tis", "pla"),
                                      molec = "parent", const = c("Q/Vp", "Q/Vt"))
    expect_equal(to_analytical_model(model), to_analytical_model(reference))
    expect_equal(lump_model(model), lump_model(reference))
    ode <- .to_ode(model)
    out <- deSolve::ode(y = ode$y0, times = c(0, 1, 20), func = ode$odefun)
    expect_equal(unname(rowSums(out[, -1])), rep(12, 3), tolerance = 1e-6)
    expect_equal(unname(out[3, 2] / 2), unname(out[3, 3] / 4), tolerance = 1e-6)
})

test_that("passive exchange supports units and rejects invalid endpoints", {
    tr <- passive_exchange(c("A", "B"), clearance = 2 [L/h])
    expect_s3_class(tr$const[[1]], "units")
    expect_equal(tr$const[[1]], tr$const[[2]])
    expect_equal(tr$const_basis, rep("concentration", 2))
    for (between in list(c("A", "A"), c("A", NA), c("A", ""), "A", NULL)) {
        expect_error(passive_exchange(between, clearance = "Q"), "two distinct compartments")
    }
    model <- compartment_model() |>
        add_compartment(name = c("A", "B"), volume = 2 [L]) |>
        add_molecule(name = "drug", initial = 1 [mg], type = "amount") |>
        add_passive_exchange(c("A", "B"), clearance = 2 [L/h])
    expect_no_error(.to_ode(model))
    model$transports <- passive_exchange(c("A", "B"), clearance = 2 [1/h])
    expect_error(.to_ode(model), "unit|Unit")
    model$compartments <- compartments(c("A", "B"), volume = NA_real_)
    expect_error(to_process_model(model), "volume|size")
})
