test_that("transport schemes preserve the from/to representation", {
    expected <- transports("gut", "plasma", molec = "drug", const = "k")
    expect_identical(transports(scheme = "gut -> plasma", molec = "drug", const = "k"), expected)
    expect_identical(transports(scheme = "gut[drug] -> plasma[drug]", const = "k"), expected)
    expect_identical(
        transports(scheme = "gut[drug] -> NULL", const = "k"),
        transports("gut", NULL, molec = "drug", const = "k")
    )
    expect_identical(
        transports(scheme = "NULL -> plasma[drug]", rate = "r"),
        transports(NULL, "plasma", molec = "drug", rate = "r")
    )
    expect_identical(
        transports(scheme = "gut -> plasma", molec = c("A", "B"), const = "k_{molec}"),
        transports("gut", "plasma", molec = c("A", "B"), const = "k_{molec}")
    )
    model <- compartment_model() |>
        add_transport(scheme = "gut[drug] -> plasma[drug]", const = 1 [1/h])
    expect_identical(model$transports,
                     transports("gut", "plasma", molec = "drug", const = 1 [1/h]))
})

test_that("transport schemes reject ambiguous or nontransport syntax", {
    expect_error(transports(scheme = "A -> B", const = "k"), "require.*molec")
    expect_error(transports(scheme = "A[x] -> B[y]", const = "k"), "molecular identity")
    expect_error(transports(scheme = "A[x] -> B", const = "k"), "Both")
    expect_error(transports(scheme = "A[x] -> B[x]", molec = "x", const = "k"), "Do not supply")
    expect_error(transports(from = "A", scheme = "A -> B", molec = "x", const = "k"), "cannot be used")
    for (scheme in list(NA_character_, character(), c("A -> B", "B -> C"),
                        "A + B -> C", "A ->", "A -> B -> C")) {
        expect_error(transports(scheme = scheme, molec = "x", const = "k"), "scheme")
    }
})

test_that("reaction formula remains a deprecated alias", {
    expected <- reactions(scheme = "A -> B", const = "k")
    expect_warning(old <- reactions(formula = "A -> B", const = "k"), "deprecated.*scheme")
    expect_identical(old, expected)
    expect_warning(model <- compartment_model() |>
        add_reaction(formula = "A -> B", const = "k"), "deprecated.*scheme")
    expect_identical(model$reactions, expected)
    expect_warning(
        expect_error(reactions(scheme = "A -> B", formula = "A -> C", const = "k"),
                     "cannot be used together"),
        "deprecated"
    )
})


test_that("named reaction schemes retain vectorization and localization", {
    expect_identical(
        reactions(scheme = c("A -> B", "B -> C"), const = c("k1", "k2")),
        c(reactions("A", "B", const = "k1"), reactions("B", "C", const = "k2"))
    )
    model <- compartment_model() |>
        add_reaction(scheme = "A -> B", cmt = "cyt", const = 1 [1/h])
    expect_identical(model$reactions,
                     reactions("A", "B", cmt = "cyt", const = 1 [1/h]))
})
