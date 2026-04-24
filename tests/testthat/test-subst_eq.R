# unit test for .subst_eq() helper function

test_that(".subst_eq() correctly substitutes equations into transports", {
    # Define test flows and equations
    transp <- c(
        transports(
            from = "A",
            to = "B",
            rate = "k1 * a[A]"
        ),
        transports(
            from = "B",
            to = "C",
            const = "k2"
        )
    )
    eqs <- equations(
        name = c("k1", "k2"),
        expr = c("k11+k12", "k21+k22")
    )

    # Perform substitution
    substituted_transp <- .subst_eq(transp, eqs)

    # Check that const expressions have been correctly substituted
    expect_null(substituted_transp$constr[[1]])
    expect_equal(substituted_transp$const[[2]], quote(k21 + k22))

    # Check that rate expressions have been correctly substituted
    # Note: we use all.equal() here because the substituted expressions lack a set of optional parentheses,
    # but they should be mathematically equivalent.
    expect_true(all.equal(substituted_transp$rate[[1]], quote((k11 + k12) * a[A])))
    expect_true(all.equal(substituted_transp$rate[[2]], quote((k21 + k22) * a[B])))
})
