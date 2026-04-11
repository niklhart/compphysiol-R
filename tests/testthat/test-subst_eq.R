# unit test for .subst_eq() helper function

test_that(".subst_eq() correctly substitutes equations into flows", {
    # Define test flows and equations
    flows <- c(
        flows(
            from = "A",
            to = "B",
            rate = "k1 * A"
        ),
        flows(
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
    substituted_flows <- .subst_eq(flows, eqs)

    # Check that const expressions have been correctly substituted
    expect_null(substituted_flows$constr[[1]])
    expect_equal(substituted_flows$const[[2]], quote(k21 + k22))

    # Check that rate expressions have been correctly substituted
    # Note: we use all.equal() here because the substituted expressions lack a set of optional parentheses, 
    # but they should be mathematically equivalent.
    expect_true(all.equal(substituted_flows$rate[[1]], quote((k11 + k12) * A)))
    expect_true(all.equal(substituted_flows$rate[[2]], quote((k21 + k22) * B)))
})