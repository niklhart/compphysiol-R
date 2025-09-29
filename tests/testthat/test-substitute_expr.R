test_that("substitute_expr handles states, params, operators, functions, and freeParams correctly", {
    stateNames <- c("Central", "Peripheral")
    name2idx <- setNames(seq_along(stateNames), stateNames)

    # Helper to call substitute_expr with a fresh freeParams environment
    call_sub <- function(expr, paramValues = list(), obsFunc = FALSE) {
        freeParams <- new.env(parent = emptyenv())
        freeParams$list <- character()
        out <- substitute_expr(expr, stateNames, name2idx,
                               paramValues = paramValues,
                               freeParamsEnv = freeParams,
                               obsFunc = obsFunc)
        list(expr = out, freeParams = freeParams$list)
    }

    # 1. Pure state reference
    res <- call_sub("Central")
    expect_equal(deparse(res$expr), "y[1L]")
    expect_equal(res$freeParams, character())

    # 2. Operator remains operator
    res <- call_sub("Central * Peripheral")
    expect_equal(deparse(res$expr), "y[1L] * y[2L]")
    expect_equal(res$freeParams, character())

    # 3. Builtin math function remains intact
    res <- call_sub("exp(Central)")
    expect_equal(deparse(res$expr), "exp(y[1L])")
    expect_equal(res$freeParams, character())

    # 4. Parameter inlined if scalar numeric
    res <- call_sub("k12 * Central", paramValues = list(k12 = 0.1))
    expect_equal(deparse(res$expr), "0.1 * y[1L]")
    expect_equal(res$freeParams, character())

    # 5. Parameter kept free if not supplied
    res <- call_sub("k12 * Central")
    expect_equal(deparse(res$expr), "params[[\"k12\"]] * y[1L]")
    expect_equal(res$freeParams, "k12")

    # 6. Parameter kept free if non-scalar provided
    res <- call_sub("theta * Central", paramValues = list(theta = c(1,2,3)))
    expect_equal(deparse(res$expr), "params[[\"theta\"]] * y[1L]")
    expect_equal(res$freeParams, "theta")

    # 7. Reserved names untouched
    res <- call_sub("t + pi")
    expect_equal(deparse(res$expr), "t + pi")
    expect_equal(res$freeParams, character())
})
