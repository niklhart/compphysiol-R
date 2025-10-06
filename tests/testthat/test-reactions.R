
test_that("Reaction linearity detection works", {
    stateNames <- c("C1","C2")

    r1 <- Reaction$new(from = "C1", to = "C2", rate = quote(k12 * C1))
    expect_true(r1$isLinear(stateNames))
    expect_equal(r1$rateConstant(stateNames), "k12")

    r2 <- Reaction$new(from = "C1", to = "C2", rate = quote(C1))
    expect_true(r2$isLinear(stateNames))
    expect_equal(r2$rateConstant(stateNames), "1")

    r3 <- Reaction$new(from = "C1", to = "C2", rate = quote(C1 * C2 * k12))
    expect_false(r3$isLinear(stateNames))
    expect_true(is.na(r3$rateConstant(stateNames)))

    r4 <- Reaction$new(from = "C1", to = "C2", rate = quote(k10))
    expect_false(r4$isLinear(stateNames)) # no state → not linear
    expect_true(is.na(r4$rateConstant(stateNames)))

    r5 <- Reaction$new(from = "C1", to = "C2", rate = quote(CL * C1 / V))
    expect_equal(r5$rateConstant(stateNames), "CL/V")

    r6 <- Reaction$new(from = "C1", to = "C2", rate = quote(CL * (C1 / V)))
    expect_equal(r6$rateConstant(stateNames), "CL * (1/V)")

})
