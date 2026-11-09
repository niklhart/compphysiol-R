test_that("fixed CV is normalized to the canonical lognormal spread", {
    d <- lognormal(median = 10 [L], cv = 0.3)
    expect_equal(d, lognormal(median = 10 [L], sdlog = sqrt(log1p(0.3^2))))
    expect_equal(lognormal(cv = 0)$sdlog, 0)
    expect_equal(lognormal(sdlog = NULL, cv = 0.3)$sdlog, sqrt(log1p(0.09)))
    expect_equal(lognormal(cv = 1e200)$sdlog, sqrt(2 * log(1e200)))
    expect_equal(lognormal(cv = 0.3 [1])$sdlog, d$sdlog)
    expect_error(lognormal(), "sdlog or cv")
    expect_error(lognormal(cv = -0.1), "non-negative")
    expect_error(lognormal(cv = 1 [L]), "dimensionless")
    expect_error(lognormal(cv = "omega"), "fixed dimensionless")
    expect_error(lognormal(cv = Inf), "finite")
    expect_error(lognormal(cv = c(0.1, 0.2)), "scalar")
    expect_equal(lognormal(sdlog = 0.5, cv = stop("unused"))$sdlog, 0.5)
    expect_identical(lognormal(sdlog = "omega", cv = -1)$sdlog, "omega")
    expect_identical(lognormal(NULL, 0.2, "individual")$level, "individual")
})

test_that("printing lognormal distributions includes percent CV", {
    expect_output(print(statistical_model(V = lognormal(median = 10 [L], cv = 0.3))),
                  " (CV = 30%);", fixed = TRUE)
    expect_output(print(statistical_model(V = lognormal(sdlog = 0))),
                  " (CV = 0%);", fixed = TRUE)
    expect_output(print(statistical_model(V = lognormal(sdlog = "omega"))),
                  "sdlog = omega; level =", fixed = TRUE)
    expect_output(print(statistical_model(V = lognormal(sdlog = sqrt(log(2))))),
                  " (CV = 100%);", fixed = TRUE)
})
