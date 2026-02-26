# Test the Equations class

test_that("Equations object is created correctly", {
    eq <- equations("co", "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski")
    expect_s3_class(eq, "Equations")
})

test_that("Length and names methods work for Equations", {
    eq <- equations(c("co"), c("Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski"))
    expect_equal(length(eq), 1)
    expect_equal(names(eq), c("co"))
})

test_that("Print method for Equations works", {
    eq <- equations(c("co"), c("Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski"))
    expect_snapshot(print(eq))
})

test_that("Subsetting Equations object works", {
    eq <- equations(
        name = c("co", "Qliv"),
        expr = c("Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski", "Qhep+Qspl+Qgut")
    )
    eq_subset <- eq[1]
    expect_equal(length(eq_subset), 1)
    expect_equal(names(eq_subset), "co")
    expect_s3_class(eq_subset, "Equations")
})

test_that("Concatenating Equations objects works", {
    eq1 <- equations("co", "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski")
    eq2 <- equations("Qliv", "Qhep+Qspl+Qgut")
    combined_eq <- c(eq1, eq2)

    expect_equal(length(combined_eq), 2)
    expect_equal(names(combined_eq), c("co", "Qliv"))
    expect_s3_class(combined_eq, "Equations")
})

test_that("Equations with mismatched name and expr lengths throw error", {
    expect_error(equations(
        c("co", "Qliv"),
        "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski"
    ))
})

test_that("Empty Equations objects behave as expected", {
    eq <- empty_equation()
    expect_equal(length(eq), 0)

    eq2 <- equations(name = "co", expr = "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski")
    expect_equal(eq, eq2[FALSE])
    expect_equal(eq2, c(eq2, eq))
})

test_that("add_equation works in programmatic and interactive paths", {
    # Programmatic path
    eq <- equations(name = "co", expr = "Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski")
    M1 <- compartment_model() |>
        add_equation(eq = eq)

    expect_equal(length(M1$equations), 1)
    expect_equal(names(M1$equations), "co")
    expect_equal(M1$equations[[1]], quote(Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski))

    # Interactive path
    M2 <- compartment_model() |>
        add_equation(co = Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski)

    expect_equal(length(M2$equations), 1)
    expect_equal(names(M2$equations), "co")
    expect_equal(M2$equations[[1]], quote(Qadi+Qbon+Qhea+Qkid+Qliv+Qmus+Qski))
})