test_that("function ionized_fractions() works for a neutral drug", {

    f <- ionized_fractions(pH = 7)

    expect_equal(f$n, 1)
    expect_equal(f$ani, 0)
    expect_equal(f$cat, 0)
})

test_that("function ionized_fractions() works for an acidic drug", {

    # equilibrium at pH = pKa
    f1 <- ionized_fractions(pH = 7, pKa_ani = 7)

    expect_equal(f1$n, 0.5)
    expect_equal(f1$ani, 0.5)
    expect_equal(f1$cat, 0)

    # very weak acid --> like neutral
    f2 <- ionized_fractions(pH = 7, pKa_ani = Inf)

    expect_equal(f2$n, 1)
    expect_equal(f2$ani, 0)
    expect_equal(f2$cat, 0)

    # diprotic acid with one large pKa --> same result as monoprotic acid
    f3 <- ionized_fractions(7, pKa_ani = c(7,Inf))

    expect_equal(f3$n, 0.5)
    expect_equal(f3$ani, 0.5)
    expect_equal(f3$cat, 0)
})

test_that("function ionized_fractions() works for a basic drug", {

    # equilibrium at pH = pKa
    f1 <- ionized_fractions(pH = 7, pKa_cat = 7)

    expect_equal(f1$n, 0.5)
    expect_equal(f1$ani, 0)
    expect_equal(f1$cat, 0.5)

    # very weak base --> like neutral
    f2 <- ionized_fractions(pH = 7, pKa_cat = -Inf)

    expect_equal(f2$n, 1)
    expect_equal(f2$ani, 0)
    expect_equal(f2$cat, 0)

    # diprotic base with one small pKa --> same result as monoprotic base
    f3 <- ionized_fractions(7, pKa_cat = c(-Inf,7))

    expect_equal(f3$n, 0.5)
    expect_equal(f3$ani, 0)
    expect_equal(f3$cat, 0.5)
})

test_that("function ionized_fractions() works for an ampholytic drug", {

    # check symmetric case, and the non-trivial mass balance
    f1 <- ionized_fractions(pH = 7, pKa_ani = 9, pKa_cat = 5)

    expect_equal(f1$ani, f1$cat)
    expect_equal(f1$n+f1$ani+f1$cat, 1, tolerance = 1e-10)

    # equilibrium at pKa
    f2 <- ionized_fractions(pH = 7, pKa_ani = 7, pKa_cat = 7)

    expect_equal(f2$n, f2$ani)
    expect_equal(f2$n, f2$cat)

    # equilibrium at pKa including zwitter ions
    f3 <- ionized_fractions(pH = 7, pKa_ani = 7, pKa_cat = 7, Kz = 1)

    expect_equal(f3$n,f3$z)
    expect_equal(f3$n+f3$z, f3$ani)
    expect_equal(f3$n+f3$z, f3$cat)
})

test_that("function ionized_fractions() works for a multiprotic ampholyte", {

    ## complex scenarios compared to httk R package
    # Nomenclature and behaviour of httk::calc_ionization() is a bit different:
    # In compphysiol, any cationic pKa must be less than any anionic pKa,
    # whereas in httk, they are simply reordered to fulfill this property.
    # Therefore, care must be taken to specify pKas in a consistent way.

    tol <- 5e-5

    # zwitter ion with two cationic pKa values
    f1   <- ionized_fractions(pH = 7, pKa_ani = 8, pKa_cat = c(4,6))

    expect_equal(f1$n,   0.8333, tolerance = tol)
    expect_equal(f1$ani, 0.08333, tolerance = tol)
    expect_equal(f1$cat, 0.08341, tolerance = tol)

    # zwitter ion with two anionic pKa values
    f2   <- ionized_fractions(pH = 7, pKa_ani = c(6,8), pKa_cat = 5)

    expect_equal(f2$n,   0.08326, tolerance = tol)
    expect_equal(f2$ani, 0.9159,  tolerance = tol)
    expect_equal(f2$cat, 0.0008326, tolerance = tol)
})
