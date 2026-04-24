# Testing the `initials()` function

test_that("initials retrieves initial concentrations/amounts if volumes are specified", {
    # Initial conditions specified as amounts, volumes provided
    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc"), volume = 1[L]) |>
        add_molecule(c("A", "B"), unit = "g", type = "amount") |>
        wire()

    A0 <- initials(model, type = "a[] only")
    C0 <- initials(model, type = "c[] only")

    expect_equal(names(A0), c("a[A,cyt]", "a[A,nuc]", "a[B,cyt]", "a[B,nuc]"))
    expect_equal(unname(A0), units::set_units(rep(0, 4), "g"))

    expect_equal(names(C0), c("c[A,cyt]", "c[A,nuc]", "c[B,cyt]", "c[B,nuc]"))
    expect_equal(unname(C0), units::set_units(rep(0, 4), "g/L"))

    # Initial conditions specified as concentrations, volumes provided
    model2 <- compartment_model() |>
        add_compartment(c("cyt", "nuc"), volume = 1[L]) |>
        add_molecule(c("A", "B"), unit = "g/L", type = "concentration") |>
        wire()

    A02 <- initials(model2, type = "a[] only")
    C02 <- initials(model2, type = "c[] only")

    expect_equal(names(A02), c("a[A,cyt]", "a[A,nuc]", "a[B,cyt]", "a[B,nuc]"))
    expect_equal(unname(A02), units::set_units(rep(0, 4), "g"))

    expect_equal(names(C02), c("c[A,cyt]", "c[A,nuc]", "c[B,cyt]", "c[B,nuc]"))
    expect_equal(unname(C02), units::set_units(rep(0, 4), "g/L"))
})

test_that("initials may fail if some volumes unspecified", {

    # Initial conditions specified as amounts, no volumes provided
    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc")) |>
        add_molecule(c("A", "B"), unit = "g", type = "amount") |>
        wire()

    A0 <- initials(model, type = "a[] only")
    expect_equal(names(A0), c("a[A,cyt]", "a[A,nuc]", "a[B,cyt]", "a[B,nuc]"))
    expect_equal(unname(A0), units::set_units(rep(0, 4), "g"))

    expect_error(initials(model, type = "c[] only"), "Cannot extract concentration initials")

    A0b <- initials(model, type = "c[] or a[]")
    expect_identical(A0, A0b)

})
