test_that("component low-level constructors trust canonical input", {
    expect_s3_class(
        .new_compartments(data.frame(name = c("A", "A"), volume = I(list(1, 1)))),
        "compphysiol_compartments"
    )
    expect_s3_class(
        .new_molecules(data.frame(
            name = "A", cmt = NA_character_, init = I(list(0)), type = "concentration"
        )),
        "compphysiol_molecules"
    )
    expect_s3_class(
        .new_transports(data.frame(
            from = "A", to = "B", molec = NA_character_,
            rate = I(list(quote(k * A))), const = I(list(quote(k))), type = "linear"
        )),
        "compphysiol_transports"
    )
    expect_s3_class(
        .new_reactions(data.frame(
            rate = I(list(quote(k * A))), const = I(list(quote(k))),
            type = "elementary", scale_cmt = "A", participants = I(list(data.frame()))
        )),
        "compphysiol_reactions"
    )
    expect_s3_class(.new_equations(list(x = quote(a))), "compphysiol_equations")
    expect_s3_class(.new_observables(list(x = quote(a))), "compphysiol_observables")
    expect_s3_class(.new_parameters(list(x = 1)), "compphysiol_parameters")
    expect_s3_class(.new_dosing(.dosing_data_frame(
        time = 0, amount = 1, rate = NA_real_, duration = NA_real_,
        molec = NA_character_, cmt = NA_character_
    )), "compphysiol_dosing")
})

test_that("component operations check only conflicts they introduce", {
    expect_error(
        c(compartments("A"), compartments("A")),
        "Duplicate compartment"
    )
    expect_error(
        c(equations(x = 1), equations(x = 2)),
        "unique"
    )
    expect_error(
        c(observables(x = 1), observables(x = 2)),
        "unique"
    )
    expect_error(
        c(parameters(x = 1), parameters(x = 2)),
        "unique"
    )
    expect_error(c(compartments("A"), molecules("x")), "compartments")
})

test_that("component subsetting restores classes directly", {
    expect_s3_class(compartments(c("A", "B"), volume = c(1, 2))[1],
                     "compphysiol_compartments")
    expect_s3_class(molecules(c("x", "y"))[1], "compphysiol_molecules")
    expect_s3_class(transports(c("A", "B"), c("B", "A"), const = "k")[1],
                     "compphysiol_transports")
    expect_s3_class(reactions("A -> B", cmt = c("A", "B"), const = "k")[1],
                     "compphysiol_reactions")
    expect_s3_class(dosing(c(0, 1), amount = 1)[1], "compphysiol_dosing")
})

test_that("named component subsetting rejects newly duplicated names", {
    expect_error(compartments(c("A", "B"), volume = 1)[c(1, 1)], "unique")
    expect_error(equations(a = 1, b = 2)[c(1, 1)], "unique")
    expect_error(observables(a = 1, b = 2)[c(1, 1)], "unique")
    expect_error(parameters(a = 1, b = 2)[c(1, 1)], "unique")

    compartments_abc <- compartments(c("A", "B", "C"), volume = 1)
    expect_identical(compartments_abc[-c(1, 1)], compartments_abc[-1])
    expect_identical(
        compartments_abc[c(TRUE, FALSE, TRUE)],
        compartments_abc[c(1, 3)]
    )

    parameters_abc <- parameters(a = 1, b = 2, c = 3)
    expect_identical(parameters_abc[-c(1, 1)], parameters_abc[-1])
    expect_identical(
        parameters_abc[c(TRUE, FALSE, TRUE)],
        parameters_abc[c(1, 3)]
    )
})

test_that("component subsetting rejects missing indices", {
    expect_error(compartments("A", volume = 1)[NA_integer_], "must not contain NA")
    expect_error(molecules("A")[NA_character_], "must not contain NA")
    expect_error(
        transports("A", "B", const = "k")[NA],
        "must not contain NA"
    )
    expect_error(
        reactions("A -> B", cmt = "cyt", const = "k")[c(TRUE, NA)],
        "must not contain NA"
    )
})

test_that("molecule-compartment states stay unique", {
    expect_error(
        molecules(c("A", "A"), cmt = c("cyt", "cyt")),
        "Molecule-compartment states must be unique"
    )
    expect_error(
        c(molecules("A", cmt = "cyt"), molecules("A", cmt = "cyt")),
        "Molecule-compartment states must be unique"
    )
    x <- molecules(c("A", "A"), cmt = c("cyt", "nuc"))
    expect_error(x[c(1, 1)], "unique")

    model <- compartment_model() |>
        add_compartment(c("cyt", "nuc"), volume = 1) |>
        add_molecule("A") |>
        add_molecule("A", cmt = "cyt")
    expect_error(wire(model, "cmt"), "Molecule-compartment states must be unique")
})

test_that("model add functions check explicitly supplied component classes", {
    expect_error(
        add_molecule(compartment_model(), molec = compartments("A")),
        "Molecules"
    )
    expect_error(
        add_reaction(compartment_model(), react = transports()),
        "Reactions"
    )
})
