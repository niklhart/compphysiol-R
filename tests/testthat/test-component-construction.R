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
