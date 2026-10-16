test_that("component low-level constructors defer intrinsic validation", {
    compartments <- .new_compartments(data.frame(
        name = c("A", "A"), volume = I(list(1, 1))
    ))
    expect_s3_class(compartments, "compphysiol_compartments")
    expect_error(.validate_compartments(compartments), "Duplicate")

    molecules <- .new_molecules(data.frame(
        name = "A", cmt = NA_character_, init = I(list(0)), type = "invalid"
    ))
    expect_s3_class(molecules, "compphysiol_molecules")
    expect_error(.validate_molecules(molecules), "representation")

    transports <- .new_transports(data.frame(
        from = NA_character_, to = "B", molec = NA_character_,
        rate = I(list(quote(k * A))), const = I(list(quote(k))), type = "linear"
    ))
    expect_s3_class(transports, "compphysiol_transports")
    expect_error(.validate_transports(transports), "source compartment")

    reactions <- .new_reactions(data.frame(
        rate = I(list(quote(k * A))), const = I(list(quote(k))),
        type = "elementary", scale_cmt = "A", participants = I(list(data.frame()))
    ))
    expect_s3_class(reactions, "compphysiol_reactions")
    expect_error(.validate_reactions(reactions), "participants")

    equations <- .new_equations(list(x = quote(a), x = quote(b)))
    expect_s3_class(equations, "compphysiol_equations")
    expect_error(.validate_equations(equations), "unique")

    observables <- .new_observables(list(x = quote(a), x = quote(b)))
    expect_s3_class(observables, "compphysiol_observables")
    expect_error(.validate_observables(observables), "unique")

    parameters <- .new_parameters(list(x = 1, x = 2))
    expect_s3_class(parameters, "compphysiol_parameters")
    expect_error(.validate_parameters(parameters), "unique")

    dosing <- .new_dosing(.dosing_data_frame(
        time = 0, amount = "invalid", rate = NA_real_, duration = NA_real_,
        molec = NA_character_, cmt = NA_character_
    ))
    expect_s3_class(dosing, "compphysiol_dosing")
    expect_error(.validate_dosing(dosing), "numeric")
})

test_that("component subsetting trusts the established class contract", {
    comp <- compartments(c("A", "B"), volume = c(1, 2))
    molec <- molecules(c("x", "y"))
    trans <- transports(c("A", "B"), c("B", "A"), const = "k")
    react <- reactions("A -> B", cmt = c("A", "B"), const = "k")
    dose <- dosing(c(0, 1), amount = 1)

    local_mocked_bindings(
        .validate_compartments = function(...) stop("unexpected validation"),
        .validate_molecules = function(...) stop("unexpected validation"),
        .validate_transports = function(...) stop("unexpected validation"),
        .validate_reactions = function(...) stop("unexpected validation"),
        .validate_dosing = function(...) stop("unexpected validation"),
        .package = "compphysiol"
    )

    expect_no_error(comp[1])
    expect_no_error(molec[1])
    expect_no_error(trans[1])
    expect_no_error(react[1])
    expect_no_error(dose[1])
})
