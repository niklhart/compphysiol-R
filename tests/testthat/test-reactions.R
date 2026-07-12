# Test for reaction-related functions

expect_states <- function(x, molec, cmt) {
    expect_s3_class(x, "States")
    expect_equal(
        as.data.frame(x),
        data.frame(
            molec = molec,
            cmt = cmt,
            stringsAsFactors = FALSE
        )
    )
}

expect_reaction_participants <- function(reaction, role, molec, cmt, stoich = rep(1, length(molec))) {
    has_participants <- "participants" %in% names(reaction)
    expect_true(has_participants)
    if (!has_participants) return(invisible(NULL))

    participants <- as.data.frame(reaction$participants[[1]])
    has_columns <- all(c("role", "molec", "cmt", "stoich") %in% names(participants))
    expect_true(has_columns)
    if (!has_columns) return(invisible(NULL))

    participants <- participants[participants$role == role, c("molec", "cmt", "stoich")]
    rownames(participants) <- NULL

    expect_equal(
        participants,
        data.frame(
            molec = molec,
            cmt = cmt,
            stoich = stoich,
            stringsAsFactors = FALSE
        )
    )
}

test_that("Reactions are created correctly", {
    # Elementary reaction
    r1 <- reactions(input = c("A","B"), output = "C", const = "k1")
    expect_equal(length(r1), 1)
    expect_false(any(c("input", "output", "cmt") %in% names(r1)))
    expect_reaction_participants(r1, "input", c("A", "B"), c(NA_character_, NA_character_))
    expect_reaction_participants(r1, "output", "C", NA_character_)
    expect_equal(r1$scale_cmt, NA_character_)
    expect_equal(r1$const[[1]], quote(k1))
    expect_equal(r1$rate[[1]], quote(k1 * c[A] * c[B]))
    expect_equal(r1$type, "elementary")

    # Complex reaction
    r2 <- reactions(input = "A", output = "B", rate = "k1 * c[A]*c[B]/(c[B]+K)")
    expect_equal(length(r2), 1)
    expect_reaction_participants(r2, "input", "A", NA_character_)
    expect_reaction_participants(r2, "output", "B", NA_character_)
    expect_equal(r2$scale_cmt, NA_character_)
    expect_equal(r2$rate[[1]], quote(k1 * c[A] * c[B] / (c[B] + K)))
    expect_equal(r2$const[[1]], NULL)
    expect_equal(r2$type, "complex")
})

test_that("States require explicit molecule and compartment names", {
    s <- state(molec = "R", cmt = "membrane")

    expect_states(s, "R", "membrane")
    expect_snapshot(print(s))

    expect_error(state("R", "membrane"), "molec.*cmt|named")
    expect_error(state(molec = "R"), "cmt")
    expect_error(state(cmt = "membrane"), "molec")
    expect_error(state(molec = "R", cmt = "membrane", stoich = 2), "stoich|unused|named")
})

test_that("States support vectorized molecule-compartment pairs", {
    s <- state(
        molec = c("R", "L"),
        cmt = c("membrane", "plasma")
    )

    expect_equal(length(s), 2)
    expect_states(s, c("R", "L"), c("membrane", "plasma"))
    expect_snapshot(print(s))
})

test_that("Programmatic cross-compartment reactions store localized participants", {
    r <- reactions(
        input = state(
            molec = c("R", "L"),
            cmt = c("membrane", "plasma")
        ),
        output = state(molec = "LR", cmt = "membrane"),
        scale_cmt = "membrane",
        const = "kon"
    )

    expect_equal(length(r), 1)
    expect_reaction_participants(r, "input", c("R", "L"), c("membrane", "plasma"))
    expect_reaction_participants(r, "output", "LR", "membrane")
    expect_equal(r$scale_cmt, "membrane")
    expect_equal(r$const[[1]], quote(kon))
    expect_equal(r$rate[[1]], quote(kon * c[R, membrane] * c[L, plasma]))
    expect_equal(r$type, "elementary")
})

test_that("Programmatic reactions normalize repeated participants", {
    r <- reactions(
        input = c(
            state(molec = "A", cmt = "cyt"),
            state(molec = "A", cmt = "cyt")
        ),
        output = state(molec = "B", cmt = "cyt"),
        const = "k"
    )

    expect_reaction_participants(r, "input", "A", "cyt", 2)
    expect_reaction_participants(r, "output", "B", "cyt")
    expect_equal(r$scale_cmt, "cyt")
})

test_that("Cross-compartment reactions require an explicit involved scale compartment", {
    expect_error(
        reactions(
            input = state(
                molec = c("R", "L"),
                cmt = c("membrane", "plasma")
            ),
            output = state(molec = "LR", cmt = "membrane"),
            const = "kon"
        ),
        "scale_cmt"
    )

    expect_error(
        reactions(
            input = state(
                molec = c("R", "L"),
                cmt = c("membrane", "plasma")
            ),
            output = state(molec = "LR", cmt = "membrane"),
            scale_cmt = "interstitium",
            const = "kon"
        ),
        "scale_cmt.*involved|involved.*scale_cmt"
    )
})

test_that("Elementary cross-compartment reactions infer scale from unique input compartment", {
    r <- reactions(
        input = state(molec = "A", cmt = "plasma"),
        output = state(molec = "B", cmt = "membrane"),
        const = "kAB"
    )

    expect_equal(r$scale_cmt, "plasma")
    expect_equal(r$rate[[1]], quote(kAB * c[A, plasma]))
})

test_that("Complex cross-compartment reactions require explicit scale compartment", {
    expect_error(
        reactions(
            input = state(molec = "A", cmt = "plasma"),
            output = state(molec = "B", cmt = "membrane"),
            rate = "kAB * c[A, plasma]"
        ),
        "scale_cmt"
    )
})

test_that("Same-compartment state reactions infer scale compartment", {
    r <- reactions(
        input = state(molec = "A", cmt = "cyt"),
        output = state(molec = "B", cmt = "cyt"),
        const = "kAB"
    )

    expect_equal(r$scale_cmt, "cyt")
    expect_equal(r$rate[[1]], quote(kAB * c[A, cyt]))
})

test_that("Character reactions remain same-compartment shorthand", {
    r <- reactions(input = c("A", "B"), output = "C", cmt = "cyt", const = "k")

    expect_reaction_participants(r, "input", c("A", "B"), c("cyt", "cyt"))
    expect_reaction_participants(r, "output", "C", "cyt")
    expect_equal(r$scale_cmt, "cyt")
    expect_equal(r$const[[1]], quote(k))
    expect_equal(r$rate[[1]], quote(k * c[A, cyt] * c[B, cyt]))
    expect_equal(r$type, "elementary")
})

test_that("Empty reactions are handled correctly", {
    r1 <- reactions()
    r2 <- reactions(input = "A", output = "B", const = "k")

    expect_equal(length(r1), 0)
    expect_equal(r2, c(r2, r1))
})

test_that("Multiple reactions can be combined and subsetted", {
    r1 <- reactions(input = "A", output = "B", const = "kAB")
    r2 <- reactions(input = "B", output = "A", rate = "kBA*c[B]")

    r12 <- c(r1, r2)
    r1s <- r12[1]

    expect_equal(length(r12), 2)
    expect_reaction_participants(r12[1], "input", "A", NA_character_)
    expect_reaction_participants(r12[1], "output", "B", NA_character_)
    expect_reaction_participants(r12[2], "input", "B", NA_character_)
    expect_reaction_participants(r12[2], "output", "A", NA_character_)
    expect_equal(r1s,r1)
})

test_that("Vectorized reaction creation with compartment substitution works correctly", {

    # Elementary reactions with substitution
    r1 <- reactions(input = "A", output = "B", cmt = c("a","b"), const = "k{cmt}")

    expect_equal(length(r1), 2)
    expect_reaction_participants(r1[1], "input", "A", "a")
    expect_reaction_participants(r1[1], "output", "B", "a")
    expect_reaction_participants(r1[2], "input", "A", "b")
    expect_reaction_participants(r1[2], "output", "B", "b")
    expect_equal(r1$scale_cmt, c("a", "b"))
    expect_equal(r1$const[[1]], quote(ka))
    expect_equal(r1$const[[2]], quote(kb))
    expect_equal(r1$rate[[1]], quote(ka * c[A, a]))
    expect_equal(r1$rate[[2]], quote(kb * c[A, b]))

    # Complex reaction with substitution
    r2 <- reactions(input = "A", output = "B", cmt = c("a","b"), rate = "k{cmt}*c[A]")

    expect_equal(length(r2), 2)
    expect_reaction_participants(r2[1], "input", "A", "a")
    expect_reaction_participants(r2[1], "output", "B", "a")
    expect_reaction_participants(r2[2], "input", "A", "b")
    expect_reaction_participants(r2[2], "output", "B", "b")
    expect_equal(r2$scale_cmt, c("a", "b"))
    expect_equal(r2$rate[[1]], quote(ka * c[A, a]))
    expect_equal(r2$rate[[2]], quote(kb * c[A, b]))
})

test_that("Reaction printing works correctly", {
    # same-compartment reaction
    r1 <- reactions(input = c("A", "B"), output = "C", cmt = "cyt", const = "k")
    expect_snapshot(print(r1))

    # wildcard compartment reaction
    r2 <- reactions(input = "A", output = "B", const = "kAB")
    expect_snapshot(print(r2))

    # cross-compartment reaction with inferred scale
    r3 <- reactions(
        input = state(molec = "A", cmt = "plasma"),
        output = state(molec = "B", cmt = "membrane"),
        const = "kAB"
    )
    expect_snapshot(print(r3))

    # cross-compartment reaction with explicit scale
    r4 <- reactions(
        input = state(
            molec = c("L", "R"),
            cmt = c("plasma", "membrane")
        ),
        output = state(molec = "LR", cmt = "membrane"),
        scale_cmt = "membrane",
        const = "kon"
    )
    expect_snapshot(print(r4))
})

test_that("Reactions can be added to compartment models", {
    model <- compartment_model() |>
        add_reaction(input = "A", output = "B", const = "k1") |>
        add_reaction(input = "B", output = "C", rate = "k2 * c[B]")

    expect_equal(length(model$reactions), 2)
    expect_reaction_participants(model$reactions[1], "input", "A", NA_character_)
    expect_reaction_participants(model$reactions[1], "output", "B", NA_character_)
    expect_reaction_participants(model$reactions[2], "input", "B", NA_character_)
    expect_reaction_participants(model$reactions[2], "output", "C", NA_character_)
    expect_equal(model$reactions$const[[1]], quote(k1))
    expect_equal(model$reactions$const[[2]], NULL)
    expect_equal(model$reactions$rate[[1]], quote(k1 * c[A]))
    expect_equal(model$reactions$rate[[2]], quote(k2 * c[B]))
})
