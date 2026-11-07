test_that("structure inspection preserves classes at every storage length", {
    examples <- list(
        compartments("central", volume = 5 [L]),
        molecules("drug"),
        transports("central", "peripheral", const = 1 [1/h]),
        reactions(input = "drug", output = "metabolite", const = "k"),
        state(molec = "drug", cmt = "central")
    )
    for (example in examples) {
        columns <- length(unclass(example))
        for (n in unique(c(0L, 1L, columns, columns + 1L))) {
            # Repeated entries exercise storage traversal independently of
            # public constructor normalization and name uniqueness.
            storage <- as.data.frame(example)[rep(1L, n), , drop = FALSE]
            class(storage) <- class(example)
            before <- storage
            output <- capture.output(result <- str(storage))
            expect_null(result)
            expect_match(output[1], class(example)[1], fixed = TRUE)
            expect_true(any(grepl(names(unclass(example))[1], output, fixed = TRUE)))
            expect_identical(storage, before)
            expect_error(storage[[1]], "as.data.frame.*inspect")
        }
    }
})

test_that("inspection retains attributes and honors traversal controls", {
    x <- compartments(c("central", "peripheral"), volume = 5 [L])
    attr(x, "note") <- "inspection metadata"
    output <- capture.output(str(x))
    expect_true(any(grepl("inspection metadata", output, fixed = TRUE)))
    expect_true(any(grepl("[L]", output, fixed = TRUE)))
    output <- capture.output(str(x, max.level = 0, give.attr = FALSE))
    expect_length(output, 1L)
    expect_match(output, "compphysiol_compartments", fixed = TRUE)
    expect_false(any(grepl("central", output, fixed = TRUE)))
    output <- capture.output(str(x, list.len = 1, give.attr = FALSE))
    expect_true(any(grepl("list output truncated", output, fixed = TRUE)))
    expect_false(any(grepl("$ volume", output, fixed = TRUE)))
    output <- capture.output(str(list(component = x), max.level = 1, give.attr = FALSE))
    expect_true(any(grepl("compphysiol_compartments", output, fixed = TRUE)))
    expect_false(any(grepl("central", output, fixed = TRUE)))
})

test_that("experiment inspection shows its class and cached attributes", {
    x <- c(treatment = experiment(parameters = parameters(BW = 70 [kg])),
           control = experiment())
    output <- capture.output(str(x))
    expect_match(output[1], "compphysiol_experiments", fixed = TRUE)
    expect_true(any(grepl("component_homogeneity", output, fixed = TRUE)))
    expect_true(any(grepl("BW", output, fixed = TRUE)))
    expect_true(any(grepl("[kg]", output, fixed = TRUE)))
    expect_error(x[[1]], "as.list.*inspect")
})
