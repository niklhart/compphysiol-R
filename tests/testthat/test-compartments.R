
test_that("Adding compartments works", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", initial = 10)
    M$addCompartment("Peripheral", initial = 0)

    compartments <- M$getStateNames()
    expect_equal(compartments, c("Central", "Peripheral"))

    initials <- M$getInitials()
    expect_equal(initials, c(10, 0))
})

test_that("Adding reactions works", {
    M <- CompartmentModel$new()
    M$addCompartment("Central", 10)
    M$addCompartment("Peripheral", 0)

    M$addReaction("Central", "Peripheral", "k12 * Central", "k12_forward")
    expect_equal(length(M$reactions), 1)
    r <- M$reactions[[1]]
    expect_equal(r$from, "Central")
    expect_equal(r$to, "Peripheral")
    expect_equal(r$rate, "k12 * Central")
})
