# Code to build predefined models (executed at package build time)
build_models <- function() {

    M <- CompartmentModel$new()

    # compartments (can be vectorized later)
    M$addCompartment("test", 0)
    M$addReaction("test", "", "ke * test", "deg")
    M$addObservable("Ctest", "2*test")

    list(test_model_sysdata = M)
}

