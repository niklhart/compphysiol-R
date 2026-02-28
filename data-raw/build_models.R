# Code to build predefined models (executed at package build time)
build_models <- function() {

    M <- compartment_model() |>
        add_compartment("test", 0) |>
        add_flow("test", "", rate = "ke * test") |>
        add_observable("Ctest", "2*test")

    list(test_model_sysdata = M)
}

