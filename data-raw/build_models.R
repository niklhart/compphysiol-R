# Code to build predefined models (executed at package build time)
build_models <- function() {

    M <- compartment_model() |>
        add_compartment("test", volume = 10) |>
        add_transport("test", "", const = "ke") |>
        add_observable(Ctest = c[test])

    list(test_model_sysdata = M)
}

