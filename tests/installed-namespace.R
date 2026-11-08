# Run against the installed namespace in a fresh R session (also by R CMD check).
library(compphysiol)

stopifnot(
    isNamespace(environment(compphysiol::compartment_model)),
    !exists(".parameter_overlay_values", envir = .GlobalEnv, inherits = TRUE)
)

local({
    check_overlay <- function(model) {
        stopifnot(
            identical(names(model$parameters), c("V", "Q")),
            identical(model$parameters$V, 2),
            identical(model$parameters$Q, 3)
        )
    }

    check_overlay(
        compartment_model() |>
            add_parameter(CL = 1, V = 2) |>
            overlay_parameter(CL = NULL, Q = 3)
    )

    q <- 3
    v <- 2
    check_overlay(
        compartment_model() |>
            add_parameter(CL = 1, V = v) |>
            overlay_parameter(CL = NULL, Q = q)
    )

    # Constructor lookup must also ignore functions in the caller's environment.
    parameters <- function(...) stop("Caller constructor was used")
    check_overlay(
        compphysiol::compartment_model() |>
            compphysiol::add_parameter(CL = 1, V = v) |>
            compphysiol::overlay_parameter(CL = NULL, Q = q)
    )
})
