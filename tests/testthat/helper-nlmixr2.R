nlmixr2_test_problem <- function() {
    model <- compartment_model() |>
        add_compartment(name = "Central", volume = 1) |>
        add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
        add_transport(from = "Central", to = NULL, molec = "drug",
            rate = "CL * c[drug, Central]"
        ) |>
        add_observable(C = c[drug, Central])
    studies <- c(
        first = experiment(observations = observation_data(
            time = c(1, 2, 4), observable = "C", value = c(82, 67, 45)
        )),
        second = experiment(observations = observation_data(
            time = c(1, 2, 4), observable = "C", value = c(78, 61, 37)
        ))
    )
    estimation_problem(
        model, studies,
        c(
            CL_pop = parameter_spec(0.2, lower = 0.001, transform = "log"),
            omega_CL = parameter_spec(0.2, lower = 0.001, transform = "log"),
            sigma = parameter_spec(2, lower = 0.001, transform = "log")
        ),
        statistical_model(
            CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
            C = normal(sd = "sigma")
        )
    )
}


# Lazy per-file fixtures share conversion and native fitting, never mutable fits.
interop_fixture <- local({
    cache <- new.env(parent = emptyenv())
    function(family = "normal") {
        if (exists(family, cache, inherits = FALSE)) return(cache[[family]])
        model <- compartment_model() |>
            add_compartment(name = "Central", volume = 1) |>
            add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
            add_transport(from = "Central", to = NULL, molec = "drug", rate = "CL * c[drug, Central]") |>
            add_observable(C = CL)
        ln <- family == "lognormal"
        problem <- estimation_problem(model,
            c(first = experiment(observations = observation_data(time = c(1,2,4), observable = "C", value = c(9,10.5,11))),
              second = experiment(observations = observation_data(time = c(1,2,4), observable = "C", value = c(8.5,9.5,10)))),
            c(mu = parameter_spec(10), omega = parameter_spec(if (ln) .2 else 1),
              sigma = parameter_spec(if (ln) .2 else 2)),
            if (ln) statistical_model(CL = lognormal(median = "mu", sdlog = "omega"), C = lognormal(sdlog = "sigma")) else
                statistical_model(CL = normal(mean = "mu", sd = "omega"), C = normal(sd = "sigma")))
        export <- to_nlmixr2(problem)
        fit <- suppressMessages(nlmixr2est::nlmixr2(export$model, export$data, "focei",
            control = nlmixr2est::foceiControl(print = 0, maxOuterIterations = 0, covMethod = "")))
        cache[[family]] <- list(export = export, fit = fit)
        cache[[family]]
    }
})

independent_interop_nll <- function(family) {
    ln <- family == "lognormal"
    sum(vapply(list(c(9,10.5,11), c(8.5,9.5,10)), function(y) {
        transformed <- if (ln) log(y) else y
        location <- if (ln) log(10) else 10
        variance <- if (ln) .04 else 1
        sd <- if (ln) .2 else 2
        covariance <- diag(sd^2, 3) + matrix(variance,3,3)
        r <- transformed-location
        as.numeric((3*log(2*pi)+determinant(covariance,logarithm=TRUE)$modulus+
            t(r)%*%solve(covariance,r))/2) + if (ln) sum(log(y)) else 0
    },numeric(1)))
}

