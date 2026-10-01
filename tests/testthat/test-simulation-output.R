output_test_model <- function() {
    compartment_model() |>
        add_compartment("Central", volume = 10 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", "", const = "k") |>
        add_parameter(k = 0.2 [1/h]) |>
        add_observable(A = a[drug, Central], C = c[drug, Central])
}

test_that("direct simulation returns long observables and rectangular states", {
    m <- output_test_model()
    out <- simulate(m, time = c(0, 1, 2) [h])
    expect_s3_class(out, "SimulationResult")
    expect_named(out$observables, c("time", "observable", "value"))
    expect_equal(nrow(out$observables), 6L)
    expect_s3_class(out$observables$value, "mixed_units")
    expect_equal(nrow(out$states), 3L)
    wide <- as_observables_wide(out)
    expect_equal(wide$A, out$states$a_drug_Central)
    expect_equal(wide$C, wide$A / with_units(10 [L]))
    expect_identical(as_observables_long(out), out$observables)
    m$observables <- observables(A = a[drug, Central], B = a[drug, Central] * 2)
    same <- simulate(m, time = c(0, 1) [h])
    expect_s3_class(same$observables$value, "units")
})

test_that("experiments preserve sparse schedule order and parameter overrides", {
    e <- experiment(parameters = parameters(k = 0.1 [1/h]), measurements = data.frame(
        time = with_units(c(120, 60, 120) [min]), observable = c("C", "A", "C")))
    out <- simulate(output_test_model(), experiment = e)
    expect_identical(out$observables$time, e$measurements$time)
    expect_identical(out$observables$observable, e$measurements$observable)
    expect_equal(as.numeric(out$observables$value[[1]]), 10 * exp(-0.2), tolerance = 1e-6)
    expect_equal(as.numeric(out$states$time), c(0, 60, 120))
    expect_error(as_observables_wide(out), "duplicate")
    e$measurements <- e$measurements[1:2, ]
    wide <- as_observables_wide(simulate(output_test_model(), experiment = e))
    expect_true(is.na(wide$A[1]))
    expect_true(is.na(wide$C[2]))
    expect_s3_class(wide$A, "units")
    expect_s3_class(wide$C, "units")
})

test_that("experiment dosing replaces model dosing and collections retain names", {
    m <- output_test_model() |> add_dosing(time = 0 [h], amount = 999 [mg])
    schedule <- data.frame(time = with_units(c(0, 1) [h]), observable = "A")
    a <- experiment(measurements = schedule)
    b <- experiment(measurements = schedule, dosing = dosing(time = 0 [h], amount = 10 [mg]))
    out <- simulate(m, experiment = experiments(control = a, treated = b))
    expect_named(out, c("control", "treated"))
    expect_equal(as.numeric(out$control$observables$value[1]), 100)
    expect_equal(as.numeric(out$treated$observables$value[1]), 110)
    expect_error(simulate(m, experiment = a, time = 0:1), "cannot.*time")
    expect_error(simulate(m, experiment = a, parameters = list()), "cannot.*parameters")
    expect_error(simulate(m, experiment = experiment()), "measurement")
    a$measurements$observable <- "unknown"
    expect_error(simulate(m, experiment = a), "Unknown observable")
})

test_that("ODE and compiled experiment simulation reuse the sparse contract", {
    m <- output_test_model()
    e <- experiment(measurements = data.frame(time = with_units(c(0, 1) [h]), observable = "A"),
                    dosing = dosing(time = 0 [h], amount = 10 [mg]))
    expected <- simulate(m, experiment = e)
    for (obj in list(to_ode_model(m), to_compiled_ode_model(m))) {
        actual <- simulate(obj, experiment = e)
        expect_equal(actual$observables, expected$observables, tolerance = 1e-6)
        e2 <- e
        e2$dosing <- dosing()
        expect_equal(as.numeric(simulate(obj, experiment = e2)$observables$value[1]), 100)
    }
})

test_that("wide helpers handle unit-free outputs and no observables", {
    m <- compartment_model() |> add_compartment("Central", volume = NA_real_) |>
        add_molecule("drug", initial = 1, type = "amount") |> add_observable(A = a[drug, Central])
    out <- simulate(m, time = 0:1)
    expect_type(out$observables$value, "double")
    expect_equal(as_observables_wide(out)$A, c(1, 1))
    m$observables <- observables()
    out <- simulate(m, time = 0:1)
    expect_null(out$observables)
    expect_equal(as_observables_wide(out)$time, out$states$time)
    expect_equal(nrow(as_observables_long(out)), 0L)
})

test_that("experiment collections prepare inactive infusion structures", {
    m <- output_test_model()
    schedule <- data.frame(time = with_units(c(0, 1, 2) [h]), observable = "A")
    a <- experiment(measurements = schedule,
        dosing = dosing(time = 0 [h], amount = 10 [mg], duration = 1 [h], cmt = "Central"))
    b <- experiment(measurements = schedule)
    out <- simulate(m, experiment = experiments(infusion = a, control = b))
    expect_identical(names(out$infusion$states), names(out$control$states))
    expect_true(any(grepl("Depot", names(out$control$states))))
    expect_equal(out$control$observables, simulate(m, experiment = b)$observables)
    expect_gt(as.numeric(out$infusion$observables$value[2]), as.numeric(out$control$observables$value[2]))
    compiled <- to_compiled_ode_model(m |> add_dosing(dose = a$dosing))
    actual <- simulate(compiled, experiment = experiments(infusion = a, control = b))
    expect_equal(actual$infusion$observables, out$infusion$observables, tolerance = 1e-6)
    expect_equal(actual$control$observables, out$control$observables, tolerance = 1e-6)
    expect_error(simulate(to_compiled_ode_model(m), experiment = a), "absent")
})

test_that("only scheduled observable-time pairs are evaluated", {
    m <- to_ode_model(output_test_model())
    e <- experiment(measurements = data.frame(time = with_units(c(1, 1) [h]), observable = "A"))
    info <- .to_deSolve(m, dimensions = list(time = "h", mass = "mg"))
    calls <- 0L
    original <- info$obsFuncs$A
    info$obsFuncs$A <- function(t, y, params) {
        calls <<- calls + length(t)
        original(t, y, params)
    }
    info$obsFuncs$C <- function(...) stop("Unrequested observable was evaluated")
    attr(m, "measurement_schedule") <- e$measurements
    out <- .simulation_solve_ode_model(m, info, with_units(c(0, 1) [h]),
        dimensions = list(time = "h", mass = "mg"), parameters = m$parameters)
    expect_equal(calls, 1L)
    expect_equal(nrow(out$observables), 2L)
})

test_that("single initial-time measurements and intermediate dosing work", {
    m <- output_test_model()
    e <- experiment(measurements = data.frame(time = with_units(0 [h]), observable = "A"))
    expect_equal(as.numeric(simulate(m, experiment = e)$observables$value), 100)
    e$measurements$time <- with_units(2 [h])
    e$dosing <- dosing(time = 1 [h], amount = 10 [mg])
    out <- simulate(m, experiment = e)
    expect_equal(as.numeric(out$observables$value), 100 * exp(-0.4) + 10 * exp(-0.2), tolerance = 1e-6)
})

test_that("constant observables repeat and reserved names remain valid in long output", {
    m <- output_test_model() |> add_observable(K = k, time = a[drug, Central])
    e <- experiment(measurements = data.frame(time = with_units(c(1, 2) [h]), observable = "K"))
    out <- simulate(m, experiment = e)
    expect_equal(as.numeric(out$observables$value), c(0.2, 0.2))
    direct <- simulate(m, time = c(0, 1) [h])
    values <- direct$observables$value[direct$observables$observable == "time"]
    expect_equal(as.numeric(units::as_units(values)), c(100, 100 * exp(-0.2)), tolerance = 1e-6)
    expect_error(as_observables_wide(direct), "collide")
})

test_that("compiled experiments retain defaults and do not modify direct dosing", {
    m <- output_test_model()
    m$molecules <- molecules("drug", cmt = "Central", initial = "initial", type = "amount")
    m <- m |> add_parameter(initial = 100 [mg]) |> add_dosing(time = 0 [h], amount = 20 [mg])
    compiled <- to_compiled_ode_model(m)
    e <- experiment(measurements = data.frame(time = with_units(c(0, 1) [h]), observable = "A"))
    first <- simulate(compiled, experiment = e)
    expect_equal(simulate(compiled, experiment = e)$observables, first$observables)
    direct <- simulate(compiled, time = c(0, 1) [h])
    expect_equal(as.numeric(as_observables_wide(direct)$A[1]), 120)
    e$dosing <- dosing(time = 0 [h], amount = 1 [L])
    expect_error(simulate(compiled, experiment = e), "unit")
})

test_that("wide output keeps replicate keys and dimensionless mixed observables", {
    out <- structure(list(states = data.frame(time = 0:1), observables = data.frame(
        time = c(0, 1, 0, 1), rep = c(1L, 1L, 2L, 2L), observable = "A")),
        class = "SimulationResult")
    out$observables$value <- with_units((1:4) [mg])
    wide <- as_observables_wide(out)
    expect_equal(wide$rep, c(1L, 1L, 2L, 2L))
    expect_equal(wide$A, with_units((1:4) [mg]))
    m <- output_test_model() |> add_observable(F = 1)
    wide <- as_observables_wide(simulate(m, time = c(0, 1) [h]))
    expect_equal(as.numeric(wide$F), c(1, 1))
})
