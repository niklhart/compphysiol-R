# Benchmark likelihood evaluation and parameter estimation across model representations.
#
# This script is intentionally not part of the automated test suite. Compilation,
# solver, and optimizer timings depend on the machine and toolchain; use the
# results as a local diagnostic, not as package performance guarantees.
#
# Run from an installed package with:
#   source(system.file("benchmarks", "estimation.R", package = "compphysiol"))
#
# Or run from the source tree after devtools::load_all(). The complete matrix is
# deliberately substantial. Set COMPPHYSIOL_BENCH_QUICK=true for a short smoke run.

library(compphysiol)

quick <- tolower(Sys.getenv("COMPPHYSIOL_BENCH_QUICK", "false")) %in%
    c("1", "true", "yes")
fixed_replicates <- as.integer(Sys.getenv(
    "COMPPHYSIOL_BENCH_ESTIMATION_REPLICATES",
    if (quick) "1" else "5"
))
if (is.na(fixed_replicates) || fixed_replicates < 1L) {
    stop("COMPPHYSIOL_BENCH_ESTIMATION_REPLICATES must be a positive integer.")
}

elapsed <- function(expr) {
    gc()
    timing <- system.time(value <- force(expr))
    list(value = value, seconds = unname(timing[["elapsed"]]))
}

one_compartment_model <- function() {
    compartment_model() |>
        add_compartment("Central", volume = 1 [L]) |>
        add_molecule("drug", cmt = "Central", initial = 100 [mg], type = "amount") |>
        add_transport("Central", NULL, molec = "drug", const = "k") |>
        add_observable(C = c[drug, Central]) |>
        add_parameter(k = 0.2 [1/h])
}

pbpk_model <- function() {
    sMD_PBPK_12CMT_wellstirred() |>
        add_parameter(
            BP = 1, CL = 5,
            Kadi = 1, Kbon = 1, Kgut = 1, Khea = 1, Kkid = 1,
            Kliv = 1, Klun = 1, Kmus = 1, Kski = 1, Kspl = 1,
            Qadi = 0.5, Qbon = 0.5, Qgut = 0.5, Qhea = 0.5,
            Qkid = 0.5, Qliv = 1, Qmus = 0.5, Qski = 0.5, Qspl = 0.5,
            Vadi = 1, Vart = 1, Vbon = 1, Vgut = 1, Vhea = 1,
            Vkid = 1, Vliv = 1, Vlun = 1, Vmus = 1, Vski = 1,
            Vspl = 1, Vven = 1
        )
}

scenario_definition <- function(model_name, schedule_name, experiment_count) {
    if (identical(model_name, "one_compartment")) {
        model <- one_compartment_model()
        times <- if (identical(schedule_name, "sparse")) {
            with_units(c(1, 4, 8, 16, 24) [h])
        } else {
            with_units(seq(0.5, 24, by = 0.5) [h])
        }
        observable <- "C"
        dosing <- dosing()
        dynamic_name <- "k"
        dynamic_initial <- with_units(0.12 [1/h])
        dynamic_truth <- with_units(0.2 [1/h])
        start <- with_units(0 [h])
    } else {
        model <- pbpk_model()
        times <- if (identical(schedule_name, "sparse")) c(0.25, 1, 4, 8, 24) else seq(0.25, 24, by = 0.25)
        observable <- "Cpla"
        dosing <- dosing(time = 0, amount = 1, cmt = "ven", molec = "drug")
        dynamic_name <- "CL"
        dynamic_initial <- 3
        dynamic_truth <- 5
        start <- 0
    }

    schedule <- observation_schedule(time = times, observable = observable)
    truth_experiment <- experiment(observations = schedule, dosing = dosing, start = start)
    prediction <- simulate(model, experiment = truth_experiment)$observables$value
    scale <- max(abs(as.numeric(prediction)))
    noise_numeric <- if (scale > 0) 0.03 * scale else 0.01
    noise_scale <- if (inherits(prediction, "units")) {
        units::set_units(
            noise_numeric,
            units::deparse_unit(prediction),
            mode = "standard"
        )
    } else {
        noise_numeric
    }
    pattern <- rep(c(-1, 0.5, 1, -0.5), length.out = length(prediction))
    observed <- prediction + pattern * noise_scale
    data <- observation_data(time = schedule$time, observable = observable, value = observed)

    studies <- lapply(seq_len(experiment_count), function(i) {
        experiment(observations = data, dosing = dosing, start = start)
    })
    names(studies) <- paste0("individual_", seq_along(studies))
    studies <- do.call(experiments, studies)

    sigma_initial <- 2 * noise_scale
    sigma_truth <- noise_scale
    dynamic_spec <- parameter_spec(dynamic_initial, transform = "log")
    specs <- do.call(c, setNames(
        list(dynamic_spec, parameter_spec(sigma_initial, transform = "log")),
        c(dynamic_name, "sigma")
    ))
    statistics <- do.call(statistical_model, setNames(list(normal(sd = "sigma")), observable))
    statistics <- estimation_problem(model, studies, specs, statistics)$statistics

    list(
        model = model,
        experiments = studies,
        specs = specs,
        statistics = statistics,
        dynamic_name = dynamic_name,
        fixed_values = list(
            dynamic_truth,
            dynamic_initial,
            dynamic_truth * 1.25
        ),
        sigma_truth = sigma_truth
    )
}

parameter_values <- function(dynamic, sigma, dynamic_name) {
    values <- list(dynamic, sigma)
    names(values) <- c(dynamic_name, "sigma")
    values
}

likelihood_at <- function(model, definition, dynamic) {
    values <- parameter_values(
        dynamic = unname(dynamic),
        sigma = definition$sigma_truth,
        dynamic_name = definition$dynamic_name
    )
    runtime <- structure(values[definition$dynamic_name], class = c("Parameters", "list"))
    studies <- lapply(unclass(definition$experiments), function(study) {
        study$parameters <- runtime
        study
    })
    names(studies) <- names(definition$experiments)
    studies <- do.call(experiments, studies)
    simulated <- simulate(model, experiment = studies)
    compphysiol:::.estimation_likelihood(
        definition$experiments,
        simulated,
        definition$statistics,
        values
    )
}

compile_cached <- function(model, definition, runtime) {
    ode <- model$ode_model
    merged <- compphysiol:::.merge_ode_parameters(ode$parameters, runtime)
    time <- definition$experiments[[1]]$observations$time
    dimensions <- compphysiol:::.simulation_dimensions(
        ode,
        time = time,
        dimensions = NULL,
        parameters = merged
    )
    elapsed(compphysiol:::.compiled_ode_model_build(
        model,
        parameters = runtime,
        dimensions = dimensions
    ))
}

run_fixed_route <- function(route, model, definition, vectors) {
    timing <- elapsed({
        values <- numeric()
        for (i in seq_len(fixed_replicates)) {
            values <- c(values, vapply(
                vectors,
                function(x) likelihood_at(model, definition, x),
                numeric(1)
            ))
        }
        values
    })
    data.frame(
        route = route,
        evaluations = length(timing$value),
        elapsed = timing$seconds,
        seconds_per_evaluation = timing$seconds / length(timing$value),
        nll_checksum = sum(timing$value),
        stringsAsFactors = FALSE
    )
}

fit_route <- function(route, model, definition, engine, compilation_seconds = 0) {
    problem <- estimation_problem(
        model,
        definition$experiments,
        definition$specs,
        definition$statistics
    )
    timing <- elapsed(estimate(problem, engine = engine))
    fit <- timing$value
    estimates <- paste(
        sprintf("%s=%.8g", names(coef(fit)), vapply(coef(fit), as.numeric, numeric(1))),
        collapse = "; "
    )
    optim_evaluations <- unname(fit$engine_result$counts[["function"]])
    data.frame(
        route = route,
        wall_clock_seconds = timing$seconds,
        likelihood_evaluations = optim_evaluations + 2L,
        final_estimates = estimates,
        final_negative_log_likelihood = fit$neg_log_lik,
        convergence_code = fit$convergence$code,
        compilation_seconds = compilation_seconds,
        total_seconds_including_compilation = timing$seconds + compilation_seconds,
        warm_cache_estimation_seconds = if (identical(route, "Warm cached CompiledOdeModel")) timing$seconds else NA_real_,
        stringsAsFactors = FALSE
    )
}

run_scenario <- function(model_name, schedule_name, experiment_count) {
    label <- paste(model_name, paste0(experiment_count, "_experiment"), schedule_name, sep = " / ")
    cat("\n", strrep("=", 78), "\n", label, "\n", sep = "")
    definition <- scenario_definition(model_name, schedule_name, experiment_count)
    ode <- to_ode_model(definition$model)

    cold_fixed <- to_compiled_ode_model(ode)
    warm_fixed <- to_compiled_ode_model(ode)
    runtime <- structure(
        list(definition$fixed_values[[1]]),
        names = definition$dynamic_name,
        class = c("Parameters", "list")
    )
    compile_fixed <- compile_cached(warm_fixed, definition, runtime)

    cat("\nFixed likelihood evaluation\n")
    fixed <- rbind(
        run_fixed_route("CompartmentModel", definition$model, definition, definition$fixed_values),
        run_fixed_route("Prepared OdeModel", ode, definition, definition$fixed_values),
        run_fixed_route("Cold CompiledOdeModel", cold_fixed, definition, definition$fixed_values),
        run_fixed_route("Warm cached CompiledOdeModel", warm_fixed, definition, definition$fixed_values)
    )
    fixed$scenario <- label
    fixed$warmup_compilation_seconds <- c(NA, NA, NA, compile_fixed$seconds)
    print(fixed, row.names = FALSE, digits = 5)
    tolerance <- 1e-6 * max(1, abs(fixed$nll_checksum[[1]]))
    stopifnot(max(abs(fixed$nll_checksum - fixed$nll_checksum[[1]])) <= tolerance)

    cat("\nEnd-to-end estimation\n")
    engine <- optim_engine(method = "L-BFGS-B", control = list(maxit = if (quick) 5 else 100))
    compiled_for_build <- to_compiled_ode_model(ode)
    compilation <- compile_cached(compiled_for_build, definition, runtime)
    cold_estimation <- to_compiled_ode_model(ode)
    fits <- rbind(
        fit_route("CompartmentModel", definition$model, definition, engine),
        fit_route("Prepared OdeModel", ode, definition, engine),
        fit_route("Cold CompiledOdeModel", cold_estimation, definition, engine),
        fit_route("Warm cached CompiledOdeModel", compiled_for_build, definition, engine,
                  compilation_seconds = compilation$seconds)
    )
    # Cold estimation compiles during its measured estimation time. Its total is
    # therefore already inclusive rather than the sum of two separate timings.
    fits$total_seconds_including_compilation[fits$route == "Cold CompiledOdeModel"] <-
        fits$wall_clock_seconds[fits$route == "Cold CompiledOdeModel"]
    fits$compilation_seconds[fits$route == "Cold CompiledOdeModel"] <- compilation$seconds
    fits$scenario <- label
    print(fits, row.names = FALSE, digits = 5)

    list(fixed = fixed, estimation = fits)
}

model_names <- c("one_compartment", "pbpk_12_compartment")
schedule_names <- c("sparse", "dense")
experiment_counts <- c(1L, 4L)
if (quick) {
    model_names <- "one_compartment"
    schedule_names <- "sparse"
    experiment_counts <- 1L
}

benchmark_results <- list()
for (model_name in model_names) {
    for (experiment_count in experiment_counts) {
        for (schedule_name in schedule_names) {
            key <- paste(model_name, experiment_count, schedule_name, sep = "_")
            benchmark_results[[key]] <- run_scenario(
                model_name,
                schedule_name,
                experiment_count
            )
        }
    }
}

fixed_results <- do.call(rbind, lapply(benchmark_results, `[[`, "fixed"))
estimation_results <- do.call(rbind, lapply(benchmark_results, `[[`, "estimation"))
rownames(fixed_results) <- NULL
rownames(estimation_results) <- NULL

cat("\nNotes:\n")
cat("- Fixed routes evaluate the same parameter vectors; the checksum assertion detects numerical disagreement.\n")
cat("- Cold compiled rows include first-use compilation. Warm rows use an explicitly pre-built cache.\n")
cat("- Compilation seconds measure compiled artifact generation without an ODE solve.\n")
cat("- Likelihood evaluations equal optim function calls plus the initial validation and final diagnostic calls.\n")
cat("- benchmark_results, fixed_results, and estimation_results remain available after sourcing.\n")
