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

has_rxode2 <- requireNamespace("rxode2", quietly = TRUE)
if (!has_rxode2) {
    warning("rxode2 is not installed; rxode2 benchmark routes will be skipped.")
}
solver_rtol <- 1e-10
solver_atol <- 1e-10

quick <- tolower(Sys.getenv("COMPPHYSIOL_BENCH_QUICK", "false")) %in%
    c("1", "true", "yes")
fixed_replicates <- as.integer(Sys.getenv(
    "COMPPHYSIOL_BENCH_ESTIMATION_REPLICATES",
    if (quick) "1" else "5"
))
if (is.na(fixed_replicates) || fixed_replicates < 1L) {
    stop("COMPPHYSIOL_BENCH_ESTIMATION_REPLICATES must be a positive integer.")
}
population_size <- as.integer(Sys.getenv(
    "COMPPHYSIOL_BENCH_ESTIMATION_N",
    if (quick) "3" else "50"
))
heterogeneous_size <- as.integer(Sys.getenv(
    "COMPPHYSIOL_BENCH_ESTIMATION_HETEROGENEOUS_N",
    if (quick) "3" else "20"
))
if (is.na(population_size) || population_size < 1L) {
    stop("COMPPHYSIOL_BENCH_ESTIMATION_N must be a positive integer.")
}
if (is.na(heterogeneous_size) || heterogeneous_size < 2L) {
    stop("COMPPHYSIOL_BENCH_ESTIMATION_HETEROGENEOUS_N must be at least 2.")
}

elapsed <- function(expr) {
    gc()
    timing <- system.time(value <- force(expr))
    list(value = value, seconds = unname(timing[["elapsed"]]))
}

elapsed_with_compilation <- function(expr, compile_fun = NULL) {
    if (is.null(compile_fun)) {
        timing <- elapsed(expr)
        timing$compile_sec <- NA_real_
        timing$compile_calls <- NA_integer_
        return(timing)
    }

    timer <- new.env(parent = emptyenv())
    timer$seconds <- 0
    timer$calls <- 0L
    old_option <- options(compphysiol.benchmark_compile_timer = timer)
    on.exit(options(old_option), add = TRUE)
    namespace <- asNamespace("compphysiol")
    entry <- quote({
        .compphysiol_compile_timer <- getOption("compphysiol.benchmark_compile_timer")
        .compphysiol_compile_timer$started <- proc.time()
    })
    exit <- quote({
        .compphysiol_compile_timer <- getOption("compphysiol.benchmark_compile_timer")
        .compphysiol_compile_timer$seconds <-
            .compphysiol_compile_timer$seconds + unname(
                (proc.time() - .compphysiol_compile_timer$started)[["elapsed"]]
            )
        .compphysiol_compile_timer$calls <- .compphysiol_compile_timer$calls + 1L
    })
    suppressMessages(trace(
        compile_fun, tracer = entry, exit = exit, print = FALSE, where = namespace
    ))
    on.exit(suppressMessages(untrace(compile_fun, where = namespace)), add = TRUE)

    timing <- elapsed(expr)
    timing$compile_sec <- timer$seconds
    timing$compile_calls <- timer$calls
    timing
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

scenario_specification <- function(name) {
    if (identical(name, "simple_single_sparse")) {
        model <- one_compartment_model()
        blueprints <- list(list(
            parameters = parameters(),
            dosing = dosing(),
            observations = observation_schedule(
                time = with_units(c(1, 4, 8, 16, 24) [h]),
                observable = "C"
            ),
            start = with_units(0 [h])
        ))
        return(list(
            label = "Simple single experiment / sparse",
            model = model,
            blueprints = blueprints,
            dynamic_name = "k",
            dynamic_initial = with_units(0.12 [1/h]),
            dynamic_truth = with_units(0.2 [1/h])
        ))
    }

    model <- pbpk_model()
    base_dosing <- dosing(time = 0, amount = 1, cmt = "ven", molec = "drug")
    if (identical(name, "solver_heavy_single_dense")) {
        blueprints <- list(list(
            parameters = parameters(),
            dosing = base_dosing,
            observations = observation_schedule(
                time = seq(0.25, 24, by = 0.25), observable = "Cpla"
            ),
            start = 0
        ))
        label <- "Solver-heavy single experiment / dense"
    } else if (identical(name, "homogeneous_population")) {
        bp <- seq(0.8, 1.2, length.out = population_size)
        schedule <- observation_schedule(
            time = c(0.25, 1, 4, 8, 24), observable = "Cpla"
        )
        blueprints <- lapply(bp, function(value) list(
            parameters = parameters(BP = value),
            dosing = base_dosing,
            observations = schedule,
            start = 0
        ))
        label <- paste0("Homogeneous-design population / n=", population_size)
    } else if (identical(name, "heterogeneous_design")) {
        blueprints <- lapply(seq_len(heterogeneous_size), function(i) {
            individual_parameters <- if (i %% 2L) {
                parameters(BP = 0.8 + 0.4 * i / heterogeneous_size)
            } else {
                parameters(
                    BP = 0.8 + 0.4 * i / heterogeneous_size,
                    Kadi = 0.9 + 0.2 * i / heterogeneous_size
                )
            }
            times <- if (i %% 2L) {
                c(0.25, 1, 4, 8, 24)
            } else {
                c(0.5, 2, 6, 12, 18, 24)
            }
            list(
                parameters = individual_parameters,
                dosing = dosing(
                    time = 0,
                    amount = 0.75 + 0.5 * i / heterogeneous_size,
                    cmt = "ven",
                    molec = "drug"
                ),
                observations = observation_schedule(time = times, observable = "Cpla"),
                start = 0
            )
        })
        label <- paste0("Stable heterogeneous designs / n=", heterogeneous_size)
    } else {
        stop("Unknown estimation benchmark scenario: ", name)
    }

    list(
        label = label,
        model = model,
        blueprints = blueprints,
        dynamic_name = "CL",
        dynamic_initial = 3,
        dynamic_truth = 5
    )
}

scenario_definition <- function(name) {
    specification <- scenario_specification(name)
    predictions <- lapply(specification$blueprints, function(x) {
        truth_experiment <- experiment(
            parameters = x$parameters,
            observations = x$observations,
            dosing = x$dosing,
            start = x$start
        )
        simulate(specification$model, experiment = truth_experiment)$observables$value
    })
    prediction_scale <- max(unlist(lapply(predictions, function(x) abs(as.numeric(x)))))
    prediction <- predictions[[1]]
    scale <- prediction_scale
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
    studies <- lapply(seq_along(specification$blueprints), function(i) {
        x <- specification$blueprints[[i]]
        prediction <- predictions[[i]]
        pattern <- rep(c(-1, 0.5, 1, -0.5), length.out = length(prediction)) *
            if (i %% 2L) 1 else -1
        observed <- prediction + pattern * noise_scale
        data <- observation_data(
            time = x$observations$time,
            observable = x$observations$observable,
            value = observed
        )
        experiment(
            parameters = x$parameters,
            observations = data,
            dosing = x$dosing,
            start = x$start
        )
    })
    names(studies) <- paste0("individual_", seq_along(studies))
    studies <- do.call(experiments, studies)

    sigma_initial <- 2 * noise_scale
    sigma_truth <- noise_scale
    dynamic_spec <- parameter_spec(specification$dynamic_initial, transform = "log")
    specs <- do.call(c, setNames(
        list(dynamic_spec, parameter_spec(sigma_initial, transform = "log")),
        c(specification$dynamic_name, "sigma")
    ))
    observed_names <- unique(unlist(lapply(
        specification$blueprints,
        function(x) x$observations$observable
    )))
    statistics <- do.call(
        statistical_model,
        setNames(rep(list(normal(sd = "sigma")), length(observed_names)), observed_names)
    )
    statistics <- estimation_problem(
        specification$model, studies, specs, statistics
    )$statistics

    list(
        label = specification$label,
        model = specification$model,
        experiments = studies,
        specs = specs,
        statistics = statistics,
        dynamic_name = specification$dynamic_name,
        fixed_values = list(
            specification$dynamic_truth,
            specification$dynamic_initial,
            specification$dynamic_truth * 1.25
        ),
        sigma_truth = sigma_truth,
        component_homogeneity = compphysiol:::.experiments_component_homogeneity(studies)
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
    runtime <- compphysiol:::.new_parameters(values[definition$dynamic_name])
    studies <- lapply(unclass(definition$experiments), function(study) {
        study$parameters <- compphysiol:::.merge_ode_parameters(
            study$parameters, runtime
        )
        study
    })
    names(studies) <- names(definition$experiments)
    studies <- do.call(experiments, studies)
    simulated <- simulate(
        model,
        experiment = studies,
        rtol = solver_rtol,
        atol = solver_atol
    )
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

compile_rxode2_cached <- function(model, definition, runtime) {
    ode <- model$ode_model
    supplied <- compphysiol:::.merge_ode_parameters(
        definition$experiments[[1]]$parameters, runtime
    )
    merged <- compphysiol:::.merge_ode_parameters(ode$parameters, supplied)
    time <- definition$experiments[[1]]$observations$time
    dimensions <- compphysiol:::.simulation_dimensions(
        ode,
        time = time,
        dimensions = NULL,
        parameters = merged
    )
    elapsed(compphysiol:::.rxode2_artifact(
        model,
        dimensions = dimensions,
        parameters = merged
    ))
}

run_fixed_route <- function(route, model, definition, vectors, compile_fun = NULL) {
    timing <- elapsed_with_compilation({
        values <- numeric()
        for (i in seq_len(fixed_replicates)) {
            values <- c(values, vapply(
                vectors,
                function(x) likelihood_at(model, definition, x),
                numeric(1)
            ))
        }
        values
    }, compile_fun = compile_fun)
    data.frame(
        route = route,
        n_eval = length(timing$value),
        sec = timing$seconds,
        sec_eval = timing$seconds / length(timing$value),
        nll_sum = sum(timing$value),
        compile_sec = timing$compile_sec,
        n_compile = timing$compile_calls,
        stringsAsFactors = FALSE
    )
}

fit_route <- function(route, model, definition, engine, compile_fun = NULL) {
    problem <- estimation_problem(
        model,
        definition$experiments,
        definition$specs,
        definition$statistics
    )
    timing <- elapsed_with_compilation(
        estimate(problem, engine = engine), compile_fun = compile_fun
    )
    fit <- timing$value
    estimates <- paste(
        sprintf("%s=%.8g", names(coef(fit)), vapply(coef(fit), as.numeric, numeric(1))),
        collapse = "; "
    )
    optim_evaluations <- unname(fit$engine_result$counts[["function"]])
    data.frame(
        route = route,
        sec = timing$seconds,
        n_eval = optim_evaluations + 2L,
        estimates = estimates,
        nll = fit$neg_log_lik,
        code = fit$convergence$code,
        compile_sec = timing$compile_sec,
        n_compile = timing$compile_calls,
        total_sec = timing$seconds,
        warm_sec = if (identical(route, "deSolve warm")) timing$seconds else NA_real_,
        stringsAsFactors = FALSE
    )
}

run_scenario <- function(scenario_name) {
    definition <- scenario_definition(scenario_name)
    label <- definition$label
    compiled_only <- scenario_name %in% c(
        "homogeneous_population", "heterogeneous_design"
    )
    design <- data.frame(
        scenario = label,
        n_exp = length(definition$experiments),
        stable_eval = TRUE,
        as.list(definition$component_homogeneity),
        check.names = FALSE
    )
    cat("\n", strrep("=", 78), "\n", label, "\n", sep = "")
    cat("Within-evaluation component homogeneity:\n")
    print(definition$component_homogeneity)
    ode <- to_ode_model(definition$model)

    cold_fixed <- to_compiled_ode_model(ode)
    warm_fixed <- to_compiled_ode_model(ode)
    runtime <- compphysiol:::.new_parameters(
        setNames(list(definition$fixed_values[[1]]), definition$dynamic_name)
    )
    if (has_rxode2) {
        cold_rx_fixed <- to_rxode2_model(ode)
        warm_rx_fixed <- to_rxode2_model(ode)
    }

    cat("\nFixed likelihood evaluation\n")
    fixed_routes <- list()
    if (!compiled_only) {
        fixed_routes <- list(
            run_fixed_route(
                "Compartment", definition$model, definition,
                definition$fixed_values
            ),
            run_fixed_route(
                "ODE", ode, definition, definition$fixed_values
            )
        )
    }
    fixed_routes <- c(fixed_routes, list(run_fixed_route(
        "deSolve cold", cold_fixed, definition, definition$fixed_values,
        compile_fun = ".compiled_ode_model_compile"
    )))
    compile_cached(warm_fixed, definition, runtime)
    fixed_routes <- c(fixed_routes, list(run_fixed_route(
        "deSolve warm", warm_fixed, definition, definition$fixed_values,
        compile_fun = ".compiled_ode_model_compile"
    )))
    if (has_rxode2) {
        fixed_routes <- c(fixed_routes, list(run_fixed_route(
            "rxode2 first", cold_rx_fixed, definition, definition$fixed_values,
            compile_fun = ".rxode2_artifact"
        )))
        compile_rxode2_cached(warm_rx_fixed, definition, runtime)
        fixed_routes <- c(fixed_routes, list(run_fixed_route(
            "rxode2 warm", warm_rx_fixed, definition, definition$fixed_values,
            compile_fun = ".rxode2_artifact"
        )))
    }
    fixed <- do.call(rbind, fixed_routes)
    fixed$scenario <- label
    print(fixed[setdiff(names(fixed), "scenario")], row.names = FALSE, digits = 5)
    tolerance <- 1e-4 * max(1, abs(fixed$nll_sum[[1]]))
    stopifnot(max(abs(fixed$nll_sum - fixed$nll_sum[[1]])) <= tolerance)

    cat("\nEnd-to-end estimation\n")
    engine <- optim_engine(method = "L-BFGS-B", control = list(maxit = if (quick) 5 else 100))
    rx_engine <- optim_engine(
        method = "L-BFGS-B",
        control = list(maxit = if (quick) 5 else 100),
        simulation = rxode2_engine(rtol = solver_rtol, atol = solver_atol)
    )
    compiled_for_build <- to_compiled_ode_model(ode)
    compile_cached(compiled_for_build, definition, runtime)
    cold_estimation <- to_compiled_ode_model(ode)
    fit_routes <- list()
    if (!compiled_only) {
        fit_routes <- list(
            fit_route("Compartment", definition$model, definition, engine),
            fit_route("ODE", ode, definition, engine)
        )
    }
    fit_routes <- c(fit_routes, list(
        fit_route(
            "deSolve cold", cold_estimation, definition, engine,
            compile_fun = ".compiled_ode_model_compile"
        ),
        fit_route(
            "deSolve warm", compiled_for_build, definition, engine,
            compile_fun = ".compiled_ode_model_compile"
        )
    ))
    if (has_rxode2) {
        fit_routes <- c(fit_routes, list(
            fit_route(
                "rxode2", ode, definition, rx_engine,
                compile_fun = ".rxode2_artifact"
            )
        ))
    }
    fits <- do.call(rbind, fit_routes)
    fits$scenario <- label
    print(fits[setdiff(names(fits), "scenario")], row.names = FALSE, digits = 5)

    list(design = design, fixed = fixed, estimation = fits)
}

scenario_names <- c(
    "simple_single_sparse",
    "solver_heavy_single_dense",
    "homogeneous_population",
    "heterogeneous_design"
)

benchmark_results <- list()
for (scenario_name in scenario_names) {
    benchmark_results[[scenario_name]] <- run_scenario(scenario_name)
}

fixed_results <- do.call(rbind, lapply(benchmark_results, `[[`, "fixed"))
estimation_results <- do.call(rbind, lapply(benchmark_results, `[[`, "estimation"))
design_results <- do.call(rbind, lapply(benchmark_results, `[[`, "design"))
rownames(fixed_results) <- NULL
rownames(estimation_results) <- NULL
rownames(design_results) <- NULL

cat("\nNotes:\n")
cat("- Fixed routes evaluate the same parameter vectors; the checksum assertion detects numerical disagreement.\n")
cat("- deSolve and rxode2 both use rtol = atol = 1e-10.\n")
cat("- fixed_replicates repeats the complete three-point parameter grid, so the default gives 5 x 3 = 15 evaluations.\n")
cat("- compile_sec is measured inside the displayed route; cold/first rows compile once and warm rows compile zero times.\n")
cat("- rxode2 may satisfy artifact generation from its process-wide source cache; n_compile counts local artifact builds, including cache hits.\n")
cat("- The end-to-end rxode2 artifact is reused by later objective evaluations in the same fit.\n")
cat("- Likelihood evaluations equal optim function calls plus the initial validation and final diagnostic calls.\n")
cat("- Population scenarios run only the cold and warm compiled routes; interpreting models on every evaluation is prohibitively slow at these sizes.\n")
cat("- The heterogeneous-design collection is stable across objective evaluations even though its components are not homogeneous within an evaluation.\n")
cat("- Population sizes are controlled by COMPPHYSIOL_BENCH_ESTIMATION_N and COMPPHYSIOL_BENCH_ESTIMATION_HETEROGENEOUS_N.\n")
cat("- benchmark_results, design_results, fixed_results, and estimation_results remain available after sourcing.\n")
