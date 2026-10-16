# Benchmark executable ODE simulation for the 12-CMT PBPK model.
#
# This script is intentionally not part of the automated test suite. Timings are
# machine-, BLAS-, compiler-, and solver-dependent, so treat the numbers as a
# local sanity check rather than as package guarantees.
#
# Run from an installed package with:
#   source(system.file("benchmarks", "compiled-ode.R", package = "compphysiol"))
#
# Or from the source tree after devtools::load_all().

library(compphysiol)

read_replicates <- function(name, default) {
    value <- as.integer(Sys.getenv(name, as.character(default)))
    if (is.na(value) || value < 1L) {
        stop("Environment variable ", name, " must be a positive integer.")
    }
    value
}

n_compartment <- read_replicates("COMPPHYSIOL_BENCH_N_COMPARTMENT", 100)
n_ode <- read_replicates("COMPPHYSIOL_BENCH_N_ODE", 100)
n_compiled <- read_replicates("COMPPHYSIOL_BENCH_N_COMPILED", 1000)
n_rxode2 <- read_replicates("COMPPHYSIOL_BENCH_N_RXODE2", 1000)
n_max <- max(n_compartment, n_ode, n_compiled, n_rxode2)

times <- seq(0, 24, by = 0.25)

model <- sMD_PBPK_12CMT_wellstirred() |>
    add_dosing(time = 0, amount = 1, cmt = "ven")

base_parameters <- parameters(
    BP = 1,
    CL = 5,
    Kadi = 1,
    Kbon = 1,
    Kgut = 1,
    Khea = 1,
    Kkid = 1,
    Kliv = 1,
    Klun = 1,
    Kmus = 1,
    Kski = 1,
    Kspl = 1,
    Qadi = 0.5,
    Qbon = 0.5,
    Qgut = 0.5,
    Qhea = 0.5,
    Qkid = 0.5,
    Qliv = 1,
    Qmus = 0.5,
    Qski = 0.5,
    Qspl = 0.5,
    Vadi = 1,
    Vart = 1,
    Vbon = 1,
    Vgut = 1,
    Vhea = 1,
    Vkid = 1,
    Vliv = 1,
    Vlun = 1,
    Vmus = 1,
    Vski = 1,
    Vspl = 1,
    Vven = 1
)

parameter_sets <- lapply(seq_len(n_max), function(i) {
    p <- base_parameters
    p["CL"] <- parameters(CL = 5 * exp(0.2 * sin(i)))
    p["BP"] <- parameters(BP = 1 + 0.05 * cos(i))
    p
})

ode_model <- to_ode_model(model)
compiled_model <- to_compiled_ode_model(ode_model)
rxode2_model <- to_rxode2_model(ode_model)
rx_engine <- rxode2_engine(rtol = 1e-10, atol = 1e-10)
benchmark_experiments <- do.call(experiments, lapply(seq_len(max(n_compiled, n_rxode2)), function(i) {
    observable_names <- names(ode_model$observables)
    experiment(
        parameters = parameter_sets[[i]],
        dosing = dosing(time = 0, amount = 1, cmt = "ven"),
        observations = observation_schedule(
            rep(times, times = length(observable_names)),
            rep(observable_names, each = length(times))
        ),
        start = 0
    )
}))
compiled_batch_experiments <- benchmark_experiments[seq_len(n_compiled)]
rxode2_batch_experiments <- benchmark_experiments[seq_len(n_rxode2)]
rxode2_trace_warmup_experiments <- do.call(
    experiments,
    list(
        trace_warmup_1 = benchmark_experiments[[1]],
        trace_warmup_2 = benchmark_experiments[[1]]
    )
)

cat("Executable ODE benchmark: sMD_PBPK_12CMT_wellstirred\n")
cat("Repeated simulations:\n")
cat("  CompartmentModel:", n_compartment, "\n")
cat("  OdeModel:", n_ode, "\n")
cat("  CompiledOdeModel direct repeated:", n_compiled, "\n")
cat("  CompiledOdeModel experiments individually:", n_compiled, "individuals\n")
cat("  CompiledOdeModel batched:", n_compiled, "individuals\n")
cat("  rxode2 model direct repeated:", n_rxode2, "\n")
cat("  rxode2 model experiments individually:", n_rxode2, "individuals\n")
cat("  rxode2 model batched:", n_rxode2, "individuals\n")
cat("Output times:", length(times), "\n\n")

cat("Warming executable model caches...\n")
compiled_warmup_time <- system.time({
    compiled_warmup <- simulate(compiled_model, time = times, parameters = parameter_sets[[1]])
})
rxode2_warmup_time <- system.time({
    rxode2_warmup <- simulate(
        rxode2_model, time = times, parameters = parameter_sets[[1]], engine = rx_engine
    )
})

reference <- simulate(ode_model, time = times, parameters = parameter_sets[[1]])
stopifnot(isTRUE(all.equal(
    compiled_warmup$states,
    reference$states,
    tolerance = 1e-6,
    check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
    rxode2_warmup$states,
    reference$states,
    tolerance = 1e-6,
    check.attributes = FALSE
)))

run_many <- function(object, n, engine = NULL) {
    for (i in seq_len(n)) {
        if (is.null(engine)) {
            simulate(object, time = times, parameters = parameter_sets[[i]])
        } else {
            simulate(object, time = times, parameters = parameter_sets[[i]], engine = engine)
        }
    }
    invisible(NULL)
}

run_experiments_individually <- function(object, experiment_collection, engine = NULL) {
    for (i in seq_along(experiment_collection)) {
        if (is.null(engine)) {
            simulate(object, experiment = experiment_collection[[i]])
        } else {
            simulate(object, experiment = experiment_collection[[i]], engine = engine)
        }
    }
    invisible(NULL)
}

time_with_solver_trace <- function(expr, backend, warmup = NULL) {
    timer <- new.env(parent = emptyenv())
    timer$elapsed <- 0
    timer$calls <- 0L
    timer$depth <- 0L
    package <- if (identical(backend, "deSolve")) "deSolve" else "rxode2"
    fun <- if (identical(backend, "deSolve")) "ode" else "rxSolve"
    namespace <- asNamespace(package)
    old_option <- options(compphysiol.benchmark_solver_timer = timer)
    on.exit(options(old_option), add = TRUE)

    entry <- quote({
        .compphysiol_benchmark_solver_timer <- getOption("compphysiol.benchmark_solver_timer")
        if (is.environment(.compphysiol_benchmark_solver_timer)) {
            if (.compphysiol_benchmark_solver_timer$depth == 0L) {
                .compphysiol_benchmark_solver_timer$started <- proc.time()
            }
            .compphysiol_benchmark_solver_timer$depth <-
                .compphysiol_benchmark_solver_timer$depth + 1L
        }
    })
    exit <- quote({
        .compphysiol_benchmark_solver_timer <- getOption("compphysiol.benchmark_solver_timer")
        if (is.environment(.compphysiol_benchmark_solver_timer)) {
            .compphysiol_benchmark_solver_timer$depth <-
                .compphysiol_benchmark_solver_timer$depth - 1L
            if (.compphysiol_benchmark_solver_timer$depth == 0L) {
                .compphysiol_benchmark_solver_elapsed <- unname(
                    (proc.time() - .compphysiol_benchmark_solver_timer$started)[["elapsed"]]
                )
                .compphysiol_benchmark_solver_timer$elapsed <-
                    .compphysiol_benchmark_solver_timer$elapsed + .compphysiol_benchmark_solver_elapsed
                .compphysiol_benchmark_solver_timer$calls <-
                    .compphysiol_benchmark_solver_timer$calls + 1L
            }
        }
    })
    suppressMessages(trace(fun, tracer = entry, exit = exit, print = FALSE, where = namespace))
    on.exit(suppressMessages(untrace(fun, where = namespace)), add = TRUE)

    # Tracing can trigger one-time reconstruction/JIT work on its first call.
    # Prime the traced function outside the timed batch, then reset the collector.
    if (!is.null(warmup)) warmup()
    timer$elapsed <- 0
    timer$calls <- 0L
    timer$depth <- 0L
    timing <- system.time(force(expr))
    list(timing = timing, solver_elapsed = timer$elapsed, solver_calls = timer$calls)
}

time_one <- function(label, n, expr, solver_backend = NULL, solver_warmup = NULL) {
    gc()
    if (is.null(solver_backend)) {
        timing <- system.time(force(expr))
        solver_elapsed <- NA_real_
        solver_calls <- NA_integer_
    } else {
        measured <- time_with_solver_trace(expr, solver_backend, warmup = solver_warmup)
        timing <- measured$timing
        solver_elapsed <- measured$solver_elapsed
        solver_calls <- measured$solver_calls
    }
    overhead_elapsed <- if (!is.null(solver_backend)) {
        # Subtract only after accumulating all solver calls in this benchmark batch.
        max(0, unname(timing[["elapsed"]]) - solver_elapsed)
    } else {
        NA_real_
    }
    data.frame(
        route = label,
        replicates = n,
        user = unname(timing[["user.self"]]),
        system = unname(timing[["sys.self"]]),
        elapsed = unname(timing[["elapsed"]]),
        seconds_per_simulation = unname(timing[["elapsed"]]) / n,
        solver_calls = solver_calls,
        solver_elapsed = solver_elapsed,
        solver_seconds_per_simulation = solver_elapsed / n,
        overhead_elapsed = overhead_elapsed,
        overhead_seconds_per_simulation = overhead_elapsed / n,
        overhead_percent = if (!is.null(solver_backend) && timing[["elapsed"]] > 0) {
            100 * overhead_elapsed / unname(timing[["elapsed"]])
        } else {
            NA_real_
        },
        stringsAsFactors = FALSE
    )
}

warmup_row <- function(label, timing) {
    data.frame(
        route = label,
        replicates = 1L,
        user = unname(timing[["user.self"]]),
        system = unname(timing[["sys.self"]]),
        elapsed = unname(timing[["elapsed"]]),
        seconds_per_simulation = unname(timing[["elapsed"]]),
        solver_calls = NA_integer_,
        solver_elapsed = NA_real_,
        solver_seconds_per_simulation = NA_real_,
        overhead_elapsed = NA_real_,
        overhead_seconds_per_simulation = NA_real_,
        overhead_percent = NA_real_,
        stringsAsFactors = FALSE
    )
}

results <- rbind(
    warmup_row("CompiledOdeModel warm-up", compiled_warmup_time),
    warmup_row("rxode2 model warm-up", rxode2_warmup_time),
    time_one("CompartmentModel", n_compartment, run_many(model, n_compartment)),
    time_one("OdeModel", n_ode, run_many(ode_model, n_ode)),
    time_one(
        "CompiledOdeModel direct repeated", n_compiled,
        run_many(compiled_model, n_compiled), solver_backend = "deSolve",
        solver_warmup = function() simulate(
            compiled_model, time = times, parameters = parameter_sets[[1]]
        )
    ),
    time_one(
        "CompiledOdeModel experiments individually", n_compiled,
        run_experiments_individually(compiled_model, compiled_batch_experiments),
        solver_backend = "deSolve",
        solver_warmup = function() simulate(
            compiled_model, experiment = compiled_batch_experiments[[1]]
        )
    ),
    time_one(
        "CompiledOdeModel batched", n_compiled,
        simulate(compiled_model, experiment = compiled_batch_experiments),
        solver_backend = "deSolve",
        solver_warmup = function() simulate(
            compiled_model, experiment = compiled_batch_experiments[1]
        )
    ),
    time_one(
        "rxode2 model direct repeated", n_rxode2,
        run_many(rxode2_model, n_rxode2, engine = rx_engine), solver_backend = "rxode2",
        solver_warmup = function() simulate(
            rxode2_model, time = times, parameters = parameter_sets[[1]], engine = rx_engine
        )
    ),
    time_one(
        "rxode2 model experiments individually", n_rxode2,
        run_experiments_individually(rxode2_model, rxode2_batch_experiments, engine = rx_engine),
        solver_backend = "rxode2",
        solver_warmup = function() simulate(
            rxode2_model, experiment = rxode2_batch_experiments[[1]], engine = rx_engine
        )
    ),
    time_one(
        "rxode2 model batched", n_rxode2,
        simulate(rxode2_model, experiment = rxode2_batch_experiments, engine = rx_engine),
        solver_backend = "rxode2",
        solver_warmup = function() simulate(
            rxode2_model, experiment = rxode2_trace_warmup_experiments, engine = rx_engine
        )
    )
)

results$speedup_vs_ode_model <- NA_real_
ode_seconds <- results$seconds_per_simulation[results$route == "OdeModel"]
cached_routes <- c(
    "CompiledOdeModel direct repeated", "CompiledOdeModel experiments individually",
    "CompiledOdeModel batched", "rxode2 model direct repeated",
    "rxode2 model experiments individually", "rxode2 model batched"
)
if (length(ode_seconds) == 1L) {
    for (route in cached_routes) {
        route_seconds <- results$seconds_per_simulation[results$route == route]
        if (length(route_seconds) == 1L && route_seconds > 0) {
            results$speedup_vs_ode_model[results$route == route] <- ode_seconds / route_seconds
        }
    }
}

print(results, row.names = FALSE, digits = 4)

cat("\nNotes:\n")
cat("- The CompiledOdeModel warm-up includes C source generation, R CMD SHLIB, dyn.load(), and one simulation.\n")
cat("- The rxode2 warm-up includes source rendering, rxode2 compilation, and one simulation.\n")
cat("- All executable-model rows run after backend compilation and cache warm-up.\n")
cat("- Direct repeated rows use time and parameters without constructing experiments.\n")
cat("- Experiments individually and batched use identical experiment inputs; compare them to isolate batching.\n")
cat("- The benchmark temporarily traces the backend functions; no package-level timing code is used.\n")
cat("- Each traced backend is primed once outside the timed batch to exclude tracing/JIT setup.\n")
cat("- Compiled-route solver time is accumulated inside the actual backend calls made by each timed batch.\n")
cat("- Overhead is total batch time minus accumulated solver time; it is not derived from separate microbenchmarks.\n")
cat("- deSolve and rxode2 use matching rtol = 1e-10 and atol = 1e-10.\n")
cat("- The batched CompiledOdeModel row uses one experiment-collection call but still performs one deSolve call per individual.\n")
cat("- The batched rxode2 row solves all experiment individuals in one rxSolve() call and reports time per individual.\n")
cat("- Set COMPPHYSIOL_BENCH_N_COMPARTMENT, COMPPHYSIOL_BENCH_N_ODE, COMPPHYSIOL_BENCH_N_COMPILED, or COMPPHYSIOL_BENCH_N_RXODE2 to change route-specific replicate counts.\n")
