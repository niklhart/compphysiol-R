# Benchmark compiled ODE simulation for the 12-CMT PBPK model.
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
n_max <- max(n_compartment, n_ode, n_compiled)

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

cat("Compiled ODE benchmark: sMD_PBPK_12CMT_wellstirred\n")
cat("Repeated simulations:\n")
cat("  CompartmentModel:", n_compartment, "\n")
cat("  OdeModel:", n_ode, "\n")
cat("  CompiledOdeModel cached:", n_compiled, "\n")
cat("Output times:", length(times), "\n\n")

cat("Warming compiled model cache...\n")
warmup_time <- system.time({
    compiled_warmup <- simulate(compiled_model, time = times, parameters = parameter_sets[[1]])
})

reference <- simulate(ode_model, time = times, parameters = parameter_sets[[1]])
stopifnot(isTRUE(all.equal(
    compiled_warmup$states,
    reference$states,
    tolerance = 1e-6,
    check.attributes = FALSE
)))

run_many <- function(object, n) {
    for (i in seq_len(n)) {
        simulate(object, time = times, parameters = parameter_sets[[i]])
    }
    invisible(NULL)
}

time_one <- function(label, n, expr) {
    gc()
    timing <- system.time(force(expr))
    data.frame(
        route = label,
        replicates = n,
        user = unname(timing[["user.self"]]),
        system = unname(timing[["sys.self"]]),
        elapsed = unname(timing[["elapsed"]]),
        seconds_per_simulation = unname(timing[["elapsed"]]) / n,
        stringsAsFactors = FALSE
    )
}

results <- rbind(
    data.frame(
        route = "CompiledOdeModel warm-up",
        replicates = 1L,
        user = unname(warmup_time[["user.self"]]),
        system = unname(warmup_time[["sys.self"]]),
        elapsed = unname(warmup_time[["elapsed"]]),
        seconds_per_simulation = unname(warmup_time[["elapsed"]]),
        stringsAsFactors = FALSE
    ),
    time_one("CompartmentModel", n_compartment, run_many(model, n_compartment)),
    time_one("OdeModel", n_ode, run_many(ode_model, n_ode)),
    time_one("CompiledOdeModel cached", n_compiled, run_many(compiled_model, n_compiled))
)

results$speedup_vs_ode_model <- NA_real_
ode_seconds <- results$seconds_per_simulation[results$route == "OdeModel"]
compiled_seconds <- results$seconds_per_simulation[results$route == "CompiledOdeModel cached"]
if (length(ode_seconds) == 1L && length(compiled_seconds) == 1L && compiled_seconds > 0) {
    results$speedup_vs_ode_model[results$route == "CompiledOdeModel cached"] <-
        ode_seconds / compiled_seconds
}

print(results, row.names = FALSE, digits = 4)

cat("\nNotes:\n")
cat("- The warm-up row includes C source generation, R CMD SHLIB, dyn.load(), and one simulation.\n")
cat("- The cached compiled row measures repeated simulations after compilation.\n")
cat("- Set COMPPHYSIOL_BENCH_N_COMPARTMENT, COMPPHYSIOL_BENCH_N_ODE, or COMPPHYSIOL_BENCH_N_COMPILED to change route-specific replicate counts.\n")
