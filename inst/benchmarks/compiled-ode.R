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

n <- as.integer(Sys.getenv("COMPPHYSIOL_BENCH_N", "100"))
if (is.na(n) || n < 1L) {
    stop("Environment variable COMPPHYSIOL_BENCH_N must be a positive integer.")
}

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

parameter_sets <- lapply(seq_len(n), function(i) {
    p <- base_parameters
    p["CL"] <- parameters(CL = 5 * exp(0.2 * sin(i)))
    p["BP"] <- parameters(BP = 1 + 0.05 * cos(i))
    p
})

ode_model <- to_ode_model(model)
compiled_model <- to_compiled_ode_model(ode_model)

cat("Compiled ODE benchmark: sMD_PBPK_12CMT_wellstirred\n")
cat("Repeated simulations:", n, "\n")
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

run_many <- function(object) {
    for (i in seq_along(parameter_sets)) {
        simulate(object, time = times, parameters = parameter_sets[[i]])
    }
    invisible(NULL)
}

time_one <- function(label, expr) {
    gc()
    timing <- system.time(force(expr))
    data.frame(
        route = label,
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
        user = unname(warmup_time[["user.self"]]),
        system = unname(warmup_time[["sys.self"]]),
        elapsed = unname(warmup_time[["elapsed"]]),
        seconds_per_simulation = unname(warmup_time[["elapsed"]]),
        stringsAsFactors = FALSE
    ),
    time_one("CompartmentModel", run_many(model)),
    time_one("OdeModel", run_many(ode_model)),
    time_one("CompiledOdeModel cached", run_many(compiled_model))
)

results$speedup_vs_ode_model <- NA_real_
ode_elapsed <- results$elapsed[results$route == "OdeModel"]
compiled_elapsed <- results$elapsed[results$route == "CompiledOdeModel cached"]
if (length(ode_elapsed) == 1L && length(compiled_elapsed) == 1L && compiled_elapsed > 0) {
    results$speedup_vs_ode_model[results$route == "CompiledOdeModel cached"] <-
        ode_elapsed / compiled_elapsed
}

print(results, row.names = FALSE, digits = 4)

cat("\nNotes:\n")
cat("- The warm-up row includes C source generation, R CMD SHLIB, dyn.load(), and one simulation.\n")
cat("- The cached compiled row measures repeated simulations after compilation.\n")
cat("- Set COMPPHYSIOL_BENCH_N to change the number of repeated simulations.\n")
