#' Test for compphysiol classes
#'
#' These predicates provide a stable, readable alternative to spelling package
#' class names in calls to [inherits()]. Each predicate corresponds to the S3
#' class obtained by adding the `compphysiol_` prefix to the predicate suffix.
#'
#' @param x An object to test.
#' @returns A single `TRUE` or `FALSE` value.
#' @name class_predicates
NULL

.inherits_compphysiol <- function(x, suffix) {
    inherits(x, paste0("compphysiol_", suffix))
}

#' @rdname class_predicates
#' @export
is_model <- function(x) .inherits_compphysiol(x, "model")

#' @rdname class_predicates
#' @export
is_compartment_model <- function(x) .inherits_compphysiol(x, "compartment_model")

#' @rdname class_predicates
#' @export
is_process_model <- function(x) .inherits_compphysiol(x, "process_model")

#' @rdname class_predicates
#' @export
is_ode_model <- function(x) .inherits_compphysiol(x, "ode_model")

#' @rdname class_predicates
#' @export
is_desolve_compiled_model <- function(x) .inherits_compphysiol(x, "desolve_compiled_model")

#' @rdname class_predicates
#' @export
is_rxode2_model <- function(x) .inherits_compphysiol(x, "rxode2_model")

#' @rdname class_predicates
#' @export
is_executable_model <- function(x) .inherits_compphysiol(x, "executable_model")

#' @rdname class_predicates
#' @export
is_analytical_model <- function(x) .inherits_compphysiol(x, "analytical_model")

#' @rdname class_predicates
#' @export
is_stochastic_model <- function(x) .inherits_compphysiol(x, "stochastic_model")

#' @rdname class_predicates
#' @export
is_compartments <- function(x) .inherits_compphysiol(x, "compartments")

#' @rdname class_predicates
#' @export
is_molecules <- function(x) .inherits_compphysiol(x, "molecules")

#' @rdname class_predicates
#' @export
is_transport <- function(x) .inherits_compphysiol(x, "transport")

#' @rdname class_predicates
#' @export
is_transports <- function(x) .inherits_compphysiol(x, "transports")

#' @rdname class_predicates
#' @export
is_states <- function(x) .inherits_compphysiol(x, "states")

#' @rdname class_predicates
#' @export
is_reactions <- function(x) .inherits_compphysiol(x, "reactions")

#' @rdname class_predicates
#' @export
is_equations <- function(x) .inherits_compphysiol(x, "equations")

#' @rdname class_predicates
#' @export
is_observables <- function(x) .inherits_compphysiol(x, "observables")

#' @rdname class_predicates
#' @export
is_parameters <- function(x) .inherits_compphysiol(x, "parameters")

#' @rdname class_predicates
#' @export
is_parameter_sets <- function(x) .inherits_compphysiol(x, "parameter_sets")

#' @rdname class_predicates
#' @export
is_dosing <- function(x) .inherits_compphysiol(x, "dosing")

#' @rdname class_predicates
#' @export
is_ode_dosing <- function(x) .inherits_compphysiol(x, "ode_dosing")

#' @rdname class_predicates
#' @export
is_experiment <- function(x) .inherits_compphysiol(x, "experiment")

#' @rdname class_predicates
#' @export
is_experiments <- function(x) .inherits_compphysiol(x, "experiments")

#' @rdname class_predicates
#' @export
is_observation_schedule <- function(x) .inherits_compphysiol(x, "observation_schedule")

#' @rdname class_predicates
#' @export
is_observation_data <- function(x) .inherits_compphysiol(x, "observation_data")

#' @rdname class_predicates
#' @export
is_statistical_model <- function(x) .inherits_compphysiol(x, "statistical_model")

#' @rdname class_predicates
#' @export
is_statistical_distribution <- function(x) .inherits_compphysiol(x, "statistical_distribution")

#' @rdname class_predicates
#' @export
is_normal_distribution <- function(x) .inherits_compphysiol(x, "normal_distribution")

#' @rdname class_predicates
#' @export
is_lognormal_distribution <- function(x) .inherits_compphysiol(x, "lognormal_distribution")

#' @rdname class_predicates
#' @export
is_proportional_sd <- function(x) .inherits_compphysiol(x, "proportional_sd")

#' @rdname class_predicates
#' @export
is_combined_sd <- function(x) .inherits_compphysiol(x, "combined_sd")

#' @rdname class_predicates
#' @export
is_correlations <- function(x) .inherits_compphysiol(x, "correlations")

#' @rdname class_predicates
#' @export
is_prediction_location <- function(x) .inherits_compphysiol(x, "prediction_location")

#' @rdname class_predicates
#' @export
is_parameter_spec <- function(x) .inherits_compphysiol(x, "parameter_spec")

#' @rdname class_predicates
#' @export
is_parameter_specs <- function(x) .inherits_compphysiol(x, "parameter_specs")

#' @rdname class_predicates
#' @export
is_engine <- function(x) .inherits_compphysiol(x, "engine")

#' @rdname class_predicates
#' @export
is_simulation_engine <- function(x) .inherits_compphysiol(x, "simulation_engine")

#' @rdname class_predicates
#' @export
is_desolve_engine <- function(x) .inherits_compphysiol(x, "desolve_engine")

#' @rdname class_predicates
#' @export
is_rxode2_engine <- function(x) .inherits_compphysiol(x, "rxode2_engine")

#' @rdname class_predicates
#' @export
is_estimation_engine <- function(x) .inherits_compphysiol(x, "estimation_engine")

#' @rdname class_predicates
#' @export
is_optim_engine <- function(x) .inherits_compphysiol(x, "optim_engine")

#' @rdname class_predicates
#' @export
is_simulation_result <- function(x) .inherits_compphysiol(x, "simulation_result")

#' @rdname class_predicates
#' @export
is_estimation_problem <- function(x) .inherits_compphysiol(x, "estimation_problem")

#' @rdname class_predicates
#' @export
is_estimation_result <- function(x) .inherits_compphysiol(x, "estimation_result")

#' @rdname class_predicates
#' @export
is_physiology <- function(x) .inherits_compphysiol(x, "physiology")

#' @rdname class_predicates
#' @export
is_drug <- function(x) .inherits_compphysiol(x, "drug")

#' @rdname class_predicates
#' @export
is_drugs <- function(x) .inherits_compphysiol(x, "drugs")
