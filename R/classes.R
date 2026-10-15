.compphysiol_class <- function(primary, parents = character(), base = character()) {
    unique(c(primary, parents, base))
}

.class_compphysiol_model <- function() {
    .compphysiol_class(
        "compphysiol_model",
        parents = "compphysiol_model_representation"
    )
}

.class_process_model <- function() {
    .compphysiol_class(
        "compphysiol_process_model",
        parents = "compphysiol_model_representation"
    )
}

.class_ode_model <- function() {
    .compphysiol_class(
        "compphysiol_ode_model",
        parents = "compphysiol_model_representation"
    )
}

.class_desolve_compiled_model <- function() {
    .compphysiol_class(
        "compphysiol_desolve_compiled_model",
        parents = c("compphysiol_executable_model", "compphysiol_model_representation")
    )
}

.class_analytical_model <- function() {
    .compphysiol_class(
        "compphysiol_analytical_model",
        parents = "compphysiol_model_representation"
    )
}

.class_stochastic_model <- function() {
    .compphysiol_class(
        "compphysiol_stochastic_model",
        parents = "compphysiol_model_representation"
    )
}

.class_parameters <- function() {
    .compphysiol_class("compphysiol_parameters", base = "list")
}

.compphysiol_class_name_label <- function(class) {
    labels <- c(
        compphysiol_model = "Compartment model",
        compphysiol_model_representation = "Model representation",
        compphysiol_process_model = "Process model",
        compphysiol_ode_model = "ODE model",
        compphysiol_desolve_compiled_model = "Compiled deSolve model",
        compphysiol_executable_model = "Executable model",
        compphysiol_analytical_model = "Analytical model",
        compphysiol_stochastic_model = "Stochastic model",
        compphysiol_compartments = "Compartments",
        compphysiol_molecules = "Molecules",
        compphysiol_transport = "Transport",
        compphysiol_transports = "Transports",
        compphysiol_states = "States",
        compphysiol_reactions = "Reactions",
        compphysiol_equations = "Equations",
        compphysiol_observables = "Observables",
        compphysiol_parameters = "Parameters",
        compphysiol_parameter_sets = "Parameter sets",
        compphysiol_dosing = "Dosing",
        compphysiol_ode_dosing = "ODE dosing",
        compphysiol_experiment = "Experiment",
        compphysiol_experiments = "Experiments",
        compphysiol_observation_schedule = "Observation schedule",
        compphysiol_observation_data = "Observation data",
        compphysiol_statistical_model = "Statistical model",
        compphysiol_statistical_distribution = "Statistical distribution",
        compphysiol_normal_distribution = "Normal distribution",
        compphysiol_lognormal_distribution = "Lognormal distribution",
        compphysiol_proportional_sd = "Proportional SD",
        compphysiol_combined_sd = "Combined SD",
        compphysiol_correlations = "Correlations",
        compphysiol_prediction_location = "Prediction location",
        compphysiol_parameter_spec = "Parameter specification",
        compphysiol_parameter_specs = "Parameter specifications",
        compphysiol_simulation_engine = "Simulation engine",
        compphysiol_desolve_engine = "deSolve engine",
        compphysiol_estimation_engine = "Estimation engine",
        compphysiol_optim_engine = "optim engine",
        compphysiol_simulation_result = "Simulation result",
        compphysiol_estimation_problem = "Estimation problem",
        compphysiol_estimation_result = "Estimation result",
        compphysiol_physiology = "Physiology",
        compphysiol_drug = "Drug",
        compphysiol_drugs = "Drugs"
    )
    label <- unname(labels[class])
    if (is.na(label)) class else label
}

.compphysiol_class_label <- function(x) {
    .compphysiol_class_name_label(class(x)[[1L]])
}
