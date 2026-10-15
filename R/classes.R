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
        compphysiol_model = "CompartmentModel",
        compphysiol_model_representation = "ModelRepresentation",
        compphysiol_process_model = "ProcessModel",
        compphysiol_ode_model = "OdeModel",
        compphysiol_desolve_compiled_model = "CompiledOdeModel",
        compphysiol_executable_model = "ExecutableModel",
        compphysiol_analytical_model = "AnalyticalModel",
        compphysiol_stochastic_model = "StochasticModel",
        compphysiol_compartments = "Compartments",
        compphysiol_molecules = "Molecules",
        compphysiol_transports = "Transports",
        compphysiol_transport = "Transport",
        compphysiol_reactions = "Reactions",
        compphysiol_states = "States",
        compphysiol_equations = "Equations",
        compphysiol_observables = "Observables",
        compphysiol_parameters = "Parameters",
        compphysiol_parameter_sets = "ParameterSets",
        compphysiol_dosing = "Dosing",
        compphysiol_ode_dosing = "OdeDosing",
        compphysiol_experiment = "Experiment",
        compphysiol_experiments = "Experiments",
        compphysiol_observation_schedule = "ObservationSchedule",
        compphysiol_observation_data = "ObservationData",
        compphysiol_statistical_model = "StatisticalModel",
        compphysiol_statistical_distribution = "StatisticalDistribution",
        compphysiol_normal_distribution = "NormalDistribution",
        compphysiol_lognormal_distribution = "LognormalDistribution",
        compphysiol_proportional_sd = "ProportionalSD",
        compphysiol_combined_sd = "CombinedSD",
        compphysiol_correlations = "Correlated",
        compphysiol_prediction_location = "PredictionLocation",
        compphysiol_parameter_spec = "ParameterSpec",
        compphysiol_parameter_specs = "ParameterSpecs",
        compphysiol_simulation_engine = "SimulationEngine",
        compphysiol_desolve_engine = "DeSolveEngine",
        compphysiol_estimation_engine = "EstimationEngine",
        compphysiol_optim_engine = "OptimEngine",
        compphysiol_simulation_result = "SimulationResult",
        compphysiol_estimation_problem = "EstimationProblem",
        compphysiol_estimation_result = "EstimationResult",
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
