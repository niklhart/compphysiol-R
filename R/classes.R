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
    class
}

.compphysiol_class_label <- function(x) {
    .compphysiol_class_name_label(class(x)[[1L]])
}
