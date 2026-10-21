#' Extend an experimental protocol across a realized population
#'
#' Combines one protocol [compphysiol_experiment][experiment()] with each individual in a
#' [compphysiol_parameter_sets][parameter_sets()] collection. The result contains one
#' `compphysiol_experiment` per individual. compphysiol_dosing, observations, and start are copied from
#' the protocol, while protocol and individual parameters are combined.
#'
#' Protocol and individual parameter names must not overlap. Names of parameter
#' sets become experiment names. Unnamed sets receive names based on their
#' one-based position, such as `"1"` and `"2"`.
#'
#' @param protocol An [compphysiol_experiment][experiment()] defining shared dosing,
#'   observations, start, and optional parameters.
#' @param population A [compphysiol_parameter_sets][parameter_sets()] collection containing
#'   realized individual parameters.
#' @returns A `compphysiol_experiments` collection with one experiment per parameter set.
#' @examples
#' protocol <- experiment(
#'     dosing = dosing(time = 0 [h], amount = 100 [mg]),
#'     observations = observation_schedule(c(1, 2, 4) [h], "C")
#' )
#' population <- parameter_sets(
#'     person_a = parameters(BW = 65 [kg]),
#'     person_b = parameters(BW = 82 [kg])
#' )
#' extend_protocol(protocol, population)
#' @export
extend_protocol <- function(protocol, population) {
    .check_class(protocol, "compphysiol_experiment")
    .check_class(population, "compphysiol_parameter_sets")

    if (!length(population)) return(experiments())

    labels <- .collection_identifiers(population, "Individual")

    schema_homogeneous <- .parameter_sets_schema_homogeneous(population)
    overlap_indices <- if (schema_homogeneous) 1L else seq_along(population)
    for (i in overlap_indices) {
        overlap <- intersect(names(protocol$parameters), names(population[[i]]))
        if (length(overlap)) {
            stop(
                "Protocol and individual parameters must not overlap for '", labels[[i]],
                "': ", paste(overlap, collapse = ", "), ".",
                call. = FALSE
            )
        }
    }

    individuals <- vector("list", length(population))
    for (i in seq_along(population)) {
        individuals[[i]] <- experiment(
            parameters = c(protocol$parameters, population[[i]]),
            dosing = protocol$dosing,
            observations = protocol$observations,
            start = protocol$start
        )
    }

    component_names <- .experiments_component_names()
    homogeneity <- stats::setNames(rep(TRUE, length(component_names)), component_names)
    homogeneity[["parameter_schema"]] <- schema_homogeneous
    homogeneity[["parameters"]] <- .experiments_compute_component_homogeneity(
        individuals, categories = "parameters"
    )
    .new_experiments(setNames(individuals, labels), homogeneity)
}
