#' Extend an experimental protocol across a realized population
#'
#' Combines one protocol [compphysiol_experiment][experiment()] with each individual in a
#' [compphysiol_parameter_sets][parameter_sets()] collection. The result contains one
#' `compphysiol_experiment` per individual. compphysiol_dosing, observations, and start are copied from
#' the protocol, while protocol and individual parameters are combined.
#'
#' Individual parameter values update protocol parameters by name. Names of
#' parameter sets become experiment names. Unnamed sets remain unnamed.
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

    update(protocol, parameters = population, mode = "update")
}
