#' Extend an experimental protocol across a realized population
#'
#' Combines one protocol [Experiment][experiment()] with each individual in a
#' [ParameterSets][parameter_sets()] collection. The result contains one
#' `Experiment` per individual. Dosing, observations, and start are copied from
#' the protocol, while protocol and individual parameters are combined.
#'
#' Protocol and individual parameter names must not overlap. Names of parameter
#' sets become experiment names. Unnamed sets receive names based on their
#' position, such as `"individual_1"` and `"individual_2"`.
#'
#' @param protocol An [Experiment][experiment()] defining shared dosing,
#'   observations, start, and optional parameters.
#' @param population A [ParameterSets][parameter_sets()] collection containing
#'   realized individual parameters.
#' @returns An `Experiments` collection with one experiment per parameter set.
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
    .check_class(protocol, "Experiment")
    .check_class(population, "ParameterSets")

    if (!length(population)) return(experiments())

    labels <- names(population)
    if (is.null(labels)) labels <- rep("", length(population))
    unnamed <- is.na(labels) | !nzchar(labels)
    labels[unnamed] <- paste0("individual_", which(unnamed))
    if (anyDuplicated(labels)) {
        stop(
            "Generated individual names conflict with named parameter sets: names must be unique.",
            call. = FALSE
        )
    }

    individuals <- vector("list", length(population))
    for (i in seq_along(population)) {
        overlap <- intersect(names(protocol$parameters), names(population[[i]]))
        if (length(overlap)) {
            stop(
                "Protocol and individual parameters must not overlap for '", labels[[i]],
                "': ", paste(overlap, collapse = ", "), ".",
                call. = FALSE
            )
        }
        individuals[[i]] <- experiment(
            parameters = c(protocol$parameters, population[[i]]),
            dosing = protocol$dosing,
            observations = protocol$observations,
            start = protocol$start
        )
    }

    .new_experiments(setNames(individuals, labels))
}
