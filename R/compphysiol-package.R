
#' compphysiol: A Package for Computational Physiology
#' @name compphysiol
#' @docType package
#' @description
#' The `compphysiol` package provides tools for building and simulating compartment
#' models in R. It includes classes for defining compartments, reactions, observables,
#' dosing events, and physiological parameters.
#' @details
#' Key classes include:
#' - `CompartmentModel`: Encapsulates the structure of a compartmental model,
#'   including compartments, reactions, observables, and dosing events.
#' - `Compartments`: Represents model compartments with names and initial amounts.
#' - `Flows`: Defines flows between compartments with specified rates.
#' - `Observables`: Represents measurable outputs derived from compartment states.
#' - `Equations`: Represents mathematical relationships between model parameters and/or states.
#' - `Dosing`: Represents dosing events, including bolus and infusion doses.
#' - `Physiology`: Manages physiological parameters for the model.
#' @keywords internal
"_PACKAGE"