
#' compphysiol: A Package for Computational Physiology
#' @name compphysiol
#' @docType package
#' @description
#' The `compphysiol` package provides tools for building and simulating compartment
#' models in R. It includes classes for defining compartments, reactions, observables,
#' dosing events, and physiological parameters.
#' @details
#' Key classes include:
#' - `Compartment`: Represents a model compartment with a name and initial amount.
#' - `Reaction`: Defines reactions between compartments with specified rates.
#' - `Observable`: Represents measurable outputs derived from compartment states.
#' - `CompartmentModel`: Encapsulates the structure of a compartmental model,
#'   including compartments, reactions, observables, and dosing events.
#' - `Dosing`: Represents dosing events, including bolus and infusion doses.
#' - `Physiology`: Manages physiological parameters for the model.
#' @keywords internal
"_PACKAGE"