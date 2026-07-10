
#' compphysiol: A Package for Computational Physiology
#' @name compphysiol
#' @docType package
#' @description
#' The `compphysiol` package provides tools for building and simulating compartment
#' models in R. It separates model locations, modelled substances, processes,
#' auxiliary expressions, outputs, parameter values, and external inputs into
#' explicit model components.
#' @details
#' Core modelling concepts include:
#' - `CompartmentModel`: Encapsulates the structure of a compartmental model,
#'   including compartments, molecules, transports, reactions, equations,
#'   observables, parameters, and dosing events.
#' - `Compartments`: Represent model spaces, tissues, organs, or abstract
#'   locations with names and volumes.
#' - `Molecules`: Represent modelled substances whose amounts or concentrations
#'   can vary across compartments.
#' - `Transports`: Define movement of the same molecule between compartments.
#' - `Reactions`: Define molecular transformations or stoichiometric changes
#'   within compartments.
#' - `Equations`: Define auxiliary expressions that can be reused in model
#'   processes.
#' - `Observables`: Represent derived outputs calculated from states and
#'   parameters.
#' - `Parameters`: Represent named inputs used in the definition of compartments, 
#'   transports, reactions, equations and observables.
#' - `Dosing`: Represents dosing events, including bolus and infusion doses.
#' @keywords internal
"_PACKAGE"
