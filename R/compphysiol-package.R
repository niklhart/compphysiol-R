
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
#' - `compphysiol_model`: Encapsulates the structure of a compartmental model,
#'   including compartments, molecules, transports, reactions, equations,
#'   observables, parameters, and dosing events.
#' - `compphysiol_compartments`: Represent model spaces, tissues, organs, or abstract
#'   locations with names and volumes.
#' - `compphysiol_molecules`: Represent modelled substances whose amounts or concentrations
#'   can vary across compartments.
#' - `compphysiol_transports`: Define movement of the same molecule between compartments.
#' - `compphysiol_reactions`: Define molecular transformations or stoichiometric changes
#'   within compartments.
#' - `compphysiol_equations`: Define auxiliary expressions that can be reused in model
#'   processes.
#' - `compphysiol_observables`: Represent derived outputs calculated from states and
#'   parameters.
#' - `compphysiol_parameters`: Represent named inputs used in the definition of compartments, 
#'   transports, reactions, equations and observables.
#' - `compphysiol_dosing`: Represents dosing events, including bolus and infusion doses.
#' @keywords internal
"_PACKAGE"
