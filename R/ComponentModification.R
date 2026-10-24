#' Modify compartment-model components
#'
#' This page is a map of the component constructors and modification functions
#' available for a [compartment model][compartment_model()]. Follow the links
#' for argument details, supported input forms, and examples.
#'
#' @section Structural components:
#' - Compartments: construct with [compartments()] and add with
#'   [add_compartment()].
#' - Molecules: construct with [molecules()] and add with [add_molecule()].
#' - Transports: construct with [transports()] and add with [add_transport()].
#' - Reactions: construct with [reactions()] and add with [add_reaction()].
#' - Equations: construct with [equations()] and add with [add_equation()].
#' - Observables: construct with [observables()] and add with
#'   [add_observable()].
#'
#' @section Value and protocol components:
#' - Parameters: construct with [parameters()], then add, overlay, or replace
#'   them with [add_parameter()], [overlay_parameter()], or
#'   [replace_parameters()].
#' - Dosing: construct with [dosing()], add with [add_dosing()], or replace the
#'   complete schedule with [replace_dosing()].
#'
#' @seealso [modify_experiments] for the corresponding experiment operations.
#' @name modify_compartment_model
NULL

#' Modify experiment components
#'
#' This page is a map of the constructors and modification functions available
#' for [experiments][experiment()]. Follow the links for argument details,
#' broadcasting and alignment rules, callbacks, and examples.
#'
#' @section Parameters:
#' Construct one set with [parameters()] or a collection with
#' [parameter_sets()]. Use [add_parameter()] to require new names,
#' [overlay_parameter()] to overwrite matching names and retain the rest, or
#' [replace_parameters()] to replace the complete component.
#'
#' @section Dosing:
#' Construct events with [dosing()]. Use [add_dosing()] to append events or
#' [replace_dosing()] to replace the complete dosing schedule.
#'
#' @section Observations:
#' Construct schedules with [observation_schedule()] and data with
#' [observation_data()]. Use [replace_observations()] to replace the complete
#' observation component or [attach_observation_values()] to transform an
#' existing schedule into observation data.
#'
#' @section Start time:
#' Supply the initial start through [experiment()] and change it with
#' [replace_start()].
#'
#' @seealso [modify_compartment_model] for the corresponding model operations.
#' @name modify_experiments
NULL
