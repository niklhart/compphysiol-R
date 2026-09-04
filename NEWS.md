# compphysiol 0.4.0

* refactored and extended the package-internal model representations:
    * introduced a `ProcessModel` class which flattens `CompartmentModel` molecule-in-compartment pairs into states and transports/reactions into processes
    * added a `StochasticModel` representation that can be solved via SSA or a hybrid algorithm
    * the analytical simulation pathway uses an `AnalyticalModel` class now
* promoted `simulate()` to the main interface for simulation of all of the above models returning `SimulationResult` objects; older `to_ode()` and `to_analytical()` have been removed.
* better error handling for incomplete model specifications
* consolidate the function index by un-exporting package functions that are still under development

# compphysiol 0.3.7

* added recursive parameter definitions
* improved `print.OdeModel` unit handling

# compphysiol 0.3.6

* introduced an `OdeModel` class as an intermediate between `CompartmentModel` and deSolve-exported models, in particular for more efficient repeated simulations with varied parameters
* added support for custom base units, e.g. `"cell"`, via a session-persistent unit registry
* at dosing times, `SimulationResults` now contains post-dose instead of pre-dose values to better match standards in pharmacometrics

# compphysiol 0.3.5

* introduced parametrized initial conditions
* enhanced compartment volume handling (vectorized parametrized volumes)

# compphysiol 0.3.4

* introduced a high-level `simulate()` function, returning a `SimulationOutput` object
* added a vignette for a model definition-simulate-ggplot workflow with units
* added a `with_units()` helper supporting the units DSL

# compphysiol 0.3.3

* added cross-compartment reactions (e.g., receptor-ligand interaction) 
* introduced a reaction minilanguage (`formula = "A+B -> C"`), complementing the programmatic specification via `input`/`output`.

# compphysiol 0.3.2

* documentation update, including a glossary
* unexported development features relating to physiology and drug data concepts

# compphysiol 0.3.1

* Updated authoring information and documentation.

# compphysiol 0.3.0

## Major refactor

* Split model structure more explicitly into compartments, molecules, transports, reactions, equations, observables, parameters, and dosing.
* Replaced the old flow concept with transports for movement between compartments and reactions for molecular transformations.
* Added reaction support to ODE export, including stoichiometry, synthesis/degradation, equations in reaction rates, and concentration-state reaction systems.
* Clarified ODE state handling: amount states are preferred when possible, with concentration states supported for reaction-only systems without volumes.
* Implemented unit checks for reaction rates using the concentration/time convention.
* Simplified `mergeModels()` to overlay-mode merging; copy/renaming workflows are left for future helpers.
* Created a modelling glossary.

# compphysiol 0.2.1

## Enhancements

* Minilanguage for units implemented: `name=value[unit]`.
* Uniformized behavior of `add_*` wrappers and removed code duplication.
* Now possible to specify output units when generating ODEs.

# compphysiol 0.2.0

## Initial release

* First public release.
* Working S3 framework for `CompartmentModel`s and related classes.
* PBPK workflows not yet finished.
