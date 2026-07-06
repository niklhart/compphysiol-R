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
