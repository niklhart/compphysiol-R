# nlmixr2 estimation API handoff

Date: 2026-11-23
Updated: 2026-11-25
Status: Initial exploration, likelihood-selection investigation, and a follow-up
on normalization and SAEM representation issues completed; implementation pending.
See [findings.md](findings.md) for evidence, proposed checks, and unresolved
SAEM translation and likelihood-normalization issues.

## Objective

Generalize the nlmixr2 estimation integration by exposing export/import and
adding SAEM to `estimate()`. Export/import is the shared foundation for the
wrapper and for native nlmixr2 workflows.

The concrete advanced use case is SAEM estimation followed by IMPMAP likelihood
evaluation at the fitted population parameters. This belongs in the native
export/import workflow; a wrapped multi-step estimation API is outside the
initial scope.

## Agreed scope

- Export a simple S3 object containing the native model and numeric data plus
  the original estimation specification and metadata required for import.
- Preserve unit conversions, parameter transformations and bounds, SD/variance
  mappings, identifiers, subject/endpoint/observation-row mappings, and the
  observation-density Jacobian needed for the public likelihood convention.
- Import compatible native results into the existing compphysiol estimation
  result representation, restoring units and public parameter coordinates.
- Reject evidence of changes that break the export/import contract. Arbitrarily
  reworked native models are not an import requirement. Missing evidence needed
  to establish compatibility must not silently count as compatibility.
- Ship SAEM in `estimate()` alongside export/import, using method-specific
  controls and compatibility checks. Preserve existing FOCEi behavior.
- Initially preserve the outputs and handling of missing outputs supported by
  the existing estimation path; adjust the contract later if needed.
- Use ordinary base R serialization if users want to save an export object.
  No additional persistent storage or cross-version compatibility guarantee is
  required initially.

## First step: investigate native results

Run small reproducible examples with FOCEi, SAEM, and SAEM followed by IMPMAP
likelihood evaluation. Record package versions, controls, seeds, scripts, and
observed result structures in [findings.md](findings.md). Verify the installed
version's actual likelihood-evaluation mechanism rather than assuming an
IMPMAP fit is evaluation-only.

Inspect the available evidence for:

1. Original and final model structure, parameterization, parameter identities,
   fixed/estimated status, bounds, and random-effect covariance structure.
2. Original data, subject IDs, endpoints, observation rows, and event records.
3. Population estimates before and after likelihood evaluation, distinguishing
   them from individual modes or sampling-proposal updates.
4. Available likelihoods, their approximation/integration methods, numerical
   conventions, and reliable extraction paths. Do not assume the default native
   `logLik()` selects the intended importance-sampling likelihood.
5. Coefficients, covariance, predictions, and behavior when optional outputs are
   absent. Establish what transfers reliably between algorithms.

Use these observations to propose the compatibility contract and likelihood
selection API. These are research outcomes, not questions awaiting the user.
Separate verified observations from proposed rules and unresolved limitations.

## Completed investigation: identify the selected likelihood

Agreed direction: `likelihood = "auto"` follows the likelihood currently
selected by native `logLik()`, provided its calculation method and value can be
identified consistently. Multiple available likelihood evaluations alone do
not make selection ambiguous. Explicit selection should allow importing another
supported evaluation. Always annotate the compphysiol result with the likelihood
calculation method, separately from the parameter estimation algorithm.

Completed on 2026-11-25 using one fixed model and dataset. The script is
`investigate-likelihood.R`; results and the decision table are in the follow-up
section of [findings.md](findings.md). The investigation scope was:

1. Inspect the installed implementations of `logLik()`, the selected-method
   accessor, the objective table, and `setOfv()`. Establish which fields are
   authoritative or derived, and which calls trigger calculation or mutate the
   result. Record the installed package versions.
2. Compare four native workflows: FOCEi estimation, SAEM estimation, FOCEi
   followed by IMPMAP evaluation, and SAEM followed by IMPMAP evaluation.
   Capture the relevant metadata before and after calling `logLik()`, then
   repeat the call to check stability. Use independent copies where calls
   mutate objects. For SAEM, determine what is selected initially and what is
   calculated on demand. For IMPMAP, distinguish the selected likelihood from
   the separately available sampling objective.
3. Probe explicit selection between available likelihood methods through the
   native API. Check agreement between the selected-method label, objective-table
   entry, and returned likelihood. Reproduce the previously observed partial
   selection failure separately and inspect the resulting state.
4. Derive and test an identification rule. Produce a concise decision table
   describing the evidence identifying each likelihood, consistency checks,
   and conditions under which `auto` must fail. Do not infer the likelihood
   calculation method solely from the estimation algorithm.

Outcome: read the selected method after calling `logLik()`, require a unique
matching objective-table row and matching finite likelihood, and annotate the
result with that method. All four standard workflow probes passed this
identification rule. The reproduced partial SAEM objective-selection failure
was rejected because its label and likelihood row disagreed. SAEM can lazily
calculate quadrature; inspected source falls back to FOCEi if that fails.

The numerical normalization and SAEM translation issues remain unresolved.
An additional eager SAEM likelihood probe failed during fit construction;
it produced no result to identify. See the findings for these limits.

Keep likelihood normalization as a separate unresolved question; an explicit
method argument cannot bypass an unresolved numerical convention. Broader
estimator, SAEM translation, and prediction-table experiments are outside this
focused step.

## Implementation sequence after exploration

The normalization/SAEM follow-up is recorded in [findings.md](findings.md).
Sampling normalization was verified against independent likelihoods for the
exercised additive/proportional normal and lognormal endpoints: count actual
non-missing responses, add normal constants, restore lognormal response
Jacobians, then apply exported unit corrections. Native `nobs` is unsuitable
for the constant and native sampling objective rows need convention checks.

SAEM needs direct native residual-scale coordinates and corresponding public
decoding mappings. The wrong bounded predictions reproduce an extra bound
transformation. Disabling native transformations is unsafe: a tight-bound
probe returned an out-of-bounds estimate. Zero-iteration FOCEi reevaluation
restores predictions while preserving theta/omega in the fixture, but does
not prove constrained estimation is valid. Finite-bound support and native
SAEM quadrature accuracy remain unresolved. Do not silently drop bounds or
substitute a different likelihood under `auto`.

Native quadrature can cap the node count while retaining the requested count
in its label. Annotate method identity separately from requested/effective
settings and retain relevant warnings.

1. Define permitted native changes and inspectable compatibility checks.
2. Define explicit likelihood selection/provenance and verification that an
   evaluation-only stage retained the population estimates.
3. Expose export/import using the existing preparation and result conversion
   boundaries where practical; settle public names after exploration.
4. Add SAEM to the single-step wrapper using the same export/import machinery.
5. Add focused regression coverage for compatible and incompatible imports,
   existing outputs, parameter/covariance transformations, unit-aware round
   trips, and the SAEM-to-IMPMAP workflow.
6. Document the public API and native workflow example; update the glossary's
   initial FOCEi scope and explain the export/import contract and the distinction
   between parameter estimation and likelihood evaluation.

## Existing implementation anchors

- `R/Estimation.R`: `nlmixr2_engine()`, `estimate()`, and public result contract.
- `R/Nlmixr2Estimation.R`: `.nlmixr2_prepare()`, `.estimate_nlmixr2()`,
  `.nlmixr2_result()`, parameter decoding, covariance conversion, and likelihood
  unit correction. Preparation already returns native source/model/data,
  dimensions, and several identifier mappings.
- `R/StatisticalModel.R`: statistical specifications and engine limitations.
- `tests/testthat/test-nlmixr2-estimation.R`: existing backend coverage.
- `tests/testthat/test-estimation-engine-conformance.R`: shared behavior and
  unit-aware conformance checks.
- `vignettes/glossary.qmd`: conceptual source of truth, particularly estimation
  problems, statistical models, units, representations, and engines.

Current restrictions include FOCEi-only configuration and translator limitations
such as diagonal individual variability and unsupported combined residual SD.
Distinguish translator support from algorithm-specific support when extending
the implementation.

## Working constraints

Follow repository `AGENTS.md` and the compphysiol concept-review skill. Never
implement on `main`. Do not hand-edit generated `man/` or `NAMESPACE`, change
`renv.lock` unintentionally, or regenerate package data. Run focused estimation
tests first and broaden for shared unit, DSL, or model-generation changes.

This handoff records the agreed direction. The initial experiments are now
recorded in `findings.md`; no public API implementation has been completed.
