# nlmixr2 estimation API handoff

Date: 2026-11-23
Updated: 2026-11-25
Status: Initial implementation and regression tests added. Public entry points
are `to_nlmixr2()`, `from_nlmixr2()`, and `nlmixr2_engine()` with likelihood options.
The verified capability restrictions below remain in force.
The API decisions below are authoritative. See [findings.md](findings.md)
for experimental evidence and remaining numerical/backend limitations; earlier
policy recommendations in the findings are superseded by this handoff.

## Objective

Generalize the nlmixr2 estimation integration by exposing export/import and
adding SAEM to `estimate()`. Export/import is the shared foundation for the
wrapper and for native nlmixr2 workflows.

The concrete advanced use case is SAEM estimation followed by IMPMAP likelihood
evaluation at the fitted population parameters. Support this through both
native export/import and explicit likelihood evaluation in the wrapper.
Arbitrary sequences of estimation algorithms remain outside the wrapper scope.

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

## Agreed likelihood API

Keep the estimation algorithm separate from likelihood evaluation. The public
argument is `likelihood`, defaulting to `"auto"`; public export/import function
names and argument placement remain implementation choices.

### `estimate()` wrapper

| Estimation algorithm | `likelihood = "auto"` | Explicit likelihood |
| --- | --- | --- |
| FOCEi | Report the verified FOCEi likelihood | Evaluate the requested supported method at the fitted population parameters |
| SAEM | Return `NA` likelihood and record that explicit evaluation is required | Evaluate the requested supported method at the fitted population parameters |

Support explicit `"focei"` and `"impmap"` where the problem/evaluation is
supported. In particular, SAEM with `likelihood = "impmap"` performs SAEM
estimation followed by evaluation-only IMPMAP. This is supported wrapper
behavior, not a general multi-step estimation framework. Evaluation must
preserve fitted population parameters; individual modes/proposals may change.
The explicit choice removes ambiguity, so the internal evaluation step does
not cause an `auto` ambiguity error.

### Native export/execution/import

| Native workflow | Import with `likelihood = "auto"` | Explicit likelihood |
| --- | --- | --- |
| Plain FOCEi fit | Report verified FOCEi likelihood | Select or perform the requested supported evaluation |
| Plain SAEM fit | Return `NA` likelihood and record that explicit evaluation is required | Select or perform the requested supported evaluation |
| FOCEi or SAEM followed by IMPMAP evaluation | Error requiring explicit likelihood selection | Choose `"focei"` or `"impmap"` |
| Inconsistent or insufficient required metadata | Error | Explicit selection cannot bypass compatibility, provenance, or normalization checks |

The IMPMAP workflow is ambiguous under `auto` even when the native object's
selected likelihood is identifiable and remains FOCEi. The user must choose
which evaluation to report. Do not silently follow `logLik()` in that case.
This applies equally to FOCEi- and SAEM-origin estimates. An explicitly selected
IMPMAP likelihood is not suppressed merely because SAEM supplied the estimates.

Import of an evaluation-only stage requires a reference to the preceding fit
so theta and omega preservation can be checked. Compare against a snapshot of
that reference; the evaluated object alone does not establish its history.
Detect supported workflow evidence from inspectable native metadata and reject
insufficient evidence rather than infer unsupported histories.

For plain SAEM with `auto`, do not trigger native lazy quadrature just to
produce a likelihood that the API deliberately leaves absent. Missing
likelihood does not prevent importing coefficients, available uncertainty, and
valid required predictions.

### Shared result and validation contract

- Annotate every reported likelihood with its calculation method separately
  from the parameter-estimation algorithm. Record why a likelihood is absent.
- Restore public parameter coordinates, units, uncertainty, predictions, and
  likelihood conventions using exported mappings.
- Accept missing/partial covariance as missing public uncertainty. Require
  the valid prediction outputs needed by the current result contract.
- Reject unsupported SAEM specifications and constraints clearly, including
  during SAEM-targeted export/preparation. Never silently remove bounds or
  disable their enforcement to obtain a fit. Algorithm changes after export
  still need capability checks at execution/import as appropriate.
- Reject incompatible changes to the scientific model, parameterization,
  data, or identifier/endpoint/row mappings. Controls and starting-value
  changes are allowed only when the compatibility contract remains satisfied.
- Native SAEM quadrature is initially unsupported. Document the observed
  discrepancies; do not silently replace a selected/requested quadrature
  calculation with FOCEi. Do not annotate requested quadrature node counts
  as effective counts when the backend caps them.
- Recommend the same explicitly chosen likelihood evaluation when comparing
  estimates obtained by different estimation algorithms.

## Evidence and implementation constraints

The initial exploration and focused follow-ups are complete. Reproduction
scripts and reports are indexed in [findings.md](findings.md).

- Native likelihood selection can often be identified after `logLik()` by
  matching the selected label to a unique objective-table row and finite value.
  This is a consistency check, not the agreed `auto` policy. A reproduced
  partial SAEM selection failure leaves a stale label and must be rejected.
- Sampling normalization was checked independently for additive/proportional
  normal and lognormal endpoints. Count actual contributing responses, restore
  normal constants and endpoint-specific lognormal response Jacobians, then
  apply the exported unit correction. Native `nobs` and native sampling rows
  are not sufficient on their own. Other families/conventions require validation.
- Direct native residual-SD coordinates avoid the transformed-residual-helper
  problem. Their mapping to public values and uncertainty must be explicit.
- Bounded SAEM predictions exhibit an extra transformation. Zero-iteration
  FOCEi reevaluation restores predictions while preserving theta/omega in the
  fixture, but does not prove correct constrained estimation. Disabling native
  bound transformations can return out-of-bounds estimates. Initially reject
  unverified finite constraints unless a constraint-preserving solution is
  independently validated.
- SAEM quadrature still disagrees with independent likelihood calculations;
  explicit FOCEi or corrected importance-sampling evaluation is the planned
  supported route, subject to validation of the relevant problem scope.

## Agreed implementation sequence

1. Define the S3 export object and compatibility signature. Preserve the
   scientific problem, native model/source/data, parsed model and parameter
   roles/bounds/fixed-status/covariance structure, units and coordinate mappings,
   subject/endpoint/row mappings, and likelihood-convention metadata. Use the
   existing preparation boundary where practical.
2. Establish algorithm capability checks and SAEM translation. Use direct native
   residual coordinates with explicit decoding/uncertainty mappings. Reject
   unsupported residual specifications and unverified constraints before
   execution; validate a supported route for required predictions.
3. Implement shared evaluation and likelihood normalization. Support FOCEi and
   evaluation-only IMPMAP on compatible problems, verify population-parameter
   preservation, and retain native results/provenance. Implement the exact
   wrapper/import `auto` tables above, including absent SAEM likelihood and
   ambiguous IMPMAP workflow errors. Do not let native selection override them.
4. Expose guarded import using the export and, for evaluation stages, a
   preceding-fit reference. Check model/data/parameter/mapping compatibility,
   required outputs, likelihood provenance and numerical convention; restore
   units/public coordinates and attach the agreed annotations. Operate on copies
   when native access/evaluation could mutate the caller's fit.
5. Generalize `estimate()` to FOCEi and SAEM with method-specific controls,
   explicit likelihood selection, and the same export/evaluation/import
   machinery. Preserve established FOCEi behavior by default. Keep arbitrary
   multi-step estimation workflows outside the wrapper.
6. Add focused regression tests for all API-table cases, including SAEM `auto`
   without lazy likelihood evaluation; explicit SAEM-to-IMPMAP evaluation;
   ambiguous native IMPMAP imports for both origins; population preservation;
   unsupported constraints; incompatible edits; missing likelihood/covariance;
   required predictions; coordinate/uncertainty/unit restoration; and independent
   likelihood normalization, including mixed endpoints and missing responses.
   Follow the independent-reference and runtime requirements below.
7. Document the wrapper and native workflow examples, the quadrature and SAEM
   capability limits, and fair algorithm comparison. Update the glossary to
   distinguish estimation from likelihood evaluation and describe the export/
   import contract. Generate public reference documentation through roxygen.

Public function/class/metadata field names and precise numerical comparison
tolerances remain implementation choices. Cross-version persistence and
arbitrary native model rewrites are not initial requirements. Existing research
limitations must be reflected in capability checks rather than silently
expanding the supported scope.

## Likelihood reference tests and test runtime

Compare calculated public likelihoods against independently established values,
not only native `logLik()`, another toolbox execution path, or saved native
output. Use the investigation's analytic Gaussian random-intercept marginal
likelihoods, analytic lognormal cases with response Jacobians, and independent
one-dimensional integration where no analytic reference is available. Reference
calculations must not call the production likelihood/normalization helpers.

- Evaluate references at the same population parameters as the native result;
  distinguish normalization errors from estimation/convergence differences.
- Cover additive normal, proportional normal, lognormal, and mixed endpoints,
  actual contributing-response counts with missing responses/events, and unit
  conversion. Explicitly verify normal constants, lognormal response Jacobians,
  and the final public-unit likelihood convention.
- Use exact or suitably tight checks for analytically exact cases. Use fixed
  seeds and justified numerical/Monte Carlo tolerances for approximate
  integration; do not require FOCEi to equal an exact marginal likelihood in
  cases where its approximation is not exact. Include a small case where it is
  exact so its normalization can be checked independently.

Keep the routine suite fast. Conversion/compilation to nlmixr2 is expensive:

- Use lazily initialized, test-run-scoped fixtures to reuse an already converted
  model/export across tests with the same scientific structure and units.
  Reuse fitted/evaluated results where independent assertions do not require a
  new fit. Avoid conversion or fitting inside each assertion/test by default.
- Copy mutable native fits before objective selection, lazy likelihood access,
  or deliberate corruption. Shared setup must not create test-order dependence
  or leak state between tests. Do not cache fits across package versions or
  unrelated test runs.
- Exercise validation, mapping, and ambiguity rules with lightweight fixtures
  where possible. Keep a small set of real backend integration tests so mocks
  do not replace execution and independent likelihood checks.
- Prefer small analytic models, evaluation-only steps at fixed parameters, and
  short seeded fits where fitting itself is under test. Do not repeat the full
  exploratory algorithm/grid matrix in routine unit tests.
- Measure focused test runtime and eliminate redundant conversion/compilation
  before broad verification. Any additional expensive numerical study belongs
  in a reproducible exploration script rather than every routine test run.

## Planned extensions

The extensions below are now implemented within the restrictions documented in
"Extension implementation status". The investigation notes retain the design
context; the additive-normal heuristic issue has since been resolved as noted
below.

### Broader SAEM random-effect distributions

The current lognormal-only SAEM restriction describes the initial validated
integration scope, not a fundamental limitation of SAEM. Extend support in a
follow-up after verifying native translation and numerical behavior:

- Normal individual variability: use the existing additive normal translation,
  `parameter = location + eta`, with normal latent eta. Investigate SAEM's
  handling of location parameters and validate fitted coordinates, predictions,
  uncertainty mappings, and explicit likelihood evaluation before relaxing the
  capability check.
- Logit-normal individual variability: extend the nlmixr2 translator, which
  currently rejects this family for all methods. Transform a normal latent
  variable into the declared interval, preserving the public median, sdlogit,
  bounds, units, and any estimated-parameter coordinate mappings. Validate the
  translation under FOCEi and SAEM rather than treating it as a SAEM-only change.

Bounds on individual realizations in a logit-normal distribution belong to the
statistical model. They are distinct from native optimizer bounds on estimated
parameters. The observed finite-parameter-bound problems do not establish that
logit-normal random effects are unsupported. Keep these capability checks and
their error messages separate.

For each extension, add small native execution and import/export tests plus
independent likelihood references where feasible. Reuse converted fixtures and
preserve the agreed likelihood-selection policy. Until validation passes,
retain explicit restrictions and describe lognormal variability as the initial
validated scope in documentation.

### Correlated individual random effects

Plan support for full and block random-effect covariance structures, with SAEM
as a primary use case. Reuse the statistical model's existing `correlated()`
concept rather than introduce a second correlation API. The current diagonal
restriction is an integration limitation; do not treat it as an intrinsic FOCEi
or SAEM restriction.

Begin with a focused investigation of what native nlmixr2 needs:

1. Map latent-normal marginal SDs and correlations to omega via
   `Omega[i,j] = rho[i,j] * SD[i] * SD[j]`. Respect units for normal marginals
   and dimensionless latent spreads for transformed marginals. Inspect native
   block syntax, eta ordering, initialization, and returned covariance metadata.
2. Establish support for fixed versus estimated correlations and SDs, including
   mixed fixed/free blocks. A fixed correlation with estimated SDs is not a
   fixed covariance. Verify how to preserve each declared constraint, rather
   than silently estimate every entry of a native omega block.
3. Preserve the existing meaning that unspecified pairs are uncorrelated.
   Establish whether native block estimation can enforce declared zero pairs
   within a connected block, and how block structure affects this. Investigate
   positive-definiteness requirements for estimation versus the positive
   semidefinite matrices allowed by population sampling; reject unsupported
   singular structures explicitly.
4. Extend export/import mappings beyond diagonal variances: decode correlations
   as covariances divided by marginal SDs, and map available joint uncertainty
   through the full transformation Jacobian. Keep random-effect correlations
   distinct from correlations between parameter estimates. Define behavior at
   zero marginal variance and for unavailable uncertainty.
5. Extend compatibility signatures to preserve eta identities/order, block
   structure, fixed/free constraints, and zero-pattern semantics through native
   estimation and evaluation-only stages. Validate both SAEM and FOCEi where
   supported, including explicit IMPMAP evaluation and population preservation.

Use small two-effect and structured-block native examples to establish the
contract before implementation. Add independent multivariate Gaussian marginal
likelihood references, unit-aware covariance/correlation round trips, and tests
for fixed, estimated, zero, invalid, and singular correlation structures. Reuse
converted fixtures to keep runtime bounded. Retain explicit rejection of unvalidated constraints and record unsupported
cases precisely.

This is planned follow-up work, not an expansion of the currently validated
scope or a request for a time-based automation.

### Representation experiment completed

The native probes for both planned extensions are recorded in the
"Planned-extension representation probes" section of [findings.md](findings.md),
with `investigate-extensions.R` and its output reports. No production scope was
expanded. The explicit latent assignment is promising for logit-normal export;
additive-normal SAEM fitting still needs diagnosis. Native omega supports free,
fixed, and some mixed fixed/free blocks, but fixed covariance is not fixed
correlation. Unspecified correlations require enforced zeros, not zero initial
values in a free block. Initially rejecting unverified fixed-rho/free-SD and
singular structures is recommended; no new top-level scientific API is needed.

### Extension implementation status

Implemented normal, lognormal, and logit-normal individual effects through the
nlmixr2 translation, including positive-definite omega blocks with estimated
correlations and fixed marginal spreads. Fixed nonzero correlations are rejected
for estimation, as agreed; simulation semantics are unchanged. Unspecified pairs
within connected blocks are fixed zeros. Singular initialization, shared SD/rho
parameters across entries, narrower correlation domains, symbolic logit-normal
bounds, and logit-normal observation error remain unsupported.

The additive-normal SAEM variance collapse was isolated to the native
`handleUninformativeEtas` heuristic. With it disabled, the reference fit gives
non-degenerate variance and plausible location/residual estimates. Normal-SAEM
exports now include `control = list(handleUninformativeEtas = FALSE)`, the wrapper
uses that default and rejects enabling the heuristic, and native import checks
it. Native callers should pass `do.call(nlmixr2est::saemControl, exported$control)`.
See `investigate-normal-saem-heuristic.R` and its report for the isolated probe.

Import decodes correlations from named omega entries and maps available joint
uncertainty through a covariance/variance Jacobian. Compatibility signatures now
also compare fixed covariance values with numerical tolerance. Tests include
independent multivariate Gaussian and latent logit-normal likelihoods, normal
SAEM safeguards, correlated logit-normal SAEM wrapping, fixed spread round trips,
zero patterns, positive definiteness, uncertainty gradients, and unit conversion.
The likelihood-selection policy is unchanged.

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

Current restrictions include the validated SAEM scope and translator limitations
such as diagonal individual variability and unsupported combined residual SD.
Distinguish translator support from algorithm-specific support when extending
the implementation.

## Working constraints

Follow repository `AGENTS.md` and the compphysiol concept-review skill. Never
implement on `main`. Do not hand-edit generated `man/` or `NAMESPACE`, change
`renv.lock` unintentionally, or regenerate package data. Run focused estimation
tests first and broaden for shared unit, DSL, or model-generation changes.

This handoff records the agreed direction. The initial experiments are now
recorded in `findings.md`. Implementation is in `R/Nlmixr2Interop.R` and the
existing estimation modules. Tests use lazy shared conversion/fit fixtures and
independent analytic/numerical likelihood references. The full-suite run found
a package-data timestamp failure in `test-sysdata.R`; no package data was regenerated.
