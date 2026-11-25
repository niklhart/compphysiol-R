# Exploratory findings

Date: 2026-11-23. Status: Initial bounded exploration completed; public API and
implementation pending. See [HANDOFF.md](HANDOFF.md) for agreed scope.

The existing preparation code can produce inputs executable by FOCEi, SAEM,
and IMPMAP. However, adding SAEM requires more than allowing its method name:
the present translation exposes backend transformations and inconsistent SAEM
prediction tables. Import also needs explicit compatibility and likelihood
checks that the current internal result conversion does not perform.

## Environment and reproduction

Repository revision: `8f1c68fb934e82e97480d35df79055409e8b7d30` on
`development`; R 4.5.2, aarch64 macOS; compphysiol 0.5.4.9000;
nlmixr2est 7.1.0; rxode2 5.1.7.1. Full session information and dependency
versions are in [exploration-output.txt](exploration-output.txt).

Run from the repository root, in this order:

```sh
Rscript doc/estimation-api/explore.R
Rscript doc/estimation-api/probe-compatibility.R
Rscript doc/estimation-api/probe-units-likelihood.R
Rscript doc/estimation-api/inspect-native.R
```

Scripts write reports alongside themselves and use
`/tmp/estimation-api-fits.rds` as transient intermediate storage. No package
implementation, generated documentation, dependencies, or package data were
changed. The scripts access internal helpers through `devtools::load_all()`.
These are exploratory examples, not tests of estimator accuracy or convergence.

The main fixture is `nlmixr2_test_problem()` from
`tests/testthat/test-nlmixr2-estimation.R`: two individuals, three observations
each, one initial amount, one clearance with lognormal variability, and additive
normal residual error. FOCEi uses at most 30 outer iterations; SAEM uses seed 99,
100 burn-in and 200 EM iterations, with covariance disabled. IMPMAP uses seed 42,
1,000 samples and zero EM iterations. The small fixture is deliberately quick;
its fitted values are not scientific recommendations.

## FOCEi result structure

Verified in [exploration-output.txt](exploration-output.txt):

- With tables, the result inherits from `nlmixr2FitData`, `nlmixr2FitCore`,
  `nlmixr2.focei`, and data-frame classes. Metadata lives in an attached
  environment exposed through native `$` accessors.
- `fit$theta` is a named vector in exported optimizer coordinates;
  `fit$omega` is a named random-effect variance/covariance matrix.
- `fit$iniUi`, `fit$finalUi`, and `fit$ui` expose parsed model information.
  `iniDf` has parameter identities, theta/eta indices, bounds, fixed flags,
  and numerical estimates. `model` contains parsed model expressions.
- `fit$origData` and `nlme::getData(fit)` retain the input data, including
  `DVID`, `CP_ROW`, and event columns. Those extra columns are absent from the
  default prediction table, which contains `ID`, `TIME`, `DV`, `PRED`, `IPRED`,
  and diagnostics instead.
- The default selected likelihood is FOCEi. Native `objDf` contains its
  normalized `Log-likelihood` as well as `OBJF`, AIC, and BIC. Raw `OBJF` is not
  simply negative twice the normalized log likelihood in this configuration.
- `vcov()` covers the two theta parameters here, but not omega. Existing
  conversion returns standard errors for those theta parameters and missing
  uncertainty for the individual SD, consistent with the public contract.

Accessor detail: `fit[[name]]` performs table-column extraction and does not
retrieve environment-backed metadata; use native `$` accessors. This matters
for programmatic inspection and must not be mistaken for absent metadata.

## SAEM result structure and translation concerns

The unchanged exported model runs with `est = "saem"`, returning the same core
fit/table interface and `nlmixr2.saem`. Theta and omega can be decoded by the
existing converter; absent covariance becomes missing public standard errors.

However, these observations require resolution before shipping SAEM:

1. **Initial metadata is rewritten.** For the bounded fixture, `iniUi$iniDf`
   contains `rxBoundedTr.cp_fix_1`, `rxBoundedTr.cp_fix_2`, and an auxiliary
   `rx.eta.cp_fix_2`. The final UI restores the original parameter identities,
   bounds, and omega structure. Literal equality of initial metadata would
   reject this native workflow. Across FOCEi, SAEM, and IMPMAP,
   `finalUi$model` was identical and the final parameter signature matched
   after excluding numerical estimates. See
   [native-metadata-output.txt](native-metadata-output.txt).
2. **Prediction tables are not consistently on the fitted scale.** The bounded
   SAEM fit decoded to CL_pop = 0.1362041, omega SD = 0.5026821, and
   sigma = 0.0888058. But its table reported CL_pop about 0.001145916 and
   population predictions near 100. For example, the first population
   prediction should be approximately `100 * exp(-0.1362041)` = 87.27;
   the SAEM table reports 99.88547. Re-evaluating the same fitted parameters
   through IMPMAP produces the expected population prediction. Thus class,
   parameter names, and successful conversion alone do not establish a usable
   result table.
3. **Removing finite transformed bounds does not remove all concerns.** A
   second SAEM run with natural log domains restores plausible clearance
   predictions, but the residual helper varies by individual in the table:
   0.07472558 versus 0.07302298, whereas decoded sigma is 0.07386937. The
   initial metadata still has an auxiliary residual-related eta. This is an
   observation about this translation and installed backend, not a proven
   diagnosis of the underlying cause.
4. **Simple inlining is not a demonstrated fix.** Rewriting clearance to
   `exp(cp_fix_1 + cp_eta_1)` and residual error to `add(exp(cp_fix_2))`
   failed with a language-related warning and `the condition has length > 1`.
   This combined probe does not isolate which change caused the failure.

SAEM likelihood access also has side effects: before requesting `logLik()`,
`objDf` had an `lFOCEi` placeholder containing NAs. Requesting `logLik()` computed
a Gaussian-quadrature likelihood (subsequently labelled `gauss3_1.6`), reporting
NLL 53.29560 in the bounded example. The same parameters evaluated through
FOCEi at the IMPMAP stage gave NLL 5.455970. Do not infer the likelihood method
from the fitting algorithm, or trust the initial `ofvType` label alone.

The root causes of the SAEM table and quadrature discrepancies are unresolved.
No general claim about SAEM correctness follows from this two-subject fixture.

## SAEM followed by IMPMAP likelihood evaluation

Verified executable native workflow:

```r
sampled <- nlmixr2est::nlmixr2(
    saem_fit, nlme::getData(saem_fit), est = "impmap",
    control = nlmixr2est::impmapControl(
        nIter = 0L, isample = 1000L, impSeed = 42L,
        covMethod = "", print = 0L, compress = FALSE
    )
)
```

`all.equal()` returned TRUE for both theta and omega before/after this stage.
The returned `impEonly` is TRUE and `impIter` is zero. Installed
`.impmapFamilyFit()` explicitly recognizes `nIter = 0L` as fixed-parameter,
E-step-only execution. This verifies population parameter preservation for the
example; the importer should still compare parameter values rather than rely
only on the controls or markers. Individual modes can change.

Crucially, the returned object's default likelihood remains FOCEi, not the
importance-sampling likelihood:

| Quantity | Observed value |
| --- | ---: |
| Default native NLL, `-as.numeric(logLik(sampled))` | 5.455970 |
| Raw sampling objective, `sampled$impObj` | -0.1147968 |
| Sampling NLL after adding six observation constants | 5.456233 |
| Independent numerical integration of the original model | 5.460664 |

The independent integral evaluates the analytic exponential-decay solution and
integrates each individual's normal observation density against the normal eta
prior. It is a useful check on scale and constants, not an assertion that this
single Monte Carlo estimate has negligible error.

**Native normalization pitfall:** `sampled$nobs` is eight despite six actual
observation rows; using that count gives NLL 7.294110 instead. Inspection of the
installed `.setOfvImp()` shows it adds `fit$env$nobs * log(2*pi)` to the raw
sampling objective. Consequently, blindly reproducing that helper would carry
this discrepancy into compphysiol. The six-row correction agrees approximately
with the direct integral in this all-observed, normal, single-endpoint fixture.
Do not generalize this formula to censored, missing, or other endpoint families
without further checks.

A separate `setOfv(saem_fit, "impmap")` probe (on a base-R serialized copy)
raised `unknown error` after partially mutating the object: it added an IMPMAP
objective row and changed `logLik()`, while `getOfvType()` still reported
`gauss3_1.6`. Theta and omega remained unchanged. This is further evidence that
an importer should verify the selected objective against its table/source,
and should not use this convenience route uncritically in this version.

Installed source excerpts for `.impmapFamilyFit()`, `.setOfvImp()`,
`.impmapRecomputeObjf()`, and `setOfv()` are captured in
[native-metadata-output.txt](native-metadata-output.txt), distinguishing source
inspection from numerical experiments.

## Compatibility evidence

See [compatibility-output.txt](compatibility-output.txt). Each probe changes
one aspect of the exported inputs and runs native FOCEi evaluation:

| Change | Retained evidence | Existing internal conversion |
| --- | --- | --- |
| Starting theta value | Different `iniDf$est`; original model/data retained | Accepts |
| Observable multiplied by two | Different parsed model expression | Accepts |
| One DV incremented | Different retained original data | Accepts |
| One DVID changed to 2 | Different endpoint mapping in original data | Accepts |
| Input rows reordered | Different retained original data/order | Accepts |
| Tables disabled | Core result without prediction data frame | Fails during conversion |

These are successful detections by inspection, not implemented import checks.
The DVID probe only demonstrates that native execution and current conversion
do not reject that edit; it does not establish valid multi-endpoint behavior.

Proposed initial contract, supported by this evidence:

- Capture the parsed exported model and a parameter signature. Compare the
  final native model expressions and declared parameter roles, bounds, fixed
  status, and covariance structure, allowing numerical starting/final estimates
  to differ. Use an explicitly supported normalization for backend rewrites;
  do not simply drop unknown parameters from comparisons.
- Require retained original numeric data and mappings to match the export,
  including doses/events, subject IDs, time, DV, DVID, CP_ROW, and known
  covariate/profile columns. Initially reject reordered data rather than
  silently guess a remapping. Harmless storage-type normalization can be
  specified explicitly during implementation.
- Require the prediction table to be traceable to original observations.
  `fit$env$.rownum` is available in these runs, but it is internal and its
  meaning with events, missing observations, or filtering is not yet validated.
  Matching row counts alone is insufficient. The existing converter orders
  by exported rows while assuming the native rows already correspond; it is
  not an import compatibility check.
- Permit controls, algorithm selection, and starting-value changes only when
  these invariants still hold. Compare theta/omega against an explicit fitting
  reference when claiming a subsequent stage evaluated the same estimates.
- Reject absent required comparison metadata and detectable mismatches. A
  native fit is mutable and cannot prove its entire history; these checks
  establish observable compatibility, not tamper resistance.

## Units, transformations, and likelihood conventions

[probe-units-likelihood.R](probe-units-likelihood.R) also constructs a
unit-aware mixed-effects problem with dosing. Original units are h, mg, L,
L/h, and mg/L; solver dimensions are min, ug, and dm.

Verified in [units-likelihood-output.txt](units-likelihood-output.txt):

- Export converts observation times to minutes, a 100 mg dose to 100,000 ug,
  and observation concentrations to ug/L. Event records are retained separately
  from observations in the exported data.
- Existing conversion restores CL_pop and its SE to L/h, sigma and its SE to
  mg/L, prediction times to h, and prediction values to mg/L.
- Omega remains a dimensionless variance internally and a dimensionless
  individual sdlog publicly. The native covariance covers theta only here;
  the individual SD's public uncertainty remains missing.
- Native NLL is 56.0840450. Public NLL is 0.8220028. Their difference is
  -55.2620422, exactly `8 * log(0.001)`, matching the existing observation-unit
  Jacobian for the eight concentrations converted from mg/L to ug/L.

Keep observation-density normalization and observation-unit conversion as two
separate operations. First establish the intended normalized likelihood, then
apply the exported unit Jacobian. The native object alone does not encode the
original reporting units or the public parameter transformations.

This run verifies restoration and the expected conversion factor. It does not
establish cross-algorithm numerical invariance under changing solver units, nor
an end-to-end unit-aware SAEM/IMPMAP round trip; those remain implementation
validation work after resolving the SAEM translation concerns.

## Missing outputs

Existing behavior was exercised rather than broadened:

- Missing covariance is accepted: standard errors/correlations remain missing.
  Partial covariance is accepted and mapped only for covered parameters.
- Missing prediction tables are not accepted: with `calcTables = FALSE`, the
  existing converter fails because the core result cannot become a data frame.
  The initial importer should require the tables needed by its existing public
  result contract, with a clear error rather than this low-level failure.
- Missing theta/omega, missing required prediction columns, missing retained
  metadata, and filtered/missing observation cases were not separately run.
  Import checks and tests must cover these cases explicitly.

## Proposed decisions and remaining limitations

The exploration supports proceeding with the public exporter and a guarded
importer, but not merely enabling SAEM in the current wrapper.

1. Store the original scientific problem, numeric export/data, dimensions,
   transformation/unit mappings, and a parsed compatibility signature in the
   simple S3 export object. Base R serialization remains sufficient.
2. Keep a distinction between fitting method and selected likelihood method.
   Make likelihood selection explicit for native import; record its source,
   normalization, and unit correction. Do not read `impObj / 2` or the default
   `logLik()` as interchangeable importance-sampling likelihoods.
3. Resolve the SAEM translation/table behavior with a focused native reproducer
   before declaring it supported. Preserve parameter semantics and finite
   bounds; removing constraints to obtain a result is not an API solution.
4. For SAEM-to-IMPMAP, retain a snapshot of the SAEM theta/omega and compare
   against the evaluated fit. Define how the importer receives that reference;
   an IMPMAP object alone does not prove which earlier fit supplied its values.
5. Investigate the native observation-count/constant issue before implementing
   general likelihood import. The direct integral here provides a concrete
   regression fixture. Initial support can be deliberately limited to verified
   likelihood families and backend versions.
6. Implement row-identity checks before broadening prediction-table import.
   Multi-endpoint data, duplicate observation times, missing observations,
   censored data, infusions, dynamic covariates, parameter fixed-status edits,
   covariance-structure edits, and no-random-effect SAEM were not validated.
7. Add the agreed public documentation and glossary updates once those
   semantics are implemented. No glossary edits were made during exploration.

## Follow-up: identifying the selected likelihood (2026-11-25)

**Conclusion:** `auto` can follow native `logLik()` and identify its method for
all four investigated workflows. Identification must happen *after* `logLik()`
returns, and must cross-check the selected label against the matching objective
row and likelihood value. The fitting algorithm is not the likelihood method.
Multiple available objective rows are not themselves an ambiguity.

### Reproduction and scope

Run `Rscript doc/estimation-api/investigate-likelihood.R` from the repository
root. It creates fresh fits and uses independent base-R serialized copies for
selection experiments; it does not depend on the previous `/tmp` fit file.
See [likelihood-selection-output.txt](likelihood-selection-output.txt) for
before/after snapshots, repeated-call checks, explicit selection probes,
errors, and installed implementation excerpts. Revision:
`beeba95f075dea47962ae58cdc6c0495be99876b`; R 4.5.2,
nlmixr2est 7.1.0, rxode2 5.1.7.1, compphysiol 0.5.4.9000.

The fixed model, data, seeds, and fitting budgets are those of the initial
exploration. The follow-up investigates selection metadata, not estimator
accuracy, the SAEM translation concerns, or importance-sampling normalization.

### Observed workflow decision table

| Workflow | Before `logLik()` | After `logLik()` | Identification outcome |
| --- | --- | --- | --- |
| FOCEi fit | Finite cached likelihood; selected `focei`; row `FOCEi` | Unchanged | FOCEi |
| SAEM fit, default quadrature settings and `logLik = FALSE` | NA likelihood/objective; placeholder row `lFOCEi`; label `foce` | Computes and selects `gauss3_1.6`; finite matching likelihood row | Gaussian quadrature, 3 nodes, nsd 1.6 |
| FOCEi followed by IMPMAP E-only evaluation | Finite FOCEi likelihood and separate `impObj`; label `focei` | Unchanged | FOCEi, not importance sampling |
| SAEM followed by IMPMAP E-only evaluation | Finite FOCEi likelihood and separate `impObj`; label `focei` | Unchanged | FOCEi, not importance sampling |

In all four cases the second `logLik()` call returned an identical value and
attributes. Both IMPMAP stages preserved theta and omega. Case-insensitive
matching is needed for native labels such as `focei`, `FOCEi`, and `IMPMAP`.

The SAEM label *before* materialization is not usable evidence of a computed
likelihood. Reading only `getOfvType()` before `logLik()` would misidentify this
case as FOCE. A single finite objective row after evaluation is corroborating
evidence, but row order or the presence of `impObj` must not select the method.

### Why SAEM can expose different likelihood methods

Installed source establishes the control flow:

1. `logLik.nlmixr2FitCore()` returns `object$logLik`; the corresponding data-fit
   method delegates to the same accessor.
2. The native metadata accessor calls `.nmObjEnsureObjective()` for likelihood,
   objective, AIC, and BIC access. If a SAEM object's objective is NA, it requests
   quadrature using configured `nnodesGq` and `nsdGq`, with defaults 3 and 1.6.
3. One node is labelled `laplace<nsd>`; more nodes are labelled
   `gauss<nodes>_<nsd>`. If this calculation fails, source explicitly falls back
   to `setOfv(obj, "focei")`. That fallback branch was inspected, not triggered
   in these runs.
4. Explicit `setOfv()` can calculate/select another likelihood afterwards.

A separate fresh SAEM fit configured with `nnodesGq = 1`, `nsdGq = 2`, and
`logLik = FALSE` materialized `laplace2` on its first `logLik()` call; repeated
access was stable and metadata agreed. Explicit selection of `laplace1.6`,
`gauss5_2`, and back to `gauss3_1.6` also produced consistent labels and values.
Thus quadrature settings and subsequent selection explain differing methods;
there is no need to infer an undocumented choice from the word SAEM.

A further eager-calculation probe (`logLik = TRUE`, one-node settings) failed
during fit construction with an auxiliary `rx.eta.cp_fix_2` parameter not found
in the model block. It yielded no fit to classify. This is another limitation
of the current translated fixture/backend combination, not an identification
ambiguity. Eager SAEM likelihood support remains unverified.

### Explicit selection and the reproduced partial failure

- On FOCEi fits, selecting IMPMAP adds/selects an `IMPMAP` row; switching back
  to FOCEi works. Both states pass selected-label/likelihood-row agreement.
- On the returned IMPMAP evaluation objects, explicit IMPMAP selection and
  switching back to FOCEi work for both fitting origins. The selection helper
  may perform a new sampling evaluation; it should not be assumed to reuse the
  object's previously stored `impObj`.
- On the original SAEM fit, `setOfv(fit, "impmap")` reproduces `unknown error`
  and leaves a partially changed object. The selected label remains
  `gauss3_1.6`, but the cached likelihood now matches the new IMPMAP row rather
  than the quadrature row. The candidate consistency check rejects this state.
  Subsequent explicit FOCEi selection restores internally consistent metadata
  in this experiment.

The failure is explained by installed source: `setOfv()` assigns objective and
likelihood values, then calls `.setSaemExtra()` before assigning `ofvType`.
`.setSaemExtra()` recognizes FOCEi/FOCE/FO and quadrature labels, but rejects
IMPMAP with `unknown error`. The exception therefore occurs after the numerical
selection changed but before the selected-method label changed. This is an
ordering problem in the inspected native implementation, not uncertainty about
which fitting algorithm ran.

### Proposed identification rule for `auto`

Operate on an independent fit copy so lazy calculation does not unexpectedly
mutate the caller's object. These experiments used base R serialization for
that purpose; no separate storage system is needed.

1. Call native `logLik()` and require a finite scalar result. Preserve relevant
   warnings/errors; a failed evaluation must not be silently accepted.
2. Read `getOfvType()` and `objDf` *after* that call. Require a supported method
   label and exactly one matching row, ignoring label case.
3. Require a finite `Log-likelihood` entry in that row matching the returned
   numerical likelihood within an explicit numerical tolerance. The probe uses
   `1e-10`. As an additional implementation check, compare the selected cached
   objective with the row's `OBJF` on the same native convention; do not compare
   raw `OBJF` directly with `-2 * logLik()`.
4. Annotate the compphysiol likelihood with its calculation family and native
   label; retain node count/nsd when encoded in quadrature labels. Store fitting
   method separately. Explicitly record that selection was automatic if useful
   for provenance.

| State after native likelihood access | Proposed action |
| --- | --- |
| Supported selected label, unique row, matching finite likelihood | Identify and use the selected evaluation, subject to normalization/support checks |
| Several stored evaluations, but a consistent selected row | Same; no ambiguity error |
| NA placeholder before lazy calculation | Evaluate first, then check the resulting state |
| Missing/unknown label, absent or duplicate matching row, non-finite value | Fail with an identification error |
| Label and likelihood row disagree, as in the partial SAEM failure | Fail; do not guess from another numerically matching row |
| Supported label/value but unresolved likelihood normalization | Identification succeeds, but importing that likelihood remains unsupported |

Only the standard workflow states and the native partial failure above were
numerically exercised. Deliberately missing, duplicate, or unknown metadata
cases are proposed rejection rules for implementation tests, not additional
experiments claimed here.

### Remaining work

The likelihood-selection policy is now concrete enough to implement. No further
user decision about `auto` is needed on the evidence gathered here. Public
field/function names and precise metadata representation remain implementation
choices.

The initial unresolved issues still apply: importance-sampling normalization
(and the native observation count), SAEM translation/prediction consistency,
eager SAEM likelihood construction, and broader data/model coverage. A correctly
identified likelihood is not proof that its numerical approximation is accurate
or that all other parts of the fit satisfy the export/import contract. Keep
those checks separate rather than treating method identification as approval
of the whole result. Public API code and glossary remain unchanged.

## Follow-up: normalization and SAEM isolation (2026-11-25)

This follow-up separates density conventions from SAEM parameterization and
prediction errors. The sampling normalization question is resolved for the
exercised normal/additive, normal/proportional, and lognormal observation
families under the standard controls. SAEM's residual translation and bounded
prediction errors are isolated; a safe general finite-bound solution and the
SAEM quadrature discrepancy remain unresolved.

### Reproduction

Run from the repository root:

```sh
Rscript doc/estimation-api/investigate-normalization.R
Rscript doc/estimation-api/investigate-saem.R
Rscript doc/estimation-api/investigate-saem.R --reference-only
```

Both scripts create fresh fits, catch and report individual probe errors, and
write reports alongside themselves. No `/tmp` fit artifacts are required.
Revision: `2489d13bb48956cf397b6e13feb23783f56b6bca`; nlmixr2est 7.1.0,
rxode2 5.1.7.1, R 4.5.2. No package implementation was changed.

### Importance-sampling normalization

[normalization-output.txt](normalization-output.txt) uses a random-intercept
model with fixed evaluation parameters and no ODE approximation. Additive
normal and lognormal cases have independent analytic multivariate-normal
marginal likelihoods (after transforming lognormal responses). Proportional
normal cases use independent one-dimensional numerical integration.

Each family was checked with positive observation times, observations beginning
at zero, extra non-observation event records, one missing response, two same-family
endpoints, and response values scaled by 1,000. A mixed normal/lognormal
endpoint model was checked separately with an analytic joint likelihood.
IMPMAP uses zero EM iterations, 10,000 samples, and seed 42.

For these standard-control evaluations, the supported conversion is:

```text
sampling NLL in solver response units
    = impObj / 2
      + N_observed * log(2*pi) / 2
      + sum(log(DV_solver)) over observed lognormal endpoints only
```

`N_observed` counts actual non-missing responses, excluding events and native
inserted records. The endpoint-specific lognormal term restores the response
transformation Jacobian. This is separate from the exported Jacobian converting
solver response units back to the user's supplied units.

| Case | Independent NLL | Corrected sampling NLL |
| --- | ---: | ---: |
| Additive normal, 6 observations | 10.749987 | 10.749987 |
| Additive normal, 5 observed responses | 8.935082 | 8.935082 |
| Proportional normal, 6 observations | 10.670782 | 10.671126 |
| Lognormal, 6 observations | 11.381294 | 11.381294 |
| Lognormal, 5 observed responses | 9.407863 | 9.407863 |
| Mixed normal/lognormal endpoints | 8.103575 | 8.103575 |

Analytic normal/lognormal agreement is approximately 1e-10 or better for the
unscaled cases, including mixed endpoints. The proportional cases differ by
about 0.00034--0.00078, consistent with the observed Monte Carlo approximation;
no uncertainty estimate or exhaustive sampling-error study is claimed.

**Why the native count is wrong for this conversion:** moving the same six
responses from positive times to start at zero changes the native `nobs` from
eight to six, without changing their likelihood. A missing response leaves the
native count at eight although only five responses contribute. Using that
count gives, for example, 12.587864 rather than 10.749987 in the six-response
additive model. Therefore native `nobs` must not set the normalizing constant
or the toolbox observation count.

**Why counting observations alone is insufficient for lognormal data:** after
adding the six normal constants, the raw sampling objective yields NLL
-2.259144 on the log-response scale. Adding `sum(log(DV_solver)) = 13.64044`
restores NLL 11.381294 on the response scale. Multiplying all responses and
medians by 1,000 leaves that raw log-space objective unchanged; the Jacobian
correctly increases NLL by `6 * log(1000)`. The mixed-endpoint check confirms
that only the lognormal rows receive this term.

The scaled additive and proportional checks have residual errors of about
0.00067 and 0.00354 after correction. Even their native FOCEi evaluations show
larger scale-related numerical approximation errors (about 0.075 and 0.070),
so these are not exact invariance tests of the numerical backend. They support
the conversion factors without establishing perfect numerical unit invariance.

Implication: implement an explicit, tested normalization path for these verified
families, using exported response/endpoint mappings. Do not reuse the native
`setOfv()` sampling row uncritically: previous source inspection showed that
it uses native `nobs` and does not add this endpoint-specific lognormal term.
`auto` still follows a verifiable native *selection*, but native sampling values
need this convention handling before becoming a compphysiol likelihood. If a
selected native sampling row has no accessible sampling-evaluation metadata/raw
objective sufficient to establish its convention, reject it rather than guess.

These experiments do not establish normalization for arbitrary controls,
censoring, other residual families, or mixtures beyond the exercised endpoint
model. They also do not fix native AIC/BIC; those require separate treatment if
exposed publicly.

### SAEM: isolate residual coordinates and finite bounds

[saem-isolation-output.txt](saem-isolation-output.txt) uses the same exported
exponential-decay fixture as the initial exploration, SAEM seed 99, 100 burn-in
and 200 EM iterations. Analytic state solutions provide independent PRED/IPRED
checks; one-dimensional integration supplies the independent marginal NLL.
Each successful fit is also reevaluated with zero outer FOCEi iterations.

| Variant | Maximum PRED error | Residual helper consistent? | Interpretation |
| --- | ---: | --- | --- |
| Current export | 41.55 | No | Reproduces both problems |
| Inline clearance as `exp(theta + eta)` only | 41.55 | No | Inlining does not fix either issue |
| Declare residual SD directly only | 42.22 | Yes | Fixes residual representation, not bounded predictions |
| Disable native bound transform only | about 0.00010 | No | Fixes clearance predictions, not residual representation |
| Direct residual SD and unconstrained log-clearance | about 0.00010 | Yes | Consistent prediction representation in this fixture |
| Direct residual SD and native bound transform disabled | about 0.00010 | Yes | Same fitted result here, but constraints need separate checks |

**Residual coordinates:** the current export sends a transformed helper,
`cp_stat_sigma = exp(cp_fix_2)`, to `add(cp_stat_sigma)`. Initial SAEM metadata
introduces `rx.eta.cp_fix_2` and marks the theta as something other than a direct
residual parameter. Declaring the native SD directly and using `add(cp_fix_2)`
removes that auxiliary eta; the residual helper then equals the fitted SD in
every individual's table. This changes backend coordinates, not the intended
statistical model. A future exporter must retain an explicit mapping from this
native SD coordinate to public parameter values and uncertainty; it cannot
blindly apply the existing log-coordinate decoder.

**Finite-bound predictions:** with lower transformed bound `L = log(0.001)`
and reported fitted log-clearance theta, the wrong table value is exactly:

```text
exp(L + exp(theta_reported)) = 0.001145916
```

The correct public value is `exp(theta_reported) = 0.1362041`. This reproduces
the extra bound transformation numerically, isolating the prediction-stage
error. Zero-iteration FOCEi reevaluation preserves theta and omega and restores
predictions with errors around 0.0005 and residual helpers at numerical precision.
It is a verified way to rebuild predictions in this fixture, not proof that
SAEM's parameter-estimation path handles every bound correctly.

**Disabling the transform is unsafe for constraints:** imposing
`0.18 <= CL_pop <= 0.21` and setting `boundedTransform = FALSE` returns
CL_pop = 0.2234434, outside the declared interval. With the transform enabled,
this tight-bound example instead fails to parse an internally generated
`expit(...)` expression (`limits must be numeric`). Thus disabling native bound
handling cannot be adopted as a general fix. Removing the bounds also changes
the scientific estimation problem and is not an acceptable automatic repair.

### SAEM likelihood accuracy remains unresolved

Even the consistent direct-SD/unconstrained-log-clearance variant has a native
quadrature discrepancy. Its independent marginal NLL is 2.552383; reevaluated
FOCEi gives 2.548625. Requested quadrature refinements give:

| Native quadrature request | NLL |
| --- | ---: |
| `gauss15_3` | 2.041791 |
| `gauss61_5` | 2.018221 |

The latter request warns that the grid is too large and uses 25 nodes, while
still labelling the row `gauss61_5`. This refines the previous identification
findings: the label identifies the selected calculation family/request, but
cannot by itself establish the effective numerical settings. Preserve warnings
and distinguish requested settings from verified effective settings in any
annotation. Do not claim 61 nodes were actually used.

A separate native-only Gaussian random-intercept reference (12 individuals,
60 responses, direct mean and SD, no toolbox export or ODE) is recorded in
[saem-reference-output.txt](saem-reference-output.txt). At the returned parameter
values its analytic marginal NLL is 103.9289, matching FOCEi reevaluation, while
three native quadrature requests return infinite NLL. The fitted random-effect
variance collapsed near zero and the mean remained at its starting value, so
this is not a successful estimator-convergence example. It does show that
non-finite quadrature can also occur without the toolbox translation; finite
likelihood checks remain necessary even with consistent method labels.

The likelihood gap persists after removing the two identified representation
problems and is not explained by simply increasing the requested grid. This
follow-up has not established whether it is due to quadrature implementation,
its use of SAEM's internal state, or another numerical approximation. Native
quadrature should not yet be presented as independently validated for this
translated fixture. The tiny two-individual example also does not establish
estimator convergence or population-estimation accuracy.

### Implementation assessment after this follow-up

- **Ready to specify:** importance-sampling normalization for the exercised
  observation families, with explicit observed-row counts and response/unit
  Jacobians. Add independent-likelihood regression fixtures before exposing it.
- **Concrete exporter change:** give residual scales direct native coordinates
  and encode their public decoding/unit/uncertainty mapping explicitly.
- **Verified limited SAEM path:** direct residual SD with natural unconstrained
  log-clearance coordinates has consistent PRED/IPRED here. Restricting support
  to verified parameterizations would be honest; silently dropping finite
  constraints would not.
- **Still needed for general finite-bound SAEM:** a constraint-preserving native
  solution or an explicit bounded model reparameterization, with independent
  tests. Rebuilding predictions can fix tables but cannot establish correct
  constrained estimation.
- **Still needed for SAEM likelihoods:** resolve quadrature behavior, or provide
  a documented explicit FOCEi/importance-sampling evaluation path. Do not silently
  replace a selected quadrature likelihood under `auto`.

This investigation gives concrete implementation requirements and restrictions.
It does not justify declaring all planned SAEM workflows supported yet. No
additional user API decision is required to retain the agreed `auto` policy;
the remaining questions are numerical/backend compatibility work.

## Planned-extension representation probes (2026-11-25)

This is a small native representation experiment, not an implementation or
validation of the extensions. Run from the repository root:

```sh
Rscript doc/estimation-api/investigate-extensions.R
Rscript doc/estimation-api/investigate-extensions.R --fixed-zero-only
```

Reports: [extensions-output.txt](extensions-output.txt) and the targeted
[extensions-fixed-zero-output.txt](extensions-fixed-zero-output.txt). The script
uses eight subjects with four observations each, simulated under seed 251125;
SAEM uses seed 99, 40 burn-in and 60 EM iterations. FOCEi marginal probes use
zero outer iterations; IMPMAP uses zero EM iterations and 500 samples. The
fixed-zero supplement reproduces the full run's block-data RNG stream.
Environment: nlmixr2est 7.1.0, rxode2 5.1.7.1; revision
`f38c9e44bb21c077a07d027bcc1e5023bd74566a`. All inputs are fresh native models,
not exports accepted by the current production scope checks. No package API
or glossary was changed in this follow-up.

### Marginal distributions

The three families share the same native shape: population location and residual
SD in `theta`, normal latent variability in `omega`, and the marginal transform
in model expressions. Direct residual SD coordinates were used throughout.

| Marginal | Native expression | Public location decoding |
| --- | --- | --- |
| Normal | `individual = location + eta` | Mean = location |
| Lognormal | `individual = exp(location + eta)` | Median = exp(location) |
| Logit-normal on (2, 5) | `latent = location + eta; individual = 2 + 3 * expit(latent)` | Median = 2 + 3 * plogis(location) |

Normal SD, sdlog, and sdlogit are all represented by the square root of the
appropriate latent omega diagonal. Normal latent coordinates carry the target
unit; transformed latent coordinates are dimensionless. Unit conversions were
not numerically exercised in these native-only probes.

FOCEi evaluation and short SAEM runs produced fits for each family. Their
`finalUi$model` matched the parsed input, and PRED/IPRED agreed with the above
expressions at returned theta/eta values to floating-point precision. E-only
IMPMAP preserved theta and omega for all successful probes.

Important limitations and observations:

- The inline logit-normal expression parsed and ran but emitted a non-mu-reference
  warning and had an empty `muRefDataFrame`. Splitting out `latent = location +
  eta` produced the expected location/eta mapping and removed that warning.
  Prefer that explicit latent assignment when implementing export.
- The additive-normal SAEM run kept its location at the initial value and
  collapsed omega from 0.09 to 1e-6 while increasing residual SD. This reproduces
  the concern seen in the previous native Gaussian example. Its representation
  and predictions are consistent, but these probes do not establish a reliable
  fitting path. Diagnose this before removing the normal-SAEM guard.
- Short stochastic runs and 500-sample evaluations do not establish convergence,
  estimation accuracy, likelihood normalization, or uncertainty coverage.
- Logit-normal individual variability is not logit-normal observation error.
  Supporting the former does not authorize extending residual likelihood
  families or adding observation-density Jacobians for individual transforms.
  Integrate over the latent normal variables; retain the actual observation
  density's convention.

### Correlated omega representation

The block models use lognormal random intercept/slope parameters to avoid
confounding the covariance probes with the additive-normal fitting issue.
A third positive random coefficient is included for structured-zero probes.

The native lower-triangular initialization

```r
eta1 + eta2 ~ c(.25, .05, .04)
```

represents variances .25 and .04 and covariance .05, hence initial correlation
.5. `iniDf` includes diagonal rows and a pair row `(eta1,eta2)`, with `neta1`,
`neta2`, and per-entry `fix` flags. Returned `omega` is a named symmetric matrix.

| Native specification | Observed behavior |
| --- | --- |
| All entries free | SAEM estimated a full block; fitted correlation about .722 |
| `~ fix(.25, .05, .04)` | All covariance entries retained; correlation remained .5 |
| `~ c(fix(.25), .05, fix(.04))` | Variances retained; covariance estimated at .0136575, correlation about .137 |
| `~ c(.25, fix(.05), .04)` | Per-entry fixed flag parsed, but SAEM failed with `chol(): decomposition failed` |
| Three-effect block with initial (1,3) covariance zero and free | Fitted (1,3) covariance .0431771; zero initialization did not enforce independence |
| Three-effect block with `fix(0)` at (1,3) | SAEM and E-only IMPMAP ran; zero covariance retained to numerical tolerance |

Successful block fits preserved parsed model expressions and exposed the final
entry-wise constraints in `finalUi$iniDf`. IMPMAP preserved theta/omega.
A singular two-effect matrix was accepted by the parser only; it was not fitted.
Parser acceptance does not establish support for singular estimation problems.

A fixed off-diagonal entry is a fixed covariance, not a fixed correlation when
its marginal SDs are estimated. The failed fixed-covariance fit does not prove
that every mixed fixed/free block is unsupported; the successful fixed-diagonal
probe proves some mixed blocks work. It does require additional numerical
validation before claiming general support.

### API implications and recommended decisions

**No new top-level export/import/wrapper option is needed for these families or
correlations.** The existing statistical model already expresses their intended
scientific semantics. Extend translator mappings and capability checks, keeping
likelihood-selection policies unchanged.

1. **Location coordinates are role-specific.** Normal population locations
   need identity-capable native coordinates. Lognormal and logit-normal public
   medians need their own latent-location decoding and uncertainty Jacobians,
   distinct from an estimated parameter's public optimization transformation.
   Preserve explicit bounds/units in the export; do not conflate distribution
   bounds with native optimizer constraints. Normal-SAEM fitting remains a
   technical prerequisite, not a new API choice.
2. **Choose a conservative first covariance scope.** Recommend free positive-
   definite blocks, fully fixed blocks, and fixed marginal SDs with estimated
   correlations once their round trips are tested. Native covariance constraints
   implement these cases directly. Fixed nonzero correlations with estimated
   SDs require a verified constrained reparameterization (for example a latent
   factor construction) or explicit rejection initially. Never substitute a
   fixed covariance for the user's fixed correlation.
3. **Unspecified zero correlations must remain fixed zeros.** No user choice is
   needed: this is already part of `correlated()` semantics. Use appropriate
   fixed entries or separate independent blocks, validate numerical zero
   tolerances, and reject structures that cannot be preserved. A dense free
   block initialized with zeros is not an equivalent export.
4. **Estimation requires a defined covariance domain.** Recommend initially
   requiring positive-definite blocks with nonzero marginal spreads. Population
   sampling may continue accepting positive-semidefinite/singular structures.
   Whether to add a rank-reduced estimation representation later is a scope
   decision; singular parsing alone is insufficient. Honor declared correlation
   bounds and shared correlation parameters, or reject those constraints until
   their native representation is verified.
5. **Import checks need semantic covariance mappings.** Map by eta identities,
   not only numerical matrix positions. Preserve block structure and fixed/free
   constraints. Decode rho as Omega_ij / sqrt(Omega_ii * Omega_jj); restore normal
   SD units and latent transformed spreads correctly. Map available joint
   uncertainty through the full Jacobian, retaining the distinction between IIV
   correlations and correlations among estimates. Define missing uncertainty
   and zero-variance behavior explicitly; rejecting undefined correlations is
   safer than inventing zero.

The main remaining user-facing scope choice is whether fixed-nonzero-rho/free-SD
and singular structures must ship in the first extension or may initially be
rejected. Recommendation: retain their existing statistical-model meaning and
stage support, with explicit capability errors. All other points above refine
implementation/validation rather than require a new public scientific API.
Independent multivariate likelihood checks, unit-aware round trips, and joint
uncertainty tests are still required before production support is widened.


## Additive-normal SAEM safeguard resolved during implementation

The normal-variance-collapse probe was repeated with only
`handleUninformativeEtas` changed. Under nlmixr2est 7.1.0, seed-7 data with eight
subjects and four observations per subject, and SAEM seed 99 / 40 burn-in / 60
EM iterations, the default heuristic returned location 3, sigma .464233, and
variance 1e-6. Disabling it returned location 2.971967, sigma .0903116, and
variance .2078103. This isolates the heuristic's effect in the native reference;
it is not a general estimator-convergence guarantee.

Reproduce using `Rscript doc/estimation-api/investigate-normal-saem-heuristic.R`;
see [normal-saem-heuristic-output.txt](normal-saem-heuristic-output.txt).
The implementation now requires this safeguard for additive-normal SAEM, exposes
it in exported control defaults, and tests the wrapper/import requirement.
Earlier recommendations to keep normal-SAEM entirely disabled are superseded by
this verified control restriction.
