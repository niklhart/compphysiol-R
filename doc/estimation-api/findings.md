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
