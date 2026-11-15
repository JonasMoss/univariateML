# Core5b-b: core-owned outcomes and strict-layer repairs

Completed on 2026-11-15, retaining the job's requested 2026-11-14 filenames.
Baseline: `da2174a11af7d31c47434a54a65d4108c6b00388`; worktree branch:
`codex/core5b-b`. No commits, board edits, installs, publishing, or changes to
another worktree were made. All computation and final checks ran on the
workstation through the prescribed slot locks; simbox was not attempted.

All 25 assigned families were audited. The core now carries the statistical,
input, transformation, hold-dependent and identification decisions removed
from their adapters. The final strict run passes with no failure outside the
existing known list. Forty-two targeted known entries were removed, none
added; the eight TASK-214.5/214.13 entries were preserved exactly.

Both final snapshots contain 3,368 cases. No previously successful fit became
a stop, and every previously successful coefficient is bit-for-bit unchanged.
Likelihood payloads now describe the returned binary64 point where repaired;
restored limit endpoints and newly exposed non-unique representatives are
separate changes, listed below. Frozen goldens and d/p/q/r sources were not
changed.

Evidence is in [the data directory](2026-11-14-core5b-b-data/).
[manifest.json](2026-11-14-core5b-b-data/manifest.json) records hashes, compiler
and baseline. The final fit-source SHA-256 is
`38d4b7056830fde844f7ff37a03816aa666f6369b3873d45ef9339e765a63021`.

## Per-family decisions and outcome mappings

Here E denotes an estimate, L a named limit, N nonexistence, and F failure.
Unless stated otherwise, outcome kinds and successful coefficients are
unchanged. Adapter argument parsing still checks supplied/missing arguments
and their representation; the core checks the corresponding typed values.
Adapter checks of fields already present in a core result are not new
decisions from the original observations.

| Family | Decisions and mappings; evidence | Targeted strict entries fixed / retained |
|---|---|---|
| beta | Removed adapter data/control classification and payload suppression. Core validates controls before the constant-data branch and classifies invalid held shapes as `InvalidInput`, rather than iteration failure. Valid concentration remains N/unbounded. Appended C++ regression. | 0 / 0 |
| bnbinom | Core carries the hold-dependent size/beta swap symmetry and uniqueness. A fully held finite point is E/certified rather than E/uncertified. A certified optimum with disjoint size/beta coefficient boxes is E/certified/non-unique, with both reflected maximizers exposed. Exact symmetry proof conditional on the existing coefficient certificate and a 128-epsilon separation margin; C++ and R regressions. | 0 / 0 |
| cmb | Core carries Bernoulli-face attainment, uniqueness, and unidentified parameter bits; adapters use these fields for messages and metadata. Size-one Bernoulli face: L/uncertified becomes E/certified/non-unique with finite CMB representatives `(size,prob,nu)=(1,p,1)` and `(1,p,-1)`. Existing successful representative is preserved. Zero-size, size-one and probability-endpoint nonidentification is explicit. Dormant `Unidentified`: F/invalid-input becomes F/unresolved when there is no listed maximizing representative. Exact pmf identities; C++/R/JS regressions. | 0 / 0 |
| dlaplace | Audited unchanged. Core already owns validation, concentration/nonexistence, compared finite modes and named limiting payloads. No data/hold/control outcome override to move. | 0 / 0 |
| dweibull | Audited unchanged. Core already owns validation, concentration and Laplace-face decisions. Adapter observation access formats the core-decided constant-data message; it does not classify the fit. | 0 / 0 |
| fatigue | Free location refusal is now core `Unsupported` -> F/unsupported. Shifted core validates raw data, controls and held values, proves raw constant concentration before shifting, and carries transformation failure -> F/evaluation. Overflow or rounded shifts that manufacture constancy cannot become invalid-data/nonexistence claims. The existing TwoDiff check is retained in core. Exact concentration argument and appended numerical regressions. | 0 / 0 |
| frechet | Removed adapter majority/constant/hold decisions and routed all held fits through the core held helper. Singleton/strict minimum majority: F/invalid-input -> N/unbounded with a core proof witness. Two-observation half-count case: F/invalid-input -> N/unattained using the existing pair bound and limiting sequence. Core flags distinguish shape collapse and minimum majority for wording. Valid shift/normalization overflow and lost positive gaps are F/evaluation, not invalid data or a manufactured concentration proof. Held-scale concentration uses the exact raw subtraction condition. Existing raw status values remain diagnostic where proof flags disambiguate them; `outcome()` owns the decision. C++ regressions plus existing pair-bound proof in the dossier. | 6 (TASK-214.17) / 0 |
| genbeta | Free-scale unsupported decision moved into core. Core records the exact `b=1`, free `a,t` product ridge; the existing representative remains E but is certified non-unique, with the identified product attached. Removed adapter data/control classification. Updated stale comparison prose. Algebraic proof below; C++ and R regressions. | 0 / 0 |
| gompertz | Core validates controls before constant-data classification. Removed adapter empty/constant/overdispersion interceptions and payload suppression. Empty data retains F/invalid-input with corrected wording instead of the vacuous constant-data message. Existing exponential escape remains L. | 0 / 3: two TASK-214.5, one TASK-214.13 |
| hcauchy | Core handles held-scale validation and finite subtraction overflow; valid overflow is F/evaluation rather than invalid input. `Ok` log likelihood is the sum of the public density at the returned location and scale, repairing subnormal-scale payloads without moving coefficients. Removed adapter classification. Independent high-precision point comparison and C++ regression. | 2 (TASK-214.15) / 0 |
| huber | Removed adapter data/control/constant checks and finite-payload suppression; structured core outcomes now reach the renderer directly. Existing unbounded, normal/Laplace limit and comparison decisions remain core-owned. Updated comparison wording in the dossier; no optimizer change. | 0 / 0 |
| invexp | `Unbounded`: F/evaluation -> N/unbounded with the explicit inverse-exponential likelihood message. This branch is dormant on the validated finite snapshot data; no frozen case changes. | 0 / 0 |
| invparalogis | Removed adapter finite-result interception. Core now marks a failed search returning a nonrepresentable reciprocal coefficient or likelihood as numerical failure -> F/evaluation, rather than F/no-convergence, and carries a range-diagnostic flag. Preserved the old double-precision message. Existing successful points unchanged. Native/double C++ regression and the unchanged independent R log-scale optimization check pass. | 0 / 0 |
| invweibull | Removed adapter data/control reclassification and finite-payload suppression. Core already decides invalid input, concentration, numerical failure and finite fit; those mappings remain unchanged. | 0 / 0 |
| llogis | Removed adapter classification and payload suppression. Core already validates data/holds/controls and distinguishes concentration from search failure. No status or coefficient change. | 0 / 0 |
| logitnorm | Removed adapter data/control/constant classification. Core already distinguishes invalid unit-interval data and constant-data unboundedness. No status or coefficient change. | 0 / 0 |
| naka | Removed adapter classification and payload suppression. Core already owns support, constant-data concentration, controls and numerical/search failures. No status or coefficient change. | 0 / 0 |
| paralogis | Audited unchanged. Core owns statistical and input outcomes. Its remaining finite-result diagnostic formats information already carried in the result; no original-data decision remains there. | 0 / 0 |
| pareto3 | Free-minimum unsupported decision moved into core. `Unbounded`: plain failure -> N/unbounded; raw constant data with both positive parameters free has a direct shape-spike proof. Raw support/nonfinite data gets the core invalid-data reason, rather than adapter invalid-argument classification. Shift overflow gets `TransformFailure` -> F/evaluation. Exact subtraction checks prevent rounded held-scale equalities from inventing a spike. C++ regressions; held-scale spike condition documented. | 8 (TASK-214.17) / 4 TASK-214.5 |
| power | Core `OutsideSupport` now carries the held-endpoint/data relationship previously checked by the adapter; maps to F/invalid-input while preserving the specific support message. Concentration remains N/unbounded. Removed adapter data/control classification; appended C++ regression. | 0 / 0 |
| sgomp | Removed adapter data/control/constant classification and finite-payload suppression. Existing core decisions, including named faces, remain unchanged. | 0 / 0 |
| sn | Singleton is now admitted to the constant-data proof rather than rejected by sample size. Constant data with compatible location and free scale carries a core infinite-likelihood witness -> N/unbounded; invalid controls remain F/invalid-input. Removed adapter constant/hold decision. Normal-subfamily concentration proof and C++ regression. | 4 (TASK-214.17) / 0 |
| st | Core owns raw standardization failure: valid finite range overflow/underflow becomes F/evaluation rather than invalid data. All-residuals-zero/free-alpha tie: unresolved comparison -> E/certified/non-unique with exact finite alpha witnesses. Held-location ties with one-sided nonzero residuals: misleading closed half-law L -> N/unattained when the supremum is bounded; core carries a diagnostic path rather than falsely naming that density version. Free-location half-law endpoints restore the exact observed endpoint before evaluating the returned law. Proofs and rounding guard below; C++/R/JS regressions. | 6 (TASK-214.16) / 1 TASK-214.13 |
| trgamma | Singleton/constant concentration and budget candidate completion moved into the raw/fixed-scale core entry points; adapter no longer inspects observations to change kind or fill witnesses. Singleton F/invalid-input -> N/unbounded when a shape is free; fully held shapes retain the finite scale fit. Power and lognormal limit log likelihoods are recomputed on original observations at the returned coefficients, with point witnesses updated. C++ regressions and independent high-precision payload comparisons. | 8 (TASK-214.16), 4 (TASK-214.17) / 0 |
| weibull | Core validates controls/start/held shape before concentration. Removed adapter data/control classification. `Ok` log likelihood is reevaluated at the returned scale and shape rather than the centered profile point; coefficients unchanged. C++ regressions and independent high-precision payload comparisons. | 4 (TASK-214.15) / 0 |

## Mathematical evidence and its limits

The bnbinom symmetry is an exact exchange of size and beta. The change only
claims distinct global maximizers when an existing global certificate supplies
separated coefficient boxes; the computed separation includes a practical
128-epsilon margin. An uncertified numerical pair is not promoted by this
test. The fully held case has no optimization problem: its single feasible
finite point is a certified maximizer.

For CMB at size one the pmf is Bernoulli for every finite dispersion. The
Bernoulli fit reaches the saturated binary-sample upper bound, so finite CMB
points with dispersion 1 and -1 prove attainment and non-uniqueness. At size
zero, or at probability endpoints, the corresponding unused coefficients
are absent from the law. These are algebraic identification results, rather
than a multistart search claim. The cross-family CMB assertions in
`bindings/r/distributions/binom/test.R` and the JS stop tests were updated to
assert the attained finite family fit and its likelihood; the binomial fitter
was not changed.

For genbeta with `b=1`, write `lambda=a*t` and
`S=sum(-log(x/scale))`. The log likelihood is
`n*log(lambda)-lambda*S-sum(log(x))`. Its unique product maximizer is
`lambda=n/S`, while every positive pair on that product hyperbola maximizes
the original parameterization. The existing canonical `t=1` representative
is retained and its non-uniqueness is now stated by the core.

The concentration classifications use family-specific paths or bounds:
normal-subfamily scale concentration for sn, generalized-gamma concentration
for trgamma, and the existing Fréchet pair/majority arguments. Raw support
validation and exact subtraction checks precede delegated concentration
claims. Finite inputs whose transformations cannot be represented yield a
numerical evaluation failure, without claiming the statistical model invalid.

For st with held location, all observations at that location have skew factor
one for every finite alpha. When these are the only observations and scale
and degrees of freedom are held, the whole alpha line maximizes; alpha -1
and +1 are exact point witnesses. With `k>0` ties and `m>0` strictly one-sided
observations, the likelihood instead strictly increases along alpha toward
the appropriate infinity. Each nonzero skew factor tends to two while each
tied factor stays one. A closed-endpoint half-t or half-normal density doubles
the tied factors as well, so it is not the pointwise likelihood on this path.
The core now reports proved nonattainment instead of that incorrect named
limit.

The bounded cases use exact-arithmetic upper bounds conditional on computed
comparisons, with a practical 128-epsilon allowance in logarithms. The
Student normalizer is at most `phi(0)` by gamma Jensen. With scale held,
`L <= (2*phi(0)/sigma)^n`. With degrees of freedom held and scale free,
splitting scale at `d/sqrt(nu)` gives
`L <= (2*phi(0)*sqrt(nu)/d)^n` when `nu*m>=k`, where d is the minimum nonzero
distance. For free `nu>=1`, the bound
`t_nu(z)<=2*phi(0)/(1+z^2)` gives
`L <= (4*phi(0)/d)^n` when `m>=k`. These bounds prove finiteness; strict
monotonicity excludes every finite alpha even if the nuisance supremum has
not been found. `unavailable:st-shape-escape` is explicitly a diagnostic path,
and its selected likelihood is only a lower bound when nuisance parameters
are free. Free-location escapes can approach the endpoint from outside the
support and retain their proper named half-law limit.

The sign test uses `fma(nu,m,-k)`. For binary64 `nu=2/3`, `m=3`, `k=2`, the
exact represented exponent is `-2^-53`, although a separately rounded product
can equal two. The unbounded scale path must survive; the next representable
nu above that value satisfies the bounded-case test. Both cases have native,
double-only and binding regressions. This is a sign argument about represented
inputs, not a claim of outward-rounded interval arithmetic.

For hcauchy/weibull finite fits and st/trgamma named limits, the repaired
objective is the likelihood at the actual returned coefficients. Trgamma's
four archived defects per mode (`008012`, `008013`, `008015`, `008016`) are
lognormal limits, not power limits; both restoration routes are covered.
Trgamma retains feasible candidate values and widens a finite numerical upper
bound by the observed restoration discrepancy. That adjustment is a numerical
bound allowance, not a new global maximizer proof.

[reference-points.py](2026-11-14-core5b-b-data/reference-points.py) evaluates
all 20 original TASK-214.15/16 defects independently through the mpmath
reference seam at adaptive precision (at least 100 decimal digits, repeated
at least 50 digits higher). All references were stable; the largest relative
discrepancy was `1.6809298358965772e-15`, below the `1e-10` judgment threshold.
See [reference-points.json](2026-11-14-core5b-b-data/reference-points.json).
For example, the represented lognormal limit at archive `008012` has log
likelihood `74.77852803231016`, agreeing with the independent reference;
the old payload was `74.77852802265097`.
These comparisons certify neither global optimization nor coefficient
accuracy. No old successful coefficient was changed, so no replacement
coefficient was accepted on a golden or density-only comparison.

## Snapshot accounting

The exact requested family selection and both snapshot/diff commands are
recorded in [run-snapshots.sh](2026-11-14-core5b-b-data/run-snapshots.sh) and
[run-judges.sh](2026-11-14-core5b-b-data/run-judges.sh). Baselines are the
native/double archives under `2026-11-14-fit-snapshot-data`.

| Mutually exclusive category | Native | Double-only | Reason |
|---|---:|---:|---|
| Outcome meaning | 29 | 29 | Core-owned input/concentration reasons, certification, or non-uniqueness. |
| Core-owned rendering | 3 | 3 | Core invalid-input diagnostic replaces an adapter interception; wording is preserved or corrected. |
| Represented-point likelihood | 198 | 202 | Returned-law objective or its point witnesses are corrected; includes harmless last-bit ordinary-data differences. |
| Restored limit endpoint | 13 | 13 | Exact observed st endpoint restored before evaluating the named law. |
| Total changed | 243 | 247 | No unclassified difference. |
| Unchanged | 3,125 | 3,121 | Of 3,368 cases per mode. |

| Family with differences | Native | Double-only |
|---|---:|---:|
| bnbinom | 15 | 15 |
| cmb | 2 | 2 |
| frechet | 3 | 3 |
| gompertz | 1 | 1 |
| hcauchy | 60 | 60 |
| pareto3 | 8 | 8 |
| sn | 1 | 1 |
| st | 104 | 105 |
| trgamma | 24 | 25 |
| weibull | 25 | 27 |

Every other assigned family has zero frozen snapshot differences. Each
differing case is listed with category, reason, changed fields and complete
old/new values in
[native-differences.jsonl.gz](2026-11-14-core5b-b-data/native-differences.jsonl.gz)
and [double-differences.jsonl.gz](2026-11-14-core5b-b-data/double-differences.jsonl.gz).
[classify-diffs.py](2026-11-14-core5b-b-data/classify-diffs.py) rejects any
unclassified case, changed input hash, new refusal of an old successful fit,
or changed successful coefficient. Full final records and their provenance
are retained as `native.jsonl.gz`, `double.jsonl.gz` and the corresponding
`.provenance.json` files in the data directory. No baseline source archive
was created.

## Strict-layer result

The judges ran with `--prune` on a temporary known-list copy, followed by
scoped pruning of the canonical file. Only the 42 assigned TASK-214.15/16/17
entries were removed: 20 density/limit-payload entries and 22 invalid-data
entries. There were no added known entries or changes to protected entries.
The original entries remain in
[pruned-known.json](2026-11-14-core5b-b-data/pruned-known.json).

TASK-214.18 work covered the st exact alpha-line witnesses, CMB identification
and finite representatives, and stale comparison wording in genbeta, huber
and st. Gompertz's empty/nonfinite classification was checked in the final
snapshot. No assigned render-2 open item requires a new shared API.

| Judge | Final result |
|---|---|
| fit-judge | 3,368 cases in each mode; only the two protected mode entries. |
| fit-density | No failure; all 20 original scoped payload defects pass. |
| fit-consistency | No failure; all 22 original scoped data-reason defects pass. |
| fit-refine | 992 cases judged; only six protected accuracy entries. |
| strict-seam native | 9,694 records: 7,331 pass, 2,363 terminal stops; six guarded cases replayed at 300 seconds, all terminal. |
| strict-seam double-only | 9,694 records: 7,329 pass, 2,364 terminal stops, one existing EMG open fit; four guarded cases replayed at 300 seconds. |
| `just strict build/snapshots/native build/snapshots/double` | Exit 0: “strict layer: no failure outside the known list”. |

## Suites and late-fix validation

All commands used the workstation, GCC 15.2.0. Builds and full suites used
`run4`; snapshots, evidence and strict judging used `run`. Runtime/scratch
paths were inside `build/tmp` and `build/scratch`. The required full suites
were run once; the late family fix received the targeted reruns required by
the brief instead of another unrelated full C++ or R dev run.

| Command/check | Result |
|---|---|
| `just cpp-test` | 110/110 pass; 328.69 seconds. |
| `just cpp-test-no-long-double` | 154/154 pass; 502.88 seconds, on the workstation. |
| `just test` (initial dev run) | Three failures, no warnings; 157 skips, 36,228 passes. Resolution described below. |
| Native invparalogis targeted C++ rebuild/test after the late fix | 1/1 pass. |
| Double-only invparalogis targeted C++ rebuild/test after the late fix | 1/1 pass. |
| `just regen` after the late fix | Pass. |
| Both affected R dev files after the late fix | 0 failures, 0 warnings, 4 skips, 124 passes. |
| `just test-full`, final source | 0 failures, 0 warnings, 1 skip, 65,378 passes; 1,037.151 seconds. |
| `just js-build` | Pass. |
| `just js-test` | 585/585 pass, no skips. |
| `just site-check` | Pass, including dossier/fitter signature and snippet checks. |
| Final native/double preflight and snapshots | Pass; both source hashes checked against final source. |
| `git diff --check` | Pass. |
| `python3 dev/tools/check_links.py` | Pass: 691 Markdown files, 5,141 destinations, zero new broken links; 45 inherited findings. |

The initial R dev failures were exactly two old CMB-limit assertions in
`binom/test.R:231,232`, and `invparalogis/test.R:43` expecting the existing
double-precision diagnostic. The former now assert the proved attained
non-unique CMB estimate and preserve likelihood checks. The latter test was
left unchanged: the core gained the nonrepresentable-search flag and the
adapter retained its old range message. Both complete R files then passed,
as did the full final-source R suite. The initial dev failure log is retained;
it is not represented as an initially green run. See
[late-recheck-resolution.json](2026-11-14-core5b-b-data/late-recheck-resolution.json)
and [suite-results.tsv](2026-11-14-core5b-b-data/suite-results.tsv).

The st fused-sign fix preceded the passing full C++ modes; later invparalogis
core changes received native and double targeted C++ checks. Final-source
preflights independently rechecked the core outcome/payload examples in both
modes. All commands started for the job have finished.

## Retained open entries and coverage limits

No new in-scope open entry remains. The following are deliberately retained,
as requested, with their original details in
[retained-known.json](2026-11-14-core5b-b-data/retained-known.json):

- TASK-214.5: Gompertz archive `002793` in both modes; Pareto3 `005590`,
  `005591`, `005593` in double-only mode and `005591` in native mode. These
  six entries require coefficient-accuracy work beyond this job; no
  coefficient was changed to hide them.
- TASK-214.13: Gompertz `class/huge` and st `archive/007040` remain the two
  cross-mode differences. Resolving the precision-mode behavior is outside
  this job.
- Global seam coverage retains the existing
  `seam-open|double|emg|archive|001311`: no terminal fit within the 300-second
  replay guard. EMG is outside the assigned families; nothing was added to
  the known list for it.
- Refinement reports eight CMB records without cached references (four
  cases in two modes), and existing open refinement attempts: beta scaled
  Newton budget (4) / line search (2), CMB 120-second guards (8) /
  300-second guards (2), hcauchy with no free regular coefficient (12),
  naka line search (2). Integer or support/domain-bound coefficients are
  separately identified by that judge. These are coverage limitations,
  not evidence that unchanged coefficients meet the accuracy contract.
  Detailed counts and case diagnostics are in `fit-refine.log.gz` and
  `strict.log.gz`.
- The density judge explicitly lists diagnostic or unavailable limit models
  lacking a public C++ density. Those paths were not counted as independently
  density-validated; the st held-endpoint path is intentionally diagnostic.

No change to shared outcome types, renderers, modifier code, validation
Python, generators, settled d/p/q/r, or forbidden status/roadmap/NEWS files
was needed. Family dossiers contain only the relevant family arguments and
conditions; scripts, snapshots and logs are confined to the evaluation data
directory. Logs and files over 200 kB are gzip-compressed.
