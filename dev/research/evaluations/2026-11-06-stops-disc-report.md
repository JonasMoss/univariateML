# Structured stops: discrete-family sweep (TASK-141.2)

Completed 2026-11-06 in `codex/stops-disc`, based on
`164d41784756606a48ddc0cedc67752acab4050b`. Changes are uncommitted, as requested.
All required final validation runs passed and every command started for this job
has exited. The audit covers all 30 assigned families.

## Contract and implementation

Statistical stops now transport evidence computed in the core to the common
binding, R conditions and JavaScript errors. Successful-fit decisions, existing
messages, goldens, d/p/q/r, truth fixtures and optimizer criteria are unchanged.
Input/control errors and numerical or iteration failures remain ordinary errors.
An explicit work-budget guard inside a mixed legacy failure status sets
`StopDetail::budget_exhausted`; the binding then reports `budget` while retaining
the original status and message.

`StopDetail::limit_model` identifies the selected diagnostic face. Target
coefficients use that model's public order. Face and candidate likelihoods are
**total** likelihoods in the original observations' units. The translations here
have unit counting-measure Jacobian: adjacent laws retain their original support,
and the negative-hypergeometric growing-urn face retains its translation by `r`.
The zero-modified binomial transport includes the fitted/held zero-indicator
likelihood, rather than reporting only the likelihood of the positive counts.
Bindings do not refit models or reconstruct searches.

Dedicated point-mass, arbitrary two-point and translated negative-binomial
families do not have package ids. Their payload `model` values explicitly say
`(no package family)`; these are descriptive labels, not dispatchable model ids.
Their named coordinates are `(point)`, `(lower, upper, prob)` (probability at the
upper point), and `(size, prob, shift)`. A law on `{0,1}` uses package `binom`
with `(size=1, prob)` instead. Poisson and other package faces use their actual ids.

The discrete-normal held-mean seam needs particular care: at an integer mean
`k`, zero scale gives equal mass to cells `k-1` and `k`; a noninteger mean
concentrates in its containing cell. Held CMB endpoint odds are raised to the
support width. Held CMP lambda fixes the adjacent Bernoulli odds. These faces
must not silently use empirical probabilities. Extreme held odds are evaluated
with stable log probabilities: the reported probability can round to zero or
one, while the likelihood remains evaluated from the original held odds.

Unresolved comparisons retain evaluated finite and face witnesses. An infinite
global upper bound explicitly records that the remaining search/tail has not
been excluded; an absent lower bound or unavailable scalar remains NaN. A
preflight budget may have no candidates. Candidate intervals describe evaluated
witnesses, not global bounds, and candidates do not assert enumeration of every
maximizer. No estimates are invented for `nomle` stops.

See the [shared conventions](../../design/structured-stop-details.md), the new
[core helper](../../../univariateML-cpp/include/cppunivariate/core/discrete_stop.hpp),
and the updated family dossiers.

## Family inventory

Every row concerns stop dispatch; successful closures and warning-only ties are
preserved. “Ordinary” means no structured statistical stop is currently needed
for the listed failure paths. Invalid inputs/controls and other numerical
failures remain ordinary in every family, including rows with structured stops.

| Family | Status / branch | Kind | Payload |
|---|---|---|---|
| bbinom | `BinomialLimit`; `TwoPointLimit`; `PointMass` | `limit` | Fitted binomial `(size,prob)`, endpoint law in original counts, or point mass; total face likelihood. |
| bbinom | `Unidentified` | `tie` | Identified Bernoulli/point law, without invented unidentified shapes. |
| bbinom | `SizeBudget`; `ProfileTie`; `FaceUnresolved` | `budget`; `tie`; `unresolved` | Available finite profiles, fitted NB face when computed, candidate likelihoods, witness lower bound and infinite global upper bound. Early preflight can be empty. |
| binom | `Underdispersed` | `limit` | Poisson `(lambda=mean(x))` and its evaluated total likelihood. |
| bnbinom | `NegativeBinomialLimit`; `PoissonLimit`; `PointMass` | `limit` | The NB or Poisson fit already computed by the face comparison; zero-count face is Poisson lambda zero. Held NB size is preserved. |
| cmb | `PointMass`; `AdjacentLimit`; `EndpointLimit` | `limit` | Point/adjacent/endpoint law; held probability constrains the face weights. |
| cmb | `FaceTie`; `FaceUnresolved`; explicit work guards within `IterationFailure` | `tie`; `unresolved`; `budget` | Available finite witness and CMP/two-point face, evaluated likelihoods and conservative global bounds. Empty preflight is allowed. |
| cmp | Domain entry: `PointMassLimit`; `TwoPointLimit`; compatibility `GeometricLimit` | `limit` | Point/adjacent law, including held-lambda weights; geometric `(prob)` compatibility payload. Geometric and zero-count closures actually returned as `Ok` stay successful. Direct legacy entry gap is recorded below. |
| dgamma | `PointMass`; `TwoPointLimit` | `limit` | Point law or empirical adjacent law on the original counts; total likelihood. |
| dlaplace | `PointMass` | `limit` | Point at the fitted/held integer location, likelihood zero on the log scale. |
| dnorm | `PointMass`; `TwoPointLimit`; `ScaleLimit` | `limit` | Point/adjacent law; held-mean zero-scale face uses the correct cell/seam probabilities. |
| dunif | Endpoint/data incompatibility | Ordinary | No new payload. Singleton-support fits remain successful. |
| dweibull | `PointMass`; `BernoulliLimit`; `TwoPointLimit`; `HeldBernoulliLimit` | `limit` | Point, binomial size one, or translated adjacent law. Held `shape1` is the Bernoulli success probability. |
| geom | Invalid/incompatible held probability or nonfinite calculation | Ordinary | No new payload. Attained zero-count closure remains successful. |
| hyper | `AllZero`; `FaceTie`; `FaceUnresolved` | `limit`; `tie`; `unresolved` | Point zero or fitted binomial/Poisson face plus best evaluated finite `(m,N,k)` witness; unresolved upper bound is infinite. |
| hyper | `Infeasible`; explicit work guards within `NumericalFailure` | `nomle`; `budget` | No fabricated feasible estimate; budget retains available face/finite witnesses and bounds, or an empty preflight. |
| lgser | `PointMass` | `limit` | Point at one, total log likelihood zero. |
| nhyper | `FaceTie`; `FaceUnresolved` | `tie`; `unresolved` | Translated NB `(size,prob,shift=r)` face and finite `(m,N,r)` witness, in total-draw units; infinite unresolved upper bound. |
| nhyper | `Infeasible`; explicit work guards within `NumericalFailure` | `nomle`; `budget` | Empty non-existence payload; budget retains available evidence or an empty preflight. |
| pois | Invalid/incompatible rate or nonfinite calculation | Ordinary | No new payload. Lambda zero remains an attained fit. |
| skellam | Invalid input, incompatible fixed rates (`Impossible`), numerical failure | Ordinary | No new payload. Existing attained rate-zero boundaries are preserved. |
| tbinom | `LowerPointMass`; `UpperPointMass`; `TpoisLimit` | `limit` | Point law or truncated Poisson `(lambda,a,b)` with the original conditioning bounds and likelihood. |
| tpois | `LowerPointMass`; `UpperPointMass` | `limit` | Point at the conditioning-support endpoint, total log likelihood zero. Singleton conditioning remains an attained successful fit. |
| yulesimon | `PointMass` | `limit` | Point at one, total log likelihood zero. |
| zeta | `PointMass` | `limit` | Point at one, total log likelihood zero. |
| zib | `ZipLimit` | `limit` | Fitted ZIP `(lambda,pi)` and unconditional likelihood, including the optimized face with pi held. |
| zip | `ZeroLimit` | `limit` | Poisson lambda zero and log likelihood zero. |
| zipf | `PointMass`; legacy `ShapeNegative` dispatch | `limit`; `nomle` | Point at one; empty non-existence payload for the legacy branch (the current solver does not emit `ShapeNegative`). `AllOnes` success/warning is unchanged. |
| zmbinom | `PoissonLimit` | `limit` | Zero-modified Poisson `(lambda,p0)` with zero-indicator likelihood restored. |
| zmgeom | Invalid input or unresolved numerical evaluation | Ordinary | No new payload. `AllZero`/`TwoPoint` successes and warnings remain unchanged. |
| zmlogarithmic | Invalid input or unresolved numerical evaluation | Ordinary | No new payload. Attained zero/two-point laws and warnings remain unchanged. |
| zmpois | Invalid input or unresolved numerical evaluation | Ordinary | No new stop payload. Shared result gains the default-empty field needed by zmbinom. Attained closures remain unchanged. |
| ztbinom | `PoissonLimit` | `limit` | Zero-truncated Poisson `(lambda)` and its fitted positive-count likelihood. |
| ztgeom | Invalid input or unresolved numerical evaluation | Ordinary | No new payload. Point-one closure remains successful. |
| ztpois | Invalid input or unresolved numerical evaluation | Ordinary | No new payload. Lambda-zero point-one closure remains successful. |

The existing hypergeometric `Tie`/`Unidentified` warning-and-success paths were
not converted into errors. Historical singleton `Unidentified` stop dispatches
in the truncated-family bindings are classified as ties if reached; current
family fitters resolve those singleton closures successfully.

## Mathematics and numerical scope

This sweep transports existing decisions; it does not replace profile searches,
prove new global maxima, or strengthen an inherited numerical limit decision.
Closed-form point and two-point payloads use the corresponding exact-arithmetic
law and its direct likelihood. Search witnesses use the existing comparisons
and practical floating-point margins, conditional on computed comparisons,
not outward-rounded interval arithmetic. In particular, the pre-existing
numerical face decisions in BNB and the zero-binomial/truncated-binomial search
paths remain numerical to the extent stated in their dossiers. A finite
candidate is never promoted to a successful fit by this change.

The new tests check payload arithmetic and transport, not existence theorems.
Golden regression checks successful fixtures, not all possible data. Raw
candidate likelihoods are not claimed as validated interval-arithmetic bounds.
The conservative infinite global bounds avoid claiming a tail certificate
where none was computed.

## Validation

The tested base revision and SHA-256 hashes of all 92 changed source/documentation
files are in [source-manifest.json](2026-11-06-stops-disc-data/source-manifest.json).
[validation.json](2026-11-06-stops-disc-data/validation.json) records the final
commands, machines and results. Raw final logs are retained alongside it; evidence
is below 1 MB. There were no source edits after the final C++/R validation
snapshot; the final JS test followed the explicit successful `just js-build`.

| Final command | Machine | Result |
|---|---|---|
| `just cpp-test` | workstation and simbox | 106/106 dev shards on each |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 full shards |
| `just cpp-test-no-long-double` | simbox | 127/127 shards |
| `just test` | workstation | FAIL 0, WARN 0, SKIP 154, PASS 20692 |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 43507 |
| `just js-build` | workstation | Passed |
| `just js-test` after that build | workstation | 423/423; zero failures, cancellations or skips |
| `just site-check` | workstation | Passed; 99 family pages and C++ examples, links, MathML, browser checks |
| `git diff --check` | workstation | Passed |

Local commands used `/tmp/umvl-cpu/run` or `run4`; remote suites used the supplied
`simbox-run`. Long jobs ran at nice 10, with BLAS/OpenMP threads pinned by the
runners. Temporary files and logs stayed in the worktree. Local R was 4.6.1;
local GCC was 15.2.0 (Ubuntu `15.2.0-16ubuntu1`), simbox GCC 15.2.0 (Ubuntu
`15.2.0-14ubuntu1~24~ppa1`), and the local Node runner was v22.22.1. The JS build
used the existing Emscripten SDK. Existing compiler warnings, including the
wasm binomial precision self-comparison, are retained in logs; R test WARN counts
are zero.

Appended C++ cases exercise the new payloads with closed-form likelihoods where
possible, held-parameter weights, original support, zero-modified likelihood
transport, finite/face witnesses, empty budgets and extreme odds. R checks in
`bindings/r/distributions/binom/test.R` cover limit/tie/budget classes, named
models and estimates, total likelihoods, empty budgets and ordinary invalid
input. JS checks cover the corresponding public Error fields. New fixtures are
deterministic; existing suites retain their checked-in seeds (including seed 7
in the scale-equivariance test). C++ likelihood checks generally use doctest
Approx; R uses testthat's comparison tolerance; the simple JS coefficient
fixtures use exact comparisons. No tolerances or goldens were relaxed.

Earlier validation failures were resolved or explicitly separated:

- An initial added Zipf test called the fitter with the wrong argument list;
  corrected before final green runs.
- Propagating CMB entropy-bound budget evidence initially broke existing direct
  helper calls. A forwarding overload now preserves their original signature;
  all final C++ tiers pass.
- A plain native syntax probe of the Emscripten binding could not find
  `emscripten/bind.h`; the actual Emscripten build and JS tests passed.
- An earlier simbox R dev run had FAIL 1, WARN 0, SKIP 154, PASS 20691: the
  unchanged `mlst` scale-equivariance df check at seed 7 missed tolerance
  (`1272 - 1272 = -0.274`, tolerance `2e-4`). Both final local R tiers passed
  without changing that fitter or test. The
  [earlier log](2026-11-06-stops-disc-data/earlier-simbox-r-failure.log) preserves
  that machine-specific result; it is not represented as a simbox R pass.

## Open entries and protected scope

1. **Direct legacy CMP entry point.** The family's `cmp_domain_mle` and both
   bindings carry the payloads and are tested. Direct callers of
   `cppunivariate::cmp_mle` still receive the legacy `CmpResult` without a stop
   field, because its definition and solver live in the protected
   `special/cmp.hpp`. Extending that legacy interface requires an explicitly
   authorized change there, followed by direct legacy-entry tests. This job
   instead supplies a derived result at the dossier-level domain entry without
   modifying the settled special-function implementation.
2. **Simbox R `mlst` scale-equivariance comparison.** The specific seed-7 scale check
   above still needs an independent machine-level investigation if simbox R
   green is required. It lies outside the assigned discrete families. The
   required final R dev/full results in this report are workstation results.

No d/p/q/r, truth, code-generation, NEWS, STATUS, KNOWN_ISSUES or roadmap changes
were needed or made. No other worktree, board record, commit or remote branch
was modified.

Artifact display was attempted with the prescribed `bin/show` command. The
viewer returned HTTP 403 because this worktree is absent from its readable-root
list. The report and evidence remain in this worktree; no viewer configuration
or other checkout was changed to bypass that restriction.
