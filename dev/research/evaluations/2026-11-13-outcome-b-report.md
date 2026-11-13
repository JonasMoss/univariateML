# TASK-214.6: outcome-b

All 25 assigned families now expose the shared outcome; 26 result types are mapped, including the CMB size-profile result. Both 3,368-case snapshots retain every original output field: **0 cases differ in either mode; 25/25 families report an outcome on every case; 0 disagreements in either mode**. The workstation suites passed **109/109 native** and **136/136 double-only**.

Worktree base: `1e0f5185432e858b39ceffa18f043ce4d54993b1`. Archived snapshot baseline: `25629c98f045427d67fc22eea82d52102853ea8e`, recorded in the supplied 2026-11-12 provenance files. Certification comes from existing result flags, dossier declarations and existing face decisions; this job adds no numerical certificate. The [structural check](2026-11-13-outcome-b-data/core-diff.log) confirms that removing the added outcome overloads and include leaves all 25 core headers unchanged, ignoring whitespace. Its [checker](2026-11-13-outcome-b-data/check-core-diff.py) reconstructs the baseline with `git show`.

`E(certified, unique=true)` denotes estimate; `L(certified)` limit; `N(reason)` nonexistence; `F(reason)` failure; `B` budget. `c` is the result's `certified`; `g` is genbeta's `allfree_certified`; `upper` is `stop_detail.loglik_upper`. Grouped enumerator names share the stated mapping.

Flag rules precede the base enum cases in the order shown. `A` maps `accuracy_stop` Tolerance/Conditioning to the corresponding failure reason. `B` in flag rules tests `stop_detail.budget_exhausted`; `sn/st` explicitly use the result's budget flag. `T` maps `comparison_tie` to `E(false)`. `Tsn` gives `E(true,false)` for the existing finite, equal-bound identifiability diagnostic and `E(false)` otherwise. `Tst` gives `E(c,!c)`: certified equal faces are non-unique; unseparated comparisons are uncertified. Tie stops retain their literal `tie` kind. Paralogistic budget/tie detail flags do not supersede its status enum, matching its adapters.

The unreached sets are identical in native and double-only snapshots. Coverage is of enumerators, not every flag combination or custom control.

| Family | Result types | Flag rules; base enumerator mappings | Unreached enumerators (both modes) | Estimate certification rule |
|---|---|---|---|---|
| beta | `BetaResult` | `Ok`→`E(true)`; `Degenerate`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)`; `InvalidInput`→`F(InvalidInput)` | — | dossier "yes" |
| bnbinom | `BnbinomResult` | **A; accuracy_failure→F(NoConvergence); B**<br>`Ok`→`E(c)`; `InvalidInput`→`F(InvalidInput)`; `NegativeBinomialLimit, PoissonLimit, PointMass`→`L(c)`; `IterationFailure`→`F(NoConvergence)`; `NumericalFailure`→`F(Evaluation)`; `ComparisonTie`→`E(false)` | `IterationFailure`, `ComparisonTie` | flag `certified` |
| cmb | `CmbResult` and `CmbSizeResult` (delegates to `fit`) | **A; B**<br>`Ok`→`E(true)`; `InvalidInput, Unidentified`→`F(InvalidInput)`; `PointMass, AdjacentLimit, EndpointLimit`→`L(true)`; `IterationFailure`→`F(NoConvergence)`; `FaceUnresolved`→`F(Unresolved)`; `FaceTie`→`E(false)`; `NotImplemented`→`F(Unsupported)` | `EndpointLimit`, `Unidentified`, `NotImplemented` | dossier "yes" |
| dlaplace | `DlaplaceResult` | **A**<br>`Ok`→`E(true, !r.not_unique)`; `InvalidInput`→`F(InvalidInput)`; `PointMass`→`L(true)` | — | dossier "yes" |
| dweibull | `DweibullResult` | **A**<br>`Ok`→`E(true)`; `InvalidData`→`F(InvalidInput)`; `InvalidControl`→`F(InvalidArgument)`; `PointMass, BernoulliLimit, TwoPointLimit, HeldBernoulliLimit`→`L(true)`; `ShapeUnidentified`→`E(true, false)`; `Unrepresentable`→`F(Evaluation)`; `IterationFailure`→`F(NoConvergence)` | `InvalidControl`, `ShapeUnidentified`, `IterationFailure` | dossier "yes" |
| fatigue | `FatigueResult` | **A; SearchFailure only: B then T**<br>`Ok`→`E(true)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `SearchFailure`→`F(NoConvergence)` | — | dossier "yes" |
| frechet | `FrechetResult` | **A; SearchContradicted→F(Evaluation); B then T**<br>`Ok`→`E(c)`; `InvalidData`→`!isnan(upper) ? N(upper == ∞ ? Unbounded : Unattained) : F(InvalidInput)`; `GumbelLimit`→`L(true)`; `IterationFailure`→`F(NoConvergence)`; `NoMLE`→`N(upper == ∞ ? Unbounded : Unattained)`; `NumericalFailure, SearchContradicted`→`F(Evaluation)` | `IterationFailure`, `NoMLE`, `SearchContradicted` | flag `certified` |
| genbeta | `GenbetaResult` | **A; B then T**<br>`Ok`→`E(g)`; `UnitGammaBoundary, PowerBoundary`→`L(g)`; `InvalidData`→`F(InvalidInput)`; `InvalidFixed`→`F(InvalidArgument)`; `SearchUnresolved`→`F(NoConvergence)`; `Unbounded`→`N(Unbounded)` | `InvalidFixed` | dossier other (`partial`); scoped flag `allfree_certified` |
| gompertz | `GompertzResult` | **A**<br>`Ok`→`E(true)`; `Overdispersed`→`L(true)`; `IterationFailure`→`F(NoConvergence)`; `Unbounded`→`N(Unbounded)` | — | dossier "yes" |
| hcauchy | `HcauchyResult` | `Ok`→`E(true)`; `InvalidData`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `ScaleBoundary`→`N(Unattained)`; `SearchFailure`→`F(NoConvergence)` | `SearchFailure` | dossier "yes" |
| huber | `HuberResult` | **A; B then T**<br>`Ok`→`E(true)`; `InvalidData`→`F(InvalidInput)`; `UnboundedLikelihood`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)`; `NormalLimit, LaplaceLimit`→`L(true)` | — | dossier "yes" |
| invexp | `InvExpResult` | `Ok`→`E(true)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded, IterationFailure`→`F(Evaluation)` | `Unbounded` | dossier "yes" |
| invparalogis | `InvParalogisResult` | **A**<br>`Ok`→`E(c)`; `BfgsFailure`→`F(NoConvergence)`; `InvalidStart`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `NumericalFailure`→`F(Evaluation)`; `CertificateBudget`→`B` | `CertificateBudget` | flag `certified` |
| invweibull | `InvWeibullResult` | `Ok`→`E(c)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)` | — | flag `certified` |
| llogis | `LogLogisticResult` | `Ok`→`E(c)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)` | — | flag `certified` |
| logitnorm | `LogitnormResult` | `Ok`→`E(c)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `IterationFailure`→`F(Evaluation)` | `IterationFailure` | flag `certified` |
| naka | `NakaResult` | `Ok`→`E(c)`; `Degenerate`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)`; `InvalidInput`→`F(InvalidInput)`; `TransformFailure`→`F(Evaluation)` | `IterationFailure` | flag `certified` |
| paralogis | `ParalogisResult` | **A**<br>`Ok`→`E(c)`; `BfgsFailure`→`F(NoConvergence)`; `InvalidStart`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `NumericalFailure`→`F(Evaluation)`; `CertificateBudget`→`B` | `CertificateBudget` | flag `certified` |
| pareto3 | `Pareto3Result` | **A**<br>`Ok`→`E(c)`; `InvalidData`→`F(InvalidInput)`; `Unbounded, AccuracyFailure`→`F(Evaluation)`; `IterationFailure`→`F(NoConvergence)` | `AccuracyFailure` | flag `certified` |
| power | `PowerResult` | `Ok`→`E(true)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `IterationFailure`→`F(Evaluation)` | `IterationFailure` | dossier "yes" |
| sgomp | `SgompResult` | **A; IterationFailure only: B then T**<br>`Ok`→`E(true)`; `InvalidInput`→`F(InvalidInput)`; `Collapse`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)` | — | dossier "yes" |
| sn | `SnResult` | **A; Tsn; IterationFailure with result budget_exhausted→B**<br>`Ok`→`E(true)`; `InvalidInput`→`F(InvalidInput)`; `IterationFailure`→`F(NoConvergence)`; `RightHalfNormalBoundary, LeftHalfNormalBoundary`→`L(false)`; `AccuracyFailure`→`F(Evaluation)` | `AccuracyFailure` | dossier "yes" |
| st | `StResult` | **A; accuracy_failure→F(NoConvergence); Tst; result budget_exhausted→B**<br>`Ok`→`E(c)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `ScaleBoundary`→`r.reason == 6 ? N(Unattained) : F(NoConvergence)`; `NumericalFailure`→`F(r.reason >= 1 && r.reason <= 4 ? NoConvergence : Evaluation)`; `SkewNormalBoundary, RightHalfTBoundary, LeftHalfTBoundary, RightHalfNormalBoundary, LeftHalfNormalBoundary`→`L(c)` | — | flag `certified` |
| trgamma | `TrGammaResult` | **A; B then T; transformation_failure or reconstruction_failure→F(Evaluation)**<br>`Ok`→`E(true)`; `LognormalBoundary, PowerBoundary`→`L(false)`; `ScaleOutOfRange`→`F(Evaluation)`; `InvalidData`→`F(InvalidInput)`; `InvalidFixed`→`F(InvalidArgument)`; `IterationFailure`→`F(NoConvergence)` | `ScaleOutOfRange`, `InvalidFixed` | dossier "yes" |
| weibull | `WeibullResult` | **A**<br>`Ok`→`E(true)`; `InvalidInput`→`F(InvalidInput)`; `Unbounded`→`N(Unbounded)`; `IterationFailure`→`F(NoConvergence)`; `AccuracyFailure`→`F(Evaluation)` | `IterationFailure` | dossier "yes" |

`InvExpStatus::Unbounded` and `Pareto3Status::Unbounded` currently reach plain range/fitting errors, not `nomle`; they therefore retain `F(Evaluation)` for compatibility. The inverse-exponential enumerator was not reached. CMB's plain `Unidentified` rejection uses the nearest existing reason, `InvalidInput`. Power and logit-normal `IterationFailure` are closed-form representation failures and map to `Evaluation`; inverse exponential's reciprocal-range failure does likewise. Coarse iterative failure statuses without a distinct arithmetic marker use `NoConvergence`.

**Adapter context absent from the result.** The existing classification guards still use the original data, controls and payload checks. In particular:

- `SnStatus::InvalidInput` can emit `nomle` for compatible constant data with free scale; otherwise it is a plain input error. The attached outcome follows that existing data/hold decision.
- `TrGammaStatus::InvalidData` can emit `nomle` for the existing positive-constant-data/hold/control predicate, including the stored infinite-likelihood case; otherwise it is a plain input error. The attached outcome follows that decision.
- `FrechetStatus::InvalidData` with all coefficients free can emit `nomle` for a finite sample with a strict minimum majority. That decision needs the original sample and hold mask. Stored collapse bounds are handled in the core mapping itself.
- `GompertzStatus::Overdispersed` is a core limit, but its guard can refuse the classification for invalid original data/controls or an unrepresentable limit payload. The adapter attaches `InvalidInput` for the former and `Evaluation` for the latter. The snapshots reach this on the `inf` and `neginf` classes; neither is a success override.

CMB's additional hold/data tests select different `FaceTie` messages, including the size-one Bernoulli face; its result lacks a separate proof marker for that message branch, so the mapping conservatively uses `E(false)`. The trgamma lognormal call only computes a diagnostic likelihood gap: the trgamma call decides its outcome. Non-finite estimate guards retain their original emitted kinds and use `overridden`; the paralogistic guards therefore keep their literal `numerical` kind until the override is attached.

**Existing core defects exposed by success overrides.** Each row occurs once in each mode. Both old adapter errors remain unchanged. Numerical repairs remain open outside this no-fit-change job; exact reproduction is provided by [replay-overrides.py](2026-11-13-outcome-b-data/replay-overrides.py) and its [output](2026-11-13-outcome-b-data/core-overrides.log).

| Family/input class | Core return | Attached adapter outcome |
|---|---|---|
| gompertz, class `subnormal`, all free: `{5e-324,1e-323,1.5e-323}` | `Ok`; `a=Inf`, `b=Inf`; finite `logLik=0x1.16b81d2c76702p+11`; 4 iterations | `F(Evaluation)`, `outcome_override=true` |
| weibull, archive `008443`, 12 observations of order `2^500`, both coefficients held | `Ok`; shape `2.9155677520153174`, scale `3.961112786792183`; `logLik=-Inf` native, NaN double-only; 0 iterations | `F(Evaluation)`, `outcome_override=true` |

**Final checks.** Every run used the workstation, GCC 15.2, the CPU-slot wrappers and one thread per worker. Snapshots used `--jobs 1`, default CPU limit 300 seconds, and the requested family list. There were no timeout/crash cases or CPU-limit reruns. Both C++ suites ran once, after all source edits, through `run4`.

After the full snapshots, the final reason/guard refinements refreshed only `power`, `logitnorm`, `invparalogis` and `paralogis` (443 cases per mode). The full-directory diffs and outcome audits below were then rerun. [Native refresh output](2026-11-13-outcome-b-data/native-targeted-snapshot.log), [double refresh output](2026-11-13-outcome-b-data/double-targeted-snapshot.log), and the saved full/targeted provenance JSON files record these stages. No suite rerun was needed: the refinements preceded both suite builds.

| Required command (common prefix: `/tmp/umvl-cpu/run python3 dev/validation/fit-snapshot.py`) | Final output | Complete output |
|---|---|---|
| `--output build/snap/native --family $F --jobs 1` | 25 families, 3,368 cases completed | [native snapshot](2026-11-13-outcome-b-data/native-snapshot.log) |
| `--output build/snap/double --family $F --jobs 1 --no-long-double` | 25 families, 3,368 cases completed | [double snapshot](2026-11-13-outcome-b-data/double-snapshot.log) |
| `--diff $B/native.jsonl.gz build/snap/native --family $F --ignore outcome,core_status,outcome_override` | `0 of 3368 cases differ` | [native diff](2026-11-13-outcome-b-data/native-diff.log) |
| `--diff $B/double.jsonl.gz build/snap/double --family $F --ignore outcome,core_status,outcome_override` | `0 of 3368 cases differ` | [double diff](2026-11-13-outcome-b-data/double-diff.log) |
| `--outcomes build/snap/native` | 25/25 complete; 0 disagreements; overrides: gompertz 1, weibull 1 | [native outcomes](2026-11-13-outcome-b-data/native-outcomes.txt) |
| `--outcomes build/snap/double` | 25/25 complete; 0 disagreements; overrides: gompertz 1, weibull 1 | [double outcomes](2026-11-13-outcome-b-data/double-outcomes.txt) |
| `/tmp/umvl-cpu/run4 just cpp-test` | `100% tests passed, 0 tests failed out of 109` | [native suite](2026-11-13-outcome-b-data/cpp-test.log) |
| `/tmp/umvl-cpu/run4 just cpp-test-no-long-double` | `100% tests passed, 0 tests failed out of 136` | [double-only suite](2026-11-13-outcome-b-data/cpp-test-no-long-double.log) |

`F=beta,bnbinom,cmb,dlaplace,dweibull,fatigue,frechet,genbeta,gompertz,hcauchy,huber,invexp,invparalogis,invweibull,llogis,logitnorm,naka,paralogis,pareto3,power,sgomp,sn,st,trgamma,weibull`; `B=dev/research/evaluations/2026-11-12-fit-snapshot-data`.

Changes are uncommitted. Shared helpers, numerical code, existing tests, dossiers and goldens are unchanged.

**Open viewing issue.** The artifact viewer's `bin/show` returned HTTP 403: this worktree is outside its configured readable roots. The CLI already supplies the worktree as its working directory. Display requires registering this worktree with the viewer or adding it to the viewer's `CODEX_MATH_WEB_VIEW_ROOTS`; the report and all evidence remain in this worktree.
