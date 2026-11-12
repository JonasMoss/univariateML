# TASK-196: high-precision references for the extreme-point oracle gaps

Baseline: `7825e912b3e5177254853cc020d52b662faeeaa8`, branch `codex/oracle-gaps`.
Completed on the workstation on 2026-11-12. No commits were made.

## Result and scope

All 127 in-scope entries in the supplied gap manifest now have independent reference rows. This comprises **124 newly generated parameter sets** and three entries already covered at baseline: two Frechet points and one inverse-Weibull point. Each of those three baseline points has 21 value rows and 44 quantile rows; parameter-list membership alone was not treated as evidence.

The manifest contains **131 entries in 61 families**. Leaving the four `gengamma` entries untouched, as instructed, gives 127 entries in 60 families. Its original description of 60 families differs from the manifest's actual grouping. The only remaining missing references are the four explicitly out-of-scope `gengamma` entries.

Generation added **9,400 value rows, 5,456 quantile rows, and 124 normalization records**. These counts include independently computed rows retained in exclusions. All historical numerical payloads, exclusions, normalization records, and sampler selectors remain unchanged. The preservation checks cover all 101 stored tables; fingerprints were refreshed for the 100 tables outside gengamma. Every byte of `gengamma/truth.json` remains identical to baseline, including its intentionally unrefreshed shared-source fingerprint.

There are still **22 production defect points in 14 families**. Their references are settled; their public evaluations are not counted as passes. Eight complete parameter members are archived after explicit failing CDF probes. Another 99 value/quantile rows at 14 parameter points are explicit row exclusions. No tolerance was relaxed, no independent value was replaced with production output, and no historical row was removed. Passing rows at those latter points remain active.

The final required C++/R/site suites and the additional JavaScript consumer suite are green. No fitter, golden, shared C++ core/special-function file, binding source, or code generator was changed.

Exact point-level accounting: [summary.json](2026-11-11-oracle-gaps-data/summary.json), [accounting.json](2026-11-11-oracle-gaps-data/accounting.json), [diagnoses.json](2026-11-11-oracle-gaps-data/diagnoses.json).

## Reference failures and remedies

The original diagnostic is retained for every requested point, together with its route, working precision, resource limit, and attempt history. Timeouts alone were not interpreted as mathematical failures. The routes use mpmath and mathematical density definitions; they never call C++ or R for reference numbers. The final environment was Python 3.14.5, mpmath 1.3.0, and SymPy 1.14.0.

Working precision was at least 160 decimal digits; selected difficult finite-support computations used 600. The isolated per-point time budget was raised from 120 to 600 seconds where necessary, retaining the 1 GiB process address-space limit. Workstation generation used a `run4` reservation with at most four isolated worker processes. Single-threaded evidence and replays used `run`; BLAS/OpenMP were pinned to one thread and the wrappers applied `nice -n 10`. No reference computation completed on simbox: it was unavailable.

| Families | Failure mechanism and replacement route |
|---|---|
| `beta`, `betapr`, `fpareto`, `gb2` | Slow hypergeometric evaluation at simultaneous large beta shapes; concentrated normalization. Euler beta continued fraction, centered logit normalization, contracting bracketed inversion. |
| `binom`, `zib`, `zmbinom`, `ztbinom`, `cmb` | Finite-support enumeration grows with the very large design size. Incomplete-beta binomial CDF and exact binomial normalization; cmb uses its nu=1 submodel. |
| `bnbinom` | The near-Poisson beta-mixture remainder did not close. Positive PMF ratio, factorial-moment Markov bound, and bounded far-count gamma ratios. |
| `burr`, `invburr`, `paralogis`, `invparalogis`, `lomax`, `pareto2`, `pareto4`, `invpareto` | Physical-coordinate quadrature does not resolve the transformed mass or enormous support range. Exponential cumulative-hazard coordinate, exact inverse, and explicit omitted exponential mass. |
| `gamma`, `invgamma`, `trgamma`, `invtrgamma`, `naka` | Concentrated or endpoint gamma mass, slow small-argument upper-gamma evaluation, and stalled inverse brackets. Scaled latent gamma coordinate, centered gamma tails, bounded small/large argument expansions, contracting inversion. |
| `ged`, `sged` | Extreme powers and gamma arguments defeat direct evaluation; concentration and inverse bracket stagnation. Explicit mp log density, exact gamma normalization, scaled physical chart, bounded gamma tails and safeguarded inversion. |
| `ht`, `lst`, `std`, `sstd` | Normalization concentrates on a small location-scale range; large-degree beta evaluation is slow. Complete Student beta normalization, stable incomplete-beta tails and density-width verification. |
| `geom`, `zmgeom`, `ztgeom`, `nbinom`, `zinb`, `zmnbinom`, `ztnbinom`, `pois`, `zip`, `zmpois`, `ztpois` | Finite normalization truncation or a slow special-function tail at a very large or diffuse count scale. Exact generating-series normalization; stable geometric, beta or gamma tails and conditional/mixture identities. |
| `dnorm`, `dlaplace` | Finite normalization truncation cannot cover the extreme spread. Exact telescoping normal bins or geometric-series normalization, with independent PMF differences. |
| `lgser`, `zmlogarithmic` | Slow normalization and repeated survival quadrature near p=1. Exact logarithmic-series normalization; positive finite prefix and enveloping Stieltjes tail expansion. |
| `pareto`, `power` | Endpoint concentration defeats physical-coordinate normalization. Exact power-law inverse and exponential-coordinate normalization. |
| `frechet`, `invweibull` | The listed gap is already referenced at the baseline commit. Retain the baseline TASK-187 reference and all numerical rows; no new point counted. |
| `cmp` | A slow diffuse normalizing series or a large Poisson submodel exhausts the reference budget. Exact nu=1 Poisson submodel; unit-rate Euler--Maclaurin tail with integrated Cauchy remainder bound. |
| `dgamma` | Concentration defeats finite normalization and interval subtraction. Telescoping normalization and centered positive gamma-density integration over each bin. |
| `dweibull` | Exponentiating astronomically negative log masses exhausts time/memory. Log-domain PMF-difference verification and negligible natural-probability guard in jump checks. |
| `lgamma` | Endpoint or large-shape gamma evaluation and inverse support concentration. Explicit log density and complete-gamma normalization; bounded gamma tails and contracting inverse brackets. |
| `nhyper` | Position-support enumeration is enormous although the marked population is small. Finite hypergeometric count over the m marked items, using m+1 positive terms. |
| `pig` | Large-order normalization and a thin count-tail integration layer exceed the reference budget. Exact Poisson-mixture normalization, scaled positive tail integral, Taylor/tail bounds, checked Gauss quadrature. |
| `skellam` | Large-rate positive convolution repeats concentrated gamma evaluations; far-count log differences lose increments. Poisson adjacent-cut recurrence, positive convolution bounds and a sharper descending Bessel-mass majorant. |
| `sn` | Gaussian submodel inversion and normalization checks lose the required extreme precision. Exact shape-zero/plus-or-minus-one normal identities; stable squared-normal tails and high-precision inverse. |
| `st` | A thin skew-t angular layer and large-degree beta evaluation exhaust the reference budget. Endpoint-scaled angular wedge, concavity remainder, Student reflection normalization and stable beta tails. |
| `tlambda` | The finite-difference verification step is too large for sigma=1e-8. Scale the existing implicit-density derivative check by the local density width. |
| `tnorm` | Far truncation creates a boundary layer and unstable subtraction of normalizing log probabilities. Stable log-normal interval density and boundary-scaled normalizing integral. |
| `yulesimon` | Polynomial survival with tiny rho makes truncated normalization impractical. Exact telescoping normalization, retaining independent mass-difference checks. |

The formulas, truncation arguments, and distinctions between proof and numerical evidence are recorded in [reference-identities.md](2026-11-11-oracle-gaps-data/reference-identities.md). In particular:

- Gamma and skew-t quadratures scale the narrow integration layer to unit width. Concavity gives explicit bounds for omitted tails. Their quadrature error estimates are numerical evidence, rather than interval enclosures.
- The CMP Euler--Maclaurin route uses an integrated Cauchy derivative bound and an explicit omitted-integral bound; it requires their combined relative remainder below `1e-45`.
- The logarithmic-series Stieltjes expansion has an enveloping next-term remainder. Positive count sums use decreasing-ratio geometric majorants, or factorial-moment Markov bounds in the beta-negative-binomial case.
- Incomplete beta uses an exact continued-fraction identity, convergence to 100 decimal digits, and independent comparisons. This convergence check is not claimed as an outward-rounded interval proof.
- Exact generating-series, complete-gamma/beta, transformation, and telescoping identities replace impossible physical-coordinate normalization sums. They are identified as analytic normalization evidence; they are not mislabeled as quadrature passes.
- Inverse searches retain a checked bracket and force contraction. Numerical verification uses the local density width, and astronomically small PMF differences stay in logarithmic arithmetic.

Three reference errors exposed by the consumers were repaired before accepting the new rows: the reciprocal-Burr endpoint exponent is `a*b-1`; distant truncated-normal intervals must retain their physical displacement before subtracting the mean; and positive truncation must preserve a tiny lower CDF rather than subtracting two rounded-one survival probabilities. Only new rows were regenerated for those corrections. Baseline rows and selectors were retained.

Reproducible generation and mathematical checks: [generate.py](2026-11-11-oracle-gaps-data/generate.py), [repair_reference_rows.py](2026-11-11-oracle-gaps-data/repair_reference_rows.py), [test_references.py](2026-11-11-oracle-gaps-data/test_references.py), [test_log_domain.py](2026-11-11-oracle-gaps-data/test_log_domain.py). Attempt receipts are retained under `2026-11-11-oracle-gaps-data/generation-pass1/` through `generation-pass4/` and `generation/`.

## Per-family reference accounting

“Closed” counts requested manifest entries now backed by rows, including the three pre-existing entries marked below. “Production open” counts distinct points with measured public-evaluator failures; it does not indicate a missing oracle.

| Family | Listed gaps before | Closed | Remaining references and reason | Production open |
|---|---:|---:|---|---:|
| `beta` | 1 | 1 | 0 | 1 |
| `betapr` | 1 | 1 | 0 | 1 |
| `binom` | 1 | 1 | 0 | 0 |
| `bnbinom` | 1 | 1 | 0 | 1 |
| `burr` | 4 | 4 | 0 | 0 |
| `cmb` | 1 | 1 | 0 | 0 |
| `cmp` | 2 | 2 | 0 | 1 |
| `dgamma` | 1 | 1 | 0 | 0 |
| `dlaplace` | 1 | 1 | 0 | 0 |
| `dnorm` | 1 | 1 | 0 | 0 |
| `dweibull` | 1 | 1 | 0 | 0 |
| `fpareto` | 1 | 1 | 0 | 1 |
| `frechet` | 2 | 2 (already present) | 0 | 0 |
| `gamma` | 4 | 4 | 0 | 0 |
| `gb2` | 1 | 1 | 0 | 1 |
| `ged` | 4 | 4 | 0 | 2 |
| `gengamma` | 4 | 0 | 4: concurrent ownership; untouched | 0 |
| `geom` | 1 | 1 | 0 | 0 |
| `ht` | 4 | 4 | 0 | 0 |
| `invburr` | 3 | 3 | 0 | 0 |
| `invgamma` | 4 | 4 | 0 | 0 |
| `invparalogis` | 2 | 2 | 0 | 0 |
| `invpareto` | 1 | 1 | 0 | 0 |
| `invtrgamma` | 6 | 6 | 0 | 0 |
| `invweibull` | 1 | 1 (already present) | 0 | 0 |
| `lgamma` | 3 | 3 | 0 | 0 |
| `lgser` | 1 | 1 | 0 | 1 |
| `lomax` | 1 | 1 | 0 | 0 |
| `lst` | 3 | 3 | 0 | 0 |
| `naka` | 3 | 3 | 0 | 0 |
| `nbinom` | 2 | 2 | 0 | 0 |
| `nhyper` | 1 | 1 | 0 | 0 |
| `paralogis` | 2 | 2 | 0 | 0 |
| `pareto` | 1 | 1 | 0 | 0 |
| `pareto2` | 1 | 1 | 0 | 0 |
| `pareto4` | 2 | 2 | 0 | 0 |
| `pig` | 4 | 4 | 0 | 2 |
| `pois` | 1 | 1 | 0 | 0 |
| `power` | 1 | 1 | 0 | 0 |
| `sged` | 6 | 6 | 0 | 4 |
| `skellam` | 2 | 2 | 0 | 2 |
| `sn` | 3 | 3 | 0 | 0 |
| `sstd` | 5 | 5 | 0 | 2 |
| `st` | 7 | 7 | 0 | 2 |
| `std` | 3 | 3 | 0 | 0 |
| `tlambda` | 1 | 1 | 0 | 0 |
| `tnorm` | 3 | 3 | 0 | 0 |
| `trgamma` | 6 | 6 | 0 | 0 |
| `yulesimon` | 1 | 1 | 0 | 0 |
| `zib` | 1 | 1 | 0 | 0 |
| `zinb` | 2 | 2 | 0 | 0 |
| `zip` | 1 | 1 | 0 | 0 |
| `zmbinom` | 1 | 1 | 0 | 0 |
| `zmgeom` | 1 | 1 | 0 | 0 |
| `zmlogarithmic` | 1 | 1 | 0 | 1 |
| `zmnbinom` | 2 | 2 | 0 | 0 |
| `zmpois` | 1 | 1 | 0 | 0 |
| `ztbinom` | 1 | 1 | 0 | 0 |
| `ztgeom` | 1 | 1 | 0 | 0 |
| `ztnbinom` | 2 | 2 | 0 | 0 |
| `ztpois` | 1 | 1 | 0 | 0 |
| **Total** | **131** | **127** | **4: gengamma only** | **22** |

The untouched `gengamma` vectors, in its dossier parameter order, are `[-1,1,0]`, `[1,1,0]`, `[0,1e-8,0]`, and `[0,1e8,0]`. Each original process timed out at 120 seconds. This job makes no claim to have resolved those four references.

## Local production repairs

The added independent rows exposed local numerical defects. The following repairs have appended C++ regressions and family-specific dossier notes:

- **Pareto IV:** preserve the physical shifted ratio in the density.
- **Negative binomial:** use exact geometric survival at size one.
- **ZIP and ZINB:** retain the signed offset around a rounded half probability when inverting the half-inflated law, with probability validation before delegation.
- **Zero-conditioned count laws:** preserve the positive-component lower CDF with stable subtraction, use stable Poisson mass evaluation, and handle the exact half-zero-probability inverse. The family-private `ztpois/dpqr_core.hpp` supplies the affected truncated/modified Poisson and negative-binomial consumers.
- **CMP:** delegate its exact `nu=1` Poisson submodel.
- **PIG:** evaluate the zero atom directly instead of summing its heavy opposite tail.
- **Yule--Simon:** retain the small-shape log-gamma increment. The omitted cubic term is bounded by `zeta(3)*a^3/3`; over the enlarged branch its effect is below the consumer tolerance.
- **Truncated normal:** use the existing mode-centered integral/inverse when a remote one-sided boundary loses physical offsets under standardization. Retain the normal-ratio route for distant log tails where the centered integral underflows.
- **Negative hypergeometric:** for at most 32 marked items, use exact finite marked-position products instead of cancellation-prone large-population log-gamma differences.
- **Poisson:** use `F(k;lambda)=Q_Gamma(k+1;lambda)` in the existing physical gamma coordinate when `k+1` is exact and at least 16.
- **Discrete gamma:** use the protected unit-gamma CDF at both bin endpoints; this removes the error amplified by subtracting generic incomplete-gamma tails. All six affected value rows were restored after 11,431 comparisons passed in each precision mode.
- **Transformed and inverse transformed gamma:** at unit power, delegate to the parent gamma/inverse-gamma physical-coordinate CDF.

These changes affect evaluation/inversion, not fitting searches or reported fitter outcomes. No fallback was converted into a new stop, and no default fit or golden was changed.

## Remaining production defects

Errors below are measured against the settled independent rows. Log density and log-CDF errors use `abs(actual-reference)/max(1,abs(reference))`; natural CDF errors are relative. Quantiles use relative error with the existing oracle bracket, lattice, and near-zero floor rules. Their tolerance remains `1e-9`; the density/CDF tolerance remains `1e-10`. A quantile error near zero should therefore not be read as a large absolute displacement.

“Both” means workstation native and `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` builds. Complete-member probes deliberately establish an unresolved CDF, rather than claiming to remeasure every archived API call.

| Family | Points | Functions / mode | Measured discrepancy | Missing work |
|---|---:|---|---|---|
| `beta` | 1 | p / both | max finite error 1.37708e-08 | A uniform large-shape incomplete-beta tail with a verified relative-error remainder is missing. |
| `betapr` | 1 | p / both | max finite error 1.37331e-08 | The inherited large-shape beta tail needs a verified relative-error remainder. |
| `bnbinom` | 1 | p / both | NaN | Near-Poisson beta-mixture evaluation needs a cancellation-free mass and a closed mixture remainder. |
| `cmp` | 1 | p / both | NaN | Diffuse factorial-power normalization and count tails need a protected Euler--Maclaurin evaluator. |
| `fpareto` | 1 | p / both | max finite error 1.37229e-08 | The inherited large-shape beta tail needs a verified relative-error remainder. |
| `gb2` | 1 | p / both | max finite error 1.37229e-08 | The inherited large-shape beta tail needs a verified relative-error remainder. |
| `ged` | 2 | d,p / both | NaN | Compensated physical standardization and a propagated gamma-power sensitivity bound are missing; the precision gate returns NaN. |
| `lgser` | 1 | p / both | NaN | Near-unit logarithmic-series survival exceeds the geometric-series guard; a uniform bounded tail evaluator is missing. |
| `pig` | 2 | p / both | NaN | Positive heavy count tails exceed the 10000000-cell remainder guard; a bounded tail integral is missing. |
| `sged` | 4 | d,p,q / both | NaN; max finite error 22.1362 | The skew moment/physical-coordinate transform and inverse need compensated evaluation and a propagated gamma-power sensitivity bound. |
| `skellam` | 2 | d,p / double only | max finite error 1.54656e-08 | Large-rate log-mass cancellation and the difference-of-Poisson tail need a protected expansion with an error bound. |
| `sstd` | 2 | p / both | NaN | The extreme skew moment transform exceeds the public natural-tail precision gate; a protected coordinate evaluation is missing. |
| `st` | 2 | p / both | NaN | The joint large-skew/large-degree angular tail is not resolved by the public evaluator. |
| `zmlogarithmic` | 1 | p / both | NaN | Near-unit logarithmic-series survival exceeds the geometric-series guard; a uniform bounded tail evaluator is missing. |

Exact parameter vectors, input selectors, computed values, errors and declarations are in [row-exclusions.json](2026-11-11-oracle-gaps-data/row-exclusions.json) and [open-point-final-probes.json](2026-11-11-oracle-gaps-data/open-point-final-probes.json). [classify_rows.py](2026-11-11-oracle-gaps-data/classify_rows.py) proves the numerical row multiset unchanged while appending exclusions; [archive_open_points.py](2026-11-11-oracle-gaps-data/archive_open_points.py) performs the corresponding whole-member archival.

The GED/SGED failures are not fixed by removing their precision checks: doing so would accept uncontrolled physical-coordinate error amplified by a huge power. They need compensated standardization and a propagated sensitivity bound. The large-beta tail, large-rate Skellam kernel, diffuse CMP normalization, heavy PIG count tail, and joint skew-t limit require numerical work beyond a local algebraic substitution. Those are explicit open production items, with their independent references now available.

## Validation and provenance

The earlier all-row measurement recorded **383,105 scalar comparisons and 383,741 API calls per precision mode**, including **318 large-count lattice quantile checks** per mode. It found 247 native and 295 double-mode failing scalars. The late discrete-gamma reformulation closes all eleven of its double-mode failures and restores six value rows. The remaining union contains 99 excluded rows; the eight complete-member exclusions remain separately explicit.

A fresh measurement with the final binaries and final active tables now checks **382,648 scalar comparisons and 383,284 API calls per mode**, including the same **318 lattice checks** per mode, with **zero failures**. All **125,182 active value/quantile rows** are covered. The excluded witnesses were also freshly replayed with those exact binaries: 255 native and 292 double-mode scalar witnesses still fail, including the sixteen probes for the eight complete members. These counts are diagnostic witnesses, not passes and not an exhaustive replay of every archived API call.

Canonical row manifests and binary hashes bind both measurements to their inputs. The current `final-native` and `final-double` receipts identify the fresh final measurements; earlier subset-classification receipts remain execution history rather than claims of a separate replay. JavaScript's full pre-late-fix suite reports **380,306 truth checks with zero failures**; its discrete-gamma consumer is rerun after the late fix and WebAssembly rebuild.

The full suite runs below are green. Following the late family-local discrete-gamma fix, only its native/double doctest shards, full-tier R family/shared loops, and JavaScript family tests were repeated. Site validation was repeated after updating that family's dossier and rebuilding WebAssembly. Shared C++ core/special functions and bindings were unchanged, so the full C++/R suites were not repeated, as instructed.

| Final run | Machine | Result |
|---|---|---|
| `just cpp-test` | workstation, GCC 15.2 | 108/108 shards; exit 0 |
| `just test` | workstation, R 4.6.1 | PASS 25,536; FAIL 0; WARN 0; SKIP 154; exit 0 |
| `just test-full` | workstation, R 4.6.1 | PASS 54,054; FAIL 0; WARN 0; SKIP 0; exit 0 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 135/135 shards; exit 0 |
| `just site-check` | workstation | build, browser, numerical, MathML, and site checks pass; exit 0 |
| `just js-test` | workstation, four Node workers | 615/615; FAIL 0; SKIP 0; exit 0 |
| Applicable Python mathematical/harness checks | workstation, one slot | 67 tests pass |
| Late discrete-gamma C++ checks | workstation | native and no-long-double: 1/1 targeted shards each; exit 0 |
| Late discrete-gamma R checks | workstation | full tier, family/shared filter: PASS 464; FAIL 0; WARN 0; 6 empty filtered tests; exit 0 |
| Late discrete-gamma site / JavaScript checks | workstation | rebuilt site passes; JS 4/4 and 11,450 truth checks, zero failures; exit 0 |
| Focused local-fix checks | workstation | 42 checks across the two precision modes; zero failures |
| Historical payload / selectors / fingerprint checks | workstation | 101 tables preserved; 100 current fingerprints; gengamma unchanged |

Simbox was tried separately for C++ dev, R dev, R full, and no-long-double C++; all four attempts returned 75 before a suite ran. Each required suite then completed locally through `run4`. No GCC 13 result is claimed. Site preparation also built WebAssembly on the workstation; no dependencies were installed. All started build/test commands were waited for to completion. The full C++/R suites ran once; the late family-local fix used the targeted reruns documented above.

Receipts and replay inputs: [validation-receipts.json](2026-11-11-oracle-gaps-data/validation-receipts.json), [receipts.txt](2026-11-11-oracle-gaps-data/receipts.txt), [js-truth-summary.json](2026-11-11-oracle-gaps-data/js-truth-summary.json), [measured-closure-native-summary.json](2026-11-11-oracle-gaps-data/measured-closure-native-summary.json), [measured-closure-double-summary.json](2026-11-11-oracle-gaps-data/measured-closure-double-summary.json), [measured-recovered-native-summary.json](2026-11-11-oracle-gaps-data/measured-recovered-native-summary.json), [measured-recovered-double-summary.json](2026-11-11-oracle-gaps-data/measured-recovered-double-summary.json), [dgamma-js-truth-summary.json](2026-11-11-oracle-gaps-data/dgamma-js-truth-summary.json), [final-native-summary.json](2026-11-11-oracle-gaps-data/final-native-summary.json), [final-double-summary.json](2026-11-11-oracle-gaps-data/final-double-summary.json), [fixed-smoke.json](2026-11-11-oracle-gaps-data/fixed-smoke.json), [fixed-dgamma-native-summary.json](2026-11-11-oracle-gaps-data/fixed-dgamma-native-summary.json), [fixed-dgamma-double-summary.json](2026-11-11-oracle-gaps-data/fixed-dgamma-double-summary.json), [dgamma-closed-exclusions.json](2026-11-11-oracle-gaps-data/dgamma-closed-exclusions.json), [fingerprints.json](2026-11-11-oracle-gaps-data/fingerprints.json), and [preservation.json](2026-11-11-oracle-gaps-data/preservation.json).

### Inherited Python harness assertions

The unfiltered Python harness has four stale assertions outside this job's numerical changes. A separate checkout reconstructed from the frozen baseline reproduces all four: the old grid-size assertion expects 9 points instead of 17; two catalogue-count assertions expect 98 instead of 101; and the CMB workload test calls a removed `truth_workload_exclusion` hook. They were not rewritten or presented as passing. The 67-test applicable gate excludes exactly these separately baseline-replayed assertions. Independent preservation and fingerprint checks cover the actual current tables; a global fingerprint rewrite of `gengamma` would violate its ownership instruction.

Their exact names, tracebacks and baseline replay command are retained in [python-baseline.log](2026-11-11-oracle-gaps-data/python-baseline.log), [baseline_python.sh](2026-11-11-oracle-gaps-data/baseline_python.sh), [python_gate.py](2026-11-11-oracle-gaps-data/python_gate.py), and [python-gate.log](2026-11-11-oracle-gaps-data/python-gate.log). These are open harness-maintenance items, not missing references or new production stops.

## Reproduction

Run commands from this worktree with `TMPDIR`, `JUST_TEMPDIR`, and `XDG_RUNTIME_DIR` under `build/tmp`. The baseline is a commit identifier, not an archived source tree in the evaluation data. The scripts reconstruct baseline inputs using `git show` or a temporary `git archive` under `build/scratch`.

- Reference generation: `TRUTH_EXTREME_TIMEOUT=600 TRUTH_EXTREME_WORKERS=4 /tmp/umvl-cpu/run4 python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/generate.py`; recorded attempt directories preserve the staged history.
- Consumer binaries: `bash dev/research/evaluations/2026-11-11-oracle-gaps-data/build_consumers.sh fixed`; all-row measurements: `bash dev/research/evaluations/2026-11-11-oracle-gaps-data/measure.sh measured-closure`. Run the measurement against the pre-archival added rows to reproduce the recorded failures; the final declarations intentionally keep them outside active checks.
- Required suite routing: `bash dev/research/evaluations/2026-11-11-oracle-gaps-data/validate.sh`; the separate final JavaScript consumer is `/tmp/umvl-cpu/run4 just js-test`.
- Preservation/membership: `bash dev/research/evaluations/2026-11-11-oracle-gaps-data/verify_final_tables.sh`; mathematical gate: `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-11-oracle-gaps-data/python_gate.py`.

Regeneration on the final source retains exclusion declarations and their independently computed rows. The recorded generation fingerprints precede the final exclusion declarations and are retained as execution provenance; `fingerprints.json` and the current table fingerprints identify the final state. No existing table, golden, or test expectation needs to be replaced to inspect the evidence.

Merge note (Claude): the per-row measurement dumps (`measured-*-rows.json.gz`, about 2 MB each) and the interim failure lists were removed at merge to keep the repository small; the summaries, final failure lists and receipts remain, and `measure.sh` regenerates the dumps.
