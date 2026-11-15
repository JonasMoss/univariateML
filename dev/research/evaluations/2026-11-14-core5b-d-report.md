# Core5b-d: family-owned outcomes and strict-layer repairs

Completed on 2026-11-15 in `core5b-d`, starting from `da2174a11af7d31c47434a54a65d4108c6b00388`. All required final commands passed on the workstation. No commits, Git writes, pushes, package installs, board edits, or other worktree edits were made.

The ten assigned families, all sixteen modifier routes, and the five censored kernels now carry the decisions described below in their core results and `outcome()` mappings. Adapter classifiers and post-success floating-point guards were removed where they made those decisions independently. Existing diagnostics were retained except where their explanation was wrong. Host argument shape/type checks remain binding responsibilities.

Exactly **44 scoped strict-known entries were removed**: twelve under TASK-214.15 and thirty-two under TASK-214.17. There were no scoped TASK-214.16 or TASK-214.18 JSON entries to remove. The relevant TASK-214.18 rendering defects were repaired in the core mappings and covered by appended tests. Every other original known entry is preserved byte-for-byte as a JSON value; no new entry was introduced. The list shrank from 318 to 274 entries. [Exact removed and retained entries](2026-11-14-core5b-d-data/strict-delta.json) and the [preservation check](2026-11-14-core5b-d-data/preserve-known.py) record the scope.

| Family | Core decisions and outcome changes | Strict entries removed |
|---|---|---:|
| `pois` | Proper held-rate likelihood overflow is core evaluation failure. ZIP carries all-zero and identification facts; ZMPois distinguishes a held parent from an unidentified free parent on all-zero data. ZTPois/ZMPois evaluate the positive conditional likelihood at the returned rate, including inherited face/candidate likelihoods. Fully held windows validate the parent, conditioning event, support and evaluation in a family header. Positive-model arithmetic failure becomes failure/evaluation rather than input or convergence failure. | 12 |
| `binom` | Base, truncated, inflated and modified kernels carry hold-dependent identification. Singleton windows and positive-conditioned size-one laws expose their flat directions. Proper finite models whose point likelihood cannot be evaluated carry an evaluation flag, including ZIB proposal failures; this replaces convergence/input diagnoses and adapter overrides. Empty TBinom and raw count rejection are core input failures. Fully held conditional laws distinguish supported parent atoms from undefined conditioning. | 10 |
| `nbinom` | Base, ZINB, ZTNB and ZMNB carry hold-dependent all-zero and positive point-law identification. Child identification propagates through the hurdle. Successful result validation and arithmetic failure gates reside in the core/certificate path; adapters read the returned outcome. | 6 |
| `geom` | ZTGeom/ZMGeom carry finite likelihood failures and all-zero identification. The count-window wrapper names proved endpoint point masses and the finite-window geometric uniform limit. A numerical bracket cap alone cannot establish a limit. Raw real-to-count preparation and estimated count-window bounds are core-owned. | 4 |
| `zeta` | The base core and Zipf kernel already supplied their terminal mathematical decisions; redundant adapter floating-result guards were removed. Raw modifier count validation now reaches a core input result, replacing pre-core argument rejection. No estimator change. | 2 |
| `lgser` | The likelihood normalizer uses returned theta. Held-parent arithmetic failure is decided by the core. All-zero hurdle identification depends on whether theta is held. If optional mean-residual replay exhausts its budget after a successful finite primary fit, that primary fit is retained as uncertified instead of being discarded into a budget/numerical stop. | 4 |
| `norm` | Base decisions remain core-owned. Estimated truncation-window collapse is distinguished from supplied invalid equal bounds, including either or both parent coefficients held. The fully held generic route uses the same family validator instead of bypassing it. Proper-window density evaluation failure is evaluation failure. | 0 |
| `exp` | Core successful-result validation replaces the adapter check. The censored result maps all-censored and unbounded cases by mathematical meaning; uninformative right-censoring bounds give a proper flat likelihood rather than an invalid-data stop. | 0 |
| `gamma` | Input/control validation and constant-data degeneracy are core decisions. Legacy `IterationFailure` carries a separate evaluation flag when likelihood evaluation at proper returned coefficients fails: convergence failure becomes evaluation failure in that case. Successful likelihoods use the returned binary64 shape and scale. | 4 |
| `laplace` | Unboundedness requires a free scale and zero residual sum. A held positive scale on constant data gives a proper estimate. The likelihood uses the actual residual penalty at the returned scale, replacing the ideal profile penalty. | 2 |

The sixteen checked routes are TBinom, TNorm, TPois, Zipf, ZIB, ZINB, ZIP, ZMBinom, ZMGeom, ZMLogarithmic, ZMNB, ZMPois, ZTBinom, ZTGeom, ZTNB and ZTPois. Generic count-window routing also uses the geometric/NB core wrapper. New fully held Poisson and binomial validators are in `pois/held_window.hpp` and `binom/held_window.hpp`; the normal equivalent is in `norm/truncation.hpp`. The binding seam renders their returned outcomes.

For compatibility with existing private C++ tests, TNorm's raw `InvalidBounds` and one Pareto raw `InvalidInput` result are retained. Core-owned cause flags distinguish estimated-window collapse and unboundedness after clamping below-support censoring bounds. Their `outcome()` overloads return nonexistence/unbounded. Adapters perform no data-based reclassification. Existing test bodies were preserved.

The censored mappings apply to exponential, Weibull, Rayleigh, Pareto and Lomax:

| Core result | Previous rendering problem | Current outcome |
|---|---|---|
| `AllCensored` with an unattained supremum | Plain numerical failure | Nonexistence/unattained |
| `Unbounded` | Plain numerical failure | Nonexistence/unbounded |
| Valid entirely uninformative censoring | Invalid-data/all-censored rejection | Estimate, with identification determined by holds |
| Unsupported censor/hold design | No new feature added | Failure/unsupported |
| Actual numerical evaluation failure | Numerical failure | Failure/evaluation |

Right-censoring below a positive family's support contributes survival one. Exact observations below support remain invalid. Exposure transformations omit these uninformative thresholds and propagate identification. With held Weibull scale, entirely censored data can have an interior shape maximum; the kernel now searches the strictly monotone score. A proved point-mass escape supplies its point and limit likelihood. An unattained boundary whose weak point limit does not reproduce the censoring likelihood carries no misleading point-law payload.

The mathematical claims have the following evidence.

* **Proof, conditional on computed comparisons.** On all-zero binomial data the likelihood is `(1-p)^N`; on all-zero NB data it is `p^r`. Boundary probabilities and zero size reveal the free flat coordinates. An inflated size-one binomial observes only `(1-pi)*p`; a hurdle with no positive observations observes no free parent coordinate. A conditional distribution on a singleton window is constant in its free parent parameters. These identities justify identification flags, rather than a numerical Hessian check.
* **Proof.** For a proper normal parent, continuity and positivity give conditional density at a constant sample asymptotic to the reciprocal window width as an estimated endpoint collapses. Holding either parent parameter does not remove this escape. Supplied equal endpoints remain an invalid design.
* **Proof and numerical regression.** On a finite integer window, the geometric conditional mean increases to the discrete-uniform mean as `p` decreases to zero. A sample mean at or above that value selects the uniform supremum; lower-endpoint constant data select a point-law boundary when it is not already attained. The wrapper checks these analytic conditions rather than treating a finite search coordinate cap as proof. Appended C++ window cases exercise the mappings.
* **Proof and arithmetic regression.** Compatible interior Poisson/binomial conditional masses and proper normal conditional densities are positive. A failed evaluation there is numerical, not invalid data. Supported parent atoms give log likelihood zero. For example, binomial size `1e308`, probability `.9` and observation zero has log probability below `-2e308`, outside finite binary64 range, although the model and observation are valid. Core, binding and R regressions distinguish this evaluation failure from optimizer failure.
* **Proof and independent root comparisons.** For held Weibull scale `s`, write `z=log(t/s)` for positive right-censoring times. With no exact events, the log likelihood is `-sum(exp(k*z))`; its shape score is strictly decreasing unless every `z` is zero. An interior maximum exists when the first log moment is negative and some time exceeds `s`. The times `(s/4,2s)` give the exact shape `1/3`. Times strictly below `s` give the point-mass limit at `s`, with log likelihood zero. A time equal to `s` retains survival `exp(-1)` along that sequence, so that boundary cannot be rendered using the point mass's survival at its atom.

The Weibull score uses `expm1` near zero. Two nonunit ratios use an error-free binary product comparison for the first-moment sign; general sums use compensation and a practical `1024*epsilon` margin. A finite checked root whose certificate cannot close is returned uncertified. A failed search still fails. These are exact-arithmetic arguments conditional on computed comparisons and stated floating margins, not outward-rounded interval certificates.

[Independent likelihood evaluation](2026-11-14-core5b-d-data/reference.py) uses 120-digit arithmetic at the exact returned binary64 coefficients for twelve final snapshot cases: Gamma concentration/offset, subnormal Laplace, large-count logarithmic and positive/hurdle Poisson, in both precision modes. Maximum absolute likelihood error after repair is `1.46e-13`. This supports point-likelihood correctness; it does not independently certify every fitted coefficient.

[Supplementary roots and likelihoods](2026-11-14-core5b-d-data/supplementary-reference.py) add ten comparisons across both modes. They cover the retained low-budget logarithmic primary fit, below-support Pareto censoring, analytic Weibull shape `1/3`, and the thin Weibull root for binary64 times `1.5` and `2/3`. Their product is exactly `1-2^-54`; the independently computed shape is approximately `1.68827528860548257e-16`. Maximum relative coefficient error across these ten checks is `1.43e-13`; the thin-root error is `3.23e-15`. The logarithmic fallback is explicitly uncertified. [Numerical results](2026-11-14-core5b-d-data/supplementary-reference.json) and [snapshot likelihood results](2026-11-14-core5b-d-data/reference.json) retain the values. C++/R/WASM regressions are identified as regressions, not additional independent proofs.

All four required final snapshots and baseline diffs completed. Their source fingerprints agree. **Every returned coefficient payload is unchanged**, including named limits and candidate coefficients; golden files are unchanged. Likelihood payloads, identification flags, core provenance and input classifications account for all differences.

| Snapshot | Fits | Changed rows |
|---|---:|---:|
| Native families | 752 | 102 |
| Double-only families | 752 | 99 |
| Native modifier routes | 1,934 | 264 |
| Double-only modifier routes | 1,934 | 264 |
| Total | 5,372 | 729 |

| Difference category | Rows | Reason |
|---|---:|---|
| Core provenance | 352 | Fully held windows now return a core status; numerical answers and outcomes agree. |
| Returned-law likelihood roundoff | 229 | Likelihood evaluation follows the returned coefficients; repaired positive-Poisson values propagate to their faces. |
| Proved nonidentification | 74 | Identification depends on holds and the observed conditional law. |
| Core input classification | 40 | Raw count/empty-window rejection moves from pre-core argument rejection to core invalid input. |
| Strict likelihood repair | 12 | High-precision comparisons support the repaired point likelihoods. |
| Held-parent identification | 12 | A held hurdle parent gives core `Ok` and removes the inappropriate AllZero warning. |
| Conditional-Poisson face likelihood | 10 | Candidate/limit likelihood and bound payloads inherit the positive-Poisson repair. |

[Every changed case and its reason](2026-11-14-core5b-d-data/snapshot-cases.tsv), [old/new records](2026-11-14-core5b-d-data/snapshot-differences.jsonl.gz), [per-mode category counts](2026-11-14-core5b-d-data/snapshot-counts.json), and [the inventory script](2026-11-14-core5b-d-data/classify.py) make the classification reproducible. The four compressed final snapshots and their provenance files are in the data directory. New censored and held-window boundary cases beyond the frozen fixture are exercised by appended C++/R tests, the [native/double metadata probe](2026-11-14-core5b-d-data/late-metadata.cpp), and the [public WASM smoke check](2026-11-14-core5b-d-data/binding-smoke.mjs).

All final runs used the workstation, with slot wrappers, one BLAS/OpenMP thread per worker and the wrapper's nice priority. Simbox was not tried. The numerical modes are workstation GCC 15 native and `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`; JS uses the rebuilt WASM backend.

| Final command | Result |
|---|---|
| Four `fit-snapshot.py` runs and four baseline diffs | Complete; 729 explained differences, zero coefficient drift |
| Judge scripts with `--prune`, followed by scope preservation | 44 scoped entries removed; no new entries |
| `just strict` on both family/route pairs | Pass; no failure outside known list |
| `just cpp-test` | 110/110 |
| `just cpp-test-no-long-double` | 154/154 |
| `just test` | FAIL 0, WARN 0, SKIP 157, PASS 36,279 |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 65,418 |
| `just js-build` | Pass |
| `just js-test` | 583/583 |
| `just site-check` | Pass |
| Additional WASM public-interface checks | 15/15 |
| Metadata probe, native and double | Pass |
| Scope audit and `git diff --check` | Pass |

The full R tier's single skip is the existing missing Zipf reference. The final seam check covers 9,694 fits per mode: native 7,331 successful comparisons and 2,363 terminal stops; double 7,329 successful comparisons and 2,365 terminal stops. There are no mismatches or remaining guard opens. Three native cases and one double case used the standard 300-second replay guard. The preserved out-of-scope EMG guard entry is now listed as passing, rather than being removed by this job.

Final suite logs use the `verified-` prefix. [Suite status](2026-11-14-core5b-d-data/verified-suite-status.txt), [strict log](2026-11-14-core5b-d-data/strict.log), [C++ log](2026-11-14-core5b-d-data/verified-cpp-test.log), [double C++ log](2026-11-14-core5b-d-data/verified-cpp-test-no-long-double.log.gz), [R dev log](2026-11-14-core5b-d-data/verified-test.log.gz), [R full log](2026-11-14-core5b-d-data/verified-test-full.log.gz), [JS log](2026-11-14-core5b-d-data/verified-js-test.log), and [site log](2026-11-14-core5b-d-data/verified-site-check.log) record the terminal results. Earlier iteration logs are retained separately. One preliminary final C++ build found an invalid compound doctest expression in an appended test; that assertion was rewritten without changing its condition, then the entire seven-suite final sequence ran green. The final sequence includes all later shared-seam changes.

Reproduction entry points are [run-snapshots-and-strict.sh](2026-11-14-core5b-d-data/run-snapshots-and-strict.sh) and [run-suites.sh](2026-11-14-core5b-d-data/run-suites.sh), run from the worktree root. The [supplementary](2026-11-14-core5b-d-data/supplementary.sh) and [metadata](2026-11-14-core5b-d-data/late-metadata.sh) compilation scripts run under a single outer `/tmp/umvl-cpu/run` slot. The [archive check](2026-11-14-core5b-d-data/archive.py) verifies matching source fingerprints and the exact allowlist delta. Evidence files over 200 kB are gzip-compressed.

The remaining open items have explicit scope and missing information:

* **Existing accuracy/precision work, retained.** Ten scoped TASK-214.5 entries remain: TBinom nesting case `007325` in both modes; Gamma refinement cases `001977` and `001991` in both modes; NB refinement cases `009668` and `010128` in both modes. All out-of-scope TASK-214.5/TASK-214.13 entries are preserved. The strict cache-only refinement stage also reports ten missing cached TPois route references; this report makes no new coefficient-accuracy claim for them.
* **Shared truncation failure detail.** The protected `core/truncation.hpp` uses one `NumericalFailure` status for failed mass evaluation, failed bracketing, an iteration cap and enumeration work exhaustion. The family wrapper conservatively reports evaluation failure; it cannot reconstruct a unique budget/convergence cause or comparison margin from that result. Distinct cause and margin fields in the shared result are needed for a finer mapping. No capped coordinate is accepted as a proved limit.
* **Representation of continua of maxima.** The existing shared outcome can return a representative with `unique=false`, but cannot encode an infinite maximizing set. The algebraic sets are described above/in dossiers; a shared symbolic-set payload would be needed to expose them in full through every binding.
* **Shared helper cleanup.** The corrected logarithmic replay fallback is localized to the public family kernel because `core/zero_modified.hpp` was protected. Folding the same rule into that shared helper is a future deduplication change; the assigned public route already uses the corrected kernel.

The [scope audit](2026-11-14-core5b-d-data/scope-audit.json) verifies unchanged HEAD, unchanged protected paths, unchanged dossier front matter and append-only existing C++/R tests. No open item remains among the scoped TASK-214.15–18 strict-known entries.
