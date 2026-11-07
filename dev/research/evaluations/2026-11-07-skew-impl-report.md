# sstd and st fitting certificates — TASK-116.16

Implemented binding-neutral global certificates for the fixed-shape sstd mean-only and sd-only fits, conditional st skewness, and the exactly held unit-skew sstd reduction to the certified all-free std fitter. Both dossiers now declare `fit.certified: 'partial'` and describe the certified subsets. Larger searches and xi-only sstd fitting remain open below. Default all-free sstd/st fitting paths and golden files were preserved.

Tested source: `017b9f8ec3bed7610ccd41c62738bce1ea987316`, branch `codex/skew-impl`, with the uncommitted changes in this worktree. The [source manifest](2026-11-07-skew-impl-data/source-manifest.sha256) fingerprints 685 core, binding, dossier, golden and check-source files; its SHA-256 is `832f7c28c059b280c7ddbf3e5c7549176bc688775b7de1a3be2a16e0614f35b3`. The final source check passed. No commits, pushes, board edits or dependency installations were made.

## Implemented coverage

| Family | Free coefficients | Certificate or explicit outcome |
|---|---|---|
| sstd | mean, with sd/nu/xi held | Exhaustive rational score enclosures, location-tail exclusion and likelihood comparison; ties retain witnesses. Arbitrary positive held xi is supported. |
| sstd | sd, with mean/nu/xi held | Complete inverse-scale score search with whole-half-line tail bounds. Negative collapse exponent is unbounded; symmetric equality has a proved finite unattained supremum. Nonunit-skew equality stops numerically pending its finite/face comparison. |
| sstd | mean/sd/nu, with xi exactly 1 | Delegate to `student_t_mle_certified`, including normal, two-df and collapse faces, budget, tie and numerical stops. Other held std subsets are not inferred to be certified. |
| st | alpha, with xi/omega/nu held | Exact one-sign half-t face, all-zero nonidentifiability tie, or exhaustive score-enclosure search inside the analytic mixed-sign bracket. Held nu may be any positive finite value. |
| both | none | Finite-density evaluation, without an optimizer. |

Every other free-subset/held-value combination retains its existing numerical route and has no new global certificate. All routing is in C++ and the binding-neutral family headers; no R-only implementation was introduced. Result flags identify the new certified outcomes. General shared headers and std/lst/sn sources were not edited.

The implementations are in [sstd/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/certificate.hpp), [sstd/scalar_search.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/scalar_search.hpp) and [st/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/st/certificate.hpp). Family README sections are the canonical algorithm descriptions. The [theory report](2026-11-07-skew-theory-report.md), [proofs](2026-11-07-skew-theory-data/proofs.md) and [32-subset inventory](2026-11-07-skew-theory-data/subsets.md) remain the canonical closure and unimplemented-route references.

## Mathematical claim and numerical contract

For sstd, the mean score uses the rational term `z/(c*c+z*z)`. The log inverse-scale score uses `z*(z-shift)/(c*c+z*z)`, with separate denominator widths on the two sides of zero. Endpoints, seams and every possible interior term extremum give score ranges for an entire interval. Mean tails have fixed inward score signs outside the translated observation range. Scale bracketing proves signs on entire exterior half-lines; two finite probe signs alone are insufficient. These reductions avoid forming high-degree floating-point polynomial coefficients and make no unimodality assumption.

For st, the mixed-sign skew bracket excludes both unbounded alpha tails by the Student tail inequalities in proof section 7. Inside it, pdf unimodality and CDF monotonicity bound each positive pdf/CDF ratio. Log Student CDF concavity is not assumed. A one-sign sample has a strictly monotone likelihood. Observations exactly at the held endpoint keep skewing factor one, giving the closed half-t likelihood minus `k*log(2)`. Reflected stop payloads retain the endpoint in the original data coordinates. The limiting distribution coefficients and the reachable pointwise likelihood are therefore recorded separately; a closed half-t endpoint convention alone does not reproduce that likelihood.

The shared subdivision discards an interval only when its padded score range excludes zero. All stationary enclosures left in the compact search are compared. A terminal interval must meet both the coefficient-width condition `2e-11*(1+abs(t))` and an integrated-score remainder no greater than its point-likelihood rounding allowance. Adjacent enclosures are grouped within `1e-9*(1+abs(t))`, preserving their likelihood upper bounds. The mean-value theorem bounds the likelihood throughout each retained box. Dominating bounds select a global maximizing enclosure to the stated numerical resolution; competing separated enclosures stop on tie. No uniqueness assertion is attached to all-free fits or to an unresolved critical component.

These are certificates in exact arithmetic conditional on the computed ranges and comparisons, with a practical allowance of 128 binary64 epsilons times one plus the absolute term sum. They are not outward-rounded IEEE interval proofs. Public-coordinate transport and the represented likelihood are checked. Nonfinite or unresolved arithmetic stops numerically. Exhaustion never returns the last candidate as a fitted estimate.

Certificate work is `128*min(iterlim,100000)` cells in 64-bit counts. Scale bracket expansion has the separate `min(iterlim,100000)` solver-pass cap. A failed bracket is non-convergence, not certificate-budget exhaustion. Budget stops retain finite evaluated witnesses and an infinite unresolved global upper bound. The exactly symmetric scale equality follows from a strictly positive inverse-scale score at every finite scale; it is an unattained supremum, not a fitted zero scale.

### Nonuniqueness and rejected numerical shortcuts

With observations `-2,2`, held sd=1, nu=3 and xi=1, sstd has two global mean maxima at `-sqrt(3),+sqrt(3)`. For st, held omega=1, nu=1 and alpha=0 give the same two location maxima. In each case likelihood maximization minimizes `c^4+2*(D-A*A)*c*c+(D+A*A)^2`, with A=2 and D=1. Its two minima are exact; blanket conditional uniqueness is false. The sstd certificate returns a tie with both witnesses. For st alpha-only fitting, all observations at the held xi give a continuum of maximizers and two distinct finite witnesses are returned in a tie stop.

The six-point, held mean=0, nu=3, xi=10 scale example has competing maximum branches near log(sd)=3.7647 and 5.8948. The certificate selects the latter, with log likelihood about -38.8073025561. The existing finite scan is not a global exclusion argument. Its old implementation remains accessible at the base revision; the nonmonotonicity proof and original data remain linked from the theory report.

Two numerical defects in the initial new implementation were found and repaired before final validation:

* Unscaled stationary-coordinate products overflowed at c=1e155 and shift=8e153 although the extrema were finite. A positive interval was assigned range `[0,0]`, missing the exact decimal-input probe value `-1/626`. Hypot-scaled ratios now recover the extrema, and unresolved radii/coordinates or subnormal widths stop numerically. The [reproducer](2026-11-07-skew-impl-data/range-overflow.py) and [values](2026-11-07-skew-impl-data/range-overflow.json) are retained, with a direct C++ range regression.
* Coefficient width alone did not control a sharp likelihood near nu=2. For observations `-2,2,2.5`, held sd=1, xi=1 and nu=`nextafter(2,+Inf)`, the earlier estimate was only about 9.1e-12 from the reference mean but lost 2.79e-7 in log likelihood. The integrated-score remainder gate resolves this. The final 70-digit comparison has absolute log-likelihood error 7.11e-15. See [before](2026-11-07-skew-impl-data/sharp-peak-before.json), [after](2026-11-07-skew-impl-data/sharp-peak.json) and [checker](2026-11-07-skew-impl-data/sharp-peak.py).

## Validation and timings

Every final command below finished green before this report was written. The five simbox runs, after both numerical repairs, share [remote-final.txt](2026-11-07-skew-impl-data/remote-final.txt); [final-suites.sh](2026-11-07-skew-impl-data/final-suites.sh) records their invocation and exit-75 fallback. No fallback was needed.

| Final run | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox / MeanMachine | 106/106 dev shards; 47.98 s |
| `just test` | simbox / MeanMachine | FAIL 0, WARN 0, SKIP 154, PASS 21011; 62.033 s |
| `UMVL_TEST_TIER=full just cpp-test` | simbox / MeanMachine | 129/129 shards; 60.75 s |
| `just cpp-test-no-long-double` | simbox / MeanMachine | 129/129 shards; 50.20 s |
| `just test-full` | simbox / MeanMachine | FAIL 0, WARN 0, SKIP 0, PASS 43831; 250.516 s |
| Targeted sstd/st doctest binary | Workstation | 43 cases, 13071 assertions, no failures |
| Independent multiprecision checks | Workstation | 19 finite cases at 50 digits, plus the 70-digit near-two peak check |
| `just js-build`; `just js-test` | Workstation | Build succeeded; 438/438 tests, no failures or skips |
| `just site-check` | Workstation | Passed; rendered binding bytes match the fresh Wasm build |

The dev R skips are tier exclusions. Full R and JS have no skips. Compiler warnings from existing LST code and shell startup references to a missing cargo environment remain nonfatal; the R testthat warning counts above are zero. The site check regenerated an unrelated historical screenshot, whose original bytes were restored with read-only `git show`. Final changed source paths are confined to the named families and bindings. Golden diffs and `git diff --check` are clean.

The [independent checker](2026-11-07-skew-impl-data/check.py) uses seed 11616, mpmath 1.3.0 at 50 digits, and tolerances `abs(coefficient error)/(1+abs(reference)) < 2e-8` and absolute log-likelihood error below 2e-10. Observed maxima were 2.502e-11 and 7.105e-15. These are finite numerical comparisons and local score refinements, not an independent proof of global optimality. Global exclusion is supplied by the mathematical bounds above. Appended C++ tests also cover seams, rational extrema, multiple modes, scale equivariance, arithmetic loss, endpoint factors and typed budget/tie outcomes. R tests exercise the binding-neutral stops.

Paired timing uses the same C++17 `-O2` source, ten fits per workload, one workstation slot and the controls tol=1e-6, iterlim=200. Baseline compilation restored the two original mle.hpp files from the base revision in a scratch include tree; all other pre-existing headers were unchanged. The initial pre-edit n=7 measurement is also retained. All six final n=7/n=500 workloads returned finite fits.

| n=500 workload | Before, ms/fit | After, ms/fit |
|---|---:|---:|
| sstd mean only, sd=1, nu=3, xi=2 | 403.324 | 2.529 |
| sstd sd only, mean=0, nu=3, xi=2 | 6.808 | 2.395 |
| st alpha only, xi=0, omega=1, nu=4 | 181.771 | 246.957 |

The st enclosure search costs more CDF evaluations than the earlier local refinements. These are indicative repeated-workload timings, without a statistical speedup claim. [Timing source](2026-11-07-skew-impl-data/timing.cpp), [baseline](2026-11-07-skew-impl-data/timing-baseline-large.txt) and [final](2026-11-07-skew-impl-data/timing-after.txt) retain coefficients, likelihoods and statuses. Local computation used the required slot wrappers, nice priority and one BLAS/OpenMP thread. Heavy workstation phases reserved all four slots. Simbox used its prescribed wrapper with four build/test workers. Lock waits were allowed to finish without interruption.

The [validation record](2026-11-07-skew-impl-data/validation.json), [environment](2026-11-07-skew-impl-data/environment.json), [targeted log](2026-11-07-skew-impl-data/targeted-cpp.txt), [JS build](2026-11-07-skew-impl-data/js-build-final.txt), [JS tests](2026-11-07-skew-impl-data/js-test-final.txt) and [site log](2026-11-07-skew-impl-data/site-check-final.txt) retain the commands and evidence. Workstation versions were GCC 15.2.0, R 4.6.1, Python 3.14.5, Node 22.22.1 and Emscripten 6.0.13. Standalone C++ checks used the preinstalled Boost headers, without R runtime linkage.

## Open implementation entries

1. **Sstd xi-only with both moments held.** The algebraic root/seam search with the positive quadratic square root and correlated endpoint remainders is not implemented. Independent numerator/width intervals lose the thin-side limiting ratio. The retained [prototype](2026-11-07-skew-impl-data/xi-obstruction.py) and [path values](2026-11-07-skew-impl-data/xi-obstruction.json) recover the fixed-nu=3 endpoint multiplier 0.604225941637; using the unpenalized closed half-t endpoint would overstate the reachable likelihood. This evidence does not certify the existing xi-only fitter.
2. **Nonunit-skew sstd scale-collapse equality.** The new route stops numerically with a finite witness. It still needs the finite collapse value, an asymptotically scaled tail enclosure and comparison against every finite branch. Equality alone is not treated as a no-MLE proof. The symmetric equality case is implemented separately by its monotonicity proof.
3. **Remaining sstd subsets.** Fixed-nu multidimensional critical-component enumeration, free-nu searches, constrained half-t/t2/normal face profilers and quantitative joint-face neighborhoods remain unimplemented. The finite free-nu endpoint factor and the held-moment normal-corner factor differ from the fixed-nu factor; they must follow proof section 6. The existing std held-subset limitations are not bypassed by the unit-skew reduction.
4. **Remaining st subsets.** Global nuisance boxes, shape-tail/corner majorants, constrained limit-model profiles and collapse-equality comparisons remain unimplemented. The exact held-alpha=0 LST reduction also still needs its restriction and stop-transport routing; no blanket parent-held-subset certification is claimed. The free-nu restriction nu>=1 is preserved. Sections 8–9 and 14 of the theory proofs specify the required faces and equality reductions.
5. **Remaining guarantees and coverage.** All-free uniqueness is not proved. Multiple roots and small likelihood gaps can exhaust the budget. The practical floating-point allowance has finite calibration and portability checks, not a uniform outward-rounded error proof. No n=100000 cost calibration or useful worst-case runtime guarantee was established. The loop budgets provide explicit stops rather than runtime or universal-success guarantees.

No change to settled dpqr, special-function, truth, code-generation or generated R d/p/q/r sources was needed. The missing work above is certificate and constrained-profiler machinery. No build or test remains running.

Artifact display: the prescribed `show` command returned HTTP 403 because the viewer does not include this worktree among its readable roots. The report remains available at its worktree path; no viewer configuration outside the worktree was changed.
