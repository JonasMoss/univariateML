# TASK-116.57: generalized beta all-free certificate

The review changes are complete. With scale held above every observation, the fitter certifies finite, moving-power and gamma-face winners when their global checks close. An unfinished optional certificate retains the successful legacy finite fit or proper selected-limit payload, explicitly uncertified. It stops when the legacy search fails or a validated comparison disproves its proposal. The family remains **fit.certified: partial**.

The seeded experiment certified **126/220 fits (57.3%)**: 52 finite, 52 gamma faces and 22 power faces. The other 94 retained uncertified legacy results. All 220 status decisions were unchanged; there were no numerical failures, budget refusals or rejected legacy proposals in this experiment. Every final local suite finished green. Simbox's final wrapper attempt returned 75, so the additional current-source GCC 13 check remains unavailable; the workstation fallback passed.

Work is uncommitted on codex/genbeta-allfree, based on 2ed5514b. Master was not merged or rebased. Goldens, d/p/q/r, truth tables, special-function code, code generators and shared core headers are unchanged. No package installs, board edits or sub-agents were used.

## Implementation and public behavior

The original C++, R and JavaScript test-file prefixes are byte-identical to HEAD. This restores the two C++ tests “genbeta: exact gamma and effective-endpoint power face comparisons” and “genbeta: structured face payloads” unchanged. All added cases follow the original tests.

The all-free return path in [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp) now preserves a successful legacy result when extra verification is unresolved or exhausted. The C++ result records allfree_certified=false and its certificate diagnostic; optional exhaustion does not set a public budget-stop flag. Unfinished global comparisons have an infinite global likelihood upper bound.

For a finite success, [the binding-neutral fitter](../../../bindings/common/distributions/genbeta/fit.hpp) exposes numeric certified=1 or 0. JavaScript receives that property directly. The [genbeta R adapter](../../../bindings/r/distributions/genbeta/adapter.cpp) carries the same value as a coefficient attribute, which the existing generated wrapper preserves. No mathematical routing occurs only in R.

An unclosed gamma proposal remains UnitGammaBoundary with its original proper shape/scale/upper payload and the advice mllgamma(scale / x). Its binding message explicitly says “uncertified global comparison.” Power proposals follow the same policy. A completed face certificate provides finite global bounds and removes that qualification. Validated improving witnesses still invalidate a contradicted legacy proposal; those numerical stops retain both candidates and clear budget/tie flags.

The default-budget (3,2,.7) midpoint-quantile fixture now returns finite **Ok, uncertified**, rather than a budget condition. Its legacy coefficients remain

    (p,q,a) = (5.3645780153723184, 1.9924355221946439, .41118368148382761)
    log likelihood = 38.823408377416484

Its certificate diagnostic is Budget after the 38400-work allowance; its public budget-stop flag is false. The refreshed [three-fixture output](2026-11-09-genbeta-allfree-data/finite-fixtures.txt) and [checker](2026-11-09-genbeta-allfree-data/finite-fixtures.cpp) verify this behavior. The other two fixtures remain certified finite fits.

## Gamma-face mathematics

The literal first-order power term cancels for **every** nonconstant sample, not only a special seam. The implemented certificate therefore uses the first term in squared power.

Write Y=log(b/x), v=Y/mean(Y), h=a mean(Y), with mean(v)=1 in exact arithmetic. Put lambda=p h and let G be the unrestricted fitted gamma log likelihood in these normalized coordinates. The expansion is

    F(h,q,lambda) = g(q,lambda) + h b(q,lambda) + O(h^2)
    b(q,lambda) = (q-1)/2 * (q/lambda-1).

At the gamma maximum lambda=q, b is zero: it is a multiple of the gamma rate score. The information matrix and the resulting first nuisance motion are

    J = [[trigamma(k), -1/k], [-1/k, 1/k]]
    (q',lambda') = (0, -(k-1)/2).

After this information adjustment, the profiled coefficient is

    P(h) = G + c2 h^2 + O(h^4)
    c2 = (k-1) * (mean(v^2) - (k+1)/k) / 24,

where k is the fitted gamma shape. Thus s=h^2 has a signed first-order coefficient c2. Using an imperfect rate fit to obtain a nonzero linear coefficient in h would instead measure rate-fit error.

The new [gamma_face_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/gamma_face_certificate.hpp) supplies an explicit uniform fourth-order bound, rather than relying on this local expansion alone. The complete derivation is in [review-gamma-expansion.md](2026-11-09-genbeta-allfree-data/review-gamma-expansion.md).

Centering nu=h*(p+(q-1)/2) gives an exact gamma-plus-remainder decomposition. Integral bounds for psi(z+1/2)-log(z) and its derivative, together with global upper/lower bounds for log(sinh(z/2)/(z/2)), isolate the signed quadratic term and an explicit fourth-order remainder. Existing uniform small-power shape/rate compactness includes all conditional beta maxima. Gamma curvature first bounds their motion by O(h); centered correction-score bounds then contract both nuisance coordinates by O(h^2), uniformly for every 0<h<=H.

Lipschitz bounds on the quadratic coefficient, absorbed against the negative gamma likelihood gaps, yield computed constants satisfying

    P(h) <= G + c2Upper h^2 + C4 h^4,     0<h<=H.

A strictly negative, padded c2Upper+C4 H^2 proves the whole neighborhood inferior to gamma. The fitter independently checks gamma coefficient accuracy, excludes the large-power tail, and covers every compact cell between the two neighborhoods. Only completing all these checks sets allfree_certified=true. The compact cover reuses the conditional beta rectangles and endpoint-score bounds, plus the moving-power certificate's q-eliminated envelope where beta shapes become enormous. It assumes neither global unimodality nor uniqueness.

A validated feasible point too close to the fixed padded gamma comparison target also detects when this particular fixed-margin cover cannot close: every upper bound on a cell containing that point is at least its feasible lower bound. The fitter then retains the legacy face uncertified instead of spending the ledger on an impossible comparison. A point above the gamma upper bound takes precedence and disproves the proposal.

These are **exact-arithmetic certificates conditional on the computed comparisons, with stated practical floating-point margins**. Gamma statistics, signs, coefficient/remainder and cell comparisons use allowances of 1024, 4096 or 8192 machine epsilons with the operands specified in the proof. No outward rounding or universal libm error bound is claimed. Solver loops use min(iterlim,100000); the certificate ledger is 128 times that limit, including legacy proposal work, profile evaluations and popped cells.

The accepted finite/power proofs and their uniform escape bounds remain in [proof.md](2026-11-09-genbeta-allfree-data/proof.md). The [family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) describes the resulting partial certification and fallback policy.

## Numerical evidence and frozen accuracy

The independent 90-digit [expansion oracle](2026-11-09-genbeta-allfree-data/check-gamma-expansion.py) checks four information adjustments and profile expansions at three decreasing powers. The [signed-bound oracle](2026-11-09-genbeta-allfree-data/check-gamma-bounds.py) checks four coefficient signs and eight conditional-profile points per width, including both nuisance-coordinate enclosures. Native and double-width outputs passed: [native](2026-11-09-genbeta-allfree-data/gamma-bounds.json), [double](2026-11-09-genbeta-allfree-data/gamma-bounds-double.json). These checks sample the proved inequalities; they do not replace the uniform proofs.

[Deterministic global gamma probes](2026-11-09-genbeta-allfree-data/gamma-global-fixtures.json) close three certificates at the default budget. A fourth retains its legacy gamma payload uncertified on exhaustion. The C++ regression at the quadratic-zero k=1 seam verifies that rounding cannot turn zero into a signed certificate. R and JavaScript both exercise a completed global gamma certificate and proper legacy face payloads.

The [70-digit golden oracle](2026-11-09-genbeta-allfree-data/golden-oracle.json), using the exact binary64 observations, gives:

| quantity | independent value |
|---|---:|
| p | 0.23474506144344839954270551384496 |
| q | 0.24298710467411032763286075225138 |
| a | 4.08499599320924346179787347052199 |
| log likelihood | 548.75168869752326129362258812539 |
| gamma-face log likelihood | 537.27953687601642067561414351299 |
| moving-power-face log likelihood | 57.30669491039657043407740660998 |

This stationary calculation alone is not a globality proof; the finite coverage certificate supplies globality. The frozen returned coefficients and likelihood are numerically unchanged. Their maximum relative discrepancy from the oracle is 3.8502579899902494e-9, within the actual family default coefficient tolerance 1e-7 and golden regression tolerance 1e-5. The golden file is byte-identical to HEAD; no out-of-tolerance discrepancy was suppressed or edited.

Existing independent finite/power evidence remains applicable to the unchanged helpers: [conditional/tail oracle](2026-11-09-genbeta-allfree-data/check-oracle.py), [cell oracle](2026-11-09-genbeta-allfree-data/check-cells.py), and [power-envelope oracle](2026-11-09-genbeta-allfree-data/check-power.py), with their saved native and double-width JSON outputs.

## Seeded coverage across shapes and sample sizes

The [paired study](2026-11-09-genbeta-allfree-data/seeded-study.py), [C++ experiment](2026-11-09-genbeta-allfree-data/seeded-study.cpp), [complete results](2026-11-09-genbeta-allfree-data/seeded-study.json) and [44-cell summary](2026-11-09-genbeta-allfree-data/seeded-cell-summary.csv) retain the inputs, hashes, compiler, build flags and timings.

There are 180 genbeta samples: p in {.3,1,3}, q in {.3,2,5}, a in {.7,3}, n in {50,300}, five seeds per cell. Another 40 gamma-like samples use Y~Gamma(k,scale=1/k), X=exp(-Y), k in {.5,1,2,5}, the same n values and five seeds. The seed rule is mt19937_64(1165700+seed), seed=0..4, with libstdc++ gamma_distribution; rounded support endpoints are redrawn. All samples are generated before fitting, and paired sample hashes match. Repeated seed identifiers couple cells; these are empirical closure fractions on this grid, not population probabilities.

| sample group | count | certified finite | certified gamma | certified power | uncertified legacy |
|---|---:|---:|---:|---:|---:|
| genbeta, n=50 | 90 | 21 (23.3%) | 18 (20.0%) | 14 (15.6%) | 37 (41.1%) |
| genbeta, n=300 | 90 | 25 (27.8%) | 16 (17.8%) | 0 (0.0%) | 49 (54.4%) |
| gamma-like, n=50 | 20 | 3 (15.0%) | 9 (45.0%) | 7 (35.0%) | 1 (5.0%) |
| gamma-like, n=300 | 20 | 3 (15.0%) | 9 (45.0%) | 1 (5.0%) | 7 (35.0%) |
| all | 220 | 52 (23.6%) | 52 (23.6%) | 22 (10.0%) | 94 (42.7%) |

Percentages are within rows and rounded. Of the legacy decisions, 52/142 finite fits, 52/54 gamma proposals and 22/24 power proposals were certified. There were no failures or status changes. All 94 uncertified results retain their legacy status and likelihood; all 90 uncertified finite coefficient vectors are exactly unchanged.

Twenty fully certified finite fits required refining the legacy coefficients. The largest relative change was 7.826463073156075e-7, with likelihood differences around 1e-12. Their independent winning boxes justify the returned coefficient accuracy. This does not change the frozen default fit or any stored golden.

For the detailed tables below each entry is **F/G/P/U**, counts out of five seeds: certified finite / certified gamma / certified power / uncertified legacy.

| generating (p,q,a) | n=50: F/G/P/U | n=300: F/G/P/U |
|---|---:|---:|
| (0.3, 0.3, 0.7) | 5/0/0/0 | 4/0/0/1 |
| (0.3, 0.3, 3) | 5/0/0/0 | 4/0/0/1 |
| (0.3, 2, 0.7) | 0/1/4/0 | 2/1/0/2 |
| (0.3, 2, 3) | 0/1/4/0 | 2/1/0/2 |
| (0.3, 5, 0.7) | 0/2/0/3 | 0/0/0/5 |
| (0.3, 5, 3) | 0/2/0/3 | 0/0/0/5 |
| (1, 0.3, 0.7) | 2/2/0/1 | 4/1/0/0 |
| (1, 0.3, 3) | 1/2/0/2 | 4/1/0/0 |
| (1, 2, 0.7) | 1/1/2/1 | 1/0/0/4 |
| (1, 2, 3) | 1/1/2/1 | 1/0/0/4 |
| (1, 5, 0.7) | 0/0/0/5 | 0/0/0/5 |
| (1, 5, 3) | 0/0/0/5 | 0/0/0/5 |
| (3, 0.3, 0.7) | 3/2/0/0 | 0/3/0/2 |
| (3, 0.3, 3) | 3/2/0/0 | 0/3/0/2 |
| (3, 2, 0.7) | 0/1/1/3 | 2/1/0/2 |
| (3, 2, 3) | 0/1/1/3 | 1/1/0/3 |
| (3, 5, 0.7) | 0/0/0/5 | 0/2/0/3 |
| (3, 5, 3) | 0/0/0/5 | 0/2/0/3 |

| gamma-like k | n=50: F/G/P/U | n=300: F/G/P/U |
|---|---:|---:|
| 0.5 | 2/3/0/0 | 1/4/0/0 |
| 1 | 1/2/2/0 | 2/0/1/2 |
| 2 | 0/1/4/0 | 0/2/0/3 |
| 5 | 0/3/1/1 | 0/3/0/2 |

## Timings

Workstation GCC 15.2, C++17 -O2, four reserved single-thread fit workers; compilation and CPU-queue delays are excluded from the seeded fit times. They are descriptive shared-machine measurements. Certification is substantially more expensive than the legacy search.

| sample group | legacy mean ms | current mean s | current median s | current maximum s |
|---|---:|---:|---:|---:|
| genbeta, n=50 | 1.464 | 5.289 | 0.660 | 117.918 |
| genbeta, n=300 | 3.072 | 11.475 | 0.811 | 134.139 |
| gamma-like, n=50 | 1.334 | 11.554 | 1.370 | 80.448 |
| gamma-like, n=300 | 3.133 | 20.280 | 3.193 | 90.641 |
| all 220 | 2.262 | 9.752 | 1.121 | 134.139 |

The legacy median was 2.132 ms. The four-worker experiment took 0.134 s for legacy fitting and 556.150 s for current fitting, excluding compilation. Summed per-fit wall times were 0.498 s and 2145.428 s respectively.

Separate repeated one-thread measurements in [timings.json](2026-11-09-genbeta-allfree-data/timings.json) and [power-timings.json](2026-11-09-genbeta-allfree-data/power-timings.json) give:

| fit | repeats | before ms/fit | after ms/fit | before/after work |
|---|---:|---:|---:|---:|
| frozen n=500, all shapes free | 10 | 13.531 | 205.396 | 262 / 749 |
| n=300 moving-power quantile fixture | 3 | 2.299 | 1065.593 | 241 / 5167 |

Both retain their former coefficients and likelihood. Held-q timing rows at .01, .1, .3, .7 and 2 retain their former statuses, coefficients and work counts; the shared inner-score arithmetic is unchanged.

## Remaining certification limits

**Gamma moment seams.** Where the quadratic coefficient is zero, the implemented positive fourth-order upper bound cannot decide a winning gamma face. A signed profiled quartic coefficient, including nuisance motion, and a uniform sixth-order remainder are still needed. The known positive-quartic k=1 example is globally excluded by the moving-power witness; it does not settle winning-gamma seams outside the excluded q≈1 strip. Those proposals remain uncertified under the requested policy.

**Finite signs and compact coverage.** The 94 unclosed study cases comprise 44 ShapeSigns, 39 RootSigns, three Arithmetic and eight Budget diagnostics. The eight budget cases are four finite, two gamma and two power proposals. More work can help budget-only cases; it does not by itself resolve precision-limited signs. Tighter correlated/centered conditional equations and score transport are needed there. In the (3,2,.7) quantile fixture, the unsigned tail excludes h<=.00213023, but loose rectangular inclusions consume the default ledger around h=.116 before isolating a maximum; the large cutoff is about 190.154. Its retained finite point is a legacy answer, not a claimed proved MLE.

**Uniqueness.** With q=1 held, only p a is identified. For Y={1/6,1/2}, every positive factorization p a=3 is a maximum, for example (p,a)=(1,3),(3,1),(6,.5). This is an exact counterexample to uniqueness in that supported held-q subset. It is not an all-free global maximum: the moving-endpoint power face strictly improves on it. No general all-free uniqueness theorem is claimed; the finite verifier must compare all retained maxima.

## Final validation and machine limitation

Every main command below finished green, one command at a time in the foreground through /tmp/umvl-cpu/run4. The wrapper supplies reservations, nice level 10 and one-thread BLAS/OpenMP settings. The three-fixture refresh and saved-study postprocessing passed through /tmp/umvl-cpu/run. Temporary files and logs are inside this worktree's build directory.

| command | machine | final result |
|---|---|---|
| just test | workstation | PASS 22183; FAIL 0; WARN 0; SKIP 154 |
| just cpp-test | workstation | 106/106 |
| UMVL_TEST_TIER=full just cpp-test | workstation | 130/130 |
| just cpp-test-no-long-double | workstation | 130/130 |
| just test-full | workstation | PASS 45015; FAIL 0; WARN 0; SKIP 0 |
| just js-build | workstation | exit 0 |
| just js-test | workstation | 559/559 |
| just site-check | workstation | exit 0 |

The workstation native and no-long-double compilers are GCC 15.2; the JavaScript build also exercises Emscripten's double-width mode. The final simbox command was /tmp/umvl-cpu/simbox-run just cpp-test-no-long-double. It returned **75, simbox unreachable**, after earlier observations of an ext4 emergency_ro filesystem. The authorized local fallback passed 130/130. A current-source GCC 13 rerun remains externally blocked until that machine is reachable with a writable filesystem; earlier pre-review results are not presented as validation of the new gamma code.

The fresh [validation record](2026-11-09-genbeta-allfree-data/validation.json) preserves every result, compiler/machine context, source and log hashes, the remote failure and seeded experiment. Source hashes match the snapshot taken before final runs. Original test prefixes, golden bytes and saved baseline source match HEAD; git diff --check passes. The site check's unrelated historical screenshot was restored byte-for-byte. No started validation command remains running.
