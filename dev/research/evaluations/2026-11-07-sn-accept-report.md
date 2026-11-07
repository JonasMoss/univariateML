# TASK-116.23: sn acceptance certificates

Completed on 2026-11-07. The sn fitter now has quantitative acceptance for every supported hold subset, using the package's exact-arithmetic certificate conditional on computed comparisons and a stated practical floating-point margin. The dossier's certified flag is now “yes”; its note distinguishes conditional concave solves from the existing joint shape-search certificate. All required final validation runs finished green.

The tested base is 3de415f0896527001caeb0d612bcb91b858d644d, with the uncommitted changes identified by the [source checksum manifest](2026-11-07-sn-accept-data/source-sha256.txt). No commits or board edits were made. Changes are confined to sn's fitter, dossier and tests, its common binding, appended JS tests, and this report and evidence directory. Site-check regenerated an unrelated tracked screenshot; its original Git blob was restored afterward. Goldens, density code, special functions, code generators, st and sstd are unchanged.

The [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md) contains the complete acceptance formulas and conditions. The supported subsets are covered as follows:

| Free coefficients | Certificate or correct stop |
|---|---|
| None | Finite original-coordinate likelihood evaluation; arithmetic failure stops numerically. |
| xi, omega, or both, with alpha held | Gaussian curvature minorant gives a global likelihood-gap bound and coordinate-error bounds for the concave conditional fit. Constant-data scale collapse stops as non-existence. |
| alpha, or omega and alpha, with xi held | Signed monotone shape-score bracket, with analytic scale when free. One-sided nonzero residuals select a half-normal limit. All-zero residuals give a shape tie when scale is held, or unbounded scale collapse when free. |
| xi and alpha, with omega held; all three free | Existing finite-interval and two-tail shape bounds, using quantitatively accepted inner fits. Failure to close bounds stops; no capped shape estimate is returned. |

For held finite alpha, write \(v=(\eta,h)\), \(\eta=1/\omega>0\), \(h=\xi/\omega\), in standardized data coordinates. Concavity of log-Phi and log-eta gives the global quadratic upper tangent with Gaussian curvature
\(G=\sum_i(x_i,-1)^{\mathsf T}(x_i,-1)\).
For nonconstant data, G is positive definite. If g is the total likelihood gradient, maximizing that upper tangent proves
\(\ell(v_*)-\ell(v)\leq g^{\mathsf T}G^{-1}g/2\).
Gradient monotonicity and Cauchy–Schwarz also prove
\(|(v_*-v)_j|\leq\sqrt{(g^{\mathsf T}G^{-1}g)(G^{-1})_{jj}}\).
The held-location and held-scale restrictions use the corresponding scalar minorants, respectively the residual sum of squares and n. These are optimization curvature bounds, not Fisher-information estimates.

Acceptance requires a total conditional likelihood gap at most \(5\cdot10^{-9}n\), relative scale error at most \(5\cdot10^{-7}\), and standardized location error at most \(5\cdot10^{-7}(1+|h/\eta|)\). The dossier gives the inverse-coordinate transport bounds, including the requirement that the inverse-scale error be smaller than eta. A small score, small objective gain, loose user tolerance or exhausted line search cannot bypass these checks.

With xi held, beta = alpha/omega separates scale and shape. For residuals of both signs the shape score is strictly decreasing through a unique zero. A signed bracket [L,R] therefore encloses that zero. At its midpoint, integration of the monotone score bounds loss by
\((R-L)\max(S(L),-S(R))/2\).
The implementation checks this against \(5\cdot10^{-9}n\), and checks the reconstructed alpha bracket width against \(10^{-9}(1+|\alpha|)\). This is a quantitative root certificate, rather than a bisection-width check in unrelated coordinates.

The outer finite-interval and tail inequalities are unchanged. Both tails must now explicitly satisfy their bounds before the search can succeed; the old fixed cutoff guard cannot substitute for closure. Solver caps remain min(iterlim,100000), and subdivision uses the standard 128 times that budget. The existing exact-arithmetic outer allowance is \(1.5\cdot10^{-8}n\), with a practical floating-point allowance of \(2\cdot10^{-8}n\). These are conditional numerical certificates, not outward-rounded interval arithmetic.

Original-coordinate checks protect the certificate's returned values and diagnostics:

- Standardization overflow, invalid reconstructed domains, or lost residual signs stop numerically. For example, observations (1,2,1e308) with xi = 0 can round the first two standardized residuals to zero. Treating them as endpoints would wrongly subtract two log(2) factors from the limiting likelihood. The fitter now rejects that loss of signs.
- Finite fits and faces must agree with an original-coordinate likelihood evaluation within \(10^{-9}n\). Returned likelihoods and validated diagnostic witnesses use original-coordinate evaluations. This also detects cancellation amplified by large held shapes.
- Held face locations and scales are retained exactly. Free face endpoints use the original sample minimum or maximum. Budget and exhausted-root witnesses carry likelihoods evaluated at their reported coefficients.
- The common binding reports numerical, budget, limit, non-existence and tie stops consistently to R and JS. Numerical failures never become successful estimates. Unclosed global upper bounds remain infinite or unavailable.

A counterexample to unconditional uniqueness is five observations all equal to 3, holding xi = 3 and omega = 2 while estimating alpha. Every finite alpha has the same likelihood, since Phi(alpha times zero) = 1/2. The fitter returns a tie with distinct alpha = 0 and alpha = 1 witnesses and equal global likelihood bounds, rather than choosing zero arbitrarily.

A rejected stopping shortcut is retained as a dated decision from this job. Returning immediately when Newton's predicted gain fell below likelihood rounding, while marking the point uncertified, broke the existing held-location alpha = -1000 case. The [failure excerpt](2026-11-07-sn-accept-data/rejected-shortcut.txt) identifies the case. The final solver continues until quantitative acceptance succeeds or its work limit is exhausted. This preserves accurate extreme conditional fits at a measurable computational cost.

Final validation used GCC 15.2.0, R 4.6.1 and BH 1.90.0-1 on the workstation and simbox; Emscripten 6.0.13 and Node 22.22.1 on the workstation; and Python 3.14.5 with mpmath 1.3.0 for the independent conditional check. Builds and computations used the supplied CPU reservations and nice priority, with BLAS/OpenMP pinned to one thread. The JS build used four local slots. JS tests and site-check ran sequentially under one reserved core with CPU affinity and GOMAXPROCS=1.

| Final command/check | Machine | Result |
|---|---|---|
| just cpp-test | simbox | 106/106 development shards passed |
| UMVL_TEST_TIER=full just cpp-test | simbox | 129/129 full shards passed |
| just cpp-test-no-long-double | simbox | 129/129 shards passed with the Boost no-long-double-math proxy macro |
| just test | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21008 |
| just test-full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43828 |
| Focused native sn binary | workstation | 26/26 selected cases, 4353 assertions passed; two unrelated main-file cases filtered |
| just js-build | workstation | Passed |
| just js-test | workstation | 442/442 passed; no failures, cancellations or skips |
| just site-check | workstation | Passed, including all 99 family pages, MathML, generated C++ snippets and browser checks |
| Independent 60-digit conditional roots | workstation | All 21 cases passed |

The final C++ and R summaries are retained in [cpp-final.log](2026-11-07-sn-accept-data/cpp-final.log) and [r-final.log](2026-11-07-sn-accept-data/r-final.log). Workstation results are in [js-site-final.log](2026-11-07-sn-accept-data/js-site-final.log), [site-check-final.log](2026-11-07-sn-accept-data/site-check-final.log) and [cpp-targeted-local.log](2026-11-07-sn-accept-data/cpp-targeted-local.log). The long local slot wait preceded compilation and is excluded from the fitter timings.

The independent [conditional checker](2026-11-07-sn-accept-data/check_conditional.py), [C++ driver](2026-11-07-sn-accept-data/conditional.cpp), [raw fits](2026-11-07-sn-accept-data/conditional.txt) and [results](2026-11-07-sn-accept-data/conditional-check.json) cover seven held shapes from -1000 through 1000 and three conditional coordinate patterns on a fixed eight-point sample. The largest excess over a computed enclosure was \(7.33\cdot10^{-14}\), below the check's \(2\cdot10^{-12}\) rounding allowance. This is numerical implementation evidence, not an interval proof.

Timing and golden comparison used the unchanged 500-point sn golden sample, holding coefficients at its golden estimates. The [benchmark script](2026-11-07-sn-accept-data/benchmark.py) builds the base and final fitter with C++17 and -O2, then alternates execution order over five trials per version and hold subset. [Raw trials and medians](2026-11-07-sn-accept-data/benchmark.json) include both process CPU and wall time. Process CPU medians below exclude compilation and queue time.

| Held coefficients | Before CPU ms | After CPU ms |
|---|---:|---:|
| None | 193.568 | 386.674 |
| xi | 1.454 | 2.505 |
| omega | 299.699 | 307.809 |
| xi, omega | 1.447 | 2.501 |
| alpha | 0.136 | 0.192 |
| xi, alpha | 0.032 | 0.093 |
| omega, alpha | 0.130 | 0.186 |
| All | 0.025 | 0.053 |

All eight hold masks returned successful fits within golden tolerance. Default all-free coefficients were unchanged in this comparison. The largest coefficient change was \(8.6875\cdot10^{-10}\); the largest total likelihood change was \(1.024\cdot10^{-12}\). No golden was regenerated.

The all-free CPU cost approximately doubled on this workload; holding scale added about 2.7%. Additional acceptance work, original-coordinate validation and continuing Newton steps beyond the rejected rounding shortcut trade speed for certified acceptance. These timings cover the stated golden workload, not large-sample throughput.

Open mathematical scope: uniqueness of the joint fit with location and shape both free remains unproved. The outer global likelihood certificate does not require uniqueness, and it does not enclose every joint maximizer in parameter space. Conditional uniqueness is proved under the stated nondegeneracy conditions; the flat-shape example explicitly identifies an exception. No implementation item from this job remains open.

Open display item: the required artifact-pane display was attempted with the provided show command. The viewer returned HTTP 403 because this worktree is absent from its configured readable roots. Display requires opening a viewer-recognized thread in this worktree or adding this worktree to CODEX_MATH_WEB_VIEW_ROOTS. No viewer configuration or files outside the assigned worktree were changed. The completed report remains at the requested path.
