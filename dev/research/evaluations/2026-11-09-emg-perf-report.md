# TASK-162: faster certified EMG fits

The held-rate joint fit is about 44 times faster at n = 500 and 197 times faster at n = 10000 in the paired process-CPU measurements. Scale-only fits with location and rate held improve by about 17 and 53 times, respectively. A controlled comparison on CPU 0 gives all-free improvements of 1.75 and 1.66 times. All 42 paired fits retain identical printed coefficients and log likelihoods. The eight final suite commands passed.

The implementation tightens the held-rate interval bound and reuses an accepted Newton point's evaluation. The existing global coverage, coefficient enclosures, face comparisons, stop details and work caps remain in force. The family retains fit.certified = 'yes'; its fit.note now names the held-rate precision bound. All seven nonempty free subsets have a certified acceptance path or a guarded stop. There are no new structural hold exceptions.

**Changes and certification scope.** The changes are confined to EMG's [shape bounds](../../../univariateML-cpp/include/cppunivariate/distributions/emg/shape_bounds.hpp), [outer certificate](../../../univariateML-cpp/include/cppunivariate/distributions/emg/certificate.hpp), [root mathematics](../../../univariateML-cpp/include/cppunivariate/distributions/emg/certificate_math.hpp), [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md) and three tests appended to [test.cpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/test.cpp). The shared binding route already calls this C++ implementation, so both R and JS receive the change.

Location-only fitting retains its signed score enclosure. The other six free subsets use the existing outer certificate. The new bound applies to the two subsets with rate held and scale free: {sigma} and {mu,sigma}. The point-evaluation reuse applies within the existing Newton solver.

The [EMG certification report](2026-11-07-emg-cert-report.md) supplies the underlying conditional concavity, escape bounds and coefficient-certificate proofs. A finite acceptance still requires a negative-definite Hessian box, a score-root contraction with q < 1/2 and d + qR < R, physical coefficient enclosures meeting the requested tolerance, and uniform nuisance-root inclusion across the winner interval. Conditional concavity and the Schur complement establish the winner's local profile maximum. Every other compact interval and both tails must be excluded, and compatible faces must be beaten. Separated surviving regions stop as comparison ties; exhausted, numerical and unresolved searches retain witnesses. Taking an additional valid upper bound preserves this argument.

**Proof of the additional bound.** Hold the positive physical rate lambda fixed. Write k = lambda sigma, tau = 1/k², a = lambda mu and d_i = lambda x_i - a. Substitution y = lambda t in the convolution gives

$$
f_i(a,\tau)=\lambda\sqrt{\frac{\tau}{2\pi}}
 \int_0^\infty \exp\{-y-\tau(d_i-y)^2/2\}\,dy.
$$

For fixed a, the second derivative in tau of the logarithm of this integral is the variance of V_i = (d_i-y)²/2 under its tilted positive measure. It is nonnegative. Thus the log likelihood minus n log(tau)/2 is convex in tau for every a. Taking the supremum over the same a domain preserves convexity. That domain is the real line for free location and a singleton for held location; neither depends on tau. Consequently, for the conditional profile P,

$$
C(\tau)=P(\tau)-\frac n2\log\tau
$$

is convex for both held-rate subsets.

For a shape interval [lo,hi], let A = hi^-2, B = lo^-2, and let U_A and U_B be the certified endpoint profile upper bounds. With theta = (tau-A)/(B-A), convexity gives

$$
P(\tau)\leq(1-\theta)U_A+\theta U_B+
 \frac n2\{\log\tau-(1-\theta)\log A-\theta\log B\}.
$$

The logarithmic Jensen gap is maximized at tau/A = (r-1)/log(r), where r = B/A. Its maximum is

$$
J=\log\frac{r-1}{\log r}-1+\frac{\log r}{r-1}.
$$

It follows that max(U_A,U_B) + nJ/2 bounds the entire interval. This proof needs no residual or location envelope and makes no unimodality assumption about the outer profile.

The implementation computes t = log(r) = 2 log(hi/lo), using log1p for the ratio increment. Away from zero it evaluates the equivalent form

$$
J=t+\log(1-e^{-t})-\log t-1+\frac{t e^{-t}}{1-e^{-t}},
$$

with expm1 for 1-exp(-t), so exp(t) is never needed. For t <= 0.01 it uses the conservative curvature bound G = (expm1(t))²/8 in place of J. This follows from the bound on the magnitude of the second derivative of log(tau) over [A,B], and avoids cancellation near r = 1. For larger t, G = J.

The returned upper bound includes the practical floating-point margin

$$
256\epsilon\{n+|\max(U_A,U_B)|+nG/2\}.
$$

Invalid or nonfinite ratio/gap calculations return an unavailable infinite bound. In particular, an overflowing ratio cannot become a zero gap through a NaN comparison. The controller takes the minimum of the new bound and the existing curvature bound. If neither bound is usable, the existing numerical-failure path applies.

This is certification in exact arithmetic, conditional on the computed comparisons, with the stated practical floating-point margin, as required by the package standard. The implementation does not claim outward-rounded interval arithmetic. The analytic inequalities above are proofs; the high-precision checks below are numerical evidence about their implementation.

**Reuse at Newton points.** An accepted line-search candidate already contains the likelihood, score and point Hessian needed by the next validation and Newton step. The solver now retains that evaluation at exactly the same model, data and coordinates. A private validator accepts the evaluated point; the original validator remains as a wrapper that evaluates its argument. Hessian-box construction, root inclusion, likelihood errors, line-search acceptance and coefficient accuracy tests are unchanged. No loop or cap was added.

**Measurement method.** The baseline fitter is revision 2ed5514b32ec72ded95809c834453d24f025f850. The benchmark reconstructs its three changed headers from that revision. Both versions use GCC 15.2.0, C++17, -O2 and the same temporary profiling counters, on the [i7-1355U workstation](2026-11-09-emg-perf-data/workstation-cpu.txt). Measurements run sequentially under one CPU reservation, nice 10, with one BLAS/OpenMP thread.

The 500-point input is the frozen EMG golden sample. The 10000-point input is an independent sample at the golden coefficients, generated with seed 162 and mt19937_64, Box-Muller normals and independent exponential delays. Each held coefficient equals its frozen golden value. Controls are the unchanged defaults, tol = 1e-8 and maxit = 500.

There are three alternating before/after pairs for every subset and sample size: before/after, after/before, before/after. The six global paths call the outer controller directly to expose its ledger and profile cache; these timings exclude public input validation and binding overhead. Location-only uses the public C++ fitter. Data generation and output are outside the timed region.

In the following tables, wall seconds are medians of three runs. Process CPU seconds were added for the final pair and are reported for that pair only. The certificate column is the outer work ledger, which charges profile, bound and winner operations; point evaluations are counted separately below. The raw CSV column has the same name for location-only solver iterations, so that row is explicitly marked inapplicable here.

n = 500:

| Free coefficients | Wall seconds, before → after | CPU seconds, before → after | Certificate work, before → after | Cached profiles, before → after |
|---|---:|---:|---:|---:|
| mu | 0.003137 → 0.003146 | 0.003338 → 0.003674 | N/A | 0 → 0 |
| sigma | 1.498787 → 0.077088 | 1.532179 → 0.090539 | 914 → 132 | 201 → 30 |
| lambda | 0.282709 → 0.051629 | 0.110582 → 0.061171 | 147 → 147 | 33 → 33 |
| mu,sigma | 9.503093 → 0.155380 | 8.184655 → 0.186317 | 1693 → 136 | 357 → 31 |
| mu,lambda | 0.984739 → 0.202105 | 0.424435 → 0.235395 | 231 → 231 | 52 → 52 |
| sigma,lambda | 1.641734 → 0.253836 | 0.562526 → 0.293557 | 229 → 229 | 52 → 52 |
| mu,sigma,lambda | 1.182849 → 0.633530 | 1.107024 → 0.727606 | 554 → 554 | 127 → 127 |

n = 10000:

| Free coefficients | Wall seconds, before → after | CPU seconds, before → after | Certificate work, before → after | Cached profiles, before → after |
|---|---:|---:|---:|---:|
| mu | 0.070025 → 0.069310 | 0.082604 → 0.079691 | N/A | 0 → 0 |
| sigma | 88.488527 → 1.683569 | 88.476087 → 1.683467 | 1474 → 129 | 326 → 29 |
| lambda | 2.082841 → 1.025399 | 2.082544 → 1.206083 | 151 → 151 | 33 → 33 |
| mu,sigma | 768.476650 → 3.893109 | 768.291066 → 3.892753 | 3541 → 134 | 740 → 30 |
| mu,lambda | 9.334770 → 5.273215 | 9.333086 → 5.550254 | 256 → 256 | 57 → 57 |
| sigma,lambda | 11.585966 → 6.070412 | 11.584953 → 6.069947 | 237 → 237 | 53 → 53 |
| mu,sigma,lambda | 24.409716 → 35.476635 | 24.407664 → 35.471482 | 582 → 582 | 132 → 132 |

Location-only uses 31 solver iterations at n = 500 and 32 at n = 10000 in both versions; it has no outer certificate ledger. Its algorithm is unchanged.

The unpinned n = 10000 all-free timing regressed despite reduced evaluation counts. A separate three-pair experiment pins both binaries to the same CPU 0, with the same flags, inputs and controls:

| n | Median CPU seconds, before → after | CPU speedup | Median wall seconds, before → after |
|---:|---:|---:|---:|
| 500 | 1.187991 → 0.677881 | 1.75× | 1.188677 → 0.678429 |
| 10000 | 23.821475 → 14.355462 | 1.66× | 23.838083 → 14.358254 |

These controlled results support the all-free speed improvement. The cause of the unpinned regression was not isolated: the original runs lack per-fit core placement and frequency telemetry. The report retains those measurements rather than attributing them to a proved cause. See the [wall summary](2026-11-09-emg-perf-data/timing-summary.json), [CPU pair](2026-11-09-emg-perf-data/cpu-timing.json), [fixed-core summary](2026-11-09-emg-perf-data/focused-summary.json) and [raw fixed-core runs](2026-11-09-emg-perf-data/focused-runs.json).

**Where the work went.** All profiling counters repeat across the three main pairs. Representative counts are:

| n | Free coefficients | Point evaluations, before → after | Hessian evaluations, before → after | Newton calls, before → after | Maximum outer subdivision depth, before → after |
|---:|---|---:|---:|---:|---:|
| 500 | mu,sigma | 8908 → 151 | 10835 → 261 | 622 → 43 | 18 → 15 |
| 500 | sigma | 1603 → 74 | 1867 → 104 | 311 → 42 | 20 → 15 |
| 500 | all | 1215 → 461 | 1936 → 1182 | 156 → 156 | 23 → 23 |
| 10000 | mu,sigma | 26410 → 170 | 31630 → 272 | 1318 → 41 | 21 → 14 |
| 10000 | sigma | 2519 → 69 | 2953 → 96 | 494 → 40 | 22 → 15 |
| 10000 | all | 1304 → 508 | 2116 → 1320 | 162 → 162 | 23 → 23 |

The old held-rate bound required many more cached profiles, repeated inner solves and subdivisions. For the n = 10000 joint held-rate fit, Newton steps fall from 8437 to 145. The fallback counter is zero before and after for every measured global path: fallback solving was not the bottleneck. For all-free fits, Newton calls and steps remain identical; reuse removes repeated point evaluations while preserving the outer search. The complete counts for all subsets are in the paired profile CSV files, including [before](2026-11-09-emg-perf-data/before-1e-8-2-profile.csv) and [after](2026-11-09-emg-perf-data/after-1e-8-2-profile.csv).

**Accuracy and stops.** The [final evidence check](2026-11-09-emg-perf-data/final-evidence-summary.txt) asserts exact printed coefficient and log-likelihood equality for all 42 pairs, all Ok statuses, repeatable counters and unchanged compiled-source hashes. Frozen golden, density and truth files are unchanged. Maximum relative difference from the existing golden coefficient fixture remains 9.8607401e-8, within its 1e-6 regression tolerance.

Independent 70-digit physical score-root calculations check every free coefficient and its enclosure for all seven golden subsets at tol = 1e-10, in native and double-intermediate modes. Maximum normalized errors are 4.3842240e-11 and 4.3842082e-11. These are numerical accuracy checks; global coverage comes from the certificate proof. See the [native roots](2026-11-09-emg-perf-data/outer-oracle.json), [double roots](2026-11-09-emg-perf-data/outer-oracle-double.json) and [oracle](2026-11-09-emg-perf-data/outer-oracle.py).

An independent 100-digit bisection calculation of the maximum logarithmic gap checks 140 finite native bounds and 136 finite double-mode bounds; four double-mode cases safely report an unavailable infinite bound. Every finite bound exceeds the high-precision maximum; the smallest surplus is approximately 6.24965e-13. This supports the cancellation and overflow handling numerically. See the [gap validation](2026-11-09-emg-perf-data/gap-validation.json) and [checker](2026-11-09-emg-perf-data/validate-gap.py).

The [13 baseline/final stop comparisons](2026-11-09-emg-perf-data/stop-comparison.json) preserve status kinds, budget/tie flags and witness presence. They cover exponential and normal faces, endpoint multiplicity, constant-data unboundedness, unresolved searches, exhausted work, invalid controls and numerical/accuracy failures. The [stop checker](2026-11-09-emg-perf-data/validate-stops.py) records the precise cases. Work limits and acceptance gates have not been relaxed.

**Uniqueness and open mathematics.** The bound proves an interval upper bound, not general outer unimodality. With held mu = 0, sigma = 1 and data {-10,9}, a finite rate-one likelihood beats the normal face N by more than 28, using the Mills lower bound at 11 and Phi(8) > 1/2:

$$
L(1)-N>\log(11/122)+32+\tfrac12\log(2\pi)-\log2>28.
$$

But as lambda tends to infinity,

$$
L-N=(-10+9)/\lambda+O(\lambda^{-2})<0.
$$

There is a finite peak and a normal-boundary local maximum, separated by a dip. Global outer unimodality therefore fails.

There is also an exact global finite/normal tie in the compatible closure for some data {-10,v}, v in (-10,9). At v = -10 the normal face strictly dominates every finite rate, while v = 9 has the finite witness above. The small-rate bound L <= 2 log(lambda) is uniform in v. The large-rate expansion has leading coefficient -10+v <= -1 and a uniformly bounded quadratic remainder on this compact v interval. Hence all finite competitors capable of reaching N lie in a common compact rate interval. Its continuous maximum relative to N changes sign, proving a global closure tie at an intermediate v.

The prior certification report records a 70-digit stationary equality near v = 2.746841848406631, lambda = 1.070870015754127. Those rounded coordinates are numerical evidence, not a rigorous location of the global transition. The counterexample establishes nonuniqueness in the closure. A general finite-interior uniqueness theorem remains open; the executor uses data-specific winner-region certificates and comparison-tie stops.

A generic signed normal-tail domination theorem with arbitrary free nuisance parameters also remains open when no finite witness beats the normal face. Those inputs stop Unresolved with witnesses unless an existing domination proof applies. Neither open theorem is assumed by the new bound or by finite acceptance. The unisolated unpinned timing variation is the remaining performance observation; resolving its cause would require the missing scheduling telemetry or a separate controlled hardware study.

**Final verification.** All commands below completed with exit status zero. Each local suite used its own blocking run4 reservation. Remote suites used simbox-run, with four CMake/CTest workers and one BLAS/OpenMP thread. The driver waited for each command before starting the next. Suite elapsed times in the JSON include reservation waits and are not fit timings.

| Command | Machine | Result |
|---|---|---|
| just cpp-test | workstation, GCC 15.2 | 106/106 dev shards passed |
| env UMVL_TEST_TIER=full just cpp-test | workstation, GCC 15.2 | 129/129 full shards passed |
| just cpp-test-no-long-double | simbox, GCC 13.3 | 129/129 passed |
| just cpp-test-no-long-double | workstation, GCC 15.2 | 129/129 passed |
| just test | simbox | FAIL 0, WARN 0, SKIP 154, PASS 22153 |
| just test-full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44985 |
| just site-check | workstation | 99 family pages with MathML, 99 C++ snippets, browser/link checks passed |
| just quality emg | workstation | emg pass |

The targeted EMG test binary also passed 39 cases and 5096 assertions in each precision mode. Durable [suite results](2026-11-09-emg-perf-data/final-suites.json), [targeted summary](2026-11-09-emg-perf-data/targeted-summary.txt) and per-suite summaries accompany the report. The initial simbox outage and successful resumed availability check are recorded [separately](2026-11-09-emg-perf-data/simbox-availability.txt); the final remote suites completed successfully.

The [benchmark](2026-11-09-emg-perf-data/benchmark.cpp), [instrumentation](2026-11-09-emg-perf-data/instrument.py), [paired reproduction script](2026-11-09-emg-perf-data/reproduce.py), [fixed-core script](2026-11-09-emg-perf-data/focused.py) and [provenance/validation](2026-11-09-emg-perf-data/validation.json) make the measurements reproducible. Run the numerical scripts under one local CPU reservation; the fixed-core script selects the first allowed CPU. The [final-suite driver](2026-11-09-emg-perf-data/final-suites.py) must be launched without an enclosing reservation because it reserves each child separately and waits between commands.

No shared header, binding implementation, default, golden, d/p/q/r or truth source was changed. No commit, rebase or merge was made.


