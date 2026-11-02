# Gompertz extreme-parameter fixes — 2026-11-02

Both failures from the [bracket-fixes report](2026-11-02-bracket-fixes-report.md) are fixed. All seven required recipes finished successfully on the local workstation. The final Git diff of every truth.json and golden.json is empty. No commits, board edits, package installs, or changes to other families were made.

The tested tree is branch codex/gompertz-extreme, based on d55df2c8, plus the seven source files identified by SHA-256 below. This report was written after all started builds, tests, truth generation, and metadata collection had finished.

The sgomp reproducer with observations (1e-300, 1, 1e300) and b held at 1 now returns eta = 0.226912564699352, with a directly evaluated gain greater than 0.028 over eta = 0 and absolute total shape score below 1e-10. The stored total log likelihood still rounds to approximately -1e300; its precision no longer decides the face.

For the extreme Gompertz fit in the brief, the R regression verifies that all five draws from rml are finite. At fixed survival uniform U = 0.5, a = 1e-300 and b = 7e302, the standalone sampler gives 1.98247946172984e-300.

The sgomp change is in [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp), with the comparison policy recorded in its [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/README.md). Put E_i = exp(-b*x_i) and A_i = -expm1(-b*x_i). At a held rate,

~~~text
Delta(eta)   = sum(log1p(eta*A_i) - eta*E_i)
Delta'(eta)  = sum(A_i/(1 + eta*A_i) - E_i)
Delta''(eta) = -sum(A_i^2/(1 + eta*A_i)^2).
~~~

Thus the conditional objective is strictly concave when any observation is positive. Its right score at zero decides whether zero can maximize it; all-zero data have the negative constant shape score -n. For the reproducer, the right score is approximately 1 - 2/e > 0, excluding zero. These are exact-arithmetic arguments. The implementation's certificate is conditional on its computed score and bracket comparisons, with the practical floating-point margins stated below; it does not use outward-rounded intervals.

Candidate differences now subtract the rate and shape contributions directly:

~~~text
ell(b1, eta1) - ell(b0, eta0)
  = n*log(b1/b0) - (b1-b0)*sum(x_i)
    + sum(log(1 + eta1*A1_i) - log(1 + eta0*A0_i))
    - sum(eta1*E1_i - eta0*E0_i).
~~~

Equal rates cancel exactly in this expression. Nearby rate ratios use log1p; shape logarithms use the existing softplus representation, and exponential differences use expm1. The common normalization Jacobian cancels as well. This adds an O(n) evaluation to candidate comparisons without changing the search interval, grid, initialization, or scalar iteration limits.

The audit within sgomp covered starting-candidate selection, grid ranking, adjacent-grid peak nomination, golden-section refinement and its final choice, refined-candidate selection, and the final zero-face comparison. They all use the direct difference, subject to the bounded roundoff rule for candidate ranking described next. Full likelihoods remain in the existing envelope bounds and reported output. An unresolved bound or scalar solve retains numerical-failure behavior.

An initial implementation always ranked by the direct difference. It passed the new extreme tests but failed the existing sgomp golden eta assertion at its unchanged relative tolerance of 1e-8: a different point was selected on a roundoff plateau. The final implementation preserves historical ordering only when the direct difference is at most 8*epsilon*n in absolute value AND the floating-point spacing of both stored likelihoods is also at most 8*epsilon*n. Epsilon is double precision machine epsilon. The rule cannot expand with the magnitude of the common likelihood. It preserves ordinary-fit rounding while retaining direct comparisons at huge likelihood magnitudes. The original golden fixture and assertion were left unchanged.

The final face decision has two requirements: its right shape score must be nonpositive, and its directly evaluated difference from the best candidate must be at least -1e-8*n. A positive score prevents snapping to zero even when the gain lies below that likelihood margin. A separate C++ and R regression uses x = log(2) + 1e-6 with b held at 1: its fitted shape is positive and its gain is positive but below 1e-8.

The sampler change is confined to [rgompertz](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/dpqr.hpp). Its exact-arithmetic inverse is

~~~text
X = log1p((b/a)*(-log(U))) / b
  = qgompertz(log(U), a, b, lower_tail=false, log_p=true).
~~~

It retains the historical arithmetic when the ratio and hazard product are finite and the product is at least 1e-3. Otherwise it calls the settled stable quantile with log survival directly. This covers overflowing ratios or products, underflowing ratios, and finite small products that lose precision when added to one. The 1e-3 cutoff also protects accuracy for finite b/a = 1e-7, rather than addressing overflow alone. Both paths consume exactly one uniform per draw; the U-versus-1-U convention is preserved.

The first 200 ordinary draws at a = b = 1 with seed 1 are bit-identical to the historical expression in both C++ and R. Draws using the stable fallback can differ from historical arithmetic by design; bit identity is claimed for the tested ordinary sequence. The generated R sampler path was rebuilt through the normal regeneration recipes. The quantile, other d/p/q code, special functions, truth generators, adapters, and code-generation scripts were unchanged.

The permanent regressions are in the two families' C++ test.cpp files and bindings/r/distributions/{sgomp,gompertz}/test.R. The final targeted C++ executable compiled distribution_tests_main.cpp with just those two family test files, using C++17, -O1, the installed BH include directory, and CPPUNIV_DISTRIBUTIONS_DIR pointing at the canonical dossiers. It passed 20 cases and 6,241 assertions.

The sgomp cases cover common likelihood magnitudes 1e17, 1e100 and 1e300, the independent limiting-data quadratic for eta, a total-score residual below 1e-10, a gain above 0.028, direct-difference agreement at relative tolerance 1e-12, the positive-score tiny-gain case, and ranking when the stored full likelihoods are identical at -1e300. Existing face, collapse, failure-status, golden and independent-score tests also pass. The quadratic oracle treats the first and last observations as their limiting contributions; the finite-data perturbations are far below double precision for these observations.

Sampler fixed-uniform checks use U in {1e-100, 0.1, 0.5, 0.99} and (a,b) in {(1e-300,7e302), (1e300,1e-300), (1e7,1), (1,1)}. Draws must be finite, positive, and agree with the log-survival quantile at relative tolerance 1e-12. R additionally checks 200 seed-1 uniforms for each parameter pair and the fitted-model reproducer. These are numerical consistency and regression checks; the inverse identity above establishes the distribution in exact arithmetic.

All final runs used the local workstation, the supplied CPU-slot locks, nice -n 10, and OMP_NUM_THREADS = OPENBLAS_NUM_THREADS = MKL_NUM_THREADS = 1. Heavy builds and suites used run4 with four build slots; truth generation and metadata used run. The final C++ pair shared one run4 allocation, without nested locks. TMPDIR, JUST_TEMPDIR and XDG_RUNTIME_DIR pointed at this worktree's build/tmp. Simbox was not used.

| Required recipe | Final result on the final numerical source |
|---|---|
| just cpp-test | 106/106 dev shards passed |
| just cpp-test-no-long-double | 127/127 shards passed |
| just test | FAIL 0, WARN 0, SKIP 151, PASS 20,321 |
| just test-full | FAIL 0, WARN 0, SKIP 0, PASS 41,924 |
| just js-build | Passed |
| just js-test | 413/413 passed; zero failed, cancelled or skipped |
| just dpqr-truth gompertz | Passed; 6 points, 2 sampler points, 126 values and 264 quantiles generated |

The C++ pair was rerun after the final sampler cutoff change. Reported harness elapsed times were 49.35 s for C++ dev, 139.42 s for no-long-double, 116.409 s for R dev, 3046.897 s for R full, and 220.405 s for JavaScript tests. These are recorded observations from the shared workstation, not performance benchmarks; outer slot waits and some build setup are outside those figures. The 151 R dev skips are tier exclusions; R full has none.

The environment was Linux 7.0.0-38-generic, GCC 15.2.0, R 4.6.1, Rcpp 1.1.2, BH 1.90.0.1, testthat 3.3.2, Node 22.22.1, Python 3.14.5 and mpmath 1.3.0. Builds emitted existing compiler warnings in untouched tpois code, and login shells reported an absent ~/.cargo/env. These diagnostics did not fail the commands; the R test warning counts above are zero. An exploratory manual targeted compile initially omitted the BH include path and failed to compile; using the same BH fallback as the Justfile resolved it before the successful targeted run.

After truth regeneration, the following read-only check returned exit status zero with no output:

~~~sh
git --no-optional-locks diff --exit-code -- +  'univariateML-cpp/include/cppunivariate/distributions/*/truth.json' +  'univariateML-cpp/include/cppunivariate/distributions/*/golden.json'
~~~

The other-family comparison audit was read-only. It searched all family mle.hpp files for face comparisons, spelling variants such as normal_ll, boundary and collapse, and their tie or allowance definitions. A reproducible nomination command is:

~~~sh
rg -n 'face.*>=|>=.*face|1e-8.*n|1e-9.*size|limit_logLik' +  univariateML-cpp/include/cppunivariate/distributions/*/mle.hpp
~~~

The following families have full-likelihood face comparisons and small-sample analytic magnitude witnesses exceeding 1e16. Paths in the comparison column are relative to univariateML-cpp/include/cppunivariate/distributions. These are source and mathematical magnitude findings; wrong-selection behavior in these other families was not numerically established by this job.

| Family | Comparison location in mle.hpp | Magnitude mechanism |
|---|---|---|
| betapr | 190, 311 | Held gamma-face shape 1e100 and observations 1,2 give a nonzero dispersion contrast multiplied by the shape, of order 1e99 |
| gb2 | 365 | Hold the power at 1 and the surviving gamma-face concentration at 1e100; the same gamma contrast can be of order 1e99 |
| genbeta | 148 | With scale 1, observations exp(-1), exp(-2), and held beta shape 1e100, the transformed gamma-face contrast is of order 1e99 |
| sn | 201 | A held small scale and fixed location can give a half-normal quadratic of order 1e20 |
| snorm | 144 | Held small sd and mean give a proportional half-normal scale and a quadratic of order 1e20 |
| st | 106, 108, 135 | Held small scale/location permit normal or half-normal face quadratics of order 1e20 |
| std | 351, 352 | The conditional normal face with held mean and sd can have a quadratic of order 1e20 |
| ht | 124, 178 | Held small scale at a held endpoint gives a half-normal quadratic of order 1e20 |
| lst | 325 | The conditional normal face with held location and scale can have a quadratic of order 1e20 |
| huber | 479, 481 | The conditional Gaussian face retains the held scale and can have a quadratic of order 1e20 |
| emg | 165, 174 | The conditional Gaussian face retains held location/scale and can have a quadratic of order 1e20 |
| bhatt | 219, 226 | The Gaussian member retains held location/scale and can have a quadratic of order 1e20 |
| ged | 274 | Holding mean 0 and sd 1e-20 with observations 1,2 gives a Laplace-face log likelihood of order -1e20 |
| pig | 98 | Holding mean 1e100 with observations 1,2 gives a Poisson-face log likelihood of order -1e100 |

For the Gaussian rows, observations of order 1, a location or endpoint of 0 where applicable, and scale of order 1e-10 produce residual squares of order 1e20. The snorm moment convention changes constant factors, not this order. For the gamma rows, the positive contrast log(mean(y)) - mean(log(y)) for y = 1,2 is multiplied by the held concentration. Coordinate normalization alone does not remove either mechanism. Whether a particular numerical fitter reaches its comparison or first stops with failure requires separate validation.

Additional full-objective comparison matches are bnbinom:178, burr:184, sged:400, sstd:643, slash:221, tlambda:180, tnorm:469 and 531, trgamma:609, zib:144 and 186, zinb:174, ztbinom:100, ztnbinom:71 and 73, and tbinom:240. Tbinom uses a 1e-9*n allowance. Their summed likelihoods can also exceed 1e16 in magnitude through sample-size amplification: in exact arithmetic, repeating a sample multiplies each candidate and face likelihood by the repetition count. This establishes magnitude reachability for representable observations without asserting a feasible large-array workstation workload or a reproduced wrong fit.

Three adjacent patterns need separate interpretation. Bbinom:529 compares a kernel with the parameter-independent log-choose term removed; the remaining common kernel is not a direct likelihood gain. Hyper and nhyper compare full likelihoods with margins proportional to 1 + abs(face), rather than the requested n-only pattern. Lomax:319 already compares a directly computed profile gain, so it is not a full-common-likelihood match. No files in any of these families were edited.

The existing open guarantee for joint sgomp fitting remains precise: the finite envelope bounds establish a search interval, but the rate grid does not certify enumeration of every stationary point. A proof of profile unimodality or certified interval enumeration is still missing. The conditional shape proof and corrected candidate comparisons do not establish that global joint guarantee. This limitation is retained in the canonical dossier; both requested reproducer fixes and the read-only audit are complete.

The SHA-256 identities of the final tested source patch are:

~~~text
402f272c8cfc9476e8611cd026e463efa10d87e32007700de6eac9d1ca5be81d  bindings/r/distributions/gompertz/test.R
ddfabfeaeb8c6ff4bb243c66041a282a95d2a86744dfc41f10838d43f4017797  bindings/r/distributions/sgomp/test.R
d1c53445a79a3def3c6137cbe7688c4df885580decbf59132b4436dec7d3748c  univariateML-cpp/include/cppunivariate/distributions/gompertz/dpqr.hpp
92f8272013c8eaec48d641d99be22628c92a6cc86f4fafdf863be3ddac84d682  univariateML-cpp/include/cppunivariate/distributions/gompertz/test.cpp
76ff8109995ac48593dcf73a3d681abea0f4781913561c3f2c78606d9ae2de96  univariateML-cpp/include/cppunivariate/distributions/sgomp/README.md
79a860d5f795d4ed266f7135577799ebe5fc22b8d1b6b79d494ae615d7b2a77d  univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp
5e049eb31a98653873e547992f50b6889677d28fcdd68f1bdebd326c8da65fea  univariateML-cpp/include/cppunivariate/distributions/sgomp/test.cpp
~~~

The prescribed codex-math-web/bin/show command was attempted after writing this report. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains at the requested path. Viewer configuration and files outside the worktree were left untouched.
