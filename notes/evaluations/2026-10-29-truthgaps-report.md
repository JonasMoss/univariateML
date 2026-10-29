# TASK-78.12: controlled oracle routes

Base: `f4d1c1224bee8e71a907b383b31c8d1b2eafe8d4` (`codex/truthgaps`), initially clean.
No commits, git mutations, board edits, sub-agents, package installs or dpqr.hpp changes.
All four oracle gaps are closed. Skellam and sn pass strictly; bnbinom and pig use may_fail.
Full simbox suites retain the unrelated fit/reference fixture failures detailed below.

Changed: four family-local truth.py routes and truth.json tables; four test.cpp adapters;
quality/test_truth_gaps.py; quality/truth.py (reject NaN cells; configurable deadline, default 900 s); four generation status JSONs;
notes/evaluations/2026-10-29-dpqr-truth/truthgaps-checks.csv; this report.

- bnbinom: swap beta-mixture integrals; factorial-series tail with a telescoping gamma-ratio remainder. Handles alpha=0.2 and huge integer targets.
- pig: exact half-integer Bessel recurrence with a geometric count-tail bound; large-order gamma-integral Taylor expansion and Euler integral for the count tail, with next-term, truncation and quadrature bounds.
- skellam: positive Poisson-convolution/Bessel-I series; geometric bounds for both the inner series and reflected count tails.
- sn: positive Gaussian wedge integration, log-concavity truncation bound and quadrature error estimate; Mills bounds at enormous arguments. No Owen T routine calls; independently checked by real-line density quadrature.

All four use the existing sampler/fit/one-coordinate rule and binary64 input rounding.
Bounds and derivations are in the route docstrings. Tables are written only after all points pass.
Each wired truth case checks log density, both CDF tails/scales, quantile inputs on both scales,
and 20,000 deterministic RNG draws per point. Cases that expose C++ failures use may_fail.

Independent validation (100–110 decimal digits): beta-geometric identity and beta-mixture
quadrature; direct Bessel K values, inverse-Gaussian mixture quadrature and positive count sums;
Poisson-convolution CDF sums, symmetry and zero-rate Poisson limits; F(z)=Phi(z)^2 at skew=1, reflection and direct
Gaussian quadrature at skew=3. The generator also checks normalization and pmf differences
or density derivatives at every parameter point.

| Family | Parameter points | x rows | Quantile rows |
|---|---:|---:|---:|
| bnbinom | 8 | 1688 | 352 |
| pig | 6 | 1253 | 264 |
| skellam | 6 | 1248 | 264 |
| sn | 8 | 152 | 352 |

Validation logs: `/tmp/umvl-codex/truthgaps/`.

- Baseline: local ctest 6/6 targets passed (316.22 s); simbox R `[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]`.
- Final: simbox ctest 5/6 targets passed (146.24 s); R `[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]`.

- Baseline distribution target: 503 passed, 3 skipped; 955,567 assertions, 8,947 permitted failures.
- Final Python: 29 tests passed (including 8 new oracle/invariant tests).
- Strict new-family audit: 4 cases, 58,323 assertions; 2 passing cases, 2 failing cases;
  700 assertion failures (bnbinom 652, pig 48); no unexpected exceptions.
- C++ distribution target on simbox: 507 cases, 505 passed, 2 failed, 0 skipped;
  1,013,890 assertions: 1,004,241 passed, 9,649 failed (9,647 permitted, 2 old fit assertions).
  Other targets passed: bfgs 6/21, sample 1/10,204, no_throw 3/56, cmp 8/619, tails 4/147 (cases/assertions).
- The two existing C++ fit failures (betapr slow-oracle status, std golden nu) reproduce alone:
  2 cases, 29 assertions, 2 failures, without any new truth case executing. Their files are unchanged.
- Baseline C++ was local; final C++ and R were on simbox. R reproduces its exact baseline failures.
- Sampling: 560,000 attempted draws; 394 guarded invalid bnbinom draws at point 4.
  1,491 empirical CDF checks (1 failed); 28 point-validity checks (1 failed).
- All 28 normalization checks passed at 1e-25; all tables contain zero NaN cells.
- Default 180 s CLI budget expired under contention; SN CLI regeneration succeeded with --timeout 900.
  The generic deadline is now configurable and defaults to 900 s. Numerical bounds are unchanged.

Observed C++ failures (strict standalone audit; expected-failure decorations removed):

| Family | Check | Failing | Worst point:input | Ours | Truth |
|---|---|---:|---|---|---|
| bnbinom | log_d | 42 | 0:5477225575051655 | nan | -104.62378069500319 |
| bnbinom | log_lower | 45 | 0:5477220 | nan | -9.99999662257214e-13 |
| bnbinom | log_upper | 225 | 0:5477220 | nan | -27.631021453671892 |
| bnbinom | lower | 45 | 0:5477220 | nan | 0.99999999999900002 |
| bnbinom | q_upper | 31 | 0:1e-300 | inf | 5.4772255750516609e+150 |
| bnbinom | q_upper_log | 39 | 0:-690.77552789821368 | inf | 5.4772255750515962e+150 |
| bnbinom | r_cdf | 1 | 4:46086347312 | 0.98029999999999995 | 0.99000000000001065 |
| bnbinom | r_valid | 1 | 4:0 | 394 | 0 |
| bnbinom | upper | 223 | 0:5477220 | nan | 9.9999966225671357e-13 |
| pig | log_d | 12 | 0:1e+100 | nan | -3.1845373111853464e+99 |
| pig | log_lower | 12 | 0:1e+100 | nan | 0 |
| pig | log_upper | 12 | 0:1e+100 | nan | -3.1845373111853464e+99 |
| pig | lower | 12 | 0:1e+100 | nan | 1 |

Full failure inventory, including passing kinds, is in `notes/evaluations/2026-10-29-dpqr-truth/truthgaps-checks.csv`.

Open questions: no remaining oracle gaps. Production tail/range fixes and StdRng int-range
sampling belong to follow-up work; cross-machine fit fixtures remain under TASK-75.
Pre-existing simbox R failures: frozen sstd reference; bnbinom and std golden comparisons (TASK-75).
BNB sampler: a test-only RNG guard returns NaN for latent Poisson means beyond int range,
avoiding the StdRng rejection loop. r_valid reports the number of affected draws; r_cdf at
a huge count also fails because those draws cannot be represented. Production RNG is untouched.
The earlier TASK-78.2 aggregate report/summary remains a historical snapshot; this report and the
four updated status JSONs record closure of its oracle gaps. Goldens and protected files are unchanged.

Viewer: the requested show command returned HTTP 403 because this worktree is outside
CODEX_MATH_WEB_VIEW_ROOTS. The report remains here; no viewer settings were changed.
