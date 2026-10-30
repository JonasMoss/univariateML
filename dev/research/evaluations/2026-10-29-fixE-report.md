# TASK-78.11 / fixE

Base: f4d1c1224bee8e71a907b383b31c8d1b2eafe8d4; branch codex/fixE.
No commits, board edits, fitter edits, wrapper/codegen edits, or golden changes.
Scratch and complete command logs: `/tmp/umvl-codex/fixE/`.
Builds/tests moved to `/tmp/umvl-simbox/run` at the reviewer's request.

## Baseline (before source edits)

- Local `just cpp-test`: **6/6 ctest targets passed**, 279.93 s test time.
  Distribution tests: **503 passed, 3 skipped**, 955567 assertions;
  8947 known numerical failures are permitted by existing `may_fail` cases.
  All targets together: 525 passing cases, 966614 assertions.
- Local `just test` was interrupted during the reviewer's compute migration,
  after package compilation; no final testthat summary was recorded.
- Untouched-source simbox `just test`: **FAIL 3 | WARN 0 | SKIP 0 | PASS 10547**.
  Failures: `sstd` frozen quantile reference at nu=100, xi=4 (about 1e-12);
  `bnbinom` fitter golden (parameter differences up to 1.13e-9);
  `std` fitter golden (sd difference -1.20e-6, nu difference +1.76e-5).
  These predate this patch. Their fitters and frozen values remain untouched.

## Per-family changes

- **beta:** log tails underflowed, extreme inverse-beta calls lost accuracy and
  threw two Boost exceptions; use log-coordinate incomplete beta and log-odds
  bisection, plus exact one-sided endpoint densities without `0*log(0)`.
  The exceptions were Boost internal `newton_raphson_iterate<long double>`
  “no root” errors at parameter point 1, upper p=1e-300 on both input scales.
- **gev:** cancellation corrupted finite-endpoint residuals and upper-log
  quantiles became infinite; retain the affine residual and invert the nested
  logarithm directly, continuous at shape zero.
- **gpd:** wrong threshold density, endpoint cancellation, and lost tiny lower
  log quantiles; use the right-hand threshold limit, accurate support residual,
  log complements, and logarithmic small-quantile scaling.
- **lst:** density overflowed its squared argument and tails/quantiles underflowed;
  use log beta coordinates for Student density, tails, and inversion.
- **std:** median quantiles had spurious offsets and inaccurate logarithmic
  median offsets; use the Student inverse with a compensated log-half expansion.
  Retain standardized log distances and combine scale before exponentiation.
  The std/sstd fitters are unchanged.
- **ht:** endpoint inverses were slightly negative; use explicit support limits
  and direct incomplete-beta half-t tails rather than subtracting half masses.
- **hcauchy:** inherited half-t endpoint and small-CDF cancellation; fixed through
  ht, preserving the existing delegation.
- **ged:** rounded log-half inputs incorrectly returned the exact median;
  invert the central gamma mass with a compensated log-half residual. Gamma
  log tails and inversion now use double-only range-preserving calculations,
  through the macro-safe seam alias when Rmath headers are present.
- **tlambda:** finite endpoints were rounded onto the boundary and logarithmic
  medians were flattened; retain the exact affine endpoint residual and the
  compensated central quantile expansion. Preserve the existing finite-density
  convention at rounded q(0/1) endpoints for lambda>=1 (R regression check).
- **gb2:** density at zero ignored its one-sided limit; distinguish divergent,
  zero, and finite limits. Add the full d/p/q/r truth signature bridge.
- **nsbeta:** add the full truth bridge and inherit beta fixes; retain both
  endpoint distances independently in density and CDF, including translated
  endpoint neighbours.
- **fpareto:** add the full truth bridge; accuracy inherits the GB2 beta bridge.
- **genbeta:** add the full truth bridge; accuracy inherits the beta bridge.

## Files

Changed `dpqr.hpp`: beta, gev, gpd, lst, std, ht, ged, tlambda, gb2, nsbeta.
Changed `test.cpp`: all 13 requested families.
Changed `gb2/beta_tails.hpp`: remove the unreliable Boost inverse fast path.
New own-folder helpers: `lst/tails.hpp` and `ged/tails.hpp`.
Shared `special/tails.hpp`: affine `support_residual`, needed by GEV, GPD,
and Tukey lambda; retains the rounded subtraction's residual using `fma`.
Shared `log_abs_difference` is also used by lst, std, and ged to retain
logarithms of standardized distances beyond double range.
This report.

## Validation

- All 13 truth cases pass without `may_fail`, at default tolerances.
- Truth coverage: **92 parameter points, 1748 value rows, 4048 quantile rows,
  1840000 sampler draws**. Checks cover log density, both tails on both scales,
  quantiles from natural/log input (including -800 and -10000), finite and
  nonfinite support endpoints, adjacent-double brackets, and empirical CDFs.
- Additional focused regressions cover threshold/endpoint density limits,
  exact binary64 finite-endpoint neighbours, subnormal standardization,
  logarithmic medians, and scale rescued before quantile exponentiation.
  Their source comments contain generating mpmath snippets (100 digits).
- No oracle or tolerance changes; sampling algorithms are unchanged.
- Final focused simbox run: **33/33 cases, 37072/37072 assertions**.
- Final simbox `just cpp-test`: **5/6 targets passed**; distribution tests:
  **527 cases: 525 passed, 2 failed, 3 skipped**; 966406 assertions:
  957720 passed, 8686 failed (8684 permitted by untouched `may_fail` cases).
  All targets together: 549 tested cases, 977453 assertions.
- Both unpermitted C++ failures reproduce against original HEAD headers in
  an isolated scratch include tree, using the same compiler and flags:
  **2 cases, 29 assertions, the same 2 failures** (`base-fitter.log`).
  They are `betapr n20 independent slow oracle fixtures` (status) and
  `std (Student-t with location/scale)` (nu golden). No tolerance changes.
- `just quality tlambda`: **pass** (the fitter calls qtlambda).
- Final simbox `just test`: **FAIL 3 | WARN 0 | SKIP 0 | PASS 10547**,
  identical counts and failing test identities to the untouched-source baseline.
  The Rmath integration and rounded Tukey-lambda endpoint checks pass.
- `git diff --check`: pass; changes stay within the permitted family files,
  own-folder helpers, `special/tails.hpp`, and this report.

Open questions: none for the requested truth cases. The reproduced simbox
fitter/reference failures above remain outside this batch's scope.
