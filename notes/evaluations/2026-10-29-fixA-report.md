# TASK-78.4 — tail-accurate Pareto and power families

Base: `f4d1c12`, worktree `fixA`, branch `codex/fixA`. No commits or git writes.
All 12 requested truth cases pass without `may_fail`, at default tolerances.
Full-suite failures on simbox reproduce with unchanged base code; details below.

Files changed, relative to `cppunivariate/include/cppunivariate/`:

- `distributions/{burr,invburr,paralogis,invparalogis,llogis,invpareto,pareto,power,betapr}/dpqr.hpp`.
- Those nine families' `test.cpp`, plus `distributions/{pareto2,pareto3,pareto4}/test.cpp`.
- New `distributions/betapr/beta_quantile.hpp` and shared `special/tails.hpp`.
- This root report. No README, fitter, adapter, R source, oracle or golden changes.

One line per family:

- **burr:** overflowing softplus, cancelled lower tails and premature probability/power conversion; use log hazards, accurate complements, guarded log-space inversion and finite-parameter checks.
- **invburr:** mirrored Burr tail/inverse defects; apply direct log hazards and inversion, with correct endpoint density coefficient and guarded shape products.
- **paralogis:** duplicated unstable Burr formulas; delegate d/p/q to corrected Burr with equal shapes.
- **invparalogis:** reciprocal observations/quantiles overflowed before a representable result; delegate d/p/q directly to inverse Burr with equal shapes.
- **llogis:** overflowing exponentials, lost upper tails and log-input quantiles; use stable softplus, log products and log-odds inversion without forming reciprocal scale.
- **invpareto:** reciprocal Lomax intermediates overflowed/underflowed; delegate d/p/q to inverse Burr with second shape one.
- **pareto:** survival subtraction and quantile denominator underflow; retain log hazards and inverse hazards, including subnormal shapes and exact probability endpoints.
- **power:** lost upper tails/log-input quantiles and wrong endpoint densities; use log hazards/inversion, retain subnormal shapes and evaluate endpoint density limits.
- **betapr:** rounded beta coordinates, cancelled tails and unreliable extreme beta inverses; retain both log coordinates, use beta-tail continuation, validate ordinary inverse candidates and bisect deep tails in log odds; fix zero density limit.
- **pareto2:** missing truth coverage; add uniform-signature truth and shifted-tail regressions; unchanged Lomax delegation passes.
- **pareto3:** missing truth coverage and inherited log-logistic defects; add truth and shifted-tail regressions; corrected log-logistic delegate passes.
- **pareto4:** missing truth coverage and inherited Burr defects; add truth and shifted-tail regressions; corrected Burr delegate passes.

Shared elementary additions are `log_positive_ratio`, `softplus`, `log_softplus`,
`log_softplus_product`, `log_expm1_from_log`, and `tails_from_log_hazard`.
Each serves at least two assigned families: principally Burr/inverse Burr,
with ratio/hazard helpers also used by Pareto, power and beta-prime.
The log-logistic-only product helper stays in its own header.
Beta-prime reuses the existing GB2 `beta_tails.hpp` continuation unchanged;
its inverse validation/fallback stays in the new beta-prime helper.
No existing special-function wrapper or other family's implementation changed.
All new arithmetic is double; no extended-precision range dependency.

Validation and exact counts:

- Baseline before source edits: local `just cpp-test` passed **6/6 targets**.
  Distribution tests: **503 passed cases, 3 skipped; 955567 assertions**,
  including **8947 expected failures** under existing `may_fail` annotations.
  Other targets: bfgs **6 cases/21 assertions**, sampling **1/10204**,
  no-throw **3/56**, CMP **8/619**, tails **4/147**.
- The initial local `just test` was interrupted during the reviewer compute move.
  The completed pristine baseline on simbox was
  **[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]**.
- Final full `just cpp-test` on simbox: **5/6 targets passed**.
  Distribution tests: **518 cases, 516 passed, 2 failed, 3 skipped**;
  **963741 assertions, 956217 passed, 7524 failed**.
  Of those failures, **7522** belong to existing expected-failure truth cases;
  the remaining **2** are the pre-existing fitter checks reproduced below.
  All five other targets pass with the same counts as baseline.
- Requested truth cases in the full binary: **12/12 cases, 31140/31140 assertions passed**.
  Focused truth plus targeted regressions: **24/24 cases, 31199/31199 assertions passed**.
  This includes **12 new regression cases / 59 assertions** with mpmath snippets.
  A final focused rebuild also checks the preserved public parameter names.
- The 12 truth tables contain **1489 density/CDF rows**, **3344 quantile rows**,
  and **76 sampler parameter points**, with **1520000 draws** in total.
  Checks cover log density, both tails on both scales, natural/log-input
  quantiles, endpoints, independent quantile brackets and empirical sampler CDFs.
  Default log/natural-tail tolerance is **1e-10**, quantile tolerance **1e-9**;
  no oracle regeneration, manual truth edits or tolerance relaxation.
- Additional **400-digit mpmath grid: 4752/4752 comparisons passed** across
  the nine primary families, scales/rates `1e-200,1,1e200`, single/first shapes `.1,1,2,20`
  (Burr second shape fixed at 2), observations `0..1e300` including `1e-300`, and log inputs through **-1e6**.
  Beta-prime uses unit shapes in this additional closed-form grid; its general
  shapes are checked by the truth tables. Maximum normalized error **7.3894e-13**;
  log outputs use `max(1,abs(truth))`, quantiles relative error with `1e-300` floor.
  Doctests additionally cover subnormal shapes and rescued overflowing products.
- Final simbox `just test` reproduces its baseline exactly:
  **[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]**; the same three failures.
- `just quality` for all 12 families: **12/12 passed**, comprising **36/36**
  log-likelihood, score/Hessian and eight-start oracle checks; maximum oracle
  improvement **1.78e-11**. Limits are listed by this harness, not executed.
- All **12 sampler bodies** match the base byte for byte; all **98 golden.json**
  SHA-256 hashes are unchanged. Public function names, parameter names/order
  and defaults match the base after whitespace normalization. `git diff --check` passes.

Baseline limitations / open questions:

- Simbox's full C++ failures are `betapr n20 independent slow oracle fixtures`
  (fitter status) and `std (Student-t with location/scale)` (nu golden).
  An isolated binary using pristine base headers and the full build's compiler
  reproduces **both failures: 2 cases, 29 assertions, 27 passed / 2 failed**.
  These are not d/p/q/r regressions; fitter code and fitter checks remain unchanged.
- The three baseline/final R failures are the `sstd` frozen fGarch quantile
  reference, `bnbinom` golden and `std` golden. Resolve these portability issues
  in the owning work; no fixtures or tolerances were changed to hide them.
- The existing shared GB2 inverse fast path returned inaccurate finite candidates
  and could throw internally for the tested beta-prime deep tails despite the
  no-throw wrapper. Beta-prime now avoids that path for deep/extreme inputs;
  the shared GB2/genbeta implementation is outside this batch and remains unchanged.

Logs/scripts: `/tmp/umvl-codex/fixA/`, especially `final-truth.log`,
`final-focused.log`, `final-ctest-details.log`, `final-r.log`,
`final-quality.log`, `base-fixtures.log`, and `grid.cpp` / `grid.py`.
Baseline compiles/tests initially used `/tmp/umvl-cpu/run`; after the reviewer
instruction every compile/test used `/tmp/umvl-simbox/run`, with its remote
slot lock, three build threads and nice priority. No installs, agents or board edits.
