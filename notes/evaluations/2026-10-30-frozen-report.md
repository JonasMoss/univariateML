# TASK-99: frozen distributions

## Implementation

- Generated `mlX()` wrappers build fixed models without data, filling missing
  coefficients from README `simulate:` values. Arguments must be named.
- Shared R construction validates declared domains and joint validity without
  imposing `fit_domain`; design and threshold coefficients use defaults too.
- Frozen objects have `n = 0`, `frozen = TRUE`, and all coefficients in `fixed`.
  Evaluation, simulation, density plotting and coefficient extraction work.
  Data-dependent methods stop: “built from fixed coefficients; it has no data”.
- All-held calls with data use generated internal C++ densities and df = 0.
  Incompatible observations stop; rejected inputs retain adapter diagnostics.
  Tests compare existing adapter routes wherever
  they return a result, including their log likelihoods.
- The 14 stats-dispatched families gain internal `core_d<id>` density adapters;
  ordinary evaluation and simulation retain their existing reference dispatch.
  No C++ library sources or per-family adapters were edited.
- Removed the unreleased simulation export. Public examples use `rml(n, mlX())`;
  existing tests use internal `ml_simulate`. Golden tooling uses frozen models.
- Each fitter help page receives one shared generated frozen-usage paragraph
  listing its README defaults; missing README examples fail generation.

## Files changed

- `univariateML/scripts/{codegen_wrappers,codegen_dpqr,goldens}.R`.
- `univariateML/R/{frozen,code_misc,code_generics,code_bootstrap,code_functions}.R`.
- `univariateML/{NAMESPACE,NEWS.md}` and generic frozen/equivariance tests.
- Synthetic parameter-infrastructure fixture and its missing-example check.
- All 98 distribution READMEs (examples); README_SCHEMA; existing distribution
  R tests that used the removed simulation helper.
- No edits to simulate values, goldens, truth tables, notes, roadmap, CLAUDE.md,
  KNOWN_ISSUES.md or STATUS.md. No commits, git writes, dependency installs or board edits.

## Examples by data source

- Normal: `trees$Height`; Poisson: `discoveries`.
- Gamma, Weibull, log-normal: `precip`; inverse Gaussian: observed `airquality$Wind`.
- Discrete normal and discrete Laplace: `diff(discoveries)`.
- Zero-truncated Poisson/geometric and logarithmic series: package `corbet`.
- GPD: `precip[precip > 40] - 40`, threshold held at zero.
- Location-scale t, standardized/skew t and GED/skew GED: log returns of the
  DAX series in `EuStockMarkets`.
- Other families: seeded samples of 200 from `rml(200, mlX())`, with README
  `fit_args` replayed for fitting. The example diagnostic checks every README
  for warnings/errors; the final package check executes the generated examples.

## Defaults review — suggestions only; no defaults changed

These are candidates for Jonas, not errors in the current distributions.
Unlisted defaults appear reasonable for demonstration or have no clear canonical alternative.
Candidate changes would need fresh truth-table sampler and example checks before adoption.

| Family / coefficient | Current | Suggested | Reason |
|---|---|---|---|
| gamma scale | 0.5 | 1 | Unit scale is easier to explain. |
| invweibull scale | 1/3 | 1 | Unit scale, avoiding a legacy rate conversion. |
| llogis, invtrgamma scale | 0.5 | 1 | Unit scale. |
| std, sstd nu; lst df; ht nu | 3 | 5 | Finite fourth moment, less extreme simulation. |
| ged, sged nu | 3 | 2 | Familiar Gaussian member. |
| snorm, sged xi; sstd xi | 3; 2 | 1 | Symmetric baseline rather than strong default skew. |
| sn alpha | 3 | 0 | Symmetric standard-normal baseline. |
| gumbel mu, sigma | 3, 3 | 0, 1 | Standard location and scale. |
| logitnorm mu, sigma | 2, 3 | 0, 1 | Symmetric, moderate spread on the probability scale. |
| dlaplace scale | 0.4 | 1 | Less arbitrary dispersion. |
| dunif min, max | 2, 10 | 0, 10 | Familiar count range with origin zero. |
| fatigue alpha, beta | 3, 2 | 1, 1 | Moderate shape and unit scale. |
| foldnorm mu | 2 | 0 | Standard folded-normal member. |
| beta / nsbeta shape2; kumar b | 3 | 2 | Symmetric beta baseline / simpler Kumaraswamy shape. |
| bbinom alpha, beta | 2, 3 | 2, 2 | Symmetric mixing distribution. |
| cmb prob, nu | 0.3, 1.5 | 0.5, 1 | Symmetric binomial member. |
| cmp lambda, nu | 4, 1.5 | 1, 1 | Unit-rate Poisson member. |
| gev, gpd xi | 0.3 | 0 | Familiar Gumbel/exponential members. |
| invgamma beta; invgauss mean, shape | 4; 3, 4 | 1; 1, 1 | Unit scale or unit mean/shape. |
| lgamma ratelog | 3 | 1 | Unit rate in log coordinates. |
| lgser theta | 0.9 | 0.5 | Less extreme abundance tail. |
| naka scale; rayleigh sigma | 3; 2 | 1 | Unit scale. |
| weibull shape, scale | 3, 4 | 2, 1 | Familiar shape and unit scale. |
| triang c | 0.4 | 0.5 | Symmetric triangular member. |
| skellam mu1, mu2 | 4, 3 | 1, 1 | Centered signed counts. |
| zipf N, s | 3, 0.5 | 10, 1 | More recognizable finite Zipf illustration. |
| gb2, fpareto, genbeta, pareto2/4 shapes | assorted fractions | unit shapes (review jointly) | Heavy/singular shapes were chosen for validation coverage; canonical subfamilies may explain defaults better. |

## Verification

- All suites run on simbox through `/tmp/umvl-simbox/run`.
- `just regen`: passed.
- Full `testthat::test_local(".", stop_on_failure = TRUE)` with
  `options(testthat.progress.max_fails = Inf)`: **37,381 passes, 0 failures,
  0 warnings, 0 skips**, 408.9 seconds.
- Frozen test file: **3,145 passing assertions**, all **98 families** checked at
  default and supplied golden coefficients, including successful adapter agreement.
- All 98 README examples passed the warning/error diagnostic.
- `just check` with `TMPDIR=$HOME/umvl/tmp` and `UMVL_DISTRIBUTIONS_DIR` set:
  **Status: OK** (0 warnings, 0 notes). Installed tests: **37,368 passes,
  0 failures, 0 warnings, 1 skip** (`test_special.R`, explicitly “On CRAN”).
- Source docs/examples contain no removed simulation API or triple-colon calls;
  `git diff --check` passes; protected-file/default invariants pass.

## Open questions

Only the default choices above require Jonas's decision; they are intentionally unchanged.
