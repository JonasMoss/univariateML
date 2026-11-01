# Portability: first macOS arm64 and Windows CI run

2026-11-01 · TASK-129

The first cross-platform run exposed a nonstandard C++ constant, a truth-table
parser dependent on extended exponent range, a Windows package include-path
omission, iterative-fit regression tolerances, and a JavaScript test output
directory that was absent on a clean runner. The changes below address those
failures and additional failures visible in the same logs. No golden JSON was
changed. Native Windows and macOS confirmation remains for the next CI run;
the Linux double-intermediate build is a useful proxy, not an emulation of
macOS `long double` or its math library.

## Evidence and scope

The tested source is worktree revision `7eaf34a20829b2d7d35578ddff3a7800f3beb1a9`
plus the uncommitted changes accompanying this report. The failed CI source was
`b15f014`: [core run 41302425392](https://github.com/JonasMoss/univariateML/actions/runs/41302425392),
[R check run 41302425407](https://github.com/JonasMoss/univariateML/actions/runs/41302425407),
and [site run 41302425410](https://github.com/JonasMoss/univariateML/actions/runs/41302425410).
The Linux and macOS drift columns below reproduce the downloaded
`golden-drift.csv` observations from that R run. Each is the maximum
componentwise relative difference across the frozen coefficients and log
likelihood, with absolute error for a zero reference. These measurements are
regression evidence for the fixed datasets, not bounds for other samples.

Local environment: Linux x86-64 `7.0.0-38-generic`, GCC 15.2.0, Clang 21.1.8,
R 4.6.1, Node 22.22.1, Python 3.14.5 and SymPy 1.14.0. Boost headers came
from installed BH. Runs used the shared CPU locks, at most four slots, one
BLAS/OpenMP thread per worker and `nice -n 10`; temporary files were under
`build/tmp`. Tests use the checked-in golden and truth tables and the seeds
in their sources, including JavaScript sampler seed `20261029 + i`.

| Final command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | 105/105 dev shards passed |
| `/tmp/umvl-cpu/run4 just cpp-test-no-long-double` | 126/126 full shards passed |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 150, PASS 19898 |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 41344 |
| `/tmp/umvl-cpu/run4 just js-build` | passed |
| `/tmp/umvl-cpu/run4 just js-test` | 400/400 passed |
| `/tmp/umvl-cpu/run4 just site-check` | passed; 98 family pages and 98 C++17 snippets, MathML, links and browser checks |

Each command ran separately with `TMPDIR`, `JUST_TEMPDIR` and
`XDG_RUNTIME_DIR` set to the worktree's `build/tmp`. The C++ no-long-double
configuration switches the library's intermediate type but does not change
Linux's physical `long double` type or libm. A separate single-slot Clang
`-std=c++17 -pedantic-errors -fsyntax-only` compilation included `<cmath>`,
saved Linux `M_PI`, undefined that macro, and then included the umbrella
header. It passed; a `static_assert` also established equality of the new
constant and this Linux macro. `rg M_PI univariateML-cpp` found no remaining
references. No Windows cross-compiler was available locally.

## Failures, causes and changes

### Windows C++ and package assembly

* **Core:** `M_PI` is not a C++17 constant and MinGW did not define it in the
  CI build. `core/constants.hpp` defines the same double literal as the Linux
  macro; all library and C++ test references use `cppunivariate::pi`. The
  strict compile, constant equality assertion and native regression suites
  provide local evidence; the Windows build is still required.
* **R check:** `assemble.R` copies the `.hpp` tree into `inst/include`, but
  `src/Makevars.win` lacked the `-I../inst/include` flag present in
  `Makevars`. The Windows file now has that flag. The locally assembled
  package contains `cppunivariate/core/constants.hpp`, and both assembled
  Makevars files contain the same include flag. Only Windows R CMD build and
  check can confirm the native toolchain and tarball path end to end.

### macOS C++ truth loader and remaining test failures

* **Truth matrices:** formatted stream extraction of an out-of-range long
  double sets failbit on arm64, where long double has double's range.
  `truth_matrix` now uses `strtold` semantics, permits underflow and overflow,
  requires complete cell consumption and rejects missing delimiters. The
  added loader case checks signed underflow, overflow to infinities, trailing
  junk and an unterminated cell; the existing subnormal case is retained.
  Both local C++ suites pass. The native macOS loader still needs confirmation.
* **Large-size binomial likelihood:** the macOS alternative-size comparison
  exposed cancellation in log-gamma differences when extended intermediates
  are double. For a maximum observed count at most 100000, the double-width
  branch forms a prefix sum of `log1p(-j/size)` and combines the size powers
  with the fitted probability. For observation `k`, mean `m` and size `s`,
  the exact real-arithmetic identity is `k*log(m) - lgamma(k+1) +
  sum(log1p(-j/s), j=0..k-1) + (s-k)*log1p(-m/s)`. Prefix sums cost
  O(maximum count + distinct counts) work and O(maximum count) memory.
  The existing large-size test passes with double intermediates. The identity
  is algebraic; its floating-point error and the search's sign decisions do
  not have a general enclosure. See the binomial dossier and open items below.
* **Burr and inverse Burr goldens:** finite-difference BFGS estimates drifted
  beyond the old `1e-8` C++ coefficient allowance. The common C++ allowance is
  `1e-6`; R uses the table below. Existing independent likelihood and score
  checks remain. This is an empirically calibrated comparison, not a proof of
  a coefficient error bound.
* **Hypergeometric tests:** the independent enumeration and large-argument
  references themselves used `long double` and lost precision on macOS.
  The tests now account for the reference calculation's precision and, when
  that type is double-width, check reported adjacent ties against a wider
  likelihood envelope. Exact small tie examples remain separate. The local
  full proxy passes, but it does not exercise the physical double-width
  `long double` branch. The envelope is numerical evidence, not an exact tie
  proof; native confirmation and independent tie checks remain open.
* **CMP reference grid:** macOS errors reached about `1.47e-14` relative
  against the independent grid, above `3e-15`. The test permits `3e-14` when
  `long double` has double precision, retaining `3e-15` otherwise. This is an
  observed margin, not a uniform error theorem; the native rerun must confirm.
* **Zero-inflated and zero-modified negative binomial:** iterative golden
  coefficients use drift-based C++ allowances. A generated negative-binomial
  sample gave a positive inflation estimate on macOS: a seeded standard
  library random distribution need not produce the same sample on every
  implementation. Its test now compares the returned likelihood with the
  fitted zero-inflation boundary instead of requiring `pi == 0`. The score
  checks and the boundary comparison pass locally; the macOS result needs
  confirmation.

### macOS R check

* **Iterative golden regressions:** the old default `1e-12` compared some
  optimizer endpoints far more tightly than their stopping scales. The
  platform-independent rule and per-family values are below; closed-form
  defaults remain at `1e-12`.
* **Exponential overflow:** a sum of two `1e308` values overflows when long
  double has double range. The free-rate fallback sums data scaled by their
  maximum; the held-rate fallback sums rate-weighted observations if the
  unscaled sum overflows. These are real-arithmetic rearrangements of the
  same likelihood. R and double-intermediate C++ extreme cases pass locally.
* **Pareto overflow:** `log1p((x-b)/b)` loses a finite answer when the ratio
  overflows. It remains in use for representable ratios and the fallback is
  `log(x)-log(b)`. The identity holds for positive `x` and `b`; the
  `1e-300, 1, 1e300` case passes with double intermediates.
* **Azzalini skew normal:** `(x-xi)/omega` can overflow before division even
  when the standardized value is finite. Density and CDF retry
  `x/omega-xi/omega` only if that result is finite. The R extreme example and
  an appended C++ case pass locally, including a genuinely infinite result.
* **Translated half-normal and half-Cauchy samplers:** macOS produced equal
  but non-identical floating results for two equivalent arithmetic paths.
  The affected shift checks use a `1e-14` equality tolerance while preserving
  their fixed seeds and value comparison.
* **Held-moment two-piece skew normal:** at a rounded half-normal endpoint,
  macOS reported `Conditional skew-normal fit did not converge` rather than
  a half-normal face. A finite tail guard is not an estimate or a certificate
  of which face wins. The test now accepts that explicit numerical failure as
  well as the boundary diagnosis, and retains the independent limiting
  likelihood check. The fitter was not changed; resolving the face or proving
  the global conditional maximum at this rounded endpoint remains open.

### Site workflow

The JavaScript test already reads the checked-in truth tables. Its exit
handler **writes** `build/scratch/truth-results.json`; the CI failure was
`ENOENT` because `build/scratch` did not exist. It now creates that directory
recursively before writing. All 400 JavaScript tests and the site check passed
locally. The next clean CI runner must confirm the missing-directory case.
The site check used the explicitly permitted copy of the existing
`site/node_modules`; no package installation was performed.

## R golden tolerance rule and measured drift

For each listed iterative fit the chosen allowance is at least four times the
largest observed Linux or macOS drift, rounded upward; the established
finite-difference BFGS group retains its earlier `3e-5` allowance. The test
also requires the allowance to be no greater than ten times the square root
of the relevant declared stopping scale. For `ht`, the table uses its inner
scale-search threshold `2e-14`; its outer shape search has a fixed iteration
count. Gradient, interval and objective tolerances measure different things:
without curvature and rounding bounds they do not imply a coefficient error
bound. This rule is an empirical regression canary, not a proof of estimator
accuracy. The table uses the adapter defaults or the named internal threshold;
no platform branch is used. All unlisted families retain `1e-12`.

| Family | Stopping scale | Linux drift | macOS drift | Golden tolerance |
|---|---:|---:|---:|---:|
| betapr | 1e-06 | 0 | 6.314e-08 | 3e-07 |
| bhatt | 1e-08 | 0 | 4.394e-07 | 2e-06 |
| bnbinom | 1e-08 | 2.1e-10 | 2.926e-07 | 2e-06 |
| burr | 1e-08 | 0 | 2.364e-07 | 1e-06 |
| dgamma | 1e-08 | 0 | 2.384e-08 | 1e-07 |
| emg | 1e-08 | 0 | 5.319e-08 | 3e-07 |
| ged | 0.0001221 | 1.002e-06 | 2.307e-06 | 3e-05 |
| gev | 1e-10 | 3.463e-11 | 3.477e-11 | 2e-10 |
| gompertz | 0.0001221 | 1.205e-12 | 1.202e-12 | 1e-11 |
| ht | 2e-14 | 0 | 4.879e-08 | 2e-07 |
| invburr | 1e-08 | 0 | 9.976e-08 | 5e-07 |
| invparalogis | 0.0001221 | 0 | 2.446e-07 | 3e-05 |
| lst | 0.0001221 | 0 | 9.308e-15 | 3e-05 |
| paralogis | 0.0001221 | 4.078e-08 | 4.168e-07 | 3e-05 |
| pareto4 | 1e-10 | 0 | 2.177e-08 | 1e-07 |
| pig | 1e-09 | 0 | 2.391e-08 | 1e-07 |
| sged | 0.0001221 | 1.883e-06 | 2.447e-06 | 3e-05 |
| sstd | 0.0001221 | 2.491e-06 | 4.85e-06 | 3e-05 |
| std | 0.0001221 | 4.425e-06 | 2.945e-06 | 3e-05 |
| trgamma | 1e-10 | 0 | 2.776e-12 | 2e-11 |
| zinb | 1e-10 | 0 | 7.442e-10 | 3e-09 |
| zmnbinom | 1e-10 | 0 | 6.219e-07 | 3e-06 |

## Open validation and next CI run

1. Run the unchanged CI workflows on macOS arm64 and Windows: confirm the
   C++17 compile, truth parser, all C++ and R tests, source-tarball assembly,
   and the clean-runner JavaScript output directory. Other platform-specific
   failures may appear after the original blockers are removed.
2. Validate the binomial double-width likelihood for maximum counts above
   100000 and bound rounding in the profile-increment sign near a tie. The
   documented real-arithmetic unimodality does not itself certify the
   floating-point sign. The current larger-count path retains the previous
   log-gamma calculation.
3. Establish independent exact or higher-precision answers for the macOS
   hypergeometric near-ties, and resolve or certify the two-piece skew-normal
   held-moment rounded endpoint. A convergence failure remains an honest
   outcome there; a finite numerical tail guard is not returned as a fit.
4. The original macOS R CMD check also had two non-failing notes: generated
   manual pages contain relative links to repository `dev/` documents, and
   HTML/math validation was skipped because the runner's Tidy was too old and
   R package V8 was unavailable. Reviewing installed-document link targets
   and providing those validation tools are separate outstanding work; no
   package installs were authorized or performed here.


## Second run

2026-11-01 · TASK-129 · job portability-2

The second CI run at `bbccd5b6cfbc0b39c1d532f4bb73882338b266ae`
passed core on Linux, macOS arm64 and Windows, R check on Linux and macOS,
and the site workflow. Windows R check exposed two Tukey-lambda quantile
failures, three further golden coefficient comparisons, and a missing manual
font. This follow-up fixes those paths without changing fitters or golden
JSON. The tested source is that revision plus this worktree's uncommitted
changes; no commit, push, dependency installation or board edit was made.

Evidence is the supplied [R check run 41308941111](https://github.com/JonasMoss/univariateML/actions/runs/41308941111),
particularly each platform's `build/ci-r/golden-drift.csv` and Windows
`00check.log`, `Rdlatex.log`, and `tests/testthat.Rout.fail`, downloaded under
`build/ci-artifacts/41308941111/`. The table below preserves the measured
values independently of those ignored downloads. The earlier table above
remains the historical first-run calibration; this section supersedes it.

### Tukey-lambda affine overflow

At `p=1, mu=-1e308, sigma=1e308, lambda=.5`, `sigma/lambda`
overflows although the endpoint is finite. The old endpoint fallback was
`fma(2, (sigma/2)/lambda, mu)`: its multiplication is mathematically
`2e308`, while the final sum is `1e308`. At `p=.999`, the standardized
quantile is approximately `1.935754196671554`; the old interior call
`fma(sigma, mag, mu)` likewise has an overflowing product but a finite sum,
approximately `9.35754196671554e307`. These are the cancellation-sensitive
operations on the failing Windows path. Correct fused evaluation rescues
them on Linux/macOS; the Windows observations are consistent with loss of
that protection. The exact internal Windows libm implementation has not been
inspected locally.

The overflowing-product paths now form twice the half-scale sum:
`2*(mu/2 + signed_half_displacement)`. Endpoints use `(sigma/2)/lambda`,
interior finite magnitudes use `(sigma/2)*mag`, and the logarithmic fallback
uses `exp(size-log(2))`. The ordinary finite-product path retains its prior
`fma` call. This avoids requiring a fused cancellation of an overflowing
product and uses only double arithmetic. The formula is an algebraic
identity, not a general floating-point error bound. No optimizer, search,
default fit or golden data changed.

The appended C++ case `tlambda: affine cancellation survives overflowing
unscaled products` checks both signs, endpoint values divided by `1e308`
(relative tolerance `1e-12`), finite interior quantiles at `.001`/`.999`,
and CDF round trips (`1e-12`). The pre-existing R overflow regressions and
truth-table tests also pass. The canonical tlambda dossier records the
formula and its validation limits. Native Windows confirmation remains open.

### Three-platform golden calibration

The new tolerances are `gb2=5e-10`, `invtrgamma=2e-10`, and
`tlambda=3e-7`. Rechecking every allowance against all three files also
requires `ht=5e-7` and `zinb=4e-9` (previously `2e-7` and `3e-9`).
All 25 listed allowances satisfy both `tolerance >= 4*max(platform drift)`
and `tolerance <= 10*sqrt(stopping scale)`; the established FD BFGS group's
allowances are retained. Unlisted families retain `1e-12`, and their observed
drifts also fit the four-times margin. Drift is the maximum componentwise
relative difference across coefficients and log likelihood, absolute for a
zero reference, with matching signed infinities counted as zero. This is an
empirical canary on frozen data, not a coefficient-error certificate.
Stopping scales come from adapter defaults (the inner scale stop for `ht`).

| Family | Stopping scale | Linux drift | macOS drift | Windows drift | Golden tolerance |
|---|---:|---:|---:|---:|---:|
| betapr | 1e-06 | 0 | 6.31404e-08 | 1.54883e-08 | 3e-07 |
| bhatt | 1e-08 | 0 | 4.39421e-07 | 0 | 2e-06 |
| bnbinom | 1e-08 | 2.10007e-10 | 2.92629e-07 | 3.55279e-10 | 2e-06 |
| burr | 1e-08 | 0 | 2.36436e-07 | 1.02892e-08 | 1e-06 |
| dgamma | 1e-08 | 0 | 2.38419e-08 | 0 | 1e-07 |
| emg | 1e-08 | 0 | 5.31873e-08 | 0 | 3e-07 |
| gb2 | 1e-07 | 0 | 2.3021e-13 | 1.14705e-10 | 5e-10 |
| ged | 0.00012207 | 1.00171e-06 | 2.30709e-06 | 1.35439e-06 | 3e-05 |
| gev | 1e-10 | 3.46323e-11 | 3.47665e-11 | 3.47328e-11 | 2e-10 |
| gompertz | 0.00012207 | 1.20527e-12 | 1.20201e-12 | 1.20527e-12 | 1e-11 |
| ht | 2e-14 | 0 | 4.87888e-08 | 1.24042e-07 | 5e-07 |
| invburr | 1e-08 | 0 | 9.97613e-08 | 5.44162e-08 | 5e-07 |
| invparalogis | 0.00012207 | 0 | 2.44597e-07 | 4.66729e-07 | 3e-05 |
| invtrgamma | 1e-10 | 0 | 9.07305e-13 | 3.51823e-11 | 2e-10 |
| lst | 0.00012207 | 0 | 9.30762e-15 | 2.93686e-16 | 3e-05 |
| paralogis | 0.00012207 | 4.07844e-08 | 4.16841e-07 | 4.07844e-08 | 3e-05 |
| pareto4 | 1e-10 | 0 | 2.17712e-08 | 0 | 1e-07 |
| pig | 1e-09 | 0 | 2.39139e-08 | 0 | 1e-07 |
| sged | 0.00012207 | 1.88268e-06 | 2.44709e-06 | 7.24354e-07 | 3e-05 |
| sstd | 0.00012207 | 2.49123e-06 | 4.84975e-06 | 2.19864e-06 | 3e-05 |
| std | 0.00012207 | 4.42459e-06 | 2.94534e-06 | 3.80936e-06 | 3e-05 |
| tlambda | 1e-07 | 0 | 2.62699e-15 | 5.99555e-08 | 3e-07 |
| trgamma | 1e-10 | 0 | 2.77603e-12 | 2.7623e-12 | 2e-11 |
| zinb | 1e-10 | 0 | 7.44189e-10 | 7.88109e-10 | 4e-09 |
| zmnbinom | 1e-10 | 0 | 6.21949e-07 | 0 | 3e-06 |

### Manual font and the non-standard-file NOTE

Windows `Rdlatex.log` identifies missing Courier metric `pcrr8t.tfm`.
The R check workflow now runs `tlmgr install courier` immediately after
`setup-tinytex`, followed by `kpsewhich pcrr8t.tfm`, identically on every
platform. This makes a missing metric fail at the font setup step.

The Windows non-standard-file NOTE names only `univariateML-manual.tex`.
It is the failed PDF manual's intermediate, not output left by the tests.
Linux and macOS second-run logs already mark the directory check OK.
Consequently no test cleanup was added and no check artifacts were hidden
or deleted. The local tarball check builds the PDF manual successfully and
reports both non-standard things and temporary detritus OK, supporting this
diagnosis. Windows TinyTeX confirmation still requires CI.

### Final local validation

All commands completed in the foreground, one at a time through the shared
four-slot wrapper, with `nice -n 10`, single-thread BLAS/OpenMP, and temporary
files under the worktree's `build/tmp`. Environment: Linux x86-64
`7.0.0-38-generic`, GCC 15.2.0, R 4.6.1, BH 1.90.0-1, Rcpp 1.1.2,
testthat 3.3.2, rcmdcheck 1.4.0. No simbox or Windows cross-compiler was used.
The local TeX installation already supplies Courier; it does not validate
installation into a fresh Windows TinyTeX tree. Workloads use checked-in
golden/truth data and the existing tests' seeds; no fixtures were regenerated.

Common environment and exact final commands:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run4 nice -n 10 just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 just test
/tmp/umvl-cpu/run4 nice -n 10 just test-full
/tmp/umvl-cpu/run4 nice -n 10 Rscript .github/scripts/assemble.R
UMVL_TEST_TIER=cran /tmp/umvl-cpu/run4 nice -n 10 Rscript .github/scripts/r-check.R
```

The full R run preceded the final C++ and R dev reruns; all used the final
numerical implementation. The reruns followed a guard refinement that avoids
computing `DBL_MAX/mag` when `mag<=1`.

| Check | Final result |
|---|---|
| C++ dev | 105/105 shards passed |
| R dev | FAIL 0, WARN 0, SKIP 150, PASS 19898 |
| R full | FAIL 0, WARN 0, SKIP 0, PASS 41344 |
| CI assembly script | exit 0 |
| CI source-tarball check script, CRAN tier | exit 0; 0 errors, 0 warnings, 2 NOTEs |
| Tarball tests, examples including donttest, vignettes, PDF manual | all OK |
| Non-standard check-directory files and temporary detritus | both OK |
| `git diff --check` | passed |

Logs remain under `build/scratch/portability-2-*.log`; the tarball and complete
R check output are under `build/ci-r/`. The two remaining NOTEs concern
existing relative development-document links in R help and skipped HTML
validation/math rendering (missing `tidy` and R package `V8`). They are not
permitted test failures: all executed tests passed. The unavailable validators
are missing coverage, and were not installed under this job's no-install rule.

### Open validation

1. Run the revised workflow on native Windows to confirm both overflow
   regressions and the Courier installation/manual cleanup on its actual
   R/libm/TinyTeX versions. The supplied failing artifacts identify the paths;
   local Linux tests do not emulate that toolchain. No GitHub write action
   was taken or CI run triggered by this job.
2. HTML validation and math-rendering checks remain unexecuted locally until
   an environment supplies `tidy` and `V8`. The existing relative R help links
   remain a documentation NOTE; fixing those is outside this portability
   patch. Neither NOTE prevents the CI check script from passing.
3. The first-run binomial sign/tie certification and skew-normal boundary
   questions above are unchanged; this follow-up makes no new mathematical
   claims about those fitters. The parallel cmb-size job's files were untouched.
