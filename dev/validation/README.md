# Validation guide

The checks below test specific inputs, not entire parameter spaces. A pass can
contain skips; inspect them and any permitted failures. Source and tolerances in
the linked code govern when they differ from a report. Mathematical proofs,
independent numerical comparisons, internal identities and regression fixtures
are different kinds of evidence.

| Check | Command | Kind of evidence | Runs in |
|---|---|---|---|
| [Core diagnostic sweep](diagnostics.md) (the 98 families of 2026-10-31; st, gengamma and expweibull are not in the checked-in sweep; supported holds, invalid/degenerate data) | `just cpp-test` | status/domain regression, not an existence proof | monorepo dev/full |
| C++ fit goldens | `just cpp-test` | agreement with independent references where present, else regression | ordinary |
| C++ family cases: holds, limits, closed forms, score residuals | `just cpp-test` | internal identities; closed forms where derived | ordinary |
| C++ truth tables | `just cpp-test` | independent numerical, given the README density | ordinary |
| C++ shared targets: BFGS, sampling, no-throw, CMP, tails | `just cpp-test` | internal identities, finite examples | ordinary |
| R golden regression, conformance | `just test` | independent reference comparisons; interface contracts | ordinary, tarball check |
| R generated d/p/q/r against truth tables | `just test` | independent numerical, given the README density | ordinary, tarball check |
| R reference values (fGarch, logitnorm, nakagami, sads) | `just test` | another implementation, with its own defects | ordinary, tarball check |
| R scale equivariance, restrictions, parameter infrastructure | `just test` | internal identities; contracts | ordinary (generator test: checkout only) |
| Codegen signature, control and example checks | `just gen` | contracts, not numerics | every regeneration |
| Self-contained R package | `just check-standalone` | self-containment under CRAN-like conditions | extended |
| Quality harness: likelihood, score, observed information, eight starts | `just quality` | independent numerical; multistart is not a proof | extended, needs Python tools |
| Truth-table regeneration | `just dpqr-truth` | produces the oracle; rewrites tracked files | extended, needs Python tools |
| Slow-oracle sweep | `dev/research/ml-behaviour-sweep.R` | independent optimizer on finite samples | historical, needs extraDistr |
| C++ export without R; assembly reproducibility | `dev/tools/` scripts | portability; reproducibility | extended |
| Existence, uniqueness and limit results | dossier `## Details`, `ml-behaviour.tex`, reports | proof, where a proof is given | not a check |

From the repository root, use the local CPU slot lock and keep temporary files in
the worktree:

```sh
export TMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
```

Use `/tmp/umvl-cpu/run` for one thread and `/tmp/umvl-cpu/run4` for
four-slot builds and full suites; prefix long jobs with `nice -n 10`.
The [Justfile](../../Justfile) defines the recipes; the
[tier timing report](../research/evaluations/2026-10-31-test-tiers-report.md) records
the paired workload, counts and measured limitations. Targeted examples below
assume the build or generated package already exists. Family names in doctest
filters match case names, so inspect `--list-test-cases` before interpreting a
filtered run.

## Test tiers

Reserve all four slots with `/tmp/umvl-cpu/run4` for `just test`,
`just test-full`, `just cpp-test` and `just check-standalone`. Set
`JUST_TEMPDIR="$PWD/build/tmp"` as well as the temporary variables above.
`just regen` and `just test` disable pkgbuild's debug compiler flags;
regeneration recreates the generated source tree, preventing reuse of an old
`-O0` DLL. The workstation R compiler flags supply `-O2`; ccache remains active.

`UMVL_TEST_TIER=cran|dev|full` selects coverage. The common testthat helper
uses `cran` when `NOT_CRAN` is absent or false and `dev` otherwise;
`just test` explicitly defaults to `dev`, and `just test-full` selects `full`.
Invalid tier names are errors. R CMD check uses serial tests unless the caller
sets `TESTTHAT_PARALLEL=true`; local recipes use four testthat workers.
Edition 3 enables scheduling; the setup file retains edition 2 comparisons and
condition handling so the existing expectation contracts remain comparable.

| Tier | R workload | C++ ctest labels |
|---|---|---|
| `cran` | API, errors, faces, identities, frozen references, one golden fit per family; one deterministic truth parameter point and 2 value/1 quantile rows per group | `cran`: ordinary family/shared checks |
| `dev` | CRAN checks plus transformation/face/profile stress sweeps and conformance with none held, each single coefficient and all regular coefficients held; 3 deterministic truth points and 5 value/3 quantile rows | `dev`: ordinary family/shared checks |
| `full` | Every pre-tier expectation, exhaustive held subsets, all baseline truth points/rows, independent optimizer comparisons and large-sample replication | `full`: ordinary and expensive family shards |

All unlisted/new tests default to all tiers; use `skip_unless_tier("full")`
inside expensive test bodies, preserving inexpensive checks in the same file.
[Measured inventory](test-tier-inventory.csv) classifies every baseline test by
cost and purpose. Oracle comparisons and replication stress checks run in full;
Cheap API/identity/face checks remain in all tiers even in expensive family files.
The measured GB2/shifted-Pareto face sweeps, CMB profile grid and shared scale
transformation sweeps start in dev.
`UMVL_TEST_TIER=full just cpp-test` selects exhaustive C++ checks.
C++ source shards use complementary case filters for oracle, multistart and
replication/large-sample, exhaustive held-subset and slow-reference names;
full runs both. No fitter or fixture changes
are implied by tier membership. Audits (`quality`, no-long-double, standalone,
relations and truth regeneration) remain explicit commands.

## C++ tests

Both `just cpp-test` and `just cpp-test-no-long-double` compile every test
target with `-g -O2 -DNDEBUG` and GNU C++17, matching R's optimization,
assertion and language modes on GCC.
The latter disables extended intermediates and runs all ctest shards;
`UMVL_TEST_TIER=full just cpp-test` runs all shards in native precision.
The C++ truth checker evaluates every stored row in every tier; the R truth
checker samples rows in cran/dev and evaluates all rows in full.
For debugging, configure a separate CMake directory with
`-DCPPUNIV_TEST_OPTIMIZE=OFF` (and `-DBoost_INCLUDE_DIR=...` when needed),
then build and run ctest there. This switches every test target to `-g -O0`
with assertions enabled, without changing the default validation builds.
Test optimization flags are
private to the executables and do not affect header-only consumers.

`/tmp/umvl-cpu/run4 nice -n 10 just cpp-test` builds the family/shared binaries and monorepo diagnostic sweep, and runs the selected ctest shards. For a family, run, for example:

```sh
/tmp/umvl-cpu/run univariateML-cpp/build/tests/distribution_tests --test-case='exp*'
```

The [target list](../../univariateML-cpp/tests/CMakeLists.txt) globs all
family `test.cpp` files. A passing ctest target exits zero; doctest reports
cases, assertions, skips and any `may_fail` cases. Read the individual cases.

- Family fit goldens replay stored data and fit arguments from each
  `golden.json`. The samples are frozen fixtures (2026-11-07): `just goldens`
  only creates missing ones, and resampling one takes
  `UMVL_GOLDEN_IDS=<id> UMVL_GOLDEN_RESAMPLE=1`. Goldens carrying a
  `reference` block store independent 50-digit references from the README
  density ([reference.py](quality/reference.py), `just golden-reference`):
  damped Newton with a score and curvature check, closed forms for median and
  support endpoints, and exhaustive integer profiles with a finite range or
  an explicit tail bound. Every golden carries such a reference.
  Held and otherwise unrefined coefficients retain explicit reasons. Local
  score/curvature evidence, dossier uniqueness statements, and numerical
  eight-start oracle comparisons are recorded separately; numerical multistart
  agreement is not proof of global optimality. Laplace records the entire
  median interval and stores its upper endpoint as a representative.
  Shared [tolerances](../../univariateML-cpp/tests/test_support.hpp) measure
  default-fit coefficient error against these references, including native,
  double-intermediate and WASM observations. Wider allowances name the
  relevant stopping rule; they are empirical tolerances, not universal error
  bounds. See the [rollout report](../research/evaluations/2026-11-09-golden-ref-port-report.md)
  for measurements and their limits. Samples and fitting inputs remain frozen;
  do not regenerate them merely to make a test pass.
- Family cases also exercise held subsets, status and limit decisions,
  support, endpoints, analytic cases, independent optimizers and score
  residuals where implemented. For examples see [exp](../../univariateML-cpp/include/cppunivariate/distributions/exp/test.cpp),
  [gamma](../../univariateML-cpp/include/cppunivariate/distributions/gamma/test.cpp),
  [gb2](../../univariateML-cpp/include/cppunivariate/distributions/gb2/test.cpp),
  [sstd](../../univariateML-cpp/include/cppunivariate/distributions/sstd/test.cpp)
  and [hyper](../../univariateML-cpp/include/cppunivariate/distributions/hyper/test.cpp).
  A density integral or $p(q(p))$ is an internal identity; shared errors may
  cancel. A closed form can supply mathematical evidence only when its
  derivation and assumptions are established separately.
- The shared [truth consumer](../../univariateML-cpp/tests/test_support.hpp)
  compares log densities, both CDF tails and quantiles to `truth.json`;
  natural probabilities below its floor are omitted. Quantiles also accept
  oracle-provided brackets or adjacent lattice values. The C++, R and JS truth
  consumers also accept a finite continuous interior quantile that fails the
  existing forward rules when its own CDF, on the input's tail and scale,
  matches within relative `1e-12` (log inputs:
  `1e-12 * max(1, abs(input))`). The default tolerances
  allow binary64 arithmetic error; brackets handle representational ambiguity.
  The family call can override them. Two seeded parameter points receive
  empirical CDF checks with a five-binomial-standard-error allowance plus
  a finite-sample term. This is independent numerical evidence conditional
  on the declared density and route, not a proof of accuracy everywhere.
- The other [shared tests](../../univariateML-cpp/tests/) check BFGS on
  known objectives and gradient/reparameterization identities, RNG primitive
  support and moments, no-throw behavior on selected errors, CMP analytic
  submodels and high-precision fixtures, and tail arithmetic. Their test-local
  tolerances reflect the specific identity or numerical reference; absent a
  stated rationale, the original choice is unknown. Run one target with
  `/tmp/umvl-cpu/run ctest --test-dir univariateML-cpp/build -R '^tails_tests$' --output-on-failure`.
  These are finite examples, not universal error bounds or distributional tests.

## R tests and assembly

`/tmp/umvl-cpu/run4 nice -n 10 just test` regenerates the
package and runs testthat. To run one assembled family file after generation:

```sh
/tmp/umvl-cpu/run env NOT_CRAN=true UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions" PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local("univariateML-r", filter="dist-exp", stop_on_failure=TRUE)'
```

Use `filter="golden_regression"`, `"conformance"`, `"generated-dpqr"`,
`"parameter_infrastructure"`, `"restricted"` or `"scale_equivariance"`
for a shared file. These loops generally cover all families and do not expose
a family selector. Testthat reports failures, warnings, skips and passes.

- [Golden regression](../../bindings/r/tests/testthat/test_golden_regression.R)
  compares the rerun fit with stored estimates and likelihood. Tolerances are
  family-specific where numerical search warrants them, with cross-machine
  evidence documented there and in the C++ helper. Agreement is regression
  evidence only.
- [Conformance](../../bindings/r/tests/testthat/test_conformance.R) checks
  adapter formals, required diagnostics and holdable subsets against metadata
  and one golden sample per family. A successful held fit must retain held
  coefficients and have finite output and likelihood no worse than the golden
  beyond a per-observation allowance for finite-difference stopping error.
  Declared structural hold exceptions or limit messages can explain an error.
  It does not establish existence or optimality for other data.
- [Generated d/p/q/r](../../bindings/r/tests/testthat/test-generated-dpqr.R)
  samples rows from the same truth tables, checks dispatch, both tails and
  quantiles, and checks seeded draws for repeatability, length, support and
  integrality. It omits stats-dispatched families and missing tables; the R
  sampler checks are not distributional accuracy tests. Its numerical tolerances
  mirror the C++ consumer. Family [reference comparisons](../../bindings/r/tests/testthat/helper-reference.R)
  use frozen `reference.json` values from fGarch, logitnorm, nakagami and
  sads, with case-specific tolerances and provenance in
  [reference_values.R](../../bindings/r/scripts/reference_values.R). Quantile
  comparisons allow more rounding drift than direct d/p values. These are
  independent implementation comparisons, subject to reference defects; see
  [known issues](../KNOWN_ISSUES.md). Regeneration requires those optional R
  packages and rewrites tracked fixtures.
- [Restricted fits](../../bindings/r/tests/testthat/test_restricted.R),
  [parameter infrastructure](../../bindings/r/tests/testthat/test_parameter_infrastructure.R),
  family R tests and other testthat files check specific interface contracts,
  restrictions, diagnostics, likelihood identities and examples. Synthetic
  generator checks require the repository sources. [Scale equivariance](../../bindings/r/tests/testthat/test_scale_equivariance.R)
  transforms seeded samples and held endpoints and compares pulled-back fits,
  likelihoods or errors; search tolerances account for finite-difference and
  profile variation. This identity can detect unit sensitivity, not prove
  numerical accuracy. The [special-function comparison](../../bindings/r/tests/testthat/test_special.R)
  uses Rmath on a fixed grid; its case tolerances are in the test, with no
  separate rationale recorded.
- `just gen` and `just regen` run the generation checks: [d/p/q/r signatures](../../bindings/r/scripts/codegen_dpqr.R)
  must match coefficient order and types, while [wrappers](../../bindings/r/scripts/codegen_wrappers.R)
  check controls, required/default arguments and examples; [metadata](../../bindings/r/scripts/codegen_metadata.R)
  checks expression and shift mappings. Pass means generation completes;
  signature compatibility does not establish numerical correctness. No
  family selector is provided.

`/tmp/umvl-cpu/run4 nice -n 10 just check-standalone` builds an R source
package and checks it with repository reads denied by Linux Landlock; see the
[script](../tools/r-standalone-check.sh). It unsets `NOT_CRAN`, uses installed
system and user R dependencies, and includes vignettes. `just check` instead
checks from the checkout without vignettes. The isolated check establishes
package self-containment under that environment, not dependency independence.
Under CRAN conditions [special.R](../../bindings/r/tests/testthat/test_special.R)
uses `skip_on_cran`; optional `sn` comparison skips if `{sn}` is absent. Some
repository-source tests skip if dossiers or codegen are unavailable. The
[step-5 record](../research/evaluations/2026-10-30-r-standalone-report.md)
identifies the actual skips in that run. An absent fixture may silently omit a
family in generated d/p/q/r tests; count coverage separately.

## Extended numerical audits

- `/tmp/umvl-cpu/run nice -n 10 just quality exp` runs one family;
  omit `exp` for all. [check.py](quality/harness/check.py) reads one golden
  sample, dossier metadata and the symbolic density, preferring a local
  `spec.py` when present. It recomputes likelihood with mpmath, differentiates
  with SymPy, checks positive observed Hessian and standardized score or
  one-sided boundary score, and compares with eight seeded SciPy
  Nelder–Mead/BFGS starts. It holds endpoints, integer and golden-held
  parameters; kinks are excluded from derivative checks. The likelihood and
  optimizer gap tolerances are numerical comparison thresholds, not error
  bounds; their original calibration is not documented. Results and skips
  are written to `build/quality/`; a family passes if no component fails,
  including when a component is skipped. The baseline skipped derivative and
  oracle checks for dunif, hyper, nhyper and unif, and derivatives for Laplace
  at its nonsmooth observation: see the [baseline](../research/evaluations/2026-10-30-migration-baseline.md).
  An observed Hessian here is not model Fisher information.
- `/tmp/umvl-cpu/run nice -n 10 just dpqr-truth exp` regenerates the tracked
  `exp/truth.json` and its audit status JSON; omit the id for all. Inspect both
  diffs. [truth.py](quality/truth.py),
  [truth_reference.py](quality/truth_reference.py) and family `truth.py`
  routes use mpmath, symbolic parsing, high-precision integration or controlled
  sums, normalization and derivative checks, tail inversion and binary64
  brackets. The historical interior/default/fitted grid is retained. Attained
  coefficient-domain bounds (inclusive brackets) are appended without the
  historical eight-point cap, including integer designs and dependent bounds.
  Dependent inclusive domains are projected to compatible values; routes may
  select a valid conditional submodel (CMP's geometric face, for example).
  GEV/GPD also explicitly cover the attained fit restriction `xi = -1`.
  Every point includes support endpoints, adjacent binary64 probes, both log
  tails, and quantile inputs at probabilities zero and one in both scales.
  Finite continuous endpoints use the declarative one-sided density limit;
  atomic count limits use exact closed-form masses, retaining the family's
  finite design endpoints and collapsing unbounded atomic supports. These are
  independent mathematical references,
  never C++ or R evaluations. One-coordinate faces do not exhaust their
  intersections or all possible endpoint density regimes.
  Tables contain a semantic input hash (generator, reference, route, density
  and numerical metadata), with timings confined to the audit output. They
  are byte-reproducible within the recorded Python environment. The boundary
  [report](../research/evaluations/2026-10-31-truth-boundaries-report.md)
  records per-family additions, exclusions and unresolved dpqr findings.
  Exact local `truth_exclusions` declarations retain known failing references
  in `excluded_values` / `excluded_quantiles`, with reasons, outside active
  checks. They change neither the oracle nor its tolerances. Complete unsupported
  laws are retained in `excluded_parameter_points`, with their parameters,
  normalization and full reference rows, so R consumers do not try to sample
  an unsupported member. The public default and fitted sampler points cannot
  be excluded. The positive-truncated Poisson/binomial zero-rate limits use
  this complete-law archive; ordinary zero-rate conditional representatives
  remain active. The CMB size-cap
  workload filter retains both support endpoints and both natural-scale median
  quantiles as active checks; its remaining independently computed probes stay
  in those excluded arrays because every C++ call enumerates 10,000,001 terms.
  The [original methodology](../research/evaluations/2026-10-29-dpqr-truth/README.md)
  explains quadrature, omitted-tail and precision thresholds. That report's
  family failures and gaps are historical: later routes cover all 98 families
  in the [baseline](../research/evaluations/2026-10-30-migration-baseline.md).
  Truth regeneration exits nonzero on gaps. Both this route and the quality
  harness derive from dossier log densities or local overrides: a shared
  formula error can pass both. Higher precision does not independently verify
  the model definition. Python dependencies, including optional mpmath, SciPy,
  SymPy, NumPy and PyYAML, are listed in [pyproject.toml](quality/pyproject.toml).
  Run their own tests with `/tmp/umvl-cpu/run env PYTHONPATH=dev/validation python3 -m unittest discover -s dev/validation/quality -p 'test_*.py'`.
- The historical [slow sweep](../research/ml-behaviour-sweep.R) fits seeded
  ordinary and edge datasets and compares selected R density likelihoods with
  six transformed-coordinate starts. It retains valid starts as candidates,
  holds binomial/Zipf integer size, and flags escape magnitudes and likelihood
  gaps; the threshold is in the script. It has no family selector, writes a
  tracked report and `/tmp` results, and uses optional `extraDistr`; do not run
  it casually. Its selected density can be our own implementation, so the
  optimization is independent of the fitter but the objective need not be.
  A higher feasible likelihood is a counterexample only in the same parameter
  space; restrictions can explain a discrepancy. A clean sweep cannot certify
  globality or existence. Use its dated results with [ml-behaviour.tex](../research/ml-behaviour.tex).

For portability, `/tmp/umvl-cpu/run4 nice -n 10 bash dev/tools/standalone-export.sh`
exports the C++ tree, audits includes and builds/tests without R in PATH;
it requires Boost headers and has no family selector. The script uses a
private directory under the shared cache. `bash dev/tools/r-assembly-reproducibility.sh`
under the slot lock generates twice and diffs source content, excluding compiler
products; it does not prove reproducible tarball metadata. `python3 dev/tools/check_links.py`
checks local Markdown link targets, not remote links or mathematical claims.
See the [C++ export](../research/evaluations/2026-10-30-cpp-standalone-report.md)
and [R assembly](../research/evaluations/2026-10-30-r-standalone-report.md) records.

## Interpreting fitting evidence

Existence is a property of a sample and parameter space. Proofs and unresolved
conditions belong in each dossier's `## Details`, [ml-behaviour.tex](../research/ml-behaviour.tex),
and the [hold](../research/evaluations/2026-10-30-holds-bbinom-report.md)
and [limit](../research/evaluations/2026-10-29-p0b.md) reports. A finite
candidate, optimizer convergence or a small score does not prove existence.
A proof of globality needs appropriate inequalities, bounds or exhaustive
finite enumeration. An eight-start oracle can expose a better candidate but
cannot rule out a missed mode, boundary or limit. The slow sweep has the same
limitation on a broader, finite sample collection.

Optimizer convergence is a termination property. Some failed solves are
accepted only when a finite free-coordinate total score passes the documented
residual gate; see [wrapper failure handling](diagnostics.md#wrapper-failure-handling-rule-r6). This is a local
stationarity check, not a maximum or global certificate. The symbolic quality
score is standardized using observed curvature and is a different check.
Numerical accuracy is supported at tested points by independent comparisons,
closed-form cases and bounded routes; goldens alone establish stability.
Consistency is an asymptotic statistical property under stated assumptions;
none of these checks establishes it. Runtime is measured on a stated workload
and machine, not implied by accuracy or convergence.

## Known gaps

- Quality skips endpoint, integer and held coordinates, and some complete
  derivative/oracle checks as described above. The eight-start fit is limited
  to the one golden dataset. An independent global oracle over all free subsets
  and samples does not exist; [fitter inventory](../research/evaluations/2026-10-30-fitter-inventory.md)
  records further unknowns and uncertified searches.
- Truth tables vary up to eight historical points plus the README default,
  often holding
  design, integer and endpoints fixed; later coordinates can get little
  variation. Even with a table for every family, regions between grid points
  and attained parameter boundaries without dedicated closed-form cases remain
  unchecked. Compare [truth generator](quality/truth.py) with each family
  `test.cpp`; a table's endpoint observations are not a boundary-parameter grid.
  The [reviewed baseline inventory](../research/evaluations/2026-10-30-migration-baseline.md)
  found only quantile-endpoint evidence for, for example, fatigue and hnorm,
  and fitter-only closed forms for zip and ztpois. Those categories do not
  establish density closure at their boundaries.
- Holds are generically exercised on golden data only. Unbounded size/design
  searches and limit faces remain open in the [hold reports](../research/evaluations/2026-10-30-holds-cmb-report.md)
  and [hyper report](../research/evaluations/2026-10-30-holds-hyper-report.md).
- Sampler checks use two parameter points and selected body CDF thresholds;
  tail probabilities, dependence between draws and rare errors have weak or
  no coverage. R checks use only 32 draws per point for support/repeatability.
- CRAN skips the Rmath special-function comparison; optional `{sn}` and
  missing repository data cause the skips above. Inference coverage and test
  calibration are separate gaps; see the [inference plan](../roadmap/2026-10-30-inference-and-dqm.md).

## Run record

Record: source commit and any working-tree changes; exact commands; OS,
compiler, R, Boost/BH and Python with mpmath/SciPy versions; seeds and workloads;
tolerances and their code definitions; results including failures, permitted
failures, skips and missing coverage. Record whether references and fixtures
were regenerated and their diffs. Dated [evaluation reports](../research/evaluations/)
are historical evidence for their tested revisions, not live coverage claims.

## GitHub Actions CI

All three workflows run on every branch push, pull request and manual dispatch.
They have read-only repository permissions, cancel superseded runs of the same
workflow/ref, and publish no package or website. They do not compute coverage
percentages or replace the independent numerical and inference audits above.

- [core](../../.github/workflows/core.yaml): standalone optimized CMake build and
  full ctest labels on Ubuntu 24.04/GCC, macOS 15 **arm64**/Apple Clang and
  Windows 2022/MinGW UCRT64 GCC (not MSVC: the test build uses GCC-style flags).
  Boost 1.87.0 is downloaded with SHA256 verification and cached; compiler caches
  are partitioned by OS/architecture. No R is involved. This tests finite core
  cases, not all parameter values or other compilers/architectures.
- [r-check](../../.github/workflows/r-check.yaml): R release and r-lib dependency
  caching on those three operating systems; portable assembly equivalent to
  `just regen`, source build including vignettes/manual, and
  `R CMD check --as-cran` via rcmdcheck. Errors and warnings fail; notes remain
  visible in logs. The check uses the CRAN test tier, not the full oracle suite.
  Tarballs, check logs and environment records are retained for 14 days, including
  on failure. It does not test old R releases, R-devel or CRAN's infrastructure.
- [site](../../.github/workflows/site.yaml): Ubuntu, Emscripten 6.0.13 (matching
  the workstation), locked npm dependencies and Node 22.23.4; `just js-build`,
  `just js-test`, `just site-check`. Caches cover SDK downloads, npm downloads and
  `node_modules`. Python PyYAML/SymPy and R's BH are build dependencies. This
  installs Playwright's Chromium headless shell for the mandatory inference and
  theory browser checks, and supplies BH's header path to the standalone snippet
  compiler. It checks generated WebAssembly, static structure and those browser
  paths, not comprehensive accessibility, other browsers or deployment.

Before the R check, the golden regression file runs separately with
`UMVL_GOLDEN_DRIFT_FILE` set to a CSV path. Its ordinary assertions and tolerances
remain enabled. The CSV contains each family's maximum componentwise relative
error across estimates and log likelihood, plus its assertion tolerance; zero
references use absolute error, equal signed infinities count as zero, and
nonfinite mismatches count as infinity. This metric differs from testthat's
vector comparison and must not be read as its pass/fail criterion. These are
finite-sample comparisons with independent references, not universal accuracy bounds. TASK-129 must inspect
the first arm64/Windows artifacts before choosing any tolerance changes. Fit
errors may leave a partial CSV; the reporting runner requires one row per golden.

The portable scripts in [.github/scripts](../../.github/scripts/) are also local
entry points: `Rscript .github/scripts/assemble.R`, then
`Rscript .github/scripts/golden-drift.R` and `Rscript .github/scripts/r-check.R`.
Use the CPU slot lock for local runs. Hosted package installation, cache restore,
action execution and actual macOS/Windows results require the first push.

## Recording evidence and decisions

Moved from the agent instructions on 2026-11-07.

For substantive algorithmic or numerical changes, update the canonical
distribution dossier or shared algorithm note and leave a reproducible check,
or name the concrete validation gap. Keep documentation proportional to
complexity: a formula, conditions and reference may suffice for a closed form.
An involved fitter needs its objective, profile or search structure,
initialization, convergence and failure behavior, boundary comparisons,
computational tradeoffs and unresolved guarantees.

Preserve the reasons for material choices and changes. Retain significant
rejected or reverted approaches when they explain the current design, with
dated decisions and links to commits, derivations, reproducers or benchmarks.
Recover lasting conclusions from existing reports into current documentation;
keep the original evidence accessible. Important findings must not exist only
in chat, task summaries, ignored build output or temporary files.

An apparent overlap with Boost or another implementation is not evidence that
custom code is redundant. Before replacing it, establish its contract, protected
cases, rationale and comparison evidence. Record unknown rationale as unknown.
Keep layout moves separate from changes to numerical behavior.

Validation records state the tested source revision, commands, environment,
workloads or seeds, tolerances and results. Distinguish passes, failures,
permitted failures, skips and missing coverage. Golden regression, internal
identities, independent numerical comparisons and mathematical proofs support
different claims. Inference additionally needs checks of coverage or test
calibration under its stated assumptions; likelihood agreement alone is
insufficient.

## Changed-family checks

`/tmp/umvl-cpu/run4 just test-changed [base]` defaults to `master` and includes
committed changes since the merge base, working-tree changes and untracked files.
`just test-changed master --dry-run` explains selection without building. Family
folders in the core and common/R bindings select their family; reverse literal
header includes add dependent families transitively. Dossier edits count because
their front matter generates bindings and fixtures. Shared implementation,
generator, harness and build changes select all families and use the ordinary
recipes. Documentation outside dossiers selects nothing.

For a family selection, the runner incrementally builds the C++ distribution
binary and runs the selected tier's family ctest entries. It regenerates the R
package and runs those family files plus filtered golden, conformance, generated
d/p/q/r, scale-equivariance, frozen-model, diagnostics, input-check and restriction loops.
Hand-written cases within these shared files still run.
`UMVL_TEST_FAMILIES=id,id` supplies that filter to R and JS; unset means all.
JS runs selected generated family cases and retains the hand-written cross-family
regressions. JS registration groups run concurrently according to
`CMAKE_BUILD_PARALLEL_LEVEL` (default one), with one V8 pool thread per process.
`UMVL_TEST_NO_LONG_DOUBLE=1` or `--no-long-double` adds that C++ build.

This is an iteration shortcut, not merge validation. It never runs relations,
the site, independent quality oracles, or cross-family R files outside the
listed loops; a family-only selection also omits C++ shared targets. Include
relationships follow implementation headers; cross-family comparison tests that
include the umbrella header remain full-suite coverage. Run
the ordinary suites and any task-specific checks before finishing a change.

## Fitter log-likelihood seam

`seam-check.py` refits the frozen fit-audit datasets and the golden samples
through the current binding-neutral adapters, preserving their default controls.
It compares `logLik` with a compensated sum of scalar log densities at the
returned public coefficients. Disagreements beyond `1e-10 * max(1, |logLik|)`
receive independent mpmath evaluations at exact binary64 inputs, repeated at
higher precision. Closed-support comparisons use exact rational arithmetic.
A stopped fit is recorded separately; an audit timeout makes no existence claim.
Documented non-Lipschitz allowances require family-specific review of the saved
records; a positive seam discrepancy alone does not establish a fitter defect.

```sh
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py \
  --output build/scratch/seam-check/all.jsonl
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --family beta,binom \
  --output build/scratch/seam-check/selected.jsonl
```

`--case INDEX` restricts the archived replay (repeatable), `--timeout SECONDS`
sets the audit guard, and `--no-long-double` builds the double-width variant.
JSONL records retain data, holds, returned coefficients, density sums and
independent attributions; adjacent summary and provenance JSON record counts
and source identity. Reusing an output after changing sources is rejected; compiled probes also carry
the source identity, so simultaneous replays cannot overwrite one another.
The frozen input archive is
`dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz`.
This is a finite regression census, not a proof over all data or hold values.

## Fit snapshot

`fit-snapshot.py` records what every fitter returns on a fixed set of hard
inputs and judges nothing. It exists so that a refactor which must not change
any fit can be checked: take a snapshot before, one after, and compare
([consolidation plan](../design/consolidation.md), steps 3 and 4). It is not
evidence that any recorded result is right.

Inputs are deterministic: the frozen fit-audit archive (every hold subset,
$n$ from 2 to 12), each golden sample, seeded resamples of it ($n$ = 3 and 7),
and magnitude and tie classes built from fixed order statistics of the golden
sample (subnormal, tiny, huge, near-constant, wide spread, large offset, ties,
half ties, one, two and constant observations, and invalid samples: empty,
negative, zeros, NaN and infinite; integer and unit-interval families get
their own variants). The classes use the default fit only; hold
subsets are covered by the archive. A class can violate a family's support or
a held design value, and the resulting stop is recorded like any other.

Each case goes through the binding-neutral adapter `umvl_binding::fit_<id>`
with default controls, compiled with the shipped flags (`-O2 -DNDEBUG`,
gnu++17), one executable per family. A record holds the estimates, log
likelihood, iterations and flags, or the stop's kind, model, bounds, candidates
and message. Doubles are hexadecimal floats, so comparison is exact.

```sh
/tmp/umvl-cpu/run4 nice -n 10 python3 dev/validation/fit-snapshot.py \
  --output build/snapshots/base --jobs 4
python3 dev/validation/fit-snapshot.py --diff build/snapshots/base build/snapshots/new
```

`--diff` and `--outcomes` take a snapshot directory or a stored
`snapshot.jsonl.gz`. `--family a,b` restricts the run, `--no-long-double` builds the double-only
variant, and `--ignore message` leaves reworded messages out of a comparison.
A case that uses more than `--cpu-limit` CPU seconds (default 300) is recorded
as `timeout`; this is an audit guard measured in CPU time, not a fitter
status, and such records need not repeat exactly. Finished families are kept
and skipped on a rerun while headers, bindings, goldens, archive and the script
are unchanged; `provenance.json` records the source hash, commit, compiler and
flags. The baselines of master `8a702cc2` (native and double-only, 13,754 cases each,
two native runs identical) are stored with the
[baseline report](../research/evaluations/2026-11-12-fit-snapshot-report.md).

Where an adapter reports the shared outcome
([consolidation plan](../design/consolidation.md), step 4), a record also
holds it, the family's raw status and whether the adapter overrode a core
success. `--outcomes DIR` lists the families that report an outcome on every
case, the raw statuses the inputs reached, and every case whose outcome
disagrees with the kind the adapter emitted; it exits nonzero on a
disagreement.
