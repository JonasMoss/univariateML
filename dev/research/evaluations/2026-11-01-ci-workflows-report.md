# Monorepo GitHub Actions — TASK-107 / TASK-129 preparation

Date: 2026-11-01. Source: `90ec2153ea2a4272bb47259bb6091d2602a176e6`,
branch `codex/ci-workflows`, plus this uncommitted CI/reporting patch.
No commits, pushes, GitHub API calls, package installations on the workstation,
board edits, fitter changes, tolerance changes or fixture regeneration were made.

## Delivered workflows

The three obsolete R-CMD-check/pkgdown/coverage workflows are replaced by:

| Workflow | Hosted matrix and checks | Limit |
|---|---|---|
| [core](../../../.github/workflows/core.yaml) | Ubuntu 24.04 GCC, macOS 15 arm64 Apple Clang, Windows 2022 MinGW UCRT64 GCC; standalone Release CMake/Ninja build and all 126 full-label ctest shards | 45 minutes per job |
| [r-check](../../../.github/workflows/r-check.yaml) | R release on all three systems; assembly, generation, documentation, golden drift, source build and `R CMD check --as-cran` through rcmdcheck | 60 minutes per job |
| [site](../../../.github/workflows/site.yaml) | Ubuntu; Emscripten 6.0.13, `just js-build`, `just js-test`, `just site-check`, including mandatory Chromium browser checks | 30 minutes |

All run on pushes and pull requests on every branch and manual dispatch.
Concurrency cancels superseded runs for the same workflow/ref; platform matrices
use `fail-fast: false`. Permissions are `contents: read`; nothing is deployed.
Windows core deliberately uses MinGW, since the existing CMake test harness uses
GCC-style warning and optimization flags; MSVC remains untested.

Core Boost is the pinned 1.87.0 archive with SHA256
`af57be25cb4c4f4b413ed692fe378affb4352ea50fbe294a11ef548f4d527d89`.
Only its headers are extracted, without involving R. Boost and ccache are cached
by OS/architecture; MSYS2 caches its packages. r-lib caches R dependencies from
`bindings/r/DESCRIPTION` plus generation/check tools. TinyTeX and Pandoc support
manual/vignette builds. The site caches the SDK, npm downloads, `node_modules`
and Chromium; its lockfile and Node 22.23.4 govern JS dependencies. Python build
dependencies are PyYAML 6.0.3 and SymPy 1.14.0. BH provides the existing JS build's
Boost headers and an explicit header path for site snippet compilation.

Core artifacts contain ctest logs and compiler/configuration records. R artifacts
retain the tarball, complete check directory, session/compiler/platform records
and drift CSV for 14 days, including after failure. A golden-test failure does
not suppress the source check. Errors **and warnings** fail the R gate; notes
remain visible. No coverage percentage or deployment replaces the removed jobs.

The [portable assembly script](../../../.github/scripts/assemble.R) mirrors
`just regen` using R file operations in place of rsync. Each generator runs in a
separate R process, matching the Justfile. An initial attempt to source the
generators in isolated environments failed with `object 'dist_dir' not found`;
the subprocess implementation fixed this and completed local generation.
No settled generator was edited.

## Golden drift

The existing golden test has one opt-in `UMVL_GOLDEN_DRIFT_FILE` output mode.
Default behavior and all assertions remain unchanged. The separate CI runner
requires 98 family rows. Each row records the maximum componentwise relative
difference over estimates and log likelihood, with absolute difference at a zero
reference, zero for equal signed infinities, and infinity for other nonfinite
mismatches. This is an observation metric, **not** testthat's vector-comparison
criterion or an accuracy bound. Fit errors can leave a partial CSV; the runner
then fails rather than claiming complete coverage.

The [Linux CSV](2026-11-01-ci-workflows/golden-drift-linux.csv) contains 98 rows;
196 golden assertions passed without warnings or skips. Largest observed values:

| Family | Maximum componentwise relative difference |
|---|---:|
| sstd | 2.11627740848624e-6 |
| paralogis | 4.07843596339336e-8 |
| gev | 3.46322673037416e-11 |
| gompertz | 1.20526985563534e-12 |

All existing assertions passed. A componentwise number above the displayed
assertion tolerance (for example GEV) does not contradict that result: the
existing vector comparison normalizes differently. TASK-129 must examine actual
arm64 and Windows artifacts before calibrating anything. One dataset per family
is regression evidence, not a proof about optimizer accuracy or other samples.

## Local validation

Workstation: Ubuntu 26.04.1 x86_64, GCC 15.2.0, R 4.6.1, Rcpp 1.1.2,
BH 1.90.0.1, testthat 3.3.2, Python 3.14.5, PyYAML 6.0.3, SymPy 1.14.0,
Emscripten 6.0.13. The core CI-equivalent run used downloaded Boost 1.87.0;
`just cpp-test` used BH through the existing fallback. Hosted Ubuntu is 24.04
and Python is 3.12, so this is not an identical hosted image.
[Session](2026-11-01-ci-workflows/session.txt),
[platform and floating-point details](2026-11-01-ci-workflows/platform.txt), and
[R compiler command](2026-11-01-ci-workflows/compiler.txt) are retained.

Builds and suites ran as foreground commands under `/tmp/umvl-cpu/run4`, with
`nice -n 10`, at most four build/test workers and BLAS/OpenMP threads pinned to one.
Shared-slot waits were allowed to finish. Temporary files and logs were kept
under `build/tmp` and `build/scratch`; no process was left running.
The site dependencies were copied once from the authorized main-checkout
`site/node_modules`, without installing packages or modifying that checkout.

Commands used these environment settings:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CMAKE_BUILD_PARALLEL_LEVEL=4
```

Each compute block below was run under `/tmp/umvl-cpu/run4 nice -n 10`:

```sh
python3 .github/scripts/boost.py
cmake -S univariateML-cpp -B build/ci-core -G Ninja \
  -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
  -DBoost_INCLUDE_DIR="$PWD/build/boost_1_87_0"
cmake --build build/ci-core --parallel 4
ctest --test-dir build/ci-core --output-on-failure -j4 -L full
just cpp-test
Rscript .github/scripts/assemble.R
Rscript .github/scripts/golden-drift.R
Rscript .github/scripts/r-check.R
just test
just test-full
just js-build
just js-test
just site-check
# Final site repetition with the explicit header path used by CI:
export BOOST_INCLUDE_DIR=$(Rscript -e 'cat(system.file("include", package="BH"))')
just site-check
```

| Validation | Result |
|---|---|
| YAML parse with PyYAML BaseLoader, expected triggers, Python script parse | Passed for all three workflows |
| `actionlint` | Unavailable; not installed |
| `git diff --check` | Passed |
| CI Release core / Boost 1.87.0 | **126/126 passed**, 46.11 s ctest wall time |
| `just cpp-test` dev / BH 1.90.0.1 | **105/105 passed**, 33.18 s ctest wall time |
| Portable assembly / documentation | Passed after subprocess correction |
| Golden reporting | **196 passed**, zero failures/warnings/skips; 98 CSV rows |
| Source build with vignettes | Passed; approximately 3 MB tarball retained locally |
| `R CMD check --as-cran` | **Failed: 1 error, 2 warnings, 2 notes**; details below |
| `just test` dev | **19,865 passed**, 151 skipped, zero failures/warnings; 67.43 s test wall time |
| `just test-full` | **41,311 passed**, one optional sn comparison skipped, zero failures/warnings; 174.20 s test wall time |
| `just js-build` | Passed with Emscripten 6.0.13 |
| `just js-test` | **400/400 passed**, zero skipped; 61.65 s |
| `just site-check`, including final explicit-header repetition | Passed: 98 family pages, MathML/local assets/links, 98 C++ snippets compiled and linked, mandatory inference/theory browser checks |

Tests replay existing frozen data and seeds; none were regenerated. The existing
assertion tolerances remain authoritative. Ctest shard passes do not establish
universal numerical error bounds or prove existence/global optimality. The site
browser checks cover their specified paths, not every browser or accessibility
requirement. Workstation shell startup printed missing `~/.cargo/env` messages;
they did not fail the commands.

## Open: source-package check blockers

The full [00check.log](2026-11-01-ci-workflows/00check.log) is retained. Its test
summary was `FAIL 3 | WARN 3 | SKIP 215 | PASS 13514`. The three errors (and their
three file-open warnings) were documentation tests reading checkout-relative
`../../man/*.Rd` paths, unavailable in the R CMD check test directory:

1. `bindings/r/tests/testthat/test_argument_names.R:23`: `ml_check_args.Rd`.
2. `bindings/r/distributions/invpareto/test.R:110`: `mlinvpareto.Rd`.
3. `bindings/r/distributions/lomax/test.R:255`: `mllomax.Rd`.

The appropriate follow-up is to read installed help during tarball checks while
retaining source-help checks in development. Those files are outside this job's
stated ownership. A scope-extension question was raised; no approval had arrived
when this record was written, so they were not changed or skipped. Their ordinary
checkout tests passed in both dev and full. A fresh source check must verify a
repair; passing checkout tests cannot settle it.

The two check-level warnings also remain open:

- CRAN incoming feasibility: version 1.5.0 is already published; documentation
  URLs returned 404/502; one DOI URL should use `\\doi`. Version/URL/documentation
  changes require package-release work outside this job.
- Tests use `sn::dsn` without declaring `sn`. The comparison already skips when
  sn is absent; adding the intended optional dependency or removing/revising the
  optional comparison needs an authored package/test change outside this job.

The notes concern relative documentation paths and the HTML manual. They are
visible, not promoted to passes or hidden. The R workflow is expected to remain
red until its package errors/warnings are resolved. No thresholds or checks were
weakened to obtain a green badge. These concrete package defects prevent an
all-green R-check result; the other requested final suites finished green.

## First-push verification

Only Jonas's first push can verify hosted action execution, runner architecture,
Rtools/MinGW selection, arm64 Clang/FMA behavior, dependency availability, cold
and warm cache restore/save, Chromium/TinyTeX installation and artifact upload.
No GitHub run was requested or started here. In particular:

- Confirm `macos-15` actually reports arm64, and inspect its compiler records.
- Inspect all three golden CSVs even if comparisons fail; distinguish fit errors,
  partial coverage and tolerance failures. Keep the current tolerances pending
  measured TASK-129 conclusions.
- Resolve the package-check blockers above, then require clean hosted checks.
- Verify Emscripten setup exports `EMSDK` as expected by the existing build script;
  check locked npm/native packages and Chromium on a fresh Ubuntu image.

Local equivalents exercised assembly, source build/check, golden reporting,
CMake, JS and site commands. Platform package-manager/action setup, npm/Python/
browser installation, GitHub environment-file transport, artifact publication and
cache behavior were not emulated locally; doing so would require prohibited
local package installation or actual hosted runs.

Primary setup references reviewed: [runner labels/architectures](https://github.com/actions/runner-images),
[r-lib dependency working directory and package inputs](https://github.com/r-lib/actions/tree/v2/setup-r-dependencies),
[MSYS2 install-path output](https://github.com/msys2/setup-msys2#outputs),
[Emscripten setup/version/cache inputs](https://github.com/emscripten-core/setup-emsdk),
and [Boost archive checksum](https://archives.boost.io/release/1.87.0/source/boost_1_87_0.tar.bz2.json).

## Expected cost

There are seven jobs per complete event: three core, three R, one site. Budget
roughly 5–15 minutes per cold core job, 10–25 minutes per cold R job including
TeX/dependencies, and 5–15 minutes for site/browser setup and validation: roughly
50–135 aggregate runner-minutes before cache benefits. These are planning
estimates, not hosted measurements. The configured timeout ceiling is 345
aggregate runner-minutes. Superseded runs cancel; push and PR events can still
create separate runs. Cache sizes and billing depend on hosted policy and
repository entitlement, so no currency estimate is asserted. R artifacts retain
14 days; Boost headers, compiler caches, dependencies, SDK and browser consume
cache storage. First-run timings should replace these estimates.

## Report delivery

The requested artifact-pane `show` command was attempted after validation.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report and supporting records remain in this worktree;
nothing was copied into another checkout or outside the authorized worktree.
