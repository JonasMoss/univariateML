# Generated JavaScript binding: TASK-124.4, 2026-10-31

**Incomplete.** This patch implements generated d/p/q/r for all 98 families,
but migrates fitting only for exp, gamma and gb2. The remaining 95 adapters
still own their R fit logic. TASK-124.4's all-family fitting and ranking goal
has not been achieved. This report must not be read as an all-family fit pass.

Source: `codex/js-all`, base `e0433d4cde9fbd8c3b6ef29a24a6b8607d992b68`,
plus the uncommitted patch. No commits, board edits, package installs, fixture
changes, numerical core changes or writes to other worktrees.
Canonical scope: [site and JS plan](../../roadmap/2026-10-30-site-and-js-binding.md).

## Shared fitting layer

`bindings/common/fit.hpp` defines held values with a separate supplied flag,
and a result containing named estimates, logLik, iterations, held names and an
error message. This distinguishes a supplied NaN from a free coefficient.
The three migrated family headers contain validation, core calls and the
existing status/diagnostic mappings, without Rcpp, exceptions or printing.
Exp explicitly recognizes `ExpStatus::Unbounded` with today's R diagnostic.
That branch anticipates the parallel core fix; the all-zero diagnostic test
here exercises the base revision's nonfinite-result path.
No numerical algorithm or convergence rule is changed or newly certified.

Their R adapters retain the original signatures/defaults and only convert
arguments/results. `assemble_package.R` copies the shared tree to `src/common`
and its R conversion helper to `src/common_fit.h`. Exp's R result still omits
iterations. JavaScript throws only after a returned error envelope, outside C++.
Shared control defaults are checked against the R formals during generation.
Metadata-generated JS boundary checks retain the exponential data diagnostic;
R public wrappers retain their existing data checks.

The other 95 families have no JS fit method. `families` lists evaluation
coverage; `fitFamilies` separately lists shared fitting coverage. No fallback
returns an unchecked optimizer result, placeholder estimates or a success.
Warning/restriction handling for those 95 adapters remains an explicit
migration gap, as do their Node golden, hold-subset and diagnostic tests.

## Generation and evaluation tests

`bindings/js/generate.py` uses PyYAML and canonical README coefficient order.
For every family it checks all four inline signatures: argument names, count,
double coefficients and bool flags, excluding the RNG type check as in R's
`codegen_dpqr.R`. It emits embind wrappers, registrations and the registry;
shared fitting wrappers are discovered from the common headers. It verifies
held coefficient order, documented controls, and R/shared default agreement.
No per-family JavaScript implementation or npm dependency was introduced.
Generated C++ lives under ignored `build/js/generated/`.

The Node audit covers every family's truth-table entries and sampler points.
Log d/p tolerances are `1e-10 * max(1, abs(truth))`; natural tails above
`1e-300` use relative `1e-10`; quantiles use native relative `1e-9`, oracle
absolute tolerance or bracket/lattice acceptance. Each sampler point uses
20,000 draws, seed `20261029 + point`, same-seed repeatability and the native
five-standard-error CDF bound plus `1/n`. No tolerances were loosened.
The three migrated fits retain pilot golden, every-held-subset and diagnostic
checks. Passing tables are numerical evidence, not a mathematical proof.

Final Node run: 106/106 tests, 225,908 scalar truth/sampler comparisons;
**every failing entry: none**. Pilot truth checks are not counted twice.
Machine-readable complete failure list: `build/scratch/truth-results.json`.
Unchanged R family tests after migration: exp 42, gamma 108, gb2 611 assertions;
all passed, with no warnings or skips. An initial R build failed because a
shared include was not relative to its header; corrected before those passes.

## Module layout

Matched-source O2 builds, five Node import/instantiation trials per module:

| Module | wasm B | wasm gzip B | glue B | first / median load ms |
|---|---:|---:|---:|---:|
| exp only | 32,016 | 17,958 | 29,335 | 7.83 / 7.57 |
| GB2 only | 276,584 | 116,383 | 31,470 | 14.94 / 10.21 |
| all 98 evaluations + three fits | 875,141 | 240,411 | 31,478 | 14.87 / 19.80 |

Retained one aggregate for this private evaluation/ranking prototype: 249,717 B
including gzip glue, one cached import. Exp-only transfers substantially less.
This is a provisional layout: the other 95 fitters' size/load cost is unmeasured.
Local import times reuse process/OS/engine caches; they are not network or
fresh-process browser measurements. Full trials: `build/scratch/module-layout.json`.
Reproduce with `/tmp/umvl-cpu/run4 bash bindings/js/measure.sh`; this restores the full module
and reruns Node tests. No measurement of a module containing all 98 fitters is claimed.

## Explorer

The generated registry makes every family page eligible for an explorer.
Integer families use mass bars, step CDFs and probability-mass histograms.
Dossier integer flags reject fractional coefficients. Required design and
threshold coefficients are numeric inputs, always held, without sliders.
Closed coefficient-domain endpoints are accepted, including tnorm infinite
bounds via text inputs; many coefficients stack. Singleton integer windows are
padded. Windows exceeding 2,000 lattice points show a message rather than
silently omitting interior masses; zero masses remain safe on the log plot.
The three available fits retain simulation, held inputs and pasted-data AIC.
The remaining 95 explorers explicitly say fitting is unavailable, and ranking
records that diagnostic. Coefficient-dependent support fitting is consequently
not implemented; their evaluation uses the core quantile/density APIs.
No support-expression resolver, all-family ranking or all-family fit coverage
is claimed. Only the explorer and its data wiring changed in the site.
The snippet generator now follows shared headers when core calls leave R adapters.

## Final validation

All reported suite runs completed through `/tmp/umvl-cpu/run` or `run4`, nice 10,
BLAS/OpenMP one thread, Emscripten four build workers; temp/cache/logs under
`build/`. Browser checks are pinned to one CPU. No simbox use.
Environment: Linux x86-64, GCC 15.2.0, R 4.6.1, Node 22.22.1,
Python 3.14.5, Emscripten 6.0.13, Chromium 151.0.7922.34.
Tested uncommitted source hashes: `build/scratch/js-all-source.sha256`.

- `just cpp-test`: 6/6 targets passed, 159.90 s.
- `NOT_CRAN=true just test`: FAIL 0, WARN 0, SKIP 1, PASS 39040;
  the permitted skip is the absent optional `{sn}` package.
- `just js-build` and `just js-test`: passed; 106/106 Node tests, zero skips.
- `just site-check`: passed, including 98 compiled/linked C++ snippets.
- `python3 site/scripts/browser-smoke.py`: passed; no page errors or external
  requests, interaction/AIC/mobile/no-JS checks. Mobile GB2 screenshot inspected.
- `bindings/js/browser-all.mjs`: all 98 default explorers rendered, discrete
  CDF steps and absence of sliders for required coefficients checked; no failures.
- `git diff --check`: passed. Logs use `build/scratch/*-final.log` or
  `js-final-{build,test}.log`; complete browser rows: `browser-all.json`.

Initial site generation failed when it searched only R adapters; corrected in
its data wiring. The first browser audit failed only tnorm infinite defaults;
corrected, rebuilt, and the complete 98-page audit passed. No failures remain
in the implemented checks. Missing 95 fit migrations/tests/ranking coverage
remain missing work, not permitted failures, skips, or completed validation.

Pane preview was attempted with the prescribed `show` helper but returned 403:
this worktree is outside its readable roots. No viewer configuration was changed.
