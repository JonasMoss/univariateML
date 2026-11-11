# TASK-193: cheaper test runs

Baseline: `200b501b7eb2e30b6cdd224dfbb55fcd7f849179`; branch
`codex/test-scope`, 2026-11-11. No commits, board edits, installs, pushes or
changes to numerical family code, dossiers, expectations, tolerances or goldens.
The numerical tree and family bindings still match the baseline byte-for-byte.

`just test-changed` is implemented. The final JS suite retains all **615 test
names** and **248,155 truth checks**, and ran in **167.84 s versus 315.55 s**
before (1.88× faster, 46.8% less wall time). Default C++ and R dev/full suites,
workstation JS build/tests, and the site check passed. One unchanged
simbox no-long-double failure remains open below.

## Changed-family runner

[The runner](../../tools/test-changed.py) unions committed changes since the
merge base, staged changes, unstaged changes and untracked non-ignored paths.
Separate diffs preserve paths even when later changes cancel earlier ones;
rename detection is disabled so both old and new paths participate.

Family dossier/common-binding/R-binding folders and per-family R test paths
select their family. Reverse literal header includes supply transitive
implementation dependencies, including common-binding headers. Every selected
family has a printed reason. Shared implementation, generators, build files and
harness changes fall back to the ordinary recipes. Documentation outside
dossiers selects nothing, including R manual/vignette assets.

The selected plan builds the existing C++ distribution binary incrementally,
runs matching ctest entries in the current tier, regenerates the R package,
runs selected R family files plus nine filtered shared files, and builds/runs
JS. R shared coverage includes goldens, conformance, generated d/p/q/r,
equivariance, frozen models, diagnostics, input checks and restrictions.
Hand-written cross-family cases within those files still run. JS filters its
family loops and retains hand-written cross-family regressions. Leftover
`FIT_FAMILY`/`JS_FAMILIES` settings cannot narrow the runner's chosen scope.
Failures propagate to the exit status; ctest rejects an empty selection.

Use `just test-changed [base]`, `--dry-run`, and optionally
`--no-long-double` or `UMVL_TEST_NO_LONG_DOUBLE=1`. The base defaults to `master`.
Reserve four local slots as documented in the
[validation guide](../../validation/README.md#changed-family-checks).
This shortcut omits relations, the site, independent quality oracles,
unlisted cross-family R files, and C++ shared targets for family-only changes.
Umbrella includes in cross-family test translation units are full-suite
coverage; closure follows implementation headers.

## Selection demonstrations

The no-commit instruction took precedence over the scratch-commit request.
The [demonstration script](2026-11-11-test-scope-data/demonstrate-selection.py)
temporarily restored the baseline, made real working-tree edits, called the
loaded runner, and restored all task edits and file modes in `finally`.
No Git write command was used.

| Real isolated edit | Observed selection/result | Output |
|---|---|---|
| Comment appended to `bhatt/test.cpp` | Exactly `bhatt` | [leaf](2026-11-11-test-scope-data/select-leaf.log) |
| Comment appended to `invweibull/mle.hpp` | `invweibull`, `frechet`, `invexp`, `invburr`, `gb2`, `fpareto`, `genbeta`, with include-chain reasons | [delegates](2026-11-11-test-scope-data/select-delegate.log) |
| Comment appended to `core/span.hpp` | All 99 public families; ordinary-recipe fallback | [core](2026-11-11-test-scope-data/select-core.log) |
| Deliberate `CHECK(false)` appended to bhatt tests | Exactly one failing assertion; runner exit 8; R/JS not started | [broken test](2026-11-11-test-scope-data/broken-family.log.gz) |

After restoring infrastructure, an injected isolated bhatt path list exercised
the complete selected plan. Injection was necessary because the infrastructure
cannot be committed in this job; the preceding selections used actual Git
changes. C++ passed, R reported **315 passes / 10 skips**, and JS passed
**203/203**. All six commands returned zero. Timings: incremental C++ build
5.18 s, bhatt ctest 263.52 s, R regeneration/tests 75.92 s, JS build 130.30 s,
JS tests 120.36 s. These are coverage demonstrations, not controlled cross-host
speed comparisons. [Full output](2026-11-11-test-scope-data/selected-family-green.log.gz).

The final [five selector checks](2026-11-11-test-scope-data/selector-unit-final.log)
cover Git-layer union, leaf/binding selection, delegation, shared/doc paths and
filter inheritance. A final [real current-tree dry run](2026-11-11-test-scope-data/current-tree-dry-run.log)
correctly selects everything because this patch changes shared infrastructure.

## JS timing and test-set identity

All trials used workstation `run4`, `--v8-pool-size=1`, the same WASM payload
sources, and unchanged test callbacks/assertions. The runner sets Node's test
concurrency to `CMAKE_BUILD_PARALLEL_LEVEL`. Parallel runs distribute sixteen
registration groups; one-worker runs use one group, avoiding repeated serial
module initialization. `npm test` uses the same runner. Group truth diagnostics
are merged back to `build/scratch/truth-results.json`.

| Configuration | Wall s | CPU s (user + system) | Tests |
|---|---:|---:|---:|
| Before: one Node test process | 315.55 | 321.41 | 615/615 |
| Initial sixteen-group trial, four workers | 221.94 | 719.72 | 615/615 |
| Four-group trial, four workers | 268.44 | 863.67 | 615/615 |
| **Retained sixteen-group configuration, final run** | **167.84** | **589.93** | **615/615** |

The final comparison checks name multiplicities, not just the total:
[before names](2026-11-11-test-scope-data/js-before-names.txt),
[after names](2026-11-11-test-scope-data/js-after-names.txt),
[machine-readable comparison](2026-11-11-test-scope-data/js-comparison.json).
Truth diagnostics are identical, including all 248,155 checks and zero failures.
[Before log](2026-11-11-test-scope-data/js-before.log),
[final log](2026-11-11-test-scope-data/js-after.log).

Wall time improved under the four-slot reservation; total CPU cost increased.
The cause of the extra CPU was not isolated. These are single trials on a
shared workstation, so the spread between runs matters. An isolated optimizing-
compiler probe did not justify changing Node compiler flags; the retained
runner keeps the default compiler settings.

## R assembly and rebuild measurements

The [stage driver](2026-11-11-test-scope-data/measure-r-rebuild.sh) measures
non-overlapping production stages. Assembly includes header synchronization;
`gen` includes attributes, metadata, wrappers and links. An explicit package
load separates compilation from roxygen, so the `regen` column below is the
remaining roxygen work. These component timings are not inclusive recipe
dependency timings. The shared ccache was not cleared: “first” means first use
of that layout in this study.

All values are workstation wall seconds, through `run4`:

| Workload | Assemble/sync | Gen | Compile/load | Remaining regen | Tests |
|---|---:|---:|---:|---:|---:|
| Initial default unity layout | 1.71 | 18.60 | 151.38 | 8.36 | 365.96 |
| Warm unchanged unity rebuild | 2.15 | 27.56 | 3.57 | 5.85 | — |
| One-adapter edit, unity layout | 1.78 | 18.98 | 126.09 | 6.70 | — |
| First per-family layout | 2.64 | 22.07 | 1142.98 | 6.18 | — |
| One-adapter edit, warm per-family layout | 1.60 | 15.30 | 10.50 | 5.59 | — |

The literal final workstation `just test` took **701.28 s**, including normal
regeneration, and passed **23,257 expectations / 154 skips / zero warnings**.
[Log](2026-11-11-test-scope-data/final-r-dev-workstation.log.gz).
The initial instrumented test phase completed the same green test summary,
but its driver then hit a trailing parse error because I edited that file while
Rscript had it open. It is timing evidence, not a successful driver run. The
stabilized literal workstation run and both final simbox R tiers exited cleanly.

A reversible `static_assert(true, ...)` in the kumar R adapter forced a real
one-family compilation without changing fitting behavior. Distinct assertions
prevented the edited variants from reusing one another's cache entries.
The [study driver](2026-11-11-test-scope-data/measure-r-study.py) records the
baseline commit and uses `git show` for that adapter; no header-tree archive is
kept. [Stage data](2026-11-11-test-scope-data/r-stage-times.json),
[study totals](2026-11-11-test-scope-data/r-study.json).

| Edited rebuild strategy, excluding tests | Unity groups = 4 | Per-family, groups = 0 |
|---|---:|---:|
| Wipe and rebuild | 153.59 s | 33.02 s |
| Experimental incremental assembly | 146.56 s | 29.72 s |
| Incremental saving within the same layout | 4.6% | 10.0% |

The candidate generated a pristine projection and copied only changed contents
into the package, preserving objects. Header/layout changes invalidated objects
because R's make rules do not track transitive headers. The existing
[reproducibility checker](../../tools/r-assembly-reproducibility.sh) now also
runs production `codegen_links.R` and can compare an existing package via
`UMVL_REPRO_COMPARE`. Both layouts matched two independent pristine generations
byte-for-byte in generated sources, excluding compiler products:
[unity identity](2026-11-11-test-scope-data/r-identity-unity.log.gz),
[per-family identity](2026-11-11-test-scope-data/r-identity-family.log.gz).
This establishes source identity, not bitwise DLL identity.

**Recommendation:** retain wipe-and-rebuild and the four-group default.
For repeated one-family adapter work, use the already-existing opt-in
`UMVL_UNITY_GROUPS=0`: the warm edited rebuild was 4.65× faster in this trial,
but its first build cost 19.6 minutes. The incremental assembly gain within
either layout was too small to expose another mode. Its
[experimental script](2026-11-11-test-scope-data/experimental-r-incremental-regen.sh)
is retained only as reproducible study evidence. Default fits and assembly
behavior are unchanged.

## Final validation

Timings include builds/regeneration and exclude waiting for CPU locks.
All commands finished. Local computing used the shared slot wrappers; simbox
runs used its wrapper, with BLAS/OpenMP pinned and lowered scheduling priority.

| Check | Machine | Result | Wall s |
|---|---|---|---:|
| `just cpp-test` | simbox | 108/108 | 419.08 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 135/135 | 383.87 |
| `just test` | simbox | PASS 23257, SKIP 154, FAIL/WARN 0 | 204.11 |
| `just test-full` | simbox | PASS 46559, SKIP/FAIL/WARN 0 | 359.76 |
| Literal `just test` | workstation | PASS 23257, SKIP 154, FAIL/WARN 0 | 701.28 |
| `just js-build` | workstation | passed | 142.35 |
| Final `just js-test` | workstation | 615/615; names/truth identical | 167.84 |
| `just site-check` | workstation | passed | 125.55 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 135/135 | 794.81 |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 134/135; baseline failure below | 534.73 |

[Native C++ log](2026-11-11-test-scope-data/final-cpp-simbox.log.gz),
[R dev log](2026-11-11-test-scope-data/final-r-dev-simbox.log.gz),
[R full log](2026-11-11-test-scope-data/final-r-full-simbox.log.gz),
[site log](2026-11-11-test-scope-data/final-site-workstation.log).
The site preparation also rebuilds WASM; that run mistakenly reserved one slot
while its sub-build allowed four. Remaining build/JS runs used four-slot reservations.

## Open: baseline simbox binary64 swap equality

The simbox no-long-double run fails the unchanged bnbinom test
“alpha-free pair certificates transport the held value under swapping”, at
`bnbinom/test.cpp:373`: three exact `a.logLik == b.logLik` assertions fail.
Status and exchanged-parameter equality assertions pass. An isolated numerical
[probe](2026-11-11-test-scope-data/probe-bnbinom-swap.py) reproduces likelihood
differences of `-4.5474735088646412e-13`, `9.0949470177292824e-13`, and
`-1.5916157281026244e-12` for the three affected held values.
[Audit log](2026-11-11-test-scope-data/no-long-double-simbox.log.gz),
[probe output](2026-11-11-test-scope-data/bnbinom-swap-simbox.log).
The same audit passes on the workstation; both native-precision tiers pass on
simbox. This is an observed host/compiler combination difference, not a proof
that the compiler alone causes it.

Settling this requires a separate bnbinom numerical change or a decision about
the bitwise symmetry contract. Both family-code changes and expectation changes
are forbidden by this brief, so neither was made. This failure is preserved
rather than hidden by a skip or a relaxed tolerance.

## Existing repository link findings

The repo-wide Markdown checker reports 45 entries in seven historical files:
[full output](2026-11-11-test-scope-data/report-links-repository.log).
All seven source files match `200b501b` exactly
([hash evidence](2026-11-11-test-scope-data/link-baseline-proof.json)). Findings
include apparent parsing false positives for type/math expressions and site
routes, plus missing local literature PDFs and an unbuilt note PDF. No unrelated
historical file or checker behavior was changed. New/changed Markdown was
checked separately with zero broken destinations
([result](2026-11-11-test-scope-data/report-links-changed.log)).
