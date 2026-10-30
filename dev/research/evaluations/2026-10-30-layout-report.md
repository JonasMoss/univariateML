# Directory migration — TASK-101.3

2026-10-30; base `a1de750e2367bafb98ac771b1fcf5a5f13fcd471`, branch `codex/layout`.
All runs green locally: `/tmp/umvl-cpu/run4`, `nice -n 10`, four build threads, BLAS/OpenMP one thread, `TMPDIR=$PWD/build/tmp`.
`NOT_CRAN=true` for R tests. No commits, Git writes, installs, board edits or simbox; R CMD check deferred by Jonas (earlier interrupted check excluded).

## Move map

| Old | New |
|---|---|
| `cppunivariate/` | `univariateML-cpp/` (public includes and namespace unchanged) |
| Family `adapter.cpp` and every `*.R` | `bindings/r/distributions/<id>/` |
| `univariateML/` except generated package | `bindings/r/` |
| `univariateML/package/` | `univariateML-r/` (generated, ignored) |
| `python/` | `bindings/python/` |
| `STATUS.md`, `KNOWN_ISSUES.md` | `dev/STATUS.md`, `dev/KNOWN_ISSUES.md` |
| `roadmap/`, `notes/` | `dev/roadmap/`, `dev/research/` |
| `quality/` | `dev/validation/quality/` |
| `scripts/`, `design/` | `dev/tools/`, `dev/design/` |

All 1,652 tracked inputs retained; `docs/` untouched. Old CMake build: `build/scratch/pre-cpp-build/`. [Full move map](2026-10-30-layout-evidence/move-map.tsv).

## Non-move edits, grouped by file

- `README.md`: new landing page and installation links; `LICENSE.md`: exact copy of the R license.
- `Justfile`: paths, dossier defaults and `PYTHONPATH`; all ccache exports/launcher retained.
- `.gitignore`: generated package, binding build artifacts, C++ build and note paths.
  `bindings/r/.Rbuildignore` unchanged: its license exclusion remains applicable.
- `bindings/r/scripts/assemble_package.R`: output/dossier paths; adapters/tests read from binding folders.
- `bindings/r/scripts/{codegen_metadata,codegen_wrappers,goldens,readme_table,reference_values}.R`:
  output/dossier defaults and path comments; `load_distributions.R` and `codegen_dpqr.R` unchanged.
- `bindings/r/tests/testthat/test_parameter_infrastructure.R`: codegen working-directory path.
  Other edited R tests/adapters contain only path references in comments.
- `dev/validation/quality/{harness/check,truth,truth_report}.py`: repository roots and paths.
  Run as `PYTHONPATH=dev/validation python3 -m quality ...`.
- `dev/tools/simbox-test.sh`: checkout discovery/exclusion paths; `check_links.py`: new link checker.
- `CLAUDE.md`, `AGENTS.md`, core `README_SCHEMA.md`: current layout and split binding paths.
  CMake, dossiers, binding docs, development records and report generators: path/link edits only.
  Historical Git-object paths remain historical; report generators' root discovery was adjusted.
- This report and its evidence directory record validation. All **263** edited existing files
  are individually enumerated in the [edit inventory](2026-10-30-layout-evidence/source-edits.tsv),
  with every exact edit grouped by file in the [patch](2026-10-30-layout-evidence/source-edits.patch).

## Pre/post checks

| Target | Pre | Post |
|---|---:|---:|
| ctest | 6/6 | 6/6 |
| bfgs: cases / assertions | 6 / 21 | 6 / 21 |
| sample | 1 / 10,204 | 1 / 10,204 |
| distribution | 915 / 1,281,358 | 915 / 1,281,358 |
| no_throw | 3 / 56 | 3 / 56 |
| cmp | 8 / 619 | 8 / 619 |
| tails | 4 / 147 | 4 / 147 |
| testthat FAIL / WARN / SKIP / PASS | 0 / 0 / 1 / 39,040 | 0 / 0 / 1 / 39,040 |
| quality | 98/98 | 98/98 |

Only R skip: optional `{sn}` absent (`test-dist-sn.R:135`). All C++ cases pass, none skipped.
Every family status matches: likelihood 98 pass; derivatives 93 pass/5 skip; oracle 94 pass/4 skip.
[98-family comparison](2026-10-30-layout-evidence/quality-comparison.tsv). These are numerical checks, not proofs.

## Equivalence and links

All **618** protected files (`*.hpp`, `test.cpp`, goldens and family truth files) are byte-identical to `a1de750`; all **98** README front-matter blocks are byte-identical.
[Protected SHA-256s](2026-10-30-layout-evidence/protected-sha256.log) and [base hashes](2026-10-30-layout-evidence/base-sha256.log).
The historical `5f11fb9` manifest has the same **185 inherited differences** as before the move: [complete list](2026-10-30-layout-evidence/historical-sha-differences.txt).
Of **719** package files, **697** are byte-identical, including every binary; **22** differ only in paths (checked by reversal):
`R/{ml00_metadata,ml02_wrappers}.R`; `man/{mlht,mlinvtrgamma,mltrgamma,mlzib,mlzmbinom,mlztbinom}.Rd`;
`src/dist_{invparalogis,invpareto,invtrgamma,lomax,paralogis,trgamma}.cpp`;
`tests/testthat/test-dist-{binom,frechet,gev,ht,sstd}.R`;
`tests/testthat/{test-generated-dpqr,test_golden_regression,test_parameter_infrastructure}.R`.
[Every package difference](2026-10-30-layout-evidence/package.diff).
`python3 dev/tools/check_links.py`: **274 Markdown/R Markdown files, 237 local destinations, zero broken**; checks Markdown/R Markdown and HTML asset destinations,
including repaired historical links; excludes code fences, remote URLs and fragment validation.
Relocated truth-module CLI help/import paths checked without regenerating fixtures.

## Build cache

`CCACHE_DIR=~/.cache/ccache/univariateML`, checkout-root `CCACHE_BASEDIR`, `CCACHE_NOHASHDIR=1` retained.
[Pre ccache -s](2026-10-30-layout-evidence/pre-ccache-stats.log): 101/202 hits (50.00%);
[post](2026-10-30-layout-evidence/post-ccache-stats.log): 303/508 (59.65%); cumulative counters, not reset.
Wall seconds (whole recipes, including tests):

| Recipe | Pre | Post | Unchanged rebuild |
|---|---:|---:|---:|
| `just cpp-test` | 172.34 | 259.69 | 158.87 |
| `NOT_CRAN=true just test` | 640.27 | 489.72 | — |
| `just quality` | 99.82 | 92.84 | — |
| `just regen` | 26.73 | 25.78 | 26.31 |

Unchanged C++: no compiler calls (hit rate N/A), 6/6 tests pass. Unchanged regen: **101 hits, 0 misses, 100%**, cumulative 404/609 (66.34%). Post-move C++ path relocation incurred
104 misses; both post-move R compilations had 101/101 hits. [Times/counter deltas](2026-10-30-layout-evidence/build-times.tsv).

## Handoff

TASK-107: adapt `.github/workflows` (unchanged here) to new paths/cache and run the deferred R CMD check in CI.
TASK-101.4: remove R/BH discovery fallback, verify system Boost-only standalone export/build/tests,
and move remaining statistical adapter behavior (notably log-gamma) into the core; check RNG boundaries.
No numerical changes or new availability claims were made; TASK-78-owned numerical files need no repair from this move.
