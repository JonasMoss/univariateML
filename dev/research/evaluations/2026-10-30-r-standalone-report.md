# Standalone R package — TASK-101.5

2026-10-30; branch `codex/r-standalone`, base `2d9cea413455ea3d07f41801ef16209ce36c8f34`.
Results apply to this uncommitted worktree. No Git writes, board edits, dependency
installs, sub-agents or simbox. Existing algorithms, defaults and tolerances retained.
Local Linux x86_64, R 4.6.1, GCC 15.2.0, CMake 4.2.3; all builds/tests used
`/tmp/umvl-cpu/run4` (four slots, `nice -n 10`, BLAS/OpenMP one thread).
`TMPDIR` and `JUST_TEMPDIR` were `build/tmp`; isolated-check temporary files
necessarily used its external directory, avoiding the quota-limited `/tmp`.

## Fixture packaging

Assembly copies 98 README front-matter blocks and 215 JSON fixtures into
`tests/testthat/fixtures/<id>/`: 98 golden, 98 truth, eight reference,
four fixed-scale golden and seven outlier files, all byte-identical to the
canonical files. Tests are not installed.
The first test helper supplies that directory when `UMVL_DISTRIBUTIONS_DIR`
is unset; an explicit dossier override still works. The restriction-constant
test falls back to the package's installed C++ headers. The synthetic generator
test exercises the repository's codegen scripts, so it runs from a checkout
(`just test`) and skips in tarball checks. DESCRIPTION now declares yaml and
withr in Suggests; the tests already used both.
All C++ sources, canonical goldens/truth tables and generators are
unchanged from HEAD; no TASK-78 numerical repair was required.

Final `univariateML_1.5.0.tar.gz`: **2,878,502 bytes (2.75 MiB)**, below 5 MB.
The generated source tree and independently extracted tarball both passed
`grep -RInE` for Unix/Windows absolute paths: **zero matches**.
Repository-relative paths remain in comments and in reference provenance
(`generated_by`); independence is established by the Landlock-isolated check
below, not by the scan.

Review change (Claude, before merge): the job first added a `project_package.R`
step that rewrote repository paths in shipped comments ("bindings/r/" became
"R binding sources: ") and copied the three codegen scripts into the test
fixtures with their default paths rewritten. Both were removed: generated
files should name the source they come from, and testing repository
generators belongs to the repository. Figures in this report are from the
job's run unless marked as rerun below.

Rerun after the review change (same commit base, local, `/tmp/umvl-cpu/run4`):
`just check-standalone` **`Status: OK`**, `[ FAIL 0 | WARN 0 | SKIP 3 | PASS 39018 ]`
(the third skip is the generator test, "no repository codegen"), 399.7 s wall;
absolute-path scan empty; the generator, restriction and golden-regression
test files pass from the checkout; the reproducibility script's two
generations are byte-identical; `just readme-table` leaves the README tables
unchanged under the pinned `C.UTF-8` locale.

## Independent check

Procedure: `/tmp/umvl-cpu/run4 just check-standalone`; existing `check` retained.
`dev/tools/r-standalone-check.sh` builds with `R CMD build .`, copies the tarball
outside the repository, unsets dossier/package overrides and NOT_CRAN, and runs
`R CMD check --no-manual --timings` from `/var/tmp/umvl-r-check.JLNt95`.
Linux Landlock denies repository reads; a failed read of CLAUDE.md verifies this
before checking. User R libraries and system tools remain available.

Final status, verbatim:

```text
Status: OK
[ FAIL 0 | WARN 0 | SKIP 2 | PASS 39027 ]
```

No ERROR, WARNING or NOTE. Skips: optional `{sn}` absent (`test-dist-sn.R:135`)
and the existing CRAN skip (`test_special.R:2`, 13 assertions run in local suite).
Both vignettes built and rebuilt successfully; none skipped. Their HTML contains
MathML and no MathJax script; vignette RNG seeds are fixed at 20261030.
An initial no-vignette check reported `Status: 1 ERROR` and
`[ FAIL 1 | WARN 1 | SKIP 2 | PASS 39019 ]`: missing dossier `mle.hpp`.
The installed-header fallback fixes that package-level assumption.

## Reproducibility

`/tmp/umvl-cpu/run4 bash dev/tools/r-assembly-reproducibility.sh` independently
assembles, synchronizes headers, generates R/Rcpp and roxygen documentation twice.
**936 source files byte-identical**, also identical to production generation;
`diff -ru` exit 0, empty diff. Compiler intermediates are removed before comparing.
Locale `C.UTF-8` and timezone UTC are pinned; discovery order is stable, private
paths projected, and no generation timestamps are embedded. Filesystem mtimes
and R CMD build archive metadata are outside this source-content comparison;
bit-identical tarballs are not claimed. An unsuccessful source-only roxygen
loader experiment was replaced by the production loader, which loads datasets.

## Timings and existing workflows

Final check wall time **402.03 s** (6 min 42 s; warm compiler cache, build excluded).
The test R process took **359.494 s** (5 min 59 s). Per-file summed testthat
elapsed times, excluding setup overhead: conformance 80.126 s; sged 59.490 s;
sstd 20.957 s; generated d/p/q/r 19.649 s; tlambda 19.271 s; trgamma 15.435 s;
golden regression 12.705 s. Conformance and sged are the first profiling candidates.
This is a substantial default check workload; no tests were reduced or moved
for speed. These are workstation timings, not CRAN-machine calibration.

| Command | Result | Whole-recipe wall seconds |
|---|---|---:|
| `just cpp-test` | 6/6 targets; test execution 158.42 s | 243.03 |
| `NOT_CRAN=true just test` | FAIL 0 / WARN 0 / SKIP 1 / PASS 39040 | 514.06 |
| `just quality` | 98/98 families | 102.42 |

C++ compilation retained warnings in protected `special/tails.hpp` (gamma_p/q
inline declarations) and `dlaplace/dpqr.hpp` (misleading indentation); all linked
and passed. These remain TASK-78-owned cleanup candidates, not package failures.
Logs, empty path scan/diff and fixture audit: `build/scratch/`; full independent
check, test timings CSV and testthat transcript retained in the external tree above.
Numerical regression checks establish agreement, not new mathematical guarantees.
