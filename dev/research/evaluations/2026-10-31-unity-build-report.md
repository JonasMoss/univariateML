# R package unity compilation (TASK-105, 2026-10-31)

## Scope and method

Source base: `13312d054b9c594306f0d6a376eb0256428565c6`, branch
`codex/unity-build`, plus the uncommitted assembly changes described below.
No fitter, d/p/q/r generator, core numerical implementation or golden fixture
was changed. Measurements use this workstation, not simbox: Intel Core
i7-1355U, Ubuntu Linux x86-64, GCC 15.2.0, R 4.6.1.

Every clean measurement runs serially under `/tmp/umvl-cpu/run`, with reduced
scheduling priority (the wrapper plus the explicit `nice -n 10` produce nice
19), with `MAKEFLAGS=-j1`, OpenMP/BLAS threads pinned to one, and ccache disabled.
A freshly copied assembled package has all `.o` and `.so` files deleted before
`R CMD INSTALL --preclean`. A private `R_MAKEVARS_USER` sets `CXX17FLAGS = -g -O2`
and uses `bindings/r/scripts/benchmark_compile.py` as the compiler launcher.
The launcher runs the system `g++` directly and records elapsed seconds and
child maximum resident set size for each invocation. Linux RSS values are KiB;
reported MiB divide these by 1024. `/usr/bin/time -v` measures the complete
install, including linking, byte compilation and load checks. CPU-slot queue
time and package generation are excluded from clean install times.

These are single observations under shared workstation load, not controlled
repeated performance estimates. Peak memory is measured, not a bound for all
compilers or platforms. Validation establishes observed regression behavior,
not a mathematical proof of numerical equivalence under all compiler options.

## Assembly design

`bindings/r/scripts/unity_build.R` runs after the existing d/p/q/r generation.
It keeps every `src/dist_<id>.cpp` intact so Rcpp's attribute scanner still sees
the original declarations. Generated `unity_XX.cpp` files include groups of
those sources; generated `OBJECTS` in Makevars selects the group objects,
`RcppExports.o` and the non-family objects. Family objects are not also linked.
The same selection is written to Makevars.win.

Families are sorted by filename and distributed round-robin among the groups.
Before including any family body, each group preloads the union of its
`common/` and `cppunivariate/` includes at global scope, then its R-shell
includes. Thus all library declarations are parsed before Rcpp/Rmath defines
function-name macros; include guards protect the later repeated includes.
There are no family-specific branches, no changes to the settled generator,
and no namespace wrapping of standard or third-party headers. The current
adapters contain no file-static helper collisions. Existing shared helpers
remain protected by their header guards and family entrypoints retain their
unique exported names.

`UMVL_UNITY_GROUPS=0 just regen` selects the original per-family layout.
Positive values select the number of groups, capped at the number of families;
invalid values fail assembly. This is an assembly-time switch, so changing it
requires regeneration. Generated Makevars dependencies name each group's family
sources for direct incremental installs. The development `just test` workflow
still regenerates the whole package and relies on ccache to reuse unchanged
translation units.

## Clean-install results and decision

| Layout | Families per group | Compiled TUs | Install wall time (s) | Largest TU peak (MiB) |
|---|---:|---:|---:|---:|
| Per family (`0`) | 1 | 100 | 481.47 | 634.0 |
| **4 groups (default)** | 24–25 | 6 | **137.01** | **963.6** |
| 8 groups | 12–13 | 10 | 168.95 | 798.3 |
| 16 groups | 6–7 | 18 | 181.97 | 771.0 |

All four reported installs completed successfully, including staged and final
load checks. Counts include `RcppExports.cpp` and `special_tests.cpp`. The
4-group layout reduces measured clean-install wall time by **71.5% (3.51×)**.
Its largest compiler process remains below 1 GiB, roughly half the requested
2 GB ceiling. Eight groups save 165 MiB but add 31.94 s; sixteen save 193 MiB
but add 44.96 s relative to four groups. Four is the best measured clean-build
tradeoff on this machine. The override supports a more conservative memory
choice or per-family debugging without editing sources.

The four selected group compile times were 23.35, 31.34, 27.37 and 31.72 s;
peak RSS was respectively 915.7, 936.1, 917.1 and 963.6 MiB. This modest
imbalance does not justify introducing a manually maintained family schedule.

### Per-group measurements for the other variants

| Layout | TU | Compile wall time (s) | Peak RSS (MiB) |
|---|---|---:|---:|
| 8 groups | `unity_01.cpp` | 15.99 | 745.8 |
| 8 groups | `unity_02.cpp` | 18.95 | 745.4 |
| 8 groups | `unity_03.cpp` | 21.27 | 737.1 |
| 8 groups | `unity_04.cpp` | 23.31 | 798.3 |
| 8 groups | `unity_05.cpp` | 17.82 | 746.3 |
| 8 groups | `unity_06.cpp` | 16.90 | 736.7 |
| 8 groups | `unity_07.cpp` | 14.97 | 737.0 |
| 8 groups | `unity_08.cpp` | 14.59 | 749.1 |
| 16 groups | `unity_01.cpp` | 9.05 | 634.6 |
| 16 groups | `unity_02.cpp` | 8.27 | 587.5 |
| 16 groups | `unity_03.cpp` | 8.23 | 602.6 |
| 16 groups | `unity_04.cpp` | 15.26 | 746.8 |
| 16 groups | `unity_05.cpp` | 8.41 | 635.5 |
| 16 groups | `unity_06.cpp` | 8.57 | 633.0 |
| 16 groups | `unity_07.cpp` | 11.22 | 745.4 |
| 16 groups | `unity_08.cpp` | 9.96 | 684.3 |
| 16 groups | `unity_09.cpp` | 8.39 | 639.4 |
| 16 groups | `unity_10.cpp` | 12.48 | 771.0 |
| 16 groups | `unity_11.cpp` | 11.77 | 760.9 |
| 16 groups | `unity_12.cpp` | 8.24 | 638.8 |
| 16 groups | `unity_13.cpp` | 10.66 | 715.5 |
| 16 groups | `unity_14.cpp` | 9.77 | 677.5 |
| 16 groups | `unity_15.cpp` | 9.36 | 665.5 |
| 16 groups | `unity_16.cpp` | 8.64 | 639.9 |

### Baseline's ten slowest translation units

| TU | Compile wall time (s) | Peak RSS (MiB) |
|---|---:|---:|
| `RcppExports.cpp` | 15.36 | 547.6 |
| `dist_sstd.cpp` | 8.63 | 634.0 |
| `dist_gb2.cpp` | 7.35 | 587.9 |
| `dist_fpareto.cpp` | 7.34 | 599.9 |
| `dist_sged.cpp` | 7.26 | 578.5 |
| `dist_std.cpp` | 6.83 | 545.0 |
| `dist_pig.cpp` | 6.71 | 546.5 |
| `dist_pareto3.cpp` | 6.31 | 546.2 |
| `dist_dgamma.cpp` | 6.27 | 545.9 |
| `dist_pareto4.cpp` | 6.11 | 545.4 |

### Reproduction and retained evidence

From the worktree root, reserve one slot for each clean install. Set the
worktree-local temporary directories specified in AGENTS.md, then:

```sh
# Repeat for groups=0,4,8,16; regeneration itself is outside the timing.
UMVL_UNITY_GROUPS=$groups /tmp/umvl-cpu/run just regen
cp -a univariateML-r build/scratch/measure-package
find build/scratch/measure-package -type f \( -name '*.o' -o -name '*.so' \) -delete
mkdir -p build/scratch/measure-library
printf 'CXX17 = python3 %s/bindings/r/scripts/benchmark_compile.py\nCXX17FLAGS = -g -O2\n' \
  "$PWD" > build/scratch/measure.Makevars
export R_MAKEVARS_USER=$PWD/build/scratch/measure.Makevars
export UMVL_COMPILE_LOG=$PWD/build/scratch/measure-compile.jsonl
: > "$UMVL_COMPILE_LOG"
CCACHE_DISABLE=1 /tmp/umvl-cpu/run nice -n 10 \
  /usr/bin/time -v -o build/scratch/measure-time.txt \
  R CMD INSTALL --preclean --library="$PWD/build/scratch/measure-library" \
  build/scratch/measure-package
```

Remove the previous `measure-package` before each copy. Unset
`R_MAKEVARS_USER` and `UMVL_COMPILE_LOG` before normal development commands.
The actual run reused the same fully generated baseline family sources for
all layouts: before each variant it restored the authored Makevars files,
deleted prior unity sources, and called `configure_unity_build()` with that
variant's count. This avoids repeated documentation-time package compilation
and keeps the compared family sources identical.

Detailed local evidence is retained under `build/scratch/`:
`baseline`, `unity4`, `unity8`, `unity16` each have `-compile.jsonl`,
`-install.log`, `-time.txt` and a `-package/` tree. These are ignored scratch
artifacts; the lasting conclusions and measured results are recorded here.

A setup attempt queued before assembly completed aborted at the package-copy
step, before compilation. The first 4-group install then exposed a Makevars
default-target bug: a group dependency became make's first target, so only
that group compiled and loading failed because no shared library was linked.
Adding `all: $(SHLIB)` before the dependency rules fixed it. The failed
`unity4-first-attempt-*` evidence is retained and excluded from successful
measurements. No adapter rename or numerical change was needed.

## Incremental rebuilds and final validation

All validation commands run sequentially under one `/tmp/umvl-cpu/run4`
reservation, at nice 10, with `MAKEFLAGS=-j4` and one BLAS/OpenMP thread per
worker. The `just test` commands use four testthat workers; the standalone
check runs its R tests serially. The private benchmark compiler configuration
is not used for these commands. Existing test files, fixtures and tolerances
are unchanged.

For each layout, touch `bindings/r/distributions/gamma/adapter.cpp` and time
`just test`, including regeneration and testing, with ccache already warm.
The initial baseline regeneration populated the per-family cache; a separate
four-group `just test` populated the selected layout's cache before its touch.
CPU queue time is excluded.

| Development command | Whole-command wall time (s) | FAIL / WARN / SKIP / PASS |
|---|---:|---|
| `UMVL_UNITY_GROUPS=0 just test`, after touch | 89.36 | 0 / 0 / 151 / 19818 |
| Default `just test`, cache-populating run | 138.38 | 0 / 0 / 151 / 19818 |
| Default `just test`, after touch | 90.70 | 0 / 0 / 151 / 19818 |

The timestamp-only incremental result is essentially unchanged (+1.34 s in
these single runs). Assembly recreates the generated source tree; unchanged
source contents still reuse cached objects. This observation does **not**
measure the cost of a content edit. A content edit in gamma's adapter
invalidates its 25-family group (`unity_02.cpp`) rather than one family TU.
Their measured clean compile costs were 31.34 s versus 3.26 s for
`dist_gamma.cpp`; these are workload proxies, not an additional timed
`just test` experiment with changed code. Edits to shared headers can affect
more groups. The per-family switch preserves the finer rebuild option.

`just check-standalone` passed with **Status: OK**, including the repository
read-denial probe, package installation, compiled-code checks, examples,
tests and vignette rebuilding. Whole-command time was 129.88 s; the isolated
`R CMD check` itself took 100.69 s. The independent check tree is retained at
`/var/tmp/umvl-r-check.CknY6C`, with copied evidence in
`build/scratch/standalone-00check.log` and `build/scratch/check-output.log`.

The final `just test-full` completed successfully in **308.94 s** overall
(290.834 s within the test runner): **FAIL 0 / WARN 0 / SKIP 1 / PASS 41260**.
The single skip is `test-dist-sn.R:136`, whose optional `{sn}` reference
package is not installed. No dependencies were installed to remove this skip.
The development skips are 150 full-tier exclusions and the same optional
reference-package skip.

The final requested sequence was the successful default-layout touch
`just test`, then `just check-standalone`, then one `just test-full`.
All commands finished; there are no outstanding test runs or validation
failures. The earlier baseline and cache-populating development runs also
passed. Logs and whole-command timings are retained as
`baseline-touch-test.log`, `unity-warm-test.log`, `unity-touch-test.log`,
`final-standalone.log`, `final-full.log` and their corresponding `*-time.txt`
files in `build/scratch/`. `git diff --check` also passed.

The counts above are those actually observed on this worktree's source base,
not substituted from the master baseline in the task brief. The
per-family and selected-layout development counts agree exactly. No test,
fixture, default fit or golden result was edited.

No Windows compiler/check run was performed. The generated Windows object
selection follows the same layout; measured memory and runtime conclusions
apply to the Linux environment above.

## Tested source fingerprints

The source base plus these SHA-256 values identifies the uncommitted build
changes. No commit was made.

```text
323c34c4cedca1c3076ca6448ccbdbce2a835cdee4f0a79dfcce22f4a1f9f5a1  Justfile
de1fdbb745cc24295f857b95a1e97881f1289e0c9e1ed42c18b6859db335e4bc  bindings/r/scripts/assemble_package.R
13da8706ab1cfb8aa45341058f5fb8c3aaad2aec532cd795b0f5ddd8c848c7ff  bindings/r/scripts/unity_build.R
d49096c9f2d61fb575d874701f0971f2daa2f6a4b19c3b15a8cbfb06d305b4ac  bindings/r/scripts/benchmark_compile.py
```
