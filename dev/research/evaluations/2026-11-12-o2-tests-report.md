# TASK-206: optimized C++ test builds

Both C++ validation configurations now default to R-compatible `-g -O2
-DNDEBUG -std=gnu++17`. The optimized-only Burr and inverse-Burr failures
are fixed in their shared overflow helper. Final native C++ full,
no-long-double C++ full, R full and JS runs passed. No fitter algorithm,
default fit, golden or truth table changed. Work is uncommitted on
`codex/o2-tests`, based on `217ab8b533cac25ccc8eba8d66eccde1b376f0b4`.

## Build contract

The workstation used GCC 15.2.0, target `x86_64-linux-gnu`, and CMake 4.2.
The family executable's complete generated flag list is:

```text
-std=gnu++17 -Wall -Wextra -Wpedantic -g -O2 -DNDEBUG
```

The no-long-double executable adds
`-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. Standalone shared-test
executables also retain CMake's `-fPIE`. Include paths, fixture definitions
and precompiled-header options are recorded in the generated
[native](2026-11-12-o2-tests-data/flags-after-native.txt),
[diagnostics](2026-11-12-o2-tests-data/flags-after-diagnostics.txt) and
[no-long-double](2026-11-12-o2-tests-data/flags-after-no-long-double.txt)
flag files.

R reports `CXX17FLAGS=-g -O2` and `CXX17STD=-std=gnu++17`;
its actual binding compilation also supplies `-DNDEBUG`, `-fpic` and
`-fopenmp`. See the [toolchain record](2026-11-12-o2-tests-data/toolchain.txt)
and [actual R command](2026-11-12-o2-tests-data/r-compile-command.txt).
The core test harness matches the optimization, assertion and language
modes, retaining its existing warning and executable-positioning options;
it does not acquire R's binding-only dependencies. No fast-math flag,
`-march=native`, contraction override or per-file optimization reduction
was introduced.

`CPPUNIV_TEST_OPTIMIZE` defaults ON and applies privately to every test
executable, including diagnostics. Both Just recipes and the changed-family
runner explicitly enable it. CI now uses this option instead of CMake
Release's `-O3`. The Justfile comments, C++ README and validation guide
describe the optimized builds and the separate debugging path:

```sh
cmake -S univariateML-cpp -B build/cpp-debug -DCPPUNIV_TEST_OPTIMIZE=OFF \
  -DBoost_INCLUDE_DIR="$(Rscript -e 'cat(system.file("include",package="BH"))')"
cmake --build build/cpp-debug -j4
ctest --test-dir build/cpp-debug -R '^tails_tests$' --output-on-failure
```

Run these compute commands through the slot wrappers. This separate build
uses `-g -O0`, with assertions enabled; the helper test passed there.
Its [generated flags](2026-11-12-o2-tests-data/flags-debug.txt) confirm the
mode. It is outside the default validation recipes.

GCC's default is `-ffp-contract=fast`, but this target uses SSE arithmetic
with `-mfma` and `-mfma4` disabled. With the shipping optimization flags,
the multiply-add probe emits `mulsd` followed by `addsd`; explicit
`std::fma` calls the library. Thus implicit FMA contraction cannot apply
on this configured target, irrespective of the physical CPU's features.
The [target options](2026-11-12-o2-tests-data/gcc-options.txt),
[probe source](2026-11-12-o2-tests-data/contraction_probe.cpp) and
[disassembly](2026-11-12-o2-tests-data/contraction.txt) record this check.

## Optimized-only failures and cause

Full replays of the frozen baseline with `-g -O2 -std=gnu++17` failed
exactly two shards in each precision mode: `distribution_burr` and
`distribution_invburr`, one existing deep-tail assertion each. Both runs
passed the other 133 shards. These exploratory full replays had assertions
enabled; the separate baseline probe reproduces the same defect with
`-DNDEBUG`. No other optimized-only failures were observed in the full
C++ replays, R full run or JS run.

The probe found four affected entry points, all sharing
`special::log_softplus_product`:

| Exact call | Unfixed GCC 15 `-O2`, either precision mode | Fixed `-O2` |
|---|---:|---:|
| `pburr(1e100,1e-308,1e308,1,false,true)` | `-Inf` | `-230.25850929941012` |
| `pinvburr(1e-100,1e-308,1e308,1,true,true)` | `-Inf` | `-230.25850929941012` |
| `dburr(1e100,1e-308,1e308,1,true)` | `-Inf` | `-460.51701859881473` |
| `dinvburr(1e-100,1e-308,1e308,1,true)` | `-Inf` | `-5.5422333389287814e-12` |

The unfixed probe passes at `-O0` and at `-O2 -fno-inline`, and fails at
`-O1` and `-O2`, in both precision modes. Printing the helper in isolation
does not expose the failing inlined context. The
[reduced reproducer](2026-11-12-o2-tests-data/overflow_probe.cpp) includes
the positive, finite input check from that context. GCC's
[optimized tree](2026-11-12-o2-tests-data/overflow_probe.gimple.txt) replaces
`log(b)` with `log(Inf)` inside the `a*b == Inf` branch, although finite
`a` and finite `b` can produce an overflowing product. This is evidence of
an erroneous compiler range inference on the overflow path, rather than
an extended-precision or FMA dependency. The reproducer uses ordinary
floating-point operations and does not depend on undefined integer or
memory behavior.

The helper now checks `b > 1 && a >= DBL_MAX/b` before forming `a*b`,
returning `log(a)+log(b)` in that range. For positive `z`,
`softplus(z)=z+log1p(exp(-z))`; near or beyond `DBL_MAX`, the correction
is negligible at binary64 precision. Including the rounded division
boundary is safe for this approximation. Ordinary, negative and zero
products retain their previous path. This avoids both the overflowing
multiply and the compiler's faulty post-overflow inference.

At the requested inputs, the log tails are approximately `-100*log(10)`,
the Burr log density approximately `-200*log(10)`, and the inverse-Burr
log density approximately zero. The residual correction from softplus is
far below floating-point resolution. The measured log-tail error against
the first expression is about `5.54e-12`; the tiny-shape product and its
log-domain evaluation still involve floating-point rounding. This is an
analytic limiting-expression check plus numerical regression evidence,
not a new general accuracy certificate for the distributions.

Appended C++ and R regressions contain both exact requested probability
calls, nearby powers and both log densities. Shared helper regressions
cover overflowing, near-boundary, ordinary, negative and zero products.
Existing tests and tolerances were retained.

## Final validation and timings

All successful final runs used the workstation and the slot wrappers,
with four workers, one BLAS/OpenMP thread per worker and nice level 10.
The C++ full runs exercised all stored rows in 101 truth tables:
67,918 value rows, 57,264 quantile rows, 1,302 parameter records and 202
sampler points. The inventory includes hashes in
[measurements.json](2026-11-12-o2-tests-data/measurements.json).

| Final command | Result | Test wall time | Whole recipe wall time | Machine |
|---|---|---:|---:|---|
| `just cpp-test` | 108/108 shards | 415.53 s | 1101.71 s | Workstation, GCC 15 |
| `just cpp-test-no-long-double` | 135/135 shards | 390.87 s | 957.40 s | Workstation, GCC 15 |
| `UMVL_TEST_TIER=full just cpp-test` | 135/135 shards | 622.09 s | 622.38 s | Workstation, GCC 15 |
| `UMVL_TEST_TIER=dev just test` | PASS 25791; FAIL 0, WARN 0, SKIP 154 | 302.72 s | 663.86 s | Workstation, GCC 15 |
| `UMVL_TEST_TIER=full just test-full` | PASS 54309; FAIL 0, WARN 0, SKIP 0 | 1112.54 s | 1143.20 s | Workstation, GCC 15 |
| `just js-build` | exit 0 | — | 147.54 s | Workstation, Emscripten |
| `just js-test` | 618/618; no failures or skips | 249.90 s | 250.25 s | Workstation, Node |

R test times are the runner's elapsed times; C++ times are CTest's real
times. Queue waits are excluded. Initial fixed C++ runs also passed with
assertions enabled. After inspecting R's actual `-DNDEBUG` compile command,
all three C++ recipes were repeated with the final matching flags; only
those final measurements appear here. R already used `-DNDEBUG`, and the
late flag correction changed no numerical code used by R or JS.

Evidence scripts passed syntax checks, both final CTest logs retain 134
nonempty doctest assertion summaries, and `git diff --check` passed.
All 77 local destinations in the changed documentation and this report
resolve. The additional repository-wide Markdown link check reports 47
broken destinations in documents byte-identical to the baseline; these
unrelated existing links were left unchanged. Details are recorded in
`2026-11-12-o2-tests-data/documentation-check.txt`.

Before/after observations use the same workstation, four-worker wrapper
and recipe order. Before reconstructs the original commit with its
default `-O0`, ISO C++17 build (diagnostics alone used `-O2`). Build time
is `cmake --build`; recipe time also includes configuration and testing.

| Recipe | State | Build | Tests | Recipe | Ccache hits / misses |
|---|---|---:|---:|---:|---:|
| `just cpp-test` | Before | 146.72 s | 687.11 s | 834.30 s | 5 / 104 |
| `just cpp-test` | After | 686.04 s | 415.53 s | 1101.71 s | 4 / 104 |
| `just cpp-test-no-long-double` | Before | 177.72 s | 722.46 s | 900.74 s | 4 / 105 |
| `just cpp-test-no-long-double` | After | 566.40 s | 390.87 s | 957.40 s | 4 / 104 |
| `UMVL_TEST_TIER=full just cpp-test` | Before | 0.14 s | 961.56 s | 961.84 s | No compilation |
| `UMVL_TEST_TIER=full just cpp-test` | After | 0.18 s | 622.09 s | 622.38 s | No compilation |

The shared ccache was not cleared. The first two recipes were mostly
cache misses rather than warm-cache builds; each final build also logged
one `modified_input_file` fallback. Both full-native invocations reused
the already-built executables. These are single observations under
changing shared load, not controlled speedup estimates.

## Reproduction and remaining machine coverage

[replay.py](2026-11-12-o2-tests-data/replay.py) reconstructs baseline
headers with read-only Git operations and emits the optimization/precision
matrix. Run it through `/tmp/umvl-cpu/run`, with `--current` for the fix.
[Baseline](2026-11-12-o2-tests-data/probe-baseline.json) and
[fixed](2026-11-12-o2-tests-data/probe-fixed.json) outputs retain exact
commands. [run-timings.sh](2026-11-12-o2-tests-data/run-timings.sh) rebuilds
the baseline from its commit and measures either `before` or `after`
through `run4`. [run-validation.sh](2026-11-12-o2-tests-data/run-validation.sh)
records the R/JS commands and simbox fallback policy;
[collect.py](2026-11-12-o2-tests-data/collect.py) collects logs, counters,
fixture inventory and source hashes. Compressed raw recipe and CTest logs
are beside these scripts. No baseline source archive or binary is kept
in evaluation data.

Open machine coverage: simbox attempts, including the requested
no-long-double run, returned 75 (`simbox unreachable; run locally`). The
authorized workstation fallbacks all passed, including the required
GCC 15 no-long-double run. GCC 13 execution on simbox remains unverified;
closing that cross-machine check requires reachable simbox and a run of
`just cpp-test-no-long-double` there. No numerical defect remains open
from this audit.
