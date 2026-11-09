# TASK-137: faster slash scale-score evaluation

The all-free fit takes 14.7% less time on the frozen 500-observation golden
sample and 15.7% less on the 500-observation Cauchy sample. Repeating the golden
sample to 10,000 observations gives a 14.8% reduction. On 10,000 distinct Cauchy
observations, the measured reduction is 2.4%; both versions exhaust the default
certificate budget. Every recorded result, status, budget flag, evaluation
count and subdivision depth agrees exactly across all 44 before/after pairs.

All required final tests passed. No golden, default control, density, binding,
shared core or certificate rule changed. The production changes are confined to
the slash fitter, an appended regression test and its dossier explanation.

## Change and mathematical justification

Slash already has a closed-form density. With `z = (x-m)/s` and `t = z*z/2`,
its density away from zero is
`(1-exp(-t))/(sqrt(2*pi)*s*z*z)`, with the continuous value at zero.
The log-scale score contribution is `g(t) = 1-2*t/expm1(t)`.
Neither the fitter's density nor its score calls incomplete gamma functions.
The small-argument polynomials and saturation threshold were already present.

The free-scale profile already cached log distances at each location. This
change shares that evaluator with the certificate's closest/furthest residual
vectors, whose scale brackets and root bisections previously recomputed
observation logarithms on every score pass. A cache lives only as long as its
fixed location or residual vector. The additional certificate storage is linear
in sample size; caches are not retained on the subdivision stack.

The arithmetic argument is stronger than an approximation argument:

1. Let `d` be the existing overflow-safe log-distance calculation. The old
   helper returns `d-log_s`. Caching its value with `log_s=0` is exact in
   binary64, including zero distances represented by negative infinity. The
   later subtraction of `log_s`, multiplication, and ordered score sum are
   unchanged. This also retains the existing opposite-sign overflow handling.
2. Previously, the evaluator formed `t=exp(min(logt,4))`. When `logt>=4`, this
   gives `exp(4)>42`, selecting the exactly represented contribution `1`.
   Returning that contribution before exponentiation preserves it. Below 4,
   the original operations and switches remain in place. The comparison uses
   the old clamp value, rather than introducing a rounded `log(42)` cutoff.
3. The same scores therefore drive the same brackets, scale roots, bounds and
   certificate decisions. No grid, warm start, refinement, work cap, objective
   margin, collapse comparison or failure gate changed.

This preserves the existing certificate scope for every held subset. Those
certificates use exact-arithmetic bounds conditional on computed comparisons,
with the stated practical floating-point margin. This work does not introduce
an outward-rounded enclosure or a new parameter-distance guarantee. The
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md#conditional-search-certificate)
states the existing objective-gap bounds and their limits.

The appended test compares cached scores with the original expression on
extreme finite observations, overflowing opposite-sign distances, zeros,
subnormal-scale arguments, NaN and neighboring values at the new shortcut.
The 44 paired fits provide numerical regression evidence, rather than an
independent proof of global optimality. Their comparison checks round-trip
result fields, status and budget flag, all instrumented counts, and maximum
depth. It does not independently compare every `StopDetail` field or iteration
counter; their generating paths are unchanged, and existing witness tests pass.

A coarser initial grid was also investigated and rejected. On the golden
sample it moved the returned location from `-0.010630181350186806` to
`-0.010630367258083119`, about `1.86e-7`, despite a tiny likelihood difference.
That exceeds the requested coefficient accuracy. The final implementation
retains the original grid; the
[rejected trial](2026-11-09-slash-perf-data/rejected-grid-golden500.csv) is kept
as evidence.

## Measurement protocol

Measurements used simbox, GCC 13.3.0, C++17 and `-O2`, under the shared machine
runner's nice priority and single-thread numerical-library settings. Controls
were the unchanged C++ defaults, `tol=1e-9` and `maxit=500`. Held values were
`mu=-0.010630182705389644` and `sigma=1.1437689913361664` for every dataset.

`golden500` is the frozen slash sample. `golden10000` repeats it twenty times;
it measures observation-count scaling with the same distinct locations and is
**not an independent 10,000-observation sample**. The Cauchy samples use Python
`Random(137)` and `tan(pi*(u-0.5))`: the first 500 draws and the next 10,000
draws, respectively.

The harness instruments private header copies, leaving production code free of
counters and timers. Compilation and data preparation are outside timed fits;
reported fit times include instrumentation. All-free before/after pairs run
concurrently as two single-thread workers: three repetitions at 500 and one
at 10,000. Held subsets run sequentially, alternating variant order, with
three repetitions at both sizes. Tables use medians where repeated. The
shared host and single large all-free pair limit timing precision, especially
for the 2.4% Cauchy result and microsecond held fits.

The [canonical 88 rows](2026-11-09-slash-perf-data/paired-simbox.csv) include
every repetition. An
[initial mixed-load run](2026-11-09-slash-perf-data/paired-concurrent-initial.csv)
is excluded: fast held fits overlapped a slower all-free fit in that protocol.
The held measurements were rerun with the controlled serial protocol above.
`paired-controlled.csv` is a retained copy of the canonical output. These
absolute timings should not be compared directly with the earlier census's
different machine and measurement setup.

## Before/after times

Times are seconds. “Held” names fixed parameters; “none” is all-free.
`Budget` means `IterationFailure` with `budget_exhausted=true`, not an MLE or
a nonexistence conclusion. Both versions have the listed stop.

| Sample | Held | Before | After | Before/after | Stop |
|---|---|---:|---:|---:|---|
| golden500 | none | 4.454637 | 3.799225 | 1.173 | Ok |
| golden500 | mu | 0.000321371 | 0.000314282 | 1.023 | Ok |
| golden500 | sigma | 0.034903674 | 0.033744080 | 1.034 | Ok |
| golden500 | both | 0.000019789 | 0.000020397 | 0.970 | Ok |
| Cauchy500 | none | 4.210420 | 3.550941 | 1.186 | Ok |
| Cauchy500 | mu | 0.000294640 | 0.000286646 | 1.028 | Ok |
| Cauchy500 | sigma | 0.032417212 | 0.031781646 | 1.020 | Ok |
| Cauchy500 | both | 0.000018638 | 0.000017484 | 1.066 | Ok |
| golden10000 | none | 80.770636 | 68.781090 | 1.174 | Ok |
| golden10000 | mu | 0.005200539 | 0.005385118 | 0.966 | Ok |
| golden10000 | sigma | 0.639599712 | 0.636651701 | 1.005 | Ok |
| golden10000 | both | 0.000190019 | 0.000194035 | 0.979 | Ok |
| Cauchy10000 | none | 543.951433 | 530.791773 | 1.025 | Budget |
| Cauchy10000 | mu | 0.008376229 | 0.008236098 | 1.017 | Ok |
| Cauchy10000 | sigma | 11.628035595 | 11.677665100 | 0.996 | Budget |
| Cauchy10000 | both | 0.000263515 | 0.000243440 | 1.082 | Ok |

Holding sigma or both parameters bypasses the changed scale solver. Their
small timing differences are measurement variation, not evidence of a speedup.
Holding mu uses one conditional scale solve and gains little from the cache
change, since that profile already cached its distances.

## Evaluation counts and attribution

Counts below are identical before and after, in every repetition. “Observations”
counts observation contributions across score passes, not distinct data.
“Bounds” counts residual upper-bound calls. Depth is measured from the initial
grid cells. The budget rows include the attempted 64,001st cell; exactly
64,000 cells are processed before the budget stop.

| Sample | Held | Profiles | Score passes | Observations | Cells | Bounds | Depth |
|---|---|---:|---:|---:|---:|---:|---:|
| golden500 | none | 8310 | 743370 | 371685000 | 8011 | 2240 | 11 |
| golden500 | mu | 1 | 65 | 32500 | 0 | 0 | 0 |
| golden500 | sigma | 4274 | 0 | 0 | 4137 | 0 | 4 |
| golden500 | both | 1 | 0 | 0 | 0 | 0 | 0 |
| Cauchy500 | none | 8184 | 730057 | 365028500 | 7909 | 2182 | 12 |
| Cauchy500 | mu | 1 | 64 | 32000 | 0 | 0 | 0 |
| Cauchy500 | sigma | 4304 | 0 | 0 | 4137 | 0 | 4 |
| Cauchy500 | both | 1 | 0 | 0 | 0 | 0 | 0 |
| golden10000 | none | 8310 | 743410 | 7434100000 | 8011 | 2240 | 11 |
| golden10000 | mu | 1 | 65 | 650000 | 0 | 0 | 0 |
| golden10000 | sigma | 4274 | 0 | 0 | 4137 | 0 | 4 |
| golden10000 | both | 1 | 0 | 0 | 0 | 0 | 0 |
| Cauchy10000 | none | 83733 | 5915186 | 59151860000 | 64001 | 1534 | 6 |
| Cauchy10000 | mu | 1 | 63 | 630000 | 0 | 0 | 0 |
| Cauchy10000 | sigma | 80804 | 0 | 0 | 64001 | 0 | 4 |
| Cauchy10000 | both | 1 | 0 | 0 | 0 | 0 | 0 |

The all-free timer decomposition identifies repeated score sums as the main
cost. The score timer is nested within the profile timer where applicable;
these columns must not be added together.

| Sample | Profile before / after (s) | Score before / after (s) | Density before / after (s) |
|---|---:|---:|---:|
| golden500 | 2.762 / 2.797 | 4.227 / 3.552 | 0.098 / 0.099 |
| Cauchy500 | 2.631 / 2.632 | 3.991 / 3.317 | 0.094 / 0.094 |
| golden10000 | 51.687 / 51.877 | 76.699 / 64.514 | 1.800 / 1.786 |
| Cauchy10000 | 504.382 / 506.070 | 522.001 / 507.222 | 14.578 / 14.529 |

Before the change, score passes occupy about 94–96% of all-free wall time;
density passes occupy about 2–3%. Time outside `Profile` falls by about 38–42%
on these measurements. The existing cached profile itself is essentially
unchanged, while certificate score reductions become cheaper.

Distinct observations still produce roughly eight initial grid knots per
observation. Each knot's scale solve scans all observations, giving quadratic
initial work. The distinct Cauchy10000 grid alone exceeds the default
`128*500=64000` certificate-cell budget. Moderate observed depths (6–12 for
all-free fits) show that very deep subdivision is not the principal large-n
cost here. The unchanged budget stops are an intentional limit of this
arithmetic-preserving improvement. Failed-result payloads in the raw records
are regression data, not reported estimates.

## Final validation

Each command completed in the foreground with exit status zero. Workstation
suite commands used `/tmp/umvl-cpu/run4`; R and long C++ suites used
`/tmp/umvl-cpu/simbox-run`. Local builds/tests used two workers and the final
simbox C++ suites one; R used four. Numerical libraries were pinned to one
thread. Shared-slot queue time is excluded from the durations below.

| Check | Machine | Result |
|---|---|---|
| Targeted slash doctest | workstation, GCC 15.2 | 21 cases, 33584 assertions passed |
| `just cpp-test` dev | workstation, GCC 15.2 | 106/106; 213.38 s test time |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129; 449.54 s |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129; 347.24 s |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129; 398.34 s |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 22218; 92.907 s |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 45053; 341.875 s |
| `just site-check` | workstation | passed; 99 family pages and 99 C++ snippets |
| Paired comparison | workstation, one-slot runner | 44/44 pairs, including every repetition |
| `git diff --check` | workstation | passed |

The full R tier includes the slash golden and held-parameter checks; all remain
within their existing tolerances. In the paired golden500 all-free records,
both versions return `mu=-0.010630181350186806`,
`sigma=1.1437689913744995`, and `logLik=-1481.5640876706589` exactly.
No fixture was regenerated. JS build/test is not triggered: no shared core,
binding code or message string changed. The site check covers the dossier
change. All numerical and validation items are complete. The site browser test
regenerated a historical screenshot; its baseline contents were restored so
that unrelated generated change is excluded from this job.

## Reproduction and retained evidence

The [baseline header](2026-11-09-slash-perf-data/baseline-mle.hpp) is the slash
fitter at `dcf0832a`, retained because simbox mirroring excludes `.git`.
[benchmark.py](2026-11-09-slash-perf-data/benchmark.py) generates private
instrumented copies and deterministic samples under `build/scratch/`.
[run_benchmarks.py](2026-11-09-slash-perf-data/run_benchmarks.py) builds and
runs the complete protocol, checkpointing completed fits for machine drops.
[compare.py](2026-11-09-slash-perf-data/compare.py) checks every result/count
and writes the per-sample median-row CSVs kept beside the raw records.

From the worktree root, after setting the required worktree-local temporary
directories, the benchmark and comparison commands are:

```sh
/tmp/umvl-cpu/simbox-run python3 dev/research/evaluations/2026-11-09-slash-perf-data/run_benchmarks.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-09-slash-perf-data/compare.py
```

Save the first command's stdout as `paired-simbox.csv` before running the
comparison. If simbox exits 75, rerun the benchmark through the local `run4`
wrapper. The rejected grid experiment can be regenerated with
`benchmark.py baseline --coarse-grid`; it is not part of production code.
Concise final log excerpts and exact suite commands are retained in
[validation.txt](2026-11-09-slash-perf-data/validation.txt).

Artifact-pane delivery (open): the standard `show` helper returned HTTP 403
because the viewer's configured readable roots omit this worktree. Display
requires registering this worktree with the viewer. The finished report remains
at the requested path; no viewer configuration was changed.
