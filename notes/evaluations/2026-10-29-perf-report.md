# GB2 / Bhattacharjee performance report

Status: two bit-identical speedups landed (`gb2/mle.hpp`, `bhatt/mle.hpp`); one
seed-tolerance idea for gb2 and one branch-and-bound idea for bhatt were tried
and reverted because they did not hold up under measurement. Neither target
(gb2 n=1e6 under 30s, bhatt n=1000 under 2s) is fully met; both remaining
gaps are explained below with profiling evidence. No commits made.

## Files touched

- `cppunivariate/include/cppunivariate/distributions/gb2/mle.hpp`: dedup the
  redundant `exp`/`log1p` calls inside `gb2_detail::evaluate`'s per-point loop
  (the hot kernel of the outer (p,q) search).
- `cppunivariate/include/cppunivariate/distributions/bhatt/mle.hpp`: hoist
  the per-call-constant `log(2b)`, `log(s)`, `s*s` out of
  `Profile::location`'s per-point, per-Newton-iteration loop.

Both diffs are small (8 and 12 lines) and, by construction, bit-identical:
they only change *when* an already-computed value is reused, never what is
computed. Nothing in `dpqr.hpp`, `special/`, `truth.*`, the "against the
truth table" test cases, `test.cpp`'s golden assertions, or any sibling
distribution's files was touched.

## Benchmark setup

Scripts (kept under `notes/evaluations/2026-10-29-perf/`, scratch binaries
and logs discarded):

- `bench_gb2.cpp` / `bench_bhatt.cpp`: single-fit timing harnesses,
  `cppunivariate::distributions::StdRng<>` (mt19937_64), seeded gamma-ratio
  draws for gb2 (`(shape1,shape2,shape3,scale)=(.3,2,.4,1)`, matching
  `2026-10-29-gb2-implementation-report.md`) and normal-plus-uniform draws
  for bhatt (`mu=0,sigma=1,a=2`, matching `bhatt/escape-study.R`'s model 2).
- `equiv_gb2.cpp` / `equiv_bhatt.cpp`: old-vs-new equivalence drivers, many
  seeds, default (interior) parameters and limit-face-shaped samples.
- `micro_erfc.cpp` / `micro_location.cpp`: isolated microbenchmarks used to
  diagnose where bhatt's time goes and to verify the hoist's effect without
  Nelder-Mead's own run-to-run noise.

All timed/ built/ tested exclusively through `/tmp/umvl-cpu/run` (one of 4
shared slots, `nice -n 10`, single thread; `OMP/OPENBLAS/MKL_NUM_THREADS=1`
exported by the lock script itself). The machine was shared with at least
one other worktree's `ctest`/R job throughout this session (confirmed via
`lsof /tmp/umvl-cpu/slot*`), so single-run wall times are noisy; multi-run
timings below report the **minimum of 3-5 interleaved repeats** per
configuration (noise can only inflate a run, never shrink it below the true
cost), and the controlled microbenchmarks avoid the issue entirely by timing
many repeats of the identical computation back-to-back in one process.
Builds used `g++ -O2 -std=c++17` against the BH package's Boost headers,
matching the methodology in the prior GB2/Bhattacharjee implementation
reports.

## GB2

### What was slow

`gb2_mle`'s outer search (25-point (log p, log q) grid, plus Burr/inverse-
Burr/beta-prime seeds, up to 8 distinct refined basins, box expansion on
escape) calls `gb2_detail::profile` repeatedly; each `profile` call runs an
inner 2-D Newton solve for (A, c) at fixed (p, q), and each Newton step calls
`gb2_detail::evaluate`, an O(n) loop over the standardized log data. This
loop is the kernel that pays for every one of the ~500-1300 `profile` calls
observed per fit (and ~2.5x that many `evaluate` calls, since the inner (A,c)
Newton solve itself needs a handful of evaluations per `profile` call).

Per point, the original loop called `softplus(-t)` and `softplus(t)`, each
of which independently recomputed `std::exp(-std::abs(t))` and
`std::log1p(...)` of that value — but `std::abs(-t) == std::abs(t)` bit for
bit, so both calls recompute the *same* `exp` and `log1p` that the loop had
already computed once (as `e`) for the logistic quantities `u`, `w`. That is
3 `exp` + 2 `log1p` calls per point where 1 `exp` + 1 `log1p` suffice.

### Fix

```cpp
const double log1pe = std::log1p(e);
const double lpu=-(std::max(-t,0.0)+log1pe), lqu=-(std::max(t,0.0)+log1pe);
```

replacing `-softplus(-t), -softplus(t)`. This is the algebraic identity
`softplus(t) = max(t,0) + log1p(exp(-|t|))` applied with the `exp(-|t|)`
term shared; no numeric value anywhere in the function changes.

### Timings (single run each column is the min of the repeats noted)

| n | before | after | speedup |
|---:|---:|---:|---:|
| 1,000 | 0.406 s | 0.282 s | 1.44x |
| 10,000 | 2.598 s | 1.832 s | 1.42x |
| 100,000 | 41.516 s | 20.630 s | 2.01x |
| 1,000,000 | 328.824 s | 168.501 s | 1.95x |

(gamma-ratio seed 12345 throughout; min of 1 run at 1e5/1e6 given their
cost, min of repeats at 1e3/1e4 confirmed the same numbers reproduce.)

Every column's `shape1/shape2/shape3/scale/logLik/iterations` is **bit-for-
bit identical** before vs after (verified directly, not just within
tolerance) at every size, and across the 90-case equivalence sweep below.

### Why n=1e6 is still ~168s, not <30s

`gprof` (`-pg`, n=20000, aware this is itself noisy under concurrent load)
attributes roughly half of sampled time to `gb2_detail::evaluate` and the
rest to `trgamma_detail::cgf` (used by the two `trgamma_fit_log` face
comparisons), `finite_diff_gradient<BurrProfileCost>` and
`bfgs_minimize<...>` (used inside `burr_mle`'s and `betapr_mle`'s own
internal multi-start searches). All of that second group lives in
`trgamma/mle.hpp`, `burr/mle.hpp`, `invburr/mle.hpp`, `betapr/mle.hpp` —
sibling distributions, out of this task's edit scope — and those three
auxiliary fits are each themselves O(n) multi-start optimizations run once
per `gb2_mle` call purely to produce extra starting points / face
likelihoods for the outer search. Speeding up `evaluate` alone therefore
caps out at roughly the evaluate-only share of total time (Amdahl), which is
consistent with the observed ~1.4-2x rather than the ~2.5x the raw
transcendental-call-count reduction would suggest in isolation.

Separately, the scaling from n=1e5 to n=1e6 is mildly super-linear even
after accounting for the search needing fewer `profile` calls at larger n
(477 "evals" at 1e6 vs 701 at 1e5): a naive evals x n model predicts about
6.8x from 1e5 to 1e6, the observed ratio is ~8.2x. This is consistent with
`d.u` (the standardized-log data vector, 8MB at n=1e6) outgrowing cache
during the ~1,200 full sequential rescans the search performs, especially
under the shared machine's cache contention from other concurrent jobs; it
is a secondary, smaller effect than the auxiliary-fitter share above.

### Explored and reverted: loosening the three auxiliary fitters' tolerance

`burr_mle`, `invburr_mle` and `betapr_mle` are each called once from
`gb2_mle` purely to extract one extra `(log p, log q)` seed; the seed's
exact precision cannot change gb2's final answer, since gb2's own `refine`
re-optimizes every seed to its own tolerance regardless of where it started
(and the 25-point grid, face checks and box-expansion coverage are
untouched by this idea — only how precisely these three *auxiliary* fits
locate their own optimum before handing off a start). Loosening
`burr_mle(x,1e-3,60)` / `invburr_mle(x,1e-3,60)` / `betapr_mle(x,1e-3,60)`
(vs. the original `1e-8,300` / defaults) measured ~30% faster for burr/
invburr in isolation (`bench_seeds.cpp`, not kept), but at the full
`gb2_mle` level, repeated interleaved timing showed no reliable net benefit
(min times matched the unmodified version within noise at n=10,000 and
100,000), while the final coefficients drifted by ~1e-7 relative (still
within the fitter's own `tol=1e-7`, but not bit-identical). Given the
uncertain payoff and the loss of the clean bit-identical property, this was
reverted; the full diff is just the `evaluate` dedup above.

## Bhattacharjee

### What is slow, and why it resists an mle.hpp-only fix

`bhatt_mle`'s outer search is a 4x4 `(log sigma, log a)` start grid, each run
through an independent Nelder-Mead simplex (`bhatt_detail::search`, up to
500 iterations). Each simplex-vertex evaluation (`Profile::evaluate`) either
gets screened by an existing, already-in-place exact upper bound (pointwise
peak-density bound, tightened by an endpoint-pair bound) against the best
known closed-form normal/uniform face, or falls through to
`Profile::location`: a safeguarded Newton/bisection solve for `mu`, observed
to average ~6.3 iterations per call, each doing a full O(n) pass, followed
by one more O(n) pass for the final log-likelihood.

Each point in that O(n) pass calls `distributions::dbhatt`, which (for
ordinary finite arguments) dispatches to `central_density` in `dpqr.hpp`:
entirely `long double` arithmetic, calling `logQ`/`mills`, which call
`std::erfc(long double)` once or twice per `dbhatt` call. Microbenchmark
(`micro_erfc.cpp`, 2,000,000 calls each):

| operation | time/call |
|---|---:|
| `std::erfc(double)` | 56.3 ns |
| `std::erfc(long double)` | 256.7 ns (4.6x slower) |
| `std::exp`+`std::log1p` pair (double) | 18.0 ns |

`std::erfc(long double)` has no hardware path on this toolchain/libm and is
materially more expensive than the double overload or a couple of ordinary
transcendental calls. `central_density`, `logQ`, `mills`, `logdiff` and
`distributions::dbhatt` all live in `bhatt/dpqr.hpp`, explicitly out of
scope for this task (`Do not edit dpqr.hpp`), so this specific cost cannot
be reduced from here without either duplicating dpqr.hpp's numerically
delicate branching inside mle.hpp (rejected: maintenance-hostile, and risks
silently drifting from the one true implementation) or weakening precision
in a way that risks the golden regression test's `epsilon(1e-12)` tolerance
on `mu`/`sigma`/`a`.

Given ~6.3 Newton iterations x up to ~16 starts x up to several hundred
Nelder-Mead iterations each, this `long double erfc` cost, replicated once
or twice per data point per pass, is the dominant and (from this file)
structural lower bound on bhatt's running time.

### Fix found: hoist per-call-constant logs out of the Newton loop

Inside `Profile::location`, `s` and `b` are the call's fixed parameters
(only `mu` moves across Newton iterations), yet the original code recomputed
`std::log(2*(long double)b)` and `std::log((long double)s)` **inside** the
per-point loop, on every one of the ~6.3 Newton iterations. Hoisting these
(and `s*s`) out to just before the iteration loop is a pure loop-invariant
motion — zero semantic change — and removes two `long double log` calls per
point per iteration.

```cpp
const long double log2b=std::log(2*static_cast<long double>(b));
const long double logs=std::log(static_cast<long double>(s));
const long double s2=static_cast<long double>(s)*s;
```

`micro_location.cpp` isolates this change from Nelder-Mead's own
variability by timing 2,000 calls to the per-point-loop body directly, old
vs new, on identical synthetic data:

| run | old | new | speedup |
|---|---:|---:|---:|
| 1 | 1307.1 ns/pt | 912.8 ns/pt | 1.43x |
| 2 | 1089.6 ns/pt | 749.4 ns/pt | 1.45x |
| 3 | 915.4 ns/pt | 817.0 ns/pt | 1.12x |

(the `sink` accumulator matched exactly across old/new in every run,
confirming the change is bit-identical, not merely close.)

### Full-pipeline timings (min of repeats, interleaved orig-vs-new)

| n | before (min) | after (min) | speedup |
|---:|---:|---:|---:|
| 100 | 0.822 s (min of 5) | 0.775 s (min of 5) | 1.06x |
| 1,000 | 8.017 s (min of 5) | 7.303 s (min of 5) | 1.10x |
| 10,000 | 89.5 s (min of 2) | 93.6 s (min of 2) | inconclusive |

The full-pipeline win at n=100/1,000 (~6-10%, from clean interleaved
repeats) is much smaller than the isolated per-point-pass win (12-45%)
because `location`'s hoisted logs are only part of its cost (dominated by
the `dbhatt`/erfc calls discussed above, which are unchanged), and
`location` is itself only part of `bhatt_mle`'s total time (screening,
bookkeeping, the final log-likelihood pass, and 16 independent Nelder-Mead
searches all sit around it). At n=10,000 the single-fit runtime (~90-130 s)
is long enough that this worktree's background load (concurrently, this
session's own `just test`/`distribution_tests` reruns, one other worktree's
`distribution_tests`, and whatever else held the other shared slots) swamps
a ~10% effect — the two "before" repeats alone varied 89.5-130.7 s — so this
row is reported as noise-dominated rather than a real regression; the
n=100/1,000 repeats and the controlled microbenchmark above are the reliable
evidence that the hoist helps. `mu`/`sigma`/`a`/`logLik` were bit-identical
before vs after in every timed case (including at n=10,000) and in the
125-case equivalence sweep below.

### Explored and reverted: tightening the screening bound progressively

The existing exact-bound screening in `Profile::evaluate` compares each
candidate's upper bound against a fixed `face` (the better of the
closed-form normal/uniform boundary likelihoods, computed once before any
start runs). Since any `location`-computed likelihood is an *actually
achieved* value, it is sound to raise the incumbent used for screening every
time one is found (`if(ll>face) face=ll;`, with `face` made `mutable`) —
standard branch-and-bound pruning: an upper bound at or below an attained
value can never be the global optimum, so screening against a tighter,
still-valid incumbent cannot lose the true maximum (the winning start's own
neighbourhood always exceeds every face in the limit, so it is never
screened near its own optimum).

This is sound but was **empirically a regression**: repeated interleaved
timing at n=100 and n=1000 showed it consistently 10-40% *slower*, not
faster, with the winning start's own iteration count actually dropping
(`iters` 137 -> 71 at n=100) while total wall time rose. The likely
mechanism: `search`'s Nelder-Mead decides reflect/expand/contract by
comparing the *returned* `logLik` values, and once a probe point is
screened it returns a conservative upper bound instead of its true
likelihood; feeding the simplex more bounds instead of more true values
apparently degrades its step-selection enough that it needs more iterations
overall, outweighing the saved full evaluations. Reverted; the full diff is
just the `location` loop-invariant hoist above.

## Equivalence checks (old vs new, many seeded samples)

`equiv_gb2.cpp`: 90 cases — 20 seeds x {n=200, n=2000} interior
(shape1,shape2,shape3,scale)=(.3,2,.4,1) samples, plus 10 lognormal-face-ish,
10 Pareto/power-face-ish, 10 double-Pareto-ish, 10 tiny-n (n=8), and 5x2
fixed-parameter-subset cases. Statuses exercised: `Ok` (58), `InvalidData`
(10), `DoubleParetoBoundary` (9), `InvTrGammaBoundary` (5),
`TrGammaBoundary` (2), `PowerBoundary` (2). **All 90 outputs
(status/shape1/shape2/shape3/scale/logLik/iterations, printed to 17
significant digits) are byte-for-byte identical** between the pre- and
post-change builds (`diff` reports no differences).

`equiv_bhatt.cpp`: 125 cases — 20 seeds x {n=30,100,300} interior
normal+uniform(-2,2) samples, 15 normal-face-ish (pure normal, n=100), 15
uniform-face-ish (pure uniform, n=100), 15 intermediate mixed samples
(n=20), 5x2 held-parameter-subset cases, and 5x2 degenerate
(constant/tiny-n=6) cases. Statuses exercised: `Ok` (80), `UniformLimit`
(40), `Unbounded` (5). **All 125 outputs are byte-for-byte identical**
between the pre- and post-change builds (`diff` reports no differences).

## Validation

- `just cpp-test`: 6/6 ctest targets passed — `bfgs_tests`, `sample_tests`,
  `distribution_tests` (836 doctest cases / 1,198,189 assertions, 0 failed,
  across all 98 distributions), `no_throw_tests`, `cmp_tests`,
  `tails_tests`. No "may_fail" truth-table case newly failed.
- `just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 22722 ]` (the one skip is
  the pre-existing, documented missing-`{sn}`-package case, unrelated to
  this change).
- `just quality gb2 bhatt`: 2/2 passed (`gb2 pass`, `bhatt pass`).

## Summary

| distribution | target | achieved | met? |
|---|---|---:|---|
| gb2, n=1e6 | <~30 s | 168.5 s (from 328.8 s, 1.95x) | no — bounded by sibling-file auxiliary fitters + cache effects, both outside this task's edit scope |
| bhatt, n=1000 | <~2 s | ~7.3 s (from ~8.0 s, 1.10x) | no — bounded by `long double erfc` inside `dpqr.hpp`'s `central_density`, outside this task's edit scope |

Both landed changes are bit-identical, minimal (8 and 12 lines), and
verified against 90 + 125 seeded/limit-face equivalence cases plus the full
local test suite. Two further ideas were tried, measured, and reverted
because they either showed no reliable net benefit (gb2 seed-tolerance
loosening) or were an outright empirical regression despite being
theoretically sound (bhatt progressive bound-tightening) — both are
documented above as negative results rather than silently dropped.
