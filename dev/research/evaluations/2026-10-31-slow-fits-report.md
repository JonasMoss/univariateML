# TASK-133: slow fits (2026-10-31)

Source: `998a3a6776ccf2204d2085f596c281067528c11a` plus this worktree's
changes. Ubuntu 26.04.1, GCC 15.2, BH 1.90.0.1; single-slot `nice -n 10`,
BLAS/OpenMP one thread; no simbox. Timings below are single wall-clock runs,
not repeated benchmarks. The exact golden samples have n = 500.

## Student-t and scale-t held-scale profiles

The shared `lst_detail::maximize` repeatedly evaluated the same grid points,
including neighbors. Each outer shape evaluation runs another grid search over
location, so those duplicate calculations multiplied. Grid values are now
memoized per invocation. This preserves evaluation results, comparisons,
local-refinement intervals, tolerance and iteration limits. For a deterministic
profile, reuse is exact; no grid point is removed. The numerical grid still does
not certify that all modes have been found, so this is not a proof of the MLE.

| Fit and held coefficient | Before (s) | After (s) | Status |
|---|---:|---:|---|
| std, sd | 16.009 | 3.514 | Ok in both |
| lst, sigma | 9.883 | 2.047 | Ok in both |

The timings use `-O2`, the golden data and estimates as held values, tol `1e-8`
and maxit 1000. Both fits exceed one second; the remaining nested grid dominates.
The reported likelihoods agree at printed precision. The deterministic cache
argument establishes equivalence of profile evaluations, not global optimality.

`dev/validation/slow-fits-equivalence.cpp` compares old and new binaries using
seed 13320261031, 200 normal, Student-t and transformed samples per hold subset
and family, n = 8, 16, 32, all seven nonempty held subsets, tol `1e-8`, maxit
200. Binary64 outputs, statuses, likelihoods and iteration counts matched
byte for byte. The regression test checks that grid points are evaluated once
and that a failed refinement remains failed. It does not prove statistical
correctness or cover every input.

## Other profiled paths and outstanding work

Baseline native `-O2 -pg` profiles used the golden n=500 samples, one run
per row. Times include profiling overhead and use the C++ controls in the
scratch driver; they are not paired with the uninstrumented timings above.
The sampling profiler does not account reliably for dynamically linked math
functions, so attribution is indicative rather than a full time breakdown.

| Fit (held) | Instrumented s | Observed work | Change |
|---|---:|---|---|
| tlambda (sigma) | 16.65 | 33 starts; 17.5M quantile inversions | none |
| emg (none) | 3.82 | 25 starts, 4,145 location profiles | none |
| bhatt (none) | 2.50 | 16 starts, 4.24M density evaluations | none |
| slash (none) | 1.17 | location grid; inlining obscures profile attribution | none |
| sstd (nu, xi) | 3.02 | 19,447 held-profile evaluations | none |
| sged (sd, nu, xi) | 3.22 | 29,287 location likelihood evaluations | none |
| fpareto (min) | 0.88 | 46,335 GB2 evaluations; limit status 6 | none |

The fpareto profile returned a boundary status, not an interior fit. The
instrumented controls for sstd, sged and fpareto differ from R defaults, and
other slow hold subsets have not been independently profiled. No after timings
or equivalence runs were made for these unchanged families. A safe speed-up
for them and an under-one-second implementation for std/lst remain open;
the existing numerical searches lack global certificates. The profiles alone
do not establish that the one-second target is mathematically impossible.
The scratch driver and logs are under `build/scratch/`, which is ignored.

## Validation

- `just cpp-test`: 104/104 passed.
- `just cpp-test-no-long-double`: 125/125 passed.
- `just test`: FAIL 0, WARN 0, SKIP 151, PASS 18887.
- `just test-full`: FAIL 0, WARN 0, SKIP 1 (`sn` unavailable), PASS 39040.
- `just quality lst std`: both passed.

All commands ran under `/tmp/umvl-cpu/run4`, after setting `TMPDIR`,
`JUST_TEMPDIR` and `XDG_RUNTIME_DIR` to `build/tmp`. The seeded comparison and
timings ran under `/tmp/umvl-cpu/run`. No goldens were changed.
