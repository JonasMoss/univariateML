# TASK-116: default profile search (round 4)

Tested source begins at `a257d53893053b653e2fd487221f80c1eb769744`;
production fitter and dossier changes are uncommitted. The [round-three
report](2026-10-31-logconcave-3-report.md) records the warm-start candidate,
its failure traces, the fixed-shape theorem and prior comparisons.

For free fits, the implementation solves the fixed-dimensionless-shape
location/inverse-scale problem with analytic scores and Hessians, traverses a
log-shape grid from -8 to 8 in steps of 0.25 with warm starts from its centre,
refines each observed interior grid-local maximum, and polishes the best one
with the existing local optimizer. Each family also evaluates one existing multistart seed
and keeps it if its likelihood is at least as high as the profile polish; this
preserves the established golden fit precision. With
no such maximum it compares both grid
ends with the existing conditional boundary-face routines and their `1e-8*n`
tie rules. If no face wins, the inner solve fails, or polishing fails, it
runs the old multistart. Held subsets retain the original multistart path.
The `CPPUNIVARIATE_LOGCONCAVE_TEST_HOOK` build macro exposes per-thread
`fallback_count` and `fallback_reason` for this purpose. The outer search is
numerical: a finite grid and face comparison do not prove global optimality.

The inner fixed-shape objective is strictly concave for either family. On
nonconstant data it has a unique finite maximizer; an exact zero score proves
it is the inner maximum. The floating-point score check is numerical and is
scaled by component-wise term magnitudes at 64 double-precision epsilons.
It does not provide an interval-certified objective error bound. The
Bhattacharjee narrow-interval evaluation uses normal conditional moments from
an eight-point Gauss–Legendre rule. See the round-three report for formulas,
fixed-shape collapse, tail handling and earlier diagnostic traces.

## Paired default-fit comparison

Using `dev/validation/logconcave4-comparison.cpp`, seed `13320261031`, `tol`
`1e-8`, `maxit` 1000, and the default free fit, the workload contains 200
samples at each of n=8,16,32 and 20 at n=500. Other masks are not part of this
experiment; held subsets retain their existing code path. The old executable
was built from revision `a257d53893053b653e2fd487221f80c1eb769744`, and the
new executable from this uncommitted source. Both use c++ 15.2.0, `-O2`, C++17,
BH headers from R 4.6, one shared CPU slot and one BLAS/OpenMP thread. The
comparison uses loss tolerance `1e-12 * max(1,abs(old logLik))`. Status codes
are the respective `EmgStatus` and `BhattStatus` enum values.

| Family | Rows | Status changes | Losses beyond tolerance | Differing emitted records | Fallbacks |
|---|---:|---:|---:|---:|---:|
| [EMG](2026-10-31-logconcave-4-data/emg-comparison.csv) | 620 | 0 | 0 | 223 | 0 |
| [Bhattacharjee](2026-10-31-logconcave-4-data/bhatt-comparison.csv) | 620 | 0 | 0 | 71 | 3 |

The linked files list every status, coefficient, likelihood and fallback
reason, including unchanged rows. The largest absolute candidate-minus-old
losses were `-6.821210263296962e-13` for EMG (sample 613) and
`-1.1368683772161603e-13` for Bhattacharjee (sample 601). The three
Bhattacharjee fallbacks were inner failures in samples 600, 604 and 605,
previously traced in the round-three report. There were no polish-failure or
unresolved-face fallbacks. `logconcave4-hook-check.cpp` additionally forced
inner failures with `maxit=1`, checked the per-thread count and reason for both
families, and checked that a held-parameter fit did not increment the count.

| Family | Golden n=500 old (s) | Golden n=500 new (s) | 620-sample total old (s) | 620-sample total new (s) |
|---|---:|---:|---:|---:|
| EMG | 3.207 | 0.363 | 232.68 | 18.23 |
| Bhattacharjee | 2.063 | 0.578 | 133.15 | 38.75 |

Each is a single wall-clock run; total timings exclude slot-queue time. Other
workstation activity, including a separate short comparison-driver build that
may have overlapped the Bhattacharjee baseline, limits the precision of the
speed comparison. Both families’ golden estimates and likelihoods were identical in these runs. The targeted EMG and Bhattacharjee
C++ shards, including the existing golden checks and added normal-inner and
Bhattacharjee affine-equivariance checks, passed.

Relevant new-source SHA-256 hashes: EMG `5f15304622667b56b53b1e6ac3c73827f5d0f6bec54870406244ae899345953d`,
shared profile `4e2e65fc6ebdd65ae24ef3152a6d99d03eaa096a618a621d1f1f81c70ab8f664`,
Bhattacharjee `020e027e4710cea64fb6b752ce9bff5d5d71ef45d524c85fc438e8c2c305efdf`.
These hashes describe the final comparison build; the baseline fitter hashes
were `49b4d7d6747756fa74b9cedfc736a1aceaa8d761c7e5f54f3a7fb95b4e191a8f`
(EMG) and `f028d96f240dbf61c9ecb3a5b4b300b8db9497abb418e2d9b51b0be40402ba13`
(Bhattacharjee). No golden fixture was changed.

## Golden precision decision

The first profile-only polish passed the seeded likelihood rule but failed the
existing Bhattacharjee coefficient golden check; adding the existing `(-1,-0.3)`
multistart seed as a competing polish restored that golden. The first R dev
run then failed the EMG coefficient golden check; the existing `(-6,0)` seed
restored that golden. For both families the additional candidate is retained
only if its optimized log-likelihood is at least that of the profile polish.
The first R dev failure is retained in
`build/scratch/logconcave4-final-test.log`; no golden or tolerance was changed.

## Final validation

On the source hashes above, with BLAS/OpenMP pinned to one thread and each
command in a separate `/tmp/umvl-cpu/run4` invocation:

| Command | Result |
|---|---|
| `just cpp-test` | pass, 105/105 |
| `just cpp-test-no-long-double` | pass, 126/126 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19818 |
| `just js-build` | pass |
| `just js-test` | pass, 400/400 |
| `just quality emg bhatt` | both pass |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41260 |

The full-suite skip is for the optional `sn` package. Final logs are in
`build/scratch/logconcave4-final2-*.log`. `git diff --check` passed.
