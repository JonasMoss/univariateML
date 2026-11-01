# TASK-116: warm-start and failure audit (round 3)

**Outcome: incomplete.** The candidate meets the seeded status and likelihood
tolerances but does not meet the zero-fallback requirement, so neither
production fitter was changed. Tested revision
`ba7ef92ce0ee7be15e22e5ea3ef385a2a8649e87`. Theorems and the earlier trials
are in [round one](2026-10-31-logconcave-report.md) and
[round two](2026-10-31-logconcave-2-report.md).

## Cold-start failure trace

The diagnostic runs the round-two strict inner solver independently at all 65
grid points, even when the original strict search would stop on an earlier
failure. If all grid solves pass, it also records any refinement failure
encountered by that search. Thus the table includes grid solves that the
original strict control flow would not actually reach. It uses the round-two
seed `13320261031`, 200 samples each at n=8, 16, 32 and 20 at n=500 for each
family, and each family's n=500 golden data. It stores `k` and the full path
`iteration:u:b:logLik:grad_u:grad_b` per failing solve in
[the compressed CSV](2026-10-31-logconcave-3-data/cold-start-inner-failures.csv.gz).
The instrumentation and its driver patch are retained alongside it. Source
code was compiled with c++ 15.2.0, `-O2 -std=c++17`, and BH 4.6 library headers;
computations used a shared one-core slot with BLAS/OpenMP at one thread.

| Family | Data | Iteration cap | Line search exhausted | Nonfinite/nonnegative Hessian test |
|---|---|---:|---:|---:|
| EMG | seeded | 887 | 0 | 0 |
| EMG | golden | 1 | 0 | 0 |
| Bhattacharjee | seeded | 740 | 509 | 7440 |
| Bhattacharjee | golden | 5 | 0 | 12 |

The Hessian label combines non-finite, singular and nonnegative-curvature
checks: the trace does **not** distinguish an underflowed CDF difference or
Mills ratio from each of those cases. It therefore does not provide the fully
specific per-solve root causes requested. For example, the first seeded
Bhattacharjee failure at `k=190.56626845863` has zero computed location score,
positive scale score and a singular computed Hessian at its initial iterate.
The uniform-like plateau makes curvature too small to represent there, but the
trace alone is not a proof of the precise underflow mechanism. An EMG example
at `k=0.0011708796207911744` repeats unchanged double-precision coordinates
at the iteration cap while its score gate is unmet.

Every seeded and golden sample here is nonconstant. For every finite positive
fixed shape, the round-one coercivity and strict-concavity proof gives a unique
finite inner maximum; no genuine unbounded inner problem was found or inferred
from a numerical failure. Constant samples with free location and scale have an
unbounded inner likelihood, as proved in round one.

## Warm-start candidate

An isolated candidate walks outward from the central log-shape grid point,
using the neighbouring solution as the next initial value. It uses extended
precision for iterates and objective, stable log-CDF differences, a gradient
ascent direction if computed Newton curvature is unusable, a rounding-aware
backtracking allowance, and a `64 * epsilon(double)` score test scaled by the
sizes of the summed terms. For a narrow Bhattacharjee interval it evaluates
conditional moments using eight-point Gauss–Legendre quadrature to avoid the
loss in a difference of nearly equal normal tail probabilities. The outer
search and per-start Nelder–Mead polish follow round two. The grid and outer
search remain numerical, not a global proof. This is scratch research code,
not a validated production implementation; its
[header](2026-10-31-logconcave-3-data/warm-start-trial.hpp) and
[EMG](2026-10-31-logconcave-3-data/emg-trial.patch) and
[Bhattacharjee](2026-10-31-logconcave-3-data/bhatt-trial.patch) patches are
retained for reproduction.

Explicit fallback counters gave the following for the same seeded samples:

| Family | Matching statuses | Loss beyond `1e-12*max(1,abs(old logLik))` | Fallbacks | Inner-grid failure | No interior grid-local maximum |
|---|---:|---:|---:|---:|---:|
| EMG | 620/620 | 0 | 296 | 0 | 296 |
| Bhattacharjee | 620/620 | 0 | 331 | 3 | 328 |

All estimates, likelihoods, statuses and fallback reasons are in the
[EMG comparison](2026-10-31-logconcave-3-data/emg-candidate-comparison.csv)
and [Bhattacharjee comparison](2026-10-31-logconcave-3-data/bhatt-candidate-comparison.csv).
Largest observed candidate-minus-old losses were `-6.8213e-13` and
`-3.4107e-13`, respectively, in absolute log-likelihood units. The three
remaining warm-start Bhattacharjee failures are
[recorded with paths](2026-10-31-logconcave-3-data/warm-start-bhatt-failures.csv):
samples 600, 604 and 605, each a line-search exhaustion with a small score
still above the gate. Their exact numerical causes have not been established.

The golden fits used the candidate with **zero fallback**: EMG took 0.210 s
and Bhattacharjee 0.497 s. The earlier single-run baseline times were 3.185 s
and 1.995 s respectively; these are separate workstation runs, not a controlled
performance distribution. Candidate and baseline golden log-likelihoods
agreed within `4e-13` absolute. The dominant seeded fallback reason is not an
inner failure: no interior grid-local maximum was detected. Selecting a face
solely because no interior maximum was seen would not rule out an unobserved
mode beyond the finite grid. In this workload, the retained fitter selected
a declared boundary face for every one of those 624 no-local-maximum cases:
EMG chose the normal face 245 times and the translated-exponential face 51;
Bhattacharjee chose the uniform face 271 times and the normal member 57.
This observation does not certify those faces for other samples. The requested
zero-fallback target was not met,
and the original multistart remains the default and the held paths unchanged.

## Final validation

With the production fitters unchanged at revision
`ba7ef92ce0ee7be15e22e5ea3ef385a2a8649e87`, all final commands ran via separate
`/tmp/umvl-cpu/run4` invocations with BLAS/OpenMP pinned to one thread:

| Command | Result |
|---|---|
| `just cpp-test` | pass, 105/105 |
| `just cpp-test-no-long-double` | pass, 126/126 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19818 |
| `just js-build` | pass |
| `just js-test` | pass, 400/400 |
| `just quality emg bhatt` | both pass |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41260 |

The full-suite skip is for the optional `sn` package. These checks validate the
retained production code, not the scratch candidate. Logs are in
`build/scratch/logconcave3-final-*.log`.
