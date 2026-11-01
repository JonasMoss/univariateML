# TASK-116: analytic-score profile experiment (round 2)

Implementation outcome incomplete; tests below apply to the retained production fitters. Tested revision `0998d888209b9eb2038173ca9f29ddf5bc8cdeb8`.
The production fitters remain unchanged. Round-one proofs and scope are in
the [round-one report](2026-10-31-logconcave-report.md).

For fixed dimensionless shape, put `z_i=b*x_i-u`, `g_i=(log f_k)'(z_i)` and
`h_i=(log f_k)''(z_i)`. Then

    grad L = (-sum(g_i), n/b + sum(x_i*g_i)),
    Hess L = [ sum(h_i),       -sum(x_i*h_i)
              -sum(x_i*h_i),   -n/b^2 + sum(x_i^2*h_i) ].

For EMG, `t=k-z`, `R=1/M(t)` with normal Mills ratio `M`, and
`g=R-k`, `h=-R*(R-t)`. For large positive `t` the implementation uses the
continued-fraction residual `R=t+r`, giving `g=r-z`, `h=-(t+r)*r` to avoid
cancellation. For Bhattacharjee, `D=Phi(z+k)-Phi(z-k)`,
`A=phi(z-k)/D`, `B=phi(z+k)/D`: `g=B-A` and
`h=(z-k)*A-(z+k)*B-g*g`. These follow from differentiating the existing
location scores and use the existing extended-real intermediates. The
Bhattacharjee trial still obtains a log-density denominator from the existing
double-returning density routine, so its derivatives are not interval bounds.

An exploratory implementation uses Newton with backtracking and the analytic
Hessian, an n-scaled score threshold of `1e-10*n` (this is not machine epsilon),
a log-k grid `[-8,8]` spaced by 0.25, golden-section refinement of every
observed grid-local maximum, and the existing per-start Nelder–Mead optimizer
as a final polish. The outer search remains numerical and has no proof that it
finds all modes. The exploratory implementation silently skips some failed
inner solves, so its passing empirical comparison is not sufficient for
production. A stricter version falls back to the original multistart if any
grid/refinement inner solve fails; that version was evaluated separately below.

For the same 620 default-fit samples per family and seed 13320261031, the
exploratory candidate had no status differences and no likelihood loss greater
than `1e-12 * max(1,abs(old logLik))`. Maximum losses were `7.9581e-13` for
EMG (sample 613) and `2.2737e-13` for Bhattacharjee (sample 606). Complete
[EMG records](2026-10-31-logconcave-2-data/emg-exploratory-comparison.csv) and
[Bhattacharjee records](2026-10-31-logconcave-2-data/bhatt-exploratory-comparison.csv)
include all differences. The recorded values are output likelihoods, not
interval-certified likelihoods.

For each round-one shortfall, the old and round-two returned shapes are in
[EMG shape diagnostics](2026-10-31-logconcave-2-data/emg-round1-shape-diagnostics.csv)
and [Bhattacharjee shape diagnostics](2026-10-31-logconcave-2-data/bhatt-round1-shape-diagnostics.csv).
The exploratory fit for the largest Bhattacharjee shortfall (sample 609)
returns `k=1.4034794117` versus the old `1.4034791325`; its likelihood loss
is `1.14e-13`, versus `0.0074089` in round one. For EMG sample 613 the shapes
are `5.46777005` and `5.46776656`, and loss decreases from `4.16e-5` to
`7.96e-13`. This comparison does not map all profile maxima or prove the cause
of every round-one miss. That diagnosis remains incomplete.

## Strict fallback experiment

The conservative variant returns to the existing multistart if any grid or
refinement inner solve fails. It does not establish that a grid or bracket
excludes modes between grid points; its outer search is still numerical.
On the same 620 samples per family, it had no status changes and no loss
beyond the stated relative tolerance. EMG emitted different records on 43
samples, with maximum shortfall `6.8213e-13`; Bhattacharjee was byte-identical
to the existing output on all 620, consistent with fallback throughout this
sample. [EMG paired records](2026-10-31-logconcave-2-data/emg-safe-comparison.csv)
and [Bhattacharjee paired records](2026-10-31-logconcave-2-data/bhatt-safe-comparison.csv)
list all statuses, estimates and likelihoods. The trial code and family patches
are [here](2026-10-31-logconcave-2-data/candidate/).

On the golden n=500 dataset, single-run wall times for the strict variant were
3.725 seconds for EMG and 3.160 seconds for Bhattacharjee. Both results were
identical to the existing fit, indicating fallback. Round-one baseline timings
were 3.185 and 1.995 seconds respectively, from separate single runs. These
measurements do not establish a speed-up. The analytic trial has not been
promoted: the faster exploratory version can skip unresolved grid points, and
the strict version has not demonstrated the intended speed improvement. In
addition, the gradient stopping threshold is not near machine epsilon and
round-one profile maxima have not all been diagnosed. The default behavior and
held-subset paths remain the original multistart. This is an incomplete outcome
for the requested implementation, not evidence that the approach cannot work.

## Final validation

On revision `0998d888209b9eb2038173ca9f29ddf5bc8cdeb8` with production
fitters unchanged, all commands ran through separate `/tmp/umvl-cpu/run4`
invocations, with BLAS/OpenMP pinned to one thread:

| Command | Result |
|---|---|
| `just cpp-test` | pass, 105/105 |
| `just cpp-test-no-long-double` | pass, 126/126 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19818 |
| `just js-build` | pass |
| `just js-test` | pass, 400/400 |
| `just quality emg bhatt` | both pass |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41260 |

The full-tier skip is the optional `sn` package. These tests validate the
existing fitters, not the trial. Logs are in `build/scratch/logconcave2-final-*.log`.
