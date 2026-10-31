# Fixed-shape log-concavity for EMG and Bhattacharjee (TASK-116)

Status: the proposed search was tested as a trial and rejected under the
no-lower-likelihood rule; the requested final checks passed on the retained fitters. Source inspected:
`77019db96d602530303087010cdd86c3e8dc149e`. The existing fitters remain
multistart searches, with the partial certificates described in their dossiers.
Nothing here establishes their global optimality or a speed improvement.

## Theorem and proof

**Proved**, for finite positive shape and finite observations. Let `f_k` be
one of the following standardized densities, and let `b=1/sigma>0` and
`u=mu/sigma`. The log-likelihood is

    L(u,b;k) = n log(b) + sum_i log f_k(b x_i - u).

For EMG, `f_k` is the density of `N(0,1) + Exp(rate=k)` and
`k=lambda*sigma=lambda/b`. For Bhattacharjee, `f_k` is the density of
`N(0,1) + Uniform(-k,k)` and `k=a/sigma=a*b`; `k=0` is the normal.
Thus the shape must be held in these dimensionless units. In particular,
holding the public `lambda` or `a` fixed while changing `sigma` is a different
problem: it traces `k=lambda/b` or `k=a*b` and the following concavity
argument does not apply to that curve.

Both summand densities are log-concave (including the extended-valued
log-density of the uniform or exponential). Their convolution is log-concave
by the Prékopa marginalization theorem; see Saumard and Wellner, *Log-Concavity
and Strong Log-Concavity: a Review*, Statistics Surveys 8 (2014), section 3,
https://doi.org/10.1214/14-SS107. Hence each `log f_k(b*x_i-u)` is concave as
a concave function composed with an affine map, and `n log b` is strictly
concave in `b`. Their sum is jointly concave on the convex half-plane `b>0`.

For these particular convolutions, the log-density is in fact strictly
concave: Gaussian convolution gives a positive real-analytic density. If a
concave log-density had an affine segment, analyticity would extend that
identity to the real line, making the density an exponential function there,
which cannot integrate to one. In any nonzero direction `(du,db)`, either
`db != 0` and `n log b` is strictly concave, or `db=0`, `du != 0` and the
log-density terms are strictly concave. Thus `L` is strictly concave.

To prove attainment for nonconstant data, an integrable positive log-concave
density has `f_k(z) <= C exp(-c |z|)` for some positive finite constants.
Since `sum_i |b*x_i-u| >= b*(max x-min x)`,

    L <= n log b + n log C - c*b*(max x-min x).

This tends to minus infinity as `b` tends to infinity. Boundedness of `f_k`
shows the same as `b` tends to zero. With `b` bounded away from zero and
infinity, the same exponential bound shows the objective tends to minus
infinity as `|u|` tends to infinity. Compactness and continuity then give an
attained maximum, and strict concavity makes it unique. For a constant sample
`x_i=x0`, take `u=b*x0-z0` for any fixed `z0`: the likelihood is
`n log b+n log f_k(z0)` and diverges as `b` tends to infinity. This includes
one observation. A fixed physical location imposes the affine constraint
`u=b*mu`; the same arguments give a unique finite maximum unless every
observation equals that location, in which case collapse is unbounded. With
positive scale held, maximizing over free location has a unique finite maximum,
including constant data. These assertions hold for fixed dimensionless `k`;
they do not classify all free-shape or physically held-parameter problems.

An **exact** interior zero of the two-component score is therefore a global
certificate for the fixed-`k` subproblem. A small computed score is numerical
evidence only unless accompanied by a validated upper bound on its objective
gap, for example one derived from a valid curvature bound on a region known
to contain the maximizer. Roundoff and special-function error also need
accounting for to call such a bound rigorous.

## Algorithm and unresolved work

The trial solves the inner problem in `(u,b)` with positive `b` using
finite-difference Newton steps and backtracking, and profiles over `log k` on
a grid from -6 to 6 in increments of 0.5, refining grid-local maxima by golden
section and evaluating the existing conditional normal and exponential/uniform
faces. The finite-difference score tolerance is 1e-5. Failed inner solves do
not produce accepted trial candidates. This is an experimental numerical
method, not a validated or interval-certified implementation. An outer bracket around a local maximum
alone does not exclude additional modes. No unimodality proof, global profile
variation bound, or exhaustive shape-tail bound has been established here.
The paired comparison below also rejects this particular trial, so no
replacement was made and no global certificate is claimed.
Physical held coefficients also require separate treatment: with held rate or
half-width, fixing `k` fixes `b`, and the remaining outer objective differs
from the unrestricted profile. Held `sigma` similarly fixes `b`, and held `mu`
imposes `u=b*mu`. All subsets and all conditional faces require validation.
The existing tolerance-based face comparisons and multistart stopping rules
should not be represented as exact global certificates.

## Comparison and validation

The complete paired default-fit experiment is reported below; the final test
suite results are recorded below. The prior [fit-speed report](2026-10-31-fit-speed-report.md)
records paired n=500 golden timings of 3.694/3.706 s for EMG and 2.340/2.400 s
for Bhattacharjee on its specified revisions and machine; those numbers are
historical and do not measure an implementation of this task. Its seeded
comparisons pertain to the fit-speed revision, not to a certified profile.
The requested final checks are recorded below.

## Other qualifying families

For fixed shape, Azzalini `sn` has density proportional to
`phi(z)*Phi(alpha*z)`; both factors are log-concave, so `(xi/omega,1/omega)`
has the same concavity. `ged` qualifies for fixed `nu >= 1`, using
`(mean/sd,1/sd)`. Fernandez–Steel `snorm` qualifies for fixed `xi`, and
`sged` for fixed `xi` and `nu >= 1`: each has a piecewise negative convex
power of an affine standardized variable, with matching one-sided slopes at
the join. For `nu < 1`, the GED log-density is not concave on either side.
These observations certify fixed-shape objective concavity only, not a shape
profile, solver implementation, or the existence and uniqueness claims for
every nonsmooth limiting case. No other fitter was changed.

## Candidate pilot, 2026-10-31

A trial implementation was built in `build/scratch/logconcave-candidate` and
compared on the first ten default EMG samples generated by
`dev/validation/fit-speed-comparison.cpp`, seed 13320261031, n=8, with the
driver's tolerance 1e-8 and maximum 1000 iterations. Compiled using Ubuntu c++
with `-std=c++17 -O2`, BH from `/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include`.
On sample 4 both return status Ok. Existing estimates `(mu,sigma,lambda)` are
`(0.13508031976906204, 0.62205396187631046, 27.614176733500578)` and logLik
`-7.5672040566475989`; trial estimates are
`(0.13508007288103202, 0.62205394578615913, 27.613988470425777)` and logLik
`-7.5672040566476007`. The trial is lower by about 1.8e-15, at floating-point
resolution, and therefore fails the requested strict no-lower-likelihood rule.
The other nine pilot records had identical emitted estimates, statuses, and
likelihoods. This is not an assessment of the exact likelihood ordering, and is
not the complete requested comparison. The trial uses finite-difference Newton
steps and an approximate score threshold; it is not a rigorous floating-point
certificate. The original EMG default has been retained.

## Full paired default-fit experiment

The seeded workload has 200 samples each at n=8, 16 and 32, and 20 at n=500,
using the driver and controls described in the pilot. Each row includes the
status, all estimates and both log-likelihoods; differing values are retained
in full rather than filtered.

| Family | Rows | Status changes | Lower trial likelihood | Higher trial likelihood | Any emitted difference |
|---|---:|---:|---:|---:|---:|
| [EMG](2026-10-31-logconcave-data/emg-comparison.csv) | 620 | 0 | 201 | 1 | 224 |
| [Bhattacharjee](2026-10-31-logconcave-data/bhatt-comparison.csv) | 620 | 0 | 35 | 13 | 76 |

The largest observed trial-minus-baseline shortfalls were -0.00004161438755545532
(EMG, sample 613) and -0.007408925203549188 (Bhattacharjee, sample 609). These
are numerical comparisons to the existing fitter, not proofs about the exact
likelihood or the profile. Both default implementations therefore remain
unchanged. [Trial Newton code](2026-10-31-logconcave-data/logconcave_trial.hpp),
[EMG patch](2026-10-31-logconcave-data/emg-trial.patch) and
[Bhattacharjee patch](2026-10-31-logconcave-data/bhatt-trial.patch) record the
rejected candidate; they are evidence, not production headers. The user asked
for the default fits only, so held-subset behavior was not changed or swept.

## Golden-data timing

Single wall-clock runs of the same `-O2` driver on the n=500 golden samples,
with one shared CPU slot and one BLAS/OpenMP thread, gave:

| Family | Existing (s) | Trial (s) | Existing logLik | Trial logLik |
|---|---:|---:|---:|---:|
| EMG | 3.185355415 | 0.247989498 | -846.75971897546572 | -846.75971897546583 |
| Bhattacharjee | 1.995246198 | 0.374546013 | -896.31045982723822 | -896.31045982723867 |

Both statuses were Ok. These single-run timings are workload-specific and the
faster trial was rejected, so they are not timings of a production change.

## Final validation

Source revision `77019db96d602530303087010cdd86c3e8dc149e`, with the dossier
and report edits in this worktree; the production fitter headers retain their
original contents (SHA-256 EMG `49b4d7d6747756fa74b9cedfc736a1aceaa8d761c7e5f54f3a7fb95b4e191a8f`,
Bhattacharjee `f028d96f240dbf61c9ecb3a5b4b300b8db9497abb418e2d9b51b0be40402ba13`).
Ubuntu c++ 15.2.0, R BH headers, shared ccache; local commands used a separate
`/tmp/umvl-cpu/run4` per command and pinned BLAS/OpenMP to one thread. Scratch
logs are under `build/scratch/logconcave-final-*.log`.

| Command | Result |
|---|---|
| `just cpp-test` | pass, 105/105 |
| `just cpp-test-no-long-double` | pass, 126/126 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19818 |
| `just js-build` | pass |
| `just js-test` | pass, 400/400 |
| `just quality emg bhatt` | both pass |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41260 |

The full-suite skip is the optional `sn` package, which is not installed.
The dev-tier skips are tests requiring the full tier. Quality checks concern
the retained fitters; they do not validate the rejected trial.
