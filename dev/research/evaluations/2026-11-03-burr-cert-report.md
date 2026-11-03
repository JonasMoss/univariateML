# TASK-138: Burr XII closure, tail bounds and global fitting certificates

Source: `86b51aad68743e6a05d91be9d4c888639971ef74`, branch
`codex/burr-cert`. All pre-existing tracked files are unchanged. This report
and its supporting directory are the only additions. No commits, board edits,
sub-agents, dependency installations or simbox runs were used.

## Outcome

A certified all-free Burr XII maximizer was not obtained. The research branch
of the brief therefore applies: no fitter, dossier metadata, adapter, production
C++/R test, default fit or golden was changed. No change is needed in the
settled d/p/q/r, special-function, truth or code-generation files for the
mathematical reductions below.

The work supplies an exhaustive classification of maximizing **sample
likelihood escape values**, a closure-based existence equivalence, a whole
small-shape exclusion region, a uniform large-shape bound, and dual/chord
upper bounds for the intervening profiles. These results support a global
likelihood-value enclosure scheme. They do not yet identify and certify a
global maximizing stationary point with controlled coefficient error.

The numerical prototype reached its requested likelihood-gap diagnostic on
27 of 39 workloads; 12 exhausted its explicit subdivision budget. None of
these outputs is exposed as a certified estimator or winning-face decision.
The exact gaps remaining before a production implementation are listed below.

## Proved results

The [derivation](2026-11-03-burr-cert-data/derivation.md) gives every inequality
and proof. These are exact-arithmetic claims. They do not rely on optimizer
convergence, outward-rounded intervals, or an assumed all-free uniqueness
theorem.

Let `n` be sample size, `y_i=log(x_i)`, `J=sum y_i`, and
`F(k)=max_(c,s) ell(k,c,s)`, with `k=shape1`, `c=shape2`. Assume positive,
finite, nonconstant data. Let `m<n` count observations at the sample minimum,
and let `P,W` be the maximized Pareto and Weibull log likelihoods, with their
full Jacobians included.

1. **Unique conditional fit.** For each fixed positive k, the likelihood is
   strictly concave in `(c,b)`, where `c>0` and `b=-c log s`. Its superlevels
   are compact, uniformly on compact interior k-intervals. Consequently the
   fixed-k maximum exists, is unique, and F is continuous. The intercept and
   then the profiled c can be solved with bracketed monotone scores.

2. **Small-k exclusion.** Every finite fit with
   `0<k<=m/(n-m)` is strictly below P. In particular, any attained all-free
   global maximum must have `k>m/(n-m)`. For smaller k the stronger uniform
   upper bound is

   $$
   F(k)\le P+C(k),\qquad
   C(k)=A\log(A/B)+(B-A)\log(1-A/B)<0,
   \quad A=nk,\quad B=m(k+1).
   $$

   This follows by retaining only the tied-minimum terms after writing
   `q=kc`, `t=c log(min(x)/s)` and profiling their penalty exactly.
   Moreover, `F(k)->P` as k tends to zero. A feasible path approaches the
   endpoint from below with `t=log(1/k)`, removing the tied-minimum penalty
   that occurs when scale is held exactly at the endpoint.

3. **Uniform Weibull tail bound.** For every `k>n-1`,

   $$
   F(k)\le W+E(k),\qquad
   E(k)=\frac{n^2(k+1)}{2(k+1-n)^2}.
   $$

   E decreases to zero. This bound is uniform over the other fitted
   coefficients. It comes from the scale score in coordinates
   `t_i=k(x_i/s)^c`, which bounds both each t and their sum by
   `nk/(k+1-n)`, followed by `log(1+u)>=u-u^2/2`.
   A feasible Weibull path supplies the matching lower limit, so `F(k)->W`.
   Any retained candidate strictly above W therefore has a finite,
   computable eventual upper-k exclusion cutoff.

4. **Likelihood closure and existence.** On an interior k-interval, escape
   of c or b loses every fixed positive likelihood superlevel. At the two
   remaining ends, the profile limits are exactly P and W. Thus these two
   values exhaust maximizing escapes for nonconstant samples, including
   oscillating sequences by taking subsequences. This is a sample-likelihood
   result, not a catalogue of every weak distributional limit.

   A finite MLE exists **if and only if some finite fit reaches or exceeds
   `max(P,W)`**. Strict improvement gives compact containment. Equality also
   gives existence: either the equal candidate already maximizes globally,
   or a better candidate gives the strict case. This equivalence is not a
   closed-form sample test and does not prove joint uniqueness. Constant
   samples instead have unbounded likelihood, already witnessed by holding
   `k=1,s=x0` and taking c to infinity.

5. **Dual upper bounds at fixed k.** Choose `p_i in [0,1]` with
   `sum p_i=n/(k+1)` and
   `A=sum[((k+1)p_i-1)y_i]>0`. With binary entropy H, the softplus supporting
   inequality gives

   $$
   F(k)\le U(k,p)=n\log k+n\log(n/A)-n
                        -(k+1)\sum_i H(p_i)-J.
   $$

   Equality holds at the exact conditional fit with `p_i=sigmoid(c y_i+b)`.
   This separates an upper certificate from an optimizer's stopping claim.
   Endpoint dual weights are allowed by continuity with `0 log 0=0`.

6. **Upper bounds between profiles.** `G(k)=F(k)+J-n log k` is convex,
   being a supremum of affine functions of k. Chords through upper endpoint
   values bound G above throughout each interval. Adding `n log k-J` gives
   an interval upper envelope whose maximum is explicit. Subdivision can
   therefore bound unsearched intervals without assuming unimodality.

7. **Weibull inward slope.** At the fitted Weibull, write
   `t_i=(x_i/a_hat)^c_hat`; then `sum t_i=n`. The inward slope in `xi=1/k`
   is `sum(t_i^2)/2-n`. If it is positive and `W>=P`, finite existence
   follows. A negative slope establishes only the local Weibull behavior.
   At c=1 this reduces to the expression in the job brief. For the all-free
   problem one must use the fitted Weibull power.

## Numerical study

[Script](2026-11-03-burr-cert-data/study.py),
[C++ baseline driver](2026-11-03-burr-cert-data/probe.cpp),
[retained samples](2026-11-03-burr-cert-data/samples.json),
[all diagnostics](2026-11-03-burr-cert-data/diagnostics.csv),
[summary](2026-11-03-burr-cert-data/summary.json), and
[independent evaluations](2026-11-03-burr-cert-data/verification.json)
are kept with this report. The entire supporting directory is about 116 KB.

The study uses NumPy seed 138. Five replicates of size 50 cover each of:
Burr `(k,c)=(1,2),(.08,2),(1000,2)` with scale `k^(1/c)`;
Weibull shape 2, scale 1; Pareto endpoint 1, shape 2; and lognormal log-mean
0, log-SD 1. The exponential draws are shared across the related Burr,
Weibull and Pareto cases within each replicate; the regimes are not
independent Monte Carlo experiments. The golden is the existing 500-value
fixture, with its recorded seed 12788.

Additional workloads are the 200-point Weibull and Pareto quantile samples,
`(1,1,1,2,4,8)`, `(1,2)`, 20 nearly constant observations
`exp(seq(-1e-6,1e-6))`, and common-uniform Burr paths toward Pareto at
`k=.01,.0001,.000001`, with `c=2/k,s=1`. The last two paths round to the
same retained sample in binary64; their equality is not evidence that the
study can resolve arbitrarily small departures from the limit.

Each fixed-k fit uses bracketed intercept and c scores on centered,
standardized log data. Root absolute/relative tolerances are `2e-13` and
`2e-14`; c-bracket expansion and tail-cutoff doubling each have an 80-step
budget and fail if unsuccessful. Reconnaissance is followed by chord-bound
subdivision, with at most 512 endpoint profile evaluations per sample.
All likelihoods in the CSV include the transformation Jacobians.

The diagnostic target is an objective gap of `1e-6*n`. A dual padding of
`1e-9*n` is added after evaluating in extended precision and repairing one
dual component to the target sum. The largest recorded sum residual is
`2.78e-17`; these observations do not prove a uniform arithmetic error bound.
The search gap is a deliberate approximation tolerance, **not** a practical
floating-point comparison margin for the requested production certificate.

| Workload | Samples | Reached value-gap diagnostic | Subdivision budget |
|---|---:|---:|---:|
| Golden | 1 | 1 | 0 |
| Burr k=1 | 5 | 5 | 0 |
| Burr k=.08 | 5 | 5 | 0 |
| Burr k=1000 | 5 | 0 | 5 |
| Weibull | 5 | 0 | 5 |
| Pareto | 5 | 5 | 0 |
| Lognormal | 5 | 5 | 0 |
| Quantile, tied, two-point, near-constant and near-Pareto cases | 8 | 6 | 2 |
| Total | 39 | 27 | 12 |

There were 8,353 budgeted profile evaluations. Among the 27 value-gap cases,
15 have finite candidates strictly above both faces and 12 have Pareto as
the retained best value. In the latter 12, the numerical compact-interval
and large-k upper bounds lie below P by more than `1e-8*n`; the small-k
region is analytically strictly below P. This is promising for a Pareto
certificate once the arithmetic conditions are enforced in production.
It is not exposed as such a certificate by the research code.

Every subdivision-budget case has Weibull as the best retained value.
For the size-50 near-Weibull samples, the analytic tail cutoff is
26,214,400, but the remaining interval envelope still leaves gaps near
0.00876 after 512 evaluations, against a requested gap of 0.00005.
The quantile Weibull and nearly constant cases leave gaps of 0.03446 and
0.003483. The script reports the exhausted budget. Its `finite_k` column
records a feasible checked candidate, including candidates at the cutoff;
it is never returned as an estimate at a cap.

For the golden, `P=-1245.1574388915974`, `W=-655.1765337522666`, and the
tail can be excluded from k=4000 against the retained finite candidate.
After 97 profiles the numerical enclosure is
`[-613.6798001451058,-613.6793533401270]`. Its retained k is
`1.3340094159180467`, whereas the unchanged default fit has
`k=1.334637717604056`, `c=1.8604176234909942`,
`s=1.1969913095113565`, and `ell=-613.6797967508302`.
The grid's likelihood loss is only about `3.4e-6`, but its relative k error
against that default is about `4.7e-4`, exceeding the C++ golden tolerance
`1e-6`. Thus the value-gap diagnostic cannot serve as a coefficient
accuracy certificate or replace the default fit.

The existing C++ fitter was replayed on all 39 samples. No new lower
candidate beat its returned log likelihood by more than `3.56e-14`.
This is finite-workload agreement, not a global validation of its multistart
or boundary decisions.

Independent 70-digit evaluations of all 39 retained finite candidates agree
with their recorded log likelihoods within `2.56e-13`. Additional checks
on every sample verify a profile-chord midpoint inequality and a large-k
comparison. Separately, 300 seeded trials at 70 digits test the small-k
entropy bound, feasible dual upper inequality and scalar logarithm bound,
with tolerance `1e-55`; all pass. These numerical checks corroborate the
proofs and implementations of the formulas, rather than proving reliable
floating-point decisions everywhere.

## Open items and rejected shortcuts

**Open: identify and certify the finite maximizing stationary point.**
The chord construction bounds likelihood values but does not isolate all
all-free roots or establish local uniqueness in the surviving k-neighborhood.
At a conditional fit, with `H=-ell_((c,b),(c,b))` positive definite and
`v=sum p_i*(y_i,1)`,

$$
F''(k)=-n/k^2+v^\mathsf{T}H^{-1}v.
$$

Conditional concavity therefore supplies no uniform negative sign for the
remaining profile curvature. A single k-score root search has no proved
global guarantee here. A sufficient next mathematical ingredient would be
root enclosures with verified local maximality/uniqueness and dominance over
all discarded intervals, using explicit derivative bounds; alternatively a
theorem controlling all-free stationary points could replace that work.
Neither was established in this job. An exactly representable root is not
required, but a merely small objective gap does not establish this result.

**Open: production comparison bookkeeping.** The dual equality constraint
must be enforced algebraically or accompanied by a valid residual bound on
a proved parameter superlevel. A small nonzero sum residual alone leaves
an uncontrolled intercept term in the global tangent bound. The Weibull
quantity in the tail formula also needs a certified upper allowance from
its monotone scalar root bracket; the study uses its numerical root value.
The derivation explains how concave-profile tangents could provide that
allowance. A stated practical margin and budget/failure diagnostics must
then be propagated through every comparison and the R adapter. The empirical
padding used for this study is not that completed error analysis. Outward
rounding is not the missing prerequisite.

**Open: useful winning-Weibull and exact-tie decisions.** E(k) is positive
at every finite k, so it cannot certify a Weibull winner against W itself.
A quantitative signed tail expansion, combined with control of the fitted
power/scale, could address the observed negative-slope cases. A local slope
without that cutoff and the remaining global comparisons is insufficient.
Budget exhaustion is a sound alternative required by the brief; it must
remain an explicit failure rather than becoming a face recommendation.
No exact finite/face tie classification is claimed by these numerical runs.

The GPD-on-the-power-scale identity is useful for the Weibull expansion, but
optimizing c changes the transformed data. A fixed-power GPD/Lomax criterion
does not settle the all-free optimization. Likewise fixed-k uniqueness,
multistart agreement, and the negative Weibull slope cannot individually
certify the requested global answer.

The publisher's accessible preview of
[Shao (2004)](https://www.sciencedirect.com/science/article/abs/pii/S0167947302003675)
describes the Pareto/Weibull boundary comparisons, an estimation algorithm,
and the possibility of multiple maxima. It also explains why relying only
on the Weibull boundary can fail. The full text could not be retrieved
(HTTP 403); no uninspected theorem from that paper is used above, and no
novelty claim is made for these reductions. The proofs here are self-contained.
The earlier [family analysis](2026-10-29-p0-genbeta.md) and
[collapse report](2026-11-02-burr-collapse-report.md) remain the historical
context for the unchanged production fitter.

## Final validation and reproduction

Every final command below finished successfully on the local workstation
before this report was written. All numerical workloads, builds and suites
used the supplied CPU slot locks. The runners apply `nice -n 10`, allow at
most four shared cores, and pin BLAS/OpenMP to one thread per worker. No
command was detached or left running.

| Final command | Machine | Result |
|---|---|---|
| Research `run.sh` below | Local, one slot | Exit 0; 39 samples, 8,353 budgeted profiles; all research assertions passed |
| `just cpp-test` | Local, four slots | 106/106 dev shards passed |
| `just test` | Local, four slots | FAIL 0, WARN 0, SKIP 153, PASS 20507 |
| `UMVL_TEST_TIER=full just cpp-test` | Local, four slots | 127/127 full shards passed |
| `just test-full` | Local, four slots | FAIL 0, WARN 0, SKIP 0, PASS 43324 |

There are no permitted final test failures. The dev skips are tier/optional
checks; the full R run has no skips. Native CMake used the existing BH-header
fallback after its first system-Boost discovery failed; its final build and
tests exited zero. No golden or truth fixtures were regenerated.

Environment: Ubuntu 26.04.1 LTS, Linux `7.0.0-38-generic`, GCC 15.2.0,
BH 1.90.0-1, R 4.6.1 (2026-06-24), Python 3.14.5, NumPy 2.4.6,
SciPy 1.18.0 and mpmath 1.3.0. Existing golden and regression tolerances
were unchanged. The C++/R suites establish regression health of the existing
implementation; they do not supply the missing all-free certificate.

From the worktree root:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-03-burr-cert-data/run.sh
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test
/tmp/umvl-cpu/run4 just test-full
```

Scratch executables and captured logs are under `build/scratch/`. Lasting
samples, measurements, scripts and proofs are retained beside this report.
`git diff --check` and the additional new-file whitespace/report-link checks
passed; all pre-existing tracked files remain unchanged.

The artifact viewer rejected this worktree path with HTTP 403 because it is
outside its configured readable roots. The report remains at the requested
path; no viewer configuration or file outside this worktree was changed.
