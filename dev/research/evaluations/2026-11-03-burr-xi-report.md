# TASK-138: Burr XII fitting through the Weibull seam

Tested source: `65866760db7f0335c7bf819f400a1c35a04a39a1`, branch
`codex/burr-xi`. Only this report and its supporting
[evidence directory](2026-11-03-burr-xi-data/README.md) are added.
All builds, research computations and tests finished locally before this
report was written. No commits, git writes, board edits, sub-agents,
dependency installations or simbox runs were used.

## Outcome

The seam coordinate gives a **proved, quantitative signed Weibull tail
exclusion**, including nuisance reoptimization. This removes the specific
always-positive-tail obstruction in the previous certificate attempt for
samples with strictly negative Weibull inward slope. An exact rational
instance certifies the tail of the earlier negative-slope counterexample.

The numerical likelihood-value enclosure now reaches its target on **all
50 workloads**, including all 12 previous near-Weibull failures. This is
an improvement over 27 of 39 in the earlier study.

A coefficient-certified global maximizing stationary point was not obtained.
In particular, neither the surviving stationary roots nor their global
dominance have been enclosed with propagated comparison allowances. The
brief's no-implementation branch therefore applies: no C++ fitter, dossier
metadata, binding, production test, default fit or golden is changed.
No changes to d/p/q/r, special functions, truth files or code generation
are needed for the mathematics established here.

## The signed seam bound

The [derivation](2026-11-03-burr-xi-data/derivation.md) supplies the proofs,
all constants, a sharper matrix version used in the study, and the remaining
accuracy requirements. These are exact-arithmetic results conditional on
the stated comparisons; numerical root convergence is not used as a proof.

For finite positive nonconstant data, let `y_i=log(x_i)`, omit the constant
Jacobian, and put `xi=1/k`, `theta=(c,eta)`, `eta=log(k)-c log(s)`, and
`v_i=exp(eta+c*y_i)`. The exact continuous extension is

\[
 L(\theta,\xi)=n\log c+n\eta+c\sum_i y_i
 -(1+\xi)\sum_i\int_0^{v_i}\frac{du}{1+\xi u},\qquad \xi\ge0.
\]

At zero this is Weibull. Its negative nuisance Hessian is positive definite:

\[
 -L_{\theta\theta}=
 \begin{pmatrix}n/c^2&0\\0&0\end{pmatrix}
 +\sum_i\frac{(1+\xi)v_i}{(1+\xi v_i)^2}
           (y_i,1)^T(y_i,1).
\]

Let `theta_W` be the exact Weibull solution, `W=L(theta_W,0)` and
`A=sum(v_i^2)/2-n`, evaluated there. Choose a nuisance ball of radius
`0<R<c_W`. On that ball and `0<=xi<=h`, explicit exponential bounds
give a positive lower Hessian bound `mu`. The exact score at its center is

\[
 L_\theta(\theta_W,\xi)=
 \xi\sum_i\frac{v_i(v_i-1)}{1+\xi v_i}(y_i,1)^T.
\]

An explicit constant D bounds its norm by `xi*D`. If `h*D<mu*R`, radial
boundary derivatives are strictly negative. Concavity then places every
conditional maximum inside the ball, including for all intermediate xi;
no unquantified appeal to continuity is needed. Nuisance reoptimization
can improve the center's likelihood by at most `xi^2*D^2/(2*mu)`.

For the remaining derivative,

\[
 L_{\xi\xi}(\theta_W,\xi)=
 2\sum_i\int_0^{v_i}\frac{u(1-u)}{(1+\xi u)^3}\,du.
\]

Bounding the positive part and retaining a bounded negative contribution
above one yields an explicit quadratic remainder. Consequently

\[
 H(\xi):=\max_\theta L(\theta,\xi)
 \le W+A\xi+B\xi^2\qquad(0\le\xi\le h).
\]

The basic version uses `B<=n/6+D^2/(2*mu)`; the sharper B in the derivation
uses the matrix Hessian bound and the negative integrals. If `A<0` and
`h*B<=-A/2`, every `0<xi<=h` is strictly worse than Weibull. Thus
**all finite `k>=1/h` are excluded against W itself**. For any fixed
admissible radius and strictly negative A, halving h eventually satisfies
both conditions in exact arithmetic. The study uses five radii and a
100-halving budget per radius, with failure if no choice qualifies.

This is stronger than a local slope observation: it supplies an explicit
interval with a remainder uniform over the reoptimized nuisances. It
does not classify the rest of the profile or assert Weibull wins globally.
Pareto and compact-interval comparisons remain necessary.

## An exact instance, and why concavity still fails

For the exact log sample
`(0,5,7,8,8,15,15)/15`,
[exact.py](2026-11-03-burr-xi-data/exact.py) reuses the earlier rational
exp/log enclosures to bracket the Weibull root and bound all constants.
It proves, using rational comparisons,

* `A < -0.055`;
* `0.2952035320 < C_W < 0.2952035333`, hence `H''(0)>0.59`;
* with `R=1/50`, every `k>=8192` has likelihood strictly below W.

[exact.json](2026-11-03-burr-xi-data/exact.json) retains the exact root
bracket and cutoff, plus decimal summaries of the bound constants.
The conservative lower eigenvalue bound is `det(M)/trace(M)`; vector
one-norms avoid square-root enclosures. The containment ratio is below
0.158 and `h*B/(-A_upper)<0.410`, providing strict comparison slack.

The earlier exact finite witness exceeds both faces on this same sample.
Thus local Weibull stability, positive seam curvature and a finite global
winner coexist. The seam does not make the unrestricted profile concave.
The numerical score reconnaissance finds a maximum near `k=0.78869`
and a minimum near `k=9.33525`; these locations are numerical, whereas
the curvature sign and signed tail above are exact certificates.

## Numerical evidence

[study.py](2026-11-03-burr-xi-data/study.py) preserves the original 39
burr-cert samples, including the golden; all seeded Burr, Weibull, Pareto
and lognormal samples; quantiles; ties; two observations; nearly constant
data; and near-Pareto paths. Their original NumPy seed is 138, with five
size-50 replicates per generating regime. These are reused samples, not
new independent Monte Carlo replication.

Eleven closure workloads add the negative-slope finite winner, the equal-face
finite winner, positive-slope nonexistence, both sides and the numerical root
of the zero-slope crossing, both equal-face bracket endpoints, and three
points of the finite/Pareto tie family. The retained
[samples](2026-11-03-burr-xi-data/samples.json) are binary64 observations.
Exponentiated exact log examples and numerically located zero/equal-face
roots are nearby proxies, not claims of exact equality. The earlier eight
rational certificate groups are rerun separately. The exact tie existence
argument remains the continuity argument in the
[closure report](2026-11-03-burr-closure-report.md); no exact tie sample
or automatic tie classifier is claimed here.

The prototype combines the signed cutoff with the earlier small-k exclusion,
fixed-k entropy dual and convex k-chord envelope. It retains the better of
the signed cutoff and the earlier unsigned bound against a finite candidate.
The likelihood-value target is `1e-6*n`, with at most 512 cached conditional
profiles per workload. Existing conditional tolerances and empirical dual
padding `1e-9*n` are inherited from the previous study. The seam solver uses
bracketed intercept and power scores with absolute/relative root tolerances
`2e-14`; shape-bracket expansion has an 80-step budget.

| Workloads | Previous value target reached | New value target reached |
|---|---:|---:|
| Original burr-cert samples | 27/39 | 39/39 |
| Added closure workloads | Not run in that study | 11/11 |

There are 4,588 cached enclosure profiles in the new run; the separate
81-point-per-sample root reconnaissance and its scalar refinements are not
included in that count. Nineteen samples use a signed-tail construction,
checked at three interior/end points each. For the ten seeded near-Weibull
samples the signed cutoff is k=64 or 128, versus the earlier numerical
tail cutoff 26,214,400. Weibull quantiles use 128; nearly constant data use
256. All diagnostic budgets complete, but the label remains `value-gap-only`.

[diagnostics.csv](2026-11-03-burr-xi-data/diagnostics.csv) records gaps,
cutoffs and constants; [roots.csv](2026-11-03-burr-xi-data/roots.csv) records
numerical score crossings. Absolute diagnostic likelihoods omit the constant
Jacobian after standardizing log data; gaps and seam derivatives are unchanged.
The unchanged C++ fitter is replayed on all 50 samples in
[existing-fitter.csv](2026-11-03-burr-xi-data/existing-fitter.csv), whose
likelihoods instead include the original-data Jacobian. Its statuses are
23 finite fits, 12 Weibull and 15 Pareto; this replay does not certify them.

The numerical golden root is
`(k,c,s)=(1.3346374721018175,1.8604177041344703,1.1969911178454098)`.
Its relative differences from the unchanged golden coefficients are
`(1.84e-7,4.33e-8,1.60e-7)`, within `kBurrGoldenTol=1e-6`. This establishes
numerical compatibility, not a coefficient enclosure.

Independent 75-digit conditional solves on seven selected workloads check
the seam derivatives, containment and quadratic bound, including the golden,
near-Weibull, nearly constant, negative-slope and near-zero-slope cases.
[high-precision.csv](2026-11-03-burr-xi-data/high-precision.csv) retains the
results. Separately, 200 seed-13803 scalar checks at 70 digits test the
Taylor bound and derivative integral with tolerance `1e-55` (derivative
identity tolerance `1e-50`); all pass. These corroborate the formulas and
numerical implementation. They do not establish uniform arithmetic errors.

## Open certificate requirements

**Open: finite maximizer and coefficient isolation.** The study locates
numerical score crossings and encloses values. It does not prove that every
competing stationary point has been isolated, prove local uniqueness in a
surviving root neighborhood, or exclude every other region below that root's
maximum. Conditional concavity and the curvature upper bound do not give
the missing negative profile curvature. A finite mesh can miss crossings.
The derivation states explicit root/nuisance enclosure widths sufficient
to control k, c and s; no such proved widths are supplied by this study.
This is the remaining mathematical certificate step, not a request for an
exactly representable root or a theorem of joint global uniqueness.

**Open: a production arithmetic margin.** The general theorem uses the
exact Weibull solution. A production route must bracket it and propagate
allowances into W, A, the nuisance ball and the matrix bound. Substituting
an approximate root leaves a nonzero nuisance score and a constant error
term, which cannot certify a signed statement all the way to xi=0.
Dual feasibility/intercept residuals and all dominance comparisons also
need controlled allowances. The exact rational example settles this for
one workload; the floating-point study does not settle it generally.
Neither its search gap nor its empirical dual padding is a justified
practical floating-point margin for the requested estimator. Outward-rounded
interval arithmetic is not being demanded as an additional prerequisite.

**Open: degenerate seam slopes and tie decisions.** The signed theorem
requires strictly negative A. The exact zero-slope family has positive C_W
and finite improvement, so treating a small slope as zero/Weibull would be
wrong. Higher-order or interval comparisons are needed for a reliable
decision in these regimes; an explicit numerical budget failure is also
permitted. This work supplies no new exact finite/face tie classifier.

These are precise gaps in the attempted estimator, not impossibility
claims. No capped estimate or unchecked candidate is promoted to a result.

## Final validation and reproduction

Every final run below finished successfully on the local workstation,
through the required slot locks. The runners use `nice -n 10` and thread
limits; no job was detached or left running. No fixtures were regenerated.

| Command after the environment setup below | Machine | Result |
|---|---|---|
| `/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-03-burr-xi-data/run.sh` | Local, one slot | 50/50 value diagnostics; 57 tail checks; seven independent workload checks; 200 scalar checks; old exact certificates and C++ replay passed |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-xi-data/exact.py` | Local, one slot | Exact signed tail and positive seam curvature passed |
| `/tmp/umvl-cpu/run4 just cpp-test` | Local, four slots | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 just test` | Local, four slots | FAIL 0, WARN 0, SKIP 153, PASS 20507 |
| `/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test` | Local, four slots | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 just test-full` | Local, four slots | FAIL 0, WARN 0, SKIP 0, PASS 43324 |

The dev skips are tier/optional checks; there are no permitted failures.
These regression suites validate the unchanged implementation on their
workloads. They do not supply the missing all-free estimator certificate.

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
```

Environment: Ubuntu 26.04.1 LTS, Linux `7.0.0-38-generic`, GCC 15.2.0,
R 4.6.1 (2026-06-24), BH 1.90.0.1, Python 3.14.5, NumPy 2.4.6,
SciPy 1.18.0 and mpmath 1.3.0. Scratch executables and captured logs are
under `build/scratch/`; all lasting data, scripts and proofs are beside
this report. Existing source, goldens, truth tables and tolerances are unchanged.

`git diff --check`, the unchanged-tracked-files check, and whitespace/local
link checks on the 16 retained files passed (27 local Markdown links).
The artifact viewer rejected this worktree path with HTTP 403 because it
is outside its configured readable roots. The report remains at the requested
path; no viewer configuration or destination outside this worktree was changed.
