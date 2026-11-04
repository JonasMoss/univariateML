# TASK-140: location-scale t closure and certificate study

The default **lst and std fitters are retained**. This job establishes a
fixed-shape global nuisance argument, an attainment criterion, exhaustive
profile-value bounds, and signed normal-tail inequalities. It also gives exact
counterexamples to deciding the normal limit from sample kurtosis alone.
A certified global **coefficient vector** has not been established; the precise
remaining requirements are recorded below. The brief's instruction to leave
the default unchanged therefore applies.

All required development, no-long-double and full suites finished green on the
local workstation before this report was written. The final research workflow
also passed. Tested base: `54b0d06a3e447f944940fc0dd4b5b87235d2acc2`, branch
`codex/t-cert`. The [validation record](2026-11-04-t-cert-data/validation.json)
identifies the uncommitted evidence and source files by SHA256. Only the
[lst dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md)
and [std dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md)
are changed among pre-existing tracked files. Their fitting metadata remains
`multistart` / `partial`. No fitter, adapter, production test, golden,
tolerance, sstd source, settled d/p/q/r, special-function seam, truth fixture,
code-generation script, status file or roadmap is changed. No commit, Git
write, board edit, sub-agent, installation, publication or simbox was used.

## Mathematical results

The [derivation](2026-11-04-t-cert-data/derivation.md) gives the arguments and
conditions. Every likelihood is a **total** log likelihood. Write ordinary t
scale as `s`; std's standard deviation is `s sqrt(nu/(nu-2))`.

**Fixed shape.** In upper-half-plane coordinates `(mu,a=s sqrt(nu))`, put
`D_i=(x_i-mu)^2+a^2`, `b_i=log(D_i/a)` and `b_inf=-log(a)`. Up to its
shape-only normalizer, the negative likelihood is

```
(nu+1)/2 sum b_i + n(nu-1)/2 b_inf.
```

Each Busemann function has covariant Hessian `g-db tensor db`, which is
positive semidefinite. Thus the nuisance objective is geodesically convex
for `nu>=1`. It is strictly convex for `nu>1` with distinct observations,
and for `nu=1` with at least three distinct support points. Under the strict
mass condition `k_max<nu(n-k_max)`, the objective is coercive and its maximum
is attained and unique. This does not assert joint convexity in shape, or
certification of the existing numerical stopping rule. Fixed shapes below
one and held-scale location searches are outside this convexity claim.

**Containment and endpoint bounds.** Every nuisance maximum has location in
the sample range. For sorted observations, define
`d_k=min_i(x_(i+k)-x_(i))/2`. The scale score implies

```
s^2 >= max_k d_k^2 [(n-k)/n-k/(n nu)],
s^2 <= (1+1/nu) range(x)^2.
```

Using a lower shape bound makes these uniform. At a numerical proposal p,
geodesic convexity bounds the optimal likelihood by
`ell(p)+||grad(-ell)(p)||_g D`, where D bounds hyperbolic distance to that
containment rectangle. Its maximum occurs at a corner. This supplies a
profile upper bound without accepting solver convergence as proof.

**Every finite shape interval.** With
`c(nu)=log Gamma((nu+1)/2)-log Gamma(nu/2)-log(sqrt(pi))`, the function
`P(nu)-n c(nu)` is convex: it is a supremum of affine functions in nu.
Since `0<-c''(nu)<1/nu^2`, an interval `[lo,hi]` has the global envelope

```
P(nu) <= max(P(lo),P(hi)) + n (hi-lo)^2/(8 lo^2).
```

Endpoint upper enclosures can replace the exact endpoint values. Subdivision
therefore covers missed shape modes; a grid does not need to find every root.
The envelope alone supplies a likelihood-value enclosure, not coefficients.

**Closure and attainment under strict masses.** For lst assume
`k_max<n/2`; for std assume `k_max<2n/3`. Uniform nuisance containment,
profile continuity and convergence to the optimized normal likelihood imply:

- lst has a finite global maximum exactly when some finite admissible point
  reaches or exceeds the optimized normal likelihood. Cauchy is an attained
  member of its fitting domain.
- std has a finite global maximum exactly when some point with `nu>2`
  reaches or exceeds both the optimized normal and ordinary-scale t2
  likelihoods. Its positive-scale t2 face is excluded from the mean/sd
  parameterization.

Equality is included in these criteria. A limit tied with an attained finite
point does not itself prove non-existence. These are profile-likelihood
closure statements under the stated mass conditions, not an unrestricted
classification of every weak limit.

**Collapse.** The exponent at an atom of multiplicity k is
`nu(n-k)-k`. Hence `k>n/2` makes lst unbounded, while `k>2n/3` makes std
unbounded by choosing nu sufficiently close to two. At equality, the
additional scale-zero face and mixed shape/scale paths must be compared with
the other candidates. The derivation supplies its face value. The exceptional
Cauchy fit on two equally weighted support points can be attained and
nonunique. Equality is not classified by merely declaring collapse.

## The normal seam and counterexamples

Let `xi=1/nu`, `s_N^2=mean((x-mean(x))^2)`,
`z=(x-mean(x))/s_N` and `m_j=mean(z^j)`. After refitting both nuisances,

```
P(1/xi) = P_normal + A xi + B xi^2 + O(xi^3),
A = n (m4-3)/4,
B = n [m4/4-m6/6+m3^2/2+(1-m4)^2/4].
```

The two positive terms in B are the nuisance-refitting correction. A positive
A excludes the normal as a global winner. A negative A establishes only local
preference for the normal. This expansion is proved asymptotically; it is not
an implemented uniform third-order remainder.

The [exact rational checker](2026-11-04-t-cert-data/exact.py), with
[results](2026-11-04-t-cert-data/exact.json), uses the sample

```
[-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]/100.
```

Its slope is exactly `A=-1170351/50160128<0`. Nevertheless, the explicit
point `mu=0, s=1/3, nu=3`, admissible in both families, beats the fitted
normal by an amount in
`[0.491870907498,0.491870907499]`. This is an exact rational proof using
Machin's identity for pi and bounded logarithm series; it uses no optimizer
or floating-point comparison. The normal is locally best at the seam but
is beaten at finite shape. Global profile unimodality and a normal decision
based solely on kurtosis at or below three are therefore invalid.

The checker also constructs an algebraic member of the same symmetric
cluster family with **exactly A=0 and B>2**. The nearby binary64 zero-slope
proxy is identified separately; its rounded observations are not asserted
to have exactly zero slope.

Two uniform signed tail bounds were developed:

- The [global-rectangle bound](2026-11-04-t-cert-data/signed-tail.md) proves
  `P(nu)-P_normal <= A/nu+K/nu^2`. For A negative it excludes every
  `nu>=max(2,2K/(-A))`. Its computed thresholds on 15 workloads range
  from `1.11e6` to `1.61e28`.
- The [nuisance-ball bound](2026-11-04-t-cert-data/local-tail.md) encloses
  the analytic nuisance Hessian, checks strict inward score conditions,
  and uses geodesic convexity to identify the resulting nuisance root as
  global. It gives the same signed form with much better constants:
  thresholds range from **8 to 134217728** on those 15 workloads.

The second bound removes the need to use the enormous first thresholds on
ordinary samples. Independent 80-digit evaluations at N and 2N agree with
all 15 local bounds. Separate 110-digit checks corroborate the conservative
bounds. The inequalities supply the uniform mathematical argument; checking
two points numerically is not its proof. Operational floating-point padding
for these interval comparisons remains a production requirement.

Hasannasab et al. discuss joint t estimation, existence and the normal limit
in their [primary paper](https://arxiv.org/pdf/1910.06623). The arguments
used here are supplied explicitly; no novelty or global-convergence claim
is inferred from that citation.

## Executed workloads and accuracy

The [frozen inputs](2026-11-04-t-cert-data/inputs.json) contain **31 samples**:
both 500-observation goldens; two seeded 80-observation samples each from t
with df 1, 2, 3, 5, 30 and 1000; three normal samples; contamination; five
cluster examples; three near-zero seam slopes; and five tied samples.
Seed: `14020261104`. The original frozen goldens are copied as data and
are never regenerated.

The [Python engine](2026-11-04-t-cert-data/study.py) uses nuisance enclosures,
finite-interval envelopes and a proved unsigned whole-tail exclusion when
a finite witness beats the normal. Its default budget is **768 cached
profiles**, and its target is **0.001 absolute total log likelihood**.
The initial grid and scalar refinements are proposals. The signed tail
bounds are checked separately and are not silently substituted into this
engine's normal-outcome logic.

| Research outcome | lst | std |
|---|---:|---:|
| Supremum value enclosed within 0.001 | 17 | 17 |
| Unsigned-tail method leaves normal decision open | 10 | 13 |
| Collapse equality left open | 2 | 1 |
| Proved unbounded | 2 | 0 |

The maximum cached-profile counts are 100 and 98. Explicit budgets of zero
and 40 on the lst golden exercise budget refusal. Those refusals do not
become accepted estimates. Diagnostic proposals in JSON are not certified
coefficient vectors. A value enclosure at the t2 face does not establish
attainment in std. Native std strict-collapse cases are also covered by the
existing all-free tie test with multiplicities seven and eight in
[std/test.cpp](../../../univariateML-cpp/include/cppunivariate/distributions/std/test.cpp).

The [independent checks](2026-11-04-t-cert-data/independent.json) re-evaluate
57 retained nuisance proposals at 70 digits; all their optimized values
fall inside the computed proposal enclosures. Maximum absolute discrepancy
from the Python likelihood is `3.04e-11`. Two independent joint stationary
roots reproduce the frozen golden coefficients within maximum relative
error `8.912e-6`, below the unchanged `3e-5` golden tolerance. Most of that
discrepancy is the existing std golden's location coefficient. Neither
frozen values nor test tolerances were altered. Numerical stationary roots
and agreement with goldens do not establish global coefficient certification.

The research enclosures are conditional on their computed numerical bounds.
The [derivation](2026-11-04-t-cert-data/derivation.md) states the practical
value/score allowances, including the base allowance
`256 epsilon (n+1)(1+abs(value)+sum(abs(x)))`, gamma evaluation allowances
and an explicit large-nu gamma-ratio interval. These have not been completed
into a justified operational error contract for every primitive and extreme
input. Outward-rounded interval arithmetic is not required by the brief and
is not being asserted as a prerequisite. The exact rational counterexamples
are independent of these floating-point allowances.

## Timing against the unchanged fitter

[Native measurements](2026-11-04-t-cert-data/baseline.json) use an optimized
C++17 driver, one discarded warmup and three subsequent calls per case.
Research timings include Python proposals and value subdivision, with one
run per case. Both run under the local one-slot lock with nice priority 10
and single-threaded numerical libraries.

| Workload | Current C++ | Research Python value engine |
|---|---:|---:|
| lst on its own golden | 13.68 ms | 303.30 ms |
| std on its own golden | 6.60 ms | 325.54 ms |
| Median of all 31 lst workloads | 2.17 ms | 75.43 ms |
| Median of all 31 std workloads | 0.520 ms | 93.67 ms |

The medians include open outcomes and collapse refusals. The Python column
excludes independent high-precision and rational checks. This compares
different implementations and different guarantees; it is not a C++ port
benchmark or evidence that coefficient certification would cost that amount.
The prototype repeatedly scans order-statistic spans in quadratic time;
production should cache them or retain a selected valid set of containment
bounds. Large-n certificate performance was not measured.

## Open entries blocking a default replacement

**Open — coefficient and stationary-root certificate.** The engine encloses
the supremum value but does not enclose every surviving joint stationary root,
prove a root's inclusion and coefficient widths, or establish profile curvature
on a neighborhood containing every conditional nuisance optimum. A small
likelihood gap does not imply golden coefficient accuracy in a flat profile.
A complete root-neighborhood argument and exclusion of every competing shape
interval are needed before a finite proposal can become an accepted global
maximizer. The 70-digit roots are numerical corroboration, not that argument.
A general joint uniqueness theorem is not required; sample-specific guarded
root and dominance certificates would suffice.

**Open — complete face decisions.** The tighter signed tails need integration
with the finite-interval engine using relative likelihood arithmetic near the
normal seam. A negative local slope alone cannot decide the normal outcome.
At exactly zero slope, the proved second coefficient helps locally, but a
uniform signed remainder or an explicit guarded refusal is still needed.
For std, dominance or ties involving the excluded t2 profile must be decided;
for equality masses, the additional scale-zero faces and Cauchy exception
must be compared. Current early equality stops are not proofs that these faces
win. The research table names these unresolved outcomes rather than labelling
them normal or t2 winners.

**Open — guarded production engine and diagnostics.** No bounded C++ engine
with propagated practical arithmetic margins, explicit bracket/domain/linear
solve failures, root-box checks and budget/tie statuses is delivered. The
Python prototype uses SciPy and assertions and is not a library implementation.
A failed global certificate must not be rescued by the existing local-score
exception. Public diagnostics must distinguish a proved limit winner or tie
from exhaustion or numerical uncertainty. The existing
[lst binding](../../../bindings/common/distributions/lst/fit.hpp) uses a broad
`NoMLE` diagnostic for its half-mass early stop; it cannot simply be reused
as a certificate failure. The
[std binding](../../../bindings/common/distributions/std/fit.hpp) similarly
needs explicit propagation of future certificate outcomes. Public R failure
and held-dispatch tests would then be required. No changes to the settled
special-function seam, d/p/q/r, truth fixtures or code generators are needed
by the mathematics obtained here. sstd remains unchanged.

These are concrete gaps in the requested global coefficient contract. They
are the reason for retaining the current default, rather than returning a
capped df or relabelling a value-enclosed proposal as a certified estimate.

## Final validation and reproduction

Every command below finished in the foreground on the **local workstation**,
through the prescribed locks. The locks supply nice priority 10 and one
BLAS/OpenMP thread per worker. No waiting script was placed inside a CPU
allocation. The shared-slot waits were allowed to finish normally.

| Command | Final result |
|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 just cpp-test-no-long-double` | 127/127 passed |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 153, PASS 20511 |
| `/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43328 |
| `/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-t-cert-data/run.sh` | All final research, exact, high-precision and budget checks passed |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-t-cert-data/validate.py` | Regression logs parsed; outcomes, environment and source hashes recorded |

The last two steps were executed consecutively within one one-slot shell
allocation. The 153 development skips are expected tier skips. No final test
failure or warning was permitted. These suites establish regression health
of the retained package; they do not turn its multistart fits into global
certificates.

Environment: Linux 7.0.0-38 x86-64, glibc 2.43, GCC 15.2.0, R 4.6.1,
BH 1.90.0.1, Rcpp 1.1.2, testthat 3.3.2, Python 3.14.5, NumPy 2.4.6,
SciPy 1.18.0 and mpmath 1.3.0. The native diagnostic uses BH as a provider
of ordinary Boost headers; the C++ core does not link to R.

Reproduce from the worktree root:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-t-cert-data/run.sh
```

The [evidence index](2026-11-04-t-cert-data/README.md) links every retained
proof, input, script and result. Verbose temporary logs are under
`build/scratch/`; no report figure or linked result depends on that directory.
The [final document checks](2026-11-04-t-cert-data/final-checks.json) record
local-link, syntax, JSON, source-identity and whitespace checks after this
report is written.

The artifact-pane `show` command was attempted for the finished report. The
viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report remains at the requested worktree path. No viewer
configuration or destination outside this worktree was changed.
