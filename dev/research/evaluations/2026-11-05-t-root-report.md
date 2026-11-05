# TASK-140: location-scale t root boxes and face decisions

The **lst and std defaults remain unchanged**. The root/coefficient and
lower-face mathematics now has an executable research certificate: 60 of
62 retained family/sample pairs have a certified finite or face outcome,
and two stop at the explicit subdivision budget. Both golden coefficient
boxes lie wholly within the unchanged tolerance. The two older multimodal
examples also have certified global winners, with inclusion boxes for all
five retained stationary candidates.

The requested **production C++ fitter and structured stop propagation are
not delivered**. This is the remaining implementation gap; the research
program must not be substituted for the portable library. The precise open
entries below explain why no default switch is made.

All requested development, no-long-double, JavaScript and full regression
suites completed green locally before this report was written. Tested source:
`fe4573e9b02a950a1440039a8395c30ff2f25926`, branch `codex/t-root`, with the
uncommitted evidence and dossier changes identified in the
[validation manifest](2026-11-05-t-root-data/validation.json).
The [evidence index](2026-11-05-t-root-data/README.md) gives reproduction
commands. No fitter, binding, production test, default, golden, tolerance,
sstd source, settled d/p/q/r, special-function seam, truth fixture or code
generator was changed. The only changed pre-existing files are the lst and
std dossiers. No Git write, board edit, sub-agent, installation, simbox,
publication or other-worktree edit was used.

## Root inclusion, coefficients and global exclusion

The [derivation](2026-11-05-t-root-data/derivation.md) supplies the formulas,
proofs and arithmetic contract. Use ordinary scale s, a=s sqrt(nu), and
coordinates (mu, log(a), w=nu/nu0). The total negative likelihood has a
particularly simple analytic Hessian in these coordinates; its shape/shape
entry is the positive monotone gamma-ratio derivative -n c''(nu).

For a proposed stationary point p, the checker encloses the preconditioned
score e and derivative defect Q throughout a product box of radii r. It
requires e+Q r<r componentwise and ||Q||_infinity<1. A contraction and
strict self-map then prove existence and uniqueness of one root in that
box. Solver convergence alone never accepts a candidate.

A separate, larger box bounds the nuisance Hessian below by lambda I and
the center nuisance score above by G uniformly over a shape interval.
G<lambda r forces a nuisance stationary point inside its ball at every
shape. Fixed-shape geodesic convexity identifies it as the unique global
nuisance optimum. An enclosed congruence of the full Hessian proves strict
positivity throughout that neighborhood; the Schur complement bounds
profile curvature strictly below zero. The small root box must fit inside
this neighborhood. Thus every conditional nuisance optimum in the retained
shape interval is covered, rather than just one numerical nuisance solve.

Outside the winning neighborhood, the
[global driver](2026-11-05-t-root-data/global.py) strictly excludes every
finite shape interval with the earlier profile chord envelope. It integrates
the signed normal tail when the slope is negative and the unsigned tail
when a strictly better finite witness exists. An unresolved interval stops;
no grid endpoint, capped shape or likelihood-value-only proposal becomes a fit.

Displayed golden endpoints below are rounded outward from the
[stored boxes](2026-11-05-t-root-data/roots.json):

| Sample / public coefficient | Root enclosure |
|---|---|
| lst mu | (0.07455070869, 0.07455070908) |
| lst sigma | (1.02709471505, 1.02709471537) |
| lst df | (3.02424270912, 3.02424270973) |
| std mean | (-0.006260316034, -0.006260315868) |
| std sd | (0.83950573677, 0.83950573712) |
| std nu | (3.97766630662, 3.97766630743) |

Maximum relative discrepancy of **any point in the actual stored box** from
the frozen golden is 5.442e-9 for lst and 8.926e-6 for std; both are below
3e-5. These are conditional enclosure results, not exact rational root
certificates. Independent 80-digit roots lie inside every checked box.
The larger golden shape neighborhoods have profile second derivative upper
bounds below -1.4867 and -0.4222 respectively, in total-likelihood units.
These Hessians concern optimization, not Fisher information or inference.

The [multimodal check](2026-11-05-t-root-data/adversarial.json) isolates all
five retained stationary candidates from the two previous examples. The
global winners near nu=33.9054 and nu=49.7052 are certified in both families;
the four global searches use 464, 461, 1348 and 1355 profiles. Root proposals
do not establish root completeness: the exhaustive upper envelopes exclude
every remaining shape interval, including missed modes. General shape
unimodality or uniqueness for arbitrary samples is not asserted.

## Lower faces, equality masses and zero normal slope

At a critical atom b with k=q(n-k), q=1 or 2, pairing Busemann terms proves
the exact fixed-shape supremum

```
C_b = n c(q) - (q+1) sum_(x_i != b) log|x_i-b|,
c(q) = log Gamma((q+1)/2)-log Gamma(q/2)-log(pi)/2.
```

It is attained only for the balanced two-point Cauchy exception. For t2 it
is unattained at positive ordinary scale. These statements follow from
the exact sum-of-squares identity in the derivation; an early equality
stop is not used as evidence that collapse wins the all-free problem.

The convex profile-minus-normalizer argument includes this finite endpoint
even when its nuisance scale maximum is unattained. Its sharper chord bound,
for 0<=u<=h, is

```
P(q+u)-P(q) <= (u/h)[P(q+h)-P(q)] + n u(h-u)/(2 q^2).
```

Consequently an upper secant D satisfying D/h+n h/(2q^2)<0 excludes the
entire punctured lower neighborhood. The exact P(q) cancels before any
numerical enclosure is substituted, avoiding a constant endpoint rounding
error. This handles an ordinary negative t2 slope and the critical-tie cusp
without requiring a separately implemented cusp remainder. Remaining shapes
and the normal tail must still be excluded. Positive-scale Cauchy and t2
endpoints also receive two-coordinate root inclusion boxes.

Two exceptional sample classes have complete symbolic decisions:

* Balanced two-point data have a strictly increasing optimized profile
  toward normal for every nu>1, hence for both fitting domains. The
  attained nonunique Cauchy endpoint is strictly dominated.
* Mass 2/3 at b and 1/6 each at b-d and b+d has a strictly decreasing
  profile for nu>2. Its std supremum is the collapse path -n log(2d).
  The normal seam has **exactly A=0 and B=n/4>0**. Along the nuisance
  profile, ordinary scale squared is d^2(nu-2)/(3nu), while standard
  deviation remains d/sqrt(3). This is a collapse path, not a positive
  ordinary-scale t2 fit or a likelihood assigned to its atomic weak limit.

The integral inequalities proving both strict monotonicity statements are
in the derivation. They are mathematical proofs; finite high-precision slope
checks merely corroborate them. General vanishing normal-seam slopes and
exact finite/face ties still need guarded refusal when the implemented
bounds cannot decide them.

## Executed outcomes and arithmetic

The unchanged [31 frozen samples](2026-11-04-t-cert-data/inputs.json) include
both 500-observation goldens, seeded t/normal samples, contamination,
near-zero-slope clusters and critical ties. Seed: 14020261104.

| Research outcome | lst | std |
|---|---:|---:|
| Certified interior finite maximum | 9 | 6 |
| Certified attained Cauchy maximum | 8 | 0 |
| Certified normal supremum | 11 | 12 |
| Certified excluded positive-scale t2 supremum | 0 | 11 |
| Certified bounded collapse supremum | 0 | 1 |
| Proved unbounded collapse | 2 | 0 |
| Explicit subdivision-budget refusal | 1 | 1 |

The [independent checks](2026-11-05-t-root-data/independent.json) cover 17
interior root boxes and 19 endpoint root boxes, with 80-digit solves.
Independent numerical differentiation checks the analytic Hessian at
relative tolerance 2e-10. Whole-box golden tolerances, endpoint approaches,
exact critical moments, outcome invariants and budgets zero and one pass.
The five older multimodal root boxes are additionally checked against
their independently retained high-precision roots.

The vector interval operations use practical allowances of 64 binary64
epsilon times one plus endpoint magnitude, and n-dependent sum allowances.
Gamma derivatives and point scores use 65/70-digit evaluation before padded
conversion. The global value engine uses 65 digits, a 1e-48 base allowance,
an explicit precision-dependent enlargement, and nu<=1e30. Its original-data
normalization, score-to-value bounds and normal comparisons are evaluated
at high precision. These are research certificates conditional on the stated
computed enclosures. They are neither outward-rounded interval proofs nor
a completed portable binary64 error contract.

One material failed approach is retained in the derivation. Direct interval
evaluation of the scale score -n nu+(nu+1)sum a^2/D produced an artificial
width of order n nu h and prevented nuisance containment near normal. The
equivalent identities n-(nu+1)sum r^2/D and F_tnu=-sum r^2/D reduce that
width to order n h. This change enabled uniform neighborhoods for the
shallow root and the older multimodal winners; shrinking the former box
without removing this dependency did not solve the problem.

## Open entries and default decision

**Open — shallow global exclusion at the prescribed research budget.**
The normal-0 sample has an enclosed stationary shape near 218.257164105,
with a certified nuisance/profile-curvature neighborhood. Its finite witness
beats normal by about 0.00083428265 total log likelihood. Nevertheless,
each family exhausts 4096 cached profiles before all exterior intervals
are excluded. The lst record retains, for example, an unresolved interval
[216.40926054,216.43090861]. The [diagnostics](2026-11-05-t-root-data/shallow.json)
retain both families' root boxes, neighborhoods, tail thresholds and final
unresolved intervals. No global finite estimate is claimed for this sample.
Sharper exterior envelopes, larger neighborhoods or a larger verified budget
would be needed for a definitive outcome; a budget stop itself is an intended
guarded outcome, not evidence of a winning face.

**Open — portable production arithmetic and engine.** No bounded C++
implementation of these root, neighborhood, relative endpoint and tail
checks is supplied. In particular the mixed-precision point-score and
whole-profile evaluations cannot be silently replaced by the old binary64
scores or by absolute full-likelihood subtractions near the normal seam.
A production implementation needs propagated arithmetic bounds, domain and
linear-solve guards, certified coefficient-width checks, bounded proposal and
subdivision work, and numerical/tie/budget failures that cannot fall through
the existing local-score exception. No impossibility of such a port is claimed.

**Open — public diagnostic transport and production validation.** lst/std
results and common bindings still lack the new certificate outcomes and
StopDetail payloads. Normal, ordinary-t2 and collapse candidates need explicit
model/coordinate treatment, preservation of original-unit likelihood bounds,
and R/JS condition tests. The existing broad lst half-mass NoMLE message
cannot be reused as proof of collapse dominance; balanced two-point data
instead select normal. Held dispatch and sstd must retain their behavior.
A native certified-fitter benchmark and portability checks remain missing.

These implementation and arithmetic gaps prevent the requested default
replacement. The dossier metadata therefore remains multistart/partial.
No change to the settled d/p/q/r, truth fixtures, special-function seam or
code generators is required by the mathematics delivered here.

## Timing against the current fitters

[Native measurements](2026-11-05-t-root-data/baseline.json) use C++17 -O2,
one discarded warmup and three subsequent calls, with their mean retained.
Research timings include the root-box stage when reused by the global search;
independent verification is excluded. Both ran locally through one-slot locks.

| Workload | Unchanged native C++ | Research Python/multiprecision |
|---|---:|---:|
| lst own golden | 12.81 ms | 8.136 s |
| std own golden | 3.362 ms | 11.777 s |
| Median of 31 lst workloads | 2.099 ms | 1.122 s |
| Median of 31 std workloads | 0.518 ms | 0.935 s |

The medians include face outcomes and budget refusals. This compares different
languages, arithmetic resources and guarantees. It is not a native port timing
or an acceptable production performance claim. Large-n certificate performance
and timing variability beyond these repetitions were not measured.

## Final validation and reproduction

Every final command completed in the foreground on the **local workstation**,
using the shared locks, nice 10, single-threaded BLAS/OpenMP, and worktree
temporary directories. Slot waits were allowed to finish. The required
launchers were initially absent and later became available; no replacement
was installed by this job. An intermediate research-wrapper run failed after
the script was edited while running; the frozen complete workflow was then
rerun successfully. No final failure was permitted.

| Final command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 just cpp-test-no-long-double` | 127/127 passed |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 154, PASS 20612 |
| `/tmp/umvl-cpu/run4 just js-build` | Completed; JS/Wasm artifacts hashed |
| `/tmp/umvl-cpu/run4 just js-test` | 418/418 passed, no skips |
| `/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43427 |
| `/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-05-t-root-data/run.sh` | Root, profile, multimodal, budget and native timing checks passed |

The final independent endpoint checks and manifest collection also passed
consecutively under a one-slot allocation. Development skips are tier omissions;
the full tier has none. Regression passes concern the retained fitters and
interfaces, not certification of their existing multistart decisions.

Environment: Linux 7.0.0-42 x86-64, glibc 2.43, GCC 15.2.0, R 4.6.1,
BH 1.90.0.1, Rcpp 1.1.2, testthat 3.3.2, Node 22.22.1, Python 3.14.5,
NumPy 2.4.6, SciPy 1.18.0 and mpmath 1.3.0. The manifest records source,
input, evidence and regression-log SHA256 identities. Final local-link,
syntax, JSON, hash and whitespace checks are recorded separately in
[final-checks.json](2026-11-05-t-root-data/final-checks.json).

The artifact-pane show command was attempted for this finished report. The
viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report remains at the requested path; no viewer
configuration or destination outside the worktree was changed.
