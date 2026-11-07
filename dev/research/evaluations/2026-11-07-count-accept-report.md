# TASK-116.26: tnorm, CMB and CMP acceptance certificates

Completed on 2026-11-07 in `codex/count-accept`, based on
`3de415f0896527001caeb0d612bcb91b858d644d`. No commits, board edits or package
installs were made. The [source manifest](2026-11-07-count-accept-data/source.sha256)
was printed by the final simbox run and subsequently verified against all 17
local source files. All final validation runs finished green.

## Implemented scope

| Family | Accepted fitting routes | Catalog flag |
|---|---|---|
| tnorm | Every parent-parameter hold subset, after exact profiling of free bounds; untruncated closed forms; quantitative acceptance for truncated interior solves; collapse, face, numerical-tie and numerical stops | `yes` |
| CMB | Every fixed-size hold subset, including negative dispersion, atomic and Bernoulli reductions; quantitative likelihood and coefficient acceptance | `partial`, because the separate free-size routes do not all enclose their coefficients |
| CMP | Every hold subset through `cmp_domain_mle`, the common R/JS route; joint and scalar interior acceptance, geometric KKT acceptance, exact zero-rate and held-geometric reductions, existing exposed-face stops | `yes` for the public domain route |

The implementations and current mathematical qualifications are in the
[tnorm dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md#numerical-acceptance),
[CMB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md#fixed-size-coefficient-acceptance)
and [CMP dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#moment-error-acceptance).
The shared acceptance helper lives in
[cmb/acceptance.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/acceptance.hpp),
keeping changes inside the named dossiers. No shared core header, protected
`special/` implementation, d/p/q/r, truth fixture, code generator or golden was
changed. The JS test change is append-only, as are the family C++ tests.

## Certificate and proof

Let `f` be the **per-observation** log likelihood in free canonical coordinates.
For a feasible evaluation point `y`, concavity gives

```text
f(z) <= f(y) + g(y) . (z-y).
```

Two opposite scalar scores, or three planar scores positively spanning the
plane, provide positive weights summing to one and cancelling the free scores.
For `y_j = theta + d_j`, their weighted supporting planes give the global bound

```text
sup f <= sum_j w_j * (f(y_j) - g(y_j) . d_j).
```

This bounds the entire proper parameter domain and its limiting likelihoods;
it does not assume a unimodal numerical search or truncate a parameter tail.

There is also a coefficient certificate. Gradient monotonicity, together with
the first-order optimality condition at a maximizer `z*`, implies

```text
g(y) . (z*-y) >= 0.
```

Thus the maximizer lies in every evaluated score half-space. In one dimension
these are a bracket; in two dimensions the positive score surround makes their
intersection a bounded triangle. Pairwise boundary intersections give its
vertices and an enclosing canonical rectangle. The family transforms that
rectangle back to its reported coefficients and checks every relevant endpoint
or corner. A flat likelihood therefore cannot pass solely because its objective
gap is small. The inverse sufficient-statistic covariance only proposes the
surrounding points; no unproved uniform curvature bound is used.

The required concavity is strict in each identifiable interior subset:

- For tnorm, no nonconstant linear combination of `U` and `U^2` is constant on
  an interval. Holding parent mean or scale gives an affine canonical line.
  Tightening a free bound to its sample extreme profiles it exactly.
- For fixed-size CMB with size at least two, the statistics `k` and
  `log choose(N,k)` are affinely independent: the two endpoints force the linear
  count coefficient to vanish, and an interior point then forces the other
  coefficient to vanish.
- For positive-rate CMP, evaluating a putatively constant combination of `k`
  and `log(k!)` at zero, one and two proves the same independence. The proper
  natural-parameter domain is convex. Existing hull and geometric-face tests
  classify the possible maxima and exposed limits before acceptance.

On the free-dispersion CMP geometric face, the rate bracket encloses its scalar
mean root. Both bracket endpoints must have a strictly negative dispersion
score. Since `Cov(X, log(X!)) > 0`, that score is monotone in log rate and has the
same sign at the enclosed root. The weighted rate scores cancel, while their
nonpositive dispersion score gives a supporting-plane upper bound for every
`nu >= 0`. A sign-uncertain seam stops; the legacy proposer's near-geometric
likelihood comparison alone cannot authorize a geometric estimate.

These are exact-arithmetic arguments **conditional on the computed moments,
scores and comparisons**, with practical floating-point margins. They are not
outward-rounded arithmetic proofs. The helper pads its per-observation upper
bound by `1e-9*(1+abs(f)+abs(unpadded_upper))` and requires its gap above the
reported likelihood to be at most `1e-8*(1+abs(f))`. Canonical intervals receive
`128*epsilon*(1+max(abs(low),abs(high)))` padding. CMB and CMP coefficient
enclosures must agree with each reported free coefficient within
`1e-8*(1+abs(coefficient))`; tnorm uses `1e-8*(standardization_scale+abs(mean))`
for mean and `1e-8*sd` for scale. CMP additionally requests evaluator remainder
tolerance `2e-16`, requires its reported relative remainder below `1e-12`, and
uses the documented `1e-12` KKT score separation. All likelihood bounds are
returned in total, original-data units.

## Numerical choices, stops and uniqueness

The surround starts at inverse-information displacements of size `1e-4` and
shrinks by four. Refinement has at most `min(iterlim,100000)` passes, with two
or three evaluations per pass, below the common certificate-evaluation
allowance. Failed surrounds, coefficient enclosures or arithmetic stop
numerically, retaining feasible witnesses and any established global upper
bound. Explicit table-work exhaustion is a structured budget stop. Common
binding routes supply the same stop kinds to R and JS.

Tnorm acceptance uses empirical centering rather than a distant parent mean.
The old likelihood line search could stall at roundoff with per-observation
moment errors around `1e-8` in the existing near-face example. It now permits
at most a `64*epsilon*(1+abs(f))` objective decrease when the free moment
residual strictly improves; the quantitative certificate remains mandatory.
Finite-versus-face likelihood comparisons within the existing tolerance retain
both witnesses as numerical ties. Squared-distance underflow on distinct data,
or on constant data away from a held mean's projection, is a numerical failure,
not a false collapse claim.

CMB retains passing historical proposals exactly. A failed coefficient
certificate can trigger one solve at moment tolerance `1e-14`; that solve and
its own certificate share the remaining enumeration ledger. In the deterministic
97-size scan, 39 fits used refinement and the largest dispersion change was
`1.81853465619497e-9`. Every fit passed. This is numerical evidence about those
inputs, not a convergence guarantee for all samples.

CMP uses the affine coordinate `alpha = eta - slope*nu` and the protected
evaluator's centered log-factorial residuals, avoiding subtraction of large
nearly equal moments. The new acceptance ledger uses 64-bit budget arithmetic
on native and WASM builds. See the inherited prepass limitation below.

Uniqueness has structural exceptions. At CMB size zero both remaining
coefficients are unidentified. At size one any dispersion gives the same
Bernoulli law; the sample `(0,1,1)` has the same maximum at dispersions `-20`
and `20`. For CMP data `(0,0)`, both `(lambda,nu)=(0,0)` and `(0,30)` attain
likelihood one. These are counterexamples to unrestricted coefficient
uniqueness, not optimizer failures. Identifiable finite tnorm maxima are
unique; collapse and coincident profiled bounds have no finite continuous MLE.

## Validation

Final commands, machines and results are recorded in
[validation-summary.json](2026-11-07-count-accept-data/validation-summary.json).
Simbox used GCC 15.2 and R 4.6.1; the workstation compiler was GCC 15.2.
Jobs used the supplied slot runners, one BLAS/OpenMP thread and lowered
priority; native builds and parallel test suites used four workers.

| Final command | Machine | Result |
|---|---|---|
| `UMVL_TEST_TIER=dev just cpp-test` | simbox | 106/106 shards; 61.37 s test time |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129; 111.07 s |
| `just cpp-test-no-long-double` | simbox | 129/129; 99.81 s |
| `just test` | simbox | 21,017 pass, 0 fail, 0 warn, 154 skip; 73.1 s |
| `just test-full` | simbox | 43,837 pass, 0 fail, 0 warn, 0 skip; 371.4 s |
| `just js-build`, `just js-test` | workstation | build succeeded; 439/439, no skips; 89.29 s tests |
| `just site-check` | workstation | exit 0 |
| `just relations` | workstation | 157 edges, 0 failed |

All 154 R dev skips require the full tier and were covered by the zero-skip
full run. The added family tests have no permitted-failure annotations.
CTest output reports shard outcomes rather than separate assertion-level
permitted-failure counts for unrelated families.

The final runs include independent R optimizer comparisons, existing truth
checks, exhaustive held subsets and golden regression. New cases check a flat
quadratic with a known maximum, boundary KKT signs across a bracket, all regular
hold subsets, non-uniqueness, numerical stops and budget witnesses through R
and JS. The deterministic CMP budget case `(0,0,0,20000)`, held rate
`5000/5001`, stops with a geometric witness at `iterlim=1` and fits dispersion
zero at `iterlim=10` in native, double-width and JS checks.

The standalone [checker](2026-11-07-count-accept-data/check.cpp) and
[output](2026-11-07-count-accept-data/check.txt) replay all four regular hold
subsets on each golden. All free-fit coefficients and all likelihoods reproduce
the goldens exactly; the only nonzero scaled held-subset coefficient difference
is CMP dispersion at `6.953723423845253e-13`. No golden was regenerated.
New workloads are deterministic; existing seeded workloads retain their seeds
in the test sources.

An initial development C++ dev run failed CMB coefficient acceptance and two
near-face tnorm checks. The refinement and roundoff-floor changes above resolved
them before the final runs. That failure is retained separately from final
successes. Full compressed logs and reproduction commands are in the
[evidence directory](2026-11-07-count-accept-data/README.md).

## Before/after timing

The [timing program](2026-11-07-count-accept-data/timing.cpp) fits
`(0,0,1,1,2,2,3,4)`: tnorm bounds `(-2,8)`, CMB size eight, and free CMP.
The baseline executable was compiled before edits at the stated base revision;
both executables used C++17 and `-O2`. Each row is the median of five
100-fit process means, measured under one workstation slot.

| Family | Before, microseconds/fit | After, microseconds/fit |
|---|---:|---:|
| tnorm | 20.124 | 74.926 |
| CMB | 600.167 | 741.019 |
| CMP | 827.560 | 2350.500 |

These short wall-clock measurements include shared-workstation variability;
they measure certificate overhead on this workload, not a general performance
bound. Raw measurements and their summary are retained in the evidence folder.

## Open entries

1. **CMB free-size coefficient enclosure.** The requested fixed-size route is
   certified. Free-size routines still use `size_candidate`, its rescue and
   historical fixed-size work calls with entropy likelihood bounds; they do not
   all invoke the new coefficient enclosure. Completing this requires threading
   quantitative coefficient acceptance through those internal candidates while
   preserving their shared ledger and size/tail bounds. No complete free-size
   uniqueness classification is asserted. The family flag therefore remains
   `partial`.
2. **Protected CMP proposer/evaluator metering (TASK-146).** The new pass meters
   its own remaining work, but the unchanged shared solver reports its prepass
   work only after it returns. The low-budget case above already reports
   **257,114** units before the wrapper sees an allowance of **100,000**. The
   wrapper correctly stops with its feasible witness and performs no unchecked
   acceptance, but it cannot retroactively enforce a cumulative prepass budget.
   A metered shared-evaluator/proposer interface, including its existing
   `max_terms` control and prepass counter width, is still needed. The historical
   raw `cmp_mle` entry point also remains a proposer; library callers requiring
   this certificate must use `cmp_domain_mle`. The brief reserves the protected
   shared work ledger for TASK-146, so `special/cmp.hpp` was left unchanged.

Floating-point outward rounding is not an open requirement here: the
implemented claim is the package's conditional exact-arithmetic certificate
with the stated practical margins. No inference or coverage claim follows
from these fitting certificates.

Artifact delivery: `bin/show` rejected this worktree with HTTP 403 because it is
outside the viewer's configured readable roots. The report remains at the
requested path; no files or viewer configuration were moved outside the
authorized worktree.
