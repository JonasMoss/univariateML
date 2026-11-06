---
id: sn
inference_numeric:
  - regime: "identifiable interior submodel"
    available: true
    condition: "finite xi, omega > 0 and finite alpha; any nonempty supported free subset except alpha = 0 with both xi and alpha free"
    classification: "dev/research/evaluations/2026-11-04-dqm-b-report.md#azzalini-sn-versus-st"
  - regime: "singular information"
    available: false
    condition: "finite xi, omega > 0 and finite alpha; alpha = 0 with both xi and alpha free"
    reason: "DQM holds but the free information has a null direction; ordinary observed or sandwich covariance is unavailable"
    classification: "dev/research/evaluations/2026-11-04-dqm-b-report.md#azzalini-sn-versus-st"
name: Azzalini skew-normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: xi
  symbol: '\xi'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: omega
  symbol: '\omega'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (-inf, inf)
  simulate: 0
  kind: regular
r:
  density: dsn
  sampler: rsn
  control:
    xi: "Optional fixed finite location; NULL estimates it."
    omega: "Optional fixed positive finite scale; NULL estimates it."
    alpha: "Optional fixed finite shape; NULL estimates it."
    reltol: "Tolerance on the normalized inner likelihood score."
    iterlim: "Maximum Newton iterations per inner fit."
log_density: "log(2)-log(omega)-log(2*pi)/2-((x-xi)/omega)**2/2+log((1+erf(alpha*(x-xi)/(omega*sqrt(2))))/2)"
limits:
  - model: hnorm
    firmness: proved
    when: 'The shape $\alpha$ tends to $+\infty$, giving a right translated half-normal limit.'
    detect: 'With shape free, the right half-normal supremum is at least as large as the best finite-shape likelihood, within the comparison tolerance. Any held location or scale is retained.'
    status: RightHalfNormalBoundary
  - name: reflected translated half-normal
    firmness: proved
    when: 'The shape $\alpha$ tends to $-\infty$, giving a reflected translated half-normal limit.'
    detect: 'With shape free, the reflected half-normal supremum is at least as large as the best finite-shape likelihood, within the comparison tolerance. Any held location or scale is retained.'
    status: LeftHalfNormalBoundary
references:
- Azzalini, A. (1985). A class of distributions which includes the normal ones. Scandinavian Journal of Statistics
  12, 171-178.
- Sartori, N. (2006). Bias prevention of maximum likelihood estimates for scalar skew normal and skew t distributions.
  Journal of Statistical Planning and Inference 136, 4259-4275.
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsn())
  mlsn(x)

group: skewed and robust location-scale
construction:
- skewing
- location-scale
relations: {}
fit:
  method: profile-search
  certified: partial
  note: 'sn/README.md, Details; sn/mle.hpp: free location and shape use interval and tail upper bounds conditional on computed comparisons; held-location and held-shape solves remain numerical.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model smoothly asymmetric Gaussian-like measurements under the Azzalini construction.
summary: Azzalini skew-normal provides a model for smoothly asymmetric Gaussian-like measurements under the Azzalini
  construction.
aliases: []
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all three coefficients free, a finite MLE need not exist: the
    supremum can lie at either half-normal limit. A finite point strictly
    above both limiting suprema ensures attainment for nonconstant data,
    but uniqueness of the joint fit is not established here. Constant data
    have unbounded likelihood when location and scale are free. Holding
    scale still permits a half-normal escape when shape is free. With a
    finite shape held, the remaining fit exists and is unique for
    nonconstant data. With location also held, this remains true whenever
    at least one observation differs from that location. With shape and
    scale held, the location fit always exists and is unique. With location
    held and shape free, observations strictly on both sides of the held
    location give a unique finite fit, whether scale is held or free.
    One-sided observations with at least one strict inequality instead
    give an infinite-shape supremum. If all observations equal the held
    location, free scale collapses to zero; with scale held, every shape
    has the same likelihood. Holding all coefficients leaves no estimation.
  reason: >-
    For fixed finite shape, the log-likelihood is strictly concave in
    inverse scale and location divided by scale. Gaussian tail bounds
    ensure attainment for nonconstant data. If location is held, put
    $d_i=x_i-\xi$, $\eta=1/\omega$ and $\beta=\alpha\eta$.
    Apart from a constant the likelihood separates as
    $n\log\eta-\eta^2\sum_i d_i^2/2+\sum_i\log\Phi(\beta d_i)$.
    The shape term is strictly concave and tends to minus infinity at
    both ends exactly when residuals of both signs occur. For one-sided
    nonzero residuals it is strictly monotone. For free location, the
    Gaussian envelope excludes scale collapse on nonconstant data and
    excludes divergent location or scale on any nonempty likelihood superlevel set.
    The remaining infinite-shape paths are bounded by the half-normal
    suprema described below. These arguments do not establish joint
    strict concavity or uniqueness when location and shape are both free.
---

The Azzalini skew-normal extends the normal distribution by a smooth shape parameter. The location $\xi$ and scale $\omega$ differ from its mean and standard deviation. Positive $\alpha$ gives right skewness, negative $\alpha$ gives left skewness, and $\alpha=0$ gives the normal distribution. This is distinct from the Fernandez–Steel skew-normal family.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-skewreal.md) derives the stated limits and held-parameter conditions.

At the normal member, Fisher information in these coordinates is singular when location and shape are both free. For each finite shape, the fitter maximizes over the free location and scale. It searches both signs of shape and compares the result with both half-normal limiting suprema. A limiting fit wins a numerical tie within $10^{-8}n$, where $n$ is the sample size. For free location and shape, the grid initializes a branch-and-bound search with exact-arithmetic interval and tail bounds, conditional on the computed comparisons. A half-normal supremum is a limiting fit, rather than a member with finite shape. A comparison within tolerance does not prove that no finite maximizer ties or slightly exceeds that limit.

Any subset of the three parameters can be held fixed. A fixed shape leaves a concave problem in inverse scale and location divided by scale. With fixed location, an infinite shape remains possible only if every observation lies on the corresponding side of that location. Observations equal to the fixed location contribute an additional $\log 2$ penalty per such observation relative to the closed half-normal density at the limit. A held scale is retained in the limiting comparison.

For finite $\alpha$, $\log\phi(z)$ is strictly concave and
$\log\Phi(\alpha z)$ is concave. Thus, with $b=1/\omega>0$ and
$u=\xi/\omega$, the fixed-shape likelihood
$n\log b+\sum_i\log[2\phi(bx_i-u)\Phi(\alpha(bx_i-u))]$ is strictly
concave in $(u,b)$ for nonconstant data. At $\alpha=0$ this is the normal
member; as $\alpha\to\pm\infty$ the corresponding translated half-normal
is a separate limiting face. The Newton inner solve uses this concavity. Its stopping check is numerical; the outer search bounds the global likelihood gap separately.

## R notes

Every coefficient can be held by supplying its value; `NULL` estimates it.
A detected right half-normal limit stops with a recommendation to use
`mlhnorm`; for the reflected limit, use `mlhnorm` on `-x` and reflect the
result. Use `mu = NULL` when location is free. Pass a held location as
`mu` and a held scale as `sigma` to `mlhnorm`. Negate the held location when reflecting. With location held,
observations at that location have the
endpoint penalty described above, so the closed half-normal likelihood
reported by that separate fit need not equal the skew-normal supremum.
Unresolved search bounds or an unsuccessful numerical score check stop
with an error. No penalised likelihood is used.

## Decision history

| TASK-116 property | Evidence |
|---|---|
| Fixed finite shape, nonconstant data | Strict concavity in inverse-scale coordinates, proved as above. |
| Computed inner maximum | Newton iteration and a floating-point score check; numerical. |
| Free location and shape, including held scale | Interval and tail likelihood bounds, conditional on computed comparisons; finite branch budget. |
| Fixed location with free shape | Separated concave one-dimensional shape score and a numerical bisection; no likelihood-gap certificate. |

An unfinished inner solve is accepted only when the normalized free-coordinate score at the returned point is below `max(reltol, 1e-3/n)` (with `n` observations). This is the Euclidean score norm in log inverse scale and scaled location divided by the sample size, with held coordinates removed.

### Global shape-search bounds (2026-11-02)

The following inequalities hold in exact arithmetic. Write
$z_i=\eta x_i-h$, with $\eta>0$, and standardize nonconstant data so that
$\sum x_i=0$ and $\sum x_i^2=n$. At any finite reference point
$(a_0,\eta_0,h_0)$, set $t_i=a_0(\eta_0x_i-h_0)$ and
$r_i=\phi(t_i)/\Phi(t_i)$. Concavity of $\log\Phi$ gives
$\log\Phi(a z_i)\leq\log\Phi(t_i)+r_i(a z_i-t_i)$. The resulting
upper bound is affine in $a$ for each $(\eta,h)$. Consequently its supremum
on $a\in[L,R]$ is the larger of its separate suprema at $L$ and $R$.
For free location and scale, put $q=\sum r_ix_i$, $s=\sum r_i$ and
$C=n\log(2/\sqrt{2\pi})+\sum(\log\Phi(t_i)-r_it_i)$. At either
endpoint $a$, the bound is maximized by
$h=-as/n$ and $\eta=(aq/n+\sqrt{(aq/n)^2+4})/2$, yielding
$C+n\log\eta-n(\eta^2+h^2)/2+a(\eta q-hs)$. For held scale,
use its fixed $\eta$ and the same maximizing $h$. This argument does not
require the reference point to be an inner maximum.

For $a\geq A>0$, $\Phi(t)\leq 1$ for $t\geq0$, while
$\Phi(t)\leq\exp(-t^2/2)$ for $t<0$. Hence the tail likelihood is at most
$n\log(2/\sqrt{2\pi})+n\log\eta-\eta^2Q_A(c)/2$, where
$c=h/\eta$ and
$Q_A(c)=\sum_i(x_i-c)^2[1+A^2\mathbf1\{x_i<c\}]$.
Its minimum $Q_A^*$ is obtained by a one-dimensional convex piecewise
quadratic minimization. With free scale the bound becomes
$n[\log(2/\sqrt{2\pi})+\frac12\log(n/Q_A^*)-\frac12]$;
with held scale use the fixed $\eta$ in the preceding expression. Reflect
$x$ for the negative tail. As $A\to\infty$, $Q_A^*$ tends to
$\sum_i(x_i-\min x)^2$, so this upper bound tends to the corresponding
translated half-normal face likelihood. The grid and local refinements supply feasible candidates, and branch-and-bound
bisects intervals until their upper bounds lie within $5\cdot10^{-9}n$ of
the largest candidate or face. The two unbounded tails are doubled until
their bounds meet the same threshold. A budget of 20,000 interval bisections,
unresolved bounds, or an unresolvable midpoint produces `IterationFailure`.
A face wins when within $10^{-8}n$ of the best finite point; the resulting
exact-arithmetic gap is at most $1.5\cdot10^{-8}n$. The practical comparison
allowance is $2\cdot10^{-8}n$ including floating-point effects, conditional on
the computed special functions and comparisons; it is not an outward-rounded
interval guarantee. Held location uses the separate score bisection and held
shape uses the Newton solve, both numerical rather than likelihood-gap bounded.
A [seeded spot check](../../../../../dev/research/evaluations/2026-11-02-sn-cert-data/check_bounds.py)
examines feasible points under both bounds; it is numerical evidence, not a proof.

### Accepted-point cache (2026-11-01)

The analytic Newton solve retains each observation's log-CDF at the last
accepted objective evaluation. Below the existing tail threshold
$\alpha z<-10$, the same 24-level continued fraction computes and retains
both the inverse Mills ratio and its derivative. The next Newton step
reuses these quantities; rejected line-search trials overwrite the cache,
and an accepted trial supplies all entries before the next step. If the
line search fails, the existing independent returned-point score check is
used. With both location and scale held, the objective is evaluated once
without computing an unused score or information matrix.

This cache changed evaluation cost, not the objective, shape grid, local
refinement, score gates or half-normal comparisons at the time. It adds three temporary
vectors of length $n$ per inner solve. Warm starts and a coarser outer grid
were not adopted: their effect on the existing numerical search and status
behavior would require separate evidence. The cache itself adds no optimality claim; the subsequent shape bounds are described above.
See the [speed evaluation](../../../../../dev/research/evaluations/2026-11-01-sn-speed-report.md)
for paired regression evidence, timings and remaining guarantees.

### Probability evaluation

Probabilities use Owen’s T and quadrature in the direct skew-normal parameters.

### Range portability (2026-11-01)

Density and CDF standardization use the extended difference when available;
if a finite input produces an infinite standardized value, they also try
`x/omega - xi/omega`. This avoids overflow in the subtraction when the
quotient is representable. The R density test includes opposite-sign
`1e308` location and observation with scale `1e308`.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
