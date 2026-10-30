---
id: gb2
name: Transformed beta (GB2)
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape2
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape3
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dgb2
  sampler: rgb2
  control:
    gradtol: Tolerance on the per-observation profile score in log beta shapes.
    iterlim: Maximum iterations per outer search start.
log_density: log(shape2)-log(x)-loggamma(shape3)-loggamma(shape1)+loggamma(shape3+shape1)+shape3*shape2*log(x/scale)-(shape3+shape1)*log(1+(x/scale)**shape2)
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: InvalidData
    message: unbounded
  - {model: trgamma, when: 'shape1 tends to infinity with rescaled scale', status: TrGammaBoundary}
  - {model: invtrgamma, when: 'shape3 tends to infinity with rescaled scale', status: InvTrGammaBoundary}
  - {model: lnorm, when: 'both beta shapes tend to infinity and shape2 tends to zero', status: LognormalBoundary}
  - {model: pareto, when: 'shape2 tends to infinity and shape1 tends to zero', status: ParetoBoundary}
  - {model: power, when: 'shape2 tends to infinity and shape3 tends to zero', status: PowerBoundary}
  - {name: double Pareto, when: 'shape2 tends to infinity and both beta shapes tend to zero', status: DoubleParetoBoundary}
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlgb2())
  mlgb2(x)
---

The transformed beta distribution models positive observations with two power tails.

## Details

2026-10-30 (TASK-125): the shared beta log-coordinate bridge uses the continued fraction when the backend input is subnormal or its result is below 1e-290, avoiding relative precision loss before taking logs. The existing independent truth rows measure the selected cases; this is not a uniform error bound. Logit quantile bisection returns NaN if it cannot bracket or reach adjacent floating-point endpoints.

In the GB2 convention $(a,b,p,q)$, the coefficients are
`shape2` = $a$, `scale` = $b$, `shape3` = $p$, and `shape1` = $q$.
Each coefficient may be held at a positive finite value. The numerical search
compares proper limiting models and stops when a face wins; a search that
cannot resolve an interior maximum reports an error.


The fit standardizes log observations and profiles the power and location
for each pair of beta shapes. The inner objective is strictly concave; the
outer search uses a grid and member-family starts, refines distinct basins
with Newton or BFGS, and expands the search box when needed. This reduces
the outer problem to at most two dimensions and improves conditioning.
A small profile score and an interior position are required for a returned
interior fit. The multibasin search is numerical: neither global optimality
nor exhaustion of all possible limit faces is proved. An unresolved search
reports failure. Available transformed-gamma, inverse transformed-gamma,
lognormal, Pareto, power and double-Pareto faces are compared, as applicable
to the held subset.

## Decision history

On 2026-10-29 commit `470175d` introduced the profiled search and explicit
face comparisons. The implementation report
`dev/research/evaluations/2026-10-29-gb2-implementation-report.md` records the
derivation, independent numerical comparisons, and the failed guarantee
of global certification. On 2026-10-30 commit `fbe1d44` reduced repeated
work in profile evaluation; the report `dev/research/evaluations/2026-10-29-perf-report.md`
records the performance investigation. The search costs repeated passes
over the data per profile evaluation; the implementation report includes
measured workloads and the slower BFGS-only comparison.
