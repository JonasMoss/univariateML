---
id: sn
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
    when: '$\alpha \to +\infty$'
    detect: right translated half-normal supremum wins the profile comparison
    status: RightHalfNormalBoundary
  - name: reflected translated half-normal
    when: '$\alpha \to -\infty$'
    detect: left translated half-normal supremum wins the profile comparison; use mlhnorm on -x
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
  method: multistart
  certified: partial
  note: 'sn/README.md, Details; sn/mle.hpp (public fitter and held overloads); sn/test.cpp: Held shape gives a concave
    location/inverse-scale problem; free skewness uses finite scans and local refinement, not a global certificate.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model smoothly asymmetric Gaussian-like measurements under the Azzalini construction.
summary: Azzalini skew-normal provides a model for smoothly asymmetric Gaussian-like measurements under the Azzalini
  construction.
aliases: []
key_references:
- 1
---

The Azzalini skew-normal extends the normal distribution by a smooth shape parameter. Its direct location and scale differ from its mean and standard deviation.

## Details

The normal member has singular direct-parameter Fisher information. The fitter searches both signs of shape, profiles the strictly concave location-scale problem, and compares both exact half-normal suprema; faces win ties within 1e-8 times the sample size. Shape search uses a finite grid and local refinement and is a numerical global search, not a mathematical certificate. An infinite shape estimate stops with a recommendation to use the appropriate translated half-normal.

Any subset of the three parameters can be held fixed. A fixed shape leaves a concave problem in inverse scale and location divided by scale. With fixed location, an infinite shape remains possible only if every observation lies on the corresponding side of that location; observations equal to the fixed location contribute an additional log(2) penalty at the limit. Fixed scale is retained in the face comparison. No penalised likelihood is used.

For finite $\alpha$, $\log\phi(z)$ is strictly concave and
$\log\Phi(\alpha z)$ is concave. Thus, with $b=1/\omega>0$ and
$u=\xi/\omega$, the fixed-shape likelihood
$n\log b+\sum_i\log[2\phi(bx_i-u)\Phi(\alpha(bx_i-u))]$ is strictly
concave in $(u,b)$ for nonconstant data. At $\alpha=0$ this is the normal
member; as $\alpha\to\pm\infty$ the corresponding translated half-normal
is a separate limiting face. The existing Newton inner solve already uses
this concavity; its floating-point stopping check and the outer shape grid
remain numerical and do not certify a global free-shape maximum.

| TASK-116 property | Evidence |
|---|---|
| Fixed finite shape, nonconstant data | Strict concavity in inverse-scale coordinates, proved as above. |
| Computed inner maximum | Newton iteration and a floating-point score check; numerical. |
| Free shape and half-normal faces | Grid, local refinement and face comparison; numerical, with no global shape certificate. |

An unfinished inner solve is accepted only when the normalized free-coordinate score at the returned point is below `max(reltol, 1e-3/n)` (with `n` observations). This is the Euclidean score norm in log inverse scale and scaled location divided by the sample size, with held coordinates removed.

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

This changes evaluation cost, not the objective, shape grid, local
refinement, score gates or half-normal comparisons. It adds three temporary
vectors of length $n$ per inner solve. Warm starts and a coarser outer grid
were not adopted: their effect on the existing numerical search and status
behavior would require separate evidence. The finite free-shape search
still has no global certificate; caching adds no new optimality claim.
See the [speed evaluation](../../../../../dev/research/evaluations/2026-11-01-sn-speed-report.md)
for paired regression evidence, timings and remaining guarantees.

## d/p/q/r

Uses direct skew-normal parameters, distinct from the Fernandez–Steel family; probabilities use Owen’s T and quadrature.
