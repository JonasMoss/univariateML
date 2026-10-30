---
id: sn
name: Azzalini skew-normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: xi
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: omega
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: alpha
  role: shape
  domain: (-inf, inf)
  simulate: 3
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
  - "Azzalini, A. (1985). A class of distributions which includes the normal ones. Scandinavian Journal of Statistics 12, 171-178."
  - "Sartori, N. (2006). Bias prevention of maximum likelihood estimates for scalar skew normal and skew t distributions. Journal of Statistical Planning and Inference 136, 4259-4275."
examples: |
  mlsn(c(-2, -1.5, -1, -0.8, -0.3, 0, 0.2, 0.6, 1, 2), alpha = 0)
holds: []
---

The Azzalini skew-normal extends the normal distribution by a smooth shape parameter. Its direct location and scale differ from its mean and standard deviation.

## Details

The normal member has singular direct-parameter Fisher information. The fitter searches both signs of shape, profiles the strictly concave location-scale problem, and compares both exact half-normal suprema; faces win ties within 1e-8 times the sample size. Shape search uses a finite grid and local refinement and is a numerical global search, not a mathematical certificate. An infinite shape estimate stops with a recommendation to use the appropriate translated half-normal.

Any subset of the three parameters can be held fixed. A fixed shape leaves a concave problem in inverse scale and location divided by scale. With fixed location, an infinite shape remains possible only if every observation lies on the corresponding side of that location; observations equal to the fixed location contribute an additional log(2) penalty at the limit. Fixed scale is retained in the face comparison. No penalised likelihood is used.

An unfinished inner solve is accepted only when the normalized free-coordinate score at the returned point is below `max(reltol, 1e-3/n)` (with `n` observations). This is the Euclidean score norm in log inverse scale and scaled location divided by the sample size, with held coordinates removed.

## d/p/q/r

Uses direct skew-normal parameters, distinct from the Fernandez–Steel family; probabilities use Owen’s T and quadrature.
