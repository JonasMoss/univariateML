---
id: invburr
name: Inverse Burr
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: shape2
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: actuar::dinvburr
  sampler: actuar::rinvburr
  control:
    abstol: Absolute Newton step tolerance in `log(shape2)` when scale is fixed.
    gradtol: Gradient tolerance for the mean profiled negative log likelihood.
    iterlim: Maximum BFGS iterations per start.
    shape10: Optional positive starting shape1; `NULL` uses automatic starts.
    shape20: Optional positive starting shape2; `NULL` uses automatic starts.
    scale0: Optional positive starting scale; `NULL` uses automatic starts.
    reltol: Relative objective tolerance for BFGS.
log_density: log(shape1) + log(shape2) - log(x) + shape2*log(scale/x) - (shape1+1)*log(1+(scale/x)**shape2)
limits:
  - model: 'invweibull'
    when: '$\mathrm{shape1} \to \infty$ with $\mathrm{scale}\,\mathrm{shape1}^{1/\mathrm{shape2}}$ fixed'
    status: 'InvWeibullBoundary'
  - model: 'power'
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    status: 'PowerBoundary'
  - model: 'power'
    fixed: [scale]
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale fixed'
    detect: 'all(x <= scale)'
    status: 'AllLeOne'
references:
  - "Shao, Q. (2004). Notes on maximum likelihood estimation for the three-parameter Burr XII distribution. Computational Statistics & Data Analysis 45, 675-687. doi:10.1016/S0167-9473(02)00367-5."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 1, Chapter 20. Wiley, New York."
examples: |
  set.seed(42)
  mlinvburr(actuar::rinvburr(1000, shape1 = 1, shape2 = 2, scale = 3))
holds:
- held:
  - shape1
  status: not-implemented
  reason: The adapter does not accept `shape1`; this hold is not implemented (P2).
- held:
  - shape2
  status: not-implemented
  reason: The adapter does not accept `shape2`; this hold is not implemented (P2).
---

The inverse Burr distribution is the distribution of the reciprocal of a Burr type XII variable.

## Details

With scale held at $s$ and nonconstant observations all at most $s$, the inverse Burr supremum is the endpoint-$s$ power log-likelihood minus $Z\log 2$, where $Z$ counts observations equal to $s$; if every observation equals $s$, the likelihood is unbounded.

univariateML 1.x estimated only the shapes, with the scale fixed at 1.

Supply `scale = s` to estimate only the shapes. This fits the historical
scale-1 family to `x / s` and subtracts `length(x) * log(s)` from its
log likelihood. The fixed-scale path profiles shape1 and refines shape2
by Newton iteration, with a profile-likelihood fallback start. Starting
controls `shape10`, `shape20`, `scale0`, and BFGS tolerances apply only
to the free-scale path. The fixed-scale limit has endpoint `s`.
