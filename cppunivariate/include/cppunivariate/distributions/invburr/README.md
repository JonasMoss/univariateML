---
id: invburr
name: Inverse Burr
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 1.0}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 2.0}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1.0}
r:
  density: actuar::dinvburr
  sampler: actuar::rinvburr
  control:
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
references:
  - "Shao, Q. (2004). Notes on maximum likelihood estimation for the three-parameter Burr XII distribution. Computational Statistics & Data Analysis 45, 675-687. doi:10.1016/S0167-9473(02)00367-5."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 1, Chapter 20. Wiley, New York."
examples: |
  set.seed(42)
  mlinvburr(actuar::rinvburr(1000, shape1 = 1, shape2 = 2, scale = 3))
---

The inverse Burr distribution is the distribution of the reciprocal of a Burr type XII variable.

## Details

univariateML 1.x estimated only the shapes, with the scale fixed at 1.
