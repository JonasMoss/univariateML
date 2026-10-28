---
id: invpareto
name: Inverse Pareto
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape, role: shape, domain: "(0, inf)", default: 2}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1}
r:
  density: actuar::dinvpareto
  sampler: actuar::rinvpareto
  control:
    scale0: Optional positive finite starting value; NULL uses the automatic start.
    reltol: Tolerance for the underlying fit.
    iterlim: Maximum number of iterations.
log_density: log(shape) + log(scale) - 2*log(x) - (shape+1)*log(1+scale/x)
limits:
  - model: invexp
    when: '$\mathrm{shape} \to \infty$ with $\mathrm{shape} \cdot \mathrm{scale}$ fixed'
    detect: mean(1/x^2) < 2*mean(1/x)^2
    status: MeanVarianceCondition
references:
  - "Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley."
  - "Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37."
---

The inverse Pareto distribution models positive heavy-tailed data.

## Details

The fit is the reciprocal of the Lomax fit on `1/x`.
