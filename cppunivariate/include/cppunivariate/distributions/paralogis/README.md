---
id: paralogis
name: Paralogistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape, role: shape, domain: "(0, inf)", default: 1}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1}
r:
  density: actuar::dparalogis
  sampler: actuar::rparalogis
  control:
    shape0: Optional starting value for `shape`; `NULL` uses `max(1.5, 2 / sd(log(x / median(x))))`.
    gradtol: Convergence tolerance on the norm of the finite-difference gradient of the negative log-likelihood.
    iterlim: Maximum number of BFGS iterations.
log_density: 2*log(shape) + shape*log(x/scale) - log(x) - (shape + 1)*log(1 + (x/scale)^shape)
references:
  - Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley.
  - Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012). Loss Models, From Data to Decisions, Fourth Edition. Wiley.
examples: |
  mlparalogis(abalone$length)
---

The paralogistic distribution is the Burr type XII distribution with its two
shape parameters equal. It is used for size and loss data.

## Details

Both `shape` and `scale` are estimated. univariateML 1.x estimated only the
shape, with the scale held at 1.
