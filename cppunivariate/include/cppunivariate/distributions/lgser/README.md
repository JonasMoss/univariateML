---
id: lgser
name: Logarithmic series
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: theta
  role: rate
  domain: (0, 1)
  simulate: 0.9
  kind: regular
r:
  density: dlgser
  sampler: rlgser
  control:
    theta: "Optional held theta; NULL estimates it."
log_density: "x*log(theta) - log(x) - log(-log(1 - theta))"
references:
  - "Fisher, R. A., Corbet, A. S., and Williams, C. B. (1943). The relation between the number of species and the number of individuals in a random sample of an animal population. The Journal of Animal Ecology, 12(1), 42-58."
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mllgser(corbet)
---

The logarithmic series distribution models positive counts and has a history of use in species-abundance data.

## Details

When every observation is one, the optimum lies at $\theta \to 0$, the point-mass boundary. The fitter reports an error instead of returning this boundary fit.

Supplying `theta` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
