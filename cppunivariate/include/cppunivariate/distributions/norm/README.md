---
id: norm
name: Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dnorm
  sampler: stats::rnorm
  control:
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
log_density: -log(sd) - log(2*pi)/2 - (x-mean)**2/(2*sd**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York."
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: Unbounded
    message: "unbounded"
examples: |
  x <- datasets::trees$Height
  mlnorm(x)
---

The normal (Gaussian) distribution is a symmetric distribution on the real line, used to model continuous measurements and errors.

## Details

The estimated `sd` uses the variance with divisor $n$, rather than the unbiased divisor $n-1$.


With mean held, scale is the root mean square residual about that value. Scale collapses only when every observation equals the held mean. Holding sd removes collapse, including for constant samples.
