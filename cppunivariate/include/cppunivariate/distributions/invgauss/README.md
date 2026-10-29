---
id: invgauss
name: Inverse Gaussian
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: mean
  role: location
  domain: (0, inf)
  simulate: 3.0
  kind: regular
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 4.0
  kind: regular
r:
  density: actuar::dinvgauss
  sampler: actuar::rinvgauss
  control:
    mean: Optional held mean; NULL estimates it.
    shape: Optional held shape; NULL estimates it.
log_density: log(shape)/2 - log(2*pi)/2 - 3*log(x)/2 - shape*(x-mean)**2/(2*mean**2*x)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 15. Wiley, New York."
examples: |
  mlinvgauss(precip)
limits:
- name: unbounded likelihood
  fixed: [mean]
  when: Every observation equals held mean.
  status: Unbounded
  message: constant data
---

The inverse Gaussian (Wald) distribution models positive quantities, including first-passage times of Brownian motion with positive drift.

## Details

The `mean` parameter is the positive mean and `shape` is the positive inverse Gaussian shape, rather than a dispersion. For nonconstant positive data the MLE is finite. Although the family has a Levy limit as the mean tends to infinity with shape fixed, no limit status is reported by this fitter.

With mean held and shape free, the likelihood is unbounded if every observation equals that mean. A held positive shape permits constant data. Held parameters must be finite.
