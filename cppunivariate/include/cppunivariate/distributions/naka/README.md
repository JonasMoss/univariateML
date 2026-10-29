---
id: naka
name: Nakagami
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: dnaka
  sampler: rnaka
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Absolute step tolerance in shape (the gamma shape for x^2).
    iterlim: Maximum number of iterations.
log_density: log(2) + shape*log(shape) - loggamma(shape) - shape*log(scale) + (2*shape - 1)*log(x) - shape*x**2/scale
references:
  - "Choi, S. C, and R. Wette. \"Maximum likelihood estimation of the parameters of the gamma distribution and their bias.\" Technometrics 11.4 (1969): 683-690."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York."
examples: |
  mlnaka(precip)
limits:
- name: unbounded likelihood
  fixed: [scale]
  when: Every squared observation equals held scale.
  status: Degenerate
  message: unbounded
---

The Nakagami distribution models positive amplitudes, including radio-fading measurements. Its square follows a gamma distribution.

## Details

The shape $m$ may be any positive value; $m < 1/2$ is not excluded. The scale is the mean of the squared variable, rather than the scale of that gamma variable.

With scale held and shape free, the likelihood is unbounded if every squared observation equals scale. Holding shape permits constant data.
