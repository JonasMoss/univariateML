---
id: ztnbinom
name: Zero-truncated negative binomial
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: size
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: dztnbinom
  sampler: rztnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
    prob: "Optional held prob; NULL estimates it."
log_density: loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)
limits:
- model: ztpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: size and prob are free and the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
examples: |
  set.seed(1)
  x <- rml(200, mlztnbinom())
  mlztnbinom(x)
---

Zero-truncated negative binomial distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 1`. This boundary is returned as an attained member.
Actuar defines `size = 0` as the logarithmic series with `theta = 1 - prob`; this attained member is returned. Fixed size may be zero.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

Holding `prob < 1` removes the infinite-size escape. Size zero remains the attained logarithmic member and is compared with all bracketed profile peaks. Held `prob = 1` is compatible only with ones. The scalar grid search is numerical, not a certificate that every profile peak has been found.
