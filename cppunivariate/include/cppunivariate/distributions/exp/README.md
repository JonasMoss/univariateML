---
id: exp
name: Exponential
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
- name: rate
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dexp
  sampler: stats::rexp
  control:
    rate: Optional held rate; NULL estimates it.
log_density: log(rate) - rate*x
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York."
examples: |
  set.seed(1)
  x <- rml(200, mlexp())
  mlexp(x)
---

The exponential distribution models nonnegative waiting times with a constant hazard rate.

## Details

The rate estimate is the inverse sample mean.

A held positive rate gives a finite likelihood even for all-zero data. A free rate has unbounded likelihood for all-zero data.


## Decision history

On 2026-05-08 the exponential dossier retained the closed-form rate fit
(commit `e5e33a1`); the
choice follows directly from the concave log likelihood, so no numerical
search or competing finite boundary is needed. The positive-rate estimate
is unique when the sum is positive; all-zero data have a supremum at
infinite rate. The held-rate case is evaluated at the supplied value.
See the family tests and the existence analysis in
`notes/evaluations/2026-10-29-p0-positive.md`.
