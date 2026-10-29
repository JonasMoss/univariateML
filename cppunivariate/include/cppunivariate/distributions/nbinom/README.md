---
id: nbinom
name: Negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  role: shape
  domain: (0, inf)
  simulate: 10.0
  kind: regular
- name: prob
  role: probability
  domain: (0, 1]
  simulate: 0.3
  kind: regular
r:
  density: stats::dnbinom
  sampler: stats::rnbinom
  control:
    reltol: "Relative tolerance on the Newton step, with an absolute floor of the same size."
    iterlim: "Maximum number of Newton iterations."
    size: "Optional fixed positive size (including fractional values); NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
log_density: "loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob)"
limits:
  - model: pois
    name: zero point mass
    when: '$size \to 0$ on the current positive-size domain'
    detect: size is free, prob is held below one, and all observations are zero
    fixed: [prob]
    status: ZeroLimit
  - model: pois
    when: '$\mathrm{size} \to \infty$ with mean fixed'
    detect: size and prob are both free and the population-form sample variance is no larger than the sample mean
    status: Underdispersed
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mlnbinom(corbet)
---

The negative binomial distribution models nonnegative counts with overdispersion relative to a Poisson distribution. The size parameter need not be an integer.

## Details

Supplying `size` fixes it and estimates only `prob`; positive fractional sizes are allowed. For all-zero data the maximum is not unique: the fitter warns and returns `size = 1`, `prob = 1`.

With `prob` held strictly below one, the size score is strictly decreasing and has a unique root whenever a count is positive. For all-zero data its supremum is at size zero, outside the current positive-size domain; the fitter stops. Held `prob = 1` is compatible only with zeros.
