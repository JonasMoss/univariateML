---
id: nbinom
name: Negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  symbol: 'r'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: stats::dnbinom
  sampler: stats::rnbinom
  control:
    reltol: "Relative tolerance on the Newton step, with an absolute floor of the same size."
    iterlim: "Maximum number of Newton iterations."
    size: "Optional fixed nonnegative size (including fractional values); NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
log_density: "loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob)"
limits:
  - model: pois
    when: '$\mathrm{size} \to \infty$ with mean fixed'
    detect: size and prob are both free and the population-form sample variance is no larger than the sample mean
    status: Underdispersed
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  set.seed(1)
  x <- rml(200, mlnbinom())
  mlnbinom(x)
---

The negative binomial distribution models nonnegative counts with overdispersion relative to a Poisson distribution. The size parameter need not be an integer.

## Details

Supplying `size` holds any finite nonnegative size; size zero is the point mass at zero and is incompatible with positive observations. All-zero data with free size return the attained member `size = 0`, preserving held probability or choosing `prob = 0.5` when unidentified. With positive size held, all-zero data estimate `prob = 1`. These boundary fits are silent.

With `prob` held strictly below one, the size score is strictly decreasing and has a unique root whenever a count is positive. Held `prob = 1` is compatible only with zeros.

### Adapter independence (2026-10-30)

The held-size/free-probability precision guard now lives in the core: positive count total with size/(size+mean(x)) rounding to one returns `PrecisionFailure`, preserving the former adapter rejection.
