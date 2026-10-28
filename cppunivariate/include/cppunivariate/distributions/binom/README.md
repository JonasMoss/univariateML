---
id: binom
name: Binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
  - {name: size, role: trials, domain: "[0, inf)", default: 10.0, integer: true, fixable: true}
  - {name: prob, role: probability, domain: "[0, 1]", default: 0.5}
r:
  density: stats::dbinom
  sampler: stats::rbinom
  control:
    size: "Optional fixed number of trials; NULL estimates size."
    abstol: "Absolute tolerance on the Newton step in continuous size, before integer candidate selection; unused when size is fixed."
    iterlim: "Maximum number of Newton iterations; unused when size is fixed."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + x*log(prob) + (size - x)*log(1 - prob)"
limits:
  - model: pois
    when: '$\mathrm{size} \to \infty$ with mean fixed'
    detect: size is not supplied and the positive sample mean is no larger than the population-form sample variance
    status: Underdispersed
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
  - "Olkin, I., Petkau, A. J., and Zidek, J. V. (1981). A comparison of n estimators for the binomial distribution. Journal of the American Statistical Association, 76(375), 637-642."
  - "Carroll, R. J. and Lombard, F. (1985). A Note on N Estimators for the Binomial Distribution. Journal of the American Statistical Association, 80(390), 423-426."
  - "DasGupta, A. and Rubin, H. (2005). Estimation of binomial parameters when both n,p are unknown. Journal of Statistical Planning and Inference, 130(1-2), 391-404."
examples: |
  mlbinom(ChickWeight$weight, size = 400)
---

The binomial distribution models the number of successes in a fixed number of independent Bernoulli trials. Both size and success probability are estimated unless size is supplied.

## Details

The estimate of `size` can be unstable; alternatives are discussed by Carroll and Lombard (1985) and DasGupta and Rubin (2005). Supplying `size` fixes the number of trials and estimates only `prob`; it must be at least `max(x)`. The support is the integers from zero to `size`, and `size` is a nonnegative integer.
