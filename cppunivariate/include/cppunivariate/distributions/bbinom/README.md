---
id: bbinom
name: Beta-binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
  - {name: size, role: trials, domain: "[0, inf)", default: 10, integer: true, required: true}
  - {name: alpha, role: shape, domain: "(0, inf)", default: 2, fixable: true}
  - {name: beta, role: shape, domain: "(0, inf)", default: 3, fixable: true}
r:
  density: extraDistr::dbbinom
  sampler: extraDistr::rbbinom
  control:
    size: "Required known number of trials, a nonnegative integer at least `max(x)` (at most 1e7); held fixed, never estimated."
    alpha: "Optional fixed value of `alpha`; `NULL` (the default) estimates it. At most one of `alpha` and `beta` may be fixed."
    beta: "Optional fixed value of `beta`; `NULL` (the default) estimates it."
    reltol: "Relative tolerance on the Newton steps in the logarithm of the free concentration or shape."
    iterlim: "Maximum number of Newton iterations per local maximum."
fit_args: {size: 10}
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + loggamma(x + alpha) + loggamma(size - x + beta) - loggamma(size + alpha + beta) - loggamma(alpha) - loggamma(beta) + loggamma(alpha + beta)"
limits:
  - model: binom
    when: '$\alpha + \beta \to \infty$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: the binomial likelihood at prob = mean(x)/size is within 1e-8 times the sample size of the best beta-binomial fit, with the entire finite search interval bounded above independently of the dispersion score
    status: BinomialLimit
  - name: two-point distribution on {0, size}
    when: '$\alpha, \beta \to 0$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: size >= 2 and every observation is 0 or size, both values occurring
    status: TwoPointLimit
  - model: binom
    when: '$\alpha/(\alpha + \beta) \to 0$ or $1$'
    detect: all observations are 0 or all equal size
    status: PointMass
references:
  - "Griffiths, D. A. (1973). Maximum likelihood estimation for the beta-binomial distribution and an application to the household distribution of the total number of cases of a disease. Biometrics, 29(4), 637-648. doi:10.2307/2529131."
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  set.seed(1)
  x <- extraDistr::rbbinom(200, size = 10, alpha = 2, beta = 3)
  mlbbinom(x, size = 10)
  mlbbinom(x, size = 10, alpha = 2)
---

The beta-binomial distribution is a binomial with a beta-distributed success probability; it models overdispersed counts out of a known number of trials. `size` must be supplied.

## Details

`size` is a design quantity, not a parameter: it is required, held fixed and not counted in `logLik()` degrees of freedom, AIC or BIC. Holding `alpha` or `beta` fixed removes the binomial and two-point escapes; only all-0 or all-`size` samples then have no maximum likelihood estimate. With `size = 1` both shapes free are not identified (the model is Bernoulli). `model_select()` skips the beta-binomial because it cannot supply `size`.

`extraDistr` has no `qbbinom`; `qml()` and `plot()` use the generalised inverse of `extraDistr::pbbinom` (see `univariateML_reference_quantiles`). `extraDistr::dbbinom` and `pbbinom` lose accuracy when both shapes are very large, around `1e10` and beyond (see `KNOWN_ISSUES.md`); the fitter compares the actual binomial face rather than substituting huge shapes for that face. The generic quantile inherits any inaccuracy in the reference CDF.
