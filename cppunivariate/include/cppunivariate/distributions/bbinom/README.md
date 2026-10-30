---
id: bbinom
name: Beta-binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  role: trials
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
- name: alpha
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: beta
  role: shape
  domain: (0, inf)
  simulate: 3
  kind: regular
r:
  density: dbbinom
  sampler: rbbinom
  control:
    size: "Required number of trials; NULL estimates size when both shapes are held."
    alpha: "Optional fixed positive alpha; NULL estimates it."
    beta: "Optional fixed value of `beta`; `NULL` (the default) estimates it."
    reltol: "Relative tolerance on the Newton steps in the logarithm of the free concentration or shape."
    iterlim: "Maximum number of Newton iterations per local maximum."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + loggamma(x + alpha) + loggamma(size - x + beta) - loggamma(size + alpha + beta) - loggamma(alpha) - loggamma(beta) + loggamma(alpha + beta)"
limits:
  - model: binom
    when: '$\alpha + \beta \to \infty$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: the binomial likelihood at prob = mean(x)/size is within 1e-8 times the sample size of the best beta-binomial fit, with the entire finite search interval bounded above independently of the dispersion score
    status: BinomialLimit
  - name: two-point (0 and size)
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
  x <- univariateML:::rbbinom(n = 200, size = 10, alpha = 2, beta = 3)
  mlbbinom(x, size = 10)
  mlbbinom(x, size = 10, alpha = 2)
holds:
- held: []
  status: not-implemented
  reason: "P0 leaves the free-size boundary comparison unresolved: fixed-size binomial and two-point escapes must be profiled over size, the negative-binomial face must respect held shapes, and no reliable integer-tail stopping rule is available."
- held:
  - alpha
  status: not-implemented
  reason: "P0 leaves the free-size boundary comparison unresolved: fixed-size binomial and two-point escapes must be profiled over size, the negative-binomial face must respect held shapes, and no reliable integer-tail stopping rule is available."
- held:
  - beta
  status: not-implemented
  reason: "P0 leaves the free-size boundary comparison unresolved: fixed-size binomial and two-point escapes must be profiled over size, the negative-binomial face must respect held shapes, and no reliable integer-tail stopping rule is available."
fit_args:
  size: 10
---

The beta-binomial distribution is a binomial with a beta-distributed success probability; it models overdispersed counts out of a known number of trials. `size` must be supplied.

## Details

`size` is a required design coefficient. A supplied value is held fixed and is not counted in `logLik()` degrees of freedom, AIC or BIC. Passing `NULL` estimates it when both shapes are held. Holding just `alpha` or just `beta` at fixed size removes the binomial and two-point escapes; only all-0 or all-`size` samples then have no maximum likelihood estimate. With `size = 1` both shapes free are not identified (the model is Bernoulli). `model_select()` skips the beta-binomial because it cannot supply `size`.

Holding both positive shapes gives an attained integer-size maximum. Each observed mass decreases once `size + 1 >= x * (alpha + beta - 1) / alpha`; the fitter enumerates through that bound. More than one million candidates stops with a work-budget failure. With size also held this evaluates the likelihood, including size zero. Free size with either shape free remains unavailable: the fixed-size kernel can itself escape to binomial or two-point laws, which must be compared across all sizes rather than terminating at the first such candidate. P0b also has no justified stopping rule for the integer tail. Holding beta is not equivalent to holding alpha under reflection when size is free, because reflecting the sample would depend on the candidate size.
