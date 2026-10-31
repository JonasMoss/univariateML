---
id: bbinom
name: Beta-binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
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
- Griffiths, D. A. (1973). Maximum likelihood estimation for the beta-binomial distribution and an application to
  the household distribution of the total number of cases of a disease. Biometrics, 29(4), 637-648. doi:10.2307/2529131.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds:
- held: []
  status: not-implemented
  reason: "Free size requires comparing every finite-size binomial and two-point face with the negative-binomial and Poisson limits; no reliable global integer-tail bound is available."
- held:
  - alpha
  status: not-implemented
  reason: "With alpha held, the negative-binomial limit has that same shape; no reliable integer-tail bound excludes a later size beating both the finite profile and this limit."
- held:
  - beta
  status: not-implemented
  reason: "With beta held, positive-count probabilities vanish uniformly as size grows, but the proved tail bound is too weak for practical enumeration and fixed-size point-mass limits still require comparison."
fit_args:
  size: 10
examples: |
  set.seed(1)
  x <- rml(200, mlbbinom())
  mlbbinom(x, size = 10L)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'bbinom/README.md, Details; bbinom/mle.hpp (public fitter and held overloads); bbinom/test.cpp: Both shapes
    held give an integer-size tail bound; fixed-size free-shape fits use numerical optimization and some free-size
    profiles are unsupported.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed success counts with heterogeneous binomial probabilities.
summary: Beta-binomial provides a model for overdispersed success counts with heterogeneous binomial probabilities.
aliases: []
key_references:
- 2
---

The beta-binomial distribution is a binomial with a beta-distributed success probability; it models overdispersed counts out of a known number of trials. `size` must be supplied.

## Details

`size` is a required design coefficient. A supplied value is held fixed and is not counted in `logLik()` degrees of freedom, AIC or BIC. Passing `NULL` estimates it when both shapes are held. Holding just `alpha` or just `beta` at fixed size removes the binomial and two-point escapes; only all-0 or all-`size` samples then have no maximum likelihood estimate. With `size = 1` both shapes free are not identified (the model is Bernoulli). `model_select()` skips the beta-binomial because it cannot supply `size`.

Holding both positive shapes gives an attained integer-size maximum. Each observed mass decreases once `size + 1 >= x * (alpha + beta - 1) / alpha`; the fitter enumerates through that bound. More than one million candidates stops with a work-budget failure. With size also held this evaluates the likelihood, including size zero. Free size with either shape free remains unavailable: the fixed-size kernel can itself escape to binomial or two-point laws, which must be compared across all sizes rather than terminating at the first such candidate. For held beta, a uniform tail bound proves coercivity for samples containing a positive count, but is too weak for a practical search. For the other subsets a useful tail bound remains open. Holding beta is not equivalent to holding alpha under reflection when size is free, because reflecting the sample would depend on the candidate size.
