---
id: lomax
name: Lomax
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: lambda, role: rate, domain: "(0, inf)", default: 1}
  - {name: kappa, role: shape, domain: "(0, inf)", default: 2}
r:
  density: extraDistr::dlomax
  sampler: extraDistr::rlomax
  control:
    lambda0: Optional starting value for `lambda`; `NULL` uses a method-of-moments start.
    reltol: Relative tolerance on the Newton step for `lambda`, with an absolute floor of the same size.
    iterlim: Maximum number of Newton iterations.
log_density: log(lambda) + log(kappa) - (kappa + 1)*log(1 + lambda*x)
limits:
  - model: exp
    when: '$\kappa \to \infty$ with $\lambda \kappa$ fixed'
    detect: mean(x^2) < 2*mean(x)^2
    status: MeanVarianceCondition
references:
  - Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley, p. 60.
examples: |
  set.seed(3)
  mllomax(extraDistr::rlomax(100, 2, 4))

  # The maximum likelihood estimator does not exist for exponential-like data.
  \dontrun{
  set.seed(5)
  mllomax(rexp(10))
  }
---

The Lomax (Pareto type II) distribution is a heavy-tailed distribution on
the positive half-line, used for incomes, losses and waiting times.
