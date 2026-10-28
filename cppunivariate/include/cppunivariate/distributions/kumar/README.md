---
id: kumar
name: Kumaraswamy
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
  - {name: a, role: shape, domain: "(0, inf)", default: 2.0}
  - {name: b, role: shape, domain: "(0, inf)", default: 3.0}
r:
  density: extraDistr::dkumar
  sampler: extraDistr::rkumar
  control:
    a0: Optional positive starting value for `a`; `NULL` uses the automatic start.
    reltol: Relative step tolerance, with an absolute floor of the same size.
    iterlim: Maximum number of iterations.
log_density: log(a) + log(b) + (a-1)*log(x) + (b-1)*log(1-x**a)
references:
  - "Jones, M. C. (2009). Kumaraswamy’s distribution, a beta-type distribution with some tractability advantages. Statistical Methodology, 6(1), 70–81."
  - "Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes. Journal of Hydrology, 46(1–2), 79–88."
examples: |
  AIC(mlkumar(USArrests$Rape / 100))
---

The Kumaraswamy distribution is a flexible distribution for continuous data between zero and one. It provides a tractable alternative to the beta distribution.

## Details

The maximum likelihood estimate exists for every nonconstant sample in $(0, 1)$: with `b` profiled out, the log-likelihood tends to $-\infty$ both as $a \to 0$ and as $a \to \infty$, so the family has no escapes.
