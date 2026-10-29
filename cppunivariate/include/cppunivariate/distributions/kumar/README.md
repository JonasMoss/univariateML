---
id: kumar
name: Kumaraswamy
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
- name: a
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: b
  role: shape
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: extraDistr::dkumar
  sampler: extraDistr::rkumar
  control:
    a: Optional held a; `NULL` estimates it.
    b: Optional held b; `NULL` estimates it.
    a0: Optional positive starting value for `a`; `NULL` uses the automatic start.
    reltol: Relative step tolerance, with an absolute floor of the same size.
    iterlim: Maximum number of iterations.
log_density: log(a) + log(b) + (a-1)*log(x) + (b-1)*log(1-x**a)
references:
  - "Jones, M. C. (2009). Kumaraswamy’s distribution, a beta-type distribution with some tractability advantages. Statistical Methodology, 6(1), 70–81."
  - "Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes. Journal of Hydrology, 46(1–2), 79–88."
examples: |
  AIC(mlkumar(USArrests$Rape / 100))
holds: []
limits:
- name: point-mass collapse
  when: Both free shapes diverge on constant data.
  detect: All observations are equal and neither shape is held.
  status: Unbounded
  message: unbounded
---

The Kumaraswamy distribution is a flexible distribution for continuous data between zero and one. It provides a tractable alternative to the beta distribution.

## Details

For constant observations the likelihood is unbounded, so no finite maximum likelihood estimate exists.

The maximum likelihood estimate exists for every nonconstant sample in $(0, 1)$: with `b` profiled out, the log-likelihood tends to $-\infty$ both as $a \to 0$ and as $a \to \infty$, so the family has no escapes.

With `a` held, `b = -n / sum(log(1-x^a))`. With `b` held, the strictly decreasing score for `a` has a unique positive root, found by bracketing. These fits exist even for constant interior data. Constant data have unbounded likelihood only when both shapes are free. Both-held fits evaluate the likelihood.
