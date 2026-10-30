---
id: nhyper
name: Negative hypergeometric
support: {type: integer, lower: "r", upper: "if (r == 0) 0 else N-m+r", closed: [true, true]}
params:
- name: m
  role: white balls in the population
  domain: '[0, N]'
  simulate: 20
  integer: yes
  kind: regular
- name: "N"
  role: population size
  domain: '[0, inf)'
  simulate: 50
  integer: yes
  kind: design
  advice: Supply the population size; this fitter estimates composition with N held.
- name: r
  role: white balls required to stop sampling
  domain: '[0, m]'
  simulate: 3
  integer: yes
  kind: design
r:
  density: dnhyper
  sampler: rnhyper
  control:
    r: "Known stopping count of white balls, a whole number."
    "N": "Known population size; must be supplied. Joint estimation of population and composition is not supported."
    m: "White-ball count; `NULL` estimates it with `N` known, otherwise holds it fixed."
log_density: "loggamma(x) - loggamma(r) - loggamma(x-r+1) + loggamma(N-x+1) - loggamma(m-r+1) - loggamma(N-m-x+r+1) - loggamma(N+1) + loggamma(N-m+1) + loggamma(m+1)"
references:
  - "Berry, K. J. and Mielke, P. W. (1998). The negative hypergeometric probability distribution: Sampling without replacement from a finite population. Perceptual and Motor Skills, 86(1), 207-210."
examples: |
  set.seed(1)
  x <- univariateML:::rnhyper(n = 30, m = 20, N = 50, r = 3)
  mlnhyper(x, r = 3, N = 50)
holds:
- held: []
  status: no-mle
  reason: Joint estimation of population and composition can escape to the growing-urn limit; use mlnbinom(x - r, size = r).
- held: [r]
  status: no-mle
  reason: Joint estimation of population and composition can escape to the growing-urn limit; use mlnbinom(x - r, size = r).
- held: [m]
  status: not-implemented
  reason: Estimating N with m held is not supported by this fitter.
- held: [m, r]
  status: not-implemented
  reason: Estimating N with m held is not supported by this fitter.
- held: ["N"]
  status: not-implemented
  reason: Estimating r is not supported by this fitter.
- held: [m, "N"]
  status: not-implemented
  reason: Estimating r is not supported by this fitter.
fit_args:
  'N': 50
  r: 3
---

The negative hypergeometric distribution counts total draws without
replacement until the `r`th white ball, from an urn with `n` black and `m`
white balls. Each observation starts with the urn restored.

## Details

Supply the known population size `N` and stopping count `r`. The fitter
estimates the integer white-ball count `m`, returning coefficients `m`, `N`, and `r`. The reference receives `n = N - m`.
Supplying `m` too evaluates a fully specified model. The fixed constraints
are `N, r` for estimation and `m, N, r` for likelihood evaluation, giving
one and zero degrees of freedom respectively.

For positive `r`, support is `r` through `n + r`. The likelihood ratio at
consecutive white-ball counts is decreasing, so the finite integer maximum
is found by bisection, including attained boundaries. Adjacent equal
maxima produce a warning and the smaller count is returned. With `r = 0`,
the observation is identically zero and composition is unidentified; the
smallest white-ball count is returned with a warning. Counts must be below
$2^{53}$.

The known-population design ensures a finite maximum for every feasible
sample. Jointly estimating population and composition is not supported. Estimating
`N` with `m` held and estimating the stopping count `r` are also not
implemented; explicit `NULL` for either design quantity stops with that reason.
For positive `r`, if both counts grow with a fixed white proportion, `x - r` tends to a
negative binomial with size `r`; its fitted probability is
$r/\bar x$. Use `mlnbinom(x - r, size = r)` for that limit model.
This is a limit of the unrestricted family, not an escape in the supported
known-population fit.
