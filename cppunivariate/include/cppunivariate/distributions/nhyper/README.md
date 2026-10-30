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
  advice: Supply the population size, or hold `m` to estimate it.
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
    r: "Stopping count of white balls; explicit `NULL` estimates it with either `m` or `N` supplied."
    "N": "Population size; explicit `NULL` estimates it with `m` held."
    m: "White-ball count; `NULL` estimates it with `N` known, otherwise holds it fixed."
log_density: "loggamma(x) - loggamma(r) - loggamma(x-r+1) + loggamma(N-x+1) - loggamma(m-r+1) - loggamma(N-m-x+r+1) - loggamma(N+1) + loggamma(N-m+1) + loggamma(m+1)"
references:
  - "Berry, K. J. and Mielke, P. W. (1998). The negative hypergeometric probability distribution: Sampling without replacement from a finite population. Perceptual and Motor Skills, 86(1), 207-210."
holds:
- held: []
  status: not-implemented
  reason: Joint estimation is not implemented; a certified comparison across stopping counts, their negative-binomial limits and the finite interior is not available.
- held: [r]
  status: not-implemented
  reason: A certified comparison against the negative-binomial growing-urn limit and global tail bound are not available.
fit_args:
  'N': 50
  r: 3
examples: |
  set.seed(1)
  x <- rml(200, mlnhyper())
  mlnhyper(x, N = 50L, r = 3L)
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
sample. Jointly estimating population and composition without either held is not supported. Estimating
`N` with `m` held uses the finite grid described below. With both `m` and `N` held, explicit
`r = NULL` estimates the stopping count on its finite feasible integer grid;
the consecutive likelihood ratio decreases, permitting bisection.
For positive `r`, if both counts grow with a fixed white proportion, `x - r` tends to a
negative binomial with size `r`; its fitted probability is
$r/\bar x$. Use `mlnbinom(x - r, size = r)` for that limit model.
This is a limit of the unrestricted family, not an escape in the supported
known-population fit.

Finite-grid fits use long-double sums of log-gamma differences. Negative hypergeometric with only `N` held searches positive
`r <= min(x)` and `r <= m <= N-max(x)+r`. With `m` held and `N` free,
each positive `r <= min(m,min(x))` searches from `m+max(x)-r` through
`max(m+max(x)-r, floor(m*max(x)/r))`; a held `r` selects one line.
The population tail bound follows from the adjacent population ratio,
whose factors are below one beyond `m*max(x)/r`.
Zero-only negative-hypergeometric samples select `r=0`, `N=m` when free,
and `m=0` when free; mixed zero and positive observations are infeasible.
Two passes find the maximum then the lexicographically smallest coefficient
vector within 64 double-precision ulps relative to the likelihood magnitude.
These are numerical comparisons, not exact ordering certificates. A total
budget of 10^7 candidate likelihood evaluations includes both passes;
exceeding it, the exact-integer range, or encountering a nonfinite likelihood
stops with numerical failure, without returning a partial maximum.
New joint grids resolve ties silently; existing one-coordinate warnings remain.
