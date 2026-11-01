---
id: nhyper
inference_numeric:
  - regime: unreviewed or unavailable
    available: false
    condition: "composition or design free"
    reason: "all coefficients are integer valued; there is no continuous free inference coordinate"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates"
name: Negative hypergeometric
support: {type: integer, lower: "r", upper: "if (r == 0) 0 else N-m+r", closed: [true, true]}
params:
- name: m
  symbol: 'K'
  role: white balls in the population
  domain: '[0, N]'
  simulate: 5
  integer: yes
  kind: regular
- name: "N"
  symbol: 'N'
  role: population size
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
  advice: Population and composition can be estimated jointly, subject to a numerical tail certificate.
- name: r
  symbol: 'r'
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
- 'Berry, K. J. and Mielke, P. W. (1998). The negative hypergeometric probability distribution: Sampling without
  replacement from a finite population. Perceptual and Motor Skills, 86(1), 207-210.'
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
fit_args:
  'N': 50
  r: 3
examples: |
  set.seed(1)
  x <- rml(200, mlnhyper())
  mlnhyper(x, N = 10L, r = 3L)

group: finite sampling
construction:
- base
relations:
  transform_of:
  - family: bbinom
    at:
      size: (N-m)*min(1,r)
      alpha: max(1,r)
      beta: m-r+1
    transform: shift
    shift: r
fit:
  method: integer-search
  certified: 'yes'
  note: 'nhyper/README.md, Details; nhyper/mle.hpp (public fitter and held overloads); nhyper/test.cpp: Supported
    integer grids are exhaustive with a proved population tail bound; one-coordinate likelihood ratios decrease.
    Unsupported joint population/composition designs stop; floating-point tie tolerances remain.'
areas:
- sampling and quality control
classification: specialist
classification_reason: Used to model finite-population waiting counts up to a specified number of successes.
summary: Negative hypergeometric provides a model for finite-population waiting counts up to a specified number
  of successes.
aliases: []
key_references:
- 2
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
sample. Joint estimation of population and composition uses the tail certificate below. Estimating
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

### Growing-population bound (2026-11-01)

For positive `r`, write `c_t(j) = sum_{i=0}^{j-1} log(1-i/t)`.
The waiting-time mass divided by the negative-binomial mass of `x-r`
with size `r` and success probability `p=m/N` equals
`exp(c_m(r)+c_{N-m}(x-r)-c_N(x))`, with endpoints understood by
continuity. Both numerator corrections are nonpositive. Consequently,
for observations `x_i` and `N >= max(x_i)`, the profile for fixed `r`
is bounded above by `B_r - sum_i c_N(x_i)`, where `B_r` is the
negative-binomial face supremum at `p = r/mean(x)`. The correction is
nonnegative, decreases with `N`, and tends to zero. With `r` free and
positive observations, `1 <= r <= min(x_i)`, so replacing `B_r` by the
maximum over that finite set gives the same uniform tail bound. A finite
candidate strictly above the appropriate face supremum gives an eventual
cutoff; every feasible point below it must still be checked. For `r=0`,
the only possible observation is zero and finite maximizers exist.

The fitter profiles composition at each integer population and each feasible
stopping count, initially over 256 populations. A candidate exceeding the
largest face by more than `1e-9*(1+abs(face))` gives a tail cutoff; every
population below it is searched, subject to the 10^7 evaluation budget.
The upper bound must fall below the candidate by that margin. These margins
and profile signs are numerical comparisons, not directed-rounding proofs.
If no strict improvement is found in the exploratory search, the fitter
stops with an unresolved face status, or a face-tie status when a finite
likelihood matches within the margin. Neither status proves a global
maximum or escape.

For an interior face probability `p=r/mean(x)`, the first-order coefficient
in `t=1/N` is one half of the sum over observations of
`x*(x-1) - r*(r-1)/p - (x-r)*(x-r-1)/(1-p)`. Its sign is a local
diagnostic, not a global search certificate; for free `r` it applies to
each face separately.
