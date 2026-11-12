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
  advice: Supply N to hold the population size, or set N = NULL to estimate it. Population and composition can be estimated jointly when the tail comparisons certify the fit.
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
  certified: partial
  note: 'nhyper/README.md, Details; nhyper/mle.hpp (public fitter and held overloads); nhyper/test.cpp: Supported
    certified integer grids are exhaustive with a proved population tail bound; one-coordinate likelihood ratios decrease.
    Growing-urn face comparisons and unfinished joint grids return their finite prefix winner uncertified; floating-point tie tolerances remain.'
areas:
- sampling and quality control
existence:
  uniqueness: unknown
  statement: >-
    With all counts free, constant samples attain likelihood one, while
    mixed zero and positive observations are infeasible. No complete
    existence or uniqueness criterion is asserted for positive nonconstant
    samples. With $N$ held, every feasible sample has a finite maximum.
    With $K$ held, every feasible sample also has a finite maximum, whether
    the stopping count is held or estimated. Ties can occur. With only
    positive $r$ held, a candidate strictly above the shifted
    negative-binomial supremum ensures finite attainment. With $r=0$,
    only zero observations are possible and composition is unidentified.
    Holding all counts specifies the law without optimization.
  reason: >-
    A known population leaves a finite integer grid. With $K$ held and
    positive $r$, the adjacent-population ratio is below one beyond
    $K\max_i x_i/r$, leaving finitely many candidates. A free positive
    stopping count has only finitely many feasible values. With both
    population and composition free, the tail bound below proves
    attainment above the relevant negative-binomial faces, conditional on
    the comparisons. It does not classify unresolved faces or prove uniqueness.
classification: specialist
classification_reason: Used to model finite-population waiting counts up to a specified number of successes.
summary: Negative hypergeometric provides a model for finite-population waiting counts up to a specified number
  of successes.
aliases: []
key_references:
- 2
---


The negative hypergeometric distribution counts total draws without
replacement until the $r$th white ball from a population of $N$ balls,
$K$ of them white. Each observation starts with the population restored.

## d/p/q/r

### Small marked populations

With five marked positions in a population of $10^8$, the third marked
position exceeds $N-3$ with probability $60/[N(N-1)(N-2)]$. This tail is
sensitive to subtracting large gamma normalizers. A finite product over the
marked positions retains it without enumerating the position support.

## Details

Holding population and stopping count estimates the integer white-ball
count. For positive $r$, the possible total draws run from $r$ to
$N-K+r$. The likelihood ratio at consecutive composition counts decreases,
so bisection locates a maximum, including attained boundaries. Adjacent
counts can tie. When $r=0$, sampling stops immediately and the law is the
point mass at zero, regardless of composition.

A known population permits joint estimation of composition and stopping
count on a finite feasible grid. With composition held and population free,
a decreasing population likelihood tail gives a finite search even when
stopping count is estimated. Holding population and composition instead
leaves a one-coordinate stopping-count likelihood with decreasing
consecutive ratio.

When population and composition are both free, their ratio can tend to a
fixed white proportion. Then the number of black draws, $x-r$, tends to a
negative-binomial law with size $r$ and fitted success probability
$r/\bar x$. This is a boundary of the unrestricted family, rather than an
escape from a known-population fit. A finite candidate strictly above the
relevant limiting suprema gives a finite search cutoff. An unresolved
comparison with a limiting model does not prove non-existence.

## R notes

The coefficients are `m` ($K$), `N` and `r`. Supply `N` and `r` for the
usual composition fit. Supplying `m` too evaluates the specified law.
These fits record respectively `N, r` and `m, N, r` as fixed, giving one
and zero degrees of freedom. Explicit `NULL` requests estimation of a
design count; population and composition can also be estimated jointly.
The reference's black-ball count is `n = N - m`.

One-coordinate fits warn on adjacent ties and return the smaller count.
At `r = 0`, free composition is represented by zero with a warning.
Joint-grid conventions and budgets are given below. Counts must be below
$2^{53}$. For the growing-population limit with positive `r`, use
`mlnbinom(x - r, size = r)`. Unresolved face comparisons stop without an
estimate.

## Decision history

### Integer grids and population certificates (2026-11-01)

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
If exploration finds no strict improvement, or a face matches within the margin, the best finite profile is returned with `certified = FALSE`. An unfinished finite grid likewise returns its prefix winner uncertified; neither result establishes the global maximum or an escape.

For an interior face probability `p=r/mean(x)`, the first-order coefficient
in `t=1/N` is one half of the sum over observations of
`x*(x-1) - r*(r-1)/p - (x-r)*(x-r-1)/(1-p)`. Its sign is a local
diagnostic, not a global search certificate; for free `r` it applies to
each face separately.

## Stop details

The growing-urn face is a negative binomial translated by r, since observations count total draws. Its payload explicitly records the shift and the absence of a corresponding package family.

With composition held, a constant draw count $c\le m$ attains likelihood one at $N=m$, $r=c$. This exact member also settles the case at the largest represented population. Outside such attained cases, omitting stopping counts whose feasible populations exceed the representation range leaves the finite prefix uncertified. A large growing-urn preflight retains the population-prefix winner at the smallest admissible stopping count; it makes no claim to have optimized all stopping counts.
