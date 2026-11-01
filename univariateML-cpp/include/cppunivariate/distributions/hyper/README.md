---
id: hyper
name: Hypergeometric
support: {type: integer, lower: "max(0, k - (N - m))", upper: "min(k, m)", closed: [true, true]}
params:
- name: m
  symbol: 'K'
  role: successes in the population
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
  advice: Supply the population size, or hold `m` or `k`; unbounded searches may stop when their numerical certificate is unavailable.
- name: k
  symbol: 'n'
  role: draws
  domain: '[0, N]'
  simulate: 5
  integer: yes
  kind: design
r:
  density: stats::dhyper
  args: {m: m, "n": "N - m", k: k}
  sampler: stats::rhyper
  control:
    k: "Number of draws per observation; explicit `NULL` estimates it with `N` supplied, jointly with `m` if free."
    "N": "Population size; explicit `NULL` estimates it with `m` or `k` held."
    m: "Number of successes `m` in the population; `NULL` (the default) estimates it."
log_density: "loggamma(m + 1) - loggamma(x + 1) - loggamma(m - x + 1) + loggamma(N - m + 1) - loggamma(k - x + 1) - loggamma(N - m - k + x + 1) - loggamma(N + 1) + loggamma(k + 1) + loggamma(N - k + 1)"
limits:
  - name: degenerate
    when: '$N \to \infty$, a point mass at zero'
    detect: all observations are 0 while m and k are positive
    status: AllZero
    fixed: [m]
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley. doi:10.1002/0471715816."
  - "Wang, W. (2015). Exact optimal confidence intervals for hypergeometric parameters. Journal of the American Statistical Association, 110(512), 1491-1499. doi:10.1080/01621459.2014.966191."
holds:
- held: []
  status: not-implemented
  reason: Joint estimation is not implemented; it requires a certified search over binomial and Poisson boundary faces and the finite interior.
fit_args:
  'N': 50
  k: 10
examples: |
  set.seed(1)
  x <- rml(200, mlhyper())
  mlhyper(x, N = 10L, k = 5L)

summary: The hypergeometric distribution counts marked items in sampling without replacement from a finite population.
classification: classical
classification_reason: A standard model for success counts in finite-population sampling without replacement.
aliases: []
key_references:
- 1

group: finite sampling
construction:
- base
relations: {}
fit:
  method: integer-search
  certified: 'yes'
  note: 'hyper/README.md, Details; hyper/mle.hpp (public fitter and held overloads); hyper/test.cpp: Supported one-coordinate
    integer fits are unimodal; fixed-population joint grids exhaust the feasible set. Unsupported joint population
    designs stop. Likelihood comparisons retain floating-point tie tolerances.'
areas:
- sampling and quality control
---

The hypergeometric distribution counts the successes in `k` draws without
replacement from a population of `N` items, `m` of them successes. The
observations are independent repetitions from the same population, which
is restored between repetitions.

## Details

The number of draws `k` is a design quantity; explicit `NULL` estimates it
with `N` supplied. When `m` is also held, the fitter uses the symmetry
of the likelihood in `m` and `k`.
Supply also the population size `N` to estimate the number of successes
`m`, or `m` to estimate `N` (capture-recapture with `m` marked items);
supplying all three only evaluates the likelihood. The returned coefficients are
`m`, `N`, and `k`; `n = N - m` is passed to `stats::dhyper` but is not itself
a coefficient.
The `fixed` attribute records `N` and `k` for a known-population fit,
`m` and `k` for a known-success fit, and `m`, `N`, `k` when all are supplied;
the degrees of freedom are respectively one, one, and zero.
With `k = 1` the model is Bernoulli on the grid `m / N`.
Both one-parameter
likelihoods are unimodal on their whole-number domains, and the fitter
finds the maximiser exactly. When two adjacent values attain the maximum
it warns and returns the smaller; with `k = 0`, or `m = 0` when `N` is
estimated, every admissible value attains it, and the fitter warns and
returns the smallest.

With `k` held, `N` and `m` can be estimated jointly; by symmetry the same applies with `m` held. To
hold the number of failures `N - m` fixed instead of `m`, fit `k - x`
with `m` set to that number. All counts must be below $2^{53}$, the range
of exact whole numbers in double precision.

## See also

[mlbinom()] with `size = k`, the limit as `N` grows with `m / N` fixed.

Finite-grid fits use long-double sums of log-gamma differences. Hyper with
only `N` held searches `m >= max(x)` and `max(x) <= k <= N-m+min(x)`.
The grid bounds are exactly the common-support constraints; finiteness
proves attainment.
Two passes find the maximum then the lexicographically smallest coefficient
vector within 64 double-precision ulps relative to the likelihood magnitude.
These are numerical comparisons, not exact ordering certificates. A total
budget of 10^7 candidate likelihood evaluations includes both passes;
exceeding it, the exact-integer range, or encountering a nonfinite likelihood
stops with numerical failure, without returning a partial maximum.
New joint grids resolve ties silently; existing one-coordinate warnings remain.

### Growing-population bound (2026-11-01)

For fixed positive `k` (or fixed `m` by symmetry), put
`c_N(t) = sum_{j=0}^{t-1} log(1-j/N)` and `p=m/N`. For `N >= k`,
the hypergeometric mass divided by the binomial mass with size `k` and
probability `p` is
`exp(c_m(x)+c_{N-m}(k-x)-c_N(k))`, with zero factors interpreted by
continuity. Since each numerator correction is at most zero, for `h`
observations the profile at size `N` is at most
`B_k - h*c_N(k)`, where `B_k` is the maximised binomial log likelihood
at `p = mean(x)/k`. The correction is nonnegative, decreases in `N`,
and tends to zero. Thus a finite likelihood strictly above `B_k` gives
an eventual cutoff: enumerate every feasible smaller population and stop
when this upper bound is strictly below that candidate. For `k=0` the
model is the zero point mass; the symmetric statement holds for `m=0`.

The fitter profiles composition at each integer population, initially over
256 feasible populations. If it finds a value above the face by more than
`1e-9*(1+abs(face))`, it computes a cutoff from the bound and exhaustively
checks all populations below it, subject to the 10^7 evaluation budget.
It requires the tail upper bound to be below the candidate by that margin.
These floating-point margins are a practical comparison policy, not a
proved rounding enclosure; the profile searches also use numerical signs.
If the exploratory search finds no strict improvement, it stops with an
unresolved face status, or a face-tie status when a finite likelihood matches
within the margin. Neither status proves a global maximum or escape.
The fully free case additionally permits growing binomial sizes and the
Poisson face; this correction is not uniform in the free size.

For an interior face probability `p=mean(x)/k`, the first-order coefficient
in `t=1/N` at the face is one half of the sum over observations of
`k*(k-1) - x*(x-1)/p - (k-x)*(k-x-1)/(1-p)`.
Its sign diagnoses the local direction at the face; it is not a global
search certificate. The symmetric formula applies with `m` held.
