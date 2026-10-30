---
id: hyper
name: Hypergeometric
support: {type: integer, lower: "max(0, k - (N - m))", upper: "min(k, m)", closed: [true, true]}
params:
- name: m
  role: successes in the population
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
  advice: Supply the population size, or `m`, or both; estimating both together
    is not implemented and can escape to the binomial limit.
- name: k
  role: draws
  domain: '[0, N]'
  simulate: 10
  integer: yes
  kind: design
r:
  density: stats::dhyper
  args: {m: m, "n": "N - m", k: k}
  sampler: stats::rhyper
  control:
    k: "Number of draws per observation; explicit `NULL` estimates it with `N` supplied, jointly with `m` if free."
    "N": "Population size; explicit `NULL` estimates it, which requires `m`."
    m: "Number of successes `m` in the population; `NULL` (the default) estimates it, which requires `N`."
log_density: "loggamma(m + 1) - loggamma(x + 1) - loggamma(m - x + 1) + loggamma(N - m + 1) - loggamma(k - x + 1) - loggamma(N - m - k + x + 1) - loggamma(N + 1) + loggamma(k + 1) + loggamma(N - k + 1)"
limits:
  - name: degenerate
    when: '$N \to \infty$, a point mass at zero'
    detect: all observations are 0 while m and k are positive
    status: AllZero
    fixed: [m]
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
  - "Wang, W. (2015). Exact optimal confidence intervals for hypergeometric parameters. Journal of the American Statistical Association, 110(512), 1491-1499. doi:10.1080/01621459.2014.966191."
examples: |
  set.seed(1)
  x <- rhyper(30, m = 20, n = 30, k = 10)
  mlhyper(x, N = 50, k = 10)
  mlhyper(x, m = 20, N = NULL, k = 10)
holds:
- held: []
  status: not-implemented
  reason: Joint estimation is not implemented; it requires a certified search over binomial and Poisson boundary faces and the finite interior.
- held: [k]
  status: not-implemented
  reason: A certified comparison against the binomial limit and global tail bound are not available.
- held: [m]
  status: not-implemented
  reason: Joint estimation of N and k requires a certified unbounded profile search.
fit_args:
  'N': 50
  k: 10
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

Estimating `N` and `m` together, with only `k` known, is not supported:
its likelihood can increase toward the binomial limit as `N` grows. To
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
