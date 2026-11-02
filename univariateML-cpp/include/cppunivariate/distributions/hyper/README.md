---
id: hyper
inference_numeric:
  - regime: unreviewed or unavailable
    available: false
    condition: "composition or design free"
    reason: "all coefficients are integer valued; there is no continuous free inference coordinate"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates"
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
  advice: Supply N to hold the population size, or set N = NULL to estimate it. A population search stops if it cannot certify the remaining tail.
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
    k: "Number of draws per observation; explicit `NULL` estimates it."
    "N": "Population size; explicit `NULL` estimates it."
    m: "Number of successes `m` in the population; `NULL` (the default) estimates it."
log_density: "loggamma(m + 1) - loggamma(x + 1) - loggamma(m - x + 1) + loggamma(N - m + 1) - loggamma(k - x + 1) - loggamma(N - m - k + x + 1) - loggamma(N + 1) + loggamma(k + 1) + loggamma(N - k + 1)"
limits:
  - name: degenerate
    firmness: proved
    when: 'As $N\to\infty$ with positive $K$ and $n$ held, the law tends to a point mass at zero.'
    detect: All observations are zero, only $N$ is free, and the held counts $K$ and $n$ are positive.
    status: AllZero
    fixed: [m]
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley. doi:10.1002/0471715816."
  - "Wang, W. (2015). Exact optimal confidence intervals for hypergeometric parameters. Journal of the American Statistical Association, 110(512), 1491-1499. doi:10.1080/01621459.2014.966191."
fit_args:
  'N': 50
  k: 10
examples: |
  set.seed(1)
  x <- rml(200, mlhyper())
  mlhyper(x, N = 10L, k = 5L)

summary: The hypergeometric distribution counts marked items in sampling without replacement from a finite population.
existence:
  uniqueness: unknown
  statement: >-
    With all three counts free, constant samples attain likelihood one.
    No complete sample criterion for existence or uniqueness is asserted
    for nonconstant samples. With $N$ held, every feasible sample has a
    finite maximum, although distinct count vectors may tie. With $K$
    and $n$ held positive and only $N$ free, a finite maximum exists
    exactly when some observation is positive. With either held count
    zero, compatible all-zero data leave $N$ unidentified. With only
    $K$ or only $n$ held, a candidate strictly above the corresponding
    binomial supremum ensures finite attainment. With all counts held,
    there is only likelihood evaluation.
  reason: >-
    A held population gives a finite feasible integer grid. With positive
    $K,n$ held, the adjacent-population likelihood ratio is eventually
    below one if the empirical mean is positive. For all-zero data the
    likelihood instead increases strictly to one. Joint population
    searches use the explicit binomial and Poisson tail bounds below.
    Those bounds establish attainment above the faces, conditional on
    the comparisons, but do not establish uniqueness or classify face ties.
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
    integer fits are unimodal; joint designs use exhaustive searches with tail bounds or stop. Likelihood comparisons
    retain floating-point tie tolerances.'
areas:
- sampling and quality control
---


The hypergeometric distribution counts successes in $n$ draws without
replacement from a population of $N$ items containing $K$ successes.
Observations are independent repetitions with the population restored
between repetitions.

## Details

Holding the population and draw count estimates its integer composition.
Holding the composition and draw count instead estimates population size,
as in capture-recapture sampling. The likelihood is symmetric in $K$ and
$n$, so holding composition and population gives the corresponding draw-count
fit. Each one-coordinate likelihood is unimodal on its feasible integers.
Two adjacent maxima can tie, and zero draws or zero successes can leave a
coefficient unidentified. A single draw gives a Bernoulli law on the grid
$K/N$.

A known population also permits joint estimation of composition and draw
count by finite enumeration. Holding only the draw count, or only the
composition, requires a population search with a binomial limiting model.
When all counts are free, Poisson limits must also be compared. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md)
and the bounds below explain these distinctions. A finite candidate that
strictly exceeds all relevant limiting suprema permits an exhaustive
search with a finite cutoff. Failure to find such a candidate does not
prove that no finite maximum exists. Constant samples attain likelihood
one when all counts are free, but need not identify a unique count vector.

## R notes

The coefficients are `m` ($K$), `N` and `k` ($n$). Supply `N` and `k` for
the usual composition fit. Explicit `NULL` requests estimation of either
design count, including joint estimation of all three. Supplying all three
evaluates the likelihood. The `fixed` attribute records all supplied
coefficients, including designs; a one-coordinate fit has one degree of
freedom and a fully specified fit has zero. The reference receives
`n = N - m`, which is not a separate coefficient.

Existing one-coordinate fits warn on ties and return the smaller count.
When every admissible count is a maximizer, they warn and return the smallest.
Joint grids resolve ties silently as described below. All counts must be
below $2^{53}$, the exact whole-number range in double precision.
To hold the failure count instead of the success count, fit `k - x`
with `m` set to the known failure count. For the growing-population
binomial limit use `mlbinom(x, size = k)`; fully free designs may also
approach `mlpois`. Unresolved face comparisons stop without an estimate.

## Decision history

### Integer grids and population certificates (2026-11-01)

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
Poisson face; this correction is not uniform in the free size. A separate
exact-arithmetic envelope covering that region is derived in the
[fully free evaluation](../../../../../dev/research/evaluations/2026-11-01-hyper-free-report.md).

For a nonconstant sample, let `X=max(x)`, `T=sum(x)`, and `Q` be its
maximised Poisson log likelihood. If `m,k >= S > X` and `N >= N0 > X`,
the joint profile is at most
`Q + T*(-2*log(1-X/S) - log(1-X/N0))`. For a fixed smaller
size `s=min(m,k)`, the earlier bound applies with binomial face `B_s`:
`B_s - h*c_N(s)`. Furthermore, for `s > X`,
`B_s <= Q - T*log(1-X/s)`. These inequalities hold in exact arithmetic;
they supply a finite tail cutoff whenever a finite candidate is strictly
above every binomial face and the Poisson face.

For a nonconstant sample the fully free search profiles the larger size at
128 populations for each of 16 consecutive smaller sizes starting at `X`.
If the best candidate exceeds the faces, it chooses `S` and `N0` from the
Poisson envelope, compares all binomial faces below `S`, and enumerates
each remaining population below its fixed-size bound. It also enumerates
the part with smaller size at least `S` and population below `N0`.
Each fixed-size profile uses the one-coordinate composition search. The
bound `-c_N(s) <= s*(s-1)/(2*(N-s+1))` gives a conservative population
cutoff. Constant samples attain likelihood one. The same 10^7 work budget
counts profile search and binomial-face evaluations, including exploration; a preflight or
runtime budget, count representation, or nonfinite failure stops without a
partial fit. A guard limits `X` and `S` to 100,000. If an explored face is
at least as good as the candidate, the search reports an unresolved face or
a numerical face tie and names the binomial or Poisson face; this does not
establish the global supremum. The `1e-9*(1+abs(face))` comparison margin
is a numerical policy, not an outward-rounded error bound. Profile signs,
face likelihoods and cutoff arithmetic also use floating-point operations.

For an interior face probability `p=mean(x)/k`, the first-order coefficient
in `t=1/N` at the face is one half of the sum over observations of
`k*(k-1) - x*(x-1)/p - (k-x)*(k-x-1)/(1-p)`.
Its sign diagnoses the local direction at the face; it is not a global
search certificate. The symmetric formula applies with `m` held.
