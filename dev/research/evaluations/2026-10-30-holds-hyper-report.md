# Hypergeometric design holds (TASK-95)

2026-10-30. This is a partial implementation: five of the seven requested
subsets remain unsupported. No commit was made. The arguments below concern
nonempty whole-number samples with positive likelihood; integer limits and
floating-point comparisons constrain the implementation separately.

## Results

Write `a = min(x)`, `b = max(x)`, and let `h` be the sample size.

* Hyper `{m,N}`: `b <= k <= N-m+a`. This finite grid attains its maximum.
  Implemented by exchanging `m` and `k` in the existing certified
  one-parameter search: the pmf is symmetric in these two arguments.
  The adjacent ratio decreases across the grid. An all-zero sample with
  `m=0` is unidentified; adjacent ties are reported to rounding.
* Hyper `{N}`: `0 <= m,k <= N`, `b <= min(m,k)` and
  `k-m <= N-a`; this is a finite grid, hence a maximum exists for every
  feasible sample. Not implemented: joint grid comparisons need a certified
  global implementation.
* Hyper `{m}`: `0 <= k <= N`, `N >= m`, `b <= min(m,k)` and
  `k <= N-m+a`. With `N` and `k` diverging and `k/N -> p`, the pmf
  tends to `Binomial(m,p)`. If `B` denotes the maximum likelihood over
  `p in [0,1]`, a finite MLE exists exactly when a finite feasible point
  has likelihood at least `B`: any higher level set is finite, because
  its sequences cannot escape to the binomial boundary. Constant samples
  attain likelihood one at `N=m,k=x`; so this subset is not universally
  without an MLE. No computable global finite-versus-limit criterion or
  certified search is implemented.
* Nhyper `{m,N}`: for positive data, `max(1,b-(N-m)) <= r <= min(m,a)`;
  zero-only data have `r=0`; mixed zeros and positives are infeasible.
  The grid is finite. Implemented with bisection: the ratio
  `L(r+1)/L(r)` is the product over observations of
  `(x-r)(m-r)/(r(N-m-x+r+1))`, and each factor decreases with `r`.
  Adjacent ties are reported to rounding.
* Nhyper `{N}`: `0 <= r <= m <= N`, and positive samples require
  `r <= a` and `m <= N-b+r`; the all-zero sample admits `r=0`.
  This finite feasible grid attains a maximum. Not implemented: the joint
  grid needs a certified global comparison.
* Nhyper `{m,r}`: for `r>0`, `r <= m`, `a >= r`, and
  `N >= m+b-r`. The consecutive ratio is
  `L(N)/L(N-1) = product_i [1+(m*x_i-N*r)/(N*(N-m-x_i+r))]`.
  Every factor is below one once `N > m*b/r`. Thus the finite grid through
  `max(m+b-r, floor(m*b/r))` contains a maximizer. If `r=0`, all-zero
  data have every `N>=m` as a maximizer; other data are infeasible.
  Not implemented: reliable global comparison on the finite but potentially
  enormous grid, including numerical ties, remains to be done.
* Nhyper `{m}`: for positive data `1 <= r <= min(m,a)`, with the same
  finite population bound for each `r`; hence a maximum is attained.
  All-zero data have `r=0` and arbitrary `N>=m`. Mixed zeros and positives
  are infeasible. Not implemented: the joint profile needs certified grid
  comparisons.

All these bounds and ratio statements are algebraic; the reference checks
below are numerical corroboration, not proofs. At fixed `k`, hyper with
`N -> infinity` and `m/N -> p` tends to `Binomial(k,p)`. At fixed positive
`r`, nhyper under the same limit has `x-r -> NegativeBinomial(r,p)`.
The latter is a limit of the unrestricted-composition family, not an
escape in the held-`m` cases above. No new escape is claimed to be detected.

The pre-existing `no-mle` labels for the unrestricted and single-design
holds were inaccurate: for example, constant samples can have likelihood
one. They now say `not-implemented`; this does not implement those holds.
Existing bisection uses compensated long-double log ratios and reports ties
within a rounding tolerance. It is not a formal interval-arithmetic proof of
all floating-point signs, so pathological near ties remain a numerical
limitation even for the two implemented subsets.

## Validation

The independent [oracle](2026-10-30-holds-hyper/oracle.R) made 12,367
checks against `stats::dhyper` and `extraDistr::dnhyper`: symmetry, monotone
stopping-count ratios, and the population ratio and tail bound. At
`N = 10,000, 100,000, 1,000,000`, maximum absolute density differences
from the binomial limit were `1.0944e-4, 1.0938e-5, 1.0938e-6`; for the
negative binomial they were `4.6894e-5, 4.6877e-6, 4.6875e-7`.
C++ tests enumerate small grids; R tests compare the new fits with reference
finite grids and exercise fixed attributes, ties and error paths.

Final checks on simbox: `just cpp-test` 6/6; `just test` 30,812
passed, 0 failed, 0 warnings, 0 skipped; `just quality hyper nhyper`
2/2. The local full C++ suite also passed 6/6. Targeted C++ grid tests
passed 2 cases and 13,920 assertions; targeted R tests passed 195 hyper
and 330 nhyper assertions with no failures, warnings or skips.

## Open items

Implement the five remaining subsets with global stopping and reliable
integer comparisons, including explicit numerical-failure statuses where
required. Establish and test hyper `{m}` finite-versus-binomial comparisons,
including equality and escapes. Add independent enumeration on larger and
more varied samples and test all associated error and escape paths. The
inherited rounding-based tie policy needs a rigorous sign certificate to
meet a literal exact-integer guarantee for adversarial near ties. No changes
to TASK-78-owned files were made or identified as necessary for the two
implemented subsets.
