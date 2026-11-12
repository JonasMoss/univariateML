---
id: lgser
inference_numeric:
  - regime: interior
    available: true
    condition: "0 < theta < 1; theta free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Logarithmic series
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: theta
  symbol: '\theta'
  role: rate
  domain: (0, 1)
  simulate: 0.5
  kind: regular
r:
  density: dlgser
  sampler: rlgser
  control:
    theta: "Optional held theta; NULL estimates it."
log_density: "x*log(theta) - log(x) - log(-log(1 - theta))"
references:
- Fisher, R. A., Corbet, A. S., and Williams, C. B. (1943). The relation between the number of species and the number
  of individuals in a random sample of an animal population. The Journal of Animal Ecology, 12(1), 42-58.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  x <- corbet
  mllgser(x)

group: discrete heavy-tailed
construction:
- base
relations:
  special_case_of:
  - family: zmlogarithmic
    at:
      prob: theta
      p0: 0
  - family: ztnbinom
    at:
      size: 0
      prob: 1-theta
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'lgser/README.md, Details; lgser/mle.hpp (public fitter and held overloads); lgser/test.cpp: Strictly monotone
    one-parameter exponential-family or log-shape score gives a bracketed unique root or the documented boundary;
    finite Zipf endpoint and modified zero mass profile exactly.'
areas:
- rank-frequency
existence:
  uniqueness: proved
  statement: >-
    With $\theta$ free, a unique finite MLE exists exactly when $\bar x>1$.
    An all-one sample has an unattained supremum as $\theta\to0$.
    Holding $0<\theta<1$ specifies the law and requires no optimization.
  reason: >-
    In the natural parameter $\eta=\log\theta<0$, the second derivative
    of the log likelihood is minus the sample size times the positive variance.
    The model mean increases continuously from one to infinity, so mean
    matching has exactly one solution when the empirical mean exceeds one.
classification: specialist
classification_reason: Used to model species abundance and other highly skewed positive frequencies.
summary: Logarithmic series provides a model for species abundance and other highly skewed positive frequencies.
aliases:
- "logarithmic"
- "log-series"
key_references:
- 2
modifiers:
  zero_exception: true
  zero:
    modified: {status: implemented, certified: "yes", note: "Logarithmic positive law with theta = prob; attained theta = 0 hurdle extension."}

---


The logarithmic series distribution models positive counts, with a history of use in species-abundance data. Increasing $\theta$ increases the mean and the relative weight of large counts.

## Details

The fit matches the model mean to the empirical mean. Writing
$u=-\log(1-\theta)>0$, its equation is $\operatorname{expm1}(u)/u=\bar x$.
The left side is strictly increasing from one to infinity. A bracketed
scalar solve therefore gives the unique maximum whenever $\bar x>1$.
When every observation is one, the likelihood increases toward the point
mass at one as $\theta\to0$. That boundary is excluded from this family.
Holding $\theta$ evaluates the specified law, including on all-one samples.

## R notes

Supplying `theta` evaluates the likelihood at that value. Held values outside
`(0, 1)` or incompatible with the positive integer observations stop.
An all-one sample with `theta` free stops with a non-existence error.

## Decision history

### Stable scalar solve

A Lambert-W expression lost precision near the all-one boundary and could
return values outside the parameter domain. The current solver brackets
and bisects the equation for $u$ on a logarithmic scale, using a series
for $(\operatorname{expm1}(u)-u)/u$ near zero.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.

The zero-modified exception includes `theta = 0`, the attained law on zero and one, and uses `p0` for the total zero probability. Positive-data fitting retains the logarithmic mean equation; all-zero data leave theta unidentified.

## d/p/q/r

A compensated finite prefix resolves small cuts even when theta is the largest binary64 value below one. For larger cuts, integrating the partial-fraction expansion of coth gives a Stieltjes expansion of the survival in terms of $e^xE_1(x)$, $x=-k\log\theta$. The first omitted Bernoulli term bounds the error for every positive x; the scaled exponential integral uses its own signed next-term bound when x is large. The implementation keeps terms through $B_6$ and requires the $B_8$ bound below $10^{-12}$ of the sum; otherwise the geometric-series evaluator is retained and returns NaN if its bound cannot close. Quantiles invert these bounded tails. Zero-modified evaluators have a separate routing layer and are not changed by this route.
