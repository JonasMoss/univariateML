---
id: kumar
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite a > 0 and b > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Kumaraswamy
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
- name: a
  symbol: 'a'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: b
  symbol: 'b'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dkumar
  sampler: rkumar
  control:
    a: Optional held a; `NULL` estimates it.
    b: Optional held b; `NULL` estimates it.
    a0: Optional positive starting value for `a`; `NULL` uses the automatic start.
    reltol: Relative step tolerance, with an absolute floor of the same size.
    iterlim: Maximum number of iterations.
log_density: log(a) + log(b) + (a-1)*log(x) + (b-1)*log(1-x**a)
references:
- Jones, M. C. (2009). Kumaraswamy’s distribution, a beta-type distribution with some tractability advantages. Statistical
  Methodology, 6(1), 70–81.
- Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes. Journal
  of Hydrology, 46(1–2), 79–88.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
limits:
- name: point-mass collapse
  firmness: proved
  when: '$a$ tends to infinity and $b=c^{-a}$ for a fixed $0<c<1$'
  detect: 'every observation equals $c$ and neither shape is held'
  status: Unbounded
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlkumar())
  mlkumar(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: genbeta
    at:
      shape1: 1
      shape2: b
      shape3: a
      scale: 1
  transform_of:
  - family: beta
    at:
      shape1: 1
      shape2: b
    transform: power
    power: a
fit:
  method: scalar-solve
  certified: partial
  note: 'kumar/README.md, Details; kumar/mle.hpp (public fitter and held overloads); kumar/test.cpp: Held a gives
    analytic b and held b a decreasing bracketed score; coercivity of the joint profile proves existence, not uniqueness
    of its numerical root.'
areas:
- proportions
classification: specialist
classification_reason: Used to model bounded proportions needing an explicit elementary quantile function.
summary: Kumaraswamy provides a model for bounded proportions needing an explicit elementary quantile function.
aliases: []
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With both shapes free, a finite MLE exists for every nonconstant
    sample in $(0,1)$, but joint uniqueness is not established. Constant
    data have unbounded likelihood. Holding either shape gives a unique
    finite MLE even for constant interior data. Holding both leaves only
    likelihood evaluation.
  reason: >-
    Put $t_i=-\log x_i$ and $S(a)=\sum_i\log(1-e^{-at_i})$.
    Profiling gives $\hat b=-n/S(a)$. As $a\to0$ the profile behaves like
    $-n\log|\log a|+O(1)$. As $a\to\infty$ it is
    $a(n\min_i t_i-\sum_i t_i)+n\log a+O(1)$, which tends to
    $-\infty$ for nonconstant data and to $+\infty$ for constant data.
    Hence the continuous profile attains a maximum in the nonconstant case.
    With $a$ held the likelihood is strictly concave in $b$.
    With $b$ held the score in $a$ strictly decreases from $+\infty$ to
    $-\sum_i t_i<0$. For $b<1$ its derivative is at most $-nb/a^2<0$,
    using $(at_i)^2e^{at_i}/(e^{at_i}-1)^2\le1$.
---

The Kumaraswamy distribution models continuous proportions strictly between zero and one. Its two shapes control behaviour near the lower and upper ends of the interval. It offers an elementary quantile function as an alternative to the beta distribution.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) derives these boundary conditions, including the relevant held parameters.

With both shapes free, constant observations have unbounded likelihood. If their common value is $c$, taking $a\to\infty$ and $b=c^{-a}$ concentrates the distribution at $c$ and makes its density there grow without bound.

Every nonconstant interior sample has a finite maximum because the likelihood, after profiling out $b$, tends to $-\infty$ at both ends of the positive range of $a$. This proves existence, but does not prove uniqueness of the maximum or certify the numerical root search. The fit uses Newton iteration with a bracketed profile fallback.

With $a$ held, $\hat b=-n/\sum_i\log(1-x_i^a)$. With $b$ held, the strictly decreasing score for $a$ has a unique positive root, found by bracketing. These conditional fits exist even for constant interior data. Holding both evaluates the likelihood.
