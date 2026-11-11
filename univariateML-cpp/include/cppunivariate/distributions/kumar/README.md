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
    reltol: "Requested relative accuracy in both shapes after propagating the profile correction (default 1e-10)."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
  certified: 'yes'
  note: 'Unique global maximum from strict profile concavity; padded opposite score signs enclose the root. Exact-arithmetic certificate conditional on computed comparisons, with a practical floating-point allowance; unresolved signs or budget stop.'
areas:
- proportions
classification: specialist
classification_reason: Used to model bounded proportions needing an explicit elementary quantile function.
summary: Kumaraswamy provides a model for bounded proportions needing an explicit elementary quantile function.
aliases: []
key_references:
- 3
existence:
  uniqueness: proved
  statement: >-
    With both shapes free, a finite MLE exists for every nonconstant
    sample in $(0,1)$, and it is unique. Constant
    data have unbounded likelihood. Holding either shape gives a unique
    finite MLE even for constant interior data. Holding both leaves only
    likelihood evaluation.
  reason: >-
    Put $t_i=-\log x_i$ and $S(a)=\sum_i\log(1-e^{-at_i})$.
    Profiling gives $\hat b=-n/S(a)$. As $a\to0$ the profile behaves like
    $-n\log|\log a|+O(1)$. As $a\to\infty$ it is
    $a(n\min_i t_i-\sum_i t_i)+n\log a+O(1)$, which tends to
    $-\infty$ for nonconstant data and to $+\infty$ for constant data.
    Hence the continuous profile attains a maximum in the nonconstant case. Strict concavity of this profile, proved below, makes the maximum unique.
    With $a$ held the likelihood is strictly concave in $b$.
    With $b$ held the score in $a$ strictly decreases from $+\infty$ to
    $-\sum_i t_i<0$. For $b<1$ its derivative is at most $-nb/a^2<0$,
    using $(at_i)^2e^{at_i}/(e^{at_i}-1)^2\le1$.
---

The Kumaraswamy distribution models continuous proportions strictly between zero and one. Its two shapes control behaviour near the lower and upper ends of the interval. It offers an elementary quantile function as an alternative to the beta distribution.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) derives these boundary conditions, including the relevant held parameters.

With both shapes free, constant observations have unbounded likelihood. If their common value is $c$, taking $a\to\infty$ and $b=c^{-a}$ concentrates the distribution at $c$ and makes its density there grow without bound.

Every nonconstant interior sample has a finite maximum because the likelihood, after profiling out $b$, tends to $-\infty$ at both ends of the positive range of $a$. The profile is also strictly concave, so its maximum is unique. The fit uses Newton only to propose a location, then certifies opposite score signs around the unique root.

With $a$ held, $\hat b=-n/\sum_i\log(1-x_i^a)$. With $b$ held, the strictly decreasing score for $a$ has a unique positive root, found by bracketing. These conditional fits exist even for constant interior data. Holding both evaluates the likelihood.

## Scalar certificate and uniqueness proof (2026-11-05)

Write $t_i=-\log x_i>0$, $T=\sum_i t_i$ and
$G(a)=\sum_i-\log(1-e^{-at_i})>0$. Profiling $b$ gives

$$h(a)=n\log a-n\log G(a)-aT+G(a)+C.$$

Each summand has the positive exponential expansion
$\sum_{k\ge1}e^{-kat_i}/k$. Thus $G$ is a Laplace transform of a
positive measure and is log-convex (Cauchy--Schwarz applied to its first
and second derivatives). Also

$$G''(a)=\sum_i\frac{t_i^2e^{at_i}}{(e^{at_i}-1)^2}<\frac n{a^2},$$

since $u^2e^u/(e^u-1)^2<1$ for $u>0$ (equivalently,
$2\sinh(u/2)>u$). Therefore

$$h''(a)=-n/a^2-n(\log G)''(a)+G''(a)<0.$$

Together with the endpoint limits above, this proves a unique finite joint
maximum for every nonconstant interior sample. This is an analytic result,
not an inference from a numerical search. Constant samples remain unbounded.
The held-$b$ strict concavity argument above supplies the same root certificate;
held $a$ uses the analytic $b$ maximum.

`certificate.hpp` brackets the unique root of $a h'(a)/n$, halving or
doubling a positive proposal until both endpoint signs are resolved.
Bisection preserves those signs. A residual near zero is accepted only after
two nearby endpoints sign in opposite directions. Relative root width is at
most $5\times10^{-7}$ (tightened by $\max(1,aT/n)$ when the second shape is sensitive); the computed endpoint values of $b$ must also have
log ratio below $10^{-6}$. Since $b(a)=n/G(a)$ increases, these endpoints
bound both coordinates. Historical Newton coefficients are retained only
inside both enclosures (with a $10^{-12}$ relative rounding pad on the profiled second shape). Otherwise the bracket midpoint is returned.

The certificate is exact in real arithmetic conditional on the computed
comparisons. Its practical floating-point score allowance is
$256\epsilon(1+aT/n+|(b-1)D|)(1+n\epsilon)$, where
$D=n^{-1}\sum at_i/\operatorname{expm1}(at_i)$. This is a practical
rounding margin, not outward-rounded interval arithmetic or a libm error
proof. Subnormal/vanishing $G$, nonfinite arithmetic, unresolved signs,
unrepresentable coefficients or an exhausted $16\min(\mathrm{maxit},100000)$
evaluation budget produce `IterationFailure`, never an unchecked fit.
No finite limit model accompanies the constant-sample unbounded stop.

The final profile enclosure bounds both a and b, including the sensitivity d log(b)/d log(a). Secant interpolation selects a point inside this signed box; likelihood rounding and a small residual alone do not establish coefficient accuracy. Refinement evaluations share the global profile-point ledger, allowing the certificate to finish a one-step proposal when the remaining budget suffices.
