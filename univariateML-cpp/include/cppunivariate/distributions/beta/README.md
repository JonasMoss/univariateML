---
id: beta
inference_numeric:
  - regime: interior
    available: true
    condition: "shape1 > 0 and shape2 > 0 finite; either or both free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "a shape at zero or a nonfinite limit"
    reason: "beta boundary and limit models are outside the interior derivative regime"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Beta
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: stats::dbeta
  sampler: stats::rbeta
  control:
    shape1: Optional held shape1; `NULL` estimates it.
    shape2: Optional held shape2; `NULL` estimates it.
    reltol: Relative step tolerance in `log(shape2)`.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: (shape1-1)*log(x) + (shape2-1)*log(1-x) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York."
holds: []
limits:
- name: point-mass collapse
  firmness: proved
  when: Both shapes grow as $\alpha=tc$ and $\beta=t(1-c)$ with $t\to\infty$ and $0<c<1$
  detect: All observations equal the same value $c$ and both shapes are free
  status: Degenerate
  message: does not exist
examples: |
  set.seed(1)
  x <- rml(200, mlbeta())
  mlbeta(x)

summary: The beta distribution models continuous proportions on the unit interval.
classification: classical
classification_reason: A standard model for continuous proportions with two independently adjustable boundary shapes.
aliases:
- "beta of the first kind"
key_references:
- 1

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: genbeta
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: 1
  - family: nsbeta
    at:
      shape1: shape1
      shape2: shape2
      min: 0
      max: 1
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'beta/README.md, Details; beta/mle.hpp (public fitter and held overloads); beta/test.cpp: Strictly concave
    beta likelihood in its two shapes; held-shape digamma scores decrease strictly. nsbeta uses supplied endpoints;
    unsupported endpoint fits stop.'
areas:
- proportions
existence:
  uniqueness: proved
  statement: >-
    For a nonempty sample strictly between zero and one, the MLE with both
    shapes free exists if and only if the observations are not all equal,
    and it is then unique. With either finite positive shape held, the
    remaining shape has a unique finite MLE even for constant data.
    With both shapes held, the likelihood is evaluated at those values.
  reason: >-
    The beta family is a minimal regular exponential family with natural
    parameter $(\alpha-1,\beta-1)$ and statistic $(\log x,\log(1-x))$.
    Its log likelihood is strictly concave. The interior of the convex support
    is $e^u+e^v<1$. Strict Jensen inequalities put the mean statistic there
    exactly for nonconstant samples, giving existence of the finite maximum.
    For constant data $x=c$, the path $(\alpha,\beta)=(tc,t(1-c))$ has
    density at $c$ growing as $\sqrt{t}$ by Stirling's formula.
    With one shape $h>0$ held, the free-shape score per observation is
    $a+\psi(z+h)-\psi(z)$, where $a<0$ is the corresponding mean log observation.
    Its derivative is $\psi_1(z+h)-\psi_1(z)<0$, and its limits are
    positive infinity at zero and $a<0$ at infinity.

---

The beta distribution models proportions and other continuous quantities strictly between zero and one.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) derives these boundary conditions, including the relevant held parameters.

The shapes $\alpha$ and $\beta$ control behavior near zero and one,
respectively. Values below one allow density to increase towards the
corresponding endpoint. Their ratio determines the center, while their
sum controls concentration at a fixed ratio.

With both shapes free, nonconstant data have a unique finite maximum.
The fit uses a scalar Newton iteration with an inner digamma inversion.
If this route fails, damped Newton ascent on the strictly concave joint
log likelihood provides a fallback. Exhausted solves report numerical failure.
Constant data have unbounded likelihood as both shapes increase at a fixed ratio.

With either finite positive shape held, the remaining shape has a unique
finite maximum even for constant data. Its strictly decreasing digamma
score is solved by bracketing. With both shapes held, the likelihood is
evaluated at the supplied values. Observations at zero or one are excluded
in every case.

## Decision history

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.

For one held shape $h$, the free-shape bracket starts at $z=\max(1,2h/T)$, with $T$ the negative mean log observation in the corresponding coordinate, subject to representability. The bound $\psi'(z)\le z^{-1}+z^{-2}$ implies $\psi(z+h)-\psi(z)\le2h/z$ for $z\ge1$. Thus tiny log complements retain a signed bracket without requiring hundreds of doubling passes; computed endpoint signs still decide acceptance.
