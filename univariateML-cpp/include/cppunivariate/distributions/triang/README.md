---
id: triang
inference_numeric:
  - regime: "interior mode"
    available: false
    condition: "known endpoints a < c < b; mode c free"
    reason: "the moving kink invalidates ordinary observed-Hessian sensitivity despite DQM"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#triangular"
  - regime: "endpoints free"
    available: false
    condition: "at least one endpoint free"
    reason: "moving endpoints have divergent squared scores and are not free inference coordinates"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#triangular"
name: Triangular
support: {type: real, lower: "a", upper: "b", closed: [true, true], endpoint_log_density: {lower: "if (c == a) log(2) - log(b-a) else -Inf", upper: "if (c == b) log(2) - log(b-a) else -Inf"}}
params:
- name: a
  symbol: 'a'
  role: location
  domain: (-inf, b)
  simulate: 0
  kind: endpoint
- name: b
  symbol: 'b'
  role: location
  domain: (a, inf)
  simulate: 1
  kind: endpoint
- name: c
  symbol: 'c'
  role: location
  domain: '[a, b]'
  simulate: 0.5
  kink: yes
  kind: regular
r:
  density: dtriang
  sampler: rtriang
  control:
    a: Optional held a; `NULL` estimates it.
    b: Optional held b; `NULL` estimates it.
    c: Optional held c; `NULL` estimates it.
    reltol: Absolute tolerance on the dimensionless concave endpoint Newton step.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: log(2) - log(b-a) + Piecewise((log((x-a)/(c-a)), x < c), (log((b-x)/(b-c)), x > c), (0, True))
holds: []
limits:
- name: zero-width collapse
  firmness: proved
  when: The free width $b-a$ tends to zero around the common observation.
  detect: The sample is constant, at least one endpoint is free, and every held coefficient equals the observation.
  status: Unbounded
  message: unbounded
- name: incompatible held parameters
  firmness: proved
  when: A valid held endpoint excludes an observation or forces zero density there.
  detect: An observation lies outside a held endpoint, or an observed held endpoint cannot also be the mode.
  status: Incompatible
  message: incompatible
examples: |
  set.seed(1)
  x <- rml(200, mltriang())
  mltriang(x)

group: bounded continuous
construction:
- base
relations: {}
fit:
  method: profile-search
  certified: 'yes'
  note: "triang/mle.hpp: Finite observation-mode enumeration uses concave endpoint solves and tangent upper bounds to prune mode intervals; unresolved candidate solves return IterationFailure. Certification is in exact arithmetic conditional on computed comparisons, with pruning allowance 1e-11*n*(1+abs(logLik)/n) and 1e-10*n separation. It does not imply uniqueness."
areas:
- proportions
classification: specialist
classification_reason: Used to model bounded measurements with a specified interior or endpoint mode.
summary: Triangular provides a model for bounded measurements with a specified interior or endpoint mode.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all three coefficients free, every nonconstant finite sample has an attained MLE, which need not be unique. For compatible held coefficients, a maximum is attained unless the sample is constant and every held coefficient equals that observation. In the latter case, free endpoints permit unbounded width collapse. With all coefficients held, only positive-likelihood compatibility is evaluated.
  reason: >-
    The density is bounded above by $2/(b-a)$. Nonconstant data, or a held value different from a constant observation, bound the width away from zero. Likelihood tends to zero as width diverges. On the remaining bounded parameter set, the endpoint-mode convention makes likelihood upper semicontinuous, so a positive candidate yields attainment. Shrinking a compatible interval around a constant observation proves the collapse case. This argument does not establish uniqueness.
---

The triangular distribution describes bounded measurements between endpoints $a$ and $b$, with mode $c$. Holding an endpoint specifies a known bound; holding the mode specifies the peak, which may lie outside the observed range.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) derives these boundary conditions, including the relevant held parameters.

Constant samples can have an attained maximum when a held endpoint or mode prevents the interval from collapsing.

Endpoint modes are included, with one-sided density $2/(b-a)$ at the mode.
Every nonconstant compatible sample has an attained maximum; constant data
have unbounded likelihood only when the held parameters allow the width to
collapse at the observation. Observations outside held endpoints, both held
endpoints observed, or held parameters inconsistent with $a<b$ and $a\le c\le b$ are incompatible with a positive-likelihood fit. Distinct maximizing modes can tie. A free mode is sought among the distinct observations, with endpoint profiles solved numerically. The interval bounds used to prune candidates do not prove uniqueness.

Nonuniqueness occurs already for observations $(0,1)$. The reflected fits $(a,b,c)=(0,3/2,0)$ and $(-1/2,1,1)$ both maximize likelihood at $16/27$. At fixed endpoints the log-likelihood is convex between successive observations as a function of mode, so a free maximizing mode can be chosen at an observation. With $c=0$, maximizing first over $a$ gives $a=0$, then maximizing $(b-1)/b^3$ gives $b=3/2$. Reflection gives the other maximizer.

For a constant sample at $z$ and held mode $c<z$, a free lower endpoint equals $c$, and a free upper endpoint is $z+\sqrt{(z-a)(z-c)}$. The reflected formulas apply when $c>z$. These attained exterior-mode maxima are evaluated directly.

## R notes

Invalid held ordering is rejected as invalid input. The smallest mode is returned for numerical ties. A warning is emitted when another candidate is within `1e-10 * length(x)` in log likelihood.

## Decision history

### Endpoint-mode inversion (TASK-134)

For `c = a`, write `z = (x-a)/(b-a)`. Then `F = z(2-z)`, so the
lower inverse is `z = p/(1+sqrt(1-p))`. For `c = b` the reflected
formula gives the distance from `b`. These identities avoid cancellation in
the short distance; the other half of each inverse uses the direct square
root. Public quantiles use log-tail inputs before exponentiating the required
small probability. The historical sampling transform is retained for seeded
sampling compatibility.
