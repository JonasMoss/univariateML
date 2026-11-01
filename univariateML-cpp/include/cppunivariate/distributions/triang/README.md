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
    iterlim: Maximum iterations per endpoint solve.
log_density: log(2) - log(b-a) + Piecewise((log((x-a)/(c-a)), x < c), (log((b-x)/(b-c)), x > c), (0, True))
holds: []
limits:
- name: zero-width collapse
  firmness: proved
  when: Free interval width tends to zero.
  detect: Constant data and every held coefficient equals the observation.
  status: Unbounded
  message: unbounded
- name: incompatible held parameters
  firmness: proved
  when: Held parameters prevent positive likelihood.
  detect: Data outside held endpoints, non-mode observed endpoints, or inconsistent parameter ordering.
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
  method: multistart
  certified: partial
  note: 'triang/README.md, Details; triang/mle.hpp (public fitter and held overloads); triang/test.cpp: Constant
    samples with a held exterior mode have the explicit endpoint maxima in Details; general nonconstant fits enumerate
    numerical profile candidates without a global certificate.'
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
---

The triangular distribution describes bounded measurements with a single mode.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) derives these boundary conditions, including the relevant held parameters.

Constant samples can have an attained maximum when a held endpoint or mode prevents the interval from collapsing.

Endpoint modes are included, with one-sided density `2/(b-a)` at the mode.
Every nonconstant compatible sample has an attained maximum; constant data
have unbounded likelihood only when the held parameters allow the width to
collapse at the observation. Observations outside held endpoints, both held
endpoints observed, or held parameters inconsistent with `a < b` and
`a <= c <= b` give an error. Distinct maximizing modes need not be unique;
the smallest mode is returned for numerical ties, with a warning when another
candidate is within `1e-10 * length(x)` in log likelihood.

For a constant sample at `z` and held mode `c < z`, a free lower endpoint equals `c`; a free upper endpoint is `z + sqrt((z-a)*(z-c))`. The reflected formulas apply when `c > z`. These attained exterior-mode maxima are evaluated directly.

### Endpoint-mode inversion (TASK-134)

For `c = a`, write `z = (x-a)/(b-a)`. Then `F = z(2-z)`, so the
lower inverse is `z = p/(1+sqrt(1-p))`. For `c = b` the reflected
formula gives the distance from `b`. These identities avoid cancellation in
the short distance; the other half of each inverse uses the direct square
root. Public quantiles use log-tail inputs before exponentiating the required
small probability. The historical sampling transform is retained for seeded
sampling compatibility.
