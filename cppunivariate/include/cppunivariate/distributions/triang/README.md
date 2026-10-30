---
id: triang
name: Triangular
support: {type: real, lower: "a", upper: "b", closed: [true, true], endpoint_log_density: {lower: "if (c == a) log(2) - log(b-a) else -Inf", upper: "if (c == b) log(2) - log(b-a) else -Inf"}}
params:
- name: a
  role: location
  domain: (-inf, b)
  simulate: 0
  kind: endpoint
- name: b
  role: location
  domain: (a, inf)
  simulate: 1
  kind: endpoint
- name: c
  role: location
  domain: '[a, b]'
  simulate: 0.4
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
  when: Free interval width tends to zero.
  detect: Constant data and every held coefficient equals the observation.
  status: Unbounded
  message: unbounded
- name: incompatible held parameters
  when: Held parameters prevent positive likelihood.
  detect: Data outside held endpoints, non-mode observed endpoints, or inconsistent parameter ordering.
  status: Incompatible
  message: incompatible
examples: |
  set.seed(1)
  x <- rml(200, mltriang())
  mltriang(x)
---

The triangular distribution describes bounded measurements with a single mode.

## Details

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
