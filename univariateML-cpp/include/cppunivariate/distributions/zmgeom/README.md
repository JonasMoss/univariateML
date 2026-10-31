---
id: zmgeom
name: Zero-modified geometric
support:
  type: integer
  lower: 0
  upper: inf
  closed:
  - true
  - false
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmgeom
  sampler: rzmgeom
  control:
    prob: "Optional held prob; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(log(prob)+(x-1)*log(1-prob)), True))
examples: |
  set.seed(1)
  x <- rml(200, mlzmgeom())
  mlzmgeom(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: geom
    at:
      prob: prob
    type: zero-modified
    mass: p0
  special_case_of:
  - family: zmnbinom
    at:
      size: 1
      prob: prob
      p0: p0
fit:
  method: multistart
  certified: 'yes'
  note: 'zmgeom/README.md, Details; zmgeom/mle.hpp (public fitter and held overloads); zmgeom/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model geometric positive counts with an independently adjustable zero probability.
summary: Zero-modified geometric provides a model for geometric positive counts with an independently adjustable
  zero probability.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-modified geometric distributions model counts with a separate zero component.

## Details

With no positives the fitter returns `p0 = 1` and conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.

The zero-indicator likelihood and the positive conditional likelihood factorize. Holding `p0`, including its empirical optimum, leaves the positive conditional fit unchanged. Held `p0 = 0` with zeros or `p0 = 1` with positives is incompatible and stops. With no positives, base parameters are unidentified; supplied values are preserved and free base parameters use conventional representatives.
