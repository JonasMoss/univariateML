---
id: ztgeom
name: Zero-truncated geometric
support:
  type: integer
  lower: 1
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
r:
  density: dztgeom
  sampler: rztgeom
  control:
    prob: "Optional held prob; NULL estimates it."
log_density: log(prob)+(x-1)*log(1-prob)
examples: |
  x <- corbet
  mlztgeom(x)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: geom
    at:
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: ztnbinom
    at:
      size: 1
      prob: prob
  - family: zmgeom
    at:
      prob: prob
      p0: 0
  transform_of:
  - family: geom
    at:
      prob: prob
    transform: shift
    shift: 1
  truncation_of:
  - family: dlaplace
    at:
      location: 1
      scale: 1-prob
    when: prob < 1
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'ztgeom/README.md, Details; ztgeom/mle.hpp (public fitter and held overloads); ztgeom/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model geometric waiting counts sampled only when positive.
summary: Zero-truncated geometric provides a model for geometric waiting counts sampled only when positive.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-truncated geometric distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 1`. This boundary is returned as an attained member.

Supplying `prob` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
