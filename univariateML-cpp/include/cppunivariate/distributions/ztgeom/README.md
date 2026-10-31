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
relations: {}
fit:
  method: scalar-solve
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- counts and ecology
classification: specialist
classification_reason: Used within counts and ecology; classified by its specialised modelling role.
---

Zero-truncated geometric distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 1`. This boundary is returned as an attained member.

Supplying `prob` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
