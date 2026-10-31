---
id: dunif
name: Discrete uniform
support: {type: integer, lower: "min", upper: "max", closed: [true, true]}
params:
- name: min
  symbol: 'a'
  role: lower
  domain: (-inf, max]
  simulate: 0
  bound: lower
  integer: yes
  kind: endpoint
- name: max
  symbol: 'b'
  role: upper
  domain: '[min, inf)'
  simulate: 10.0
  bound: upper
  integer: yes
  kind: endpoint
r:
  density: ddunif
  sampler: rdunif
  control:
    min: "Optional held integer lower endpoint; NULL estimates it."
    max: "Optional held integer upper endpoint; NULL estimates it."
log_density: "-log(max - min + 1)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mldunif())
  mldunif(x)

group: finite sampling
construction:
- base
relations: {}
fit:
  method: closed-form
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- sampling and quality control
classification: classical
classification_reason: A standard distribution in introductory probability or applied statistical modelling.
---

The discrete uniform distribution assigns equal probability to every integer between two endpoints.

## Details

The boundary maximum likelihood estimates are the sample minimum and maximum. Both endpoints must be integers, with `min <= max`; the support contains every integer between them, including both endpoints.
