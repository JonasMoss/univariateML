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
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
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
  certified: 'yes'
  note: 'dunif/README.md, Details; dunif/mle.hpp (public fitter and held overloads); dunif/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- sampling and quality control
classification: classical
classification_reason: A standard model for equally likely integers in a finite range.
summary: Discrete uniform provides a model for equally likely integers in a finite range.
aliases: []
key_references:
- 1
---

The discrete uniform distribution assigns equal probability to every integer between two endpoints.

## Details

The boundary maximum likelihood estimates are the sample minimum and maximum. Both endpoints must be integers, with `min <= max`; the support contains every integer between them, including both endpoints.
