---
id: dunif
inference_numeric:
  - regime: unreviewed or unavailable
    available: false
    condition: "integer endpoints free"
    reason: "integer endpoints are discrete and excluded from the free inference vector"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates"
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
relations:
  transform_of:
  - family: bbinom
    at:
      size: max-min
      alpha: 1
      beta: 1
    transform: shift
    shift: min
  - family: zeta
    at:
      s: 0
    transform: shift
    shift: min-1
    configuration:
      kernel: zipf
      truncate:
      - 1
      - max-min+1
    coefficient_map:
      N: max-min+1
      s: 0
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
existence:
  uniqueness: proved
  statement: >-
    With both integer endpoints free, every nonempty integer sample has a unique MLE at its sample minimum and maximum. Holding either endpoint leaves the other at its sample extreme, provided the held interval contains every observation. With both endpoints held, only compatibility is evaluated.
  reason: >-
    The likelihood is $(b-a+1)^{-n}$ on a compatible integer interval. It strictly decreases with its number of integers, so each free endpoint must be its sample extreme. Unlike a continuous interval, a one-point integer interval has positive probability and is admissible.
---

The discrete uniform distribution assigns equal probability to each integer between a lower endpoint $a$ and an upper endpoint $b$.

## Details

A free endpoint is estimated by its sample extreme. A held endpoint specifies a known integer limit and must contain the entire sample. Equal endpoints are allowed: a constant sample with both endpoints free gives a point mass at that integer. With both endpoints held, fitting evaluates the specified law after checking compatibility.
