---
id: dunif
name: Discrete uniform
support: {type: integer, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: min
  role: lower
  domain: (-inf, max]
  simulate: 2.0
  bound: lower
  integer: yes
  kind: endpoint
- name: max
  role: upper
  domain: '[min, inf)'
  simulate: 10.0
  bound: upper
  integer: yes
  kind: endpoint
r:
  density: extraDistr::ddunif
  sampler: extraDistr::rdunif
  control: {}
log_density: "-log(max - min + 1)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mldunif(corbet)
holds:
- held:
  - min
  status: not-implemented
  reason: The adapter does not accept `min`; this hold is not implemented (P2).
- held:
  - max
  status: not-implemented
  reason: The adapter does not accept `max`; this hold is not implemented (P2).
---

The discrete uniform distribution assigns equal probability to every integer between two endpoints.

## Details

The boundary maximum likelihood estimates are the sample minimum and maximum. Both endpoints must be integers, with `min <= max`; the support contains every integer between them, including both endpoints.
