---
id: zipf
name: Zipf
support: {type: integer, lower: "1", upper: "N", closed: [true, true]}
params:
- name: 'N'
  symbol: 'N'
  role: upper
  domain: '[1, inf)'
  simulate: 10
  integer: yes
  bound: upper
  kind: endpoint
- name: s
  symbol: 's'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dzipf
  sampler: rzipf
  control:
    "N": "Optional integer upper endpoint; NULL estimates it as max(x)."
    s: "Optional finite nonnegative shape; NULL estimates it."
    reltol: "Relative tolerance on the Newton step, with an absolute floor of the same size."
    iterlim: "Maximum number of Newton iterations."
log_density: "-s*log(x) - log(Sum(k**(-s), (k, 1, N)))"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
limits:
- name: point mass at one
  when: '$s \to \infty$'
  detect: all observations are one and held N exceeds one
  status: PointMass
  fixed: ['N']
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlzipf())
  mlzipf(x)
---

The finite Zipf distribution describes power-law frequencies on consecutive positive integers.

## Details

The fitted upper endpoint `N` is `max(x)` and is a positive integer. The support is the integers from one to `N`. A nonpositive score at zero selects the attained uniform boundary `s = 0`. Large supports use bounded-memory Euler–Maclaurin harmonic sums. With all observations equal to one the estimate is not unique and the fitter warns. This function fits observations directly; for rank-abundance data see `sads::fitzipf`.

For fixed `s > 1`, the limit as `N` tends to infinity is the zeta distribution, fitted by `mlzeta`.

With `N` held above one, all-ones samples have no finite shape MLE. Holding `s` always selects `N = max(x)` when the endpoint is free.

## d/p/q/r

N is capped at .Machine$integer.max; s = 0 is uniform. Both tails are summed directly.
