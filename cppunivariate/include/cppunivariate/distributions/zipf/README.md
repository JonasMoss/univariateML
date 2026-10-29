---
id: zipf
name: Zipf
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: 'N'
  role: upper
  domain: '[1, inf)'
  simulate: 3.0
  integer: yes
  bound: upper
  kind: endpoint
- name: s
  role: shape
  domain: '[0, inf)'
  simulate: 0.5
  kind: regular
r:
  density: dzipf
  sampler: rzipf
  control:
    reltol: "Relative tolerance on the Newton step, with an absolute floor of the same size."
    iterlim: "Maximum number of Newton iterations."
log_density: "-s*log(x) - log(Sum(k**(-s), (k, 1, N)))"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  AIC(mlzipf(corbet))
  AIC(mllgser(corbet))
holds:
- held:
  - 'N'
  status: not-implemented
  reason: The adapter does not accept `N`; this hold is not implemented (P2).
- held:
  - s
  status: not-implemented
  reason: The adapter does not accept `s`; this hold is not implemented (P2).
---

The finite Zipf distribution describes power-law frequencies on consecutive positive integers.

## Details

The fitted upper endpoint `N` is `max(x)` and is a positive integer. The support is the integers from one to `N`. A nonpositive score at zero selects the attained uniform boundary `s = 0`. Large supports use bounded-memory Euler–Maclaurin harmonic sums. With all observations equal to one the estimate is not unique and the fitter warns. This function fits observations directly; for rank-abundance data see `sads::fitzipf`.

For fixed `s > 1`, the limit as `N` tends to infinity is the zeta distribution, fitted by `mlzeta`.
