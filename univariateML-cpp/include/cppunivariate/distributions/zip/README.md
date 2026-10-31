---
id: zip
name: Zero-inflated Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzip
  sampler: rzip
  control:
    lambda: "Optional held lambda; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
log_density: "Piecewise((log(pi + (1 - pi)*exp(-lambda)), Eq(x, 0)), (log(1 - pi) - lambda + x*log(lambda) - loggamma(x + 1), True))"
limits:
  - model: pois
    when: '$lambda \to 0$ with inflation held below one'
    detect: all observations are zero
    fixed: [pi]
    status: ZeroLimit
  - name: undefined zero-rate reference
    when: held lambda is zero
    fixed: [lambda]
    status: InvalidInput
    message: lambda positive
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  set.seed(1)
  x <- rml(200, mlzip())
  mlzip(x)

group: modified counts
construction:
- modification
relations: {}
fit:
  method: closed-form
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- counts and ecology
classification: specialist
classification_reason: Used within counts and ecology; classified by its specialised modelling role.
---

The zero-inflated Poisson distribution mixes a Poisson count with an additional point mass at zero.

## Details

For zero-deflated data the estimate is `pi = 0`, the Poisson fit. All-zero data give a nonunique point-mass fit: the fitter warns and returns `lambda = 1`, `pi = 1`. These are boundary fits within the family.

Holding `lambda` profiles inflation by clipping the empirical zero fraction adjustment to `[0, 1]`. Holding `pi < 1` gives a finite rate when a count is positive. All-zero data then approach lambda zero, which is undefined by the reference; the fitter stops. A held zero rate is rejected for that reason. Held `pi = 1` is compatible only with zeros.
