---
id: power
name: PowerDist
support: {type: real, lower: "0", upper: "alpha", closed: [false, true], endpoint_log_density: {upper: "log(beta) - log(alpha)"}}
params:
- name: alpha
  role: upper
  domain: (0, inf)
  simulate: 1.0
  bound: upper
  kind: endpoint
- name: beta
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
r:
  density: dpower
  sampler: rpower
  control:
    alpha: Optional held alpha; NULL estimates it.
    beta: Optional held beta; NULL estimates it.
    epsilon: Deprecated compatibility argument, validated but ignored; alpha is exactly max(x).
log_density: log(beta) - beta*log(alpha) + (beta-1)*log(x)
references:
  - "Arslan, G. \"A new characterization of the power distribution.\" Journal of Computational and Applied Mathematics 260 (2014): 99-102."
examples: |
  mlpower(precip)
holds: []
limits:
- name: zero likelihood
  fixed: [alpha]
  when: Held alpha is below the sample maximum.
  status: InvalidInput
  message: zero likelihood
---

The power distribution models positive quantities below a finite upper endpoint.

## Details

The fitted support is $(0, \alpha]$. The upper endpoint is exactly
`max(x)` when estimated; its density is `beta/alpha`, the interior limit.
`epsilon` is retained for compatibility and ignored after validation.

A held alpha must be at least max(x). A free beta is the reciprocal of
`mean(log(alpha/x))`. If all observations equal alpha, the likelihood is
unbounded as beta tends to infinity; the fitter stops. With beta held,
constant and singleton samples have an attained fit at alpha = max(x).
