---
id: fatigue
name: Birnbaum-Saunders
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: alpha
  role: shape
  domain: (0, inf)
  simulate: 3.0
  kind: regular
- name: beta
  role: scale
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: threshold
  advice: Supply a finite threshold below every observation; NULL is unsupported because
    of endpoint spikes and a normal escape.
r:
  density: extraDistr::dfatigue
  sampler: extraDistr::rfatigue
  control:
    abstol: Relative Newton step tolerance in `beta` (the argument name is retained for compatibility).
    iterlim: Maximum number of iterations.
log_density: log(x-mu+beta) - 3*log(x-mu)/2 - log(beta)/2 - log(2*alpha) - log(2*pi)/2 - ((x-mu)/beta + beta/(x-mu)-2)/(2*alpha**2)
references:
  - "From, S. G. and Li, L. (2006). Estimation of the parameters of the Birnbaum–Saunders distribution. Communications in Statistics, Theory and Methods, 35(12), 2157–2169. doi:10.1080/03610920600853563."
examples: |
  mlfatigue(precip, mu = 0)
holds:
- held:
  - alpha
  status: not-implemented
  reason: The adapter does not accept `alpha`; this hold is not implemented (P2).
- held:
  - beta
  status: not-implemented
  reason: The adapter does not accept `beta`; this hold is not implemented (P2).
- held: []
  status: no-mle
  reason: 'Estimating `mu` is not supported: the three-parameter fatigue likelihood
    has endpoint spikes and a normal escape, with no weakest shape restriction.'
fit_args:
  mu: 0.0
---

The Birnbaum--Saunders distribution models lifetimes arising from cumulative fatigue damage.

## Details

Supply the threshold `mu`; only `alpha` and `beta` are estimated on `x - mu`. A held threshold must be finite and strictly below every observation. `beta` is a scale parameter. Shifted observations must be strictly positive.

Estimating `mu` as well is not possible: the three-parameter likelihood is unbounded for every sample with a unique minimum. Along $\beta = \varepsilon$, $\alpha^2 = s/\varepsilon$ and $\mu = \min(x) - c\varepsilon^2/s$, half of the probability mass collapses onto the smallest observation, whose log-density grows like $-2\log\varepsilon$, while the other observations keep a finite log-density. Pass a known threshold as `mu`; `mu = NULL` stops with this reason.
