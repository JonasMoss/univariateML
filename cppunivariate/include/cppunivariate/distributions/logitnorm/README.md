---
id: logitnorm
name: LogitNormal
support: {type: real, lower: 0.0, upper: 1.0, closed: [false, false]}
params:
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 2.0
  kind: regular
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: dlogitnorm
  sampler: rlogitnorm
  control: {}
log_density: -log(sigma) - log(2*pi)/2 - log(x) - log(1-x) - (log(x/(1-x))-mu)**2/(2*sigma**2)
references:
  - "Aitchison, J., & Shen, S. M. (1980). Logistic-normal distributions: Some properties and uses. Biometrika, 67(2), 261-272."
examples: |
  AIC(mllogitnorm(USArrests$Rape / 100))
holds:
- held:
  - mu
  status: not-implemented
  reason: The adapter does not accept `mu`; this hold is not implemented (P2).
- held:
  - sigma
  status: not-implemented
  reason: The adapter does not accept `sigma`; this hold is not implemented (P2).
---

The logit-normal distribution models proportions whose logits follow a normal distribution.

## Details

The parameters `mu` and `sigma` describe the normal distribution of the logits, not the mean and standard deviation of the proportions. The scale estimate uses divisor $n$ on the logit scale.
