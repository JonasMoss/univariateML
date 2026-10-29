---
id: sgomp
name: Shifted Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: b
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: eta
  role: shape
  domain: '[0, inf)'
  simulate: 3
  kind: regular
r:
  export: true
  density: dsgomp
  sampler: rsgomp
  control:
    b: Optional fixed positive rate; `NULL` estimates it.
    eta: Optional fixed nonnegative shape; `NULL` estimates it.
    reltol: Tolerance for the scalar profile searches.
    iterlim: Maximum iterations per scalar search.
log_density: log(b) - b*x - eta*exp(-b*x) + log(1 + eta*(1-exp(-b*x)))
examples: |
  set.seed(3)
  mlsgomp(rsgomp(100, 1, 3))
holds: []
---

The shifted Gompertz distribution models nonnegative waiting times. The
parameter `b` is a rate; this family has no location parameter.

## Details

The domain includes `eta = 0`, the attained exponential member. The fitter
prefers that member within $10^{-8}n$ log-likelihood units. With both
parameters free, constant samples have unbounded likelihood and are rejected.
An all-zero sample has unbounded likelihood whenever `b` is free.
Zeros are valid observations. The joint likelihood need not have an interior
maximum, and uniqueness is not asserted. The shape is profiled by its strictly
monotone score; all grid-bracketed rate maxima are refined on a finite interval
bounded by likelihood envelopes. This numerical search is not a certified
enumeration of every stationary point.
