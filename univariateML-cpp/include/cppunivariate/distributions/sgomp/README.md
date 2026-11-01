---
id: sgomp
inference_numeric:
  - regime: "exponential member with shape known"
    available: true
    condition: "finite b > 0 free; eta = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite b > 0 and eta > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Shifted Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: b
  symbol: 'b'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: eta
  symbol: '\eta'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dsgomp
  sampler: rsgomp
  control:
    b: Optional fixed positive rate; `NULL` estimates it.
    eta: Optional fixed nonnegative shape; `NULL` estimates it.
    reltol: Tolerance for the scalar profile searches.
    iterlim: Maximum iterations per scalar search.
log_density: log(b) - b*x - eta*exp(-b*x) + log(1 + eta*(1-exp(-b*x)))
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsgomp())
  mlsgomp(x)

group: other positive
construction:
- base
relations: {}
fit:
  method: scalar-solve
  certified: partial
  note: 'sgomp/README.md, Details; sgomp/mle.hpp (public fitter and held overloads); sgomp/test.cpp: Held rate leaves
    a strictly monotone shape score; joint rate grid refines sampled peaks without certifying enumeration of all
    stationary points.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model waiting times with flexible onset in diffusion and adoption models.
summary: Shifted Gompertz provides a model for waiting times with flexible onset in diffusion and adoption models.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
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

## d/p/q/r

The eta = 0 member is exponential, with rate b; there is no location parameter.
