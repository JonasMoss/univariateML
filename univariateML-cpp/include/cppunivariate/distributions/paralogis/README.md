---
id: paralogis
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Paralogistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dparalogis
  sampler: rparalogis
  control:
    shape: Known positive finite coefficient; NULL estimates it.
    shape0: Optional starting value for `shape`; `NULL` uses `max(1.5, 2 / sd(log(x / median(x))))`.
    gradtol: Gradient tolerance for the free fit after dividing data by the sample median; with fixed scale, tolerance on the mean shape score in log coordinates (at most 1e-10).
    iterlim: Maximum number of BFGS or fixed-scale Newton iterations.
limits:
  - name: unbounded likelihood
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: InvalidStart
    message: unbounded
log_density: 2*log(shape) + shape*log(x/scale) - log(x) - (shape + 1)*log(1 + (x/scale)^shape)
references:
- Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012). Loss Models, From Data to Decisions, Fourth Edition.
  Wiley.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlparalogis())
  mlparalogis(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: shape
      shape2: shape
      scale: scale
fit:
  method: multistart
  certified: partial
  note: 'paralogis/README.md, Details; paralogis/mle.hpp (public fitter and held overloads); paralogis/test.cpp:
    With shape held, the Burr delegate solves the strictly monotone log-scale equation; the default joint shape/scale
    BFGS has no global certificate.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance severity with a constrained Burr shape relationship.
summary: Paralogistic provides a model for insurance severity with a constrained Burr shape relationship.
aliases: []
key_references:
- 3
---

The paralogistic distribution is the Burr type XII distribution with its two
shape parameters equal. It is used for size and loss data.

## Details

Both `shape` and `scale` are estimated. univariateML 1.x estimated only the
shape, with the scale held at 1.

Supply `scale` to hold it at a positive finite value. With scale fixed,
there are no nondegenerate limit-model escapes.
The fixed-scale likelihood tends to minus infinity as shape tends to zero
or infinity, including for constant data, so a finite maximum exists.

### Adapter independence (2026-10-30)

The fully free constant-sample rejection now lives in `paralogis_mle` as `Unbounded`; the reciprocal delegate preserves this status. This moves the former adapter decision without changing nonconstant fits.
