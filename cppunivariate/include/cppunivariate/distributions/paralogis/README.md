---
id: paralogis
name: Paralogistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: actuar::dparalogis
  sampler: actuar::rparalogis
  control:
    shape0: Optional starting value for `shape`; `NULL` uses `max(1.5, 2 / sd(log(x / median(x))))`.
    gradtol: Gradient tolerance for the free fit after dividing data by the sample median; with fixed scale, tolerance on the mean shape score in log coordinates (at most 1e-10).
    iterlim: Maximum number of BFGS or fixed-scale Newton iterations.
log_density: 2*log(shape) + shape*log(x/scale) - log(x) - (shape + 1)*log(1 + (x/scale)^shape)
references:
  - Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley.
  - Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012). Loss Models, From Data to Decisions, Fourth Edition. Wiley.
examples: |
  mlparalogis(precip)
holds:
- held:
  - shape
  status: not-implemented
  reason: The adapter does not accept `shape`; this hold is not implemented (P2).
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
