---
id: dlaplace
inference_numeric:
  - regime: primary interior
    available: true
    condition: "integer location known and held; 0 < scale < 1 free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates"
name: Discrete Laplace
support: {type: integer, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: location
  symbol: '\mu'
  role: integer centre
  domain: (-inf, inf)
  integer: yes
  simulate: 0
  kind: regular
- name: scale
  symbol: 'b'
  role: geometric probability
  domain: (0, 1)
  simulate: 0.5
  kind: regular
r:
  density: ddlaplace
  sampler: rdlaplace
  control:
    location: Optional fixed integer location; NULL estimates it.
    scale: Optional fixed probability strictly between zero and one; NULL estimates it.
log_density: "log(1-scale)-log(1+scale)+Abs(x-location)*log(scale)"
limits:
- name: point mass at location
  when: '$scale \to 0$'
  detect: all observations equal the fitted or held location
  status: PointMass
holds: []
examples: |
  x <- diff(datasets::discoveries)
  mldlaplace(x)

group: skewed and robust location-scale
construction:
- base
relations:
  convolution_of:
  - family: geom
    operation: difference
    components:
    - family: geom
      at:
        prob: 1-scale
    - family: geom
      at:
        prob: 1-scale
    shift: location
fit:
  method: closed-form
  certified: 'yes'
  note: 'dlaplace/README.md, Details; dlaplace/mle.hpp (public fitter and held overloads); dlaplace/test.cpp: Closed-form
    maximum for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model integer deviations with geometric decay on either side of a central value.
summary: Discrete Laplace provides a model for integer deviations with geometric decay on either side of a central
  value.
aliases:
- "two-sided geometric"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
---

Discrete Laplace distributions model integer observations with symmetric geometric tails.

## Details

Location is restricted to integers. The reference RNG permits a noninteger
location by translating the integer lattice, while its density does not enforce
that lattice; such locations are not part of this fitter's model.
The smallest integer sample median is returned, with a warning when the median
interval contains multiple integers. This rule also applies with scale held.
Writing $d = n^{-1} \sum_i |x_i-\mu|$, the scale estimate is
$d/(\sqrt{1+d^2}+1)$, obtained from $d(1-p^2)=2p$.
Absolute deviations are minimized by the integer medians, and the scale score
has exactly one root for positive deviations. These facts establish global
maximality for all supported fixed subsets. Constant data escape to a point
mass only if the held location equals that constant or location is free;
with a different held location the scale MLE is interior. Holding scale removes
this escape.
