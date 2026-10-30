---
id: dlaplace
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
