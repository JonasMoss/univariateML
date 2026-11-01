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
  firmness: proved
  when: 'The geometric probability $b$ tends to zero with integer location $\mu$ fixed.'
  detect: Scale is free and all observations equal the estimated or held location.
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
existence:
  uniqueness: proved
  statement: >-
    For a nonconstant integer sample with both coefficients free, a finite
    MLE exists. Every integer in the sample median interval maximizes
    location, with the same unique scale. Location is unique exactly when
    that interval contains one integer. Constant samples have only an
    unattained point-mass supremum. With integer location held, a unique
    interior scale MLE exists unless all observations equal that location.
    With scale held strictly between zero and one, the integer medians
    maximize location even for constant samples. Holding both coefficients
    leaves likelihood evaluation.
  reason: >-
    Writing $p=b$ and $d=n^{-1}\sum_i|x_i-\mu|$, the log likelihood per
    observation is $\log(1-p)-\log(1+p)+d\log p$.
    Since $\log p<0$, location maximization is exactly absolute-deviation
    minimization over the integers. For $d>0$, the scale score vanishes
    exactly when $d(1-p^2)=2p$, giving the unique maximizer
    $p=d/(\sqrt{1+d^2}+1)$. For $d=0$, the likelihood decreases strictly
    in $p$ and approaches its supremum only as $p\to0$.
---

The discrete Laplace distribution models integer deviations with symmetric geometric tails. Its location $\mu$ is an integer centre. Its coefficient $b$ lies strictly between zero and one and controls geometric decay, with larger values giving wider tails.

## Details

Every integer in the sample median interval minimizes the sum of absolute deviations. These are exactly the maximizing locations, whether scale is free or held. The fit returns the smallest integer median. For positive mean absolute deviation $d$, the unique scale estimate is $d/(\sqrt{1+d^2}+1)$.

Constant data escape to a point mass only when location is free or held at that constant and scale is free. With a different held location, the scale estimate is interior. Holding scale removes this escape. Scale zero is excluded from the declared parameter space, so the point mass is an unattained limit.

## R notes

The fit warns when the median interval contains multiple integers. Supplying `location` holds an integer centre, and supplying `scale` holds the geometric probability.

The historical reference random generator permits a noninteger location by translating the integer lattice, while its density does not enforce that lattice. Such locations are outside this fitter's model.
