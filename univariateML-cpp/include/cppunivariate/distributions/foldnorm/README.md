---
id: foldnorm
inference_numeric:
  - regime: "symmetric fixed location"
    available: true
    condition: "mu = 0 held, finite sigma > 0 free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: primary interior
    available: true
    condition: "finite mu > 0 and sigma > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Folded Normal
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: mu
  symbol: '\mu'
  role: parent normal location
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: parent normal scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dfoldnorm
  sampler: rfoldnorm
  control:
    mu: Optional fixed nonnegative parent location; NULL estimates it.
    sigma: Optional fixed positive parent scale; NULL estimates it.
    reltol: Positive tolerance for the scalar profile search.
    iterlim: Positive iteration limit for each bracket refinement.
log_density: -log(sigma) - log(2*pi)/2 - (x-mu)**2/(2*sigma**2) + log(1+exp(-2*x*mu/sigma**2))
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlfoldnorm())
  mlfoldnorm(x)

group: normal and Student
construction:
- folding
relations:
  transform_of:
  - family: norm
    transform: abs
    at:
      mean: mu
      sd: sigma
fit:
  method: profile-search
  certified: partial
  note: 'foldnorm/README.md, Details; foldnorm/mle.hpp (public fitter and held overloads); foldnorm/test.cpp: Joint
    profile has analytic bounds and convex-chord likelihood bounds; held-coordinate profiles are searched numerically.
    The joint tolerance certificate does not establish every held subset.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model unsigned Gaussian measurements when the sign is discarded.
summary: Folded Normal provides a model for unsigned Gaussian measurements when the sign is discarded.
aliases: []
limits:
  - name: scale collapse
    firmness: proved
    when: 'As $\sigma\to0$ with $\mu=x_0\ge0$, the density at $x_0$ diverges.'
    detect: 'Scale is free and all observations equal $x_0$; location is free or held at $x_0$.'
    status: Unbounded
    message: unbounded
    kind: nonexistence
references:
- Leone, F. C., Nelson, L. S. and Nottingham, R. B. (1961). The Folded Normal Distribution. Technometrics, 3(4),
  543–550. doi:10.1080/00401706.1961.10489974.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite maximum exists for every nonconstant
    nonnegative sample, including a possible maximum at $\mu=0$. Constant
    samples have unbounded likelihood. With $\mu$ held and scale free, a
    finite maximum exists unless every observation equals $\mu$. With positive
    $\sigma$ held, the location maximum exists and is unique. Joint uniqueness
    and uniqueness with positive $\mu$ held are not established here.
  reason: >-
    For a nonconstant sample, the likelihood tends to zero as scale vanishes
    or as the coefficients escape to infinity, so a maximum is attained.
    The same argument applies with location held unless all residuals vanish.
    With scale held, the positive-location score equation is
    $n^{-1}\sum_i x_i\tanh(x_i\mu/\sigma^2)/\mu=1$.
    Its left side decreases strictly for positive $\mu$ whenever any observation
    is positive. Thus there is one positive maximizer if
    $n^{-1}\sum_i x_i^2>\sigma^2$, and otherwise the unique maximizer is zero.
---

The folded normal distribution models unsigned Gaussian measurements, obtained by taking the absolute value of a normal variable.

## Details

The coefficients $\mu$ and $\sigma$ are the parent normal location and scale. The sign of the parent location is unidentifiable, so $\mu$ is restricted to nonnegative values. The attained member $\mu=0$ is the half-normal with endpoint zero. Zeros are allowed and use the density limit from above.

For nonconstant nonnegative data a maximum is attained, possibly at the half-normal boundary. Constant data have unbounded likelihood when both coefficients are free. Holding location prevents this collapse unless all observations equal that location. Holding positive scale permits constant data and gives a unique location fit.

Boundary and interior candidates can compete. The joint fitter compares the half-normal boundary with candidates from an analytically bounded scalar profile. Convex-chord upper bounds certify the joint search to a likelihood tolerance. With positive location held, similar bounds check the scale search. Exhausted bounds report numerical failure. These comparisons do not prove uniqueness of the joint or held-location maximum.

At constant $x_0=\mu>0$, the positive normal component alone is $1/(\sqrt{2\pi}\sigma)$; at $x_0=\mu=0$ the density is twice this. This proves the declared scale-collapse paths.

## R notes

Either coefficient can be held by supplying its value. `mlhnorm` also allows the half-normal endpoint to be estimated, whereas `mlfoldnorm` keeps the folding point at zero.
