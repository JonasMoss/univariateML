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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
  certified: 'yes'
  note: "foldnorm/mle.hpp, foldnorm_mle: Joint and held-positive-location profiles use analytic bounds and convex-chord upper bounds to certify the retained likelihood within tol per observation in exact arithmetic, conditional on computed comparisons. Held scale has a unique monotone location equation; held zero location gives analytic scale. Work-budget exhaustion stops; floating-point comparisons are not outward rounded."
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

After the global likelihood comparison, positive-location fits solve the analytic score in a=mu/sigma^2. The joint profile has sigma^2+a^2 sigma^4=mean(x^2); differentiating this identity propagates the Newton error into both mu and sigma. With positive mu held, the scale correction uses its scalar observed curvature. Likelihood rounding therefore does not determine the final coefficients.

For a positive mode with both the sample minimum and fitted location exceeding forty centered standard deviations, the reflected score correction is below a polynomial factor times exp(-3200). The normal mean and variance are then evaluated from original-data deviations, before normalization can round away the spread. This error bound is far below binary64 resolution at the supported tolerance.

Near the half-normal face, a likelihood difference can round away while the location error remains large. With $b=\mu^2/\overline{x^2}$ and $z_i=x_i/\sqrt{\overline{x^2}}$, the joint profile score has the sign of
$H(b)=(1-b)^2+\overline{z^4c(bz^2/(1-b)^2)}$, where
$c(u)=(\tanh(\sqrt u)/\sqrt u-1)/u$ and $c(0)=-1/3$.
If $H(0)>0$, the boundary is a local minimum. A signed bracket for the adjacent root, with a negative derivative bound throughout the final bracket, replaces that boundary candidate. The power series for $c$ avoids cubic score cancellation; its tail is bounded using $|\tanh t|<3$ on $|t|\le1$. The bracket controls both coefficients, using $\sigma$ as the natural absolute scale for location at zero. The existing global likelihood upper bound remains applicable.

The final conversion from normalized coefficients checks reconstruction error. In particular, subnormal samples whose fitted positive scale lies below the binary64 lattice return a numerical-accuracy failure rather than an `Ok` fit with a rounded or zero scale.

The half-normal quartic coefficient $H(0)=1-\overline{x^4}/(3\overline{x^2}^2)$ uses a practical sign margin of $32\epsilon$. An unresolved quartic sign permits a positive location displacement of order $\sqrt\epsilon$, exceeding the default accuracy target; such a fit reports a conditioning stop. An exact exception is a sample supported on $\{0,c\}$ with positive fraction at most $1/3$: the half-normal maximum follows from
$\operatorname{atanh}(t)>3t/(3-t^2)$ for $0<t<1$, by comparing the positive power-series coefficients. This reduction preserves the attained boundary without relying on a rounded fourth-moment sign.
