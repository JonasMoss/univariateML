---
id: trgamma
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Transformed gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 'a'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dtrgamma
  sampler: rtrgamma
  control:
    scale: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
    shape1: Known positive finite coefficient; NULL estimates it.
    reltol: Tolerance in `log(shape2)` for the one-dimensional profile search (free `shape2`).
    iterlim: Maximum number of iterations per root of the profile score.
log_density: log(shape2) + shape1*shape2*log(x/scale) - (x/scale)**shape2 - log(x) - loggamma(shape1)
limits:
  - name: fixed-endpoint power face
    firmness: proved
    fixed: [scale]
    when: The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held.
    status: PowerBoundary
    message: fixed-endpoint power face
  - name: unbounded likelihood
    firmness: proved
    when: The free coefficients concentrate the distribution at the common positive observation.
    detect: All observations are equal and the free coefficients allow concentration. With scale free, either free shape suffices. With scale held, a free power suffices when the common observation equals $a$.
    status: InvalidData
    message: unbounded
  - model: lnorm
    firmness: proved
    when: 'The gamma shape $k$ tends to infinity with $p\sqrt{k}\to1/\sigma$ and $\log a + \log(k)/p\to\mu$, for fixed $\sigma>0$ and $\mu$.'
    status: LognormalBoundary
  - model: power
    firmness: proved
    when: 'The power $p$ tends to infinity with $kp\to d>0$ and $a$ converging to a positive endpoint.'
    status: PowerBoundary
references:
- Stacy, E. W. (1962). A generalization of the gamma distribution. The Annals of Mathematical Statistics 33(3),
  1187-1192. doi:10.1214/aoms/1177704481.
- Noufaily, A. and Jones, M. C. (2013). On maximization of the likelihood for the generalized gamma distribution.
  Computational Statistics 28, 505-517. doi:10.1007/s00180-012-0314-4.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mltrgamma())
  mltrgamma(x)

group: gamma and Weibull
construction:
- base
- generalisation
relations:
  transform_of:
  - family: gamma
    at:
      shape: shape1
      scale: scale**shape2
    transform: power
    power: shape2
fit:
  method: multistart
  certified: partial
  note: 'trgamma/README.md, Details; trgamma/mle.hpp (public fitter and held overloads); trgamma/test.cpp: Held
    power reduces to gamma and held gamma shape to a concave transformed problem; the free-power/shape profile and
    face enumeration are numerical.'
areas:
- reliability and survival
- waiting times and queues
classification: specialist
classification_reason: Used to model lifetimes and severity sizes related to gamma by a power transform.
summary: Transformed gamma provides a model for lifetimes and severity sizes related to gamma by a power transform.
aliases:
- "Stacy generalized gamma"
- "generalized gamma"
key_references:
- 4
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant positive samples have unbounded
    likelihood. For nonconstant samples, the lognormal or power limit
    can prevent a finite MLE. General uniqueness of an interior maximum
    is not established. With scale free and either shape held, a unique
    finite MLE exists for nonconstant samples. With both shapes held,
    the scale MLE is unique even for constant samples. With scale and
    power held, the remaining gamma shape has a unique finite MLE.
    With scale and gamma shape held, the power MLE is unique unless
    every observation equals the held scale, when likelihood is unbounded.
    With only scale held, attainment is assessed numerically against the
    fixed-endpoint power limit.
  reason: >-
    Holding the power reduces fitting to the gamma family. Holding the
    gamma shape gives a strictly concave profile in log-power after
    eliminating scale. With scale held, the gamma-shape score is strictly
    decreasing; at held gamma shape, the power log-likelihood has second
    derivative $-n/p^2-\sum_i z_i^2 e^{p z_i}<0$, where
    $z_i=\log(x_i/a)$ for transformed gamma and
    $z_i=\log(a/x_i)$ for its reciprocal. Its score crosses zero unless
    all $z_i=0$. Reciprocation adds a parameter-independent Jacobian and
    preserves maximizers.
---

The transformed gamma (Stacy's generalized gamma) distribution is the
distribution of $aG^{1/p}$ for a unit-scale gamma variable $G$ with shape $k$; it models positive sizes, losses and lifetimes.

## Details

With $k\downarrow0$, $p\to\infty$ and $kp\to d>0$, $p/\Gamma(k)\to d$. In the displayed density,
put $u=x/a$. For $u<1$, $u^p\to0$ and the density tends to
$d u^d/x$; for $u>1$ it tends to zero. This is the power density
with endpoint $a$ (support below the endpoint). Scheffé's lemma gives
total-variation convergence. The endpoint value and a global likelihood
comparison are separate questions.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and the scope of the boundary comparisons.

The gamma ($p=1$) and Weibull ($k=1$) distributions are members, not limits. Holding $p$
fixed gives the gamma fit of $x^p$; holding $k$ fixed gives a
strictly concave problem (the Weibull fit when $k=1$). For nonconstant data both have a maximum likelihood estimate.

The unrestricted fit fails to have a finite MLE with positive probability. For nonconstant samples, the inward likelihood slope from the lognormal face has sign opposite
to the sample skewness of the logarithms. Under lognormal sampling the
asymptotic boundary probability is one half. Faces win likelihood ties
within $10^{-8}$ per observation. Fits very close to the lognormal limit can have a scale outside the numerical representation range.

With $a,p$ held, the score for $k$ has one root. With $a,k$ held, the likelihood in $p$ is strictly concave and has a finite maximum unless every observation equals $a$. With only $a$ held, the fixed-endpoint power limit is compared with the interior profile. This search is numerical.

## R notes

A detected limiting model stops with advice to fit that model. When the fitted
scale cannot be represented, the error reports its logarithm.
A near-lognormal fitted scale below the smallest positive normal double stops
and reports the log scale.

## Decision history

The C++ face helper `trgamma_faces_log` accepts logarithms of observations
and returns the maximised lognormal and power likelihoods and their fitted
parameters. The derivation and numerical search qualifications are in
`dev/research/evaluations/2026-10-29-trgamma-derivation.md` in the source tree.
