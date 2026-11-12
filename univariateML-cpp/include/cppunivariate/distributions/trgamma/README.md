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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: log(shape2) + shape1*shape2*log(x/scale) - (x/scale)**shape2 - log(x) - loggamma(shape1)
limits:
  - name: fixed-endpoint power face
    firmness: proved
    fixed: [scale]
    when: The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held.
    status: IterationFailure
    message: fixed-endpoint power face
    numerical: true
  - name: unbounded likelihood
    firmness: proved
    when: The free coefficients concentrate the distribution at the common positive observation.
    detect: All observations are equal and the free coefficients allow concentration. With scale free, either free shape suffices. With scale held, a free power suffices at the common observation $a$; with both shapes free, concentration is also possible above $a$.
    status: InvalidData
    message: unbounded
  - model: lnorm
    firmness: proved
    when: 'The gamma shape $k$ tends to infinity with $p\sqrt{k}\to1/\sigma$ and $\log a + \log(k)/p\to\mu$, for fixed $\sigma>0$ and $\mu$.'
    status: LognormalBoundary
    numerical: true
  - model: power
    firmness: proved
    when: 'The power $p$ tends to infinity with $kp\to d>0$ and $a$ converging to a positive endpoint.'
    status: PowerBoundary
    numerical: true
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
  method: profile-search
  certified: 'yes'
  note: 'trgamma/mle.hpp: exact scalar reductions; convex-conjugate secant bounds, uniform refitted-normal seam,
    analytic power tails and certified concave root neighborhoods for the free and held-scale profiles.
    Reciprocal fits transport coefficients and all likelihood bounds. Numerical, comparison and work-budget stops retain no successful estimate.'
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
    With only scale held, global profile bounds compare the interior with the
    fixed-endpoint power limit; constant samples above scale additionally
    have unbounded likelihood when both shapes are free.
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

The lognormal limit is an interior member of the Prentice generalized gamma
family, at $Q=0$; this transformed-gamma family contains only its $Q>0$ side.

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
asymptotic boundary probability is one half. A positive finite improvement within $10^{-8}$ per observation of a face
stops as an unresolved comparison, retaining both witnesses and a global upper
bound. A face is selected only within the floating-point comparison margin;
this does not assert exact finite-sample equality. Fits very close to the lognormal limit can have a scale outside the numerical representation range.

With $a,p$ held, the score for $k$ has one root. With $a,k$ held, the likelihood in $p$ is strictly concave and has a finite maximum unless every observation equals $a$. With only $a$ held, the fixed-endpoint power limit is compared with the interior profile. The convex conjugate of log-gamma bounds the profile between evaluated powers, and analytic bounds close both tails.

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

The free transformed-gamma certificate shares its segment allowance across all initial grid cells. Exhaustion retains the best represented finite profile and the computed lognormal or power face; a depth cutoff is not used as a substitute for excluding an unresolved interval.

An inner gamma-shape proposal is accepted only when its represented shape has relative equation residual at most `1e-12`; exhaustion without that check is a numerical failure. Unsuccessful optimizer proposals are discarded; a failed certificate evaluation cannot be replaced by a face comparison. This is a numerical residual gate, not a proof that floating-point profile bounds are outward rounded.

The certificate is valid in exact arithmetic conditional on the computed
comparisons. Its practical per-observation floating-point comparison margin
is $64\epsilon(1+|\Delta|)$ for finite free-profile comparisons (with
$|\Delta_{\rm face}|$ added when comparing a face), and
$2\times10^{-12}(1+|F(1)|)$ at held scale, where $F(1)$ is the initial normalized profile value; it does not use outward-rounded
interval arithmetic. A successful interior fit excludes all competing cells
and isolates a root inside a neighborhood proved strictly concave by
subdivision. A likelihood tolerance alone never accepts coefficients.
The signed quadratic normal-seam bound includes zero third log moment.

With the endpoint held, each observation exactly at that endpoint contributes
$-1$ to the power-face log likelihood. The fixed-endpoint comparison therefore
subtracts the endpoint multiplicity; a free endpoint can move to remove this
factor. At held scale, constant samples on the concentrating side of the
endpoint have unbounded likelihood when both shapes are free, even when
the observation differs from the held endpoint.

The proof, coefficient checks, rejected objective-only acceptance and
reproducible validation are recorded in
[the certificate report](../../../../../dev/research/evaluations/2026-11-07-trgamma-seam-report.md).

With only scale held, every nonconstant positive sample has a finite
maximizer; constant samples on the nonconcentrating side do as well.
The fixed-endpoint profile approaches its limiting likelihood from above,
with leading excess $\gamma_E/(-p\,\overline{\log(x/a)})$ per observation
for transformed gamma, after reversing the logarithms for its reciprocal.
An interior/endpoint comparison within the likelihood tolerance stops as
an unresolved comparison, rather than asserting non-existence of the MLE.

Near the normal seam the profile score is evaluated through exponential and
Stirling remainders, avoiding cancellation of two terms of order $1/t$.
The root neighborhood shrinks with standardized power while retaining a
floor based on the practical score margin; a fixed relative neighborhood
would require increasingly many curvature cells as $t$ tends to zero.

A small log-data spread can turn a moderate standardized power into a huge
physical power. Rounding the reconstructed scale can then lose likelihood
even though the profile root is well resolved. Returned coefficients and
finite diagnostic witnesses are evaluated at their represented values;
reconstruction loss beyond $10^{-10}(1+|\ell|)$ gives a numerical stop.

Logarithmic collisions between distinct raw observations, or between an
observation and a held endpoint, stop numerically rather than inventing
multiplicities or endpoint factors. Free power witnesses keep the raw
sample maximum; reciprocal Pareto witnesses keep the raw minimum, including
when the corresponding reciprocal endpoint overflows.
