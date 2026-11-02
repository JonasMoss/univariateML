---
id: genbeta
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; scale held; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Generalized beta of the first kind
support: {type: real, lower: "0", upper: "scale", closed: [false, true], endpoint_log_density: {upper: "if (shape2 < 1) Inf else if (shape2 > 1) -Inf else log(shape3) - log(scale) - lbeta(shape1, shape2)"}}
params:
- name: shape1
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: 'q'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape3
  symbol: 'a'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 'b'
  role: upper endpoint
  domain: (0, inf)
  simulate: 1
  advice: 'With scale and all shapes free, the likelihood is unbounded because shape2 can be below one at the upper endpoint. Supply scale, for example mlgenbeta(x, scale = 1). Estimating scale with shapes held is not supported.'
  kind: threshold
r:
  density: dgenbeta
  sampler: rgenbeta
  control:
    scale: Known positive finite upper endpoint, strictly above every observation.
    gradtol: Tolerance on the per-observation log-shape3 profile score.
    iterlim: Maximum Newton iterations per beta-shape profile evaluation.
log_density: log(shape3)-log(x)-loggamma(shape1)-loggamma(shape2)+loggamma(shape1+shape2)+shape1*shape3*log(x/scale)+(shape2-1)*log(1-(x/scale)**shape3)
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The free shapes concentrate the distribution at the common observation while $b$ remains held.
    detect: All observations are equal, $q$ is free, and at least one of $p$ and $a$ is free.
    status: InvalidData
    message: unbounded
  - {firmness: proved, model: lgamma, name: log-gamma on scale / x, when: 'The shape $p$ tends to infinity and the power $a$ tends to zero with $pa\to\lambda>0$ and $q,b$ fixed. Then $-\log(X/b)$ tends to a gamma law with shape $q$ and rate $\lambda$.', status: UnitGammaBoundary, numerical: true}
  - {firmness: proved, model: power, when: 'The power $a$ tends to infinity with $pa\to\lambda>0$, while $q$ stays bounded away from zero and $\log(q)/a\to c\ge0$. The limiting power law has upper endpoint $be^{-c}$.', status: PowerBoundary, numerical: true}
holds:
- held: []
  status: no-mle
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape1]
  status: no-mle
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape2]
  status: not-implemented
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape1, shape2]
  status: not-implemented
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape3]
  status: no-mle
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape1, shape3]
  status: no-mle
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape2, shape3]
  status: not-implemented
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
- held: [shape1, shape2, shape3]
  status: not-implemented
  reason: 'Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.'
fit_args:
  scale: 1
examples: |
  set.seed(1)
  x <- rml(200, mlgenbeta())
  mlgenbeta(x, scale = 1L)

group: beta and generalised beta
construction:
- base
- generalisation
relations:
  transform_of:
  - family: nsbeta
    at:
      shape1: shape1
      shape2: shape2
      min: 0
      max: scale**shape3
    transform: power
    power: shape3
fit:
  method: profile-search
  certified: partial
  note: 'genbeta/README.md, Details; genbeta/mle.hpp (public fitter and held overloads); genbeta/test.cpp: With
    endpoint and power held, beta-shape fitting is strictly concave; the free-power mesh and envelope-score search
    do not certify every maximum.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model bounded loss sizes with a power transform and two beta shapes.
summary: Generalized beta of the first kind provides a model for bounded loss sizes with a power transform and two
  beta shapes.
aliases:
- "GB1"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, the likelihood is unbounded for every
    positive sample. With the upper endpoint $b$ held above the data,
    constant samples can still have unbounded likelihood when concentration
    remains free. For nonconstant samples the gamma and power limits may
    prevent attainment. No general uniqueness theorem is established for
    the three-shape fit. With $b$ and $a$ held, the two beta shapes have a
    unique finite MLE for nonconstant data; holding either beta shape as
    well gives a unique finite MLE even for constant data. When $q=1$,
    only the product $pa$ is identified if both factors are free.
  reason: >-
    For $0<q<1$, moving $b$ down to the sample maximum makes its density
    diverge there. At held $a,b$, transforming to $(x/b)^a$ gives a beta
    likelihood, strictly concave in its shapes with an interior sufficient
    statistic for nonconstant samples. A held beta shape leaves a strictly
    decreasing score with opposite signs at its endpoints. At $q=1$ the
    density depends on $p,a$ only through their product.
---

The generalized beta of the first kind models positive observations below a supplied upper endpoint.

## Details

The endpoint is $b$, the power is $a$, and $(X/b)^a$ has beta shapes $p,q$.
Each shape may be held. The endpoint is held strictly above every observation.
At fixed $a$, constant samples have unbounded likelihood when both beta shapes
are free. Holding either beta shape gives a finite fit. With $a,q$ free,
concentration remains possible on constant samples even with $p$ held.

For $q=1$, only $pa$ is identified if both factors are free. Away from this
ridge, the exponential tail of $Y=-\log(X/b)$ identifies $pa$, and its first
nonzero exponential correction identifies $a$ and $q$. Beta ($a=1$),
Kumaraswamy ($p=1$), and power ($q=1$) are attained subfamilies.

At fixed $a$, the likelihood is strictly concave in the beta shapes. A
logarithmic mesh and an analytic envelope-score root search compare local
maxima with gamma and power limits, using a log-likelihood tie tolerance
$10^{-8}n$. The numerical search cannot certify that an arbitrarily narrow
basin has not been missed. An unresolved search stops.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
describes the limiting paths and boundary comparisons. The transformed density is
$a e^{-pay}(1-e^{-ay})^{q-1}/B(p,q)$ for $y>0$.
If $pa\to\lambda>0$, $a\to0$ and $q>0$ is fixed, the relation
$\Gamma(p+q)/\Gamma(p)\sim p^q$ gives a gamma limit for $Y$, with
shape $q$ and rate $\lambda$. Thus $b/X$ has a log-gamma limit. Its density
must be transformed back to $X$ with Jacobian $b/x^2$.

If instead $a\to\infty$, $pa\to\lambda>0$, $q$ is bounded away from
zero and $\log(q)/a\to c\ge0$, then $1/B(p,q)\sim p q^p$.
The factor $(1-e^{-ay})^{q-1}$ tends to zero for $y<c$ and one for $y>c$.
The limiting density is $\lambda e^{-\lambda(y-c)}\mathbf1_{y>c}$.
Scheffé's lemma gives convergence in total variation. Transforming back gives
a power law with endpoint $be^{-c}$. Its fitted endpoint is the sample
maximum and its fitted shape is
$1/(\log(\max_i x_i)-\overline{\log x})$ for nonconstant samples.
With $q$ held, the limiting endpoint remains $b$. At $q=1$, that power
law is attained and is returned as a member rather than an escape.

For free $b$, fix $0<q<1$ and positive $p,a$. At $k$ tied sample maxima,
letting $b=\max_i x_i+\varepsilon$ gives likelihood proportional to
$\varepsilon^{k(q-1)}$. The other factors approach positive constants.
The unrestricted likelihood therefore diverges for every positive sample.
This argument does not exclude MLEs with held shapes. For example, holding
$p=q=a=1$ gives a uniform law whose upper endpoint MLE is the sample
maximum under the declared endpoint closure. Such endpoint fits are not implemented.

The gamma-limit rate is $q/\bar Y$. After profiling that rate, the first
nonzero local log-likelihood correction is
$n a^2(q-1)\{\overline{Y^2}-\bar Y^2(q+1)/q\}/24$.
A positive coefficient rules out a local maximum at that limit. A negative
coefficient is only a local condition; the full profile still needs search.
No simple necessary and sufficient sample inequality for a finite
three-shape maximum is claimed.

Mixed limits also occur. If $p,q\to0$, $a\to\infty$,
$pa\to\lambda>0$ and $p/q\to r>0$, then $Y$ has mass $r/(1+r)$
at zero and an exponential continuous part of weight $1/(1+r)$.
For the fitted samples, which exclude observations at $b$, this atom only
reduces the likelihood. Its best weight is zero, already covered by the
power comparison. This is a closure analysis, not a proof that every joint
parameter path has been exhausted.

## R notes

Supply scale; scale = NULL stops with the endpoint explanation. The former
actuar notation is alpha = shape1, beta = shape2, tau = shape3, theta = scale.
The log-gamma limit on scale / x has shapelog = shape2 and ratelog equal to
the limiting product shape1 * shape3. With shape2 held, the limit retains
that shapelog value. The mllgamma fitter currently estimates shapelog, so
there is no matching constrained log-gamma fit; the error states that restriction.
A power limit with endpoint held can be fitted by
mlgenbeta(x, scale = scale, shape2 = 1); mlpower estimates its endpoint.

When shape2 = 1 and shape1 and shape3 are free, the fitter returns shape3 = 1
and records it as fixed, so likelihood degrees of freedom count only the
identified product.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
