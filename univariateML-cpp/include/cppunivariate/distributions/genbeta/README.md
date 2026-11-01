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
  advice: 'With scale estimated the likelihood is unbounded: shape2 < 1 makes the
    density singular as scale falls to the sample maximum. Hold scale, e.g. `mlgenbeta(x,
    scale = 1)`. Free-scale fits with other shapes held are not supported.'
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
    when: A free concentration can collapse on constant observations
    detect: Constant samples; the held coefficients determine whether concentration remains free
    status: InvalidData
    message: unbounded
  - {model: lgamma, name: unit gamma on x/scale, when: 'shape1 tends to infinity, shape3 tends to zero, shape1*shape3 stays finite; fit mllgamma(scale / x)', status: UnitGammaBoundary}
  - {model: power, when: 'shape1 tends to zero and shape3 tends to infinity; shape2 may grow exponentially to lower the effective upper endpoint', status: PowerBoundary}
holds:
- held: []
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape2
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
- held:
  - shape1
  - shape2
  - shape3
  status: no-mle
  reason: 'Estimating the threshold is not supported: endpoint singularities or unattained
    limit models prevent an unrestricted MLE (P0, A4).'
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
  method: multistart
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
---

The generalized beta of the first kind models positive observations below a known upper endpoint.

## Details

The actuar coefficients are shape1 = alpha, shape2 = beta, shape3 = tau,
and scale = theta. Then (X/theta)^tau is beta(alpha,beta).
Every shape may be held. Scale is always held. At fixed shape3, constant samples are unbounded when both beta shapes are free; holding either beta shape gives a finite fit. With shape3 free and shape2 free the likelihood can still concentrate on constant samples, even with shape1 held.

For beta = 1 only alpha*tau is identified. If both are free the fitter
returns tau = 1 and records shape3 as fixed, so likelihood degrees of
freedom count the single identifiable product. Away from this ridge the exponential tail of
Y = -log(X/theta) identifies alpha*tau, and its first nonzero exponential
correction identifies tau and beta. Beta (tau = 1), Kumaraswamy
(alpha = 1), and power (beta = 1) are attained subfamilies.

At fixed tau the likelihood is strictly concave in the two beta shapes.
A logarithmic mesh and analytic envelope-score root search compare its
local maxima with gamma and power limits, using a 1e-8*n log-likelihood
tie tolerance. This numerical global search is not a certificate that
an arbitrarily narrow basin cannot be missed. An unresolved search stops.

If alpha increases and tau decreases with alpha*tau = lambda, Y converges
to gamma(beta, rate=lambda). The corresponding actuar log-gamma has
shapelog = beta and ratelog = lambda on theta/X. Its likelihood must be
converted back to X with Jacobian theta/x^2.

If tau increases, alpha*tau stays finite, and log(beta)/tau tends to c>=0,
Y tends to c plus an exponential law. This is a power law with upper
endpoint theta*exp(-c), whose endpoint MLE is the sample maximum.
With beta held the endpoint stays theta. When beta = 1 this law is
attained and is returned, rather than reported as an escape.

For free theta, fix 0<beta<1 and positive alpha,tau. At k tied sample
maxima, letting theta=max(x)+epsilon gives a likelihood proportional to
epsilon^(k*(beta-1)); the other factors approach positive constants.
Thus the unrestricted likelihood diverges for every positive sample.

The gamma-face rate is beta/mean(Y). After profiling this rate the first
nonzero local correction to the gamma-face log likelihood is
n*tau^2*(beta-1)*(mean(Y^2)-mean(Y)^2*(beta+1)/beta)/24.
A positive coefficient rules out a local maximum on that face. A negative
coefficient is only a local condition: the full profile still needs search.
The power-face endpoint is the largest observation; its shape is
1/(log(max(x))-mean(log(x))). No simple necessary and sufficient
sample inequality for the full three-shape interior maximum is claimed.

There are also mixed limits: if alpha,beta tend to zero, tau tends to
infinity, alpha*tau tends to lambda>0, and alpha/beta tends to r>0,
Y has mass r/(1+r) at zero and an exponential continuous part with weight
1/(1+r). Since observations at theta are disallowed, that mass only
reduces their likelihood; its best weight is zero, already included in
the power comparison. The catalogue is a closure analysis, not a proof
that every joint parameter path has been exhausted.

For held beta the gamma face retains shapelog = beta. The package's
mllgamma currently estimates shapelog, so that constrained face has no
matching log-gamma fitter; its stop message states the restriction.
A power face with endpoint theta held can be fitted by
mlgenbeta(x, scale = theta, shape2 = 1); mlpower estimates its endpoint.
