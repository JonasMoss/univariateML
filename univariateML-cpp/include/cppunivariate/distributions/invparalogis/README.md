---
id: invparalogis
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Inverse paralogistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dinvparalogis
  sampler: rinvparalogis
  control:
    shape: Known positive finite coefficient; NULL estimates it.
    shape0: Optional positive finite starting value; NULL uses the automatic start.
    gradtol: Gradient tolerance for the underlying paralogistic fit on median-normalized reciprocals; with fixed scale, tolerance on the mean log-shape score (at most 1e-10).
    iterlim: Maximum number of iterations.
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The free shape $\alpha$ tends to infinity while the free scale $\theta$ follows a concentrating path at the common observation.
    detect: All observations are equal and both coefficients are free.
    status: Unbounded
    message: unbounded
log_density: 2*log(shape) + shape*log(scale/x) - log(x) - (shape+1)*log(1+(scale/x)**shape)
references:
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlinvparalogis())
  mlinvparalogis(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: invburr
    at:
      shape1: shape
      shape2: shape
      scale: scale
  transform_of:
  - family: paralogis
    at:
      shape: shape
      scale: 1/scale
    transform: inverse
fit:
  method: delegation
  certified: partial
  note: 'invparalogis/README.md, Details; invparalogis/mle.hpp (public fitter and held overloads); invparalogis/test.cpp:
    Reciprocal paralogistic delegation preserves its held-shape monotone scale solve; the joint BFGS fit has no
    global certificate.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance severity using a constrained inverse Burr family.
summary: Inverse paralogistic provides a model for insurance severity using a constrained inverse Burr family.
aliases: []
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, constant positive samples have unbounded
    likelihood. A finite maximum exists for nonconstant samples, but joint uniqueness
    is not established.
    Holding scale gives a finite shape maximum for every positive sample,
    including constant data. Uniqueness of this conditional shape maximum is
    not established here. Holding shape gives a unique finite scale maximum
    for every positive sample. Holding both leaves likelihood evaluation.
  reason: >-
    With shape $\alpha$ held, the score for log scale is strictly monotone
    and changes sign. With scale held, the log-likelihood tends to
    $-\infty$ as $\alpha$ tends to zero or infinity, including for constant
    samples. For the joint problem, the log-data density bound given below forces
    the profiled likelihood to $-\infty$ at both shape ends for nonconstant
    samples. It proves existence, but does not certify the numerical optimizer.
    Reciprocation preserves these existence and uniqueness properties.
---

The inverse paralogistic distribution is the reciprocal of the paralogistic distribution.
It models positive size and loss data. The scale $\theta$ sets the measurement
scale and the shape $\alpha$ links the lower and upper tail behaviour.
The upper-tail exponent is $lpha$ and the
lower-tail power is $lpha^2$.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
gives the boundary argument. Both coefficients are estimated unless held.
With both free, constant data permit concentration at their common value and
have unbounded likelihood. The joint numerical fit has no global certificate.

Holding scale removes that collapse. There are no nondegenerate limit-model
escapes in this conditional fit. Its log-likelihood tends to minus infinity
at both shape endpoints, including for constant data, so a finite maximum
exists. This argument does not prove uniqueness. Holding shape leaves a
strictly monotone log-scale score with a unique root, also for constant data.
The fit uses the paralogistic fitter on reciprocal observations and maps a held scale to its reciprocal. The Jacobian depends only on the data, so maximizers and failure conditions transfer.

For a direct joint existence argument, consider the paralogistic log-data
$y_i=\log x_i$, put $a=\alpha$, $v=\log\theta$ and
$t=a(y-v)+\log a$. Its density on the log axis is
$g_a(t)=a e^t/(1+e^t/a)^{a+1}$.
For $a\ge1$, its log derivative in $t$ is at most $-3/5$ when
$t\ge\log4$, while for $t\le0$ its density is at most $a e^t$.
Its maximum is at most $a$, so everywhere
$g_a(t)\le4^{3/5}a e^{-3|t|/5}$.
Writing $\Delta=\max_i y_i-\min_i y_i>0$ gives a joint log-likelihood
bound $n\log(4^{3/5}a)-(3/5)a\Delta$, which tends to minus infinity
as $a\to\infty$, uniformly in location. As $a\to0$, the maximum density
$a\{a/(a+1)\}^{a+1}$ tends to zero. On compact positive shape intervals,
escaping location also makes the likelihood tend to zero. A finite maximum
therefore exists. For constant $y_i=y_0$, choosing
$v=y_0+\log(a)/a$ gives density asymptotic to $a/e$, proving unboundedness.
Reciprocation transports the argument to the inverse distribution.

## R notes

Supply `scale` to hold it at a positive finite value, or `shape` to hold the
common shape. The default estimates both. An unbounded constant-sample fit
stops instead of returning a finite approximation.

## Decision history

univariateML 1.x estimated only shape, with scale held at 1.
On 2026-10-30, the fully free constant-sample rejection was moved to the
core as `Unbounded`. Reciprocal delegation preserves this status. This moved
the former adapter decision without changing nonconstant fits.
