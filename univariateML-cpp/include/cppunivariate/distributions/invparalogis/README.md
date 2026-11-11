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
    gradtol: Tolerance for the reciprocal paralogistic numerical proposal; a successful proposal is uncertified when the global certificate does not close.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
  note: 'invparalogis/README.md, Global certificate; paralogis/certificate.hpp; invparalogis/test.cpp: Free-shape certification uses root enclosure, analytic tail exclusion and exhaustive compact-domain exclusion; an unclosed certificate retains the successful reciprocal search. Held shape uses the monotone Burr scale solve. Reciprocal transformation preserves the certificate and stop details.'
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
    samples. It proves existence; the compact-domain certificate below verifies each returned free-shape fit.
    Reciprocation preserves these existence and uniqueness properties.
---

The inverse paralogistic distribution is the reciprocal of the paralogistic distribution.
It models positive size and loss data. The scale $\theta$ sets the measurement
scale and the shape $\alpha$ links the lower and upper tail behaviour.
The upper-tail exponent is $\alpha$ and the
lower-tail power is $\alpha^2$.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
gives the boundary argument. Both coefficients are estimated unless held.
With both free, constant data permit concentration at their common value and
have unbounded likelihood. A numerical proposal is returned only after the global certificate below succeeds.

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


## Global certificate

All free-shape fits use the
[paralogistic compact-domain certificate](../paralogis/README.md)
on $y_i=1/x_i$. A held inverse scale $\theta$ becomes the held paralogistic
scale $1/\theta$; a free scale is reciprocated after certification.
The Jacobian $-2\sum_i\log x_i$ is independent of the coefficients, so
existence, the enclosed stationary root and every global likelihood
comparison transfer. Candidate scale coordinates and likelihood bounds in
structured stops are transformed in the same way. Held values remain exact.

The parent certifier uses analytic tail bounds, a verified local root box
and exhaustive exclusion of competing maxima, with a budget of `128 * min(iterlim, 100000)` boxes.
It certifies in exact arithmetic conditional on the computed comparisons,
using the documented practical 64-epsilon floating-point margin. Its small
root box bounds relative shape error by $10^{-8}$ and the mapped scale
interval must have relative width below $10^{-6}$. It assumes no global
unimodality. Ambiguous arithmetic and unrepresentable reciprocals stop with
`NumericalFailure`. If reciprocal rounding merges a nonconstant sample,
the fit also stops numerically rather than declaring an unbounded likelihood.
Proof-budget exhaustion stops with `CertificateBudget`;
proposal exhaustion preserves `BfgsFailure`. The bindings transport
structured stop details and never return the unchecked proposal.

There is no additional density in the Hellinger closure. The weak closure
on $[0,\infty)$ also contains point masses, including at zero.
Tight concentration paths at infinite shape yield weak point masses; these
do not supply a Lebesgue-density likelihood face. Constant joint samples
have unbounded likelihood, while held scale removes that collapse, as in
the parent proof. General analytic uniqueness for arbitrary samples remains
open; successful certification establishes the unique global maximum for
the actual sample.

## R notes

Supply `scale` to hold it at a positive finite value, or `shape` to hold the
common shape. The default estimates both. An unbounded constant-sample fit
stops instead of returning a finite approximation.

## Decision history

univariateML 1.x estimated only shape, with scale held at 1.
On 2026-10-30, the fully free constant-sample rejection was moved to the
core as `Unbounded`. Reciprocal delegation preserves this status. This moved
the former adapter decision without changing nonconstant fits.

On 2026-11-06 (TASK-116.6), free-shape proposals gained a compact-domain
certificate. A proposed concavity shortcut was rejected: dividing the
likelihood by $a+1$ does not produce the needed softplus perspective.
The implementation therefore excludes competitors explicitly. See the
[validation report](../../../../../dev/research/evaluations/2026-11-06-paralogis-cert-report.md).

Reciprocal joint and held-scale fits preserve the paralogistic parent's per-fit certification flag together with its physical likelihood and transformed witnesses.
