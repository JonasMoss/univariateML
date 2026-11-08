---
id: gb2
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Transformed beta (GB2)
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'q'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape2
  symbol: 'a'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape3
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: scale
  symbol: 'b'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dgb2
  sampler: rgb2
  control:
    gradtol: Tolerance on the per-observation profile score in log beta shapes.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: log(shape2)-log(x)-loggamma(shape3)-loggamma(shape1)+loggamma(shape3+shape1)+shape3*shape2*log(x/scale)-(shape3+shape1)*log(1+(x/scale)**shape2)
limits:
  - {firmness: proved, model: weibull, when: 'With shape3 held at one, the Burr submodel tends to its Weibull face.', status: WeibullBoundary, fixed: [shape3], numerical: true}
  - {firmness: proved, model: invweibull, when: 'With shape1 held at one, the inverse Burr submodel tends to its inverse-Weibull face.', status: InvWeibullBoundary, fixed: [shape1], numerical: true}
  - name: unbounded likelihood
    firmness: proved
    when: The log-scale dispersion tends to zero at the common observation while the remaining free coefficients allow concentration.
    detect: All observations are equal, and the held coefficients still permit concentration at that value.
    status: InvalidData
    message: unbounded
  - {firmness: proved, model: trgamma, when: 'The beta shape $q$ tends to infinity with $a,p$ and $b/q^{1/a}$ fixed.', status: TrGammaBoundary, numerical: true}
  - {firmness: proved, model: invtrgamma, when: 'The beta shape $p$ tends to infinity with $a,q$ and $bp^{1/a}$ fixed.', status: InvTrGammaBoundary, numerical: true}
  - {firmness: proved, model: lnorm, when: 'The beta shapes satisfy $p=q=K\to\infty$, the power is $a=\sqrt{2\psi_1(K)}/\sigma$, and $b=e^\mu$, for fixed $\sigma>0$ and $\mu$. Here $\psi_1$ is the trigamma function.', status: LognormalBoundary, numerical: true}
  - {firmness: proved, model: pareto, when: 'The power $a$ tends to infinity with $qa>0$, $p$ and $b$ fixed.', status: ParetoBoundary, numerical: true}
  - {firmness: proved, model: power, when: 'The power $a$ tends to infinity with $pa>0$, $q$ and $b$ fixed.', status: PowerBoundary, numerical: true}
  - {firmness: proved, name: double Pareto, when: 'The power $a$ tends to infinity with $qa>0$, $pa>0$ and $b$ fixed.', status: DoubleParetoBoundary, numerical: true}
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlgb2())
  mlgb2(x)

summary: The generalized beta distribution of the second kind provides a flexible four-parameter model for positive data.
classification: specialist
classification_reason: Used to model income and loss sizes with independently adjustable tails and scale.
aliases:
- "generalized beta of the second kind"
- "GB2"
- "transformed beta"
- "generalized F"
key_references:
- 1
references:
  - "McDonald, J. B. (1984). Some Generalized Functions for the Size Distribution of Income. Econometrica, 52(3). doi:10.2307/1913469."

group: beta and generalised beta
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: fpareto
    at:
      shape1: shape1
      shape2: shape2
      shape3: shape3
      scale: scale
      min: 0
  transform_of:
  - family: betapr
    at:
      shape1: shape3
      shape2: shape1
      scale: scale**shape2
    transform: power
    power: shape2
fit:
  method: multistart
  certified: partial
  note: 'Certified exact held-unit beta-shape reductions and conditional concave problems; with power held, one non-unit beta shape held and scale free, the beta-prime certificate transports finite maxima, transformed-gamma faces and explicit stops. The seven remaining mixed subsets retain numerical outer searches, including the default joint fit; whole-family certification remains partial.'
areas:
- actuarial
- economics and income
existence:
  uniqueness: unknown
  statement: >-
    With all four coefficients free, constant positive samples have unbounded
    likelihood. For nonconstant samples, a supremum may occur on a limiting
    model rather than at finite coefficients. No general existence or
    uniqueness criterion is established. Holding coefficients can remove
    particular escapes. For nonconstant data, holding both beta shapes gives
    a unique finite maximum in power and scale. Holding power and scale
    instead gives a unique finite maximum in the beta shapes. With power,
    scale and either beta shape held, the remaining shape has a unique
    finite maximum even for constant data.
  reason: >-
    On the log-observation scale, put $c=-a\log b$. At fixed $p,q$ the
    objective is $n\log a+p\sum_i(a\log x_i+c)
    -(p+q)\sum_i\log(1+\exp(a\log x_i+c))$ plus a constant.
    Its Hessian is negative definite. For nonconstant data it tends to
    negative infinity as power tends to zero or infinity after profiling
    location, so the maximum is attained. With power and scale held, the
    logistic transform of log observations reduces the shape problem to
    beta fitting. These arguments do not establish uniqueness of the outer
    beta-shape profile. The limiting paths below are proved, but their
    numerical likelihood comparisons do not exhaust all possible maxima.
---

The transformed beta distribution models positive observations with two power tails.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) describes the limiting paths and the scope of the boundary comparisons.

In the GB2 convention $(a,b,p,q)$, $a$ is the power, $b$ is the scale, and $p,q$ control the lower and upper power tails. Their exponents are $ap$ near zero and $aq$ at infinity.
Each coefficient may be held at a positive finite value. The numerical search
compares proper limiting models and stops when a face wins; a search that
cannot resolve an interior maximum reports an error.

The fit standardizes log observations and profiles the power and location
for each pair of beta shapes. The inner objective is strictly concave; the
outer search uses a grid and member-family starts, refines distinct basins
with Newton or BFGS, and expands the search box when needed. This reduces
the outer problem to at most two dimensions and improves conditioning.
A small profile score and an interior position are required for a returned
interior fit. The multibasin search is numerical: neither global optimality
nor exhaustion of all possible limit faces is proved. An unresolved search
reports failure. Available transformed-gamma, inverse transformed-gamma,
lognormal, Pareto, power and double-Pareto faces are compared, as applicable
to the held subset.

## R notes

A detected limiting model stops with advice to fit that model. A small score
checks stationarity of a returned fit; it does not certify global optimality.

## Decision history

For samples larger than 1000, the 2026-10-31 fit-speed revision forms the Burr,
inverse-Burr and beta-prime member seeds from 1000 deterministically selected
observations, one distinct index from each of 1000 contiguous index blocks.
This changes only candidate starts;
samples of size at most 1000 use the original member fits. Transformed-gamma and
reciprocal transformed-gamma boundary fits, all profile likelihoods, basin
refinement, boundary comparisons and score/failure gates still use the complete
sample. This is a numerical search change, not an exact-equivalence theorem for
arbitrary samples. A repeated-golden large-sample comparison and a regression
exercise the new branch; the prescribed small-sample comparison does not
exercise subsampling. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

2026-10-30 (TASK-125): the shared beta log-coordinate bridge uses the continued fraction when the backend input is subnormal or its result is below 1e-290, avoiding relative precision loss before taking logs. The existing independent truth rows measure the selected cases; this is not a uniform error bound. Logit quantile bisection returns NaN if it cannot bracket or reach adjacent floating-point endpoints.

On 2026-10-29 commit `470175d` introduced the profiled search and explicit
face comparisons. The implementation report
`dev/research/evaluations/2026-10-29-gb2-implementation-report.md` records the
derivation, independent numerical comparisons, and the failed guarantee
of global certification. On 2026-10-30 commit `fbe1d44` reduced repeated
work in profile evaluation; the report `dev/research/evaluations/2026-10-29-perf-report.md`
records the performance investigation. The search costs repeated passes
over the data per profile evaluation; the implementation report includes
measured workloads and the slower BFGS-only comparison.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).


The held-scale two-shape reduction now inherits the
[Burr scalar concavity certificate](../burr/README.md#held-scale-certificate-task-1164),
including strict endpoint score comparisons, coefficient accuracy checks,
practical floating-point margins and explicit tie/budget/numerical stops.
The unit-beta-shape reductions and the held-power certificate below do not cover every mixed subset, so the family-wide certification tag stays partial.

Profile and envelope backtracking stop if both proposed coordinates round to
the current coordinates. Such stagnation does not establish convergence:
the existing inner and outer score gates still decide whether that point
can be used. This prevents repeated evaluation of an identical point when
an increased work limit cannot produce a representable step.

An outward search-box edge also completes a proposal, using the same
outward-score condition as the existing completion flag. The final edge
guard still excludes it from an attained fit. This avoids spending an
increased iteration allowance repeatedly pursuing an already detected
search-domain escape.

Shape-box expansion uses the caller’s solver limit. Expansion stops early when a resolved face dominates an explored outward proposal, or when another box would exceed representable positive shapes; the final edge gate still rejects an unresolved edge as a finite fit.

## Held power and one held beta shape

Here the public order is `(q,a,p,b) = (shape1,shape2,shape3,scale)`.
For held power $a>0$, choose a fixed log center $m$ and set
$y_i=\exp\{a(\log x_i-m)\}$ and $s=\exp\{a(\log b-m)\}$.
Then $y$ has beta-prime parameters $(p,q,s)$ and

\[
 \ell_{GB2}(p,q,b)=\ell_{BP}(p,q,s)
 +\sum_i[\log a+(a-1)\log x_i-am].
\]

The Jacobian does not depend on the free shape or scale, and $b\mapsto s$
is a bijection. Thus the [beta-prime conditional certificate](../betapr/README.md)
settles free subsets $\{p,b\}$ and $\{q,b\}$ for every held power,
subject to representable transformations and coefficient accuracy checks.
A gamma face $(k,\theta)$ maps to transformed gamma $(k,a,
\exp(m)\theta^{1/a})$; the inverse-gamma face has the identical scale
map into inverse transformed gamma. The held power and held beta shape
must remain held when fitting the proposed limit.

The parent encloses every surviving shape/scale segment, proves negative
profile curvature on the final root enclosure, and bounds both exteriors.
Its emitted left endpoints and relative segment widths below $10^{-10}$
give a conservative log-scale enclosure here. The transported enclosure must
have relative width at most $10^{-6}$ in $b$; otherwise this route stops
numerically. This extra check is essential for small $a$. The parent shape
criterion is also $10^{-6}$; these are golden-accuracy certificates, not a
claim of arbitrary user-specified coefficient precision.

Certification is in exact arithmetic conditional on the computed comparisons.
The parent uses its stated practical floating-point margins; transport adds
$128\epsilon[1+\sum|J_i|+n(1+|am|)]$ to likelihood bounds and rejects
unrepresentable scales or scale reconstruction error estimates above
$10^{-7}$. Distinct observations that collapse under the transformation stop
numerically. These margins are not outward-rounded interval arithmetic or a
uniform perturbation theorem for arbitrarily ill-conditioned samples.
The parent's evaluation ledger remains $128\min(\mathrm{iterlim},100000)$;
transformation and witness transport add no search loop.

Conditional uniqueness fails: the two separated beta-prime maxima in the
[conditional nonuniqueness example](../../../../../dev/research/evaluations/2026-11-07-betapr-prof-report.md#nonuniqueness)
transport through $x_i=y_i^{1/a}$ for any held positive power. The continuity
bracket proving an exact global tie also transports, with a common Jacobian
added to every competing likelihood. A rounded crossing is only a numerical
tie stop, not proof that those rounded observations are an exact tie.

The remaining free subsets $\{p,a\}$, $\{q,a\}$, $\{p,q,a\}$,
$\{p,q,b\}$, $\{p,a,b\}$, $\{q,a,b\}$ and $\{p,q,a,b\}$
are open outside existing unit-shape reductions. In particular, the
three-free-parameter beta-prime fit cannot certify $\{p,q,b\}$.
The held-power reduction supplies neither uniform outer-shape bounds when
power varies nor effective neighborhoods of intersecting gamma, lognormal
and power-tail corners. Those subsets keep their numerical
searches; their small scores do not establish global maxima.
