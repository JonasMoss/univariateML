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
    gradtol: "Requested relative coefficient accuracy for all-free fits (default 1e-10); checked by the final observed-information Newton correction."
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
  note: 'One free beta shape with free power and held scale uses a global profile certificate in either orientation. Held beta shapes use a strict-concavity coefficient enclosure. Held unit shapes and held-power conditional shape/scale fits preserve the delegate certificate or its successful uncertified search. Five larger mixed subsets retain numerical searches; whole-family certification remains partial.'
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

The final joint correction propagates log beta-shape and standardized power/location
errors into the public scale. Near a gamma face, centering the log location by
log(q) removes the leading gamma-limit score before differentiation. When both
beta shapes are large, centering the logit kernel at their mean removes the
normalizer/entropy cancellation before taking derivatives. With power
and scale held, a beta mean/precision solve preserves shape information in
concentrated samples. These accuracy refinements leave the separate held-subset
certificate routes intact. See the
[accuracy validation](../../../../../dev/research/evaluations/2026-11-10-acc-allfree-report.md).

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) describes the limiting paths and the scope of the boundary comparisons.

In the GB2 convention $(a,b,p,q)$, $a$ is the power, $b$ is the scale, and $p,q$ control the lower and upper power tails. Their exponents are $ap$ near zero and $aq$ at infinity.
Each coefficient may be held at a positive finite value. The numerical search
compares proper limiting models and stops when a face wins; a search that
cannot resolve an interior maximum reports an error.

Outside the certificates below, the fit standardizes log observations and profiles the power and location
for each pair of beta shapes. The inner objective is strictly concave; the
outer search uses a grid and member-family starts, refines distinct basins
with Newton or BFGS, and expands the search box when needed. This reduces
the outer problem to at most two dimensions and improves conditioning.
Profile scores nominate candidates; the returned numerical interior fit
also passes the joint coefficient-error check or the beta reduction above. The multibasin search is numerical: neither global optimality
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

The remaining free subsets $\{p,q,a\}$, $\{p,q,b\}$, $\{p,a,b\}$,
$\{q,a,b\}$ and $\{p,q,a,b\}$ are open outside existing unit-shape
reductions. The held-scale $\{p,a\}$ and $\{q,a\}$ certificates are described below. In particular, the
three-free-parameter beta-prime fit cannot certify $\{p,q,b\}$.
The held-power reduction supplies neither uniform outer-shape bounds when
power varies nor effective neighborhoods of intersecting gamma, lognormal
and power-tail corners. Those subsets keep their numerical
searches; their small scores do not establish global maxima.

## Held beta shapes: power/scale certificate

For held positive $p,q$, write $z_i=(\log x_i-m)/s$, $A=as>0$ and
$c=a(\log b-m)$. Apart from the data Jacobian, the log likelihood is
$n\log A-n\log B(p,q)-\sum_i[p\,\operatorname{softplus}(-t_i)+q\,\operatorname{softplus}(t_i)]$,
where $t_i=Az_i-c$. Its negative Hessian is
$\operatorname{diag}(n/A^2,0)+(p+q)\sum_i w(t_i)(z_i,-1)(z_i,-1)^T$,
with $w(t)=e^{-|t|}/(1+e^{-|t|})^2$. It is positive definite.
Holding scale restricts this function to $c=A(\log b-m)/s$; holding
power restricts it to a vertical line. Thus any enclosed stationary point
is the unique global maximizer on its supported affine domain.

`concave_certificate.hpp` bounds each weight on a Euclidean ball of radius
$r$ around the Newton candidate using
$|t_i|+r(|z_i|+1)$ (the held-scale line instead uses $z_i+(m-\log b)/s$).
The resulting positive matrix bounds negative curvature throughout that ball;
its determinant divided by its trace bounds its least eigenvalue $M$.
If the score norm, including its arithmetic margin, is less than $Mr/2$,
the likelihood decreases in every outward direction on the ball boundary.
It therefore has an interior stationary point there; strict concavity proves
globality without any exterior search or assumption about other profiles.
The radius bounds relative power error and log-scale error under
$a=A/s$, $\log b=m+sc/A$ by $\min(\mathrm{tol},10^{-9})/2$.
The likelihood upper bound follows from the supporting tangent at the center
and the enclosed maximum.

This is an exact-arithmetic certificate conditional on the computed
comparisons. Practical margins are $8\epsilon$ times an absolute score
scale, a $128\epsilon(1+n)$ curvature discount, and $128\epsilon$
likelihood and coefficient-transport allowances. These are not directed
rounding bounds. Free-scale floating-point spacing must also be below one quarter of the target relative accuracy. Ill-conditioned arithmetic or stagnation stops numerically;
exhausting the solver allowance stops numerically with its finite witness.
Newton and line search each use at most $\min(\mathrm{iterlim},100000)$
iterations. Distinct observations collapsed by logarithms stop numerically instead of acquiring a constant-data classification. For constant data with exactly one free power/scale coefficient, the same certificate uses unit log scale. Jointly free power/scale retains its analytic unbounded stop. Exact unit-shape delegates precede this certificate.

## Subunit held shape: endpoint domination

If $0<q<1$ is held, $a,p$ are free, scale $b$ is held and every
observation lies at or below $b$, with at least one strictly below it, the global supremum is the power
law on $(0,b)$ with exponent $r=-n/\sum_i\log(x_i/b)$. Indeed
$B(p,q)>\int_0^1 t^{p-1}\,dt=1/p$, so for $r=ap$ the ratio of
each GB2 density to the power density is
$\{pB(p,q)\}^{-1}\{1+(x/b)^a\}^{-(p+q)}<1$.
For a tied observation the density ratio is bounded by $2^{-q}$. Taking
$a\to\infty$, $p=r/a$ attains these bounds in the limit. Maximizing the power likelihood over $r$ therefore proves
the face selection, without a finite search or uniqueness assumption about
the mixed profile. If $Z$ observations equal $b$, the reported supremum is
the power log likelihood minus $Zq\log2$, using the same pointwise endpoint
convention as [Burr](../burr/README.md). A refitted power density has the
unpenalized endpoint value. The endpoint remains held. Samples crossing $b$
are excluded.
Reciprocation gives the Pareto face with held endpoint $b$ when $0<p<1$
is held and $a,q$ are free and every observation is at least $b$, with one
strictly above it; its tie penalty is $Zp\log2$.

The face certificate uses compensated sums and checks that the relative
rate uncertainty from $128\epsilon\sum_i(1+|\log x_i|+|\log b|)$ is below
$\min(\mathrm{tol},10^{-9})$. Unresolved log differences stop numerically.
Power witnesses follow public order `(alpha = b, beta = r)`; Pareto
witnesses follow `(a = r, b = b)`. These pointwise bounds cover the one-sided data regimes; the global profile certificate below handles the other samples.

For constant observations below held scale and held $q>1$, with $p,a$
free, a finite maximum exists. Put $u=a|\log(x/b)|$. The inner $p$
maximum is unique. Its power profile tends to minus infinity as $u$ tends
to zero and approaches the power supremum from above as $u$ tends to
infinity: its first correction is $n\{\gamma+\psi(q)\}/u>0$,
where $\gamma$ is Euler’s constant and $\psi$ is the digamma function.
The beta expansion at $p=0$ and the inner score give $p=1/u+O(u^{-2})$.
Continuity and these exterior limits force a finite global maximum. Its coefficients are enclosed by the global profile certificate below,
instead of classifying these constant data as unbounded. Reciprocation
gives the corresponding result above held scale when $p>1$ is held.

The curvature ball includes log-coordinate uncertainty: each normalized
observation has allowance $8\epsilon(1+|m|+s|z_i|+|\log b|)/s$.
Multiplication by $A$ and a $16\epsilon$ arithmetic allowance give an
argument error $\delta t_i$. The score margin uses the logistic bound
$|\delta u_i|\le\min(1,\delta t_i/4)$; likelihood bounds add
$(p+q)\sum_i\delta t_i$ and the log-Jacobian error. Weight bounds include
these argument errors, and the least-curvature bound subtracts the
spectral perturbation from uncertainty in $(z_i,-1)$. Thus a large power
cannot turn unresolved input logarithms into a coefficient certificate.

## Held-scale mixed-power global certificate

`shape_power_certificate.hpp` covers free $(p,a)$ with $q,b$ held; swapping
the beta shapes and reversing log ratios covers free $(q,a)$ with $p,b$ held.
Put $w_i=\log(x_i/b)$ and $S_\pm(a)=\sum_i\operatorname{softplus}(\pm aw_i)$.
For each $a>0$, the strictly decreasing score
$n\{\psi(p+q)-\psi(p)\}-S_-(a)$ has exactly one positive root.
It is enclosed by signed scores, using
$p\{\psi_1(p)-\psi_1(p+q)\}\ge q/(p+q)$ and the first trigamma-series
term as lower derivative bounds in log shape. Inner enclosures remain
bounds when their refinement allowance ends; no unchecked inner estimate
is accepted.

Write $H(p)=\log\{pB(p,q)\}$. For $q\le1$, $H(p)\ge0$. For $q>1$,
$H$ is convex with $H(0)=0$, $H'(0)=-C$, where
$C=\psi(q)-\psi(1)>0$; consequently $H(p)\ge-Cp$. Let
$L=\sum_{w_i<0}|w_i|$, $R=\sum_{w_i>0}w_i$, and let $Z$ count endpoint ties.
On a one-sided lower sample ($R=0<L$), $nC-Z\log2\le0$ proves endpoint
domination even for a held shape above one. A signed practical margin is
required to select that face. If this coefficient is positive, the profile
approaches the penalized power supremum from above, so a finite point can
beat it. With $r=ap$, the uniform upper bound
$n\log r-r\{L-(nC-Z\log2)/a\}-\sum\log x_i-Zq\log2$
then closes the large-power exterior once a feasible lower witness beats
the face.

For crossing samples ($L,R>0$), the same inequality gives, for
$a\ge2n\max(C,0)/L$, the upper bound
$n\log(2n/L)-n-qaR-\sum\log x_i$, which tends to minus infinity.
For an upper one-sided sample,
$B(p,q)\ge\Gamma(q)/(p+q)^q$ follows by integrating
$\psi(t)<\log t$. Maximizing over $p+q$ and bounding
$\log(1+e^{-aw_{\min}})\ge e^{-aw_{\min}}/2$ gives
$n\log a-qaD+K$, where $D=\sum_i(w_i-w_{\min})>0$ for nonconstant data
and $K=nq\{\log(2nq)-1\}-n\log\Gamma(q)-\sum\log x_i$.
Constant upper-side data instead have unbounded likelihood: choose
$p=q e^{aw}$ and let $a$ grow. The endpoint case also concentrates.

At small power, $S_-(a)/n\ge\log2/2$ whenever
$a\sum|w_i|\le n\log2/2$. Maximizing the strictly concave shape
function $-\log B(p,q)-p\log2/2$ gives a finite constant $M$, hence the
upper bound $n\log a+nM-\sum\log x_i$. These exterior bounds produce
a compact power interval whose complement is strictly below a feasible
witness; they do not use profile unimodality.

Inside it, convexity bounds $S_-$ on each power segment and encloses every
inner shape root. Both diagonal entries of the joint $(a,p)$ Hessian are
negative. Its cross entry $\sum_i w_i/(1+e^{aw_i})$ decreases with $a$;
its endpoint bounds therefore give a supporting-plane likelihood upper
bound with remainder $K_{\rm cross}|\Delta a\Delta p|$. The power score
decreases in $a$ at fixed $p$ and is affine in $p$, so signed corner bounds
exclude root-free segments. Likelihood bounds discard dominated stationary
regions. Every remaining global maximum lies in the retained coefficient
envelope; success requires relative spans below $\min(\mathrm{tol},10^{-9})/8$
in both free coefficients. Separated unresolved maxima stop as a tie;
precision/range failures and exhausted work stop without an estimate.

This is an exact-arithmetic certificate conditional on the computed
comparisons. Practical allowances include $8\epsilon$ log-input errors,
$16\epsilon$ transform/reduction errors, $128\epsilon$ special-function,
normalizer and enclosure-transport margins, and subnormal floors. Input
uncertainty enters the likelihood and both score bounds. It is not
outward-rounded arithmetic. Every profile point/segment consumes the
shared $128\min(\mathrm{iterlim},100000)$ ledger; each scalar bracketing
or refinement pass uses at most $\min(\mathrm{iterlim},100000)$ steps.
Large intermediate shapes outside the working range stop numerically.
The five larger mixed subsets still require their own global certificates.

A cell contracts by interval Newton only when an upper bound on the
profile Schur complement $L_{aa}-L_{ap}^2/L_{pp}$ is strictly negative
throughout the cell. Logistic weight bounds enclose $L_{aa}$; the trigamma
series bounds $-L_{pp}$; the decreasing cross score bounds $L_{ap}$.
A lower derivative bound is supplied by $L_{aa}$ itself. Midpoint score
intervals then contract every possible stationary point in that cell;
likelihood and inner-shape bounds are recomputed on the contracted interval.
Cells without a verified sign retain the general global subdivision.

Held-power beta-prime transport applies coefficient enclosures only to certified parents. A rounded crossing between separated stationary profiles does not prove exact equality. Successful uncertified beta-prime and unit-first-beta-shape Burr results retain their flag; nonfinite transformed estimates or likelihoods still fail.

### Probability coordinates at large power

A nonunit scale makes subtraction of log(x) and log(scale) lose the displacement amplified by the power. The positive log-ratio helper retains the exact near-scale subtraction. For equal beta shapes, central inversion uses the original probability offset and the bounded local inverse of the symmetric log-odds density.

The concentrating symmetric tail uses t=shape2*log(x/scale), with a=shape1=shape3. Rounding its two beta coordinates can erase a resolved displacement. This probability bridge returns NaN for sqrt(a)*E > 2.5e-11 and 0 < |t| < 2E/1e-10, where E is eight binary64 machine epsilons. At t=0 the exact symmetric half-probability remains available; the stated margin is practical floating-point evidence.

For shape1 = 2, the beta-coordinate CDF is u^shape3 (1 + shape3(1-u)). Its negative logarithm is the sum of two positive logarithmic deviances. Retaining that hazard in log coordinates avoids both a cancelled complement and a backend beta normalizer whose relative error grows at tiny shapes in double-width builds. The one-shape coordinate uses its exact power CDF.

### Unequal concentrating beta shapes

The density normalizer is centered at the beta mean and combined with two nonnegative Bernoulli deviances. In the unit-power submodel, a compensated product expansion retains shape2 times the physical excess minus shape1 times scale before either ratio rounds; a rounded beta coordinate can lose the entire standardized displacement. For other powers, the log-odds kernel returns NaN near concentration when its practical coordinate-error margin exceeds 1e-12 times the log-density scale. This named precision region requires more accurate transcendental coordinates to close.

If the ordinary log-odds margin fails, two-word logarithms recompute the centered coordinate before rounding. The beta mean is subtracted in the same arithmetic. NaN remains confined to cases where this practical two-word margin also fails; the ordinary margin selects the recomputation rather than a refusal.

In the fixed-power beta-prime delegate, the public scale is the parent scale
raised to the reciprocal power. The local scale correction therefore
divides the parent log-scale correction by the held power. This native
coefficient bound and the final exponential map are checked separately from
the parent shape equations.

The powered observations are rounded before the beta-prime delegate sees
them. A final score and information check uses the original log observations;
when its correction is appreciable, it refines that transformed point before
returning it. The check uses the same rare-shape rate coordinate as the parent
so the transformation check does not restore the cancelled gamma terms.
