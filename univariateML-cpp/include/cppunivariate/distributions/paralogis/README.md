---
id: paralogis
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Paralogistic
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
  density: dparalogis
  sampler: rparalogis
  control:
    shape: Known positive finite coefficient; NULL estimates it.
    shape0: Optional starting value for the numerical proposal; NULL uses an automatic start.
    gradtol: Tolerance for the numerical proposal; the global certificate is attempted independently of this tolerance.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The free shape $\alpha$ tends to infinity while the free scale $\theta$ follows a concentrating path at the common observation.
    detect: All observations are equal and both coefficients are free.
    status: Unbounded
    message: unbounded
log_density: 2*log(shape) + shape*log(x/scale) - log(x) - (shape + 1)*log(1 + (x/scale)^shape)
references:
- Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012). Loss Models, From Data to Decisions, Fourth Edition.
  Wiley.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlparalogis())
  mlparalogis(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: shape
      shape2: shape
      scale: scale
fit:
  method: multistart
  certified: partial
  note: 'Free-shape fits attempt root enclosure, analytic tail exclusion and compact-domain exclusion. A converged proposal survives an unclosed cover unless a feasible likelihood bound refutes it. Held shape uses the monotone Burr scale solve.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance severity with a constrained Burr shape relationship.
summary: Paralogistic provides a model for insurance severity with a constrained Burr shape relationship.
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

The paralogistic distribution is a Burr type XII distribution with its two shapes equal.
It models positive size and loss data. The scale $\theta$ sets the measurement
scale and the shape $\alpha$ links the lower and upper tail behaviour.
The upper-tail exponent is $\alpha^2$ and the
lower-tail power is $\alpha$.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
gives the boundary argument. Both coefficients are estimated unless held.
With both free, constant data permit concentration at their common value and
have unbounded likelihood. A converged numerical proposal remains available with `certified=false` when
the global certificate below does not close.

Holding scale removes that collapse. There are no nondegenerate limit-model
escapes in this conditional fit. Its log-likelihood tends to minus infinity
at both shape endpoints, including for constant data, so a finite maximum
exists. This argument does not prove uniqueness. Holding shape leaves a
strictly monotone log-scale score with a unique root, also for constant data.
The joint proposal uses BFGS in log coordinates after scaling observations by their median. With scale held, safeguarded shape-score iterations propose the fit. Both paths then attempt the global certificate below. With shape held, the fit uses the Burr conditional scale solve.

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

For concentrated log data, the coordinates
$E=a\log(s/u)-\log a$ and $\log a$ keep the finite Gumbel rate resolved
as $a$ grows. The log-moment identity supplies a spread-dependent starting
point if the finite-difference search has lost the sample spread. Complementary
logistic terms and $\log(1+r)-r$ retain the shape curvature in this limit.

## Global certificate

Write $y_i=\log x_i-c$, $a=\alpha$ and
$b=a(c-\log\theta)$, centering at the proposed log scale $c$.
Apart from the data Jacobian, the total log likelihood is

$$L(a,b)=2n\log a+\sum_i(ay_i+b)-(a+1)\sum_i\log(1+e^{ay_i+b}).$$

For $z_i=ay_i+b$, $p_i=\operatorname{sigmoid}(z_i)$ and
$q_i=p_i(1-p_i)$, the free-coordinate derivatives used by the enclosure are

$$L_a=2n/a+\sum y_i-\sum\operatorname{softplus}(z_i)
 -(a+1)\sum y_i p_i,\qquad L_b=n-(a+1)\sum p_i,$$

$$L_{aa}=-2n/a^2-2\sum y_i p_i-(a+1)\sum y_i^2q_i,$$

$$L_{ab}=-\sum p_i-(a+1)\sum y_iq_i,\qquad
L_{bb}=-(a+1)\sum q_i.$$

The joint proposal starts scale at the sample median. Its automatic shape
start is twice the reciprocal sample standard deviation of log data, with
a floor of 1.5 when that standard deviation is below 10; zero standard
deviation uses 1.5. A positive finite `shape0` overrides it. The historical
BFGS convergence rules are retained; a failed BFGS proposal is eligible only
when its finite total free log-coordinate score norm is below 1e-3, with
objective normalization undone. The held-scale proposal uses safeguarded
Newton inside a positive shape-score bracket and requires mean log-shape
score below the smaller of `gradtol` and 1e-10. These are proposal conditions;
the root enclosure below independently controls returned coefficient accuracy.

The algorithm does not assume joint or conditional unimodality. BFGS (joint)
or safeguarded Newton (held scale) supplies a proposal. Up to eight analytic
Newton corrections improve its center. A fixed-point enclosure with an
approximate negative inverse Hessian $R$ verifies that
$x\mapsto x+R\nabla L(x)$ maps a small closed box strictly into itself
and has weighted infinity norm derivative below $0.9$. It therefore contains
one stationary root. Shape radius is $10^{-8}a$; the intercept radius is
$10^{-8}a$ when scale is free. The mapped scale enclosure must have relative
width below $10^{-6}$. A larger verified contraction box, tried at radii
$0.03a$ down to $0.0001a$, removes the winning root's neighborhood.

A feasible center likelihood gives a lower bound $B$. For free scale,
$L\le n\log a$ at the small-shape end. At the large-shape end the
existence bound above gives
$L\le n\log(4^{3/5}a)-(3/5)a\Delta$, decreasing beyond
$5n/(3\Delta)$. Doubling and halving locate finite tail cutoffs below $B$.
At any stationary scale,
$\sum_i\operatorname{sigmoid}(ay_i+b)=n/(a+1)$, hence

$$-\log a-a\max_i y_i\le b\le-\log a-a\min_i y_i.$$

These bounds enclose every possible competing maximizer. With scale held,
$c=\log\theta$ and $b=0$. Then $L\le2n\log a$ at the small end.
If any $y_i\ne0$, choose $d=\max_i|y_i|>0$; for $a\ge1$,
$L\le2n\log a-da$. If all $x_i=\theta$, the exact likelihood is
$2n\log a-n(a+1)\log2$. Both upper tails decrease after a finite cutoff.

The initial shape slabs have endpoint ratio at most two; this avoids losing
a positive lower bound in the padded interval product $a^2$ when the
tail cutoff is very far from the root. On the remaining compact domain
the certificate recursively excludes boxes
whose likelihood upper bound is below $B$, whose free-coordinate score
excludes zero, or whose mean-value fixed-point image has empty intersection
with the box. Intersections can also shrink boxes. Remaining boxes split in
shape (geometrically) or intercept, with the stationary-scale bounds
re-applied. A box contained in the larger contraction neighborhood is
resolved by its unique enclosed root. Existence plus exhaustive exclusion
proves that this root is the global maximizer. General uniqueness for
arbitrary samples remains unproved; a successful certificate establishes
uniqueness of the returned global maximum for that sample.

Except for the relative held-scale margins described below, enclosures use
the Burr certificate's practical floating-point policy:
64 machine epsilons times one plus the operand magnitude per operation,
with an additional term-count and magnitude allowance for sums. The claim
is certification in exact arithmetic conditional on the computed comparisons,
with this practical margin; it is not outward-rounded interval arithmetic.
No long-double precision is required. An unclosed root enclosure or compact
cover leaves the converged proposal with `certified=false`; a strictly better
finite certificate point refutes it. Proposal exhaustion retains `BfgsFailure`.
The compact cover uses at most `128 * min(iterlim, 100000)` boxes. `shape0`
affects only the proposal.

The held-scale likelihood can have competing modes. If the historical
proposal fails certification, a dyadic shape scan proposes additional
positive-to-negative score crossings, refined by bisection. Its feasible
likelihoods select a candidate; the scan does not establish completeness
or globality. The same root enclosure, tail bounds and exhaustive exclusion
must certify the replacement. The scan supplies proposals only; successful global exclusion supplies certification. For `x = exp(-0.001)` and scale
1 the returned shape is 9262.7781345 (log likelihood 8.1267299), rather than
the local mode at 2.89538 (log likelihood -0.570099).

For the nearest double below 1 at scale 1, practical likelihood allowances
can prevent separation of the small-shape mode from the large-shape root.
A converged local proposal is then uncertified; a separated feasible lower
bound still refutes it. The [precision check](../../../../../dev/research/evaluations/2026-11-06-paralogis-modes-data/precision-stop.cpp)
records the tail-bound difficulty.

Sorted centered logarithms are compressed into exact repeated-value groups.
Their sum is cached. Conservative bins of at most 128, 1024 and 8192 groups
replace all their observations by a containing log interval with the exact
multiplicity. These bounds can exclude boxes and bound the mean-value map;
finer groups resolve inconclusive comparisons, with individual groups used
for small unresolved boxes and the root contraction. This changes cost,
not the exclusion logic. The held-scale score and curvature use relative
operation margins (including a subnormal allowance) rather than absolute
unit-sized padding: the latter obscures tiny scores at large shape. Log
rounding and centering carry a margin proportional to the log operands and
their difference. Other interval operations retain the Burr margin. The
claim remains conditional certification with practical floating-point
margins, without outward rounding.

There is no additional density in the Hellinger closure (equivalently,
closure in $L^1$ for densities). As $a\downarrow0$, the uniform log-axis
density bound vanishes, so every compact interval strictly inside
$(0,\infty)$ receives vanishing probability. Any proper weak limit on
$[0,\infty)$ along this boundary must therefore be a point mass at zero.
For $a\uparrow\infty$, dividing the displayed log-data envelope by $a$
bounds the standardized $t$ density by an integrable envelope. Hence
$\log X-(\log\theta-\log(a)/a)$ converges to zero in probability; tight
weak limits on the original real axis are point masses. On compact positive
shape intervals, scale tending to zero gives a weak point mass at zero,
while scale tending to infinity is not tight. Escaping location is not
tight on the log axis. Thus the weak closure on $[0,\infty)$ contains point
masses, including at zero, but no additional nondegenerate absolutely
continuous distribution. Atoms cannot be Hellinger or $L^1$ limits of
Lebesgue densities and do not supply likelihood faces. Constant joint
samples instead have the unbounded concentrating path above; with scale
held their likelihood vanishes at both shape ends. Reciprocation preserves
the Hellinger conclusion, all likelihood comparisons and statuses; its log
Jacobian is $-2\sum_i\log x_i$, and scale is reciprocated.

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
