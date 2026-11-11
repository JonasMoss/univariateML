# Held-nuisance extensions

The earlier [constrained-cover proof](../2026-11-10-gev-held-data/proof.md)
supplies the likelihood jets, unique held-scale location roots, infinite
Fréchet shape tail and lambda=1 face. The following additions are
exact-arithmetic arguments conditional on computed comparisons. The existing
128/512/4096-epsilon practical margins remain; no outward rounding is claimed.

## Shape-slab coverage

Where the inner root is unique, every maximum is on its graph. Start with the
proved root region on a shape slab. A uniformly positive inner score at a
nuisance midpoint excludes every root below that midpoint; a uniformly negative
score excludes every root above it. When the nuisance Hessian excludes zero,
interval Newton intersects the enclosure with `mid-score(slab,mid)/Hyy`.
Every step encloses all roots on the slab. A failed contraction only triggers
shape subdivision. Fréchet additionally intersects with its shape-dependent
lower log-gap bound on every slab, and with log-gap <= 0. Positive-shape GEV
uses its proved fixed-scale location region. Other GEV slabs retain the
original two-coordinate cover.

The envelope theorem makes the partial shape score the profile score on this
graph. Its uniform sign excludes interior maxima; shape endpoints are retained.
Likelihood bounds, the centered two-coordinate Taylor envelope and inclusion
in the already isolated basin also exclude slabs. No profile unimodality is
assumed. Local existence and coefficient contraction still use the existing
Krawczyk/interval-Newton proof. The outer cover is one-dimensional even though
its function bounds are obtained from two-coordinate jets.

## Negative-shape inner location root

With scale held, put t_i=1+xi*(x_i-mu)/sigma and y_i=t_i^(-1/xi),
using y_i=exp(-(x_i-mu)/sigma) at xi=0. On the smooth support,
direct differentiation gives

    d2 loglik/dmu2 = (1+xi)/sigma^2 * sum((xi-y_i)/t_i^2).

It is strictly negative for -1<xi<=0. For -1<xi<0 the location score
tends to positive infinity at its lower support boundary and to negative
infinity as mu increases without bound, proving existence and uniqueness of
the inner root. At xi=0 the exponential-sum formula gives the unique root.
At xi=-1 the score is the constant -n/sigma, and the optimum is its support
face, rather than an interior root.

This uniqueness does not supply a cheap uniform root enclosure on a whole
shape slab. Near xi=-1 the graph approaches moving support, and smooth
rectangular jets may include infeasible points or zero residuals. The broader
negative-profile trial retained in the exploratory CSVs used more allowance
and lost certifications; production therefore retains the two-coordinate
cover there. A usable support-aware slab enclosure and a stable profile-root
inclusion remain separate numerical tasks. No profile unimodality follows
from concavity in location alone.

## Active reverse-exponential corner

Let xi=-a, p=1/a, c=p-1, with 0<a<=1. For t>=0,
`t^p >= t + c*t*log(t) >= t-c/e`. Terms at zero have their limiting meanings.
Let k count sample maxima. All sums below are over normalized gaps d_i from
the maximum; L and H omit zero gaps and equal sum(log(d_i)) and sum(1/d_i).

With scale held, put d_i=(M-x_i)/sigma and w=(mu+sigma/a-M)/sigma>=0.
Then t_i=a(d_i+w). Subtracting the xi=-1,w=0 likelihood gives the bound

    difference <= c*(S+n/e+L) + c*k*log(w) - a*n*w + c*H*w,

where S=sum(d_i). If c*H<a*n/2, maximize the last three terms over w>=0:

    difference <= c*[S+n/e+L+k*(log(2*c*k/(a*n))-1)].

The bracket tends to minus infinity as c decreases to zero. A strictly negative
computed bracket therefore isolates the xi=-1 support corner on a whole
one-sided shape strip, for every feasible location. At c=0 the face likelihood
strictly decreases in w, so its unique optimum is w=0.

With location held and s0=M-mu>0, an active face has sigma=s0. Put
sigma=a*s0*(1+w), d_i=(M-x_i)/s0 and A=2*n-S. For A>0 and 0<=w<=1,
`log(1+w)>=w/(1+w)` and `log(d+w)<=log(d)+w/d` give

    difference <= c*(n+n/e+L) + c*k*log(w) - A*w/2 + c*H*w.

If c*H<A/4, maximize in w to obtain

    difference <= c*[n+n/e+L+k*(log(4*c*k/A)-1)].

Again a negative bracket isolates the corner. The rectangle used by the code
has sigma no greater than 2*a_min*s0; support supplies sigma>=a*s0 at every
feasible point. Consequently every feasible point in it has 0<=w<=1. Infeasible points carry no likelihood. The face optimum itself
is unique there. The rest of the scale domain remains in the ordinary cover.
Both constructions halve c within the solver budget; failure to establish a
strict comparison preserves the uncertified search. The isolated face has
exact shape and its explicit nuisance coefficient; no jet through t=0 is used.

## Held-location compactness when 2j<=n

Here j counts observations different from the held location.

Suppose observations occur on both sides of held mu. Write b=mu-min(x)>0,
h=max(x)-mu>0 and C=b*h/(b+h). For xi>=0, at min(x) the inequality
`-log(1-xi*b/sigma)/xi >= b/sigma` holds. For xi<0, support at the
maximum implies abs(xi)<sigma/h; `log(1+q)>=q/(1+q)` then gives
`-L >= C/sigma`. Thus the same lower bound holds uniformly on [-1,1].
For u=-L>=4, `2*u-exp(u)<=-u*u/4` (differentiate, starting at 4).
All other standardized densities are at most one. Consequently

    loglik <= -n*log(sigma)-C*C/(4*sigma*sigma).

Writing t=C/sigma, the right side is `n*log(t/C)-t*t/4`, decreasing for
`t>=sqrt(2*n)` and tending to minus infinity. Budgeted doubling finds a
strict lower-scale cutoff at the incumbent level. This closes the compactness
premise for straddling data irrespective of tie counts. It does not prove
uniqueness of the inner scale root. One-sided majority/equality cases still
require the existing collapse analysis; the generic cutoff does not claim
to resolve those cases.

## Numerical fallback

A successful certificate sets the per-fit flag. On an incomplete all-free
certificate, the existing bounded scalar profile search is run with its own
convergence test. Its inner fits must succeed and an interior proposal must
pass the total shape-score gate abs(score)<1e-3. A finite endpoint is retained
only as a converged closed-endpoint search result. A strictly superior feasible
certificate witness prevents this fallback. No finite Fréchet result is made
from the xi=0 seam. Public support, likelihood and coefficient-representation
checks still apply after the fallback is mapped back. These checks establish
numerical acceptance, not a global maximum or coefficient certificate.

For positive-shape held-scale GEV the root region is tightened further.
At the minimum, let `t_min=(k/(2n))^xi`; then its negative location-score
term is `2n/t_min`, while all positive terms together are at most
`(1+xi)n/t_min`. Hence the root satisfies
`mu<=min(x)+sigma*(1-exp(-xi*log(2n/k)))/xi`. The latter displacement decreases
with xi, so a slab uses its lower shape endpoint; the zero limit is the
previous sigma*log(2n/k) bound. This also keeps thin slabs inside support.
When a slab midpoint score straddles zero and the initial nuisance curvature
changes sign, quarter-point signs can still contract the root bracket.
Without this step, shape subdivision alone can stagnate at a root crossing
that midpoint.

## A unique inner scale root

For held mu below every observation and xi>=0, write z=(x-mu)/sigma>0,
t=1+xi*z and y=exp(-L)<=1. The derivative of the log-scale score is

    -z/t^2 * [(1+xi)+y*(z-1)].

The bracket is positive: for z>=1 this is immediate; for 0<z<1 it is
at least xi+z>0. Thus log likelihood is strictly concave in log scale.
The score tends to a positive value as scale decreases (to infinity at xi=0)
and to -n as scale increases. The root is unique. This supplies a possible scalar reduction on the nonnegative
shape branch, but the production held-location route retains its two-coordinate
cover: the tested profile enclosure spent more of the default allowance.
For mixed signs the displayed derivative is
not uniformly negative; neither uniqueness nor concavity is assumed there.

For -1<xi<0 the held-scale location likelihood is strictly concave (the
existing fixed-shape proof). Its root lies between
`max(min(x)+shift, max(x)+sigma/xi)` and `max(x)+shift`, with
`shift=-sigma*expm1(-xi*log1p(xi))/xi`. Interval evaluation of shift and the
monotone support floor gives slab root bounds. A tested profile variant using
these bounds exhausted the default allowance on previously certified cases;
production therefore retains the two-coordinate cover on negative shape.
Uniqueness does not make the interval root graph narrow near moving support.
No smooth jet is extended across that face. Slabs touching xi=-1 use the
separate corner/value mechanism.

The analytic two-point GEV result also preserves the ordinary centered case
`(-9,1,...,1)` when its relative-location inclusion rejects a zero or near-zero location. Normal scale,
finite likelihood, a normal user tolerance and agreement with the proved face
value are still required; the result is explicitly uncertified. Subnormal,
unrepresentable and excessively tight-tolerance cases retain their precision
stops. The count formula's fully enclosed results now set the per-fit flag true.

If an observation lies below held mu but none lies above, compactness follows
without the straddling premise. At that observation let t=(mu-x)/sigma.
The shape integral shows `-L>=log(1+t)` for every feasible xi in [-1,1], so
`y>=1+t`. For t>=8, `2*log(y)-y<=-t/2`. The total likelihood is consequently
at most `n*log(t/(mu-x))-t/2`, decreasing for t>=2*n. Budgeted doubling again
finds a strict lower-scale cutoff. In exact arithmetic this closes the `2j<=n` compactness gap: if no observation
lies below mu, either mu<min(x), giving j=n, or mu=min(x). In the latter
case 2j<=n means a minimum multiplicity at least n/2, already resolved by
the earlier collapse/equality classifiers. Rounded normalization that loses
a nonzero difference can still leave a successful fit uncertified.

A strict witness rejection of the all-free fallback is repeated after public
coordinate restoration, using fresh public likelihoods and 4096-epsilon
operand margins. If that ordering is lost, the successful search is retained
uncertified and still undergoes the usual support/representation checks.
Fréchet preserves the typed counterexample status while mapping witnesses.

The Fréchet two-point reduction similarly retains its explicit finite member
uncertified when only the relative-location guard is defeated by cancellation,
provided location measured against scale, normal scale/gap and fresh likelihood
checks pass. A Gumbel comparison unresolved at arithmetic stagnation also
retains that finite member under these checks. Exhausted scalar Gumbel solves,
proved Gumbel winners, exact analytic nonexistence and public representation
failures keep their stops. This does not turn a partly solved Gumbel root into
a finite shape estimate.

Computed slab root cutoffs are widened by 128 machine epsilons times one plus
the cutoff magnitude (and the held scale for GEV). The positive GEV initial
location upper bound receives the same margin. These are practical margins,
consistent with the jet contract, rather than directed rounding.

A value-only scalar scan can miss a finite stationary maximum very close to
the Gumbel seam, or choose the seam when their values are unordered at machine
precision. The existing certificate proposer has already supplied finite
Newton witnesses in these cases. If the scalar scan supplies no acceptable
finite result, the fallback rechecks these witnesses with a converged inner
solve, the total shape-score gate and negative numerical profile curvature.
The curvature check excludes stationary minima; it is not a global concavity
certificate. It retains an acceptable finite witness
uncertified. A seam value is never converted into a capped finite Fréchet fit.
The deterministic near-seam construction in `seam-fallback.cpp` exercises this
case; public representation and likelihood checks still apply after mapping.

The held-location corner rectangle extends down to the compact domain's
lower scale bound. This does not extend the value proof to negative w:
every feasible point has w>=0 automatically; added points below its moving
support floor are infeasible. It avoids leaving machine-width boundary boxes
for a smooth cover to classify. The observed stalled box for sample 60 sits
at this lower support interface.

A rounded normalized search point can fail the strict support predicate even
though its explicit reverse-exponential member is known. For that member the
certificate uses `-log(s0)-sum(d)/n`, with compensated sums, as its analytic
face value; a 512-epsilon downward margin supplies the compactness witness.
Such a point bypasses the generic support guard only after the one-sided
corner proof succeeds. Before marking the returned point certified, its
fresh normalized likelihood must agree with the analytic face value within
`1e-7*n`; original-coordinate support and representation checks still follow.
Failure preserves the original successful search unchanged.

Certified active-corner coefficients are restored directly from the original
sample maximum and the hold (`sigma=M-mu` or `mu=M-sigma`). This avoids
subtracting independently rounded normalized quantities. The existing public
support repair and likelihood checks apply to this restored member.

## Negative-shape lower-tail value bound

For held scale and a shape slab in [-1,0], write a=-xi. At the minimum
observation with mu>min(x), put q=(mu-min(x))/sigma and u=-L. Then

    u=log(1+a*q)/a >= q/(1+a*q),

including u=q at a=0. The right side increases in q and decreases in a,
so the lower location and lower shape endpoints give a uniform lower bound
u0 for the entire box. The standardized log density is
`(1-a)*u-exp(u) <= u-exp(u) <= -u*u/4` for u>=4. The last inequality follows
by differentiation from u=4. Every other standardized log density is at most
zero: its maximum at fixed c=1+xi in [0,1] is c*log(c)-c<=0, with the
continuous value zero at c=0. Thus the average likelihood satisfies

    mean_loglik <= -log(sigma)-u0*u0/(4*n),  u0>=4.

This is a value bound independent of the likelihood jets. It remains usable
when exponentials or their derivatives overflow in boxes touching xi=0.
The computed u0 receives a downward 128-epsilon margin and is capped below
sqrt(DBL_MAX)/2 solely to prevent squaring overflow; decreasing u0 weakens
the bound. The resulting value upper bound receives the existing upward
512-epsilon margin. No search coefficient is capped. The box's second
coordinate here is normalized location.

## Combined smooth basin and local accuracy

The smooth basin is a rectangle containing the included stationary root. The
four strips checked by `holds` cover the proposed enlarged rectangle minus
the already verified rectangle, with overlaps only on edges. A negative
definite Hessian on every strip subdivision makes the objective strictly
concave on the entire rectangle. At an attained shape edge, the corresponding
uniform outward shape score reduces the maximization to that edge, where
negative nuisance curvature and the enclosed nuisance root give uniqueness.
Every smooth evaluation requires the entire piece to be within support.
Pieces crossing a moving support boundary fail the smooth test.

A separately proved one-sided support-face basin is never enlarged with this
criterion. It has no smooth root or Hessian at the corner. Its strip is
excluded only by the original one-sided value inequality. The associated
smooth-basin radii and edge flags have outer scope and are initialized, but
are not used by the face branch. Thus the merged enlargement cannot turn a
support-corner value proof into an unchecked Hessian exclusion.

Profiling encloses every unique inner nuisance root on each shape slab.
Discarding a slab enclosure contained in the verified basin is valid because
any maximizer outside the basin would require another inner root. Slabs in
the basin's shape range are initially retained with the whole nuisance range;
no stationary root is excluded merely because its shape lies in that range.
The negative-shape GEV and held-location regions retain their box covers.

Local coefficient accuracy is a separate condition from this global proof.
The numerical incumbent uses analytic second-order jets and a Newton solve
with positive definite observed information. Coordinates are scaled location,
log scale and shape (log relative shape for a Fréchet image), or log shape and
log location gap for held-scale Fréchet. The core polisher checks the full,
undamped inverse-information score correction with a factor-16 accuracy
margin, and requires two small corrections or quadratic contraction to the
rounding floor. A failed refinement remains a numerical accuracy failure.
Closed reverse-exponential support corners use their analytic conditional
coefficients, without differentiating through zero support residuals.

For a small positive all-free GEV shape mapped to Fréchet, relative accuracy
of Fréchet shape requires relative accuracy in the small GEV shape, not just
an absolute shape tolerance. The final refinement evaluates the original
binary64 observations, before any data normalization can round their score.
Two-component arithmetic with FMA product residuals protects the small shape
score; an analytic series supplies the ratio and its shape derivative. Range
reduction and a convergent exponential series avoid losing that score to
cancellation in an elementary function call. The Hessian still comes from
analytic jets. The local coordinate correction, positive information and
contraction tests remain mandatory, including in double-only builds.
A compensated log-ratio in the fitter's Fréchet likelihood avoids subtracting
two nearly equal large logarithms; the public d/p/q/r code is unchanged.

Independent roots computed at 70-digit working precision use exact rational conversions of the input
doubles, direct score formulas, and mpmath root finding. They do not reuse
production jets, profile searches, interval arithmetic or the two-component
implementation. The resulting root comparisons are numerical evidence for
local accuracy, not proofs of global coverage.

For held-scale Fréchet, the attained shape-one edge uses the initial
outward-score basin and the profile directly. Piecewise enlargement is
reserved for its interior roots. On the unchanged 40-case Fréchet grid,
13 edge fits need 3,544 evaluations without growth and 9,781 with it; the
27 interior fits need 37,593 without growth and 31,326 with it. All
variants certify all 40 cases. The selected policy therefore requires
34,870 evaluations, versus 41,107 for growth everywhere and 41,137 for
no growth. These counts include every cover evaluation and are independent
of workstation load. The counting copies of the headers live only in
scratch directories; production headers contain no counters or logging.

At `xi=-1`, only an analytically coupled active support corner bypasses
smooth derivatives. If the reverse-exponential support is inactive, the
conditional scale is refined with the smooth edge objective and the lower
outward shape score is checked. This distinction is covered by a regression
that perturbs the inactive-face scale by 1e-8 before refinement.


The held-scale local polisher evaluates its log ratio as
`L=u+log1p(d/exp(u))`. Forming `log(d+exp(u))` loses small observation
gaps when the normalized location gap is close to one. On 80 deterministic
quantiles with generating shape 1e9, the latter formulation returned a
double-only shape estimate with relative error 1.968e-9 despite a small
computed Newton correction. The stable identity reduces the error to
1.608e-15 in both native and double-only arithmetic. Four large-shape
private-refinement and four public-fit checks in each arithmetic mode are
compared with independent roots at 70-digit working precision. These are
local-accuracy checks, separate from global inclusion of the large-shape
root. The public JavaScript regression uses the frozen exact input doubles.

For `d>exp(u)`, the same local log ratio uses
`log(d)+log1p(exp(u)/d)`; both branches keep their quotient at most one.
This preserves the small-gap identity while avoiding an overflowing
`d/exp(u)` for a finite extreme observation, as tested on `{0,1e308}`.
The formulas agree analytically at the switch, including both derivatives.

Public restoration is also checked in dimensionless coordinates against the
refined normalized coefficients. The core location and scale projection
checks use extended differences and logarithmic ratios, so a physical-unit
rounding error cannot vanish merely because its absolute bound underflows.
On a three-observation subnormal sample whose reverse-exponential scale is
not representable to the requested relative tolerance, both all-free and
held-location GEV now return numerical failure. The corresponding held-scale
Fréchet location also fails the projection check. These are representation
failures, independent of certificate completion; ordinary study outcomes are
unchanged. The probe and its before/after records are retained as
`combination-projection.cpp` and `combination-projection-{before,after}.csv`.
