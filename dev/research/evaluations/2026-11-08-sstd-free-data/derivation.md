# sstd fixed-moment skew certificate and remaining proof obligations

Let Z have the Student law standardized to variance one, with nu>2,
and let m=E|Z|. In the Fernandez–Steel construction, put
v=xi-1/xi and s_u^2=1+(1-m^2)v^2. The standardized residual in
the kernel is z=d s_u+m v, where d=(x-mean)/sd. Use q=xi^2/(1+xi^2).
Multiplying numerator and denominator by sqrt(q(1-q)) yields

H=1-m^2+(4m^2-3)q(1-q), N=d sqrt(H)+m(2q-1),

z/xi=N/q, z*xi=N/(1-q), s_u/(xi+1/xi)=sqrt(H).

Thus the entire skew line is represented by the open unit interval, and
both half-t faces are its endpoints. H is strictly positive: at q=1/2
it equals 1/4, and at either endpoint it equals 1-m^2>0. No artificial
skew cap is used. Cauchy–Schwarz gives m<1.

On each sign branch with denominator D=q or 1-q,

ell_i=log(2 sqrt(H)/sd)+log t_unit(0)
      -(nu+1)/2 log(1+N^2/((nu-2)D^2)),

ell_i'=H'/(2H)-(nu+1)N(N'D-ND')/[D((nu-2)D^2+N^2)].

The join N=0 is continuously differentiable: the kernel score vanishes
there. Evaluating both sign branches on their intersected N ranges
therefore encloses seam-crossing cells. Elementary interval operations
include every value and score on a cell. At a denominator containing
zero, the absolute residual lower bound still gives a likelihood upper
bound; the score enclosure is deliberately infinite if division is unsafe.
Unsupported skew tails have at least one residual bounded away from zero
with D approaching zero, so their likelihood upper bound tends to minus
infinity and they are eventually excluded.

When all endpoint residuals have the supported sign, D tends to one.
The limiting ordinary-t scale is sd*sqrt((nu-2)/nu)/sqrt(1-m^2).
The endpoint is mean plus sd*m/sqrt(1-m^2) at q=0, or minus that
quantity at q=1. These are the actual held-moment constraints, not a fit
with endpoint at the sample extreme. Nonzero residuals justify passage
to the limit term by term. Face witnesses use Ht/ReflectedHt coordinates
(endpoint, nu, ordinary scale).

Contact is different: N=0 and D=0 require the first nonzero derivative
of N, and the shrinking-side contribution does not equal the ordinary
half-t endpoint density. Put r=sqrt(1-m^2). At q=0, exact contact means
d=m/r, and N'(0)=m*(4*m^2-3)/(2*r^2)+2*m=m/(2*r^2)>0.
Hence the shrinking-side residual N/q tends to m/(2*r^2), giving density
2*r/sd * t_unit(m/(2*r^2)); reflection gives the q=1 factor. Thus the
exact contact factor is known. What is not implemented is a safe
near-contact comparison that distinguishes this removable limit from the
zero-density unsupported side and the ordinary supported-side limit when
a rounded residual is extremely close to zero. The implementation detects
contact with a 128-epsilon margin and stops numerically with a finite
witness when representable. It does not claim a half-t maximum in this
case.

The search evaluates an initial finite witness and both supported faces.
A cell is excluded only if its likelihood upper bound is below a known
lower bound, or if its score has a definite sign. In the latter case it
contains no interior maximum; every finite interior maximum is stationary,
and the score is continuous through every join. The endpoint maxima are
already candidates. Repeated bisection either resolves every stationary
component to the declared coordinate width, exhausts the caller's work
budget, or stops on unresolvable arithmetic. Midpoint likelihood plus the
score enclosure times half-width bounds each retained root cell. The
representative of a merged component is its full-span midpoint; the
likelihood bound includes the union of all cells. Comparisons include
all stationary components, even local minima. Strict dominance returns
one maximizer or a face; overlapping competitors return a tie. The q-span
criterion transports to log xi by derivative 1/[2q(1-q)], hence gives a
log-xi span approximately bounded by 1e-10. Density is re-evaluated in
public coordinates before success.

These arguments are exact-arithmetic proofs conditional on computed
bounds and comparisons. Practical padding is 128 binary64 epsilons times
one plus the sum of absolute term bounds, and not outward rounding.
Independent high-precision probes test this arithmetic convention; they
are numerical evidence, distinct from the enclosure argument.

## Nonuniqueness

Reflection of the data about the held mean maps xi to its reciprocal
without changing likelihood. For x=(-1,1), mean=0, sd=1, nu=5 the
certificate leaves the two supported half-t faces as tied global winners.
Reflection proves exact equality of their heights. Their superiority to
the stationary components is conditional on computed enclosure comparisons,
not an independent symbolic uniqueness theorem. Existing sstd evidence
also gives two finite mean maxima at fixed sd=1,nu=3,xi=1 and three
scale-score roots at asymmetric held skew. Neither mean nor scale profiles
can be treated as generally unimodal.

## Open: held asymmetric skew with free nu

With ordinary scales a and b and mode c, xi=sqrt(b/a) is fixed. Holding
mean and/or sd constrains c and the common scale through the t moments.
Even when scale can be eliminated by a monotone score at a fixed mode,
mode is not globally concave for t kernels. When mean is held, changing
scale also moves mode, and the existing three-scale-roots counterexample
precludes monotone root elimination. Necessary work:

1. Enclose every constrained mode/scale stationary component for each
   shape interval, not just sampled or locally refined profiles.
2. Bound the entire nu down-to-two region, retaining sd-held collapse
   paths as well as positive ordinary-scale t2 faces when sd is free.
3. Prove a uniform large-nu remainder relative to the constrained
   skew-normal profile. Pointwise t-to-normal convergence is insufficient
   when mode and scale move with nu.
4. Compare every reachable face and transport a winning root enclosure
   to mean/sd/nu. A scalar nu scan inherits errors and branch switches
   from its inner profiles and supplies none of these guarantees.

Exactly held xi=1 with both moments free continues to use the existing
certified std delegation. The base does not contain the parallel std-free
job's new held-moment certificates, so those cannot be claimed here.

## Open: fixed nu, free skew with free moments

The unstandardized two-half-scale objective is

n log(2 t_nu(0)/(a+b))
 -(nu+1)/2 sum log(1+(x-c)^2/(nu s_side^2)).

There is a useful exact reduction when **both moments are free**. It is
not used by the current numerical fitter and does not certify its output.
For a fixed interior mode c let nL and nR count strictly left/right
observations and let k count observations at c. Write C=n/(nu+1), and

T_L(a)=sum_left (c-x)^2/(nu*a^2+(c-x)^2),
T_R(b)=sum_right (x-c)^2/(nu*b^2+(x-c)^2).

Each T is strictly decreasing from its side count to zero. At a stationary
pair, r=a/(a+b) satisfies T_L(a)=C*r and T_R(b)=C*(1-r).
Define aL(r) and bR(r) as the unique solutions of those two equations.
They exist on

max(0,1-nR/C) < r < min(1,nL/C).

This interval is nonempty exactly when nu*(n-k)>k. The function

aL(r)/r - bR(r)/(1-r)

is strictly decreasing: aL decreases with r while bR increases. Its
left endpoint limit is positive (possibly infinite), and its right
endpoint limit negative. Hence there is exactly one stationary pair.
The inner likelihood tends to zero when either side scale tends to zero
alone, when the sum tends to infinity, and when both scales tend to zero:
writing a=r*s,b=(1-r)*s, the latter power is
s^[nu*(n-k)-k] times bounded ratio factors. It follows that the unique
stationary pair is the unique global side-scale maximizer. These arguments
require both nL,nR>0 and the strict collapse inequality. They are
exact-arithmetic statements, independent of an optimizer or a numerical
experiment.

For fixed r, aL grows as c moves right and bR decreases. Therefore the
root r(c) increases with c, whenever both side inverse problems exist.
On an interior mode interval [l,h], this gives rectangular scale bounds
from aL(r(h),l), aL(r(l),h), bR(r(l),h), bR(r(h),l), with
appropriate one-sided extensions when a side becomes empty. This supplies
a possible basis for an exhaustive one-dimensional mode certificate.

What is still missing is an implemented, metered inversion of those
monotone inner problems with coefficient enclosures, and exhaustive mode
score/likelihood bounds using the enclosures. The t kernel is not
homogeneous, so the explicit GED side-power profiles do not transfer. Its
residual curvature changes sign at |x-c|=sqrt(nu)*s_side; neither mode
concavity nor a single mode maximum follows from the inner-scale lemma.
At an endpoint the smaller scale vanishes. Quantitative one-sided mode
bounds there must accompany the half-t face comparison. For example the
left scale near a left endpoint with a repeated contact behaves at leading
order like (c-xmin)^(2/3), from T_L(a)/a tending to a positive constant;
treating its derivatives as uniformly bounded would be incorrect. This
last asymptotic is a diagnostic calculation, not an implemented uniform
remainder bound.

With either mean or sd held, the moment constraints couple mode and
scales and invalidate the independent a/b stationarity equations above.
Those subsets still need their own constrained stationary enclosures and
half-face comparisons. None of these obligations is supplied by the
current bounded scans. Fixing a computational box in log scales or skew
would leave the tails uncertified.

## Open: all-free and partial free-shape/free-skew combinations

In addition to those inner certificates, uniform shape/skew corner
bounds are needed. Pointwise nu-to-infinity and xi-to-infinity limits
do not bound paths where both move, and nu down-to-two with held sd
allows vanishing ordinary scales. Existing qualitative closure analysis
identifies possible faces in the strict tie regime, but the retained
face profilers are numerical scans. Correct numerical stops remain
possible; their existing finite results and selected limits are explicitly
uncertified. Default estimates are kept unchanged by instruction; they
are not relabeled as globally verified.

## Reproducible numerical checks

The [70-digit checker](check.py) tests the bounds emitted by
[bounds.cpp](bounds.cpp), independently solves the finite skew roots, and
compares the frozen-sample xi-only solution. Its [results](math-checks.json)
include 900 interior likelihood/score checks with zero violations, eight
small-sample roots and one frozen-sample root. These are numerical checks,
not the enclosure proof. [reproduce.sh](reproduce.sh) rebuilds the original
baseline header through read-only Git access, reruns the targeted cases and
benchmarks, and reruns this checker. All 15 unchanged subset routes must
match their original results exactly.

The independent [side-scale checker](check-side-scales.py) solves nine
fixed-mode pairs at 50 digits and checks both side scores, the predicted
mode-to-r ordering, and 225 likelihood comparisons around those pairs.
Its [results](side-scales.json) have zero violations. This is numerical
support for the inner uniqueness lemma, not a certificate of any larger
subset returned by the production fitter.

## Arithmetic stop for large held nu

The q identities are exact for every nu>2. The production certificate stops
numerically for held nu>=128 because the settled sstd_sigma seam still
forms m by subtracting two large log-gamma values. The density normalizer
already changes to its stable asymptotic expression at 128; its skew
moment does not. This is an arithmetic guard, not a shape search cap or a
new parameter restriction, and increasing iterlim does not remove it.

The [C++ probe](moment-probe.cpp) and [80-digit checker](check-moment.py)
retain [measured errors](moment-checks.json). At nu=1e8 the moment error is
about 1.22e-8, at 1e12 about 1.49e-4, and at 1e16 the computed moment is
about 5.64e7 instead of approximately 0.79788456. The 128 threshold is
conservative; it does not assert that every evaluation just above it is
inaccurate. The new route retains a symmetric finite witness when that
witness is representable, and reports unresolved gamma-ratio arithmetic
rather than a skew estimate.

Required repair outside this job's authorized changes: stabilize the skew
moment in special/skewed_densities.hpp through the special-function seam,
using a stable half-step gamma ratio or a bounded asymptotic expression;
then verify the d/p/q/r and fitter consumers at large nu. The existing
fixed-skew scalar routes also consume that moment. No repair to special/,
dpqr.hpp or truth tables is made here, as instructed. Their earlier
large-held-shape arithmetic needs that same follow-up; the new certificate
does not extend its claim to that range.
