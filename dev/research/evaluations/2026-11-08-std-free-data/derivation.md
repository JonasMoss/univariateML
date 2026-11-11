# Constrained Student shape certificates

Write v for shape, m for held location, s for held physical scale, and
q_i=((x_i-m)/s)^2. For lst, s is ordinary scale and r=v; for std, s is
standard deviation and r=v-2. Apart from -n log(s),

    L(v)=n[c(v)+log(v/r)/2] -(v+1) sum log(1+q_i/r)/2,
    c(v)=log Gamma((v+1)/2)-log Gamma(v/2)-log(v*pi)/2.

These are actual-hold formulas. In particular, std cannot call an ordinary
held-scale t certificate with the same numeric scale as v changes.

## Scalar scores and finite-domain coverage

Put R(u)=log(1+u)-u/(1+u). Its derivative is u/(1+u)^2, so it is
nonnegative and increasing on nonnegative u. The existing integrated,
alternating gamma-ratio remainders give c, c', -c'' for v>=1/2.
For 0<v<1/2 the exact step-two recurrence extends those bounds:

    c(v)=c(v+2)-log(1+1/v)+log(1+2/v)/2,
    c'(v)=c'(v+2)+1/[v(v+1)(v+2)],
    -c''(v)=-c''(v+2)+(3v^2+6v+2)/[v^2(v+1)^2(v+2)^2].

For ordinary held scale the score and curvature are

    L'=n c' -sum R(q/v)/2 +sum q/[2v^2(1+q/v)],
    L''=n c'' +sum q(q-2-q/v)/[2v^3(1+q/v)^2].

For held standard deviation, r=v-2, they are

    L'=n[c'-1/(vr)] -sum R(q/r)/2 +3 sum q/[2r^2(1+q/r)],
    L''=n[c''+(1/v+1/r)/(vr)]
          +sum q(q-6-3q/r)/[2r^3(1+q/r)^2].

These combined formulas avoid subtracting terms of order 1/v at the normal
seam to obtain a score of order 1/v^2.

Every surviving finite shape interval is covered. A strict score sign
excludes an interior maximum; strictly positive curvature excludes one as
well. Strict negative curvature and positive/negative endpoint scores
isolate exactly one maximum. A lower bound on -L'' turns an enclosed score
at the midpoint into a root error bound. Root enclosures must be narrower
than min(tol,1e-7) times shape (the solve targets min(tol,1e-10)); a
stalled or wide enclosure refuses. Covered isolation regions avoid counting a subdivision seam twice.
Full likelihood intervals compare every isolated maximum and eligible
face; overlapping values stop as a tie. No number of nominated grid peaks
establishes completeness. The initial finite grid supplies lower witnesses
only. Its termination is not a shape bound.

## Lower tail: held location and ordinary scale

For v>0,

    Gamma((v+1)/2)/Gamma((v+2)/2) <= sqrt(pi).

The ratio decreases: its logarithmic derivative is the difference of two
increasing digammas at arguments separated by one half, and its limit at
zero is sqrt(pi). Thus exp(c(v)) <= sqrt(v)/2 and, for every observation,

    f_v(x;m,s) <= sqrt(v)/(2s),
    L(v) <= n log(v)/2 -n log(2s).

A finite witness determines an explicit positive lower cutoff. This bound
also covers observations equal to m; held positive scale prevents collapse.
The excluded v=0 face is minus infinity, not a proper fitted member.

## Lower tail: held mean and standard deviation

Let w=v-2 and let k observations equal m. For 0<w<=min(1,min_nonzero q),
c(v)<=-log(2*pi)/2, v<=3, and log(1+q/w)>=log(q/w)>=0. Therefore

    L(2+w) <= C+(n-3k/2) log(w),
    C=-n log(s)-n log(2*pi)/2+n log(3)/2
       -3 sum_nonzero log(q)/2.

If k<2(n-k), this tends to minus infinity uniformly on the whole lower
shape interval and a witness determines its cutoff. If k>2(n-k), the
fixed-sd path is unbounded; the public fitter detects this before searching.

At critical equality k=2(n-k), write

    L(2+w)=B(w)+(n-k)w log(w)/2,
    B(w)=n c(2+w)-n log(s)+n log(2+w)/2
          -(3+w) sum_nonzero log(q+w)/2.

Then B(0)=L0=-n log(2s)-3 sum_nonzero log(q)/2. Since c'(v)<=1/(4v^2),

    B'(w) <= D=5n/16-sum_nonzero log(q)/2.

Consequently L(2+w)-L0 <= w[D+(n-k)log(w)/2]. An explicit small w makes
this strictly negative on the entire punctured lower interval. This
certifies that face locally and permits a global comparison to every
finite candidate and the normal supremum. The face is not attained in std.
For (0,0,1), m=0,s=1, its value is -3 log(2). This is a fixed-sd collapse
path, not the positive ordinary-scale t2 face returned by all-free std.

All cutoffs are checked after binary64 rounding. A rounded cutoff that
cannot establish the asserted inequality gives a numerical stop.

## Infinite normal tail

The normal endpoint keeps the supplied m and s in both parameterizations:

    LN=-n log(2*pi)/2-n log(s)-sum q/2.

With h=1/v in lst, log(1+u)>=u-u^2/2 gives the unsigned bound

    L(v)-LN <= sum(q^2) (h+h^2)/4.

With w=1/(v-2) in std the corresponding weaker bound is

    L(v)-LN <= n w+sum(q^2)(w+3w^2)/4.

A finite witness strictly above LN excludes the entire large-shape tail
when the bound is strictly below its gain. This never selects normal.

To select normal, use a signed bound on the inverse-shape derivative.
The gamma integral implies

    1/(4v^2)-1/(8v^4) <= c'(v) <= 1/(4v^2).

One can derive this directly from u-u^3/3 <= tanh(u) <= u integrated
against exp(-vt)/2; equivalently use the alternating gamma bounds.
Also, for all t>=0,

    t^2/2-2t^3/3 <= R(t) <= t^2/2,

by integrating u/(1+u)^2 and using 1-2u <= (1+u)^-2 <=1.
For lst and every 0<h<=H,

    dL/dh <= A+H sum(q^2)/2+n H^2/8,
    A=sum(q^2-2q-1)/4.

For std and every 0<w<=W,

    dL/dw <= n[1-1/(4(1+2W)^2)+W^2/8]
                +sum[-3q/(2(1+qW))+q^2/4].

If this is negative the entire punctured tail is below LN. Geometric
expansion seeks a cutoff; failure to prove either signed or unsigned
exclusion refuses numerically. The 1e12 shape ceiling is a practical
arithmetic-domain refusal, never a fitted cap. Every expansion, subdivision
and root refinement has the standard iterlim work cap.

## Arithmetic and evidence

The scalar engine reuses t_cert_detail's practical binary64 enclosures,
64 epsilon operation margins and binary-carry sum margins. Below 1/2 it
adds the enclosed recurrence above; no special-function seam changes.
A wide interval whose dependency bounds cross a forbidden denominator is
subdivided, not accepted or used as proof of failure of the statistical
model. A point or irreducible arithmetic failure stops. The package claim
is certified in exact arithmetic conditional on these computed comparisons
and practical floating-point margins; it is not outward rounding.

The 80-digit oracle independently differentiates the density on the exact
binary64 frozen inputs. It checks coefficients, scores and likelihoods,
not globality. Branch coverage and face inequalities supply globality.
The frozen default golden fits and files are unchanged. The conditional
shape-only std root differs from the frozen approximate all-free shape by
about 1.03e-6 relatively, within the existing golden comparison tolerance;
this is expected after holding its approximate nuisance coefficients.

## Nonuniqueness below shape one

Take x=(-100,-99,0,99,100), v=1/2. Every atom has mass 1/5< v/(v+1)=1/3.
The likelihood tends uniformly to zero at scale zero (using the strict
collapse exponents), and at scale infinity. Location can be restricted to
the sample range because moving toward that range improves every factor.
Thus a finite global maximum exists.

At location zero put a=v*s^2, B=10000, C=9801. The strictly decreasing scale
score has the unique root

    5a^2+2(B+C)a-BC=0.

The positive algebraic solution determines the best possible centered
likelihood. A rational witness m=99.5,s=0.5 beats that value by an enclosed
amount between .44 and .45. The appended C++ test evaluates this difference
with the practical interval operations; the gamma constant cancels.
Therefore no global maximum has m=0. Reflection preserves likelihood,
so every global maximum has a distinct reflected maximum. This disproves
general global uniqueness under the strict atom condition at held shape
below one. The argument uses existence and a strict comparison, not an
assumption that a multistart run found every root.

## Open constrained problems and uncertified numerical fallback

With location held and ordinary scale free, the monotone scale score gives
one conditional scale for each shape. If every residual is nonzero,
min|d| <= s_hat(v) <= sqrt((1+1/v) mean(d^2)). The small-shape Wendel bound
from the previous held-subset study closes the lower v=0 tail in lst.
For std, location held and free sd is a different constraint: ordinary
scale is free and the lower v=2 face can have a positive fitted scale.
The scalar fixed-sd collapse bound above must not be used on it.
Missing in the fitter integration: constrained joint scale/shape root inclusions and uniform
neighborhood/profile envelopes, a signed normal-tail bound with scale
refitted about the held location, and relative positive-scale t2 endpoint
exclusion/selection. A converged admissible legacy profile remains an Ok result with
certified=false; it does not establish global optimality. Failed searches stop.

With ordinary scale held and location free, all stationary locations are
in the sample range but can exchange order as shape changes. The existing
uniform decreasing-shape theorem still certifies its Cauchy regime and
calls exhaustive location isolation (including tie stops). Outside that
regime, missing: joint location/shape boxes covering branch switches and
root neighborhoods, actual-hold normal-tail control. In std the ordinary
scale changes with shape; the lower-v=2 bound must cover locations moving toward every sample atom.
The final section below supplies that uniform bound, including critical
multiplicity, but it is not integrated with joint root coverage. A pointwise
legacy comparison does not supply this certificate. These profiles likewise retain converged admissible numerical fits with
certified=false; failed searches still stop.

For joint location/scale with fixed subunit shape, strict atoms give
existence but not the geodesic convexity used above one; the counterexample
shows why a single included EM root is insufficient. Missing: exhaustive
nuisance-root enumeration and every collapse-face comparison, including
critical equality. The existing numerical stop is retained. No missing
piece here is an outward-rounding requirement, an invitation to enlarge
the old grid, or a claimed proof that the sample has no MLE.

### Exact rational version of the nonuniqueness comparison

The strict off-center comparison also has a proof independent of the
floating-point interval margins. For the positive centered scale root a,
P(a)=5a^2+39602a-98010000 satisfies P(1979)=-55437 and P(1981)=63367;
P is strictly increasing for positive a. Thus 1979<a<1981.
Let U_i=1+2(2x_i-199)^2, positive integers. If R is the likelihood ratio
of the rational off-center witness to the best centered fit, cancellation
of the common gamma normalizer and raising to the fourth power give

    R^4 = 8^10 prod_i(a+x_i^2)^3 / [a^5 prod_i U_i^3]
        > 8^10 prod_i(1979+x_i^2)^3 / [1981^5 prod_i U_i^3] > 5.

The final inequality is exact integer arithmetic, reproduced by oracle.py
with Fraction. In particular R>1 without evaluating a logarithm. The
80-digit log-likelihood difference and the C++ practical enclosure are
independent numerical checks of this algebraic nonuniqueness proof.

## Nonuniqueness with held standard deviation and free shape

For std take x=(-100,100), sd=1, and estimate both mean and shape v>2.
For any centered symmetric unimodal density with variance one,

    1 >= 2 integral_0^b x^2 f(x) dx >= 2 f(b) b^3/3.

Thus every centered Student member has sample likelihood at most
9/(4b^6). The centered normal limit has the same bound. The off-center
point mean=b, sd=1, v=3 has likelihood

    4/[pi^2 (1+4b^2)^2] > 1/[4(1+4b^2)^2],

using pi<4. For b=100, the exact integer inequality

    b^6=1000000000000 > 9(1+4b^2)^2=14400720009

proves that this witness beats every centered finite member and the best
normal-limit fit. oracle.py reproduces the integer comparison.

Every optimum's location can be restricted to [-b,b] by monotonicity of
each density away from its center. The v-down-to-two collapse exponent
is strictly positive at both distinct observations, so likelihood tends
uniformly to zero on that location interval. This follows also from the
lower-face bound with at most one residual inside a fixed neighborhood
of either atom. The normal limit is uniform on the compact location range
and lies strictly below the displayed witness. Hence some finite global
maximum is attained. No maximum is centered, and reflection gives at
least two distinct maximizing (mean,shape) vectors. This is a free-shape
held-sd counterexample, not merely a fixed-shape location tie.

## Explicit bounds for the two-dimensional constrained profiles

These are analytical bounds for the actual holds. They sharpen the open
entries above: the remaining implementation needs root/branch coverage
and signed normal-face comparisons, rather than an assumed compact search
box or an arbitrary shape cap. They are not used to bless legacy estimates.

### Held location, free ordinary scale and shape

Put d_i=x_i-m, D2=mean(d_i^2), and let k residuals vanish. The scale score is

    G(v,s)=(v+1) sum d_i^2/(v*s^2+d_i^2)-n.

It decreases strictly in s when any residual is nonzero. For a positive
scale root and a positive lower bound v>=v0, write dmin for the smallest
nonzero |d_i|. Directly bounding every nonzero summand below and every
summand above gives

    s_hat(v)^2 >= dmin^2 * [(n-k)/n-k/(n*v0)],
    s_hat(v)^2 <= (1+1/v0)*D2.

The lower bound is useful when its bracket is strictly positive. If k=0,
it simplifies to s_hat>=dmin for every v>0. More generally one may replace
k by j and dmin by a positive residual radius exceeded by at least n-j
observations. This can produce a substantially stronger bound.

For lst, a zero residual makes the free-scale/free-shape problem unbounded:
choose 0<v<k/(n-k), then collapse s to zero. With no zero residual, each
observation's density is maximized over its own scale at s=|d_i|. Log
convexity of Gamma gives Gamma((v+1)/2)/Gamma(v/2)<=sqrt(v/2), and

    (v/(v+1))^((v+1)/2) <= sqrt(v).

Consequently, uniformly over a common scale,

    sup_s L(v,m,s) <= n*log(v)/2-n*log(2*pi)/2-sum log|d_i|.

This supplies an explicit excluded neighborhood of v=0 from any finite
witness, without solving a nuisance profile there. On a remaining compact
shape interval v>=epsilon, the scale bounds above enclose every maximum.

For std with mean held and sd free, ordinary s is a free nuisance coordinate
and v>2. If 2(n-k)>k, the v=2 profile has a unique positive scale root of G;
this is the proper ordinary-scale t2 face, while physical sd diverges.
If 2(n-k)<k, sufficiently small v>2 permits unbounded scale collapse.
At equality the lower profile has the finite collapse value

    L0=-n*log(2)-3*sum_nonzero log|d_i|.

Equality does not justify selecting that face over a finite maximum.
For strict inequality the scale bounds with v0=2 are strictly positive.

The normal face for either held-location problem fixes m and fits
sN=sqrt(D2); let LN be that normal log likelihood. For v>=v0>=2 and a
positive uniform lower scale bound S, the ordinary-t value inequality gives

    profile(v)-LN <= C*(1/v+1/v^2),
    C=sum d_i^4/(4*S^4).

A finite witness W>LN therefore excludes every v>N, for example with
N>=max(v0,1,4*C/(W-LN)). This is a uniform unsigned tail bound on the
refitted-scale profile. It cannot select the normal face when W<=LN;
that case still requires a signed comparison under the actual hold.

### Held ordinary scale, free location and shape

Every stationary location is in [xmin,xmax], since outside this interval
all location-score summands have the same sign. The lower fitted shape is
v=1, an attained Cauchy member. Holding ordinary scale removes scale
collapse, but does not exempt the lower fitting restriction when location
is free. The normal face fixes scale at its supplied value and fits
location at the sample mean.

Let b_i=max(x_i-xmin,xmax-x_i), and let LN be the optimized conditional
normal likelihood with held scale s. The same value inequality now gives

    profile(v)-LN <= C*(1/v+1/v^2),
    C=sum (b_i/s)^4/4.

Thus a witness strictly above LN closes the entire normal tail explicitly.
Location modes can exchange order on the finite shape interval; endpoint
location solves and this tail inequality alone do not exclude branch
switches or supply joint coefficient inclusion. The implemented decreasing-
shape Cauchy theorem remains a separate sufficient certificate.

### Held standard deviation, free mean and shape: uniform lower face

Write w=v-2 and keep physical sd=s fixed. Let j be an integer at least the
largest atom multiplicity and less than 2n/3. In sorted observations choose

    delta_j = min_i(x_(i+j)-x_i)/4 > 0,
    Q_j=(delta_j/s)^2.

An interval of radius delta_j contains at most j observations. At every
location, at least n-j residuals therefore have squared standardized
magnitude at least Q_j. For 0<w<=min(1,Q_j),

    L(2+w,m,s) <= C_j+(n-3*j/2)*log(w),
    C_j=-n*log(s)-n*log(2*pi)/2+n*log(3)/2
          -3*(n-j)*log(Q_j)/2.

This is uniform over location. Its positive log(w) coefficient closes the
whole lower face whenever the largest atom is strictly below 2n/3.
It also explains why using the smallest adjacent gap is unnecessary:
several admissible window sizes j may be compared for a stronger bound.
If an atom exceeds 2n/3 the likelihood is unbounded along its fixed-mean
path, as already detected by the public fitter.

At exact critical multiplicity k=2n/3, let b be that atom, h=n-k, and let d
be its distance to the closest other atom. There is only one critical atom.
Set delta=d/4 and a=s*sqrt(w). For w<=min(1,(delta/s)^2), a<=delta.
For a<=|z|<=delta the score from the k observations at b has magnitude at
least (v+1)*k/(2*delta), directed toward b, whereas all other observations
contribute at most (v+1)*h/(d-delta). The former strictly dominates.
Every maximum within |m-b|<=delta consequently has |m-b|<=a.

On that smaller interval, log(1+t^2)>=t^2/2 for |t|<=1 bounds the loss
from the critical atom, and the other log densities are Lipschitz with
M=4*h/(d-delta). Hence

    L(2+w,b+z,s)-L(2+w,b,s)
      <= M*|z|-3*k*z^2/(4*a^2) <= K*w,
    K=M^2*s^2/(3*k).

Combine this with the scalar critical-face bound at b:

    sup_|m-b|<=delta L(2+w,m,s)-L0
      <= w*[D+K+h*log(w)/2].

Choosing w small enough makes the bracket strictly negative. Outside this
atom's delta neighborhood, choose delta' smaller than delta/2 and one
quarter of the smallest positive atom gap. At most j' observations lie
within delta' of any remaining location, where j' is the largest other
atom multiplicity and j'<=h<2n/3. The preceding positive-exponent bound
with j',delta' then excludes that entire outer region below L0 as well.
This proves a uniform punctured lower-face exclusion, including moving
location, at critical multiplicity. Integration with finite joint root
coverage is not implemented; a pointwise legacy comparison remains
insufficient.

For v tending to infinity, the normal face fits mean at the sample mean
and preserves held sd. On the compact location range, with b_i as above,

    profile(2+1/w)-LN <= n*w+C*(w+3*w^2),
    C=sum (b_i/s)^4/4.

Again a witness above LN supplies a finite whole-tail cutoff. The signed
normal-winner proof with a compact exclusion cover is implemented in
[signed-derivation.md](signed-derivation.md). General finite joint root
inclusion and coefficient accuracy certificates remain open.

### Joint location/scale at fixed 0<v<1

The strict atom condition kmax<v*(n-kmax) implies existence, without
implying uniqueness. Choose any integer j>=kmax with E_j=v*(n-j)-j>0,
and the positive window radius delta_j defined above. At every stationary
scale, the score identity and the at-least-n-j distant observations imply

    s^2 >= delta_j^2*E_j/(n*v).

All stationary locations lie in the sample range, so the other direction is

    s^2 <= (1+1/v)*(xmax-xmin)^2.

These give an explicit compact rectangle in location and log scale that
contains every finite maximizer. Existence follows independently from
uniform tails: for s<=delta_j/sqrt(v),

    L(m,s) <= n*c(v)+(v+1)*(n-j)*log(v/delta_j^2)/2+E_j*log(s),

uniformly in m; for large s, L(m,s)<=n*c(v)-n*log(s). Both tails tend to
minus infinity. These inequalities follow by bounding near observations
by exp(c(v))/s and the other n-j observations by their power-tail factors.
The counterexample above shows why every candidate inside the resulting
rectangle must still be isolated and globally compared. At critical atom
equality E_j=0, this coercivity argument does not apply; every eligible
collapse face must be compared separately. The current subunit route
retains its structured numerical refusal rather than accepting one EM root.

## Solver-stop witness retention

Held free-shape nominations may fail before a coefficient bracket closes.
Their failure status remains authoritative. The objective callback now
retains an admissible evaluated point (including a conditional solver's
existing diagnostic candidate) before returning its scalar likelihood to
the nominator. Early iteration stops and unresolved pointwise comparisons
transport that witness and set the unknown global upper bound to infinity.
When necessary, an admissible fallback fixes only free coordinates; held
coefficients remain exact. `retain_point` evaluates the represented physical
coefficients with the same gamma and pairwise-sum enclosures as the scalar
certificate. It never promotes the diagnostic point to an MLE.

The C++, R and JS regressions force iterlim=1 for each of the three held
free-shape subsets in both families. They require a stopped result, a finite
lower witness and an infinite global upper bound. C++ additionally evaluates
each displayed candidate at its own model coefficients and checks that the
reported likelihood interval contains that value.

Unresolved pointwise face comparisons use NumericalFailure rather than
IterationFailure: a converged local solve with a rounding-uncertain face
comparison is not a failed score solve. Those stopped results do not accept their main coefficient fields as estimates;
StopDetail supplies the evaluated witnesses. Genuine solver iteration
exhaustion keeps IterationFailure and its witness.

The retained nominator skips nonfinite profile values instead of treating
failed conditional solves as peaks. A nonfinite first grid point no longer
poisons later finite nominations, and an all-failed grid cannot report
convergence. This affects witness generation and failure cost; it does not
supply a globality certificate or relax any acceptance condition.


## Implemented global nonuniqueness certificates

The held-SD two-atom argument above extends to any equal multiplicities and
half-separation b > 15 SD. In SD units, for any symmetric unimodal density of
variance one, monotonicity gives
1 >= 2 integral_0^b t^2 f(t) dt >= 2 b^3 f(b)/3.
Thus every centered likelihood for one atom pair is at most 9/(4 b^6).
The shape-three witness at one atom has pair density
4/[pi^2(1+4b^2)^2] > 1/[4(1+4b^2)^2].
For b>15, b^6>225 b^4 >=9(1+4b^2)^2, since
1+4b^2<=5b^2 and strictness holds. Repeated balanced pairs raise both
products to the same power. The globally optimized normal location is the
center and is excluded by the same variance bound. The moving-location
shape-two face vanishes uniformly, by the strict atom exponent already
proved. Large locations vanish uniformly too. Hence a finite maximizer
exists off center and reflection supplies another. Code checks the
half-separation bound conservatively with the practical arithmetic enclosure.
Two actual shape-three likelihood witnesses are returned; no coordinates of
the unknown maximizers are asserted.

For held 0<nu<1, exact reflection is checked by error-free TwoSum of each
sorted pair and exact comparison with twice the represented center. Strict
atom inequalities give existence. At the center, let k be the zero-residual
count, q the squared normalized residuals, n the count, and
E=nu(n-k)-k>0. The scale score is strictly decreasing in log scale;
its root lies between squared scales
min(q_nonzero)*E/(n*nu) and (1+1/nu)*sum(q)/n.
Bisection preserves inclusion with the practical score intervals. Evaluating
the likelihood on the whole remaining log-scale interval gives an upper
bound on every centered fit. A budgeted nomination of adjacent-pair
midpoints and half-gaps supplies reflected finite witnesses only. If their
likelihood lower bounds both strictly exceed that centered upper bound,
all global maximizers are off center. Existence plus exact symmetry proves
at least two global maxima without needing their coefficients. Otherwise
this helper makes no claim and leaves the existing numerical refusal.
This proves the five-point shape-half counterexample constructively. Work
is at most 128*min(iterlim,100000) evaluations and at most iterlim bisections
and witness nominations. A tie candidate is explicitly a witness, not a
claimed optimizer. Global upper bounds remain infinity when no finite global
upper bound has been computed.

## Rejecting a dominated numerical fallback

An unresolved joint profile retains its converged admissible numerical fit,
marked uncertified. Before accepting it, evaluate its represented likelihood
with the practical intervals above. Evaluate a reachable normal-face point
with the actual holds, and, for std with mean held and sd free, a positive
ordinary-scale t2 point. A strict alternative lower bound above the numerical
point's upper bound proves that point is not a global maximizer. This is a
numerical stop with both witnesses and an infinite unknown global upper
bound. It does not select the face as the global winner. Overlap supplies no
rejection and no certificate. When the legacy search nominates a reachable
face, a finite point beaten by that face supports the nomination: preserve
the original limit class and message, marked uncertified. Reject that face
only if a strict finite witness (or a signed asymptotic argument) disproves
it. This preserves ordinary golden fits and legacy limit decisions while
keeping numerical nominations distinct from global certificates.

## Normal-relative value enclosures

Let H(u)=u-log(1+u), with the existing alternating-remainder enclosure,
and g(v)=c(v)+log(2*pi)/2, the correction relative to the normal constant.
Writing a=1 in lst and a=3 in std gives the exact identity

    L(v)=LN+n[g(v)+log(v/r)/2]
         +sum[(v+1)H(q_i/r)/2-a*q_i/(2r)].

The implementation encloses g directly. Accumulate the already weighted relative terms:
multiplying an absolute pairwise-summation margin by v after reduction
would create an artificial O(eps*v) width. The implementation uses this
identity when v>=32 and every q_i/r<=.01; elsewhere it keeps the original
logarithmic formula to avoid cancellation for large residual ratios.
These thresholds choose between exact identities, without truncating the
parameter domain. The narrow value enclosure closes strict domination of
near-normal numerical points at the default budget. That value comparison
alone does not prove a global normal winner; the separate signed-tail and
compact-cover certificate can now establish that conclusion.
