# Fixed-shape t held-subset certificates

Write the ordinary t density with location m, scale s>0 and shape v>0.
All statements here concern the total sample likelihood and the stated
held subset. The arithmetic implementation is conditional on its computed
comparisons with practical floating-point margins, not outward rounding.

## Free scale, fixed location and shape

Let d_i=x_i-m and let k residuals vanish. The log-scale score is

    G(t) = (v+1) sum d_i^2/(v exp(2t)+d_i^2) - n.

Every nonzero-residual term decreases strictly with t. Its limits give
G(-infinity)=v(n-k)-k and G(+infinity)=-n. Thus a positive collapse
exponent gives exactly one stationary scale and the global maximum; a
negative exponent gives an unbounded likelihood. At equality G(t)<0 for
every finite t, and the unattained supremum is

    n*c(v) + (v+1)*(n-k)*log(v)/2
      - (v+1)*sum_{d_i != 0} log|d_i|,

where c(v)=log Gamma((v+1)/2)-log Gamma(v/2)-log(v*pi)/2.
This equality is specific to held location: it does not prove that a
free-location joint fit has no competing interior maximum.

Collapse comparisons use fma(v,n-k,-k). A separately rounded product can
manufacture equality: binary64 (1/3)*3 rounds to one although the exact
product of the supplied binary64 shape and three is smaller. Likewise,
held binary64 8/3 lies below the critical 8-to-3 multiplicity ratio. Fused
subtraction preserves these small signs. Observation counts in any
representable workstation sample are exact binary64 integers; at a
nonzero critical shape this residual cannot underflow. No input is
reinterpreted as an exact rational supplied by the author.

The code brackets t inside the representable positive-scale range,
bisects to width <=1e-10, checks the represented-scale score, and checks
signs at t +/-5e-9 against its documented rounding margin. The latter
bracket certifies relative scale width about 1e-8, conditional on those
comparisons. The iteration cap must never turn a wide bracket into an
accepted scale. Shape is held in the std transformation, so multiplying
by sqrt(v/(v-2)) preserves relative scale accuracy.

## Free location, fixed scale and shape

Put a^2=v*s^2. Apart from a negative factor, the location score is

    S(m)=sum (m-x_i)/(a^2+(m-x_i)^2).

S is negative below the data range and positive above it, so every
maximum lies in the closed sample range. Except for a constant sample,
the range endpoints cannot be maxima. A covered interval cannot contain
an interior maximum if S has a strict sign or if S'<0 throughout it.
When S'>0 and the endpoint signs straddle zero, that interval has exactly
one stationary point and it is a maximum. A slight interval enlargement
allows roots exactly on subdivision boundaries to be included. The
positive lower derivative bound L maps an enclosed midpoint score to a
root enclosure by |m-root| <= |S(m)|/L. Repeated contraction, with solver
and certificate caps, supplies coefficient accuracy. All surviving maxima
are compared through likelihood enclosures; overlapping values yield a
tie stop with the candidates. Covered regions and overlapping root boxes
avoid counting the same root twice. Degenerate stationary points can
require a budget or numerical stop; there is no assumption of simple
roots or of uniqueness at the API boundary.

The implementation divides S by a common positive factor and standardizes
the data before interval evaluation. It reuses the 64*epsilon practical
interval operations and binary-carry sums from lst/certificate.hpp, and
encloses the transformation of a as well. Its root width is at most
1e-8*min(1,s/unit) in normalized coordinates; the final location transform
must additionally have width <=1e-6*unit.

For data -b,+b the numerator is

    2m*(m^2+a^2-b^2).

When b^2>a^2, zero is a minimum and the two maxima are
+/-sqrt(b^2-a^2). Their values are exactly equal by symmetry. For std,
a^2=(v-2)*sd^2. The nonuniqueness threshold therefore changes to
b^2>(v-2)*sd^2. check.py verifies the rational identity at 80 digits;
the identity itself is an algebraic proof, not inferred from that grid.
Displayed candidate coefficients are rounded in physical units; their likelihood enclosures evaluate those represented points. Root-enclosure likelihoods remain separate for global exclusion and upper bounds. This distinction matters for a large common translation with small held scale.

At b^2=a^2 the root is degenerate and the current scalar interval method
may refuse; it does not return an unchecked estimate.

## Joint location and scale, fixed shape

For v>=1 under the strict atom condition k_max<v*(n-k_max), the
geodesic-convexity result proved in the [t certificate study](../2026-11-04-t-cert-report.md) gives
a unique stationary nuisance fit and makes it global. EM supplies a
score-checked nomination. We refine it using the stable ordinary-scale
nuisance Hessian and require a two-dimensional contraction inclusion.
For an approximate inverse C, gradient enclosure g and Hessian enclosure H
on a radius-r box, each row must satisfy

    |(C*g)_i| + r*sum_j |(I-C*H)_ij| < r,
    sum_j |(I-C*H)_ij| < 1.

The contraction maps the closed box strictly inside itself, proving a
unique root there. The global theorem supplies globality; positive local
curvature alone would not. Complete transformed location and scale widths
are checked. Ordinary nuisance coordinates avoid multiplying large shape
powers into the nuisance inverse, including at v=1e300.

Balanced two-point Cauchy equality is outside the strict atom theorem:
there is a continuum of maxima satisfying
s^2=(m-x_min)*(x_max-m), x_min<m<x_max. Its established Cauchy nomination is accepted only after verifying a
positive interior location and the curve scale enclosure. The chosen representable interior location is projected onto that scale curve, and the resulting relative scale width must be at most 1e-6; the nominated half-separation scale is not assumed to remain optimal after rounding location. With adjacent
binary64 observations there may be no representable interior midpoint;
rounding the midpoint to an endpoint gives a strictly suboptimal likelihood
(log(1.25) below the maximum for two points at the nominal half-separation
scale). That case now refuses numerically and evaluates its displayed
witness directly, rather than reusing the analytic maximum's likelihood. Joint fits with 0<v<1 do not inherit the v>=1 theorem and now refuse numerically, with a finite witness where available. Exact unboundedness still precedes this refusal. At subunit-shape joint equality the same numerical refusal applies: a finite collapse path does not establish that it is the global winner. Held-location equality is separately proved by its monotone scale score.

## Joint critical atom at held shape at least one

In upper-half-plane coordinates (m,a=s*sqrt(v)), write
beta_x=log(((m-x)^2+a^2)/a) and beta_infinity=-log(a).
Apart from constants, the negative likelihood is

    F=(v+1)*sum beta_x/2 + (v-1)*n*beta_infinity/2.

For v>=1 this is geodesically convex by the Busemann-function proof in
the [t certificate study](../2026-11-04-t-cert-report.md). Along a hyperbolic geodesic ray ending at atom b,
a tends to zero and m-b=O(a^2). At k=v(n-k), its divergent log(a)
coefficient cancels, giving a finite limit independent of the starting
point and ray. A convex function on a ray with a finite limit cannot
increase, so this limit is a global infimum of F: the collapse value
is the global likelihood supremum. For v>1, beta_infinity is strictly
convex on a nonvertical finite-endpoint ray. On a vertical ray, any
non-b finite observation supplies a strictly convex Busemann term instead.
Thus finite attainment is excluded in either case.
For v=1, strictness fails only when every other observation is at the
ray's other boundary endpoint: exactly the balanced two-point case.
With three or more support points at critical half mass, the supremum
is unattained. This gives the critical fixed-shape joint stop a proved
global likelihood enclosure and diagnostic (b,v) collapse path.
Subunit shape has a negative Busemann coefficient and does not inherit
this argument; critical subunit joint fits refuse numerically.

## Held-scale decreasing-shape regime

For ordinary scale held, write q_i=((x_i-m)/s)^2 and

    b_v(q)=log(1+q/v)-(v+1)*q/[v*(v+q)].

Then the total shape derivative is n*c'(v)-sum b_v(q_i)/2.
Differentiation in q gives b'_v(q)=(q-1)/(v+q)^2, so the global minimum
is b_v(1)=log(1+1/v)-1/v. For every location, at least one observation
has q_i >= Q=((max x-min x)/(2s))^2. If Q>1 this gives the uniform bound

    shape derivative <= B(v)=n*c'(v)-[b_v(Q)+(n-1)*b_v(1)]/2.

The code computes a lower enclosed Q and uses it in this inequality.
For v>=Q, set u=Q/v<=1. Integration gives
R(u)=log(1+u)-u/(1+u)>=u^2/[2(1+u)^2]>=u^2/8. Hence
b_v(Q)>= (Q^2/8-Q)/v^2 and b_v(1)>=-1/(2v^2).
The digamma integral representation gives

    c'(v)= integral_0^infinity exp(-v*t)*tanh(t/2) dt / 2
          <= 1/(4v^2).

Consequently B(v)<0 throughout v>=Q whenever

    Q^2/8-Q > (2n-1)/2.

A practical interval subdivision verifies B(v)<0 on [1,Q]. If all
segments close, every location's likelihood decreases strictly with shape
on the whole fitting domain v>=1. Its profiled likelihood therefore has
its global maximum at v=1. The fixed-shape location certificate then
isolates and compares all Cauchy location maxima; no symmetry argument
or local Hessian is substituted for that check. Shape and location stages
share the single certificate evaluation budget. An inconclusive sufficient
bound falls through to the uncertified numerical profile search; exhausting the certificate
budget is a budget stop with a finite witness. This theorem applies to
all datasets satisfying the tested bounds, not just the retained seven-
point restriction example. It does not apply with held location's lower
shape exemption or to std's varying ordinary scale at held sd.

## Open: actual-hold free-shape certificates

Beyond the proved held-scale decreasing-shape regime, free-shape held fits retain their prior numerical profile searches and face comparisons. Search convergence is required; a failed solve stops, and lack of a global certificate alone does not stop a successful numerical fit. The family-wide certified flags consequently remain partial. The bounds below concern missing global certification, not procedure availability.

With location held and scale free, the scale score above provides a unique
profile for each admissible shape. If any residual vanishes, lst with
free shape below one is unbounded through sufficiently small shape and
scale; that exact diagnostic remains available. If no residual vanishes,
optimizing each observation's scale separately gives the useful bound

    sup_s prod f_v(d_i;s) <= prod f_v(d_i; |d_i|).

Each factor on the right is asymptotic to v/(2|d_i|), so this profile tends
to minus infinity as v decreases to zero. Wendel's gamma-ratio inequality gives the explicit weaker bound

    f_v(d; |d|) <= sqrt(v)/(sqrt(2*pi)*|d|),
    profile(v) <= n*log(v)/2 - n*log(2*pi)/2 - sum log|d_i|.

Consequently a lower cutoff can be obtained directly from a finite witness.
This closes the mathematical lower tail for nonzero held-location residuals;
its integration with uniform profile/root neighborhoods remains unimplemented. The existing all-free
engine cannot be called unchanged: it refits location instead of enforcing
the held location and has a different shape floor.

For ordinary fixed scale and location, the normal-seam coefficient in
h=1/v is sum(z_i^4-2*z_i^2-1)/4, z_i=(x_i-m)/s. For std with fixed sd
and mean it is sum(z_i^4-6*z_i^2+3)/4, z_i=(x_i-m)/sd. With held location
and fitted scale, the normal optimum has average z^2=1, giving
sum(z_i^4-3)/4. The first two coefficients can differ even in sign and
neither is the all-free refitted formula without its constraints.

For fixed ordinary scale and location restricted to the sample range,
let B=max_i,m |(x_i-m)/s|, which is finite. The inequalities
log(1+u)>=u-u^2/2 and c(v)<=-log(2*pi)/2 imply

    logLik(v,m,s) - logLik_normal(m,s)
      <= n*B^4*(1/v+1/v^2)/4.

A finite witness above the conditional normal supremum therefore supplies
an explicit large-shape exclusion cutoff, also for the held-scale/free-
location profile. For held location/free scale, the scale-score equation
bounds the maximizing scale above by sqrt((1+1/v)*mean(d^2)). If every
residual is nonzero it bounds it below by min|d|; the same tail-value bound
then applies uniformly. These value inequalities do not resolve cases whose
winner is the normal supremum: that still needs a signed uniform derivative
remainder under the actual holds. They also do not isolate finite roots or
branch switches. Their implementation is open even where the displayed
mathematical value bound is sufficient.

For fixed scale and free location, location roots can exchange order or
tie as shape varies. A profile certificate must include all of them, with
uniform envelopes across branch switches. For std with held sd the ordinary
scale s=sd*sqrt((v-2)/v) changes with shape. Its v-down-to-two endpoint is
scale collapse, not the positive ordinary-scale t2 face. A lower-end
uniform bound, the actual-hold normal-tail bound and root inclusions for
all surviving constrained branches remain to be implemented. Increasing
the legacy shape grid cannot establish any of these claims.

For joint fixed shape below one, geodesic convexity fails; global nuisance
box enumeration and comparisons to every collapse face remain open.
None of these gaps is an interval-arithmetic requirement: the package's
conditional practical-comparison standard suffices once the bounds exist.

## Standard-deviation transport

At fixed v>2 the scale factor sqrt(v/(v-2)) preserves relative coefficient
accuracy. The transformed coefficient must still be positive and finite,
and its likelihood finite. A finite ordinary-scale root can overflow sd
near v=2; that is a numerical refusal, never an Ok result with infinite sd.
The proper ordinary-scale point remains a diagnostic Lst witness, so its
coefficients are not mislabelled as an unrepresentable std point. Finite
data whose joint nuisance normalization range overflows likewise stop
numerically, not as invalid observations.

## Solver and arithmetic refusals

Solver caps retain the IterationFailure core enum and are transported as
structured numerical stops in both bindings. Where a finite last point
has been computed, its physical-coordinate likelihood is retained solely
as a witness, with an unknown global upper bound. A nonfinite likelihood
evaluation is an arithmetic numerical failure. No score-failed point is
returned as a fitted parameter vector.

## Physical-coordinate accuracy

The scalar location certificate normalizes by empirical population standard
deviation, enclosed after centering and range scaling. Its final location
width must be at most $10^{-6}$ times the lower enclosure for that deviation.
The held scale is not an accuracy unit: using it would allow an arbitrarily
wide location interval when scale dwarfs the sample. With data
$(-2,-1,0,1,3)$ and scale $10^{100}$, the maximum is effectively the mean
$0.2$; this is a regression guard against that mistake.

Exponentiating a tight log-scale bracket does not establish coefficient
accuracy for subnormal doubles. The scalar and joint certificates additionally
require the represented scale's logarithm to agree with the nominated
log-scale to $5\times10^{-9}$. Standard-deviation transport has the same
check. If a held standard deviation maps to a nonnormal ordinary scale,
nonconstant location search refuses rather than optimize the rounded scale.
All-held points and constant-sample location maxima use direct standardized
log-density evaluation, avoiding that conversion. These are practical
floating-point safeguards, not an outward-rounded interval arithmetic claim.

Diagnostic points with free standard deviation retain their proper ordinary-t
model coordinates. When standard deviation is held, a transported diagnostic
point is reevaluated at that exact standardized coefficient before its
likelihood interval is displayed. The initial location search interval also
encloses both transformed data endpoints, rather than rounding its coverage
inward.
