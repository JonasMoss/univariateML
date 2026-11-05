# Root neighborhoods and lower faces

This note uses total likelihood, ordinary t scale s, a=s sqrt(nu),
t=log(a), and c(nu)=log Gamma((nu+1)/2)-log Gamma(nu/2)-log(pi)/2.
It extends the [previous derivation](../2026-11-04-t-cert-data/derivation.md).
These are optimization derivatives, with no inference interpretation.

## Analytic interval Hessian

For r_i=x_i-m and D_i=r_i^2+exp(2t), the negative likelihood is

    F=-n c(nu)-n nu t+(nu+1)/2 sum log D_i.

Its gradient in (m,t,nu) is

    F_m=-(nu+1) sum r_i/D_i,
    F_t=-n nu+(nu+1) sum exp(2t)/D_i,
    F_nu=-n c'(nu)-n t+sum log D_i/2.

The six Hessian entries are

    F_mm=(nu+1) sum (exp(2t)-r_i^2)/D_i^2,
    F_mt=2(nu+1) sum r_i exp(2t)/D_i^2,
    F_tt=2(nu+1) sum r_i^2 exp(2t)/D_i^2,
    F_mnu=-sum r_i/D_i,
    F_tnu=-n+sum exp(2t)/D_i,
    F_nunu=-n c''(nu).

The Laplace representation from the previous derivation gives positive,
decreasing c' and -c''. Their interval bounds require just two endpoint
evaluations each. This avoids subtracting unrelated interval trigamma
values over a shape box. Shape coordinates are w=nu/v0 for an exact
binary64 constant v0; the Hessian rows and columns transform accordingly.

The 2026-11-05 implementation rejects direct interval evaluation of
F_t=-n nu+(nu+1) sum a^2/D: treating its two occurrences of nu independently
gives width of order n nu h on a shape interval of relative width h.
That bound prevented the radial inward check for the nu approximately 218
workload and for the older multimodal winners, even after shrinking h.
Use instead the exact identities

    F_t=n-(nu+1) sum r_i^2/D_i,
    F_tnu=-sum r_i^2/D_i.

Their interval widths track n h rather than n nu h. The Hessian and the
mathematical objective are unchanged. Numerical Hessian differentiation
independently checks the transformed expressions. The earlier refusal
was a dependency artifact, not evidence that the stationary root was absent.

## Root existence and uniqueness

For a proposal p, radius vector r, and numerical inverse C of its Hessian,
enclose e=|C grad F(p)| and Q=|I-C Hess F(B)| over B=[p-r,p+r].
Accept a box only if e+Q r<r componentwise and ||Q||_infinity<1.
The map p-C grad F(p) is a strict self map and contraction on the box.
Brouwer and the mean value inequality give existence and uniqueness of
one stationary root. Invertibility of C is required. Solver convergence
and the numerical residual alone never cause acceptance.

Finite positive scales and public coefficients are transformed as intervals.
In particular sd=s/sqrt(1-2/nu) for std; its interval is computed only when
the shape box lies strictly above two.

## All nuisance optima in a larger neighborhood

Let J be a shape interval and N a nuisance rectangle containing a Euclidean
ball of radius r centered at the proposed nuisance coordinate. Bound the
two-dimensional nuisance Hessian below by lambda I throughout N x J and
the nuisance gradient at its center above by G uniformly over J. If
G<lambda r, the radial outward derivative of F on the ball is positive.
Its minimum in the closed ball is therefore interior and stationary.
The existing fixed-shape geodesic convexity theorem identifies this
stationary point as the unique global nuisance optimum for every shape
in J. The argument requires the strict mass condition throughout J.

Independently enclose a congruence C H C^T of the full three-coordinate
Hessian over N x J. C is the inverse Cholesky factor at the proposal;
||C H C^T-I||_F<1 proves positive definiteness everywhere. This is a
uniform matrix bound rather than a point eigenvalue check. A positive
lower bound lambda_full on H gives P''(nu)<=-lambda_full/v0^2 on J
by the Schur complement. Thus a certified root inside this neighborhood
is the unique profile maximum in J. The small root box must be contained
in the larger product box. Outside J, every shape interval still needs
strict exclusion by its global upper envelope.

For an endpoint q, full Hessian positivity is unnecessary. The same
nuisance-ball argument plus a strictly positive lower bound on F_nu
throughout N x [q,q+h] proves P'<0 there. At q=1 this certifies the
attained Cauchy member, provided its nuisance root is also isolated.
At q=2 it certifies an excluded t2 neighborhood. The global engine still
has to exclude every other interval and the normal tail.

## Equality masses: an exact endpoint, not an early winner

Suppose an atom b has multiplicity k=q(n-k), q=1 or 2. Put
b_i=log(D_i/a) and b_inf=-log a. The negative likelihood minus its
normalizer is

    F+n c = (q+1)/2 sum b_i+n(q-1)/2 b_inf.

The weight of b is half the total Busemann weight. For every other
finite atom x, the identity

    D_b D_x-a^2(x-b)^2=((b-m)(x-m)+a^2)^2 >= 0

proves b_b+b_x>=2 log|x-b|, while b_b+b_inf>=0.
Pair the weight of b with all the remaining finite and infinity weights.
This gives, globally at shape q,

    ell_q <= C_b = n c(q)-(q+1) sum_(x_i!=b) log|x_i-b|.

Taking m=b and a down to zero reaches C_b as a supremum. Thus its value
is exact. For q=2 positive infinity weight forces m=b at equality and
the finite-atom inequalities cannot also be equal for a>0. The endpoint
is unattained. For q=1 there is no infinity weight; equality is possible
exactly when there is only one distinct off-atom value. These are the
balanced two-point Cauchy data. With more support points the endpoint
is unattained. This proves the claimed attainment distinction at fixed q.

The convex function P(nu)-n c(nu), defined as a supremum of affine
functions, includes the finite endpoint C_b-n c(q). Its chord inequality
therefore remains valid on [q,h] despite loss of positive nuisance-scale
containment at q. Taking a collapse path with
(nu-q)|log(s)| -> 0 also proves right continuity to this endpoint.
Consequently C_b can be used as an endpoint upper enclosure in the
existing shape subdivision. Equality does not prove that C_b wins the
all-free fit. The relative endpoint chord argument below supplies a
computable lower-neighborhood exclusion; unresolved intervals must stop.

## Relative endpoint chords include the cusp

The sharper interpolation remainder for 0<=u<=h gives

    P(q+u)-P(q) <= (u/h) [P(q+h)-P(q)]
                   + n u(h-u)/(2q^2).

Keep the exact P(q) on both sides before bounding the secant difference.
If an upper enclosure D for P(q+h)-P(q) satisfies

    D/h+n h/(2q^2) < 0,

then the entire punctured endpoint neighborhood lies strictly below P(q).
There is no constant endpoint evaluation error on the right. Its evaluated
enclosure only affects D, which is multiplied by u/h. This covers an
ordinary negative endpoint slope and a critical-tie negative infinite
slope. Right continuity and the known cusp guarantee that sufficiently
small h works in exact arithmetic for unattained critical equality,
excluding the balanced two-point Cauchy exception. A guarded
finite-precision search still may refuse if it cannot verify the inequality.
It does not need a separately implemented uniform cusp remainder.

For q=1 the attained positive-scale Cauchy fit requires its coefficient
root certificate before it can become a successful fit. For q=2 the
endpoint is excluded. With critical mass, the collapse endpoint is also
excluded except for the separately dominated balanced two-point Cauchy
case. A successful neighborhood check alone cannot select any face:
the remaining intervals and normal tail still must be strictly dominated.

## Balanced two-point all-free fits select normal

For two equally weighted distinct support points, let d be half their
separation. For every nu>1 the unique nuisance root is their midpoint
and s=d, by substitution in both scores and geodesic convexity.
The derivative of profile log likelihood per observation is

    c'(nu)-log(1+1/nu)/2 > 0.

Indeed the Laplace representations reduce this strict inequality to

    1/(1+exp(-u)) > (1-exp(-u))/(2u), u>0,

which follows from 2u>1-exp(-2u). Hence the optimized likelihood is
strictly increasing to normal for both lst and std. The Cauchy endpoint
can be attained and nonunique, but is strictly dominated. Its per-observation
gap below normal is (log(2 pi)-1)/2 > 0. This settles the exceptional
balanced two-point case without searching or a df cap.

## An exactly zero normal slope with a proved collapse winner

Consider mass 2/3 at b, and mass 1/6 each at b-d and b+d, d>0.
This includes the retained six-observation critical-tie sample. For nu>2,
fixed-shape uniqueness and symmetry force m=b. The scale score gives

    a^2=d^2(nu-2)/3, s^2=d^2(nu-2)/(3nu).

Thus the optimized derivative per observation is

    P'(nu)/n=c'(nu)-log((nu+1)/(nu-2))/6 < 0.

Use the Laplace representation of c' and of the logarithm. Pointwise
the strict inequality is equivalent to

    (exp(2u)-exp(-u))(1+exp(-u)) > 6u, u>0,

or 2 sinh(2u)+2 sinh(u)>6u, which follows from sinh(v)>v for v>0.
The whole profile is therefore strictly decreasing from its collapse
supremum -n log(2d). No numerical tail or grid is needed. In std
coordinates sd=d/sqrt(3) along the profile even though ordinary scale
collapses as nu decreases to two. The excluded face is a collapse path,
not a positive ordinary-scale t2 fit.

The sample has exactly m4=3, m6=9 and m3=0, hence A=0 and B=n/4>0
at the normal seam. The normal is beaten near infinity, while collapse
is the global winner. This is a complete zero-slope face decision for
this sample class, not a general zero-slope classifier. Exact rational
support/count comparisons guard its executable recognition.

## Arithmetic contract and limits

The vector interval code enlarges each elementary operation by
64 binary64 epsilon times (1+maximum endpoint magnitude). A sum enlarges
by 64 epsilon (n+1)(1+sum endpoint magnitudes). Affine normalization of
original binary64 inputs is enclosed using those operations; its constants
are treated as exact represented constants. Digamma/trigamma differences
are evaluated with 70-digit mpmath and then converted and padded.
The revised point score also uses 65 or 70-digit evaluation on the exact
original binary64 observations and exact represented affine constants,
then receives the binary64 conversion allowance. The larger-box Hessian
still uses the enlarged binary64 interval operations. This mixed precision
point check avoids amplifying a cancellation-heavy binary64 score sum in
the shallow shape direction; it is an additional research requirement,
not an implicit claim about the existing C++ score arithmetic.
This is a practical conditional arithmetic contract, not directed-rounding
interval arithmetic or a uniform libm error proof for every platform.

The separate whole-profile experiment uses 65-digit mpmath and an absolute
1e-48 base value/score-to-value allowance, augmented by
4096 mp.eps (n+1)(1+nu log(1+nu)+sum|x|), nu<=1e30, finite positive nuisance
containment and 4096 cached profiles. These stronger arithmetic resources
are research tools; a binary64 C++ implementation would need its own
propagated margins and performance validation. Relative likelihoods are
computed in exactly normalized normal coordinates at high precision;
the normal tail's signed bound does not incorporate a fixed absolute
normal-likelihood rounding error. Neither experiment changes a default.
