# General GEV profile certificate: exact-arithmetic argument

The fitted all-free branch is -1 <= xi <= 1. Standardize the nonconstant
sample to minimum zero and maximum one. Minimum multiplicity is k<n/2;
the existing exact pair/collapse classifications handle the other cases.
Statements here are real-arithmetic proofs. The implementation is conditional
on its computed comparisons with explicit practical floating-point margins;
it does not claim outward rounding.

## Inner coverage

For positive shape write a=1/xi, endpoint -e, e>0. For negative shape
write a=-1/xi>1, upper endpoint 1+e, e>0. The monotone ratio arguments in
[two-point-proof.md](two-point-proof.md) prove a unique profiled endpoint-gap
root, and e(a) increases with a on each branch. Opposite signs at finite
bracket ends, not convergence of a point Newton solve, enclose that root.
The numerical point solver supplies only a proposer for this sign bracket.

On a positive slab [l,h], endpoint brackets give e(h)<=e(xi)<=e(l), hence
the minimum-pivot scale v=xi*e lies in [l*e(h),h*e(l)]. On a negative slab,
v=-xi*(1+e) lies in [-h*(1+e(l)),-l*(1+e(h))]. Away from zero the reflected
Weibull endpoint gap itself is a better nuisance coordinate: its rectangular
box always stays within support. Both coordinates describe the same unique
inner optimum and have the same profiled shape value and derivatives.

At xi=0 the unique Gumbel optimum is the continuous inner limit. On a slab
crossing zero, uniform opposite pivot-score signs at two v boundaries,
together with uniqueness for each shape, give an inner-root enclosure for
every shape in the slab. A pair of point signs is insufficient and is not
used. If an endpoint-root upper bound is at or below the rectangular
support floor max(0,-xi_lower), no uniformly feasible pivot rectangle can
contain that root; subdivision is immediate. Otherwise lower-bracket
expansion approaches this floor from above instead of jumping to zero or
outside support. One-sided slabs also intersect this bracket with the proved
endpoint-gap corner box; expansion stops once that box is contained. Uniform interval Newton and the implicit derivative
v'(xi)=-f_xv/f_vv contract existing enclosures only when f_vv is bounded
strictly away from zero throughout the entire box.

## Minimum pivot and derivatives

For d_i=x_i>=0, v>0 and 1+xi*d_i/v>0, put

    L_i=log1p(xi*d_i/v)/xi,
    f=ell/n=-log(v)-log(mean(exp(-L)))-(1+xi)*mean(L)-1.

The expression profiles the other location/scale orbit exactly. L_i>=0,
exp(-L_i)<=1 and sum exp(-L_i)>=k. Therefore no exponential of a large
standardized radius enters an interval remainder. With z=d/v and q=xi*z,

    L_x=(z/(1+q)-L)/xi,
    L_xx=(-z^2/(1+q)^2-2L_x)/xi,
    L_v=-z/[v(1+q)],
    L_vv=z(2+q)/[v^2(1+q)^2], L_xv=z^2/[v(1+q)^2].

When |q|<=r<=1/4 the implementation sums the convergent power series for
L, L_x and L_xx separately, including xi=0. After powers through j,
valid remainder bounds are

    |R_L| <= Z*r^(j+1)/[(j+2)(1-r)],
    |R_Lx| <= Z^2*r^(j+1)/(1-r),
    |R_Lxx| <= Z^3*r^(j+1)*[(j+3)/(1-r)+r/(1-r)^2],

where Z bounds z. Other derivatives use the rational expressions above.
Writing E for the probability weights proportional to exp(-L), derivatives
of -log mean exp(-L) are E(L_j) and
E(L_jk)-Cov(L_j,L_k). These identities give all six interval jet fields.

On negative slabs bounded away from zero, put w_i=e+1-x_i and a=-1/xi.
The alternative exact profile is

    f=log(a)+(a-1)*mean(log w)-log(mean(w^a))-1.

Normalize w by the fixed upper distance in each box. All power exponentials
then remain at most one. With r=1/w, weighted expectations proportional to
w^a, and l=log w,

    f_a=1/a+mean(l)-E(l), f_aa=-1/a^2-Var(l),
    f_e=(a-1)*mean(r)-a*E(r),
    f_ae=mean(r)-E(r)-a*Cov(l,r),
    f_ee=-(a-1)*mean(r^2)-a*(a-1)*E(r^2)+a^2*E(r)^2.

Transform shape derivatives with a'=a^2 and a''=2a^3. Profile curvature
in either nuisance coordinate is f_xx-f_xv^2/f_vv where f_vv<0.

## Reverse-exponential face

At xi=-1 the maximizer is mu=mean(x), sigma=1-mean(x), with upper
endpoint one. For a>1 the maximizer has positive gap e(a). Convexity of
log mean(w^a) in a and log a<=a-1 imply

    ell(a,e)<=ell(1,e)+(a-1)*[n+sum log(w)-n*sum(w log w)/sum w].

For a in [1,A], monotonicity gives 0<e<=e(A). Bound each nonzero log
term at e(A), retain k_max*log(e(A)) for the k_max observations at the
sample maximum, and bound
w log w below using its minimum at exp(-1). Also ell(1,e)<=ell(1,0).
This gives a finite one-sided face-value enclosure, without a derivative
through log(0). A neighbourhood is removed only if this bound is no larger
than the retained face itself, or is strictly below a feasible incumbent.
Failure to establish either comparison is a numerical or budget stop.
The value-only reflected-Weibull envelope also applies to every strictly
negative interior slab, even when its nuisance derivatives use the pivot
chart. It can exclude a slab before rectangular pivot support is possible.

## Outer coverage and coefficient isolation

Every shape slab is retained until one of the following is established:
its value upper bound is below the incumbent; its profile score has a fixed
sign; its curvature is strictly positive (no interior maximum); or it
contains a unique stationary maximum with public coefficient inclusion.
Centered Taylor bounds retain the zero nuisance score at an exact inner
optimum, avoiding the first-order value overestimate of a raw box.

For strictly negative nuisance and profile curvature, opposite endpoint
profile-score signs prove a unique stationary maximum within the slab.
The full two-dimensional score vanishes there. An interval inverse of the
Hessian gives a joint interval-Newton enclosure in shape and nuisance.
The mean-value theorem applies to the straight segment inside the feasible
box; a positive determinant bound verifies every required inverse.
Contraction never supplies existence: the already proved crossing does.

Public negative-shape coefficient bounds retain the power-mean dependencies.
For s(e,a)=(mean((e+1-x)^a))^(1/a), a>=1, the power-mean inequality
makes s increase in a and e, and Holder gives partial_e s<=1.
Thus s-e decreases in e and increases in a. With e in [u,v] and a in [A,B],

    1+u-s(u,B) <= mu <= 1+v-s(v,A),
    s(u,A)/B <= sigma <= s(v,B)/A.

These four point-corner evaluations avoid subtracting two independently
varying copies of the endpoint gap. They retain the same arithmetic margin.

The root is accepted only after its mapped mu, sigma and xi enclosures
meet the package coefficient accuracy target. Endpoints are retained as
candidates separately. Competing separated candidate intervals whose value
bounds cannot be ordered yield a tie with witnesses. Budget exhaustion or
arithmetic stagnation cannot return an unchecked incumbent.

For k<n/2 the positive-branch endpoint-gap criterion holds uniformly on
0<xi<=1; the collapse e->0 has likelihood tending to minus infinity.
At fixed nonzero shape e->infinity also has likelihood tending to minus
infinity. The finite-scale xi->0 transition is covered by the pivot slab.
The xi=-1 face is explicitly compared. Thus orbit profiling, inner
uniqueness, the face enclosure and the outer cover account for every
all-free nuisance escape on the fitted shape branch.

## All-free Fréchet transport

Restrict the GEV cover to 0<=xi<=1 and retain the xi=0 Gumbel closure
candidate. With the original sample minimum m and pivot v,

    lambda=1/xi, mu_F=m-v/xi, sigma_F=v*exp(xi*g)/xi.

For each positive-shape stationary enclosure the implementation bounds these
three public coefficients directly. In particular lambda requires a relative
xi enclosure, not merely an absolute GEV-shape tolerance. The same slab
value and derivative proofs and minimum-multiplicity collapse classifications
cover the entire all-free Frechet fitted branch. A strictly winning xi=0
closure member is unattained by every finite Frechet parameter, so its
proper Gumbel coefficients are returned only as a typed limit. Finite and
Gumbel witnesses have separate public-coordinate transport in the bindings.

A public held Frechet scale imposes sigma_G=sigma_F*xi and
mu_G=mu_F+sigma_F. It removes the free orbit used in this profiling argument.
Consequently this certificate is wired only into the all-free public route;
held-scale and other constrained joint fits keep their existing algorithms
and the previously proved exact compatible-member shortcuts.
