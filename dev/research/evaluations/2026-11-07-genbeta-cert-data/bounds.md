# Exact-arithmetic bounds for held p and free q, a

These inequalities concern held endpoint b and held p > 0. They underlie
`genbeta/power_profile_certificate.hpp`. The convex triangle envelope below
is an alternative bound, not the implemented enumeration. The independent
`check-bounds.py` checks their formulas numerically at 70 decimal digits;
the arguments below are the mathematical evidence.

Write y_i = log(b/x_i), m = mean(y), v_i = y_i/m, h = a*m,
g = mean(log(v)), and r(h) = -mean(log(1-exp(-h*v))). Every v_i is
positive and mean(v) = 1. After removing the constant Jacobian and -n log(m),
the log likelihood per observation is

    F_p(h,q) = log(h) - p*h + r(h) - q*r(h) - log B(p,q).

For each h > 0 the q problem is strictly concave: its derivative is
psi(q+p)-psi(q)-r(h), with negative derivative
psi'(q+p)-psi'(q). It tends to positive infinity at q=0 and to -r(h)
at infinity. Thus its unique maximizer exists. Denote the profile by P_p(h).

## A uniform small-power bound

For 0 < q <= 1,

    log Gamma(q+p)-log Gamma(q)
      = log(q) + log Gamma(q+p)-log Gamma(q+1)
      <= log(q) + log Gamma(p) + log(max(1,p)).

Indeed, the derivative of the second gamma difference has the sign of
p-1, by monotonicity of digamma. Its maximum on [0,1] is log Gamma(p)
when p <= 1 and log Gamma(p+1) when p >= 1.

Also log(q)-q*r <= -log(r)-1 for every q,r>0. The elementary inequalities
z*exp(-z) <= 1-exp(-z) <= z imply

    -log(h)-g <= r(h) <= -log(h)-g+h.

Fix H > 0 with R = -log(H)-g >= 1. For every 0 < h <= H and 0 < q <= 1,

    F_p(h,q) <= U1(H)
      = -g + max(0,1-p)*H + log(max(1,p)) - log(R) - 1.

For q >= 1, Jensen's inequality for a Gamma(t,1) variable gives
psi(t) <= log(t), hence

    log Gamma(q+p)-log Gamma(q) <= p*log(q+p).

Since r >= 1, the function p*log(q+p)-q*r is decreasing on q >= 1.
Its maximum there is p*log(p+1)-r. Consequently

    F_p(h,q) <= U2(H) = log(H)-log Gamma(p)+p*log(p+1).

The uniform bound max(U1(H),U2(H)) tends to negative infinity as H tends
to zero. It includes arbitrary q approaching zero or infinity; it is not a
fixed-q gamma expansion.

A sharper bound used in the implementation avoids the factor log(p) in U1.
For p >= 1, the function
log Gamma(p+q)-log Gamma(1+q) is concave in q, since trigamma decreases.
Its tangent at q=0 gives

    log Gamma(p+q)-log Gamma(1+q) <= log Gamma(p)+A*q,
    A = log(p)+EulerGamma >= psi(p)-psi(1).

For p <= 1 the same inequality holds with A=0 by monotonicity. Thus, for
R > A and R >= 1, the implemented small-tail bound is

    U1sharp(H) = -g+max(0,1-p)*H-log(R-A)-1.

The initial feasible member q=1,h=1/p has value -1 for every held p.
Profiling q there supplies a lower witness without a large held-p penalty.

## A uniform large-power bound

Let vmin = min(v), and assume the sample is nonconstant, so vmin < 1.
For h >= log(2)/vmin,

    exp(-h*vmin)/n <= r(h) <= -log(1-exp(-h*vmin)) <= log(2) < 1.

Using the same gamma-ratio bound and maximizing over q > 0 gives

    F_p(h,q)
      <= log(h)-p*h+(1+p)*r-p+p*log(p/r)-log Gamma(p)
      <= log(h)-d*h+C,
    d = p*(1-vmin) > 0,
    C = p*log(n)+1+p*log(p)-log Gamma(p).

The first maximum is at q = p/r-p > 0. For h >= 1/d the last bound
decreases, and it tends to negative infinity. Thus if
H >= max(log(2)/vmin,1/d), then log(H)-d*H+C bounds the whole h >= H tail.
No upper bound on q is assumed, so moving-endpoint concentration is included.

Together these inequalities imply finite attainment for every nonconstant
sample with p,b held and q,a free. On any remaining compact h interval,
r is bounded away from zero and infinity. The q tails are then uniformly
excluded by the bounds log(q)+constant-q*r for q <= 1 and
p*log(q+p)-q*r for q >= 1. Continuity yields a finite global maximum.
This proves existence, not uniqueness.

For a constant sample y_i=y0, choose q=p*exp(a*y0) and let a increase.
The gamma-ratio asymptotic and log(1-exp(-a*y0)) ~ -exp(-a*y0) give

    f_Y(y0) ~ a*p^p*exp(-p)/Gamma(p).

The likelihood is unbounded. This is the existing constant-sample stop.

## Implemented complete interval enumeration

Write D(h)=-r'(h), K(h)=r''(h), and
J(q)=trigamma(q)-trigamma(q+p). All three are positive and decreasing in
their arguments. For J, subtract the convergent positive trigamma series
term by term; its derivative is negative. Also
J(q) >= p/[q*(q+p)], by integrating -psi''(q+t) >= 1/(q+t)^2 over t in [0,p].
The unique conditional shape q(h) is increasing, with q'(h)=D/J. The profile
has derivatives

    P'(h)  = 1/h-p+(q(h)-1)*D(h),
    P''(h) = -1/h^2-(q(h)-1)*K(h)+D(h)^2/J(q(h)).

On a cell [L,H], let qL <= q(h) <= qH be validated endpoint shape bounds.
Monotonicity gives a score lower bound

    1/H-p+(qL-1)*[D(H) if qL>=1, else D(L)]

and a score upper bound

    1/L-p+(qH-1)*[D(L) if qH>=1, else D(H)].

A curvature upper bound is

    -1/H^2 + [-(qL-1)*K(H) if qL>=1, else (1-qL)*K(L)]
            + D(L)^2/J(qH).

A curvature lower bound is

    -1/L^2 + [-(qH-1)*K(L) if qH>=1, else (1-qH)*K(H)]
            + D(H)^2/J(qL).

A score interval excluding zero excludes an interior maximum. A positive
curvature lower bound excludes interior maxima even when a score root is
present. A negative curvature upper bound establishes strict concavity on
the entire cell. Validated opposite endpoint scores then isolate its sole
maximum; a same-sign derivative excludes a maximum in that cell. Remaining
cells are subdivided, without assuming a global number of modes. A root
on an artificial cell boundary with unresolved signs is a numerical stop,
not a discarded root.

The q solves first bisect the strictly decreasing beta score, then independently
validate opposite signs around q. The relative radius is at least 4e-13,
and increases to four times the score rounding allowance divided by a lower
bound on q*J(q). A radius reaching 0.01 stops numerically; it is not accepted
as a coefficient enclosure. For the starting upper bracket,
psi(q+p)-psi(q) <= p/q+p/q^2, so max(1,2p/r) suffices.
The h root solve uses padded profile score intervals, not an unchecked zero.
Monotonicity of q(h) encloses q over the retained h interval. Acceptance
requires relative h width and relative q width both below
min(tol,1e-9)/2. The midpoint's conditional q and reconstructed a=h/m
therefore satisfy the coefficient inclusion criterion in exact arithmetic.
Ill-conditioning that prevents those inclusions stops numerically.

A concave tangent bounds the conditional q maximum above its evaluated
midpoint likelihood. A tangent to the strictly concave h profile bounds
the isolated root value above that conditional maximum, using the validated
score interval and retained h width. Evaluated midpoint likelihoods provide
lower bounds. The winner must have a lower bound above every other root's
upper bound. Otherwise the fit stops with a comparison tie and retains the
root witnesses. No global uniqueness theorem is asserted.

The compact domain, its subdivision cells, and both excluded tails cover the
whole parameter space. Every finite global maximum must therefore appear
in a retained concave cell. Degenerate stationary points need not terminate:
the certificate budget or a numerical stop preserves a finite lower witness
and an infinite global upper bound. Each popped cell and profile evaluation
consumes the shared 128*min(iterlim,100000) certificate ledger; inner q/h
solves and tail bracketing have min(iterlim,100000) iteration caps.

The C++ implementation uses the package's conditional exact-arithmetic
standard. Practical rounding allowances are 64*epsilon times the absolute
beta score terms (including uncancelled log/digamma operands below the
asymptotic seam), 128*epsilon for trigamma differences, 256*epsilon for profile
scores, 512*epsilon for likelihood/interval comparisons, and 1024*epsilon for
curvature comparisons. These are not outward-rounded intervals or universal
libm error bounds. Near unresolved cancellations it stops rather than returning
a capped or unchecked estimate.

## Compact power-interval upper envelope

r'(h) = -mean(v/expm1(h*v)) < 0 and
r''(h) = mean(v^2*exp(h*v)/expm1(h*v)^2) > 0.
On [L,H], its graph lies below the secant and above both endpoint tangents.
The resulting triangle has vertices (L,r(L)), (H,r(H)), and (k,rk),
where the last is the tangent intersection. Define

    C_p(r) = sup_{q>0} [-q*r - log B(p,q)],
    G_p(h,r) = -p*h + r + C_p(r).

C_p and G_p are convex, because they are suprema of affine functions.
For any M in (L,H), log(h) <= log(M)+(h-M)/M. Therefore

    sup_{L<=h<=H} P_p(h)
      <= max over triangle vertices (z,w) of
         [log(M)+(z-M)/M+G_p(z,w)].

If rk <= 0, this envelope is infinite and the interval must be subdivided.
For positive rk it converges as the interval shrinks. An implementation must
supply upper enclosures for C_p, not substitute a fitted q's likelihood
(which is only a lower bound). Root or coefficient inclusion remains a
separate requirement even when the likelihood envelope is small.

## Exact non-uniqueness counterexample

At held q=1 the density of Y is (p*a)*exp(-(p*a)*y). For y=(1/6,1/2),
the fitted rate is 3. The distinct pairs (p,a)=(1,3),(3,1),(6,1/2), and
indeed every positive pair with p*a=3, all globally maximize the likelihood.
The fitter's a=1 convention selects an identified representation of this
attained ridge. A singular information matrix does not exclude this fit.

## What this does not settle

For free p, the small-H constant above depends on p, so it cannot exclude
p tending to infinity while h tends to zero. The large-H rate d also tends
to zero when p does; it cannot exclude p*h tending to a finite rate. These
are precisely the gamma and fixed/moving-endpoint power faces, whose uniform
signed neighborhoods and intersections need different bounds. The fixed-q
small-power expansion's quadratic coefficient can be positive, negative,
or zero. Its sign alone is not a global comparison.
