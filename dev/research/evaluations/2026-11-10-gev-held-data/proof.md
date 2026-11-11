# Constrained likelihood cover

This note proves the exact-arithmetic bounds used by `gev/held_certificate.hpp`
and the held-scale addition to `frechet/certificate.hpp`. Computed signs and
interval inclusions use practical floating-point margins, not directed rounding.
A failed inclusion or incomplete cover leaves the successful search unchanged.
A strictly better feasible point is different: after rechecking the comparison
in public coordinates, it disproves the search and produces a numerical stop
with both feasible point witnesses.

## GEV compact nuisance regions

Write `L=log(1+xi*z)/xi`, `y=exp(-L)` and
`h_xi(z)=exp(-(1+xi)*L-y)`. The removable-zero interpretation is used.
For `-1<=xi<=1`, maximizing `(1+xi)*log(y)-y` proves `h<=1`.
Moreover,

    z^2 h = L^2 [sinh(xi L/2)/(xi L/2)]^2 exp(-L-exp(-L))
          <= (1-y)^2 exp(-y) <= 1.

The middle inequality follows because `sinh(t)/t` increases in `|t|`,
and the last bound follows by differentiation: the only positive stationary
point other than the zero at y=1 is y=3, with value 4 exp(-3)<1.
Thus `f(x)<=min(1/sigma,sigma/(x-mu)^2)`. At xi=-1 the same
inequalities hold for the upper semicontinuous closed endpoint.

Let B be a feasible full log likelihood and n the sample size. With mu held,
let j count observations different from mu and S sum their log absolute
distances. Any point with likelihood >=B satisfies

    log(sigma) <= -B/n,
    (2j-n) log(sigma) >= B+2S.

When 2j>n these are a compact interval. Otherwise this implementation leaves
the collapse comparison unresolved and returns the search result uncertified.
With sigma held, put R=sqrt(sigma exp(-B/n)). A location outside
`[min(x)-R,max(x)+R]` makes every density smaller than exp(B/n), so cannot
beat B. The implementation expands the level bounds slightly; their artificial
nuisance boundaries cannot maximize. xi remains in the public interval [-1,1].
For a nonnegative-shape slab with scale held, the earlier fixed-scale
location-ratio proof gives the uniform inner-root enclosure
`min(x)<=mu<=min(x)+sigma*log(2n/k)`. The zero-shape formula has the same
bound. Boxes outside that enclosure contain no maximum and can be discarded
without a likelihood comparison. This prevents the data range from setting
the positive-shape location search width.

There is no additional limit model at infinity in these compact regions. The
Gumbel member is the smooth xi=0 seam, with the same public hold. The closed
reverse-exponential member and xi=1 are retained shape boundaries, never
substituted by unconstrained face fits.

## Jets and moving support

The two coordinates are xi and log(sigma) with mu held, or xi and mu with
sigma held. Second-order chain-rule jets evaluate the original likelihood.
For |xi*z|<=1/4, scalar geometric series enclose L, L_xi and L_xixi
with explicit remainders; L_z=1/(1+xi*z),
L_zz=-xi/(1+xi*z)^2 and L_xiz=-z/(1+xi*z)^2. Integral
representations give the extrema of the shape derivatives at corners in
(xi,z), also considering z=0 when crossed. In particular L_xi has its
maximum at z=0; L_xixi increases in z and decreases in xi. Outside that
radius, division by xi is used only on intervals separated from zero.
Series iterations obey the caller's solver budget.

A box crossing support does not supply derivative jets. Instead its feasible
t range bounds log(y)=-log(t)/xi, including the infinite limiting endpoint.
For each endpoint c of 1+xi, maximize c log(y)-y over that log(y)
interval. Convexity in c makes the larger endpoint bound a uniform envelope.
For c=0,y=0 its value is zero, preserving the finite reverse-exponential
face. Each term also subtracts log(sigma)'s interval lower bound. A box
entirely outside support is discarded. Boxes through xi=0 that cannot
supply jets retain the universal h<=1 envelope until subdivision.

## Fréchet with public scale held

Normalize by the held scale and shift the minimum to zero. Write the
location as -e and lambda>=1. At its unique fixed-lambda location root,

    e >= [lambda*k/((lambda+1)*n)]^(1/lambda)
      >= [k/(2n)]^(1/lambda),       e < 1.

The lower inequality is the minimum-term location-score bound established
in the [earlier G2/F1 analysis](../2026-11-08-gev-allfree-data/held-scale-region-proof.md); the upper follows directly because every
`x+e>=1` makes the location-gap score strictly negative. Existence and
uniqueness follow from the strictly decreasing fixed-shape ratio score,
with its opposite limits. Thus every global maximizer has
`log(k/(2n)) <= log(e) <= 0`. On a shape slab [a,b], the stronger
uniform enclosure uses the sharper bound
`log(e)>=(log(k/n)-log1p(1/a))/a`; its right-hand side increases with a. This is an enclosure of
all inner roots, not an assertion that every feasible parameter lies there.
The cover only needs maxima and may exclude points below this root bound.

The shape endpoint infinity needs a separate constrained comparison. Let
D be the normalized data range and c=asinh(D/2)>0. At least one endpoint
has |log(x+e)|>=c, because an interval [exp(-c),exp(c)] has length D.
All densities are <=lambda+1 in normalized units. For the selected endpoint,
when lambda*c>=log(1+1/lambda), its log density is at most log(lambda)
plus

    max(-(lambda+1)*c, c-1-(lambda*c)^2/2).

For the negative-log-distance side the log density increases up to its
mode, so the constrained maximum is at -c; exp(t)>=1+t+t^2/2 gives
the displayed polynomial bound. Therefore the full log likelihood is at most

    n log(lambda+1) + max(-(lambda+1)*c,c-1-(lambda*c)^2/2).

This tends to minus infinity and is decreasing once lambda exceeds both
2n/c and 2sqrt(n)/c. Budgeted doubling finds a finite upper endpoint whose
envelope loses strictly to B. Slab versions use the increasing log term at
the upper shape endpoint and the decreasing penalty at the lower endpoint.
No free-scale Gumbel face is feasible here: public sigma_F is fixed, so
sigma_G=sigma_F/lambda tends to zero. Nonconstant data rule out that spike
by the tail envelope. The lambda=1 member remains an attained restriction.

## Root existence, coefficient inclusion, and global cover

Newton iterations only propose a candidate. An interval negative definite
Hessian establishes strict concavity on a small rectangle. A Krawczyk image
strictly inside that rectangle proves an interior stationary point exists;
strict concavity makes it unique. At a shape endpoint, a uniform outward
shape score, strictly negative nuisance curvature and opposite nuisance endpoint
signs prove the corresponding one-dimensional maximum and its uniqueness in
the rectangle; the full Hessian need not be negative definite in this case. Interval
Newton, using an interval inverse Hessian (or the scalar nuisance curvature
at the endpoint), then contracts an enclosure of that exact maximizer.

The stopping rule checks both coordinate widths, including relative shape
precision, with internal tolerance min(tol,0.01). This keeps the relative
coefficient implication valid even for a deliberately loose user tolerance. Exponentiated nuisance widths bound relative scale/gap precision.
GEV location precision is checked after the original affine map, using
max(abs(public mu),public sigma). A normalized location width alone is
insufficient when the data minimum is far from the fitted public location. Root widths reserve three quarters
of tol for elementary arithmetic and public-coordinate restoration. Tolerances
below 1024 machine epsilons, and free public scales without sufficient normal
floating-point resolution, leave the successful search uncertified.

All remaining boxes are covered until one of these proves no global maximum
lies there: a value envelope below a feasible lower witness; a nonzero nuisance
score; a shape score pointing away from every admissible shape boundary; or
inclusion in the already isolated strictly concave basin. A centered value
Taylor envelope with interval Hessian bounds accelerates value comparisons.
Splitting along the basin boundaries makes that exclusion exact. This is a
complete stationary/value box cover when it finishes; it does not assume
profile unimodality or compare only sampled stationary points. It is an
alternative to constructing profile Schur jets: the full constrained Hessian
retains both free coordinates and encloses all maxima simultaneously.

### Enlarged basin (added 2026-11-10, TASK-172)

The basin that isolates the root starts at relative radius 2e-6 and only
shrinks. Near a maximum the score and value tests above weaken, so a cover
that excludes only that basin pays a roughly constant number of boxes per
decade of distance to the root while it approaches it. The basin's own
criterion does not weaken there, and it is a pointwise condition, so it can
be established on a larger box by pieces.

Let x* be the enclosed root and B a box containing the basin, clipped to the
domain. Suppose B is covered by finitely many pieces and the interval Hessian
is negative definite on each piece. Then the Hessian is negative definite at
every point of the convex set B, the likelihood is strictly concave on B, and
its stationary point x* is the unique maximizer of B. No other maximizer lies
in B, whether or not any single interval evaluation covers all of B.

At a shape edge the basin criterion is instead an outward shape score and
negative nuisance curvature. If both hold on every piece of B, the likelihood
strictly decreases along the shape coordinate away from the edge at every
nuisance value, so the maximum of B lies on the edge face. On that face the
likelihood is strictly concave in the nuisance coordinate and x* is its
stationary point, hence again the unique maximizer of B.

A piece without smooth jets (crossing support, or outside an inner-root
enclosure) cannot be verified and is subdivided; it never counts as verified.
The implementation doubles both radii until an attempt fails and keeps the
last verified box. An attempt checks only the ring around the box verified
so far and examines at most 400 pieces (1/32 of the default certificate
allowance), charged to the shared cap. The same practical floating-point
margins as the basin test apply. The
[measurements](../2026-11-10-held-concave-report.md) show where the previous
cover spent its allowance and what this changes.

All cover evaluations, including point proposals and contraction evaluations,
consume a shared cap of 128*min(maxit,100000); individual solver loops use
min(maxit,100000). Tail bracketing uses the solver cap. Failure to establish
any premise, resolve a floating comparison, or complete coverage returns the
unchanged successful search result, with `certified=false`. It creates no
budget/numerical stop. A strict counterexample above the isolated candidate
upper bound plus a 4096-epsilon operand margin instead rejects that search.
The public-coordinate likelihood comparison is repeated before stopping, with
both feasible witnesses retained. Existing search failures are preserved.

Interval value, gradient and Hessian endpoints use the joint module's
compensated summation. The practical jet pad is 128 machine epsilons times one plus the magnitude
of each computed interval; support/value envelopes use 512 epsilons. These
are exact-arithmetic certificates conditional on those computed comparisons
and on the adequacy of that practical margin. They are not outward-rounded
interval certificates. Independent finite differences and point-versus-box
jet checks are numerical validation of the formulas, not proofs of rounding.
