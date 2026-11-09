# Two-point Fréchet certificate and profile-dependent GEV bounds

## Exact reduction

The public free-location fit restricts Fréchet shape to lambda >= 1. Let
m < M, d=M-m, p=k/n<1/2 and q=1-p. Put b=1/lambda and
r=(M-mu)/(m-mu)>1, t=lambda log r. Profiling scale exactly gives

    ell/n = log((exp(bt)-1)/b) - q(1+b)t
            - log(p+q exp(-t)) - 1 - log d = F(b,t).

For every fixed t>0,

    F_bb = 1/b^2 - t^2/(4 sinh(bt/2)^2) > 0.

The inequality follows from sinh u > u for u>0. Thus F(b,t) is strictly
below max(F(0,t),F(1,t)) for 0<b<1. This is global coverage of the entire
open shape domain, including b approaching zero; it does not assume that
the profiled likelihood is unimodal in b. The two endpoint problems suffice.

For b=1 the derivative in r has numerator

    p(2p-1)r^2 + 4pqr + q(1-2p).

Its constant is positive and leading coefficient negative, so it has exactly
one positive root. Its value at r=1 is positive. The likelihood tends to
minus infinity at r=1 and infinity, so this root is the unique maximum.
Writing h=1/(r-1) gives

    h = sqrt(pq)-p = p(1-2p)/(sqrt(pq)+p),
    mu=m-dh, sigma=d(1-2p), lambda=1.

The second form avoids cancellation near p=1/2. Integer counts compute
1-2p as (n-2k)/n. The alternative scale identity
h(1+h)/(p+h)=1-2p provides an independent algebraic check.

For b=0 the objective is

    F(0,t) = log t - qt - log(p+q exp(-t)) - 1 - log d.
    F_t   = 1/t - q + q exp(-t)/(p+q exp(-t)).
    F_tt  = -1/t^2 - pq exp(-t)/(p+q exp(-t))^2 < 0.

Its score is positive at 1/q and tends to -q. Doubling the upper bracket
and then bisecting isolates its unique maximum. A tangent at the midpoint
bounds its maximum on the bracket by F(mid)+|F_t(mid)|*(hi-lo)/2.
The Gumbel coefficients are scale=d/t,
location=m-scale*log(p+q exp(-t)). These coefficients describe a face,
not finite Fréchet coefficients. Strict convexity proves nonattainment if
that face is strictly larger. If the shape-one member wins, its uniqueness
and strict convexity prove a unique finite Fréchet maximizer. Equality between
the endpoints would still leave only one finite maximizer; code stops on
unresolved comparison rather than asserting an exact equality.

## Arithmetic and work

Statements above are proofs over the reals. The implementation is certified
in exact arithmetic conditional on computed comparisons, with practical
floating-point margins; it does not implement outward rounding.
It uses a total log-likelihood margin
4096 epsilon n (1+|log d|+|log h|), and a separate 256 epsilon propagation
guard for scale and translated location. Cancellation near zero public
location, overflow, underflow and tighter requested tolerances stop numerically.
A finite witness is retained when representable. The Gumbel supremum stop
retains lower and tangent upper bounds plus the limit's public coefficients.
Bracketing and bisection share min(iterlim,100000) solver steps. Exhaustion
is a solver/numerical stop, not a certificate-evaluation budget stop.

The independent 70-digit check verifies the reduction, scale identity and
980 strict convexity comparisons. It also computes face and finite reference
values and the crossing in p. Those evaluations cross-check the proof; they
are not the proof. The ordinary untied goldens are outside this dispatch.

## Compatible GEV holds

The previously proved unique all-free GEV two-point member has xi=-1,
sigma=p*d, mu=M-p*d. Restricting the parameter set to contain that member
cannot change the maximum or its uniqueness. The held overload now recognizes
exact equality of a public held coefficient with the accurately computed
member, and reuses the certificate. No tolerance-based hold substitution is
made. This covers, for example, data (0,2,2,2) with mu=1.5 and/or sigma=0.5.

## Profile-dependent bounds for general samples

Here is a sharper route than the previous whole-box Gumbel remainder, but it
also identifies why an exact inner stationary point is not enough.
For fixed -1<xi<0, alpha=-1/xi, let b=1/s, a=E/s and t_i=a-b*x_i.
Write ell=n log b+sum phi(t_i)+n log alpha, where
phi(t)=(alpha-1)log t-t^alpha is strictly concave.
At a stationary point (a_hat,b_hat), concavity of each phi implies

    ell(a,b) <= ell_hat + n[log(b/b_hat)-(b/b_hat-1)].

Indeed the summed tangent has zero a coefficient and b coefficient
-n/b_hat by the two stationary equations. If an incumbent has likelihood L,
every surviving nuisance scale therefore satisfies

    r - 1 - log r <= (ell_hat-L)/n, r=b/b_hat.

This is a practical *profile-dependent* scale interval when ell_hat is close
to L: its width is of order sqrt((ell_hat-L)/n), rather than the enormous
uniform range. For a nonstationary center the exact extra linear terms are
its two scores; dropping these terms would make an invalid upper bound.
At xi=0 the same argument applies to the concave Gumbel natural coordinates.
At fixed b, strict concavity in a and the explicit likelihood permit bracketing
the superlevel endpoints. Affine public holds can be intersected exactly.

These bounds are conditional on a fixed shape. Applying them over a shape
interval requires certified enclosures of the center, its scores, the inner
maximum, and their change with shape. The existing Newton inner solve has
neither coefficient inclusion nor a uniform interval Hessian bound. In
particular its score/step convergence test cannot justify substituting an
exact stationary point into the inequality above. On xi>0 the natural-coordinate
joint objective is not concave; the existing monotone fixed-shape location
certificate gives a root at a point shape but no enclosure over a shape interval.
At xi=-1 the upper endpoint observation changes from an interior zero-density
constraint to a positive-density closed boundary; ordinary derivative bounds
through that face are invalid. At xi=0 the Frechet public map is singular.

For a concrete numerical measure, `profile-probe.cpp` records the nuisance
scales and score/curvature on both goldens at selected shapes. It also
records the necessary scale ratio intervals from the inequality above when
a slice exceeds the incumbent. These are diagnostics computed with the
existing numerical profile; they are explicitly not certified enclosures.
The tighter intervals show a route on nonpositive shapes, but they cannot
exclude a competing positive-shape maximum or justify an outer global result.
No unused production enclosure helper is added and the default search is
not promoted to certified status.

The same restriction argument applies to Fréchet when the finite two-point
winner wins the endpoint comparison and the public held scale is exactly
its scale d*(n-2k)/n. A losing all-free Gumbel face cannot be transported to
a finite held scale. The fitter therefore reuses only a certified finite
member matching the hold, and leaves every other held-scale problem on its
own constrained route.

Public holds need an additional exactness guard: equality to a rounded free
estimate does not prove containment of the exact global member. The reused
certificate therefore also verifies the exact count identity
n*sigma=k*(M-m) for GEV, or n*sigma=(n-2k)*(M-m) for Fréchet.
An error-free subtraction checks that the finite normal range is exact;
fused multiply-add residuals check both integer products. Counts must be
exactly representable integers, and overflow rejects reuse. For held GEV
location, an error-free subtraction additionally verifies the exact relation
mu=M-sigma. For example, the Fréchet sample with seven zeroes and nine ones
has exact winning held scale 1/8; the rounded double 0.1 for nine zeroes and
eleven ones is not an exact 1/10 hold and does not use this shortcut.
Other holds continue through their existing constrained searches. This avoids
asserting an exact restriction argument solely from rounded equality.

## Shape-dependent nuisance regions from monotone endpoint gaps

The following is additional exact mathematics for the open outer certificate.
It is not used to label an ordinary joint fit certified.

### Positive GEV shape: Frechet coordinates

Let a=1/xi>0, mu_F=m-e, d_i=x_i-m>=0 and e>0. Write
S_r(e)=sum_i(e+d_i)^r. Profiling Frechet scale gives
s_F=[n/S_{-a}]^(1/a). The location equation is

    R_a(e)=S_{-a-1}/(S_{-a} S_{-1}) = (a+1)/(na).

For Q_r=S_{r-1}/S_r, Q_r strictly decreases with r on a nonconstant sample.
Consequently

    d/de log R_a = -(a+1)Q_{-a-1}+a Q_{-a}+Q_{-1}<0.

At fixed e, d/da log R_a is positive: it is the difference of means of
log r_i under weights r_i^(a+1) and r_i^a, where r_i=e/(e+d_i), and the
increasing weight tilt has positive covariance with log r_i. The right
side (a+1)/(na) decreases with a. Thus the unique location gap e(a)
strictly increases with a wherever a(n-k)>k. Here k is the minimum count;
for all-free fitted shapes a>=1, the whole region exists when k<n/2.

Scale s_F(e,a) is a negative-order power mean. It increases with e and
decreases with a. Moreover

    partial_e s_F = s_F^(a+1) mean[(e+d)^(-a-1)] >= 1,

so q_F=s_F-e increases with e and decreases with a. If a is in [A,B]
and certified endpoint roots give e_A<=e(a)<=e_B, then

    s_F(e_A,B) <= s_F(e(a),a) <= s_F(e_B,A),
    q_F(e_A,B) <= q_F(e(a),a) <= q_F(e_B,A).

The mapped GEV region is therefore

    m+q_F(e_A,B) <= mu_G <= m+q_F(e_B,A),
    s_F(e_A,B)/B <= sigma_G <= s_F(e_B,A)/A.

These bounds keep the cancellation in mu_F+s_F correlated. In particular
m <= mu_G <= mean(x), since a negative-order power mean lies between the
minimum and the arithmetic mean. A stable exact identity for computing q_F is

    q_F = e*expm1[-log(mean(exp(-a*log1p(d/e))))/a].

This is a profile-dependent region on every finite positive-shape interval,
without a three-dimensional uniform nuisance box.

### Negative GEV shape: reflected Weibull coordinates

Put a=-1/xi>1, endpoint E=M+e, d_i=M-x_i>=0. Profiling Weibull scale gives
s_W=[S_a/n]^(1/a), and its location equation is

    T_a(e)=S_{a-1}/(S_a S_{-1})=(a-1)/(na).

Using the same decreasing Q_r,

    d/de log T_a = (a-1)Q_{a-1}-a Q_a+Q_{-1}>0.

At fixed e, T_a decreases with a: the log-shape derivative is the
difference between the log-distance means under powers a-1 and a.
Its right side increases with a. The unique gap e(a) therefore increases
with a as well. The limits in e are 0 and 1/n, proving the bracket and
unique crossing, independently of the concave natural-coordinate proof.
At a=1 the continuous boundary solution is e=0, s_W=mean(d).

The positive-order power mean s_W increases with both e and a; its derivative
in e is at most one. Thus q_W=s_W-e decreases with e and increases with a.
Endpoint roots for [A,B] give

    M-q_W(e_A,B) <= mu_G <= M-q_W(e_B,A),
    s_W(e_A,A)/B <= sigma_G <= s_W(e_B,B)/A.

The scale bounds follow from monotonicity of s_W and division by a; the
location bounds follow from the two monotonicities of q_W. Unlike an
independent location/scale box obtained from a density supremum, these
regions shrink with the shape interval and use the actual inner maximizers.
The independent script checks both sets of corner bounds at interior shapes.

### The Gumbel seam in pivot coordinates

There is also a way to remove the previous exponential whole-box constant.
Use the minimum pivot, v=xi*e on the positive branch, d_i=x_i-m, and
L_i=log1p(xi*d_i/v)/xi. After the exact location/scale orbit profiling,

    ell(xi,v) = -n log v+n log n-n log sum_i exp(-L_i)
                -(1+xi)sum_i L_i-n.

At zero shape L_i=d_i/v. On a near-zero slab with |xi|*max(d_i)/v<1,
every L_i is nonnegative, on either side of zero, and sum exp(-L_i)>=k.
All exponentials are at most one. Taylor remainders for L_i and the
exponential/log-sum terms therefore use powers of max(d_i)/v, 1/(1-rho), and 1/k,
where rho bounds |xi|*max(d_i)/v below one;
they do not require the exp(Z) factor of an arbitrary public-nuisance box.

For each fixed shape in this slab, the inner root is unique: the positive
side has the monotone Frechet ratio above; the negative side has either
the Weibull ratio or strict natural-coordinate concavity. Uniform signs
of the pivot-scale score at two v boundaries would enclose all the inner
maximizers throughout the slab, including zero. Implementing those interval
signs and profile derivative enclosures is still required; a pointwise
Newton result cannot stand in for them.

At xi=-1, an interval in a starting at one has e bounded below by zero.
The minimum Weibull distance then has a log singularity. Its density factor
can instead be bounded by the closed-endpoint envelope and the other positive
gaps bounded separately. This requires a value enclosure at the face; a
regular derivative enclosure that includes log(0) is not valid.

These are explicit routes to tighter finite-shape nuisance regions and a
better Gumbel parameterization. Missing software is the certified endpoint
root enclosures exposed as intervals, interval score/profile evaluation across
shape slabs, the reverse-endpoint value enclosure, coefficient isolation, and
complete comparison of all surviving candidates. The current fixed-shape
routine returns a point and does not expose the required interval object.
No production helper for this unfinished outer procedure is added.


Scale profiling is essential to these gap equations. They bound the two free
GEV nuisance coefficients at fixed shape, and the corresponding all-free
Frechet profiles. A public held Frechet scale changes the location equation;
these unconstrained roots cannot replace that constrained problem. Other
public nuisance holds likewise require their own restricted inner solve.

A useful global corollary applies to the profiled GEV location on the fitted
branch [-1,1]:

    min(x) <= mu_G <= mean(x).

For positive shape it follows from the negative-order power mean bound above.
For negative shape a>=1, the positive-order power mean lies between the
arithmetic mean and the maximum distance, so mean(d)<=q_W<=max(d).
The Gumbel value is m-s*log(mean(exp(-d/s))), and Jensen's inequality gives
the same bounds. At xi=-1, mu_G=mean(x). These bounds already hold after
exact orbit/scale profiling, before the endpoint-gap root is solved; they
do not describe all nuisance points in an incumbent superlevel set. They
therefore cannot be substituted for bounds under a public nuisance hold.

The finite-shape corner regions do not by themselves isolate outer coefficients.
A termwise interval evaluation generally loses the stationary cancellations:
its value width can be first order in the shape width although the true
likelihood loss near a smooth maximum is quadratic. A useful outer certificate
therefore still needs score/curvature enclosures and root inclusion, rather
than only pointwise maxima and these value boxes. Near zero it also needs a
uniform inner-score sign or nonsingularity bound on the proposed pivot box;
pointwise strict concavity alone does not provide that computed comparison.


When transporting a failed free certificate to a recognized exact scale hold,
its global upper bound remains an upper bound on the constrained supremum.
Its Gumbel face need not satisfy the hold, so the free global lower bound
cannot be reused. Only evaluated candidates satisfying the held scale supply
a lower witness. The binding preserves the resulting numerical stop instead
of rescuing it with the old unconstrained or conditional search.
