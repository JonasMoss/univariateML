# Root boxes and propagated face bounds

These are exact arithmetic results conditional on enclosure of every displayed
quantity. The research implementation uses the practical arithmetic allowance
below; it is not a proof of a universal libm error bound.

## A root box, without an assumption of joint uniqueness

Use log observations y and coordinates t=(k,c,b), b=-c log s. Drop only the
constant -sum y. Write z_i=c y_i+b, p_i=sigmoid(z_i), w_i=p_i(1-p_i).
The three scores are

\[
 g=(n/k-\sum_i\operatorname{softplus}(z_i),\quad
 n/c+\sum_i y_i-(k+1)\sum_i y_i p_i,\quad n-(k+1)\sum_i p_i).
\]

Their symmetric Jacobian has upper triangular entries

\[
 H_{00}=-n/k^2,\ H_{01}=-\sum_i y_i p_i,\ H_{02}=-\sum_i p_i,
\quad H_{11}=-n/c^2-(k+1)\sum_i y_i^2w_i,
\quad H_{12}=-(k+1)\sum_i y_iw_i,\quad H_{22}=-(k+1)\sum_i w_i.
\]

Let B be the coordinate box t0+[-r,r], with k,c strictly positive. Let R
be any nonsingular matrix (numerically -H(t0)^(-1) is useful). Enclose
H throughout B. For the map T(t)=t+R g(t), let Q_ij bound
|[I+R H(B)]_ij| and e_i bound |[R g(t0)]_i|. If

\[
 e_i+(Qr)_i<r_i\quad\hbox{for every i},
\]

T maps B into itself, by the mean value integral. Brouwer gives a fixed
point. The weighted infinity norm of its derivative is bounded by
max_i (Qr)_i/r_i<1, so Banach gives uniqueness in B. Nonsingularity of R
makes this a score root. This is a complete three-coordinate certificate,
not a scalar score crossing or an optimizer convergence flag. The code
requires each ratio below 0.9, leaving a 0.1 comparison margin.

## Uniform nuisance containment and a local maximum

A larger box is needed for exclusion of the rest of the profile. Fix
k in [k0-delta,k0+delta]. Apply the same map only in the two nuisance
coordinates, with a fixed nonsingular 2 by 2 R. Enclose the center nuisance
score over the entire k interval, and its nuisance Jacobian over the
full product box. The same inequalities give a nuisance root in the
rectangle at every k. Strict fixed-k concavity identifies it as the unique
conditional maximum, including outside the rectangle.

For that entire product box, choose any nonsingular C, practically the
inverse transpose of the Cholesky factor of -H(t0). Enclose
D=C^T(-H)C-I. If max row sum |D| is below one, symmetry and the spectral
norm bound show -H positive definite. Thus the full likelihood is strictly
concave there. The unique contained conditional branch has

\[
 F''(k)=H_{00}-H_{0\theta}H_{\theta\theta}^{-1}H_{\theta0}<0
\]

by the Schur complement. If the small three-score box lies strictly inside
this slab and its nuisance rectangle, its stationary point uniquely
maximizes the entire slab. This supplies the missing negative profile
curvature bound. Quantitatively -H >= (1-rho)/||C||_2^2 times the identity,
where rho is the enclosed row sum. The code uses the smaller lower bound
(1-rho)/||C||_F^2, with an enclosed sum of squares, to avoid relying on
a numerical singular-value computation. Since the profile tangent has
k component one, the same lower bound gives -F''(k) at least this value.
The full likelihood is not asserted
concave outside this box.

Outside the slab, use the previously proved dual/chord and face bounds.
Every interval must either be strictly dominated by a feasible candidate
lower bound or subdivided. Thus no root survey is required to be exhaustive:
a missed competing stationary point prevents the upper bounds from closing.
If all other intervals close, this is a global uniqueness certificate for
this sample, without a general Burr uniqueness theorem. The implementation
retains candidate roots from the previous study as proposals; their previous
root locations and numerical type labels are not certificate premises.

## Coefficient widths

The code encloses k and c directly and encloses s=exp(-b/c) by interval
arithmetic, in original log coordinates. Its half widths in k,c,b are
1e-8 times (k,c,max(1,|b|)). These are selected trial widths, not assumptions:
the box test must pass. The retained golden box is compared with the frozen
golden, including both endpoints, to check its 1e-6 tolerance.

## Weibull bracketing without a spurious constant seam error

For any log sample let E(c)=sum exp(c y_i), and

\[
 f(c)=n/c+\sum y_i-n\frac{\sum y_i e^{c y_i}}{E(c)}.
\]

Then f'(c)=-n/c^2-n Var_c(y)<0. A bracket [l,u] with enclosed
f(l)>0 and f(u)<0 contains the unique Weibull power. Enclose
eta=log(n/E([l,u])) and v_i=exp([l,u] y_i+eta). This encloses the
*exact* Weibull nuisance solution; an approximate score is never substituted
for a zero score in the signed seam theorem. In particular
A=sum v_i^2/2-n is enclosed by direct evaluation.

The concave profiled Weibull likelihood V obeys
V(l)<=W<=V(l)+f(l)(u-l); any feasible Weibull value supplies a lower
bound. The script uses a center feasible value and this upper tangent.

To propagate into the basic seam bound, fix a radius R<l and use
q_i=|y_i|+1 >= ||(y_i,1)||_2. If the enclosed v_i are [a_i,b_i], use
lower weights

\[
 \omega_i=\frac{a_i e^{-Rq_i}}{(1+h b_i e^{Rq_i})^2},\qquad
 M=\operatorname{diag}(n/(u+R)^2,0)+\sum_i\omega_i r_i r_i^T.
\]

An enclosed positive det(M)/trace(M) is a lower bound mu on its smallest
eigenvalue. Enclose d=sum v_i(v_i-1)r_i and
E=sum v_i^2 |v_i-1|q_i; take D=||d||_1+hE and B=n/6+D^2/(2mu).
The existing proof applies with these conservative constants. Test
hD/(mu R)<0.5 and hB/(-A_upper)<0.5, with A_upper<0.
It proves H(xi)<W for 0<xi<=h **relative to the same exact W**. The width
of the W enclosure is used only for finite-versus-face comparisons;
it is not added as a constant to H(xi)-W. This avoids the residual-score
obstruction identified in the earlier report.

If the A enclosure contains zero, this signed test supplies no conclusion.
A finite witness strictly above W may still use the unsigned tail bound.
Otherwise the finite attempt must report a tie/budget outcome. It must never
round A to zero and select Weibull. The exact zero-slope example is handled
by its finite witness; the retained binary64 proxy is not an exact zero.

## A controlled dual residual

Take any numerical p_i in [0,1] for all but one index j. Define, in exact
arithmetic,

\[
 p_j=\frac{n}{k+1}-\sum_{i\ne j}p_i.
\]

Enclose this expression. Only if its whole enclosure lies in [0,1] is
feasibility proved. The residual is then exactly zero by definition, not
approximately zero because a summation happened to return the target.
Enclose the dual A and every entropy term, and require A_lower>0.
Binary entropy is concave, has maximum log 2 at 1/2, and has minimum
at an endpoint of an interval; this gives stable entropy enclosures including
0 and 1. The previous dual formula yields the upper endpoint. If correction
is infeasible, mix the proposal toward 1/(k+1), then retry; infeasibility
or excessive width is a numerical budget outcome, not an estimate.

## Chord evaluation and arithmetic contract

For a<=k<=b, convexity of F(k)-n log k gives a chord upper bound.
A stable, slightly looser whole-interval maximum is

\[
 \max(U_a,U_b)+\frac{n(b-a)^2}{8a^2}.
\]

Indeed the interpolation error of log on [a,b] is bounded by
(k-a)(b-k)/(2a^2). This avoids subtracting two almost equal endpoint
likelihoods and dividing by a small interval width.

In the research arithmetic, each elementary interval operation enlarges
each endpoint by 64 epsilon (1+max(|lo|,|hi|)), epsilon=2^-52.
An n-term sum enlarges by
64 epsilon (n+1)(1+sum max(|lo_i|,|hi_i|)), to cover cancellation.
The practical assumption is correctly rounded basic binary64 arithmetic,
finite operations, and absolute libm error at most 8 epsilon max(1,|result|)
for exp, log, softplus, sigmoid and entropy evaluations. This is deliberately
a practical margin, not a universal libm theorem. Domain failures must stop.
The code checks the mathematical inequalities on the enlarged intervals;
box and curvature tests additionally require a 0.1 or greater norm margin.
All dominance comparisons are strict after padding. No empirical n*1e-9
likelihood pad is used by the new global check.

This prototype still obtains *proposals* from the previous numerical solver,
whose own empirical dual output is discarded. A production port must give
that proposal solver explicit numerical failure/budget returns for every
failed bracket and nonfinite operation, and preserve those returns through
the public fitting and reciprocal-delegation paths. The research Python
interval class asserts domains and is not that port.

## A rejected curvature diagnostic

At a stationary point, the change xi=1/k gives
H''(xi)=k^4 F''(k), because the additional 2k^3 F'(k) term vanishes.
Consequently their signs must agree. The retained earlier 40-node quadrature
reports positive seam curvature on `burr-k0.08-r1` and `burr-k0.08-r2`, despite
labeling them maxima. The new product-box Hessian certificates and independent
80-digit Schur complements give negative profile curvature at these points.
[verification.json](verification.json) records both values. The prior
quadrature values must not be used as maximum/curvature certificates.

This decision is dated 2026-11-04. The old evidence is retained unchanged.
The present box proof uses analytic logistic Hessian entries and the basic
analytic seam bound; it uses no Gauss-Legendre derivative quadrature.
The sign discrepancy alone already rejects reuse of the old diagnostic;
it is not evidence against the analytic seam derivative identity.

The trial nuisance radii use 2 delta |R| |H_theta,k|, with a small positive
componentwise floor, rather than 2 delta |R H_theta,k|. Component cancellation
in the latter can make it too narrow for the rectangular interval extension
of the center score, even when it approximates the actual branch derivative
well. The certificate inequalities, not this trial formula, decide acceptance.
The basic n/6 seam remainder and one-norm D are retained for simpler allowance
propagation; they can be much looser than the previous matrix remainder.
