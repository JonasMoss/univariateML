# All-free conditional and tail bounds

These are exact-arithmetic bounds conditional on the computed comparisons,
with practical floating-point margins. `allfree_bounds.hpp` supplies conditional
and uniform tail enclosures; `allfree_certificate.hpp` independently covers the
compact power interval before a finite all-free result is marked certified.
The old mesh proposes for the certificate and supplies uncertified legacy
returns when optional verification cannot close. The default coefficients are retained when already within
the final coefficient enclosure. The moving-power face now has a separate complete upper-envelope certificate.
A gamma-winning proposal has an additional signed quadratic/fourth-order
neighborhood and compact coverage verifier; incomplete verification retains
its legacy selected limit payload, explicitly uncertified. A successful finite or power-face legacy answer is likewise
retained when optional coverage cannot close and has not proved it inferior.
Quadratic-zero gamma seams and unclosed coverage remain uncertified, so the family remains `partial`.
The review derivation in [review-gamma-expansion.md](review-gamma-expansion.md)
proves that the proposed profiled first-order coefficient is identically zero.

Write s=a mean(Y), r=-mean(log(1-exp(-aY))), Y=log(b/x). At this fixed
power, the beta-shape objective apart from log(a) is

    F(p,q)=-p s-(q-1)r-log B(p,q).

For nonconstant Y the sufficient statistics are interior. The beta family
is minimal, so F has a negative definite Hessian and a unique finite maximum.
The contrasts cp=s+log(1-exp(-r)), cq=r+log(1-exp(-s)) are positive.
The [previous conditional shape proof](../2026-11-07-genbeta-cert-data/remaining-profile-bounds.md)
gives the rectangle

    exp(-r)/s <= p* < 1/cp,
    exp(-s)/r <= q* < 1/cq.

The implementation allows s,r to vary over padded intervals. Both contrasts
increase in both arguments, so their lower endpoints bound the upper shape
endpoints. The lower shape endpoints use the upper statistics.

## Sharper conditional-shape rectangles

The original 2/cp and 2/cq bounds can be sharpened to 1/cp and 1/cq.
From the trigamma series, 1/t<trigamma(t)<1/t+1/t^2. Thus for
alpha(t)=log(t)-digamma(t),

    0<alpha(p)-alpha(p+q)<q/(p*(p+q)),
    0<alpha(q)-alpha(p+q)<p/(q*(p+q)).

Call these differences dp,dq. The exact score identities imply

    cp=dp+log(1+(q/p)*(1-exp(-dq)))
      < q/(p*(p+q)) + 1/(p+q) = 1/p.

The symmetric inequality gives cq<1/q. This sharpening is wired into both
point and whole-cell shape rectangles, the small-power bound Q=2/c, and
the moving-power p upper bound B(h)=1/(h*delta-log(n)-1). It tightens a
proved enclosure; it does not impose a parameter restriction.

## Inner maximizers over intervals

The p score is psi(p+q)-psi(p)-s. It strictly decreases in p and s, and
strictly increases in q. Consequently, on [sL,sH] x [qL,qH], the conditional
p root lies between its roots at (sH,qL) and (sL,qH). Each corner is bisected
with independently padded opposite score signs. An uncertain sign stops
bisection while retaining the last validated endpoints. A wide interval is
still an enclosure; it is not a requested coefficient-accuracy claim.
The upper initial bracket max(1,2q/s) follows from
psi(p+q)-psi(p) <= q/p+q/p^2. No positive lower endpoint means no enclosure.

Apply this to the entire retained q interval, intersect with the previous p
rectangle, then apply the symmetric q-root inclusion over the new p interval.
Every intersection contains the joint maximizer: its p coordinate is an
inner maximizer at its q coordinate, and conversely. There is no assumption
that the maximizer occurs at the numerical proposal. This reuses the
held-q certificate's strictly monotone inner-profile mathematics; the
existing full held-q power enumerator is not run independently at each q.
It would only give isolated q fits, rather than a uniform interval enclosure.
The score implementation is shared with `free_p_certificate.hpp`: its existing
held-q fitter retains the same 64-epsilon score calculation, and the interval
extension there uses twice that allowance for the two corner solves. The
all-free conditional rectangles call that interval inner solver directly.

For any positive proposal (p0,q0), concavity gives

    F(p*,q*) <= F(p0,q0)+grad F(p0,q0) dot ((p*,q*)-(p0,q0)).

Maximize the right-hand tangent over the retained rectangle. Its maximum
is a valid conjugate upper bound, including for a deliberately nonstationary
proposal. The evaluated proposal gives a lower bound. Add n log(a) and the
original Jacobian -sum(log(x)) to obtain total-likelihood bounds. These
bounds concern this single fixed power only; they must never replace the
global upper bound of an unfinished search. In particular the core's new
`conditional_bounds.loglik_upper` is distinct from `stop_detail.loglik_upper`.

## Uniform tails and intersections

Normalize v=Y/mean(Y), h=a mean(Y), c=-mean(log(v)), delta=1-min(v)>0.
The [previous uniform-tail proof](../2026-11-07-genbeta-cert-data/remaining-profile-bounds.md)
controls the conditional two-shape maximizer, including diverging shape paths.
For h<=c/2, q<=2/c and

    P(h) <= G+max(1,2/c) h.

Here G is the unrestricted gamma maximum on v. The gamma profiled objective
G(k)=k log(k)-k-log Gamma(k)-(k-1)c is strictly concave; its maximizing shape
lies in [1/(2c),1/c], since 1/(2k)<log(k)-psi(k)<1/k. A tangent plane at the
already computed gamma proposal bounds G above over this entire interval.
Thus no unchecked fitted gamma likelihood is treated as a conjugate upper
bound. Halving the cutoff closes the tail if a finite lower witness strictly
dominates. At equality, the unsigned bound supplies no face decision.

For h min(v)>=log(2), h delta>=log(4n), and h delta-log(n)-1>=2, put
B(h)=1/(h delta-log(n)-1), W=-log(delta)-1 and R(z)=-log(1-exp(-z)). Then

    P(h) <= W+B(h)*(log(4n+1)+EulerGamma)+R(h min(v)).

All remainders decrease, so evaluation at H bounds every h>=H, including
p->0 and q growing exponentially with h (the moving power endpoint).
The signed improvement from the preceding proof is implemented too. Write
C=log(n)+EulerGamma. If H>=1/min(v), B(H)<exp(-C-1),
log(H)-C+1>0 and

    2 e H exp(-H min(v)) < log(H)-C+1,

then P(h)<W for every h>=H. This follows from
P(h)<=W+R(h min(v))+(e^-1/h)*(C-1-log(h)) and the decreasing ratio in the
last inequality. The implementation retains the weak uniform upper W,
which is a valid supremum bound even though every finite point is lower.
Both tail bounds include the original transformation shift. A strict lower
witness comes from the conditional rectangle's evaluated-likelihood lower
bound. A tail is marked excluded only after a padded strict comparison.
These tail bounds alone do not supply a global supremum bound: compact
coverage is still required. The finite and power-face verifiers below supply
that coverage before their respective successful decisions.

## Work and numerical scope

Corner bisection, interval contraction, halving and doubling each use
`solver_budget(maxit)=min(maxit,100000)`. Nested corner solves retain their
own solver caps. There is no fixed mesh or fixed evaluation cap in these new
pieces. The proposal mesh has at most 240 intervals, metered by the same certificate
allowance. Its coverage is irrelevant to certification: the new certificate
counts its proposal evaluations and independently covers the entire retained
power interval. Increasing maxit enlarges that coverage allowance.

Practical allowances use double epsilon even with extended sums: 128 epsilon
for statistics and inner score operands, 256 for contrasts and tangent scores,
1024 for conditional values and the gamma tangent, 2048 for small-tail
comparisons, and 4096 for large-tail and signed-neighborhood comparisons.
Uncancelled log/digamma magnitudes are included in inner score errors. Lost
positive contrasts, underflowed shape lower bounds and nonfinite arithmetic
leave the corresponding enclosure unavailable. An unavailable conditional enclosure cannot establish a root or accept a fit;
the verifier may still use the uniform gamma envelope to exclude its cell. None of these margins
claims outward rounding or a universal libm error bound.

## Open obligations

The signed quadratic/fourth-order gamma neighborhood and full comparison
are implemented in `gamma_face_certificate.hpp`; see
[review-gamma-expansion.md](review-gamma-expansion.md) for their derivation.
The literal linear power coefficient vanishes for every sample. In squared
power, a strictly negative leading coefficient together with the explicit
remainder can certify the neighborhood. Gamma selection additionally needs
requested coefficient inclusion, exclusion of the large-power tail, and
complete compact coverage. Quadratic-zero/moment-cancellation seams outside
the excluded q≈1 strip still require higher-order control. The known k=1
quartic example is globally excluded by the moving-endpoint power witness
and is not by itself an unresolved winning-gamma example. Unfinished
certificates retain proper legacy results explicitly uncertified; proved
contradictions and failed legacy searches still stop.

## A uniform positive q and gamma-rate lower bound

The exponential member q=1, p=1/h has normalized likelihood -1 at every
power. At a conditional beta maximum the held-q gamma comparison gives
P(h)<=G_q+max(1,q)h. For 0<q<=1,
Gamma(q)>=exp(-1)/q by integrating t^(q-1) exp(-t) over (0,1). Hence

    G_q = q log(q)-q-log Gamma(q)-(q-1)c <= log(q)+1+c.

Since P(h)>=-1, q>=exp(-2-c-h). For q>1 that lower bound is automatic.
It holds at every conditional maximum, without a small-power assumption.
It is wired into the initial q rectangle before interval contraction.
The c allowance includes the uncancelled log(mean(Y)) and mean(log(Y)).

At the conditional p maximum, the decreasing digamma correction gives
h>log(1+q/p), while the trigamma upper bound gives h<=q/p+q/p^2. Thus

    q/expm1(h) < p < q/h+1.

These bounds are also wired into the initial p rectangle. In particular,
for 0<h<=H<=c/2, the previous q upper bound Q=2/c combines with
q0=exp(-2-c-H)>0 to give

    q0 <= q <= Q,
    q0*H/expm1(H) <= p*h <= Q+H.

Moreover p*h-q tends to zero uniformly, since
q*h/expm1(h) < p*h < q+h. The entire small-power conditional maximum
therefore has q and gamma rate in a compact positive rectangle and p
uniformly diverging as h goes to zero. This closes a compactness gap in
constructing uniform gamma-ratio expansions. It does not itself provide a
signed likelihood remainder or settle the quadratic-zero seams. Unlike a
pointwise Stirling expansion, this compactness statement includes joint
shape/power sequences.

## Excluding a whole q strip around the exponential ridge

The held-q=1 ridge is a non-uniqueness counterexample for that constrained
subset, but **not** an all-free maximizer: its normalized likelihood is -1,
whereas W=-log(delta)-1>-1 at the moving-endpoint face. This distinction
matters for using a held-q p/power certificate as an inner problem.

A quantitative global exclusion extends to q around one. For l<=q<=u,
0<l<1<u, uniformly over every h>0 after maximizing p,

    P_q(h) <= -1 + max((1-l)*max(c+1+1/l, R(vmin)-psi(l)),
                       (u-1)*(log(2u+1)+EulerGamma)).

For q>=1, rewrite the objective as log(lambda)-lambda plus
log Gamma(p+q)-log Gamma(p+1)-log Gamma(q)-(q-1)r. The gamma-ratio integral
is bounded by (q-1)log(p+q). When h<=1, r>=c-log(h) and
lambda<=q+h give log(p+q)-r<=log(2q+1). When h>=1, r>=0 and
p<q/h+1 give the same upper bound. Finally
-log Gamma(q)<=EulerGamma*(q-1), by the tangent at one.

For q<=1 and h<=1, psi(p+t)>=log(p)-1/q on t in [q,1], while
r<=c-log(h)+h. Maximizing q log(lambda)-lambda gives an upper bound
Gbase(q)+(1-q)*(c+1+1/q), where
Gbase(q)=q log(q)-q-log Gamma(q)<=Gbase(1)=-1.
For h>=1, r<=R(vmin), psi(p+t)>=psi(q), and Gamma(q)>=1 give
-1+(1-q)*(R(vmin)-psi(q)). All positive factors increase as q decreases
below one. These bounds establish the stated entire-strip upper bound.

The implementation halves the strip width until its upper bound is strictly
below W with a practical margin. The bound converges to -1 as the width
goes to zero, so a strip exists in exact arithmetic for every nonconstant
sample with min(v)>0. Lost floating-point separation leaves it unavailable.
`tail_bounds` records the strip and its total-likelihood upper bound. A
numerical finite proposal in this excluded strip is rejected with structured
finite and moving-power witnesses and an infinite global upper bound. The
usual default proposal lies outside it and is unchanged. This is an actual
fitter decision, not an uncalled mathematical helper. Outside this strip the new certificate uses uniform conditional-p enclosures
on q intervals and a joint beta-profile curvature bound, rather than treating
pointwise held-q fits as uniform interval certificates.

## What successful exclusion of both tails does prove

If both `small_excluded` and `large_excluded` are true for the same finite
witness, an all-free finite global maximum exists. The full conditional
profile is continuous: the beta sufficient statistic stays interior for
each positive h and its strictly concave optimum varies continuously.
On the compact retained h interval, the explicit shape rectangles have
uniform positive lower and finite upper endpoints. Hence the maximum is
attained there, and the excluded tails cannot win. This proves attainment
and covers joint shape/power escapes; it does not prove that the legacy
numerical proposal is that global maximum or establish its coefficient error.
For the frozen sample both exclusions are validated by the appended test.

## Complete compact power coverage for finite winners

Both tails must be strictly below the same feasible witness before a finite
proposal can pass. The retained interval is [L,H], where h=a mean(Y). At a
cell [l,u], sufficient statistics lie in [l,u] x [r(u),r(l)]. The explicit
shape rectangle and monotone inner solves therefore enclose the two-shape
conditional maximizer at every power in the cell. A nonpositive lower
contrast requests subdivision; it is not evidence excluding the cell.

Let D=-r'>0 and K=r''>0, both decreasing. If the rectangle is
[pL,pU] x [qL,qU], the profile score is enclosed by

    smin = 1/u-pU+(qL-1)*(D(u) if qL>=1 else D(l)),
    smax = 1/l-pL+(qU-1)*(D(l) if qU>=1 else D(u)).

A signed score excludes an interior maximum. Integration gives the whole-cell
likelihood upper bound min(Pupper(l)+max(0,smax)*(u-l),
Pupper(u)+max(0,-smin)*(u-l)). A cell below a feasible lower witness is
excluded. Every other cell must be subdivided or certified to contain
exactly one maximum; no unimodality assumption is used globally.

For the conditional beta information J=[A,-T;-T,B], implicit differentiation
of both shape scores yields

    P'' = -1/h^2-(q-1)K + (B-2TD+AD^2)/(AB-T^2).

The diagonal differences A=trigamma(p)-trigamma(p+q) and B with p,q
exchanged decrease in their own shape and increase in the other. T decreases
in both shapes. Corner evaluations bound all three uniformly. A padded
positive determinant lower bound and the nonnegative quadratic numerator
then give a curvature upper bound. The numerator upper uses Bmax+Amax*D(l)^2
-2*Tmin*D(u), with uncancelled-operand margins; a failed positive determinant
bound merely prevents concavity certification. Strict negative curvature
plus independently positive left and negative right scores isolates exactly
one maximum. Ambiguous artificial-boundary roots subdivide or stop, rather
than disappear from the coverage.

Bisection retains independently padded opposite endpoint scores. A rounded
zero only nominates two nearby points; both signs must validate before the
bracket shrinks. Certification requires relative widths below min(tol,0.1)/4 for h, p
and q over the entire retained root interval. Each root has an evaluated
feasible midpoint lower bound and a concave-profile tangent upper bound.
All retained maxima are compared; an overlapping rival bound ends the
verifier as a tie. The public route retains a successful legacy answer
uncertified unless a validated better witness invalidates it. Missing coverage,
sign failures and unavailable arithmetic never certify the numerical mesh.

On a certified finite return the original proposal is retained only when the complete winning rectangle
lies inside its coordinatewise 1+-min(tol/2,1/4) box. Otherwise the already enclosed
root midpoint is evaluated and returned. Thus the frozen coefficients can
remain exactly unchanged while a separate certificate checks their actual
coefficient error. The core's `allfree_certified` is set only after this
entire sequence succeeds. It remains false on stopped searches and uncertified legacy gamma-face statuses;
it is also set by the separately completed moving-power and gamma-face verifiers.

The ledger is 128*min(maxit,100000), including proposal evaluations, every
profile point and popped cell. All inner local solves, root contractions
and root bisections separately have min(maxit,100000) iterations. Exhaustion
terminates the verifier with structured witnesses and an infinite global upper
bound. The public all-free route retains a successful legacy answer
uncertified when that verifier has not proved it inferior.
A failed local proposal may supply an exponential tangent instead; only the
independent rectangle, sign, coverage and inclusion checks can certify the
final fit. This is a separate rescue certificate, not a score exception.

Cell score margins use 2048 epsilon; beta-information comparisons 512;
determinant and numerator margins 1024 and 2048 with uncancelled operands;
curvature comparisons 4096. These complement the conditional and tail
allowances above and include practical normalization error. No outward-rounded
interval claim is made.

All new sufficient-statistic, log-statistic, normalization, Jacobian, D and K
reductions use the existing compensated `genbeta_power_cert::Sum`, including
when extended intermediates are double. The legacy mesh's reductions remain
proposal arithmetic and never supply an unchecked certificate upper bound.
The certificate recomputes its own mean and Jacobian. The retained replication
case checks the coefficient certificate after repeating the frozen sample
20 times, rather than testing only short reductions in extended precision.

## A complete moving-power face certificate

`power_face_certificate.hpp` is called before an all-free PowerBoundary can
return. It does not accept a mesh comparison as a global decision. To track
normalization rounding explicitly, let mu=mean(v), delta=mu-min(v)>0 and
c=log(mu)-mean(log(v)). The normalized likelihood is

    F(h,p,q)=log(h)-p*h*mu-(q-1)*r(h)-log B(p,q).

The fitted moving-power face has value W=-log(delta)-1, endpoint max(x),
and shape 1/mean(Y-min(Y)). The mean gap is summed directly with compensation,
rather than subtracting two rounded means. In original units its likelihood
is n*(-log(mean(Y-min(Y)))-1)-sum(log(x)).

The gamma conjugate upper from the earlier proof is retained with the extra
-log(mu) shift. For h*mu<=c/2 the small-power bound is
Gupper+max(1,2/c)*h*mu. Halving closes that entire tail strictly below a padded
lower bound on W, or stops numerically. Thus a larger or unresolved gamma
face can never be passed as a certified power winner.

For the large-power tail, the conditional p interval is
exp(-1)/(h*mu)<=p<=1/D(h), where D(h)=h*delta-log(n)-1, once the
unsigned-tail thresholds hold. Put C=log(n)+EulerGamma. The earlier proof
gives P(h)<=W+R(h*min(v))+p*(log(p)+C). The last p term is convex, so its
maximum on the entire interval is attained at one of its two endpoints.
This removes the unnecessary requirement that it decrease everywhere.

Choose H satisfying the unsigned-tail thresholds, H>=1/min(v), and

    D(H)>delta/min(v),
    a=log(H*mu)-C+1>0, b=log(D(H))-C>0,
    2*exp(1)*H*mu*exp(-H*min(v)) < a,
    2*D(H)*exp(-H*min(v)) < b.

Then both endpoint corrections dominate R, so P(h)<W for all h>=H.
The first ratio decreases because H>=1/min(v). The second has logarithmic
derivative at most delta/D(h)-min(v)<=0. Both positive right-hand sides
remain positive as h increases. These are whole-neighborhood comparisons,
validated with practical margins, rather than a test at one large power.

### Eliminating an enormous q analytically

The interval from moderate power to the signed cutoff can have exponentially
large conditional q. The following bound avoids computing q entirely.
For r(h)<1, the digamma integral gives

    log Gamma(q+p)-log Gamma(q) <= p*log(q+p).

The maximum of p*log(q+p)-q*r over all q>0 is
p*log(p/r)-p+p*r. Therefore, for **all** p,q, not just stationary shapes,

    F(h,p,q) <= log(h)+r + M(p; A),
    M(p; A)=p*log(p)-p-log Gamma(p)-A*p,
    A=h*mu+log(r)-r.

For A>0, M is strictly concave, its score is alpha(p)-A, and its unique
maximum is in [1/(2*A),1/A]. Analytic brackets, padded score bisection and a
concave tangent over the retained bracket supply an upper bound on sup M.
No unchecked scalar iterate becomes an upper bound.

On a whole cell [l,u], r decreases, so
A>=l*mu+log(r(u))-r(l). Since sup M decreases with A,

    P(h) <= log(u)+r(l)+sup_p M(p; l*mu+log(r(u))-r(l)).

This is a whole-cell envelope. It includes p->0/q->infinity moving-endpoint
paths and the intersections of those paths, without any representable upper
q being needed. The proof uses no asymptotic truncation of Gamma ratios.

The implementation sorts v once and computes log(r) by a compensated
log-sum-exp. A sorted omitted tail is bounded analytically: R(t)<=2*exp(-t)
and R(tmin)>=exp(-tmin), so at most n remaining terms contribute a ratio
below 2*n*exp(-(t-tmin)). Once t>40 and

    t-tmin > log(n)+log(4/epsilon),

the omitted sum is below epsilon/2 of the leading term. This is an error
bound on a data reduction, not a heuristic search cap. For t>40,
log(-log(1-exp(-t))) is replaced by -t; its omitted correction is below
2.2e-18, covered by the stated rounding margins. Thus underflow in r itself
cannot fabricate a zero sufficient statistic for this envelope.

### Compact coverage and return contract

Where the q-eliminated envelope is loose, a beta-shape rectangle encloses
the actual conditional maximizer at every power in the cell. Write
K(p,q)=-p*l*mu-q*r(u)-log B(p,q). Since

    F(h,p*,q*) <= log(u)+r(l)+K(p*,q*),

a concave tangent to K, maximized over that actual-maximizer rectangle,
is another whole-cell upper bound. The tangent point is feasible and need
not solve K. In particular this does **not** presume that the fictional
corner statistics have the same maximizer as any real power. A failed
contrast requests subdivision, not exclusion.

The ledger excludes a cell only when the better of these two padded upper
bounds is below the padded lower W witness. Every remaining cell splits.
Only an empty queue together with both validated neighborhoods returns
PowerBoundary with `allfree_certified=true`. Exhaustion and arithmetic
failure terminate this verifier; it retains the Power candidate, clears selected
limit fields, and reports an infinite global upper bound. The public route
retains the original proper PowerBoundary payload uncertified if it has not
been contradicted. A proved finite improvement still invalidates that proposal.

Successful global supremum bounds are the closed-form power likelihood plus
or minus its practical evaluation allowance and are transported through the
binding-neutral stop payload. No R or JS adapter refits a face. The same
128*min(maxit,100000) ledger counts proposal evaluations, popped cells and
new r evaluations; all inner scalar solves have min(maxit,100000) iterations.

The additional allowances are 512 epsilon for beta cell statistics, 1024
for the q-eliminated A, 2048 for tangent values, and 8192 for comparisons,
including an inverse-delta normalization allowance near concentration. These
are conditional exact-arithmetic certificates with practical floating-point
margins, not outward-rounded interval arithmetic.

## Uniform quadratic small-power remainder

This bound is also wired into `tails` and into whole-cell pruning by the
finite verifier. It is unsigned, so it cannot select a winning gamma face.
Allow for normalization with mu=mean(v), c=log(mu)-mean(log(v)) and
m2=mean(v^2). Put s=h*mu and T(z)=log(sinh(z/2)/(z/2)). Direct
integration of the coth bound gives 0<=T(z)<=z^2/24, and
T''(z)=1/z^2-1/(4*sinh(z/2)^2)>0. Thus the exact contrast is

    cq = c-mean(T(h*v))+T(h*mu),
    c-h^2*m2/24 <= cq <= c.

The beta score identities, p>q/expm1(s), and alpha(q)>1/(2q) give

    alpha(q) < cq+(1-exp(-s))/q,
    q > (exp(-s)-1/2)/cq                 (s<log(2)).

For the first inequality use alpha(p+q)<1/(p+q) and
log(1+p/q)-log(1/(1-exp(-s)))>0 in the q score identity.
Consequently for 0<s<=A<log(2), the following are uniform over the
conditional maximum:

    qlo=(exp(-A)-1/2)/c <= q,
    q <= qhi=1/(c-A^2*m2/(24*mu^2)).

The implementation takes A no greater than c/2, log(4/3),
log(1+1/(4*c)), and sqrt(12*c*mu^2/m2), using padded c endpoints.
This ensures qlo>0, qhi finite, and p>=1. It is a mathematical domain
bound, not a search cap.

Center the gamma rate at nu=h*(p+(q-1)/2), and write t=nu/h. The exact
likelihood decomposition is

    F(h,p,q) = GammaLogLik(q,nu;v) + Ac(t,q) + (q-1)*mean(T(h*v)),
    Ac(t,q) = logGamma(t+(q+1)/2)-logGamma(t+(1-q)/2)-q*log(t).

For z>0, psi(z+1/2)-log(z)<=1/(8*z^2): use
psi(z+1/2)<log(z+1/2)-1/(2*(z+1/2)) and
log(1+u)-u/(1+u)<=u^2/2. Integrate over the symmetric interval
[t-q/2,t+q/2], and apply Jensen to its log term. Since its lower endpoint
is p-1/2>0,

    Ac(t,q) <= q/(8*(p-1/2)^2).

Put rho=expm1(A)/A and f=1-expm1(A)/(2*qlo)>0. The uniform p lower
bound implies p-1/2>=q*f/expm1(s), whence

    P(h) <= Gupper + C*h^2,
    C = mu^2*rho^2/(8*qlo*f^2) + max(0,qhi-1)*m2/24,
    0<h<=A/mu.

Both the linear and quadratic gamma envelopes are valid simultaneously;
the verifier uses their minimum wherever each domain applies. A cell can
be pruned by this bound even if its beta-shape rectangle is unavailable.
A failed local shape proposal may instead use the validated rectangle
midpoint as its feasible tangent point. Neither operation accepts a
failed optimizer. The quadratic coefficient carries an additional
4096-epsilon allowance, besides the padded statistics and gamma upper.

In a cell with strictly negative curvature, a negative score at the left
endpoint, or a positive score at the right,
also excludes a maximum throughout that cell. The implementation uses
these endpoint exclusions in addition to the whole-cell score rectangle.

### Remaining practical obstruction near the gamma face

The deterministic n=300 quantile fixture with generating shapes (3,2,.7)
illustrates a remaining coverage-budget obstruction. The old finite proposal
has coefficients approximately (5.364578015,1.992435522,.411183681) and
log likelihood 38.8234083774. The new unsigned tail bound excludes
h<=0.00213023, but the independent rectangular profile score enclosure is
loose close to that cutoff. The default ledger reaches h about .116 before
its 38400 evaluations are consumed, without isolating a maximum. The public result
is the legacy finite Ok with `allfree_certified=false` and Budget as its
certificate diagnostic, together with an infinite global upper bound. This
does not prove that the finite proposal is an MLE,
or that the gamma face wins. Raising the work allowance or deriving a
correlated/centered score enclosure could improve this case; no certificate
accuracy or comparison threshold is weakened to make it pass.

A partial finite verifier invalidates a successful legacy point only when a
validated witness beats its likelihood beyond both the practical evaluation
allowance and the retained legacy comparison tolerance 1e-8*n. A proved
contradiction has certificate diagnostic LegacyContradicted and clears the
budget/tie stop flags, so it is not presented as an optional budget refusal.
Gamma or power-face contradictions compare against validated face upper
bounds. Failed legacy searches keep their original failure handling.
