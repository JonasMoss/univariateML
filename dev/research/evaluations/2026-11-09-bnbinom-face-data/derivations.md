# Signed NB and Poisson exterior bounds

These arguments concern the likelihood supremum, not the validity of local
optimizer convergence. Every claim below is in exact arithmetic, conditional
on the computed strict comparisons with the practical floating-point margins
listed at the end. Numerical validation is separate evidence.

## A uniform log-gamma remainder

For positive z,h define F(z,h)=log Gamma(z+h)-log Gamma(z)-h log z.
The digamma integral and

    0 < 1/(1-exp(-t)) - 1/t - 1/2 < t/12

imply the next bound. For completeness, put u=t/2. The lower elementary
inequality is u cosh(u)>sinh(u); its difference has derivative u sinh(u)>0.
The upper one is (3+u^2)sinh(u)>3u cosh(u); the difference has derivative
u[u cosh(u)-sinh(u)]>0. Both differences vanish at zero.
These prove 0<coth(u)-1/u<u/3 and hence the displayed kernel bounds.

    |psi(z)-log z+1/(2z)| <= 1/(12z^2).

Integrate from z to z+h, using

    |log(1+u/z)-u/z| <= u^2/(2z^2),
    |1/(z+u)-1/z| <= u/z^2.

Consequently, for all z,h>0 without a large-z restriction,

    |F(z,h)-h(h-1)/(2z)| <= H(h)/z^2,
    H(h)=h^3/6+h^2/4+h/12.

This is an explicit bound, rather than an unbounded asymptotic O symbol.

## NB face, including both labels

Write a=alpha, b=beta, s=a+b, p=a/s, q=1-p. The exact identity is

    log f_BNB(x;r,sp,sq) - log f_NB(x;r,p)
      = F(sp,r) + F(sq,x) - F(s,r+x).

Thus ell_BNB-ell_NB=C(r,p)/s+E, where

    C(r,p) = [n r(r-1) q/p + F2 p/q - 2r sum x]/2,
    |E| <= R(r,p)/s^2,
    R(r,p) = sum [H(r)/p^2 + H(x)/q^2 + H(r+x)].

On a rectangle r in [rl,rh], p in [pl,ph], replace H(r) and H(r+x)
by H(rh) and H(rh+x), and the denominators by pl^2 and (1-ph)^2.
For an upper bound on C use the maximum of rl(rl-1),rh(rh-1), multiplied
by (1-pl)/pl if that maximum is nonnegative and (1-ph)/ph otherwise.
Replace p/q by ph/(1-ph), and -2r sum x by -2rl sum x.
This is the implemented signed coefficient bound.

At the NB MLE p*=r*/(r*+mean), c1*=p*C(r*,p*) is the coefficient of 1/a.
Direct differentiation of r log p+x log(1-p) gives

    c1* = [n p*^2 (v-mean)/(1-p*) - n r* (1-p*)]/2.

Equivalently, in the requested alpha coordinate,

    ell_BNB-ell_NB = c1(r,p)/a+E,
    c1(r,p)=p C(r,p),   |E|<=p^2 R(r,p)/a^2.

On the rectangle above, ph^2 times its explicit R bound is therefore a
uniform quadratic remainder constant even while r,p vary with a.

No envelope approximation is needed in the certificate. The inequality
bounds every (r,p), including the displacement of the optimized profile.

Here is the uniform-tail argument with explicit constants. On a compact
rectangle containing the unique NB maximizer, choose a neighbourhood N
where the rectangle upper bound on C is at most -kappa<0. Let Rmax be
the rectangle remainder bound, Dmax an upper bound on |C|, and g>0 a
lower bound on the NB profile deficit on the compact complement of N.
Then it suffices to take

    S >= max(2 Rmax/kappa, 2 Dmax/g, sqrt(4 Rmax/g)).

On N, C/s+Rmax/s^2<0 for s>S. On its complement the increment is less
than g, so the NB deficit dominates it. In implementation the existing
absolute first-order remainder Cabs/s can replace the last two terms
by 2 Cabs/g. Bounds are local to rectangles, which avoids one unusably
large global remainder. A finite cover of the compact rectangle gives
an explicit common S as the maximum of its cutoffs. Since s>=a, a>=S
suffices. Thus this is uniform over all relevant r,p, not a pathwise
expansion holding those coordinates fixed.

The compact region is effective. A positive-count sample and an attained
NB witness exclude small r and b by f_x <= (r)_x/x! and its reflection.
The mixed-Poisson exterior bound below excludes both r,b large when NB
has a strict advantage over Poisson. Order r<=b. This bounds r in a
positive finite interval. The Jensen and Markov bounds below exclude
extreme p. Outside these regions the likelihood is already below the
attained NB witness, uniformly in a. Interchanging r and b is an exact
identity of BNB probability masses, so the proof covers both face labels
without requiring one labeling to remain bounded along the path.

## Poisson corner

Put lambda=rb/a and h=1/r+1/b. The identity

    log f_0 = - integral_0^r integral_0^b psi1(a+u+v) dv du

and psi1(z)>=1/z>=1/a-(u+v)/a^2 give

    log f_0 <= -lambda + lambda^2 h/2.

For an integer count x, log(1+y)<=y and log(1+y)>=y-y^2/2 yield

    log (r)_x - x log r <= x(x-1)/(2r),
    log (b)_x - x log b <= x(x-1)/(2b),
    -sum_{j=0}^{x-1} log(1+(r+b+j)/a)
      <= -x lambda h + 1/2 sum_{j=0}^{x-1} ((r+b+j)/a)^2.

A negative term -x(x-1)/(2a) was discarded in the last inequality.
Since 1/(rb)<=h^2/4, for h<=h0 the last positive remainder is at most

    (lambda^2 h^2/2) x [1+(x-1)_+ h0/4]^2.

Summing proves the global one-sided bound

    ell_BNB <= ell_Pois(lambda)+B(lambda)h+R(lambda,h0)h^2,
    B(lambda)=n [(lambda-mean)^2+v-mean]/2,
    R(lambda,h0)=lambda^2/2 sum x [1+(x-1)_+ h0/4]^2.

It holds for every positive r,a,b with h<=h0; h0 need not be small.
The coefficient at lambda=mean is strictly negative when v<mean.
Around that mean the signed term excludes h sufficiently small. Off the
neighbourhood, the strict Poisson deficit dominates the two remainders.
The compact interval lambda<=2M follows from the existing effective
mass bound m=rb/(a+r+b)<M when r,b>=4M. A one-dimensional interval
cover computes a sufficient U such that r,b>=U are excluded. For each
lambda interval the code uses h0=1, a coefficient upper bound BU and
R at its upper endpoint. If BU<0, h<=min(1,-BU/[2(1+R)]) suffices.
Otherwise, with Poisson deficit g>0,
h<=min(1,g/[4(1+abs(BU)+R)]) suffices. U is at least 4M and 2/h for
every terminal interval. This proves a finite U for strictly negative,
resolved dispersion. Equality or a sign inside its arithmetic margin
returns no proof. Local versions of the same bound also prune boxes
with r below this deliberately conservative U.

For a Poisson winner the remaining compact NB face has a strict deficit
at every finite size. The certified underdispersed NB profile is monotone,
so evaluating its upper size endpoint bounds an entire interval. The
same NB concentration bound therefore closes that remaining tail without
requiring an NB maximizer at a finite size.

## Compact stationary cover

An attained face point with likelihood L supplies a lower comparison
witness. For an NB winner this need not be the exact NB maximizer:
using an attained point minus its evaluation error only weakens exclusions.
The NB supremum itself supplies the upper bound for signed-tail boxes.

Small r and b are excluded by the rising-factorial mass bound. For
m=rb/(a+r+b), the inherited bound

    log f_x <= -m+log (m)_x-log x!

excludes m>=M once M>mean and the bound at M is below L.
The inherited unsigned mixed-Poisson bound gives a finite upper bound on
min(r,b) whenever L exceeds the Poisson supremum. For a Poisson winner,
the signed Poisson argument above supplies that bound instead.

For r>=delta, 0<delta<=1, stochastic ordering of NB laws in size gives

    Pr_NB(r,P)(X=x) <= Pr_NB(delta,P)(X<=x)
      <= P^delta (delta+1)_x/x!.

Taking expectations under the beta law and Jensen gives the uniform bound
f_x <= p^delta (delta+1)_x/x!. Also
Pr(X>0)<=max(r,1)(1-p). These yield effective positive lower bounds
on p and q once r<=U. The cover reapplies them with each box's local
size endpoints. Probabilities are capped by one before logs are summed.

The NB upper bound optimizes p analytically. The certified NB profile is
increasing up to its unique certified root and decreasing after it (or
increasing to Poisson for underdispersed data). The delegated root enclosure
has relative radius below 1e-6. Its height error is retained. On an r interval,
the best profile endpoint, or the root enclosure when it intersects the
interval, is an upper bound. The exact probability deficit is

    ell_NB_profile(r)-ell_NB(r,p)
      =n(r+mean) KL(Bernoulli(r/(r+mean)) || Bernoulli(p)).

Pinsker's inequality bounds this below by twice n(r+mean) times the
squared distance between the two intervals of probabilities.

Order r<=b and cover r in [lower,U], logit(p) in a finite interval,
t=1/(a+b) in [0,1/lower]. The boundary t=0 has NB likelihood at most
the fitted NB (or Poisson) supremum. The signed expansion or the NB
profile deficit excludes a neighbourhood of that boundary. The
`concentration_upper` diagnostic records a common sufficient cutoff
from all terminal, t=0-touching boxes needing concentration exclusions;
uniform probability exclusions need no concentration cutoff.

For t>0 a box maps into the ordinary positive (r,a,b) box

    [rl,rh] x [pl/th,ph/tl] x [(1-ph)/th,(1-pl)/tl].

The existing monotone-coordinate score exclusions and full Hessian
stationary-point mapping apply. A mapping reduction may narrow this box;
its inverse image conservatively narrows the compactified box too.
An additional Taylor upper bound integrates the interval Hessian along
a segment from the geometric center. It is used only to exclude possible
stationary maxima whose likelihood is below the face witness.

If any finite point beat the face supremum, the compactified superlevel
would contain a global maximum away from t=0 and the excluded exterior.
It would be an interior stationary point in the original positive
parameters. A completed cover excludes every such point, a contradiction.
This proves a global face supremum. Finite BNB parameters do not attain
that face: the same cover excludes stationary attainment, and attainment
would also be a global interior maximum. An unresolved box, arithmetic
range loss or exhausted allowance supplies no face proof.

## Arithmetic and work

The proof is conditional exact arithmetic with practical margins, not
outward-rounded interval arithmetic. New signed coefficients and moments
use 4096 times double epsilon times accumulated absolute terms;
positive remainders are inflated by 1+4096 epsilon. Inherited likelihood
margins include 1e-10 n (1+abs(ell/n)); Hessian, score, inverse mapping
and delegated NB height margins are retained. The Taylor sum receives
an additional inherited likelihood margin. Root and sign comparisons
must be strict after their margins. Nonfinite bounds never exclude a box.

Each signed cover, including exterior cutoffs, spends at most
128 min(iterlim,100000) certificate evaluations; no fixed cutoff replaces
that allowance. Internal dimension and support loops are finite sums,
not iterative convergence criteria. The enclosing fitter changes no
legacy result on proof failure and creates no certificate-budget stop.

`high-precision.py` independently evaluates log-gamma masses at 90 digits,
checks the general F bound and both likelihood remainders, verifies exact
reflection, and computes the NB signed coefficient in two ways. Those
finite grids validate formulas and their implementation ingredients; they
are not substitutes for the uniform arguments above.
