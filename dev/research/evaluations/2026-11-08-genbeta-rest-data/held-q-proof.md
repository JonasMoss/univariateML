# Held-q, free-p/free-power certificate

This is a conditional exact-arithmetic proof for `free_p_certificate.hpp`.
It does not establish a certificate for the unrestricted three-shape fit.
Write Y=log(b/x), m=mean(Y), v=Y/m, h=a*m, g=mean(log(v)),
r=-mean(log(1-exp(-h*v))), D=-r', K=r''. All v are positive and
mean(v)=1. The normalized per-observation likelihood is

    F(h,p)=log(h)-p*h-(q-1)*r(h)-log B(p,q).

The removed constant is -log(m)+mean(-log(x)). q>0 is held, q!=1.
The q=1 ridge uses the existing exact identified-product route.

## Conditional shape and its inclusion

The p score S=psi(p+q)-psi(p)-h is strictly decreasing, with limits
+infinity at zero and -h at infinity. Its unique root is the global
conditional maximizer. J=trigamma(p)-trigamma(p+q)>0 decreases in p,
and J>=q/[p*(p+q)], by the integral trigamma bound. Also
psi(p+q)-psi(p)<=q/p+q/p^2, so max(1,2q/h) brackets it above.
Bisection only proposes a p. Independent opposite score signs at p*(1+-rho)
validate its enclosure; rho>=4e-13 is expanded to four times the score
rounding allowance divided by a lower bound for p*J. If rho reaches .01,
validation fails. No rounded zero certifies a root. Inner solves have
min(maxit,100000) iterations. Implicit differentiation gives p'(h)=-1/J.

For fixed h, concavity gives an upper bound on the conditional maximum:
evaluated F plus |S| times the p enclosure width. A padded evaluated F is
a lower bound. These conditional value bounds are used in every global
comparison, rather than treating a fitted likelihood as an upper bound.

## Uniform tails

The previous job's [remaining-profile bounds](../2026-11-07-genbeta-cert-data/remaining-profile-bounds.md)
prove, uniformly over conditional shape maximizers,

    P_q(h)<=G_q+max(1,q)*h,
    G_q=q*log(q)-q-log Gamma(q)+(q-1)*g.

Here G_q is the gamma face **retaining held q**, with rate q in v units.
A finite lower witness strictly above this bound at L excludes every h<=L.
The implementation halves L until exclusion validates, or stops with
`GammaNeighborhood`. The unsigned bound cannot select a winning gamma face.
It does not infer a global decision from the quadratic seam coefficient.

For large power, p<=(q+1)/h. Define A=max(0,psi(q)-psi(1)). Monotonicity
of psi(p+q)-psi(1+p), integrated from zero to p, gives

    log Gamma(p+q)-log Gamma(q)-log Gamma(1+p)<=p*A.

Writing lambda=p*h and maximizing log(lambda)-lambda gives

    P_q(h)<=-1+A*(q+1)/h+max(0,1-q)*R(h*vmin),
    R(z)=-log(1-exp(-z)).

Both remainder terms decrease. A finite lower witness above this upper
bound at H excludes every h>=H. Otherwise doubling H ends in a numerical
`PowerNeighborhood` stop or arithmetic failure. The implementation does not
select an unproved face. Tail loops have min(maxit,100000) iterations.

For q>1, the fixed-endpoint power face can never be a global maximum:
take p=1/h. The gamma-ratio expansion is

    log Gamma(p+q)-log Gamma(q)-log Gamma(1+p)
      = p*(psi(q)-psi(1))+O(p^2),

whose linear coefficient is strictly positive. The data term
(q-1)*mean(log(1-exp(-h*v))) is exponentially small as h tends to infinity.
Thus F(h,1/h)>-1 for sufficiently large finite h. This statement covers
constant samples too. It does not settle whether the gamma face wins.
For a concrete strict improvement, q=2, v=(.5,1.5), h=100, p=.01 gives
F=-1+log(1.01)+mean(log(1-exp(-100*v)))>-1+.009.

The signed large-power q<1 bound from the previous job is not needed to
certify finite winners and is not used to select a face here.

## Complete compact coverage

D and K decrease in h. p decreases and J increases along the conditional
profile. The envelope derivatives are

    P'=1/h-p+(q-1)*D,
    P''=-1/h^2+1/J-(q-1)*K.

On a cell [L,H], endpoint conditional enclosures pL and pH give the score
bounds

    smin=1/H-pLhi+(q-1)*(D(H) if q>=1 else D(L)),
    smax=1/L-pHlo+(q-1)*(D(L) if q>=1 else D(H)).

The curvature bounds are

    cmax=-1/H^2+1/J(pLhi)-(q-1)*(K(H) if q>=1 else K(L)),
    cmin=-1/L^2+1/J(pHlo)-(q-1)*(K(L) if q>=1 else K(H)).

Integration of the score enclosure gives another whole-cell upper bound:

    min(Pupper(L)+max(0,smax)*(H-L),
        Pupper(H)+max(0,-smin)*(H-L)).

Cells below a feasible witness are excluded. Signed score cells exclude
interior maxima, positive-curvature cells exclude maxima, and strictly
negative curvature plus padded opposite endpoint scores isolates exactly
one maximum. All other cells subdivide. An uncertain artificial-boundary
root stops numerically instead of disappearing from coverage. A global
maximum cannot be on the outside endpoints, because the excluded tails
are strictly below a retained feasible witness.

Isolated roots are bisected until both (H-L)/L and
(pLhi-pHlo)/pHlo are below min(tol,1e-9)/2. The returned midpoint power
and conditional shape lie in those enclosures. A concave-profile tangent
bounds each root likelihood above by its midpoint conditional upper plus
its midpoint absolute score bound times the retained power width. The
winner must have a lower likelihood bound above every rival upper bound;
overlap is a comparison-tie stop. No global unimodality or uniqueness
assumption is made. A degenerate root may end in a numerical or budget stop.

Every profile evaluation and popped cell consumes the shared
128*min(maxit,100000) certificate ledger. Exhaustion is terminal with a
budget detail. Solver iterations are separately capped at min(maxit,100000).
Proposals only improve the finite lower witness; they do not replace coverage.
Stopped searches retain finite feasible witnesses in (p,q,a,b) order and
an infinite global upper bound. A stopped witness is never returned as Ok.

## Floating-point scope

This uses the package's exact-arithmetic certificate conditional on computed
comparisons, with practical floating-point margins, not outward-rounded
intervals. Score allowances are 64 epsilon times absolute operands, including
uncancelled log/digamma operands below the asymptotic seam. Conditional
trigamma differences use 128 epsilon times absolute operands at each endpoint.
Profile scores use 256 epsilon; objective and cell comparisons 512 epsilon;
curvature comparisons 1024 epsilon. Compensated extended sums support the
no-long-double build too. Rounding allowances are not universal libm error
bounds. Near the gamma seam, or nearly flat power roots, they can prevent
inclusion; the result is an explicit stop.

## Non-uniqueness and limits of the result

The attained q=1 ridge remains a counterexample to general uniqueness:
Y=(1/6,1/2) is maximized by every p*a=3, including (p,a)=(1,3),(3,1).
No non-ridge uniqueness theorem is claimed by this interval enumeration.
The all-free conditional beta problem and its moving-endpoint and gamma
intersections are outside this implementation. The existing all-free
numerical route is unchanged, as required for the default fit and goldens;
its successful estimates and selected faces remain explicitly uncertified.
