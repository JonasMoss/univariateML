# Mathematical checks and acceptance rules

Write p for the smaller tail, q=1-p, s=log(p),
h=(q^lambda-p^lambda)/lambda and D=p^(lambda-1)+q^(lambda-1).
Then h_s=-pD and h_ss=-pD-p²D_p. In the endpoint regime use
log(d), where d=p^lambda+1-q^lambda, with d_s=lambda pD.
The two log equations have second derivative G_s(1+pD_p/D-G_s).
These identities give Halley without numerical differentiation. The centre
uses h-z directly; the tail uses log(h)-log(z); the deficit uses
log(d)-log(d_target). All have a known strict derivative sign.

The initial analytic s bracket is [L,-log(2)]. For lambda=-a<0,
p^-a=a z+q^-a <= a z+2^a, so L=-log(a z+2^a)/a.
For 0<lambda<1, d<=2 p^lambda, so L=(log(d)-log(2))/lambda.
For lambda>1, d<= (1+lambda)p, so L=log(d)-log(1+lambda).
The endpoint deficit supplied by the affine support residual is authoritative;
z can have rounded to the endpoint. These are exact-arithmetic bounds.
The computed lower endpoint receives 64 epsilon*(1+abs(L)) padding.

A Halley iterate is only a proposal. A nonfinite step or one outside the
current sign bracket is replaced by its midpoint. Acceptance requires
abs(G/G_s)<=32 epsilon*(1+abs(s)) and computed opposite signs at
s +/- 64 epsilon*(1+abs(s)), clipped to the retained bracket. Thus the
returned iterate has an explicitly checked root enclosure of that width,
conditional on the computed signs. This is the package's practical floating
point comparison standard, not outward-rounded interval arithmetic. The
log-density sensitivity is bounded by abs(lambda-1) times the s error,
since abs(d log(D)/ds)<=abs(lambda-1) on p<=1/2. Exact logistic and the
stable quadratic at lambda=-1 bypass iteration. At 1 and 2 the inverse
is log(p)=log(d)-log(2).

The table contains 33*65 values of s+eta; it only affects the proposal.
Its fixed dimensions and first-use construction are evaluator initialization,
not a fitted parameter search. A failed table-node solve uses the crude seed;
the caller still has to verify its root. Fitting inverse iterations, inner
solves, damping, backtracking, profile root refinement, cutoff expansion and
shape-derivative series all use the caller's solver budget. No search uses
a numerical shape floor that iterlim cannot remove.

For fixed 0<=lambda<1, the likelihood is concave in A=1/sigma,
B=-mu/sigma on its convex support domain, as proved in the existing theory.
Holding mu imposes B=-mu*A; holding sigma fixes A. Each Newton system is
preconditioned by the exact local affine frame dA=A*u, dB=B*u-v. Its scores
are n+sum(z*psi), -sum(psi), and its Hessian entries are
-n+sum(z²*psi'), -sum(z*psi'), sum(psi'). These sums avoid subtracting large
entries of the unscaled A,B Hessian. The exact induced relative scale and
location-in-fitted-scale corrections are -u/(1+u) and v/(1+u). The curvature
uses the expanded log-power identity already used in the certificate code;
for extreme tails, multiplication by z and z² occurs in log space before
underflow can discard information. Shape scores use scaled power ratios,
so a finite log density does not require constructing D itself. Data are
centred at the median or held location. FMA reconstruction and a conservative
coordinate-rounding bound stop when conversion cannot meet tol.

An undamped positive
information matrix and componentwise Newton error estimate are required.
Recent full corrections must show quadratic contraction (or both lie at the
requested numerical floor). Location corrections use the fitted scale as the
natural zero floor. The target is min(.05*tol,1e-12). Negative inner searches
use the same local acceptance after multistart; this does not establish globality.
A failed inner start with a materially larger finite likelihood invalidates
the competing inner proposal. A shared ledger also rejects the final profile
result if any failed start had a materially better finite feasible likelihood;
failed starts are not silently dropped between shape grid points.

The envelope theorem gives the shape derivative at a stationary inner fit:
-D_lambda/D+D_u Q_lambda/D². A series avoids cancellation near lambda=0.
Illinois refinement keeps opposite profile derivative signs, with bisection
if interpolation leaves the interval. Acceptance contracts shape width below
min(.01*tol,1e-13)*max(.001,abs(lambda)), and also contracts the free nuisance
variation between bracket endpoints below .1*tol in their own scales.
A computed zero derivative alone cannot terminate the refinement. The
lambda=-1 candidate must satisfy the one-sided score condition. A feasible
grid point materially above the accepted stationary/face candidates causes
failure, rather than returning a known inferior candidate. A finite shape
grid and local negative inner searches remain uncertified globally.

At held mu with no tied observation, the theory's scale-independent bound
|x_i-mu| f_i < 1/(2a), lambda=-a<0, implies
ell < -n log(2a)-sum(log(abs(x_i-mu))). If ell0 is any feasible incumbent,
a > exp((-ell0-sum(log(abs(x_i-mu))))/n)/2 excludes all improvements.
The implementation expands the negative grid to this data-dependent cutoff,
with a practical 1e-12 relative outward margin. Nonfinite cutoffs or exhausted
expansion stop numerically. No scale-concavity claim is needed.
