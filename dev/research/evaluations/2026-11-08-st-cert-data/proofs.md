# Conditional skew-t certificates

These arguments concern plain likelihood with proper, finite held coefficients.
They establish exact-arithmetic bounds. Computed special functions and the
128-epsilon term-sum margins are practical floating-point conditions, not
outward-rounded interval arithmetic.

## Location, with scale, slant and degrees of freedom held

Put m=nu+1, z=(x-xi)/omega, h=alpha z sqrt(m/(nu+z²)),
R(h)=t_m(h)/T_m(h). The density's z log derivative is

    g(z)=-m z/(nu+z²) + R(h) alpha nu sqrt(m)/(nu+z²)^(3/2).

For alpha>0 and z<0 both terms are positive. For z>=0, h>=0,
T_m(h)>=1/2 and t_m(h)<=t_m(0)=c. Thus g(z)<0 whenever
z sqrt(nu+z²)>2c alpha nu/sqrt(m). In particular this holds for
z>=sqrt(4c alpha nu/sqrt(m))>0. For every observation the location
score has the same strict sign outside the interval used by location_fit.
Reflection handles alpha<0; alpha=0 has its usual [min(x),max(x)] interval.
The likelihood tends to zero as location escapes, so all global maxima
are in this finite interval.

On any location interval, each residual has a closed interval. Exact extrema
of z/(nu+z²) are its endpoints and +/-sqrt(nu) when contained. The function
h(z) is monotone. Its endpoint interval gives positive ratio bounds using
pdf unimodality and CDF monotonicity. The remaining derivative multiplier
|alpha| nu sqrt(m)/(nu+z²)^(3/2) decreases in |z|. Multiplication of these
positive bounds and the sign of alpha encloses -g(z). Summation encloses
the entire location score. Intervals whose padded range excludes zero
cannot contain an interior global maximum. Every retained stationary
component is narrowed and compared with likelihood remainder bounds by the
location-specific scalar enumerator, retaining connected-component hulls
to public location width 4e-11 (1+abs(xi)). An unresolved component, competing
likelihood enclosures or exhausted budget stops, preserving witnesses.

Neither uniqueness nor concavity is used. For observations -2,2, held
omega=nu=1 and alpha=0, maximizing likelihood minimizes
((xi+2)^2+1)((xi-2)^2+1)=xi^4-6xi²+25.
The global minima are exactly +/-sqrt(3), disproving conditional uniqueness.

There is also a continuum counterexample when both location and scale are
free. For data -1,1, held nu=1 and alpha=0, the likelihood is

    omega² / (pi² [4 omega² + (xi²+omega²-1)²]) <= 1/(4 pi²).

Every point xi²+omega²=1 with omega>0 attains equality. The two collapse
endpoints have the same limiting likelihood, but finite global maximizers
exist all along the open semicircle. Thus an equality collapse exponent
cannot establish nonexistence for the joint location/scale problem. This
counterexample concerns an open joint subset; it is not a new fitter route.

## Symmetric scale and one-sign joint slant/scale

At alpha=0 and held xi,nu, the score with respect to log omega is

    S(omega)=-n+(nu+1) sum d²/(nu omega²+d²).

If any residual is nonzero, S is strictly decreasing in log omega. Its
limits at zero and infinity are e=nu(n-k)-k and -n. For e>0 a unique root
exists and is the global maximum, with likelihood tending to zero at both
scale ends. Whole exterior half-lines are excluded by monotonicity once
padded endpoint scores have opposite signs. Bisection retains those signs;
a sign-uncertain point is accepted only if two nearby sign-certain points
bracket it to the required width. Solver exhaustion is numerical failure,
not permission to return the last iterate.

For e<0 the likelihood diverges as scale tends to zero. For e=0 its
finite-scale score is strictly negative (each nonzero residual's summand
is strictly below one), so the finite supremum is unattained. Its log value is

    n log t_nu(0) + (nu+1)(n-k) log(nu)/2
      - (nu+1) sum_nonzero log |d|.

All-zero data are unbounded. These conclusions hold for every positive
finite held nu, without the free-nu restriction.

If all nonzero d have the same sign, every finite-scale likelihood is
strictly monotone in alpha toward that sign's infinity. Its pointwise
supremum is the symmetric likelihood times 2^(n-k). For any finite scale,
this is a strict upper bound. Taking a scale that maximizes the symmetric
objective and then alpha to infinity attains that supremum as a limit;
therefore the sup over both free coordinates equals that one-dimensional
supremum times 2^(n-k). At equality, a diagonal sequence of decreasing
scale and increasing alpha gives the corresponding collapse supremum.
An observation at held xi always contributes its symmetric density, not
the closed half-t endpoint factor two. Reflection preserves the held xi in
original coordinates. No analogous monotonicity argument holds for
mixed-sign residuals.

## Explicit implementation gaps

For location/slant with scale and nu held, compact location alone does not
give a uniform finite slant bracket across xi=x_i: residual signs change
there, the conditional bracket diverges and moving-boundary half-t layers
have a factor two at reachable free-location endpoints. A location box
independent of slant can nevertheless be obtained explicitly. Given any
finite incumbent L0, put m=nu+1, B=n log(2 t_nu(0)/omega), and

    D=omega sqrt(nu expm1(2(B-L0+1)/(n m))).

Every location outside [min(x)-D,max(x)+D] has log likelihood at most
B-(n m/2) log(1+D²/(nu omega²))=L0-1, uniformly over alpha. This follows
by bounding every skew factor by two and every residual magnitude below
by D/omega. Thus lack of a location-tail bound is not the remaining
obstacle. This joint box is a derived implementation ingredient, not an
implemented certificate. With omega,nu held,
the right half-t face itself is maximized at min(x), and the reflected face
at max(x): moving the feasible endpoint toward its sample extreme strictly
increases every nonendpoint Student density. Their face suprema are therefore
explicit closed-density values, including factor two at reachable ties. The
missing part is a global 2D search with quantitative bounds on those layers
and the finite joint boxes. The scalar enclosure here does not provide it.
With fixed nu, a free scale on either half-face also has the monotone
scale profile given above. These explicit face suprema must be combined
with the finite joint search and the moving endpoint layers. Existing ht
certificates can be reused when their holds and fitting domains match.

For mixed-sign slant/scale, conditional slant roots can have competing
branches. Their scale derivatives and endpoint approaches require joint
bounds; a set of scalar fits at selected scales is insufficient. For scale
alone at a nonzero held slant, the skew derivative adds a nonmonotone term
to S, invalidating the symmetric proof. A complete scalar tail enclosure
and enumeration, including equality collapse comparisons, remains needed.

For the remaining free-nu subsets, the sn family already supplies global
hold-compatible normal-face certificates, and ht supplies global shape
certificates on its own fitting domains. What is missing is the finite-nu
St global search, quantitative computed df-tail/layer bounds, and the
hold-compatible integration of those parents. In particular, St keeps
nu>=1 even with the endpoint held, whereas held-endpoint ht ordinarily
allows every positive nu. The current St held-endpoint grid does not
certify that constrained face. The nu=1 restriction face and the joint
normal/half-normal corners must be included. The closure analysis in the 2026-11-07 skew theory
proofs supplies the escape structure but not an implemented global search.

## Constant-data joint location and slant

With all observations equal to c, omega and nu held, each density is bounded
by 2 t_nu(0)/omega. For finite alpha, equality is impossible: z=0 gives
skew factor one; z!=0 gives strictly smaller symmetric density and skew
factor strictly below two. Set xi=c-sign*delta, alpha=sign/delta² with
delta decreasing to zero. Then z tends to zero, alpha z tends to positive
infinity, and every density approaches the bound. Both signs give different
half-t laws with the same sharp likelihood supremum. The implemented tie
contains these two face witnesses and global supremum bounds. It does not
claim two finite maximizers. This is an exact exceptional joint-location
route; it does not supply missing box bounds for nonconstant samples.

## Exact symmetric Student delegation

For alpha held exactly at zero, T_(nu+1)(0)=1/2 cancels the factor two,
so the entire finite likelihood equals the location-scale Student likelihood
in (xi,omega,nu)=(mu,sigma,df). Both fitters restrict free df to at least
one. The all-free Student certificate therefore transports without a
Jacobian or a changed feasible domain. Its finite maxima, normal limit,
collapse, tie and budget decisions carry exactly the parent's evidence and
precision. This uses `lst_mle_certified`, not the parent's numerical held-
subset routes. Candidate model tags retain the Student coordinates; only
the selected normal face is re-expressed as (xi,omega,alpha=0) for the
existing skew-normal stop. This identity does not prove a new theorem
about any remaining nonsymmetric or held Student search.

## Quantitative compact rectangle for mixed-sign slant/scale (not implemented)

This makes the remaining strict fixed-nu, held-xi blocker more specific.
Let c=t_nu(0), m=nu+1, k=#(d=0), e=nu(n-k)-k>0, and let L0 be
any finite incumbent log likelihood. Uniformly in alpha,

    log likelihood <= n log(2c) - n log omega,
    log likelihood <= C + e log omega,
    C = k log c + (n-k)(log(2c)+m log(nu)/2)
        - m sum_nonzero log|d|.

The second bound uses t_nu(z)<=c (z²/nu)^(-m/2) for every nonzero z;
zero residuals have factor one, not two. Consequently every maximizing
scale lies strictly between the exponential of

    a=(L0-C)/e-1,   b=log(2c)-L0/n+1.

These inequalities exclude whole tails, not finite grid endpoints.
For each nonzero d, put q_min(d)=|d| sqrt(m)/sqrt(nu exp(2b)+d²).
This is a strictly positive uniform lower bound for |q(d,omega)| throughout
the compact scale interval. Let q_* be their minimum and, for each sign,

    C_sign=2 t_m(0) m^((m+1)/2) sum_sign q_min(d)^(-m).

The existing alpha-tail proof applies uniformly on this interval: on the
positive alpha tail the positive residual terms contribute at most
C_positive alpha^(-m-1), while each negative residual contributes at most
-m/(2 alpha), provided alpha q_*>=sqrt(m). Thus an upper bound on every
conditional stationary alpha is

    A_plus=max(sqrt(m)/q_*, (4 C_positive/(m n_negative))^(1/m)).

Reflection gives A_minus with the signs exchanged. Both signs are present,
so the counts in the denominators are positive. Conditional likelihood
increases toward the rectangle on both excluded alpha tails. Combined with
the scale bounds, this supplies a quantitative compact rectangle containing
all global finite maximizers. No half-t face is feasible for mixed-sign
residuals at held xi, and the likelihood on either infinite-alpha tail tends
to zero.

What is still missing is a tested 2D interval score/likelihood search on this
rectangle, stationary-component handling, coefficient enclosures in both
public coordinates, and practical floating-point validation of the new
rectangle arithmetic. The implemented one-dimensional enumerator does not
supply these pieces. Equality e=0 also falls outside this compactness proof
and needs its finite-versus-collapse comparison. This derivation is an
implementation route, not a certificate attached to current mixed-sign fits.

## Practical floating-point ratio allowance

The exact pdf/CDF enclosure requires evaluating a positive weighted ratio.
Forming `exp(logpdf-logcdf)` and then multiplying by a score weight fails
in very thin Student tails: both logs can be huge, while their difference
is small. The old final-score term-sum padding does not cover that loss.
The 80-digit check in thin-score.py found 33 of 37 point probes outside the
old allowance; at nu=alpha=1e6 and z=-1 the score error was about 9.31e-4
against an allowance of 2.84e-8. These are numerical counterexamples to the
old floating-point allowance, not to the exact-arithmetic inequality.

The corrected scalar alpha and location scores combine the weight in log
space and pad before exponentiation. For computed log-pdf p, log-CDF c,
and log-weight w, the practical log error is

    E=128 epsilon (1+|p|+|c|+sum |working log-weight terms|+|p-c+w|).

The positive contribution is enclosed by exp(p-c+w-E), exp(p-c+w+E).
This includes the arithmetic magnitudes that canceled, and avoids losing a
small ratio before multiplying by a large weight. The existing final-score
padding is retained. Nonfinite log bounds or exponentiated bounds cannot
license a fit: the scalar search stops numerically with its available
witnesses. This is still the package's practical floating-point contract,
not an outward-rounded special-function proof. Before-repair values and
80-digit references are retained in thin-score-before.json; the final
thin-score-checks.json distinguishes resolved enclosures from arithmetic
that correctly cannot provide a finite enclosure.

## Constant-data free-nu normal/skew corner

With all data equal to c, scale omega held, and xi,alpha,nu free, each
finite density is strictly below 2 phi(0)/omega. To see this, the Student
normalizer satisfies t_nu(0)=phi(0) E sqrt(V/nu), where V has Gamma(nu/2,2)
law. Its mean is nu and variance is 2nu, so strict Jensen gives a value
below phi(0) for every finite nu. Also E|sqrt(V/nu)-1|<=sqrt(2/nu), proving
convergence of the normalizer to phi(0). Skewing increases a density by at
most two, and the Student density is bounded by its normalizer.

The bound is sharp within the nu>=1 fitting restriction. For integer j
choose nu_j=j+4, xi_j=c-sign*omega/j, alpha_j=sign*j². Then z_j=sign/j,
h_j=alpha_j z_j sqrt((nu_j+1)/(nu_j+z_j²)) tends to positive infinity,
and the Student density tends to phi(0). The t_(nu_j+1) second moments
are uniformly bounded, so its upper tail at h_j vanishes by Chebyshev.
Thus both signs approach n log(2 phi(0)/omega). No finite parameter
vector attains the bound; the two half-normal laws have equal supremum
values. The fitter returns both face witnesses and the sharp global bound.

Stopped location witnesses are evaluated again at their represented public
coefficients. Their bounds include the difference from the normalized
point's likelihood as well as the point-rounding allowance. This matters
when location is very large relative to scale: a small normalized change
can round to a different public location with a visibly different density.
Unrepresentable witnesses are omitted; if none remain, an arithmetic stop
cannot be mislabeled as a certificate budget with an invalid payload. The
large-location/narrow-scale regression checks the serialized likelihood
bounds against the public density evaluator, independently of normalization.
