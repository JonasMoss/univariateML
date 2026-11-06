# Native arithmetic and acceptance checks

The native attempt uses ordinary binary64 on every platform. It does not rely
on extended `long double`, arbitrary precision, R or a sibling repository.
The mathematical arguments are those of the [root derivation](../2026-11-05-t-root-data/derivation.md)
and [profile derivation](../2026-11-04-t-cert-data/derivation.md).

Every elementary interval operation expands both endpoints by
`64*epsilon*max(abs(endpoints))+denorm_min`. Reciprocals receive the same
magnitude-scaled allowance before multiplication. Normalized input intervals
add `64*epsilon*(1+abs(x/unit)+abs(center/unit)+abs((x-center)/unit))`
to cover the original-data affine transformation. An n-term sum uses binary-carry pairwise accumulation. It adds
`64*epsilon*(2*bit_length(n)+2)*(1+sum(max(abs(term endpoints))))`,
with an additional relative enlargement for accumulating the absolute sum.
The maximum rounded-addition depth is logarithmic; this bound is not used
for naive sequential accumulation. Invalid domains,
nonfinite intervals, singular or badly conditioned linear solves, unresolved
root neighborhoods and coefficient enclosures fail closed. These are practical
floating-point margins, not outward-rounded IEEE interval arithmetic or a
proof of libm accuracy. Computed strict comparisons remain conditional on the
enclosures, as in the package's other certificates.

## Gamma ratio without large gamma subtraction

Write c(v)=log Gamma((v+1)/2)-log Gamma(v/2)-log(pi)/2. The partial-fraction
expansion of tanh(u/2) has alternating polynomial bounds: expand each
positive summand's factor `1/(1+u^2/a^2)` by its finite geometric identity.
The remainder has the next term's sign and magnitude at most that term.
Integration against exp(-v*u) gives

```
c(v)-log(v)/2+log(2*pi)/2
  = -1/(4*v)+1/(24*v^3)-1/(20*v^5)+17/(112*v^7)+R0,
|R0| <= 31/(36*v^9),
c'(v) = 1/(2*v)+1/(4*v^2)-1/(8*v^4)+1/(4*v^6)-17/(16*v^8)+R1,
|R1| <= 31/(4*v^10),
-c''(v) = 1/(2*v^2)+1/(2*v^3)-1/(2*v^5)+3/(2*v^7)-17/(2*v^9)+R2,
|R2| <= 155/(2*v^11).
```

These are integrated remainder inequalities; the derivative error bounds are
not obtained by differentiating an unspecified error term. Shift v to at least
32 using the exact two-unit gamma recurrence (at most 16 shifts). The
log-normalizer correction at v equals its value at v+2 minus
`log1p(1/v)-log1p(2/v)/2`. The positive derivative differences are
`1/(v*(v+1))` and `(2*v+1)/(v^2*(v+1)^2)`. A separate relative margin
`(8+2*shift_count)*64*epsilon*(sum(abs(recurrence terms))+abs(expansion))` protects these
operations; the explicit analytic remainder is added as well.
`check-gamma.py` checks the resulting enclosures against independent 80-digit
mpmath values. Such point checks corroborate the derivation; they do not prove
the complete floating-point error contract.

## Relative likelihood

Let b and V be the exact empirical mean and variance after normalization,
and let u=t-log(V)/2. The normal log likelihood difference at nuisance (m,t)
from its fitted value is

```
-n * [u + expm1(-2*u)/2 + exp(-2*t)*(m-b)^2/2].
```

The first bracket term is nonnegative and evaluated by its quadratic Taylor
series near zero. For residual z=(x-m)*exp(-t) and w=z^2/v, the t correction
per observation is

```
v/2 * (w-log1p(w)) - log1p(w)/2.
```

A series evaluates `w-log1p(w)` near zero. The truncation of each small-argument
series is below the elementary practical margin on its stated branch. The
normalizer correction above completes the relative likelihood. No two full
large likelihoods are subtracted to decide the normal seam. Normalization
constants are finite binary64 values; interval transforms of the original
binary64 observations preserve their uncertainty. Absolute original-unit
likelihoods are formed only for diagnostics and returned values.

## Proposal, proof and bounded refusal

Fixed-shape EM and guarded Newton steps nominate nuisance points. A geodesic
score norm times the maximum distance to the proved containment rectangle
bounds the gap to the exact nuisance optimum. The gap, together with the
relative likelihood enclosure, encloses the complete conditional profile.
Only a subset of the valid order-statistic gap bounds is needed for
containment; choosing fewer can weaken bounds but cannot invalidate them.

Joint Newton steps nominate stationary roots. The strict componentwise
contraction self-map and infinity-norm check establish a unique enclosed
root. A uniform nuisance-ball inward condition and full Hessian congruence
bound identify a strictly concave profile neighborhood. Root boxes must lie
inside it. Lower endpoints use the relative endpoint chord and a nuisance
root box when the scale is positive. Neither EM convergence nor Newton's
residual accepts a result.

Signed normal-tail exclusion uses interval moments and the uniform nuisance
ball; unsigned exclusion uses a strictly positive feasible witness and the
proved C/v bound. The finite shape complement is exhaustively subdivided
with the profile chord envelope. All evaluations share an explicit cache
budget; proposals, line searches, neighborhood trials and tail trials also
have finite loop bounds. Shapes beyond 1e14, unrepresentable midpoints,
unresolved comparisons, and arithmetic/domain failures refuse a result.
The 1e14 ceiling is a numerical-domain guard, never an estimate.

The returned ordinary-scale coefficient box must pass its width check.
The std integration transforms and checks the complete sd enclosure, using
sd=sqrt(v/(v-2))*s, before accepting a public fit. Conversion to original
physical units uses homogeneous relative allowances, avoiding a fixed absolute
error floor that could encompass negative scales for tiny positive data.

## Rejected native arithmetic (2026-11-06)

The first native attempt used `64*epsilon*(1+magnitude)` at every operation,
including a reciprocal before multiplication. At shape around 2e12 the
reciprocal of the chord denominator is around 1e-26, so a 1e-14 absolute
allowance made its product with a large squared width enormous. Recorded
cluster segment bounds reached millions despite likelihood gaps around 16.
The same absolute floor made a positive squared denominator near 1e-16
appear to cross zero in tight-cluster nuisance Hessians. Those bounds refused
safely, but caused avoidable budgets and numerical stops. Magnitude-scaled
operation allowances and a separate sum allowance address the source of the
problem without accepting failed comparisons. The original-unit affine
transform similarly needs homogeneous error control to preserve scale
equivariance.

Exact common replication is reduced by the gcd of all atom multiplicities
before fitting; every likelihood bound and candidate likelihood is then
multiplied by that integer. This is an exact reduction of the objective,
not jittering ties, approximate compression or a change to held fits. It
avoids doing the same arithmetic 100 times for an exactly replicated sample.

## Stable preconditioned point score

At fixed ordinary log scale the negative shape score is

```
F_v|s = -n*(c′(v)-1/(2*v))
         + sum[log1p(u)-u/(1+u)-u/(v*(1+u))]/2,
u=(x-m)^2/exp(2*t_a).
```

Both the normalizer derivative correction and `log1p(u)-u/(1+u)`
start at second order in 1/v. Direct recurrence for the correction has
positive increment `1/(v*(v+1)*(v+2))` under a two-unit shift. Its large-v
expansion is the c′ expansion with the first term omitted, with the same
integrated remainder. The logarithmic remainder uses its alternating series
near zero; the ordinary subtraction branch carries a margin based on both
terms, rather than the much smaller difference.

For a row C_i of the numerical inverse Hessian, the exact change of
coordinates gives the point-score combination

```
(C grad F)_i = C_i0*F_m + (C_i1-C_i2/(2*w))*F_t
               + C_i2*v0*(F_v|s).
```

The interval check combines these coefficients before bounding the scores.
This preserves their analytic cancellation near normal. It changes neither
the score, Hessian, contraction theorem nor optimization objective. The
original uncombined point enclosure refused the 4,000-point near-normal
case at root inclusion, despite a valid numerical proposal near v=539.
`arithmetic.py` independently checks the stable score against 80-digit
values, alongside the original-coordinate score and Hessian.

The direct-subtraction branches of `u-log1p(u)` and the normal-loss function
also use an allowance based on the magnitudes of both operands. Their small
series branches use a result-relative margin, with truncation below that
margin. Checks at the binary64 neighbors of the 0.01 cutoff protect the
switch: applying only a result-relative allowance to the direct subtraction
would understate its rounding error near that cutoff.

## Production search and work bounds

The objective is the **total** negative likelihood, with ordinary scale s,
a=s*sqrt(v), and t_a=log(a):

```
F(m,t_a,v)=-n*c(v)-n*v*t_a+(v+1)/2*sum(log((x-m)^2+exp(2*t_a))).
```

These derivatives and Hessians are for optimization; no information-matrix or
inference claim is made. The fitting sequence is:

1. Normalize sorted original binary64 observations using empirical mean and
   divisor-n scale, enclosing the affine transform. Reduce an exact common
   replication factor. The public shape domains have lower faces q=1 and q=2.
2. Evaluate 33 geometric shape nominations from q to 1000. Each nuisance
   proposal starts at normalized location 0 and ordinary log scale 0. EM runs
   for at most maxit updates, with a 1e-11 step nomination backstop. Up to
   min(40,maxit) guarded Newton steps follow; each permits 24 halvings and
   stops nominating at step size below 1e-13. The score/distance bound, not
   these solver backstops, encloses the complete conditional profile.
3. Choose the best lower witness. A lower-face nomination first attempts the
   endpoint certificate; failure falls back to joint root nomination, since
   an interior maximum can lie between q and the next grid point. Joint
   Newton has min(60,maxit) steps, 30 halvings and strict shape/domain guards.
   A trial midpoint value within 1e-10 of its predecessor is allowed only as
   a nomination; it does not establish likelihood ascent or accept a fit.
4. Root radii from 1e-10 through 1e-6, scaled by max(1,abs(coordinate)), must
   pass the strict self-map and contraction checks. Larger neighborhoods try
   relative shape widths from 0.01 down to 1e-8 and nuisance radii h,3h,10h,30h.
   Every accepted root box must lie strictly inside a valid uniform tube.
   Positive-scale endpoints need a two-coordinate root box. Endpoint chords
   try relative widths 0.1 through 1e-5. An unchecked final iterate is never
   returned.
5. Signed tails try normal nuisance radii 0.01,0.03,0.1 and 80 dyadic h values,
   with strict mass, inward and sign comparisons. An unsigned tail uses
   N=max(1000,4*C/witness), C=3*n*(range/L)^4/8, for a positive finite witness.
   This is an exclusion threshold, not a fitted shape. N above 1e14 refuses.
6. A maximum heap subdivides the finite complement with geometric midpoints.
   Every interval outside the proved neighborhood needs its chord upper
   bound strictly below the witness. Acceptance requires an empty heap, an
   excluded tail, the appropriate root/endpoint certificate and public
   coefficient widths within the declared target. No shape-unimodality
   assumption is used. An unrepresentable midpoint gives an unresolved tie;
   the 4096-profile cache budget gives a budget stop. Arithmetic, domain,
   conditioning and width failures give numerical stops.

The implementation bounds a matrix solve by pivot and inverse-residual guards;
the inverse itself must pass a strict defect check. Proposal convergence never
overrides any failed proof comparison. Public all-free gradtol selects a width
target clamped to [1e-7,1e-6]; the standalone certificate also permits an explicit
positive width target and cache budget. Location widths use empirical-scale
units; scale, shape and std standard-deviation widths are relative. Returned
finite coefficients approximate the unique maximum enclosed in the root box.
Held routes retain their existing score/control meanings.

The cache/heap use O(budget) storage, the sample O(n), and interval sums fixed
scratch storage. Most work is O(n * profiles * nuisance_iterations), with
bounded Newton/backtracking and neighborhood overhead. Containment checks use
at most eighteen order-statistic gap choices and reuse the lower-face bound
when it is positive. This trades speed against a global guarantee: shallow
profiles and near-winner likelihood comparisons can refuse even when an
independent numerical root is known. The report names those retained cases.
