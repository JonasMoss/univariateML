# Supporting bounds for the open held-scale Fréchet cover

This is an exact-arithmetic compactness argument for F1, not an implemented
certificate or a new certification claim for the held-scale fitter.
Let sigma be held, lambda >= 1, m=min(x), R=max(x)-m>0, and k the minimum
multiplicity. Use d_i=(x_i-m)/sigma and e=(m-mu)/sigma>0.
For each fixed lambda, the unique location root satisfies e<1: at e>=1,
every location-score summand

    [(lambda+1)-lambda/(e+d_i)^lambda]/[sigma*(e+d_i)]

is strictly positive, so increasing mu increases the likelihood. The
monotone location-ratio argument in the family dossier proves uniqueness.
The maximum of each density is less than lambda/sigma; hence every
individual log density is bounded by log(lambda/sigma): its peak factor is
exp(r*log(r)-r)<1 for r=1+1/lambda in [1,2].

Put tau=R/sigma, delta=min(tau/2,1/2), c=1-delta,
A=-log(c)>0 and C=log(1+tau/2)>0. If e<=c and
lambda>=log(2)/A, each minimum has y=e^(-lambda)>=2. The function
(1+1/lambda)*log(y)-y decreases there, so

    ell <= U1(lambda)
        = n*log(lambda/sigma)+k*((lambda+1)*A-exp(A*lambda)).

If e>c, the largest standardized distance is at least 1+tau/2. Dropping
its negative exponential term and bounding all other densities by their
peaks gives

    ell <= U2(lambda)
        = n*log(lambda/sigma)-(lambda+1)*C.

Both U1 and U2 tend to minus infinity, and their derivatives

    U1' = n/lambda+k*A*(1-exp(A*lambda)),
    U2' = n/lambda-C

are strictly decreasing. Given a finite feasible incumbent L, any Lambda
at or above max(1,log(2)/A) with U1(Lambda)<L, U2(Lambda)<L and both
derivatives negative excludes the whole tail lambda>=Lambda. These are
value/monotonicity comparisons, not a finite parameter cutoff assumed to
contain the maximum.

For 1<=lambda<=Lambda and 0<e<=epsilon<=1/2, write y=e^(-lambda).
Then y>=1/epsilon>=2 and

    (1+1/lambda)*log(y)-y <= 2*log(1/epsilon)-1/epsilon.

Consequently

    ell <= n*log(Lambda/sigma)
           +k*(2*log(1/epsilon)-1/epsilon).

Choose epsilon making this upper bound smaller than L. Every global
maximizer is then in the explicit compact rectangle

    1 <= lambda <= Lambda, epsilon <= e <= 1.

The remaining F1 work is to implement useful, budgeted profile enclosures
on this rectangle, expose certified held-scale inner location brackets,
isolate all competing maxima in public coordinates, and compare them.
Joint concavity or unimodality has not been proved. The all-free GEV orbit
cannot supply this cover: held public Fréchet sigma constrains the orbit,
and its lambda-to-infinity face has GEV scale sigma/lambda tending to zero,
not the proper positive-scale Gumbel face used by F2.

## Positive-shape location bounds with GEV scale held

These support Open G2; its public fitter is not promoted to certified.
Let sigma be held and 0<xi<=1. Write c=1+xi*(m-mu)/sigma and
t_i=c+xi*(x_i-m)/sigma. At the unique fixed-shape location root,

    sum t_i^(-1-1/xi) = (1+xi)*sum 1/t_i.

Uniqueness follows from the fixed-shape, fixed-scale Fréchet location-ratio
argument: at this fixed xi the map holds lambda=1/xi and sigma_F=sigma/xi.
The minimum terms on the left and all terms on the right give

    k*c^(-1-1/xi) <= (1+xi)*n/c,
    c >= [k/((1+xi)*n)]^xi.

Each individual location score crosses zero at x_i+shift, where
shift=sigma*[1-(1+xi)^(-xi)]/xi. Their sum is positive below m+shift.
Hence the whole location maximizer lies in

    m+shift <= mu
             <= m+sigma*[1-(k/((1+xi)*n))^xi]/xi
             <= m+sigma*log(2*n/k).

The last inequality uses 1-exp(-xi*z)<=xi*z for z>=0. Thus the
positive-shape held-scale location region does not require exp(R/sigma)
in a Gumbel-neighbourhood error bound. Its interval score/curvature cover,
the negative-shape moving-support face, and the distinct held-location
scale problem still need implementation before G2 can be certified.

## Sharper F1 root region and exponential bound

The fixed-shape location-score equation, in the same dimensionless variables,
is

    lambda*sum(w^(-lambda-1))=(lambda+1)*sum(1/w), w=e+d.

The k minimum terms on the left and sum(1/w)<=n/e on the right imply

    e >= b(lambda)=[lambda*k/((lambda+1)*n)]^(1/lambda),
    e^(-lambda) <= ((lambda+1)*n)/(lambda*k) <= 2*n/k.

Every individual score is negative for e>=1, so the unique root lies in
[b(lambda),1). At the lower endpoint the score is positive for nonconstant
data, supplying a bracket independently of a numerical proposer. Moreover

    d log(b)/d lambda
      =[log((1+1/lambda)*n/k)+1/(lambda+1)]/lambda^2 > 0.

Thus a shape slab [l,h] has the uniform inner-root box [b(l),1], and all
stationary inner roots have e>=k/(2*n). This is stronger than the small-gap
superlevel bound above when only inner maxima need enclosure. It also
bounds every power exponential at an inner optimum by 2*n/k, rather than
an exponential in the data radius or in the compact shape upper bound.
A rectangular slab box may still require intersection/refinement before
this pointwise exponential bound applies uniformly: it cannot simply be
inserted into an arbitrary partial-derivative evaluation. The remaining
implementation is a budgeted uniform inner-root enclosure, constrained
profile jets and outer stationary/value coverage. No new fitter claim
is made by this supporting result.

For a full slab [l,h], its certified root rectangle e>=b(l) also gives
an explicit uniform bound without using a pointwise root identity:

    (e+d)^(-lambda) <= b(l)^(-h)
      = [((l+1)*n)/(l*k)]^(h/l) <= (2*n/k)^(h/l).

When h<=2*l this is at most (2*n/k)^2 everywhere in that rectangle,
including points that are not inner roots. Thus logarithmic shape slabs
avoid an exponential in the compact shape upper bound even in partial
interval-derivative evaluations. The sharper 2*n/k bound remains a
root-only statement. Neither inequality implements the missing constrained
profile cover.
