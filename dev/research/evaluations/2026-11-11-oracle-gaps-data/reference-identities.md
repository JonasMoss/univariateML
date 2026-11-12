# Reference identities used by TASK-196

These routes use mpmath and the declared mathematical density. None calls the
production C++, an R distribution, or a fitted sample as a numerical oracle.

* Burr: set `y = a log(1 + ((x-m)/s)^b)`. Then `S(x)=exp(-y)` and
  `f(x) dx=exp(-y) dy`. Reciprocal Burr reverses the tails and replaces
  `(x-m)/s` by its reciprocal. The reference integrates `exp(-y)` from
  `1e-40` to 120; the omitted mass is below `1.0001e-40`. Physical density
  derivatives and transformation Jacobians are checked separately.
* Gamma powers: `(X/s)^c ~ Gamma(a,1)`, or `(s/X)^c ~ Gamma(a,1)` for
  the reciprocal law. The latent logarithm has scale `1/a` below unit
  shape and `1/sqrt(a)` above it. For a cut `z`, the gamma Chernoff bound
  is `exp(a*(1-z/a+log(z/a)))`, on the appropriate side of the mean.
  Both normalization cuts are checked against `1e-40` using this bound.
* Concentrated gamma tails: integrate the exact density after shifting the
  integration endpoint to zero and scaling by the lesser of the central
  width and the local endpoint decay length. Concavity of the log gamma
  density (`a>1`) bounds an omitted tail by the endpoint integrand divided
  by the magnitude of its logarithmic slope. The implementation requires
  that bound below `exp(-180)` and a quadrature error estimate below
  `1e-50` times the integral. The quadrature estimate is numerical evidence,
  not an interval enclosure.
* Incomplete beta: Euler's continued fraction is evaluated on the smaller
  tail with modified Lentz arithmetic. The iteration must converge to
  100 decimal digits; it stops on exhaustion. This is a numerical convergence
  criterion, not a proved interval bound on continued-fraction rounding.
  Independent special-function comparisons and the generator's derivative
  checks provide additional evidence. For simultaneous large beta shapes,
  logit quadrature is centered at `log(a/b)` with scale
  `sqrt(1/a+1/b)`; concavity gives explicit tangent bounds on the omitted
  normalization tails.
* Binomial: `P(X<=k)=I_(1-p)(n-k,k+1)`. The finite polynomial normalization
  is `(p+(1-p))^n=1`. Zero modification and positive truncation operate on
  the upper probability and its exact conditional normalizer.
* Negative hypergeometric: if `X` is the position of the r-th marked item,
  `P(X<=k)` is the probability that at least r of the m marked items are
  among the first k positions. Summing the hypergeometric weights over the
  marked count needs `m+1` terms, rather than the support length of X.
* Geometric normalization uses its geometric series; negative-binomial
  normalization uses the binomial series; Poisson normalization uses the
  exponential series; logarithmic-series normalization uses
  `sum(p^k/k)=-log(1-p)`. The discrete normal bins telescope to one.
  Their zero-modified/conditioned forms retain total mass one. The generator
  still checks PMF differences against independently evaluated density rows.
* Skew normal with shape one has `F(z)=Phi(z)^2`; shape minus one is its
  reflection. Direct inversion avoids Newton stagnation at a special cut.

The exact identities establish transformations and normalization in exact
arithmetic. Floating-point comparisons and mpmath quadrature remain numerical
checks. A generated finite grid is not a whole-domain proof.

## Additional concentrating and count routes

GED normalization follows from `u=(abs(x-m)/b)^nu` and the complete gamma
integral. Its inverse chart scales the physical displacement by b before
applying sinh; b can be exponentially small in `1/nu`. Two-piece GED stretches
the two half integrals by xi and its reciprocal; their weights and the final
affine Jacobian cancel. Student, half-Student and two-piece Student normalize
by `C*sqrt(df)*B(1/2,df/2)=1`. Skew-t uses
`f(z)+f(-z)=2*t_df(z)` before applying that beta identity. Log-gamma uses
`y=ratelog*log(x)`. These are analytic normalization identities, not numerical
quadrature passes.

For skew-t the positive angular integrand has logarithm
`g(r)=-df/2*log(1+t^2/(df*sin(r)^2))`. Its derivative is positive and strictly
decreasing on `(0,theta)`. Scaling distance from theta by the inverse endpoint
slope resolves the thin angular layer. Concavity bounds an omitted integral
by `exp(g(end))/abs(g'(end))`, with the scale factor included. The code checks
that remainder and the quadrature error estimate separately.

The integer-shape upper gamma identity is a finite positive polynomial:
`Q(n,z)=exp(-z)*z^(n-1)/Gamma(n) * sum_j (n-1)^falling_j/z^j`.
When `z>=2*n`, the remaining positive ratios decrease and are at most one half;
a geometric majorant bounds the truncated polynomial. At enormously larger
z the integral `exp(-t)*(1+t/z)^(a-1)` encloses its own leading term with log
width below `1e-50`; this avoids ever forming `exp(-exp(large))`.

PIG normalization follows from its normalized inverse-Gaussian mixing density
and the Poisson exponential series, using Tonelli. For its existing count-tail
integral put `delta=n*(1-A)/A`, `v=delta*expm1(w)`. The narrow v layer becomes a
smooth integrand in w. Its Taylor error is multiplied by the bound
`min(1,6*n*(1-A)+(2*(1-A))^(3/2)*exp(-n))` for the integral of
`exp(-v)*h(v)^(3/2)`, rather than the much looser bound one. To obtain this,
use `h(v)<=(1-A)*(n+v)/(n*(1-A)+v)`, split at v=n and integrate the two
positive bounds. The omitted v tail and numerical quadrature error remain
separate checks.

For beta-negative-binomial near its Poisson region, use the exact PMF ratio
`(r+k)*(b+k)/((k+1)*(a+r+b+k))`. The factorial moment is
`E[(X)^falling_j]=(r)^rising_j*(b)^rising_j/(a-1)^falling_j`, for j<a.
Markov therefore bounds `P(X>=j)` by that moment divided by j!. The remaining
positive PMF sum must fall below `1e-48` times the retained sum. At the two
far-count probes, gamma-ratio bounds enclose the density by a power law;
the positive power sum lies between its integral and integral plus first term.
The threshold `n>1e50*(a+r+b+1)^2` makes their combined absolute log width
smaller than `1e-45`.

For asymmetric Skellam, canonicalize the rates to A>=B and reflect integer
CDFs with the required `-k-1` cut. Bessel recurrence gives a descending-count
ratio bound `sqrt(B/A)+max(k,0)/A`. The inequality
`I_(n+1)(z)/I_n(z)<1` follows by integration by parts: this ratio is the
expectation of t in `(-1,1)` under the positive weight
`exp(z*t)*(1-t*t)^(n-1/2)`. Near the mode condition on the smaller Poisson
count. Lower-CDF convolution term ratios are bounded by
`B/(j+1)*(1+A/(k+j+1))`; upper ratios are bounded by
`B/(j+1)*min(1,A/(k+j+2))`. These majorants decrease, so their geometric
remainders control the full omitted positive sum. The implementation uses
the descending-count branch wherever the convolution would have a negative
Poisson cut. Product Poisson normalization is exact by Tonelli.

## CMP Euler--Maclaurin remainder

The unit-rate small-nu route sums `f(k)=Gamma(k+1)^(-nu)`. It retains a positive
prefix through 9999, then uses Euler--Maclaurin of order n=20. For x>=10000,
on the complex disk of radius `R<=x/2` about x, the digamma series gives
`abs(psi(z+1))<=L(x)=log(2*(x+1))+2`. Indeed, writing z+1=u+iv,
`abs(psi(u+iv)-psi(u))<=abs(v)*psi1(u)`,
`psi(u)<=log(u)` and `psi1(u)<=1/u+1/u^2`. Set
`R=min(x/2,1/(nu*L(x)))`. Analyticity in the right half-plane then gives
`abs(f(z))<=e*f(x)`, and Cauchy's estimate gives
`abs(f^(n)(x))<=n!*e*f(x)*((2/x)^n+(nu*L(x))^n)`.

At a tail start s, log-concavity yields
`f(s+t)<=f(s)*exp(-delta*t)`, `delta=nu*psi(s+1)>0`, while
`L(s+t)<=L(s)+t/(s+1)`. Thus the integrated derivative bound, divided by f(s), is

```
n!*e/delta * ((2/s)^n + nu^n *
  sum_{j=0}^n binomial(n,j)*L(s)^(n-j)*j!/(delta*(s+1))^j).
```

Multiply by `2*zeta(n)/(2*pi)^n` for the Euler--Maclaurin remainder. The
continuous integral uses `u=delta*(x-s)` and cuts at u=200; the missing
integral, divided by f(s), is at most `exp(-200)/delta`. The implementation
adds that bound, the Cauchy remainder and the numerical quadrature error
estimate, and requires their sum below `1e-45` of the retained tail.
Odd derivatives are evaluated by complete Bell polynomials in the polygamma
derivatives of log f. A finite positive-sum additivity check independently
checks the tail assembly. The remainder argument is exact-arithmetic;
160-digit arithmetic and the explicit comparisons supply the practical
floating-point margin, rather than outward-rounded intervals.

## Small gamma arguments and logarithmic-series tails

For positive a and z, substitute t=z*u in the lower gamma integral:
`P(a,z)=z^a/Gamma(a+1) * E[exp(-z*U)]`, where `U~Beta(a,1)`.
For `0<z<1e-50`, `1-z <= E[exp(-z*U)] <= 1`. Consequently the leading
log probability has truncation error at most `-log(1-z)<2e-50`.
This avoids the huge reciprocal argument in mpmath's upper-gamma 2F0 trial
when a gamma-power density is almost flat in its physical coordinate.

Write `lambda=-log(p)`. The unnormalized logarithmic-series tail is
`sum_(n=k)^infinity p^n/n = integral_lambda^infinity exp(-k*t)/(1-exp(-t)) dt`.
The partial-fraction identity
`1/(1-exp(-t))=1/t+1/2+2*t*sum_(n=1)^infinity 1/(t^2+4*pi^2*n^2)`
gives an enveloping Bernoulli expansion: expand each positive denominator
as a finite geometric series. For every positive t, its signed remainder
is bounded in magnitude by the next term. Integrating the positive bound
gives `abs(B_(2j))/(2j)! * Gamma(2j,k*lambda)/k^(2j)` for the next omitted
correction. The scaled implementation requires this bound below `1e-50`
of its retained sum; exhaustion raises an error. Below k=200 it instead
uses a finite positive prefix and the exact generating-series normalizer.
The optimized branch uses p>1/2, retaining at least about 100 decimal
digits in that prefix complement at the working precision.

Discrete PMF/antiderivative verification now compares the same relative and
absolute error thresholds in log arithmetic. Natural-probability jump checks
replace probabilities below exp(-10000) by zero, far below every binary64
input in this grid. Neither change accepts an unchecked quantile: the root
bracket and the jump tolerance are retained. Newton proposals with extreme
log slopes are skipped, preserving bisection and its acceptance bracket.
Accepted Newton proposals also stay within the central half of the bracket,
so a proposal cannot stagnate at an endpoint. Discrete expansion retains its
last proved lower endpoint; very wide positive count brackets first bisect
their logarithmic scale, preserving the same monotone crossing invariant.

At negative canonical Skellam counts, termwise comparison of the positive
0F1 series gives `I_(n+1)(z)/I_n(z)<z/(2*(n+1))`. The descending-mass bound
therefore improves to the lesser of the previous bound and `B/(1-k)`.
It recognizes when the first mass alone has a relative remainder below
`1e-50`, without subtracting indistinguishable far-count log masses.
Near the mode, adjacent Poisson CDF cuts are obtained by adding or subtracting
their positive PMF. The upper-tail subtraction falls back to a direct
incomplete gamma call if its remaining fraction is below `1e-100`.

## Checks prompted by the consumers

For reciprocal Burr, the density at the finite lower endpoint is governed
by the power `a*b-1`, not by the reciprocal map alone. It is infinite,
finite `a*b/s`, or zero according to the sign of that power. The initial
new rows used the wrong endpoint limit; those rows were regenerated.

Positive conditioning uses both base tails:
`F_positive(x)=(F(x)-F(0))/(1-F(0))`. Subtracting survival logs loses this
lower probability when both survivals round to one even at 160 digits.
The reference instead subtracts the two lower probabilities in log
arithmetic whenever the base lower probability is below one half.

For a normal interval, form its physical width before subtracting the
parent mean. With standardized left endpoint z and width w, midpoint
integration has relative error bounded by its second derivative times
`w^2/24`. The branch `w*(1+abs(z)+abs(w))<1e-35` keeps that error below
`1e-70`. This resolves a subnormal interval next to a truncation boundary
even when its standardized endpoints are indistinguishable at 160 digits.
The corrected reference rows, rather than the retired preliminary rows,
are used in the final measurements.

## Production identities verified against these references

The Poisson CDF at an integer k is the upper gamma tail with shape k+1 at
lambda. When k+1 is represented exactly and is at least 16, its implementation
now uses the existing physical-coordinate gamma evaluator. It exchanges the
two tails, without a rounded log/exp reconstruction of the gamma argument.
Unit-power transformed gamma and inverse transformed gamma likewise use the
parent gamma APIs. These are exact submodel identities; the independent
mpmath routes above still supply their numerical checks.

The production fixes do not change fitter searches, statuses, goldens, or
sampler selectors. Unsupported precision regions retain their independent
numbers in truth exclusions. `classify_rows.py` archives exactly the rows whose
final-binary measurements failed, proves all numerical payloads unchanged,
and verifies that the final active set is a subset of those measurements.
Its receipts explicitly describe classification rather than claiming a fresh
numerical replay. The subsequent required C++/R suites consume the final tables.
