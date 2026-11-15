# Mathematical evidence

These are algebraic arguments, separate from the numerical regressions.

For a translated half-normal, the log likelihood at public coefficients is
`n*(log(2/pi)/2-log(sigma)) - sum(((x-mu)/sigma)^2)/2`.
Replacing the last term by `n/2` is valid at the exact optimizing scale, but
not after that scale rounds to a subnormal binary64 number. The same issue
occurs for Rayleigh: `sum(log(x))-2*n*log(sigma)-sum((x/sigma)^2)/2`.
The fixes evaluate these expressions at the unchanged returned subnormal coefficients;
the rounding-accurate ordinary-scale identities retain their existing evaluation order.
The half-normal and normal boundary payloads of skew-normal and Student-t
require the same distinction. The free skew-normal half-normal endpoint is the
original sample extreme; reconstructing it through a normalized coordinate can
round it inside the sample and must not define the public support. Both the joint Burr certificate and the held-power Burr-to-Lomax
transformation map face coefficients back to physical units. The latter
includes a map from exponential rate to Weibull scale. Face payload
likelihoods must be evaluated after these maps; global certificate bounds
remain separate from the payload point value.

For positive constant beta-prime data at x0 and fixed scale s, let
u=x0/(x0+s), alpha=t*u, beta=t*(1-u). The beta density at u grows as
sqrt(t/(2*pi*u*(1-u))) by Stirling's formula; the transformation Jacobian
is positive and fixed. Thus the likelihood is unbounded, including a
singleton. Unrestricted GB2 contains this subfamily (power=1), so its
constant-data likelihood is also unbounded. More generally, with scale=x0
and any fixed positive beta shapes p,q, its density at x0 is
`power/(x0*B(p,q)*2^(p+q))`; a free power therefore proves unboundedness
even in either held unit-beta-shape submodel. For Burr, take shape1=1 and scale=x0: the density at x0 is
shape2/(4*x0), which diverges as shape2 increases. This applies equally
to a singleton and a repeated constant sample.

For constant Kumaraswamy data x0 in (0,1), take a to infinity and
b=x0^(-a). Then (1-x0^a)^(b-1) tends to exp(-1), while
`a*b*x0^(a-1)=a/x0` diverges. `Unbounded` is therefore nonexistence,
not invalid input. No finite estimates were replaced by this correction.

For the double-Pareto log-density, write m=log(b),
L=sum((m-log(x))_+), R=sum((log(x)-m)_+). For L,R positive, profiling the
left and right exponential rates gives
alpha=n/(sqrt(L)*(sqrt(L)+sqrt(R))) and
beta=n/(sqrt(R)*(sqrt(L)+sqrt(R))). After restoring the standardized log
unit these are the exported left/right shapes. The profile log likelihood
is `n*log(n)-2*n*log(sqrt(L)+sqrt(R))-n-sum(log(x))`.
The existing kink enumeration chooses m; the new payload stores that
choice and evaluates the density at its rounded coefficients. If L or R
vanishes, the corresponding one-sided face is Pareto or power. The GB2
search's choice among faces remains numerical, hence uncertified; these
identities alone do not certify its global face comparison.

A count below a held negative-hypergeometric stopping count cannot be in
support for any composition. Hypergeometric infeasibility has the same
support meaning. An observed Rayleigh zero has density zero at every
positive scale. These are reported as invalid input, with the existing
specific diagnostic, rather than claiming a positive finite unattained
supremum. The existing outcome vocabulary has no separate zero-likelihood
reason.

EMG comparison candidates are marked accurate only after the same local
Newton refinement and public-coordinate projection checks used for Ok
fits. Their point likelihood is recomputed from the family density.
This is local coefficient evidence and does not complete a global proof:
a selected finite comparison winner is an uncertified estimate.

For an unrestricted GPD threshold let m be the minimum, repeated k times.
Choose mu=m, sigma=exp(-t), xi=t as t increases. Each minimum contributes
exp(t); each observation strictly above m has density asymptotic to
exp(-1)/(t*(x-m)). The product diverges as exp(k*t)/t^(n-k). This path
respects the free-shape restriction xi>=-1. It proves nonexistence when all
three coefficients are free; it does not decide every held slice.

For Pareto IV with a free threshold, choose min=m-epsilon and any allowed
power c in (0,1). At a minimum the density is asymptotic to
shape1*c*epsilon^(c-1)/scale^c, while the other distinct observations have
positive finite limiting densities. Hence every slice permitting c<1 has
unbounded likelihood. With c>=1 held, this particular argument does not apply. If shape1 and
scale are both free, however, take shape1=1/t, scale=exp(-t), and
min=m-scale. The minimum densities grow as c*exp(t)/(2*t), and each
nonminimum density is asymptotic to c*exp(-c)/(t*(x-m)); this again proves
unboundedness for every fixed c>0. Other held slices remain unsupported
when these paths do not apply.

A held GB2 scale b strictly below the minimum does not pin the endpoint of
all simultaneous escapes. For any c>b, let power a tend to infinity,
p=exp(a*log(c/b)-sqrt(a)), and q=r/a. Then a/(B(p,q)*p^q) tends to r,
p*log(1+(b/x)^a) tends to zero for x>=c, and
q*log(1+(x/b)^a)-q*log(p) tends to r*log(x/c).
Consequently the densities at x>=c converge to r/x*(x/c)^(-r), including
the endpoint x=c. Taking c=min(x) gives the freely fitted Pareto face.
Reflection x -> 1/x gives the power face when b>max(x). The old held-scale
search could select a lower lognormal face; the corrected comparison checks
these one-sided faces before accepting that selection. Their existence is
proved by this path; the selection remains uncertified. No old Ok branch
or coefficients change. The archived nested fit's likelihood was independently
checked at 90 digits (error 2.71e-12 in absolute log likelihood); it really
exceeds the old lognormal face, rather than merely having a misreported value.

A held endpoint is copied exactly into the double-Pareto payload. Mapping it
through exp(log(b)) can move b even without any optimization error (for example
at b=1e300). The double-Pareto and one-sided payloads are checked after point
evaluation; a nonfinite or unrepresentable computed face is an evaluation
failure, not a limit recommendation with invalid coefficients. This check
does not reject an existing Ok search result.

The same endpoint-face check applies when the old search selects a held-endpoint
Pareto or power face. For Pareto, the profiled likelihood is
n*log(n)-n*log(T)-n-sum(log(x)), T=sum(log(x/c)). Raising c toward min(x)
strictly reduces T and raises the profiled likelihood. Thus the free-endpoint
face dominates every smaller held endpoint; reciprocation gives the power
statement. The implementation compares evaluated public point likelihoods,
so it never substitutes a lower computed point for the old selected face.
These changes occur only on the existing limit-return branch, leaving every
old Ok coefficient vector unchanged.

### GPD: finite scale-collapse supremum

For the sample {0,1}, held threshold mu=0 and shape xi=1, the likelihood
is L(sigma)=(1+sigma)^(-2) for sigma>0. It increases to 1 as sigma decreases
to zero but never attains 1. This is an exact algebraic nonexistence proof.
The weak point-mass limit at zero cannot reproduce the joint density at the
positive observation. Consequently the outcome is nonexistence, reason
`unattained`, rather than a named limit-model fit. The finite supremum remains
in `limit_loglik` as a diagnostic. The same distinction applies to the
fixed-shape equality case in the existing threshold-collapse calculation.

More generally, let k threshold observations have excess zero and let m=n-k
have positive excesses y. At fixed xi>0 the likelihood is
`L(sigma)=sigma^(m/xi-k) * product((sigma+xi*y)^(-1-1/xi))`.
In the equality case m/xi=k, the power of sigma cancels. Every remaining
factor strictly decreases with sigma, and m>0. Thus the finite supremum
`product((xi*y)^(-1-1/xi))` occurs only as sigma decreases to zero and is
unattained throughout this equality case, not just for the regression sample.

For the double-Pareto kink profile, between consecutive log observations
both L(m) and R(m) are affine and nonnegative. Hence
`sqrt(L(m))+sqrt(R(m))` is concave there and has a minimum at an endpoint.
The profile log likelihood is maximized at that same endpoint. Checking the
knots and the one-sided endpoint cases therefore suffices for the profiled
limit model. This statement does not certify the enclosing GB2 fit.

### GB2 constant dispatch and logarithmic collapse

The final constant-data concentration stop is reached only after the
one-free-beta-shape held-power/scale solvers and the scalar constrained
solvers have run. The remaining free-beta-shape pair admits beta
concentration at the observation, which proves unboundedness. This does not
classify all constant held subsets as unbounded: the existing regression
`GB2 constant mixed-power finite regimes have certified finite maxima`
(and its R counterpart) explicitly covers the finite maximum with q=2,
x=.5 and b=1. Those specialized solvers precede the constant fallback.

Equality of rounded logarithms is insufficient evidence of constant data.
For example, 1e300 and its next larger binary64 value are distinct, but
have equal binary64 logarithms. With power and scale held, the transform
to the beta observation is strictly monotone in exact arithmetic. Distinct
observations therefore do not admit the constant-data beta concentration
escape. The final nonexistence branch now checks equality of the original
observations; a collapsed logarithmic variance instead gives an arithmetic
evaluation failure. No previously accepted coefficient vector changes.

For the two exact beta observations u1!=u2, put
`A=(log(u1)+log(u2))/2`, `B=(log(1-u1)+log(1-u2))/2`.
Strict Cauchy-Schwarz gives `exp(A)+exp(B)<1`. The largest concentration
coefficient in the beta log likelihood is
`max_r {r*A+(1-r)*B-r*log(r)-(1-r)*log(1-r)} = log(exp(A)+exp(B))<0`.
Stirling's expansion therefore excludes an unbounded concentration escape;
the positive-parameter axes also have vanishing likelihood. A finite beta
maximum exists. The independent 100-digit reproducer
`log-collapse-reference.py` resolves the strict Cauchy gap as approximately
2.76402410762e-33. The binary64 search loses that distinction in its
logarithmic summary, so it reports evaluation failure instead of claiming
mathematical nonexistence.
