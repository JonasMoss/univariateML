---
id: cmb
inference_numeric:
  - regime: "size zero"
    available: false
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with dispersion known"
    available: true
    condition: "known size = 1; 0 < prob < 1 free and finite nu held; the Bernoulli probability is identifiable; dispersion is not"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with dispersion free"
    available: false
    condition: "known size = 1; nu free; DQM and pointwise C2 hold but the dispersion score is identically zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer size >= 2 held, 0 < prob < 1, finite nu; prob and/or nu free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Conway--Maxwell--binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, 10000000]'
  simulate: 10
  integer: yes
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: nu
  symbol: '\nu'
  role: dispersion
  domain: (-inf, inf)
  simulate: 1
  kind: regular
r:
  density: dcmb
  sampler: rcmb
  control:
    size: "Required number of trials; NULL estimates it where supported. Fixed sizes are limited to 1e7 for allocation safety."
    prob: "Optional fixed canonical probability between zero and one inclusive; NULL estimates it."
    nu: "Optional fixed finite real dispersion; NULL estimates it. Both prob and nu may be held fixed."
    reltol: "Relative tolerance for the free-coordinate moment residuals."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations (default iterlim 2000); per enumeration: 100000 * min(iterlim, 100000) enumeration cells."
log_density: "nu*(loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)) + x*log(prob) + (size-x)*log(1-prob) - log(Sum((factorial(size)/(factorial(k)*factorial(size-k)))**nu * prob**k * (1-prob)**(size-k), (k, 0, size)))"
limits:
  - model: cmp
    when: 'As $n\to\infty$ with positive $\nu$ held and odds proportional to $n^{-\nu}$, the law approaches a CMP member.'
    detect: With positive $\nu$ held and $p$ free, the exploratory search finds no strict improvement over the limiting model. A larger finite maximum is not excluded.
    status: FaceUnresolved
    fixed: [nu]
    firmness: proved
    numerical: true
  - name: point mass
    firmness: proved
    when: 'As $\nu$ diverges along an exposing direction, the law concentrates at an interior support point.'
    detect: With $n$ held and $p,\nu$ free, the data are constant at an interior point. With $n\ge2$ even and interior $p$ held, all counts equal $n/2$. With $n,\nu$ free and interior $p$ held, a positive constant count has twice its value within the supported size range.
    status: PointMass
  - name: adjacent two-point law
    firmness: proved
    when: 'As $\nu\to+\infty$, suitable canonical probabilities concentrate mass on two adjacent integers.'
    detect: With $n\ge2$ held and $p,\nu$ free, both of two adjacent integers are observed. With odd $n\ge3$ and interior $p$ held, only the two central integers occur. With $n,p,\nu$ free, both of two adjacent positive integers occur. With $p=1/2$ held and $n,\nu$ free, adjacent positive counts occur equally often and the central size is supported.
    status: AdjacentLimit
  - name: endpoint two-point law
    firmness: proved
    when: 'As $\nu\to-\infty$, mass concentrates on the two support endpoints.'
    detect: With $n\ge2$ held and $p,\nu$ free, both endpoints are observed. With $n\ge2$ and interior $p$ held, all data lie at the endpoints. With $n,p,\nu$ free, the sample contains zero and an integer at least two and no other values. With $p=1/2$ held and $n,\nu$ free, zero and an integer at least two occur equally often and no other values occur.
    status: EndpointLimit
references:
- 'Kadane, J. B. (2016). Sums of Possibly Associated Bernoulli Variables: The Conway-Maxwell-Binomial Distribution.
  Bayesian Analysis. doi:10.1214/15-BA955.'
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
fit_args:
  size: 10
examples: |
  set.seed(1)
  x <- rml(200, mlcmb())
  mlcmb(x, size = 10L)

group: Poisson, binomial and negative binomial
construction:
- base
- generalisation
relations: {}
fit:
  method: profile-search
  certified: 'yes'
  note: 'cmb/README.md, Fixed-size coefficient acceptance and Free-size acceptance; cmb/mle.hpp, cmb_finish_work and cmb_detail::finish_size: finite winners pass score half-space coefficient enclosures at their selected size, with exact atomic and Bernoulli reductions. Entropy, integer-tail and block bounds exclude other sizes. Unresolved size comparisons, faces, numerical failures and shared-ledger exhaustion stop with witnesses. Certificates are conditional on computed comparisons and practical floating-point margins.'
areas:
- counts and ecology
existence:
  uniqueness: unknown
  statement: >-
    With $n$, $p$ and $\nu$ free, constant samples attain likelihood one.
    Observations on zero and one attain the Bernoulli maximum, which also
    equals the adjacent CMP limiting supremum. Observations on
    two adjacent positive integers, or on zero and an integer at least two, have
    an unattained saturated supremum. A uniform empirical distribution on
    zero through a maximum of at least two attains its saturated likelihood.
    General free-size existence and uniqueness do not have a simple
    closed-form criterion here. With positive $\nu$ held and $n,p$ free,
    a finite candidate strictly above the CMP supremum ensures finite
    attainment by the tail bound below. With $p$ held and $n,\nu$ free,
    the constant and symmetric two-point cases below are settled, but no
    complete criterion is asserted. With $n\ge2$ held, the joint
    finite interior maximum exists exactly when the empirical sufficient
    statistic lies in the polygon interior; endpoint vertices are also
    attained through endpoint probabilities. For held $0<p<1$
    and $n\ge2$, a finite dispersion maximum exists when the
    empirical mean log binomial coefficient lies strictly between its
    minimum and maximum. With finite $\nu$ and positive $n$ held,
    the free probability has a unique mean-matching maximum, including
    endpoint probabilities. With $p,\nu$ held, a feasible free-size maximum
    is attained when $0<p<1$: for positive dispersion the fixed odds force
    mass away from bounded counts as $n$ grows, and for negative dispersion
    the normalizer's endpoint terms bound the tail. At zero dispersion the
    smallest feasible size is optimal. Endpoint probability holds require
    compatible data and can leave size unidentified. At $n=0$ both remaining
    coefficients are unidentified, and at $n=1$ dispersion is unidentified.
    Holding all coefficients only evaluates the specified law.
  reason: >-
    For $n\ge2$, the fixed-size family is a minimal finite exponential
    family and its polygon has consecutive-point upper edges and an endpoint lower edge.
    Saturated-likelihood comparisons establish the stated special cases;
    the free-size procedure bounds the tail and checks each remaining size,
    stopping when its numerical comparisons or work budget are insufficient.
classification: specialist
classification_reason: Used to model bounded counts with dispersion departing from independent Bernoulli trials.
summary: Conway--Maxwell--binomial provides a model for bounded counts with dispersion departing from independent
  Bernoulli trials.
aliases:
- "CMB"
- "COM-binomial"
- "CMP-binomial"
key_references:
- 2
---


The Conway--Maxwell--binomial distribution models bounded counts with a
trial count $n$, canonical probability $p$ and dispersion $\nu$. Dispersion
one gives the binomial law. The canonical probability is generally not
the mean divided by the trial count. Dispersion can be negative.

## Details

With a known trial count of at least two, the sufficient statistics are
$x$ and $\log\binom{n}{x}$. Their points form a convex polygon with
consecutive-point upper edges and an endpoint lower edge. An empirical
statistic in its interior has a unique finite joint fit. Other boundary
statistics approach point masses, adjacent two-point laws or endpoint
two-point laws. Endpoint point masses are also attained by $p=0$ or $p=1$.
Holding dispersion leaves a mean-matching probability fit. Holding an
interior probability instead leaves a strictly concave dispersion fit;
only the central mode or modes and the endpoints can support its escapes.
A constant sample elsewhere can therefore have a finite fit.

At trial count zero the law is the point mass at zero. At trial count one
it is Bernoulli, so dispersion is unidentified. Holding both probability
and dispersion leaves only an integer search if trial count is free.
When all coefficients are held there is no optimization.

Estimating trial count requires comparisons across different supports.
Tail and entropy bounds can certify accepted fits in exact arithmetic,
conditional on the computed comparisons and stated practical floating-point
margins. Work exhaustion or an unresolved limiting-model comparison stops
the search. General uniqueness across trial counts is not asserted.
The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md)
and the retained derivations below give the special sample conditions and
certificate scope.

## R notes

Supply `size`; a numeric value holds the design and `size = NULL` requests
an integer search. Either `prob` or `nu` can also be held. Sizes are limited
to $10^7$ for allocation safety. Endpoint probability holds require compatible
observations. With size zero, unidentified free `prob` and `nu` are represented
by 0.5 and one. With endpoint samples and free probability, free unidentified
dispersion is represented by one. At size one, unspecified dispersion is
set to one, warned about for interior probabilities, and marked fixed.
Nonattained limiting laws and unresolved faces stop with a diagnostic.
The detailed search budgets and stopping rules below apply to free-size fits.

### Detailed held-fit conventions

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) derives the stated limits and held-parameter conditions.

The size is a design quantity that can be held or searched over feasible integers. The canonical probability is generally not the mean divided by size. Dispersion is unrestricted in sign, and `nu = 1` is binomial. For held size the fitter profiles the canonical log odds by mean matching and solves the remaining dispersion moment equation. Finite log-sum-exp enumeration costs O(size) per evaluation; size is limited to 1e7 for allocation safety.

For size at least two, the joint MLE is attained when the mean sufficient statistic lies inside the convex polygon of the statistic points or is an endpoint vertex. Other boundary samples stop with a named limiting law. With fixed dispersion, endpoint point masses are attained. With fixed probability, dispersion can escape only toward the central mode(s) or the endpoints; constants elsewhere can have finite fits. With both coordinates fixed there is no optimization or escape. Size zero is the attained point mass at zero: unidentified free prob and nu are represented by 0.5 and one. Endpoint samples with free prob return prob zero or one, with unidentified free nu represented by one. Endpoint probability holds are accepted only for compatible observations. With size one, dispersion is unidentified: an unspecified dispersion is set to one with a warning for interior probabilities and marked fixed, leaving a Bernoulli fit.

With both `prob` and `nu` held, `size = NULL` enumerates feasible integer sizes until a decreasing upper bound excludes the entire remaining tail. For positive dispersion the bound uses a normalizer term at `max(x) + 1`; for negative dispersion it uses the endpoint terms. At zero dispersion the normalizer increases with size, so the smallest feasible size is optimal. This argument does not assume size-profile unimodality. `100000 * min(iterlim, 100000)` cumulative normalizer terms form the enumeration-cell budget; exhaustion stops rather than returning a capped estimate. Endpoint probabilities and size zero are handled exactly. With `nu` held and `prob` free, the separate search below handles the CMP face and bounds the remaining sizes. The free-dispersion search and its separate adjacent-sample bound are described below.

For a constant sample at `k`, free probability attains likelihood one at size `k`, probability one (or probability zero for `k = 0`). With probability held at an interior value, `k = 0` attains likelihood one at size zero. For `0 < k <= 5000000`, size `2*k` is supported and `nu -> +inf` concentrates its mass at `k`: the supremum is one and no finite member attains it. Held probability one attains the constant sample at size `k`; held probability zero is incompatible with positive observations. Positive constants above 5000000 with an interior held probability remain unimplemented because the central size exceeds the supported size cap.

If observations have equal positive counts at every integer from zero to
their maximum, the empirical uniform law is attained at that maximum as
size, probability `1/2` and dispersion zero. It is therefore a global fit
for free probability or probability held at `1/2`. For size one and free
probability it ties the adjacent CMP limit, so the face-tie policy stops;
with probability held the unidentified dispersion is represented by one.


## Decision history

### Free-size profile bounds

The following inequalities are exact-arithmetic results, not floating-point
roundoff certificates. Write `N` for size,
`a = logit(prob) + nu*log(N)`, `c_N(k) = sum_{j=0}^{k-1} log(1-j/N)`, and
`b_k(a) = k*a - nu*log(k!)`. For `N >= K >= max(x)`, retain only the
normalizer terms `0,...,K` and let `T_K(a)` be their log likelihood using
weights `exp(b_k(a))`. Then, for fixed `nu`,

```
log L_N(a) <= T_K(a) + D_N,K
D_N,K = nu * sum_i c_N(x_i) - n * min(0, nu*c_N(K)).
```

The bound follows because `c_N(k)` decreases in `k`; the smallest
`nu*c_N(k)` for `0 <= k <= K` bounds the retained normalizer from below.
Both forms of `D_N,K` are nonnegative and decrease to zero with `N`.
For fixed `K` they are `O(1/N)`, with leading term
`nu*(n*K*(K-1)-sum_i x_i*(x_i-1))/(2*N)` when `nu > 0`, and
`-nu*sum_i x_i*(x_i-1)/(2*N)` when `nu < 0`. Hence the supremum of `T_K`
over `a`, plus `D_N,K`, bounds every larger size. The truncated supremum is
a one-parameter concave exponential-family optimization and is an upper
bound on the positive-`nu` CMP face. For positive fixed `nu`, increasing
`K` makes it converge to that face for nonconstant data with positive mean;
a finite candidate strictly above the face can therefore be certified by
choosing a large enough `K`, finding a cutoff from the correction, and
checking every feasible size below it. This existence argument does not
bound the work needed or supply validated numerical comparisons.

For negative fixed `nu` there is also a direct bound: each observed
probability is at most `choose(N,x)^nu`, because the normalizer contains
both endpoint weights and `exp(x*eta) <= max(1,exp(N*eta))`. Thus, if at
least one observation is positive, the profile is at most
`nu*sum_i log choose(N,x_i)`, which decreases to minus infinity for
`N > max(x)`. At `nu = 0`, the weights are independent of `N` at fixed
odds, so the smallest feasible size maximizes the free-probability profile
as well. Constant samples can instead attain likelihood one at a size and
endpoint probability compatible with the observations.

For nonconstant samples spanning at least three integers there is also a
candidate-dependent bound on free dispersion. Let `u=min(x)`, `v=max(x)`,
`j=u+1`, `w=(v-j)/(v-u)`, and suppose a parameter value has total
log likelihood at least `B < 0`. Write `n_u,n_v` for the endpoint sample
counts, and
`delta = w*log(u!) + (1-w)*log(v!) - log(j!) > 0`.
Concavity of `c_N` and the identity
`log p(j) = w*log p(u) + (1-w)*log p(v) + nu*delta_N`, where
`delta_N = log choose(N,j) - w*log choose(N,u) - (1-w)*log choose(N,v)`,
give `delta_N >= delta` and
`nu <= -B*(w/n_u + (1-w)/n_v)/delta` whenever `nu > 0`.
The computed `delta` is reduced by `1e-10` times one plus the sum of the
absolute terms in its evaluation; a nonpositive result stops the search.
For `nu < 0` and `N > v`, the endpoint argument above gives
`nu >= B/(n_v*log(N))`, because `choose(N,v) >= N`. Thus any fit
beating the candidate has dispersion in a bounded interval, with its
negative part shrinking at rate `1/log(N)`. Substituting these bounds into
the truncated inequality yields a uniform `O(1/N)` correction at fixed
`K`, including for free dispersion. This is a bound on the relevant
superlevel set, not a classification of its limiting faces. Samples spanning
at most two adjacent values need separate boundary analysis.

The implementation tightens those bounds using the empirical saturated
log likelihood `S`. If category `v` occurs `c` times in `n` observations,
then setting its model probability to `p` and optimizing the other category
probabilities yields the upper bound

```
S + c*log(p/f) + (n-c)*log((1-p)/(1-f)),   f=c/n.
```

On a `B`-superlevel set, bisection on `(0,f)` gives a lower bound on `p`;
the lower bracket endpoint is retained, with a practical rounding margin.
Its logarithm replaces `B/c` in the dispersion inequalities above. This
coarsened relative-entropy bound is exact in real arithmetic and can be much
tighter when the finite candidate is close to saturation. The same binary
inequality on `(f,1)` bounds the probability of the intermediate point
`j` from above when it is observed. Otherwise its probability is at most
one minus the lower bounds at `u` and `v`. Replacing the coarse
`log p(j) <= 0` by the tighter applicable upper bound further reduces the
positive-dispersion limit. A negative numerator excludes positive dispersion
from the superlevel set; a nonpositive computed denominator stops the search.

With free probability and dispersion and observations on both `k` and
`k+1`, the saturated two-point likelihood is an upper bound for every
distribution. If `k=0`, size one attains it as a Bernoulli member and the
CMP adjacent limit ties it, so the fitter reports `FaceTie`. If
`k>0`, size `2k+1` has these two central points and a sequence with
`nu` tending to infinity and suitable fixed odds approaches their empirical
proportions. Every finite feasible CMB member has additional positive-mass
support points, so the saturated bound is not attained. This proves an
adjacent-face stop for that subset, including under the size cap whenever
`2k+1` is allowed; more generally a supporting slope can expose the same
edge at any feasible size at least `k+1`.

With free probability and sample support `{0,v}`, where `v>=2`, the
saturated two-point likelihood is also a strict upper bound for every
finite feasible CMB member. At size `v`, sending `nu` to negative infinity
and choosing the endpoint odds matches the empirical proportions, proving
that the endpoint face reaches this bound in the limit.

For held `prob=1/2` and equally frequent observations on the adjacent pair
`{k,k+1}`, the odd design `2k+1` gives equal central weights as
`nu -> +inf`. When that design is supported and `k>0`, the resulting
saturated likelihood is not attained; for `k=0`, size one attains it.
With equally frequent observations on `{0,v}`, the size-`v` endpoints
likewise have equal weights as `nu -> -inf`, yielding the saturated
likelihood as an unattained supremum for `v>=2`. These are exact special
cases; they do not settle the other held-probability adjacent samples.

For arbitrary positive counts `n0,n1` on `{0,1}` with held `prob=1/2`,
sizes one and two can be compared exactly. Size one has log likelihood
`-(n0+n1)*log(2)`. For every size at least two, symmetry gives `p(0)=p(N)`,
so maximizing over `p(0),p(1)` yields the upper bound
`n0*log(n0/(2*n))+n1*log(n1/n)`. Size two attains this bound with
`nu=log(2*n1/n0)/log(2)`. Comparing the two values proves the global fit;
if their computed values differ by at most `1e-12` times one plus their
largest absolute value, the fitter stops with numerical failure.
For `{1,2}` and held probability approximately `0.69830412636804606`, the
size-two profile and size-three central face tie numerically: the probability
is the root in `(0.6,0.8)` of `4*(1-p)*(p^2+(1-p)^2)-p=0`. The reported
tie does not exclude a larger finite maximum at another size.

At any fixed size at least two, `(x, log choose(size,x))` is a minimal
two-dimensional sufficient statistic: strict concavity of the log binomial
coefficients makes its points affinely independent. The likelihood is
strictly concave in log odds and dispersion. Its finite joint maximum exists
exactly when the empirical statistic lies inside their convex hull. The
upper hull consists of consecutive-point edges, and the lower hull is the
endpoint edge; vertices are point-mass limits, with endpoint vertices also
attained by endpoint probabilities. Holding interior probability leaves a
strictly concave one-dimensional likelihood, with finite dispersion exactly
when the empirical mean log binomial coefficient is strictly between its
minimum and maximum. A small score is a practical convergence check, but
alone does not supply a numerical upper bound for the remaining likelihood
error without additional curvature or parameter bounds.

More explicitly, the upper polygon edges join consecutive statistic
points, while its lower edge joins the endpoints. Thus a joint fit on an
upper edge approaches a freely weighted adjacent two-point law, and one on
the lower edge approaches a freely weighted endpoint law. With interior
probability held and log odds `eta`, the `nu -> -inf` endpoint law gives
probability `logistic(N*eta)` to `N`. For odd `N=2k+1`, the
`nu -> +inf` central law gives probability `prob` to `k+1`; for even `N`
the upper face is the point mass at `N/2`. Size one has constant
log-binomial statistic, so dispersion is not identified.

The certificate for the truncated two-coordinate profile uses
the following entropy dual. For `K >= max(x)`, put
`h_k = -log(k!)`, `m = mean(x)`, and `t = mean(h_x)`. If a probability
vector `q_0,...,q_K` satisfies `sum(q)=1`, `sum(k*q_k)=m`, and
`sum(h_k*q_k)=t`, then for every `a,nu`

```
T_K(a,nu) <= n * sum_k q_k * log(q_k).
```

This is the nonnegativity of the relative entropy of `q` against the
truncated exponential-family distribution; the right side is a global upper
bound even when `a,nu` do not maximize the likelihood. It also bounds the
held-probability subset because dropping a constraint can only increase a
supremum. In exact arithmetic, a candidate vector can be corrected to the
two empirical moments on any three affinely independent support points by
solving a three-by-three linear system. The result is a certificate only if
all corrected weights are nonnegative and the moment equations and entropy
comparison are checked with a stated floating-point margin. A small score
alone does not ensure nonnegative corrected weights, especially near a hull
face. Combining such an upper bound with the candidate-dependent dispersion
and `D_N,K` bounds above would exclude the complete large-size tail whenever
their sum is strictly below the finite candidate. The implementation uses
Newton iterations followed by a three-weight correction and rejects
negative weights or failed moment checks. The entropy comparison includes
`1e-9*(1+n*abs(entropy))` as a practical floating-point margin; it is not
an outward-rounded interval bound.

For adjacent observations `k,k+1` with fixed interior probability, write
`eta=logit(prob)` and suppose a likelihood exceeds `B<0`. Each observed
log probability is at least `B/n_k` or `B/n_(k+1)` respectively. Their
log ratio equals `eta + nu*log((N-k)/(k+1))`. Consequently, uniformly for
all `N>=N0>2k+1`, `abs(nu)` is at most

```
max(abs(B/n_(k+1)-eta), abs(-B/n_k-eta)) /
    log((N0-k)/(k+1)).
```

The implementation uses the tighter two-outcome version of this argument.
For fixed log ratio `d` of the two observed probabilities, the largest
possible likelihood places all mass on those outcomes and is
`n_(k+1)*d - n*log(1+exp(d))`. This is strictly concave in `d`. Bracketing
both roots of its `B`-superlevel interval bounds `abs(d-eta)` and hence
`abs(nu)` after division by the same logarithmic size factor. Retaining the
outer bracket endpoints and a numerical margin makes the comparison
conservative in exact arithmetic conditional on the computed values.

For fixed truncation `K>k+1`, the truncated likelihood at any such `nu`
is bounded above by its optimized `nu=0` geometric profile plus
`n*abs(nu)*log(K!)`: every truncated log-factorial lies between zero and
`log(K!)`. Adding the `D_N,K` correction and using the displayed bound on
`abs(nu)` gives a decreasing all-size upper bound. The optimized geometric
profile is bracketed by mean matching. Thus a strict finite improvement
can be certified without assuming unimodality in size, although the bound
can be very conservative. The finite odd-design central face has odds
`prob/(1-prob)` and is compared explicitly; size one is an attained
Bernoulli member. The free-probability adjacent cases are the exact
saturated-likelihood cases above.

For other nonconstant free-dispersion samples, a numerical candidate is sought through
32 sizes above the smallest feasible size. It must exceed any applicable
finite-design face by `1e-8*(1+abs(logLik))`; a close comparison reports a
tie and a face above the candidate reports an unresolved result. A
truncated-profile upper bound and decreasing correction exclude the tail;
blocks below the cutoff are excluded by a second bound or split until a
single size is checked. The bound also covers
the CMP, geometric and escaping-mass paths, without identifying which of
them is largest. For free probability, an additional numerical CMP fit
compares that face directly; if it fails, the conservative truncated bound
remains the tail certificate. At each finite size, a moment-corrected
entropy vector bounds the profile from above. The fixed-probability version
matches only the log-binomial moment and retains the known odds in its dual
bound. A finite-size candidate is accepted only when this upper bound is
within `1e-8*(1+abs(logLik))` of its reported likelihood. The free-size
fixed-design solves start at dispersion one and binomial mean-matching odds
(or the held odds), take Newton steps using the sufficient-statistic
covariance, cap the largest coordinate displacement at four and backtrack
on likelihood, and require the free moment residuals to meet `reltol`. A
singular covariance, unsuccessful line search, failed residual check or
iteration limit stops the search. The cumulative
work allowance is `100000 * min(iterlim, 100000)` charged support cells. Every fixed-size
Newton normalizer and final entropy table is charged by its support length;
truncated solves are charged separately. When a Newton solve fails, a
monotone dispersion bisection shares the same remaining allowance, with safeguarded
mean matching when probability is free; it must pass the same entropy check.
Nested mean matches spend the same cell ledger. These are work units,
not a count of every arithmetic operation. Beyond the exact special cases,
the current search stops when the required support or cumulative table work
exceeds its enumeration-cell budget; it does not return a size capped
at a work threshold.
Exhaustion, failed
moment checks, unsupported truncations or inability to establish the
cutoff stop without returning an estimate. Fixed-size moment solves and
floating-point comparisons are numerical; the entropy and tail inequalities
are exact-arithmetic arguments conditional on the stated comparisons.

For a block `L <= N <= R` with `L >= K >= max(x)`, write
`d(k)=c_R(k)-c_L(k)`. It is nonnegative and nondecreasing in `k`. Retain
only normalizer terms `0,...,K`, parameterize the odds by
`a=eta+nu*log(N)`, and relax `a` even when probability is held. The
truncated log likelihood at `N` is at most its counterpart at `L` plus
`nu*(sum_i d(x_i)-n*d(K))` when `nu < 0`, and at most its
counterpart at `L` plus `nu*sum_i d(x_i)` when `nu >= 0`. Equivalently,
using the established superlevel bounds `-negative <= nu <= positive`,
a uniform block upper bound is the entropy-dual upper bound for the
truncated profile at `L` plus
`max(positive*sum_i d(x_i), negative*(n*d(K)-sum_i d(x_i)))`.
The entropy vector matches the observed mean and mean of
`-log(k!)+c_L(k)`. If the block upper bound is below the incumbent minus
the comparison margin, no size in it can improve the incumbent; otherwise
the block is bisected, and a singleton is evaluated and certified as above.
The tail cutoff still excludes every larger size. Each truncated solve and
block correction is charged to the same work budget. The bound is proved in
exact arithmetic, conditional on the computed comparisons; it uses the
existing entropy allowance and an additional `1e-9*(1+abs(anchor)+abs(correction))`
floating-point margin, not outward-rounded intervals. It does not assume a
unimodal size profile.

For the fixed-size entropy check, if `h_k=log choose(N,k)`, a feasible
vector matching both empirical moments gives the same entropy upper bound.
With held log odds `eta`, it suffices to match only the `h` moment: the
upper bound becomes `n*(sum(q*log(q)) + eta*(mean(x)-sum(k*q_k)))`.
The implementation corrects three or two probabilities respectively and
rejects negative weights and failed moment checks. These upper bounds make
the score test a convergence check, rather than using the score alone as
the likelihood-error certificate.

The [numerical illustration](../../../../../dev/research/evaluations/2026-11-02-cmb-free-2-data/entropy_certificate.py)
uses `x = (0,1,2,2,3,4)` and `K = 8`, with SciPy BFGS followed by the
three-point correction and a grid of 806 parameter pairs. At revision
`6fea876e` plus the current uncommitted changes it gave minimum corrected
weight `0.000530335056`, maximum moment residual `1.11e-16`, and a
likelihood upper bound of `-10.0251201259`. This single floating-point
example checks the construction; it is not a rounding-error proof or
evidence that the correction succeeds in all regimes.

The positive-dispersion CMP face, the zero-dispersion geometric face and
negative-dispersion escaping-mass paths are reachable as described in the
[earlier analysis](../../../../../dev/research/evaluations/2026-10-30-holds-cmb-report.md).
This does not classify every joint limit with free dispersion. In particular,
the initial bound is not uniform in unrestricted `nu`; the candidate-dependent
and adjacent bounds above address that issue without classifying every path.
For `{nu}`, a complete implementation
uses the fixed-dispersion bound. It evaluates the CMP face by mean matching,
explores 256 sizes, and reports an unresolved face or numerical tie if no
strict improvement is found. The comparison margin is `1e-9*(1+abs(face))`;
this is a practical floating-point allowance, not a proved rounding bound.
When the candidate exceeds the face, the algorithm increases the truncation
point until the truncated profile lies strictly below the candidate, finds a
tail cutoff, and checks every feasible size below it using the block bound
and individual profiles described below. It charges the
support length per profile against the cumulative enumeration-cell budget and stops on
exhaustion, solver failure, or the allocation domain; exhaustion of the cumulative enumeration budget also
stops. For negative dispersion it enumerates until the endpoint bound falls
below the best fit. At zero dispersion the first feasible size is optimal.
The floating-point profile solves, CMP face and comparisons are numerical;
the truncation and monotonicity arguments are exact-arithmetic statements.
A face comparison without a strict improvement cannot rule out a larger
finite-size maximum.

## d/p/q/r

Finite log normalization is reused for tables; the binomial is the nu = 1 member. The size cap is 1e7.

### Exact dpqr submodels (TASK-134)

At size zero or probability zero/one, d/p/q evaluate the atom directly without
constructing the enumeration table. Quantile endpoint queries retain the
nominal design interval `[0, size]`. At `nu = 1`, d/p/q delegate to the binomial
implementation: the binomial theorem supplies its normalizer. Apart from
these and the geometric submodel below, members retain finite enumeration; no new complexity guarantee is made at the size cap
for nondegenerate, non-binomial members. Seeded sampling retains its existing
inverse-transform path and atomic shortcuts.

At `nu = 0`, weights are proportional to `exp(k*eta)` with
`eta = log(prob)-log1p(-prob)`. The normalizer is the finite geometric
sum `(1-exp((size+1)*eta))/(1-exp(eta))`; positive eta is handled by
reflection, and eta zero uses the uniform limit. Direct lower and upper
partial sums use `log1mexp` before quantile bisection on the finite design
support. Size one is binomial for every nu. These algebraic reductions are
exact; agreement of their floating-point evaluations with enumeration is
checked numerically.

## Stop details

Held-probability endpoint faces use the support-width power of the held odds. Free-size comparisons retain the evaluated finite witness and fitted CMP or two-point face when available; unresolved tails have infinite global upper bounds.

Fixed-size normalizers, nested mean matches and the free-size rescue share the enumeration ledger with their caller. A normalizer table is charged by its support cells, including its fixed number of moment/tail passes. Budget witnesses retain a feasible represented probability; the bound $|\partial\ell/\partial\eta|\le nN$ covers conversion from canonical log odds to that probability, in addition to the practical rounding margin. The CMP-face kernel spends the same remaining 64-bit enumeration ledger.

If a constant sample with held interior probability needs its central design
beyond the allocation domain, the endpoint face at the smallest feasible size
is retained as a lower witness; the remaining size comparison stops unresolved.

The default iteration allowance is 2000 so the fixed-dispersion finite-size examples and seeded CMB/binomial samples retain certified fits under shared table-cell accounting.

## Fixed-size coefficient acceptance

For held size at least two and interior probability, the public fixed-size
entry point checks every free subset in canonical coordinates
$(\eta,\nu)=(\operatorname{logit}(p),\nu)$. Holding probability or dispersion
selects the corresponding affine line. The finite sufficient-statistic table
supplies the likelihood, score and inverse covariance used by
[acceptance.hpp](acceptance.hpp). The resulting probability and dispersion enclosure must be
within $10^{-8}(1+|\hat\theta|)$ of each reported free coefficient.
The canonical half-space and likelihood calculations, including their
practical floating-point margins, are specified in that header.

A passing historical proposal is retained exactly. If its coefficient enclosure
fails, refinement proposals at moment tolerances $10^{-14}$, $10^{-13}$ and
$10^{-12}$ share the same remaining table-cell ledger. The first that passes
its own certificate is retained. In particular,
a small log-binomial residual at large size need not imply accurate dispersion.
The certificate uses at most `min(iterlim,100000)` refinements, with two or
three tables per refinement. Every table is charged to the existing enumeration
allowance. Exhaustion retains available witnesses as a budget stop; other failed
certificates stop numerically. No support or dispersion cap becomes an estimate.

Size zero, endpoint probabilities and Bernoulli size one use their exact law
reductions. They also supply counterexamples to coefficient uniqueness:
any dispersion gives the same size-one law, and at size zero both probability
and dispersion are unidentified. Free-size comparisons also have nonunique examples: all-zero counts with free
probability, or probability held at zero, have likelihood one at every size.
The fitter reports a finite-size tie with size-zero and size-one witnesses.

## Free-size acceptance

Every numerical finite winner shares the search's remaining enumeration ledger
with the fixed-size acceptance and refinement above. Exact uniform and
size-two symmetric solutions pass the same gate; atomic and Bernoulli laws
use their exact reductions. Failure retains the finite candidate and available
face witnesses, with an infinite global upper bound. Refinement never resets
the work allowance. A runner-up among the evaluated distinct sizes is retained;
overlap of the likelihood enclosures `logLik +/- 1e-8*(n+abs(logLik))`
stops as a numerical size comparison, with both witnesses. This is a
conservative unresolved comparison, not a proof of exact equality. The
enclosures cover the total profile likelihood errors.
Tail and block exclusions compare against a feasible incumbent's lower anchor,
so they do not require coefficient accuracy at discarded sizes.

For positive held dispersion, a block `L <= N <= R`, `L >= K`, has upper
bound equal to the scalar truncated-profile upper bound at `L` plus
`nu*sum_i (c_R(x_i)-c_L(x_i))`. Here
`c_N(k)=sum_{j<k} log(1-j/N)` and the canonical coordinate is
`a=eta+nu*log(N)`. The difference `c_N(k)-c_L(k)` is nonnegative,
so the truncated normalizer at `N` is at least that at `L`; dropping its
increase gives the displayed bound. Replacing the observed differences by
their values at `R` bounds the entire block. Blocks that cannot be excluded
are bisected down to individually evaluated sizes. No size unimodality is
assumed. The scalar truncated likelihood is concave in `a`; a bracket of
its mean-matching root gives the upper allowance `n*K*bracket_width`.
Each evaluated normalizer is charged by `K+1` cells; its fixed sufficient
statistics are computed once. The block comparison adds
`1e-9*(1+abs(anchor)+abs(correction))`. These are exact-arithmetic bounds
conditional on computed comparisons, with practical floating-point margins.
