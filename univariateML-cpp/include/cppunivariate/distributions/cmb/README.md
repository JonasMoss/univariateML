---
id: cmb
inference_numeric:
  - regime: size zero
    available: true
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with dispersion known"
    available: true
    condition: "known size = 1; 0 < prob < 1 free and finite nu held; the Bernoulli probability is identifiable; dispersion is not"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with dispersion free"
    available: true
    condition: "known size = 1; nu free; DQM and pointwise C2 hold but the dispersion score is identically zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
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
    size: "Required number of trials; NULL estimates it when nu is held. Fixed sizes are limited to 1e7 for allocation safety."
    prob: "Optional fixed canonical probability between zero and one inclusive; NULL estimates it."
    nu: "Optional fixed finite real dispersion; NULL estimates it. Both prob and nu may be held fixed."
    reltol: "Relative tolerance on the dispersion moment residual."
    iterlim: "Maximum number of dispersion bisection iterations."
log_density: "nu*(loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)) + x*log(prob) + (size-x)*log(1-prob) - log(Sum((factorial(size)/(factorial(k)*factorial(size-k)))**nu * prob**k * (1-prob)**(size-k), (k, 0, size)))"
limits:
  - model: cmp
    when: '$size \to \infty$ with positive $\nu$ fixed and odds scaled as $size^{-\nu}$'
    detect: exploratory search finds no strict improvement; a larger finite maximum is not excluded
    status: FaceUnresolved
    fixed: [nu]
    firmness: proved
    numerical: true
  - name: point mass
    when: '$\nu$ diverges along an exposing direction toward an interior point mass'
    detect: constant interior data in a joint fit; central constant data with interior prob fixed
    status: PointMass
  - name: adjacent two-point law
    when: '$\nu \to +\infty$ with the canonical probability adjusted'
    detect: size at least two, with both adjacent integers observed in a joint fit; only the two central integers with interior prob fixed
    status: AdjacentLimit
  - name: endpoint two-point law
    when: '$\nu \to -\infty$'
    detect: all data in {0, size} with both present in a joint fit; any endpoint-only data with interior prob fixed
    status: EndpointLimit
references:
- 'Kadane, J. B. (2016). Sums of Possibly Associated Bernoulli Variables: The Conway-Maxwell-Binomial Distribution.
  Bayesian Analysis. doi:10.1214/15-BA955.'
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds:
- held: []
  status: not-implemented
  reason: "Free size with both coordinates free requires a global truncated two-coordinate optimizer and comparison with finite-size dispersion-limit faces; no reliable stopping rule is established."
- held:
  - prob
  status: not-implemented
  reason: "Free size with prob held requires a global truncated dispersion optimizer and held-probability limit-face comparison, including finite-size dispersion limits; no reliable stopping rule is established."
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
  method: multistart
  certified: partial
  note: 'cmb/README.md, Details; cmb/mle.hpp (public fitter and held overloads); cmb/test.cpp: Moment matching exploits
    exponential-family convexity for fixed design/dispersion subsets; integer-tail or normalizer bounds certify
    only their stated scope, not every joint numerical solve.'
areas:
- counts and ecology
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

The Conway--Maxwell--binomial distribution extends the binomial by raising its combinatorial factor to a real dispersion power.

## Details

The size is a required design quantity. The canonical probability is generally not the mean divided by size. Dispersion is unrestricted in sign, and `nu = 1` is binomial. The fitter profiles the canonical log odds by mean matching and solves the remaining dispersion moment equation. Finite log-sum-exp enumeration costs O(size) per evaluation; size is limited to 1e7 for allocation safety.

For size at least two, the joint MLE is attained when the mean sufficient statistic lies inside the convex polygon of the statistic points or is an endpoint vertex. Other boundary samples stop with a named limiting law. With fixed dispersion, endpoint point masses are attained. With fixed probability, dispersion can escape only toward the central mode(s) or the endpoints; constants elsewhere can have finite fits. With both coordinates fixed there is no optimization or escape. Size zero is the attained point mass at zero: unidentified free prob and nu are represented by 0.5 and one. Endpoint samples with free prob return prob zero or one, with unidentified free nu represented by one. Endpoint probability holds are accepted only for compatible observations. With size one, dispersion is unidentified: an unspecified dispersion is set to one with a warning for interior probabilities and marked fixed, leaving a Bernoulli fit.

With both `prob` and `nu` held, `size = NULL` enumerates feasible integer sizes until a decreasing upper bound excludes the entire remaining tail. For positive dispersion the bound uses a normalizer term at `max(x) + 1`; for negative dispersion it uses the endpoint terms. At zero dispersion the normalizer increases with size, so the smallest feasible size is optimal. This argument does not assume size-profile unimodality. Four million cumulative normalizer terms form a work budget; exhaustion stops rather than returning a capped estimate. Endpoint probabilities and size zero are handled exactly. With `nu` held and `prob` free, the separate search below handles the CMP face and bounds the remaining sizes. The two free-dispersion profiles remain unavailable.

### Free-size profile bounds (2026-11-01)

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
For `nu < 0` and `N > v`, the endpoint argument above gives
`nu >= B/(n_v*log(N))`, because `choose(N,v) >= N`. Thus any fit
beating the candidate has dispersion in a bounded interval, with its
negative part shrinking at rate `1/log(N)`. Substituting these bounds into
the truncated inequality yields a uniform `O(1/N)` correction at fixed
`K`, including for free dispersion. This is a bound on the relevant
superlevel set, not a classification of its limiting faces. Samples spanning
at most two adjacent values need separate boundary analysis.

The positive-dispersion CMP face, the zero-dispersion geometric face and
negative-dispersion escaping-mass paths are reachable as described in the
[earlier analysis](../../../../../dev/research/evaluations/2026-10-30-holds-cmb-report.md).
This does not classify every joint limit with free dispersion. In particular,
the initial bound is not uniform in unrestricted `nu`. The superlevel-set
bound needs a global truncated two-coordinate optimization and careful
handling of adjacent samples and held-probability limit faces before it can
support `{}` or `{prob}`. For `{nu}`, a complete implementation
uses the fixed-dispersion bound. It evaluates the CMP face by mean matching,
explores 256 sizes, and reports an unresolved face or numerical tie if no
strict improvement is found. The comparison margin is `1e-9*(1+abs(face))`;
this is a practical floating-point allowance, not a proved rounding bound.
When the candidate exceeds the face, the algorithm increases the truncation
point until the truncated profile lies strictly below the candidate, finds a
tail cutoff, and enumerates every feasible size below it. It charges the
support length per profile against a ten-million-term budget and stops on
exhaustion, solver failure, or the allocation cap; truncation above 8192 also
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
