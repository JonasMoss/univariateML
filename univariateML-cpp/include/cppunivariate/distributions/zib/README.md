---
id: zib
inference_numeric:
  - regime: "size zero"
    available: false
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "known unit binomial probability"
    available: true
    condition: "known integer size >= 1, prob = 1 held, 0 < pi < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "binomial member with inflation known"
    available: true
    condition: "integer size >= 1 held, 0 < prob < 1 free, pi = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "known integer size >= 1, 0 < prob < 1 free, pi = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "size one with one coefficient known"
    available: true
    condition: "known size = 1; exactly one of prob and pi free, the other held in (0,1); the remaining scalar Bernoulli parameter is identifiable"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with both coefficients free"
    available: false
    condition: "known size = 1; prob and pi both free in (0,1); DQM and pointwise C2 hold but the joint information has rank one"
    reason: "The two free probability coefficients have rank-one information; covariance is singular"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer size >= 2 held; 0 < prob < 1 and 0 < pi < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-inflated binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  integer: yes
  simulate: 10
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzib
  sampler: rzib
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per global certificate: 128 * min(iterlim, 100000) evaluations; per integer scan: 100000 * min(iterlim, 100000) enumeration cells."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
    pi: "Optional fixed pi in `[0, 1]`; NULL estimates it."
log_density: Piecewise((log(pi+(1-pi)*(1-prob)**size), Eq(x,0)), (log(1-pi)+loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob), True))
limits:
- model: zip
  firmness: proved
  when: 'The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive rate.'
  detect: 'The relaxed zero-modified signed-tail and finite-prefix certificate proves an admissible ZIP face wins; samples without zeros delegate to the ordinary binomial certificate. Noninflated faces also use a compact rate certificate that proves inflation is inactive near the Poisson optimum. Other comparisons remain unresolved.'
  status: ZipLimit
- model: zip
  firmness: proved
  when: 'The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive rate.'
  detect: 'Zero inflation or a sample without zeros delegates to the binomial face decision, retaining held inflation. With observed zeros and nonzero held inflation, an inflation-uniform or localized signed rate bound and complete prefix exclusion prove the held ZIP face wins; otherwise the comparison remains unresolved.'
  status: ZipLimit
  fixed:
  - pi
- model: zip
  firmness: proved
  when: As trial count tends to infinity and success probability tends to zero with their product fixed, the law tends to zero-inflated Poisson with the same inflation.
  detect: Signed tail and complete finite-prefix exclusion prove escape, while the held-inflation ZIP certificate cannot order multiple face-rate maxima at its comparison margin.
  status: FaceTie
  fixed:
  - pi
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlzib())
  mlzib(x, size = 10)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-inflated
    mass: pi
fit:
  method: multistart
  certified: partial
  note: 'zib/README.md, Details; zib/mle.hpp; zib/test.cpp: Certified finite free-size winners require constrained retained-support tail exclusion and complete integer-prefix coverage; held-inflation ZIP witnesses use the ZIP certificate. Free inflation inherits admissible signed positive-Poisson face certificates; zero inflation and no-zero samples reduce to binomial. Held inflation uses an inflation-uniform normalized-pmf tail for sufficiently dispersed positive counts and a localized rate-interval certificate otherwise; globally winning tied ZIP faces retain both rate candidates; noninflated free-inflation faces use a binomial submodel certificate and an explicit zero-mass bound. Unclosed higher-order signs and near-branch comparisons return the successful finite profile uncertified.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model bounded success counts with an additional structural-zero component.
summary: Zero-inflated binomial provides a model for bounded success counts with an additional structural-zero component.
aliases:
- "ZIB"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, all-zero data attain a nonunique maximum. For
    positive data, finite-size attainment must be distinguished from a supremum on
    the infinite-size zero-inflated Poisson boundary. Retained-support certificates
    settle strict finite winners within their work budgets. Signed positive-count
    certificates also settle admissible free-inflation ZIP faces; remaining
    comparisons stop explicitly. With size held at an integer at
    least the largest count, a maximum always exists for compatible data, but it
    need not be unique. With $0<p<1$ held, a finite integer maximum exists.
    Holding only inflation retains the infinite-size comparison with that same
    inflation. Held $p=0$ or $\pi=1$ permits only zeros. Held $p=1$ requires every
    positive count to equal the size. Size zero permits only zeros and leaves both
    probabilities unidentified. Holding all coefficients leaves likelihood
    evaluation.
  reason: >-
    At a fixed size the probabilities range over a compact square and the
    likelihood is continuous, so a maximum is attained. For size one the law
    depends only on $(1-\pi)p$, giving nonunique pairs when both probabilities are
    free. With $0<p<1$ held and positive data, every observed positive-count
    probability tends to zero as size increases, so the integer tail cannot
    prevent attainment. With both probabilities held and all counts zero, the
    smallest compatible size attains the maximum. When size and probability are
    free, a finite-mean path to infinite size has the zero-inflated Poisson
    boundary. A retained-support bound and complete integer-prefix coverage certify
    returned finite winners; they do not establish a general face-win criterion or
    uniqueness.
---

The zero-inflated binomial distribution mixes a binomial count with a structural-zero component of probability $\pi$. The integer size $n$ is the number of trials and $p$ is the base success probability.

## Details

The [binomial-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) records the boundary and held-subset arguments.

Holding size removes the infinite-size escape. The binomial member at $\pi=0$ remains attained. Size zero gives the point mass at zero and requires all-zero data. At size one, only the product $(1-\pi)p$ is identified when both probabilities are free.

A held probability is preserved. With $0<p<1$ and at least one positive count, the integer search bounds the remaining likelihood tail before returning. An unfinished integer tail comparison returns the best successful finite profile uncertified. Boundary probabilities must assign positive likelihood to the observations.

Holding inflation leaves a scalar probability profile at each integer size. Its stationary points are isolated using a quadratic turning-point equation. Root brackets must resolve relative to the probability: an absolute-width test can accept an unchecked tiny probability at a large held size. An unfinished solve stops numerically and retains its evaluated finite witness. The turning-point polynomial is formed in $u=Np$, with coefficients $n_{\mathrm{obs}}n_+$, $-S(n_{\mathrm{obs}}+n_+)-Sn_0/N$ and $S^2+Sn_0$, where $S$ is the count sum and $n_0,n_+$ are zero and positive sample counts. Differencing polynomial values in probability coordinates loses its linear coefficient at large size. Discriminants within $4096\epsilon(1+b^2+4ac)$ of zero leave branch isolation uncertified. Multiplying the discriminant by integer $N^2$ clears denominators; when all positive integer terms fit the working mantissa with a two-bit reserve, an exact sign recheck settles the turning comparison. Certification also requires opposite padded score signs within a relative probability band and a separated competing likelihood; an unresolved nearly multiple root remains uncertified. Otherwise the existing score brackets, additionally split at the quadratic vertex, still supply stationary candidates; a successful finite solve is returned with `certified = FALSE`. The necessary interval $[S/(n_{\mathrm{obs}}N),S/(n_+N)]$ does not introduce parameter boundaries: its interior endpoints must not replace a stationary point merely because their likelihoods round alike. Only $p=1$ can compete as a physical upper boundary. Zero mixtures also need log-scale arithmetic. For $\pi=2^{-1074}$, $N=1075$ and $p=1/2$, the base zero mass is $\pi/2$ and the inflated mass is $3\pi/2-\pi^2/2$; materializing the base mass as a double before mixing can change its logarithm by an order-one amount. The score needs a common log scale for the same reason. With no zeros, inflation is zero when free and contributes only a constant when held; the ordinary-binomial mean fit applies. The infinite-size comparison uses the zero-inflated Poisson boundary with the same held inflation.

When size and probability are free, the grid supplies a proposal. Success requires the retained-support correction $D_{N,K}$ from [ztbinom](../ztbinom/README.md), retaining $0,\ldots,K$, and complete evaluation or bounded block exclusion of all feasible sizes below the resulting tail cutoff. Positive counts have the same correction bound. At zero, the inflated mass is at most $e^{-c_N(K)}[\pi+(1-\pi)/Z_K]$, so the same total correction applies with inflation held or free.

The positive-mean root is checked before deciding whether the unconstrained inflation is feasible; otherwise an inaccurate tiny probability could select the wrong branch.

The retained likelihood factors into the positive conditional likelihood and a Bernoulli zero-indicator likelihood. On a log-rate interval the first has a concave tangent upper bound. The second is maximized over its attainable zero-mass interval: $[\pi+(1-\pi)/Z_K(\eta_+),\pi+(1-\pi)/Z_K(\eta_-)]$ when inflation is held and $[1/Z_K(\eta_+),1]$ when free. Subdivision bounds all branches without assuming unimodality. The base conditional mean brackets every maximum between the full and positive sample means. The work units and floating-point margins are those of the shared retained-support certificate.

The held-inflation ZIP face uses [ZIP's root certificate](../zip/README.md). When its two rate peaks cannot be ordered, a completed signed-tail and finite-prefix certificate still proves that no finite size wins. The stop then has kind `tie`, finite global likelihood bounds and both ZIP rate intervals; no exact equality is asserted for the supplied floating-point inflation. With free inflation, dropping its nonnegativity constraint gives the zero-modified likelihood as a global upper bound. If its positive-count signed certificate proves a winning positive-Poisson face, and that face has admissible inflation and agrees with the fitted ZIP face, the bound is attained in the closure and proves the ZIP limit. Holding inflation at zero delegates to the ordinary binomial certificate. A sample without zeros has the same binomial profile, plus the constant $n\log(1-\pi)$ when inflation is held; free inflation is then zero. There is also a tail bound uniform in held or free inflation. Write $S=\sum x_i$, $k=n_+$, $v=S/k$ and $G=\sum x_i(x_i-1)-S^2/k$. At every sufficiently large-size probability maximum, $u=Np\in[S/n,v]$. For $N\ge2v$, direct normalized-pmf comparison with ZIP at rate $u$ gives

$$\ell_N-\sup\ell_{\mathrm{ZIP}}\le-\frac{G}{2N}+\frac{C}{N^2},\qquad C=Sv^2+n_0v^4/32.$$

Thus $G>0$ excludes every size beyond $\max(\max x_i+1,2v,4C/G)$, uniformly in inflation; the held face comes from ZIP's all-branches certificate. Complete finite-prefix coverage is required to certify an escape. The zero-mass bound uses the fact that the second derivative of $\log(1-w+w e^t)$ is at most $1/4$. When $G$ is nonpositive and inflation is held, the implementation subdivides $[S/n,v]$. On each interval it bounds the actual coefficient $A(u)=-\sum x_i(x_i-1)/2+uS-(k+n_0w(u))u^2/2$, with $w(u)=(1-\pi)e^{-u}/[\pi+(1-\pi)e^{-u}]$. A negative coefficient bound gives a signed tail relative to the certified global ZIP face. Other intervals need a strict ZIP likelihood gap; their upper bounds combine the concave positive-count tangent with the convex zero-term chord. The resulting finitely many cutoffs cover the entire necessary rate interval, including every face mode. Near-winning points without a negative coefficient margin retain an unresolved comparison. For a free-inflation face with $\pi=0$, the fitter first checks the ordinary binomial submodel. A finite improvement becomes the incumbent for full ZIB certification. If the binomial certificate proves Poisson escape, set $z=n_0/n$ and $t=-\log z$. On a rate interval $[a,b]$ with $b<t$, the bound $(1-u/N)^N\ge\exp(-b-b^2/N)>z$ for $N\ge\max(2v,2b^2/(t-b))$ forces the profiled inflation to zero. The binomial certificate then excludes the entire interval. Other rate intervals need a strict face-likelihood gap; an upper bound combines a tangent to the positive conditional Poisson likelihood in log rate with the best zero-indicator likelihood over $q\ge e^{-b}$. Subdivision covers the full necessary rate interval. Higher-order equality cases and nearly active branch comparisons without a strict numerical margin remain unresolved. Unclosed face comparisons preserve both witnesses internally and return the finite profile uncertified. For data on $\{0,1\}$, size one is saturated; with inflation held, choose $p=\min(1,\bar x/(1-\pi))$. This also attains the best constrained zero frequency when held inflation exceeds the observed zero fraction. With both probabilities free only their product is identified.

## R notes

Supply size explicitly: a number holds it and `size = NULL` estimates it. Supply prob or pi to hold either probability. With both probabilities free, all-zero data return `prob = pi = 0`, preserving a held size or choosing size one when size is free.

For all-zero data with free size and either probability held, the fitter returns size zero, preserving held values and choosing zero for a free probability. The maximum log likelihood is exactly zero. No all-zero free-size subset requires an escape or non-identification stop: as with the existing all-free convention, the fitter returns an attained representative with status `Ok`, without claiming unique coefficients.

Indeed, the zero mass is $\pi+(1-\pi)(1-p)^n\leq1$, with equality exactly when $n=0$, $p=0$, or $\pi=1$ (interpreting size zero as the point mass even at $p=1$). With both probabilities held, size zero is unique if $p>0$ and $\pi<1$; otherwise every size maximizes. With only $p>0$ held, the maximizing set is $n=0$ or $\pi=1$; with $p=0$ held, both free coefficients are unidentified. With only $\pi<1$ held, the maximizing set is $n=0$ or $p=0$; with $\pi=1$ held, both free coefficients are unidentified. With neither held, the maximizing set is the union of all three faces, so no individual coefficient has a unique maximizing value. At the returned size-zero representative every free probability is unidentified. These are exact boundary statements, requiring no numerical size search.

The size grid has no fixed magnitude cap. After its initial grid depth it continues until more than ten exponential steps pass without improvement, subject to the iteration budget `iterlim`. A failed probability solve stops. Certificate budget or comparison resolution failures return the successful finite profile with `certified = FALSE`, retaining the ZIP comparison witness internally.

## Decision history

The [2026-10-29 tail investigation](../../../../../dev/research/evaluations/2026-10-29-tail85-report.md) records comparisons with independent reference-density multistart oracles, including tied and near-Poisson samples. The [2026-10-31 size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) reduces the free profile to positive-binomial and binomial branches and identifies the remaining certification gap. These comparisons support the implementation on tested samples rather than proving globality.

The [2026-11-02 all-zero correction](../../../../../dev/research/evaluations/2026-11-02-zib-zero-report.md) records the size-one non-maximizer with both probabilities held and the exact size-zero replacement.

With probability held, the size scan charges every falling-factorial term in the positive-count likelihood. A large-count prefix can therefore exhaust the allowance after only a few candidate sizes; its finite result is returned uncertified rather than leaving an effectively unbounded scan.

With size two, inflation zero and observations $1,1,2$, the ordinary-binomial reduction has the unique probability $2/3$. Its exact real-valued maximum is known, but the nearest double misses a relative $10^{-18}$ coefficient target. A closed likelihood or size comparison cannot supply that missing probability-precision certificate.

For a ZIP face with $z>0$ observed zeros, $k>0$ positive observations and held rational $0<\pi<1$, write $S=\sum x_i$ and $T=\sum x_i(x_i-1)$. At an interior stationary rate $\lambda$, the posterior base-component probability of a zero is $w=(1-\pi)e^{-\lambda}/[\pi+(1-\pi)e^{-\lambda}]$, and the score gives $S=\lambda(k+zw)$. The first binomial profile coefficient is $(\lambda S-T)/2$. If it vanished, $\lambda=T/S$ would be a nonzero rational (the case $T=0$ would force the inadmissible rate zero). The score would make $w$ rational, and then $e^{-\lambda}=\pi w/[(1-\pi)(1-w)]$ would be rational, contradicting Lindemann's theorem. Thus exact equality is excluded for valid binary held inflation on a mixed sample. This supplies neither a usable separation from zero nor integer-prefix exclusion; the reductions with $\pi=0$ or no observed zeros are outside this argument.


### Nonzero masses at large trial counts

At zero inflation, every positive mass is exactly binomial. Its independent rising-factorial normalizer can nevertheless lose several digits near a concentrated large-n fit. Delegating the positive mass to the centered binomial density preserves this identity; the zero-mass mixture still uses its separately computed complementary tails.
