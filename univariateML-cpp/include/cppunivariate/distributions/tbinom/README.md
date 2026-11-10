---
id: tbinom
inference_numeric:
  - regime: "singleton retained support"
    available: false
    condition: "known design and truncation bounds with exactly one retained integer; probability free in (0,1); the conditional law is a point mass, so information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer size, known a and b held; 0 < prob < 1 free; retained support has at least two integers"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Truncated binomial
support: {type: integer, lower: "max(0, floor(a)+1)", upper: "min(size, floor(b))", closed: [true, true]}
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: a
  symbol: 'a'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 0
  kind: design
- name: b
  symbol: 'b'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 10
  kind: design
- name: size
  symbol: 'n'
  role: known design quantity
  domain: '[0, inf)'
  integer: yes
  simulate: 10
  kind: design
r:
  density: dtbinom
  sampler: rtbinom
  control:
    prob: "Optional fixed parameter; NULL estimates it."
    a: "Required lower bound; NULL estimates it as min(x) - 1 (strict truncation)."
    b: "Required upper bound; NULL estimates it as max(x)."
    size: "Required number of trials; NULL estimates integer size, with probability held or free."
log_density: "x*log(prob)+(size-x)*log(1-prob)-loggamma(x+1)-loggamma(size-x+1)-log(Sum(exp(k*log(prob)+(size-k)*log(1-prob)-loggamma(k+1)-loggamma(size-k+1)), (k, Max(0, floor(a)+1), Min(size, floor(b)))))"
limits:
- name: point mass at lowest support point
  firmness: proved
  when: 'With size fixed, $p\to0$ on a nonsingleton window whose lowest retained count is positive.'
  detect: 'Size is held, probability is free, and every observation equals the strictly positive lowest count of a nonsingleton retained window.'
  status: LowerPointMass
- name: point mass at highest support point
  firmness: proved
  when: 'With size fixed, $p\to1$ on a nonsingleton window whose highest retained count is below $n$.'
  detect: 'Size is held, probability is free, and every observation equals the highest count of a nonsingleton retained window below $n$.'
  status: UpperPointMass
- name: point mass at highest support point
  firmness: proved
  when: 'The size satisfies $n\to\infty$ with $0<p<1$ held and a finite upper truncation bound.'
  detect: 'Size is free, probability is held strictly between zero and one, and constant data equal the finite upper truncation edge and no feasible size can make the retained support singleton.'
  fixed:
  - prob
  status: UpperPointMass
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the comparison remains unresolved.'
  fixed: []
  status: TpoisLimit
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the comparison remains unresolved.'
  fixed: [a]
  status: TpoisLimit
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the comparison remains unresolved.'
  fixed: [b]
  status: TpoisLimit
- model: tpois
  firmness: proved
  when: 'The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window.'
  detect: 'A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the comparison remains unresolved.'
  fixed: [a, b]
  status: TpoisLimit
fit_args:
  a: 0
  b: 10
  size: 12
examples: |
  set.seed(1)
  x <- rml(200, mltbinom())
  mltbinom(x, a = 0L, b = 10L, size = 10L)

group: modified counts
construction:
- truncation
relations:
  truncation_of:
  - family: binom
    at:
      size: size
      prob: prob
fit:
  method: integer-search
  certified: partial
  note: 'tbinom/README.md, Details; tbinom/mle.hpp; tbinom/test.cpp: Finite free-size winners require retained-support tail exclusion and complete integer-prefix coverage for finite or infinite windows. Exact constant, two-point and untruncated reductions remain. Exact empirical Poisson proportions on a finite window attain the multinomial bound. Positive factorial-moment gaps permit signed tails and complete prefix exclusion; Exact finite-window factorial-moment dominance can settle zero gaps; a mean-preserving second-order law also certifies negative second-order gaps; exact zero second-order gaps admit a checked rational equality and a signed third-order remainder, still requiring complete prefix coverage; exact rational zero-first/second boundaries with a finite improvement use histogram profiles, a quadratic Jensen tail bound and exhaustive integer coverage; remaining ambiguous comparisons return the successful finite profile uncertified.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model success counts observed only within a specified conditioning window.
summary: Truncated binomial provides a model for success counts observed only within a specified conditioning window.
aliases: []
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, existence and uniqueness are not classified
    for general nonconstant samples. Held bounds and sizes must permit every
    observation. Constant samples attain likelihood one
    at $n=\max_i x_i$ and $p=1$. Free bounds can be tightened to the sample
    extremes, but their real values are not unique. With size held and
    nonsingleton retained support, a unique interior probability MLE exists
    exactly when the sample mean lies strictly between the retained edges.
    Edge-only samples attain $p=0$ or $p=1$ only when the window contains
    zero or $n$, respectively. Otherwise they have an unattained point-mass
    supremum. Singleton support leaves an interior probability unidentified.
    With an interior probability held and size free, a finite integer
    maximum exists unless constant data equal the finite upper truncation edge and no
    feasible size can make the retained support singleton. Integer maxima
    need not be unique. With both size and probability
    held, free bounds tighten to the sample extremes whenever the observations
    and conditioning event have positive probability. A held $p=0$ permits only
    zeros and leaves a free size unidentified among feasible sizes. A held $p=1$ permits only constant data and fixes a free size to
    that count. Holding every coefficient
    leaves only likelihood evaluation.
  reason: >-
    Tightening a window removes only unobserved mass. For fixed size the
    log likelihood is strictly concave in $\log(p/(1-p))$ on nonsingleton
    support, with curvature minus the sample size times the conditional
    variance. With an interior probability held, increasing size concentrates
    a finitely upper-truncated law at its upper edge. If an observation lies
    below that edge the likelihood tends to zero. Without an upper bound,
    the probability of any fixed finite sample tends to zero. These tail
    arguments ensure attainment outside the stated escape. With both size
    and probability free, the truncated-Poisson limit competes with finite
    profiles. Finite winners require a global retained-support certificate. Unresolved face
    comparisons do not prove global uniqueness or a general existence criterion.
---

The truncated binomial distribution models success counts observed only within a conditioning window $a<X\le b$. The trial count $n$ describes the underlying binomial experiment and $p$ is its success probability. The bounds describe which counts can be recorded.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) derives the stated limits and held-parameter conditions.

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Estimating size requires an integer profile. With probability held strictly between zero and one, the search uses explicit tail bounds.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. With size held, every sample mean strictly
between the support edges therefore gives one interior probability maximum.
Edge-only data give either an attained boundary mass or an unattained limit,
as described below. Singleton support does not identify the probability.

When the conditioning event includes zero, the point mass at zero is attained
at $p=0$. When it includes $n$, the point mass there is attained at $p=1$.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.

At fixed size, free truncation bounds select the smallest conditioning support containing the sample: $a=\min_i x_i-1$, $b=\max_i x_i$. The likelihood cannot decrease when unobserved support is removed. Singleton support attains likelihood one with any interior probability. Bounds giving the same integer window are equivalent, so their real values are not unique. A held boundary probability requires the conditioning event to have positive probability.

## R notes

Supply `a`, `b` and `size` explicitly. A numeric value holds the corresponding coefficient. Use `a = NULL` or `b = NULL` to estimate a bound at `min(x) - 1` or `max(x)`. Use `size = NULL` to estimate the trial count, and supply `prob` to hold the probability. Singleton support represents a free probability by 0.5. The truncated-Poisson comparison stops with advice to use `mltpois` on the same bounds.

Scalar d/p/q/r use the same positive-probability conditioning contract. At
`prob = 0` or `prob = 1`, the window must contain zero or size, respectively;
otherwise they return NaN. The atom at the corresponding extreme allowed
count exists as a weak limit but is not an attained member here. The ztbinom
family separately includes its positive-truncation probability-zero limit.

If the smallest feasible trial count leaves only two support atoms, its mean-matched law attains the empirical multinomial upper bound even when the supplied upper window extends further. This closes that case without a tail comparison. On a narrow window at a large count, conditional kernel ratios to the normalizer mode cancel the common factorial and product before evaluation; their cost depends on distance from the mode rather than the absolute count.

## Decision history

The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

With probability held in $(0,1)$, integer-size profiling bounds the entire tail by geometric ratios below a finite upper bound, or binomial stochastic ordering without an upper bound. A constant sample at the finite upper truncation edge escapes as size increases unless some feasible size gives singleton support. An exhausted enumeration-cell tail comparison returns its best evaluated integer profile uncertified. With probability free, the search scans 128 consecutive sizes, expands a bracket geometrically and refines every observed peak over integers. It compares the profile with the exact truncated-Poisson face on the same bounds; this is a numerical search, without a proof of global unimodality. Four consecutive expansions within $10^{-9}$ times the sample size of the face stop the search. A finite fit must beat that face by this tolerance; otherwise the fitter retains a truncated-Poisson comparison witness. This decision is numerical, not a proof that a finite maximizer cannot exist. Two-point conditioning windows attain their saturated likelihood at finite size and are returned directly. The budget is 512 expansions, with no size magnitude cap; a finite profile survives an unclosed certificate as an uncertified result. Poisson-scale coordinates and `log1p` corrections avoid cancellation at large sizes. Constant samples attain likelihood one at $n=\max_i x_i$, $p=1$.

The finite-window pass is required for a certified free-size fit and also covers infinite upper windows through the [retained-support bound](../ztbinom/README.md). Every integer from the observed maximum to the tail cutoff is evaluated or excluded by the shared convex-correction block bound, including sizes below the upper conditioning endpoint. A failed or unaffordable certificate returns its successful finite profile with `certified = FALSE`; the profile probability solve must still succeed. This fitter has no `iterlim` argument: its fixed certificate budget is 65,536 subdivision evaluations and 51,200,000 enumeration cells. The shared margins are used. Constant samples, two-point windows and full untruncated delegation retain their exact reductions. For a finite integer window $L,\ldots,U$, take its mean-matched Poisson law $Q$. Jensen's inequality on the full window gives the signed bound uniformly over probability. With $g=\overline{x(x-1)}-E_Q[X(X-1)]>0$ and $M=E_Q[(X)_3]/3+E_Q[(X)_2]/2$, every $N\ge\max(2U,8M/g)$ has likelihood strictly below the face. Infinite upper windows use the mean-preserving Poisson-tail compression in ztbinom. Every smaller integer must still pass complete prefix exclusion; no monotonicity is asserted. If every count on a finite window is observed and the integer ratios $(k+1)n_{k+1}/n_k$ agree exactly (tested as reduced integer fractions), the empirical law itself is truncated Poisson and attains the multinomial upper bound. On three or more support points no finite binomial has those constant factorial ratios, so this is a strict face limit even when the first-order gap vanishes. For windows starting at zero, the identity $E_Q[(X)_2]=\lambda(m-U)+Um$ supplies the rational candidate $\lambda=(Um-\overline{x(x-1)})/(U-m)$ at a zero first-order gap. The [exact moment comparison](face_certificate.hpp) uses checked integer arithmetic to test its mean equation and every factorial-moment inequality. The Newton expansion of $c_N(k)$ has strictly negative coefficients $\Delta^r c_N(0)$ for $r\ge2$. Thus $\overline{\binom{x}{r}}\ge E_Q\binom{X}{r}$ for every such $r$ excludes every finite size, with strictness from one moment gap or strict Jensen inequality. Arithmetic overflow refuses this criterion. When this first gap is exactly zero but the higher moment differences have mixed signs, let $a(k)=-k(k-1)/2$, $b(k)=-k(k-1)(2k-1)/12$, and let $h=a-E_Qa-\beta(X-m)$, where $\beta=\operatorname{Cov}_Q(a,X)/\operatorname{Var}_Q(X)$. The law $Q_N=Q(1+h/N)$ has unit mass and mean $m$. Jensen's inequality with this law and its relative entropy gives $(\ell_N-\ell_\infty)/n\le B/N^2+R/N^3$, uniformly in probability, for $N\ge\max(2U,2\max|h|)$, with $B=E_Pb-E_Qb-E_Qh^2/2$ and $R=(2/3)E_Q|h|^3+E_Q|hb|+E_Qa^2$. The entropy remainder follows from the third derivative of $(1+z)\log(1+z)$ on $|z|\le1/2$; the binomial log-product remainder is bounded by $(2/3)a^2/N^3$. A negative padded $B$ therefore supplies a cutoff, followed by complete integer-prefix exclusion. The implementation recognizes the zero gap using the same checked rational mean equation and pads the floating moment comparisons. Counts $(50,41,33,3,3)$ on $0,\ldots,4$ have $B=-20333/128830$ and a certified face of rate one despite mixed higher moments. Counts $(104,0,0,608,112)$ instead give $B=20224/331351>0$; the sufficient criterion refuses them. Other nonpositive gaps, unrecognized exact identities and near equal prefix values remain unresolved; unaffordable full-window moments or unrepresentable certificate windows leave the successful finite profile uncertified. The fitted bounds are first profiled to the observed extremes, so the same argument applies to every bound-hold subset. The [earlier investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) preserves the optional pass that this mandatory certificate replaces.

At a rational finite-window Poisson face with exactly zero first and second
profile coefficients, the mean-preserving law used by the second-order
certificate also supplies a third-order bound. Checked rational arithmetic
recognizes the second equality; overflow declines this sufficient test. With
$c(k)=-k^2(k-1)^2/12$, its coefficient is
$C=E_Pc-E_Qc-E_Q(hb)-E_Q(h^3)/6$. A padded negative $C$ supplies a
uniform tail cutoff using the fourth-order remainder. Every smaller integer
size must still be excluded; a negative local coefficient alone does not
prove that the face wins. Stable degree-eight series with geometric remainders compare the normalized Jensen gap over the remaining prefix; they remove the two exact zeros before numerical evaluation. Positive third-order coefficients are not accepted
as face certificates.

A vanishing second coefficient does not imply that a Poisson face wins.
For rational matching faces, exact first/second cancellation activates a
histogram certificate when there is a strict finite improvement. The
first-order cancellation gives the uniform tail bound
$(\ell_N-\ell_\infty)/n\le E_Q\sum_{j<X}j^2/N^2$ for $N\ge2U$.
All smaller integers are profiled with probability enclosures. For the rate-four
boundary with counts $(242187,60672,121344,1488980,406274)$ on $0,\ldots,4$,
the finite global winner has size seven despite the first two zero face
coefficients. Histogram evaluation preserves the sufficient statistics and
avoids charging millions of identical observations at every candidate size.

The rate-six boundary with counts $(21251,11790,0,244158,168771)$ has
third coefficient $-226332657/36450495095$. The third-order tail and complete
prefix comparison certify its truncated-Poisson supremum. Thus zero first and
second coefficients allow both finite and limiting winners.

The held-probability integer scan uses the same mode-relative kernels as the free-probability profile. It charges normalizer recurrence cells and kernel-ratio terms against the existing 512-unit default allowance; the supplied probability is never re-optimized during that scan.
