---
id: ztbinom
inference_numeric:
  - regime: "size one"
    available: false
    condition: "known size = 1; 0 < prob < 1 free; the zero-truncated law is a point mass, so information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer size >= 2 held; 0 < prob < 1 free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated binomial
support: {type: integer, lower: "1", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[1, inf)'
  integer: yes
  simulate: 10
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dztbinom
  sampler: rztbinom
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per global certificate: 128 * min(iterlim, 100000) evaluations; per integer scan: 100000 * min(iterlim, 100000) enumeration cells."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
log_density: loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)
limits:
- model: ztpois
  firmness: proved
  when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law.
  detect: With both coefficients free and at least one count above one, the fitted zero-truncated Poisson log likelihood is at least the best searched finite-size value minus $10^{-8}$ times sample size.
  status: PoissonLimit
  numerical: true
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlztbinom())
  mlztbinom(x, size = 10)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: zmbinom
    at:
      size: size
      prob: prob
      p0: 0
fit:
  method: integer-search
  certified: partial
  note: 'ztbinom/README.md, Details; ztbinom/mle.hpp; ztbinom/test.cpp: Finite free-size winners require retained-support tail exclusion and complete integer-prefix coverage. Fixed size and held probability use their exact profiles. Winning-face comparisons remain unresolved.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model binomial success counts sampled only when positive.
summary: Zero-truncated binomial provides a model for binomial success counts sampled only when positive.
aliases:
- "positive binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both trial count and probability free, a general finite-MLE criterion and uniqueness result are not established here. The supremum can lie at the zero-truncated Poisson limit as $n$ tends to infinity. All-one data attain a maximum with nonunique coefficients. With integer $n\ge\max_i x_i$ held and $n>1$, the probability MLE exists and is unique, including its endpoints. At $n=1$, only ones are possible and $p$ is unidentified. With $0<p<1$ held, a finite integer maximum exists but uniqueness is not asserted. Held $p=0$ requires only ones and leaves $n$ unidentified. Held $p=1$ requires constant counts and sets $n$ to that count. Holding both coefficients requires compatible observations.
  reason: >-
    At fixed $n>1$, the log likelihood is strictly concave in logit probability, with curvature minus sample size times the conditional variance. The conditional mean ranges from one to $n$, giving a unique mean-matching solution or endpoint. At held interior probability, each observed mass tends to zero as $n$ tends to infinity, so a finite integer maximum exists. These arguments do not establish unimodality or uniqueness of the free-size profile.
---

The zero-truncated binomial distribution describes binomial success counts observed only when positive. The integer trial count $n$ bounds the counts, and $p$ is the success probability before conditioning.

## Details

Holding the trial count removes the infinite-size escape. For a trial count greater than one, the fit matches the conditional mean to the sample mean. All-one data give the attained endpoint $p=0$, and counts all equal to the held trial count give $p=1$. At trial count one every probability gives the same point mass, and the probability is unidentified. An interior probability solve must pass a total log-odds score check, $|\sum_i x_i-n_{\mathrm{obs}}E[X]|<10^{-7}$. An absolute probability-bracket width alone can accept an unresolved tiny probability at a large held size; those fits stop numerically with their evaluated witness.

With both coefficients free, the exponential grid and local refinements propose an integer size; they do not certify it. A successful finite proposal must exclude every size at or beyond a cutoff and cover every feasible integer below it by evaluation or an explicit block bound. For $N\ge K\ge\max x_i$, write $c_N(k)=\sum_{j<k}\log(1-j/N)$ and $\lambda=Np/(1-p)$. Retaining only counts $1,\ldots,K$ in the normalizer gives

$$\ell_N(\lambda)\le T_K(\lambda)+D_{N,K},\qquad D_{N,K}=\sum_i[c_N(x_i)-c_N(K)].$$

Here $T_K$ is the retained finite-support Poisson log likelihood. It is concave in $\log\lambda$; its mean equation brackets the unique maximum unless the retained support is degenerate. Tangent bounds on that bracket bound its supremum. The nonnegative correction decreases to zero with $N$, so strict finite improvements produce a tail cutoff. Increasing $K$ tightens the retained-support bound for infinite windows. Exhausted certificate or enumeration budgets stop with a finite witness and an infinite global upper bound. The certificate is valid in exact arithmetic conditional on the computed comparisons. It uses a $10^{-9}n$ likelihood separation plus $4096\epsilon(1+|\ell|)$ and a retained-sum bound margin $4096\epsilon(1+|\ell|+nK)$; these are practical floating-point margins, without outward rounding.

For a finite block $A\le N\le B$, let $\delta_N(k)=c_N(k)-c_A(k)$ and choose an integer $h<A$. Its increments $d_N(j)=\delta_N(j+1)-\delta_N(j)$ increase with $j$. Thus $r_N(k)=\delta_N(k)-\delta_N(h)-(k-h)d_N(h)$ is nonnegative. Each residual summand also increases with $N$, giving $r_N(k)\le r_B(k)$. The affine part is absorbed into the log odds and normalizing constant, so

$$\sup_{A\le N\le B,p}\ell_N(p)\le\sup_p\ell_A(p)+\sum_i r_B(x_i).$$

The bound uses the support at $A$ as a subset of every later support, so it remains valid below a finite upper truncation endpoint. The same bound holds after zero inflation: a zero mass is bounded by $e^{r_B(0)}$ times the corresponding inflated mass at $A$. Blocks that fail strict exclusion are bisected down to singleton sizes. This avoids exhaustive profiling of a long prefix without assuming profile unimodality.

An unsigned tail bound does not establish that the Poisson face wins. Such comparisons stop as unresolved, preserving the finite and Poisson witnesses. The signed-tail and face-tie analysis is open. See the [certificate investigation](../../../../../dev/research/evaluations/2026-11-07-binom-rel-report.md) for the bound proof, coverage checks and remaining limitations.

With an interior probability held, the integer search bounds the remaining tail before returning. Exhausting its work budget is an error. Probability zero permits only ones. Probability one permits only counts equal to the trial count. These are attained members, not escapes.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-binomial.md) discusses the boundaries. The [search checks](../../../../../dev/research/evaluations/2026-10-29-tail85-report.md) compare the fit with independent reference-density multistart oracles, including tied and near-Poisson samples. Such comparisons do not prove global optimality.

## R notes

A Poisson comparison witness does not assert non-existence; the public condition has kind `unresolved`. The other binomial relatives report such comparisons the same way.

Supply `size` explicitly. A numeric value holds it, while `size = NULL` estimates it. Supplying `prob` holds the probability exactly. The boundary convention at `prob = 0` agrees with actuar. Unresolved Poisson comparisons retain advice to use `mlztpois` as a comparison model. Iteration exhaustion stops instead of returning a capped size. Increasing `iterlim` raises the certificate and enumeration budgets.

## Decision history

2026-10-29: the free-size grid's fixed magnitude cap was removed. The current search grows subject to its iteration budget and numerical stopping rule. See the search checks linked above.

2026-10-31: the [size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) records the remaining certificate gap. DeRiggi, D. F. (1983), “Unimodality of Likelihood Functions for the Binomial Distribution”, Journal of the American Statistical Association, 78(381), 181–183, doi:10.1080/01621459.1983.10477949, concerns the untruncated law. DeRiggi, D. (1994), “Sufficient conditions for unimodality of the positive binomial likelihood function”, Statistics & Probability Letters, 19(1), 1–4, doi:10.1016/0167-7152(94)90061-2, reports a continuous-profile counterexample. As recorded in that investigation, the full paper and correction remain unchecked, and this does not establish an integer-size counterexample.
