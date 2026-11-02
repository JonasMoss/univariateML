---
id: bbinom
inference_numeric:
  - regime: size zero
    available: true
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with one shape known"
    available: true
    condition: "known size = 1; exactly one positive finite shape free; the other shape known; rank one for the two-shape model; the scalar known-nuisance submodel is identifiable"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with both shapes free"
    available: true
    condition: "known size = 1; both positive finite shapes free; DQM and pointwise C2 hold, but joint information has rank one and covariance is singular"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
    available: true
    condition: "known integer size >= 2 held; finite alpha > 0 and beta > 0; either or both free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
name: Beta-binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
  advice: Supply the number of trials.
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dbbinom
  sampler: rbbinom
  control:
    size: "Required number of trials; NULL estimates size when beta is held."
    alpha: "Optional fixed positive alpha; NULL estimates it."
    beta: "Optional fixed value of `beta`; `NULL` (the default) estimates it."
    reltol: "Relative tolerance on the Newton steps in the logarithm of the free concentration or shape."
    iterlim: "Maximum number of Newton iterations per local maximum."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + loggamma(x + alpha) + loggamma(size - x + beta) - loggamma(size + alpha + beta) - loggamma(alpha) - loggamma(beta) + loggamma(alpha + beta)"
limits:
  - model: binom
    firmness: proved
    when: '$\alpha + \beta \to \infty$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: no beta-binomial fit beats the binomial with success probability $\bar x / n$ (up to $10^{-8}$ per observation)
    status: BinomialLimit
    numerical: true
  - name: two-point law on 0 and n
    firmness: proved
    when: '$\alpha, \beta \to 0$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: '$n \ge 2$, every observation is 0 or $n$, and both values occur'
    status: TwoPointLimit
  - model: binom
    firmness: proved
    when: '$\alpha/(\alpha + \beta) \to 0$ or $1$'
    detect: 'all observations are 0, or all equal $n$'
    status: PointMass
references:
- Griffiths, D. A. (1973). Maximum likelihood estimation for the beta-binomial distribution and an application to
  the household distribution of the total number of cases of a disease. Biometrics, 29(4), 637-648. doi:10.2307/2529131.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds:
- held: []
  status: not-implemented
  reason: "Estimating size together with both shapes is not implemented. It would require comparing the fits at every finite size, including their binomial and two-point limits, with the negative binomial and Poisson limits, and no usable bound on large sizes is known."
- held:
  - alpha
  status: not-implemented
  reason: "Estimating size with alpha held is not implemented. As size grows the fit approaches a negative binomial with the same alpha, and no usable bound rules out a larger size beating both."
fit_args:
  size: 10
examples: |
  set.seed(1)
  x <- rml(200, mlbbinom())
  mlbbinom(x, size = 10L)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: binom
    at:
      size: size
      prob: mix
    mixed: prob
    mixing:
      family: beta
      at:
        shape1: alpha
        shape2: beta
fit:
  method: profile-search
  certified: partial
  note: "bbinom/mle.hpp, bbinom_mle and bbinom_detail::certify: Fixed-size free-shape profiles use tangent/chord bounds, including the binomial face, with a 1e-8 per-observation face preference; this numerical preference is not an exact nonexistence test. Supported free-size fits use integer tail bounds, including free alpha with beta held; unresolved profile ties and budgets stop. Other free-size profiles remain unsupported."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed success counts with heterogeneous binomial probabilities.
summary: Beta-binomial provides a model for overdispersed success counts with heterogeneous binomial probabilities.
aliases: []
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    Take $n$ known and both shapes free. If every count is 0 or $n$, no MLE
    exists: the likelihood is maximised in the limit by a two-point law on 0
    and $n$ when both values occur, and by a point mass when only one does.
    Otherwise the only possible escape is to the binomial, and an MLE exists
    exactly when some beta-binomial fits at least as well as the best
    binomial. Overdispersion, $S^2 > n\hat p(1-\hat p)$ with
    $\hat p = \bar x/n$ and $S^2$ the sample variance with the number of
    observations as divisor, is sufficient. For $n = 2$ it is also necessary;
    for $n \ge 3$ that is a conjecture. Holding one shape leaves only the
    point-mass case. With $n = 1$ the shapes are not identified, because the
    model is a Bernoulli distribution. With beta held and size free,
    constant positive data have only a point-mass supremum; all-zero data
    attain likelihood one at size zero with alpha unidentified. For other
    samples a finite maximum exists, though the numerical search may stop
    without resolving it.
  reason: >-
    Write $p = \alpha/(\alpha+\beta)$ and $s = 1/(\alpha+\beta)$. A sample with
    a count strictly between 0 and $n$ loses all likelihood as $s \to \infty$
    or as $p$ tends to 0 or 1, so the binomial face $s = 0$ is its only
    escape. On that face the profile score in $s$ has the sign of
    $S^2 - n\hat p(1-\hat p)$, so overdispersion makes the likelihood increase
    into the interior.
---

The beta-binomial distribution is a binomial distribution whose success probability is itself beta distributed. It models overdispersed counts out of a known number of trials $n$.

## Details

The limit and sample conditions are supported by the [existence analysis](../../../../../dev/research/evaluations/2026-10-31-open-existence-report.md); numerical face searches do not certify global optimality.

The number of trials $n$ is normally known. The fit with both shapes free does not test for overdispersion; it compares the best interior fit with the binomial likelihood, within $10^{-8}$ per observation. Evidence for the existence results: `dev/research/evaluations/2026-10-31-open-existence-report.md`.

With both shapes held it can also be estimated: each observed probability decreases in $n$ once $n + 1 \ge x(\alpha + \beta - 1)/\alpha$, so a finite search over $n$ finds the maximum. The search stops after one million candidates. With $n$ also held, the likelihood is simply evaluated. Estimating $n$ with $\alpha$ held or both shapes free remains unsupported. Holding $\beta$ is not equivalent to holding $\alpha$ after reflecting the counts, because the reflection $x \mapsto n - x$ depends on the unknown $n$.

## Free-size research: joint held-beta tail

Let $h$ be the number of observations, $m>0$ the number of positive
observations, $M=\max x_i$, and let $b>0$ be held. For $N\ge2M$ put
$H_N=\sum_{j=0}^{N-1}(b+1+j)^{-1}$. The held-beta inequalities proved in
`dev/research/evaluations/2026-10-30-holds-bbinom-report.md` give, for
$0<a\le1$, $P_0\le e^{-aH_N}$ and
$P_x\le 2^x a e^{-aH_N}$ for $x>0$. For $a\ge1$, stochastic monotonicity
and the same report give $P_0\le e^{-H_N}$ and
$P_x\le(2^{x+1}-1)e^{-H_N}$. Thus the **whole likelihood**, uniformly in
$a>0$, is at most

$$
\max\left\{2^{\sum_i x_i}\left(\frac{m}{e h H_N}\right)^m,
 e^{-hH_N}\prod_{x_i>0}(2^{x_i+1}-1)\right\}.
$$

The first term maximises $a^m e^{-haH_N}$ over all $a>0$; allowing
$a>1$ in that maximisation only weakens the bound. Both terms decrease in
$N$. A candidate with positive likelihood supplies a finite cutoff, but
that cutoff can be impractically large. This improves the earlier
single-observation $O(1/\log N)$ likelihood bound to
$O((\log N)^{-m})$. The power is sharp in general: along
$a=t/\log N$ for fixed $t>0$, $P_0\to e^{-t}$ and
$P_x\sim e^{-t}t/(x\log N)$ for each fixed $x>0$. Consequently the
likelihood times $(\log N)^m$ tends to
$e^{-ht}t^m/\prod_{x_i>0}x_i$.

The earlier bound alone does not certify the integer profile comparison: the
existing fixed-size shape optimizer supplies an approximate profile with a
nonzero likelihood allowance, and those upper allowances must be retained
when comparing sizes and point-mass faces. Held alpha has a fixed-shape
negative-binomial face, while the unrestricted subset also has arbitrary
negative-binomial and Poisson faces and fixed-size binomial and endpoint
faces. A uniform tail correction against those faces and a global
comparison of the finite-size faces remain open. The held-beta search below supplies a separate certificate. Details and numerical checks are recorded in
`dev/research/evaluations/2026-11-01-bbinom-size-report.md`.

## R notes

`size = NULL` estimates the number of trials when beta is held. A supplied `size` is held fixed and is not counted in the degrees of freedom of `logLik()`, AIC or BIC. `model_select()` skips this family because it cannot supply `size`.


## Held beta: finite profile and size certificate

Fix $b>0$, write $h$ for sample size, $m$ for the number of positive
counts and $M$ for their maximum. At fixed $N$, the profile in $a$ is
not generally concave in $\log a$: for one observation $x=N=2$ and
$b=10$, its second derivative in $\log a$ is positive for sufficiently
small $a$, with leading coefficient $1-1/10-1/11$ times $a$. Its score is
a sum of positive numerator terms and negative denominator terms. The
implementation brackets all stationary points using score bounds, scans and
refines candidates, and bounds every intervening interval by a tangent to
the concave numerator minus a chord of the concave denominator. It accepts
a profile within $10^{-8}h$ log-likelihood units, with the floating-point
margin in `shape_bound`; failure to establish this stops. This is an
exact-arithmetic certificate conditional on computed comparisons, not
outward-rounded arithmetic.

At fixed $N>0$, $a\to0$ gives a point mass at zero and $a\to\infty$
a point mass at $N$. For nonconstant data both faces have zero likelihood.
For constant positive data $N=M$ has supremum one at the upper point mass,
which no finite positive shapes attain. For all-zero data, $N=0$ attains
likelihood one and leaves $a$ unidentified. For a positive observation the
joint likelihood tends to zero uniformly as $N\to\infty$, as shown above.

A practical tail certificate follows from the exact ratio
$$
\frac{P_{N+1,a,b}(x)}{P_{N,a,b}(x)}
=\frac{N+1}{N+1-x}\frac{N-x+b}{N+a+b}.
$$
It is at most one when $a\ge x(b-1)/(N+1-x)$. For $b\le1$
every likelihood decreases in $N$. Otherwise the alpha score at size $N+1$
is positive below $m/(hH)$, where $H=\sum_{j=0}^{N}(b+j)^{-1}$.
Consequently every maximizer lies above this bound. If
$m(N+1-M)\ge hM(b-1)H$, the profile at $N+1$ cannot exceed that at $N$.
The left side divided by $H$ increases with $N$, so this comparison
holds for all subsequent sizes. The search enumerates up to the first such
$N$, at most 1000, and stops on exhaustion. It compares profile values
with a $2\cdot10^{-8}h$ ambiguity margin and stops on a numerical tie.
The cutoff inequality uses an additional floating-point margin; neither
margin is an interval-arithmetic guarantee. The fixed-size profile allowance
is propagated in the comparison. See the task report for measurements.
