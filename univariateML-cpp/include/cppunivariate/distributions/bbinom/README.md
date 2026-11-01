---
id: bbinom
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
    size: "Required number of trials; NULL estimates size when both shapes are held."
    alpha: "Optional fixed positive alpha; NULL estimates it."
    beta: "Optional fixed value of `beta`; `NULL` (the default) estimates it."
    reltol: "Relative tolerance on the Newton steps in the logarithm of the free concentration or shape."
    iterlim: "Maximum number of Newton iterations per local maximum."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + loggamma(x + alpha) + loggamma(size - x + beta) - loggamma(size + alpha + beta) - loggamma(alpha) - loggamma(beta) + loggamma(alpha + beta)"
limits:
  - model: binom
    when: '$\alpha + \beta \to \infty$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: no beta-binomial fit beats the binomial with success probability $\bar x / n$ (up to $10^{-8}$ per observation)
    status: BinomialLimit
  - name: two-point law on 0 and n
    when: '$\alpha, \beta \to 0$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: '$n \ge 2$, every observation is 0 or $n$, and both values occur'
    status: TwoPointLimit
  - model: binom
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
- held:
  - beta
  status: not-implemented
  reason: "Estimating size with beta held is not implemented. A joint tail bound tends to zero when some count is positive, but a certified search still needs global comparison of fixed-size profiles and point-mass limits."
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
  method: multistart
  certified: partial
  note: 'bbinom/README.md, Details; bbinom/mle.hpp (public fitter and held overloads); bbinom/test.cpp: Both shapes
    held give an integer-size tail bound; fixed-size free-shape fits use numerical optimization and some free-size
    profiles are unsupported.'
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
    model is a Bernoulli distribution.
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

The number of trials $n$ is normally known. The fit with both shapes free does not test for overdispersion; it compares the best interior fit with the binomial likelihood, within $10^{-8}$ per observation. Evidence for the existence results: `dev/research/evaluations/2026-10-31-open-existence-report.md`.

With both shapes held it can also be estimated: each observed probability decreases in $n$ once $n + 1 \ge x(\alpha + \beta - 1)/\alpha$, so a finite search over $n$ finds the maximum. The search stops after one million candidates. With $n$ also held, the likelihood is simply evaluated. Estimating $n$ with a shape free is not implemented, because the fit at each fixed $n$ can itself escape to a binomial or two-point law, and these limits must be compared across all $n$. Holding $\beta$ is not equivalent to holding $\alpha$ after reflecting the counts, because the reflection $x \mapsto n - x$ depends on the unknown $n$.

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

This bound alone does not certify the integer profile comparison: the
existing fixed-size shape optimizer supplies an approximate profile with a
nonzero likelihood allowance, and those upper allowances must be retained
when comparing sizes and point-mass faces. Held alpha has a fixed-shape
negative-binomial face, while the unrestricted subset also has arbitrary
negative-binomial and Poisson faces and fixed-size binomial and endpoint
faces. A uniform tail correction against those faces and a global
comparison of the finite-size faces remain open. No additional subset is
implemented by this bound. Details and numerical checks are recorded in
`dev/research/evaluations/2026-11-01-bbinom-size-report.md`.

## R notes

`size = NULL` estimates the number of trials when both shapes are held. A supplied `size` is held fixed and is not counted in the degrees of freedom of `logLik()`, AIC or BIC. `model_select()` skips this family because it cannot supply `size`.
