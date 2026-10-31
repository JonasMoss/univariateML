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
  reason: "Estimating size with beta held is not implemented. A tail bound shows that the likelihood eventually decreases in size when some count is positive, but it is too weak for a practical search, and the point-mass limits at each size still need comparison."
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

## R notes

`size = NULL` estimates the number of trials when both shapes are held. A supplied `size` is held fixed and is not counted in the degrees of freedom of `logLik()`, AIC or BIC. `model_select()` skips this family because it cannot supply `size`.
