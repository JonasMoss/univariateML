---
id: zeta
inference_numeric:
  - regime: interior
    available: true
    condition: "finite s > 1; s free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zeta
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
- name: s
  symbol: 's'
  role: shape
  domain: (1, inf)
  simulate: 2
  kind: regular
r:
  density: dzeta
  sampler: rzeta
  control:
    s: "Optional held coefficient; NULL estimates it."
    reltol: "Relative tolerance for the bracketed scalar solve."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: "-s*log(x) - log(zeta(s))"
limits:
  - name: point mass at one
    firmness: proved
    when: 'The exponent satisfies $s\to\infty$.'
    detect: 'The exponent is free and every observation equals one.'
    status: PointMass
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlzeta())
  mlzeta(x)

group: discrete heavy-tailed
construction:
- base
relations:
  truncation_of:
  - family: zeta
    at:
      s: s
    when: s > 1
    source_configuration:
      kernel: zipf
      truncate:
      - 1
      - N
      coefficient_map:
        s: s
fit:
  method: scalar-solve
  certified: 'yes'
  note: "zeta/mle.hpp: The strictly decreasing exponent score gives a unique bracketed root when a count exceeds one; all-one data have an excluded infinite-exponent point-mass supremum. Held exponent evaluates the likelihood."
areas:
- rank-frequency
classification: specialist
classification_reason: Used to model unbounded positive frequencies following a pure power law.
summary: Zeta provides a model for unbounded positive frequencies following a pure power law.
aliases:
- "Zipf (infinite support)"
- "discrete Pareto"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    A unique finite MLE exists exactly when at least one observation exceeds
    one. All-ones samples have an unattained point-mass supremum as
    $s\to\infty$. Holding finite $s>1$ leaves only likelihood evaluation
    and permits all-ones samples.
  reason: >-
    The score per observation is $E_s[\log X]-\overline{\log x}$.
    Its derivative is $-\operatorname{Var}_s(\log X)<0$.
    The model mean decreases from infinity as $s\downarrow1$ to zero
    as $s\to\infty$. A positive empirical log mean therefore gives
    exactly one score root and a strictly concave likelihood. If every
    observation is one, the score stays positive and the mass at one
    tends to one.
modifiers:
  truncate:
    status: implemented
    certified: "yes"
    domain_bounded: "finite [1,N], integer N >= 1: every finite real s"
    note: "Finite harmonic normalizer; strict concavity in s, endpoint point-mass escapes."
---

The zeta distribution models unbounded positive integer frequencies following a pure power law. Its exponent $s>1$ controls tail decay, with larger values concentrating probability closer to one.

## Details

For fixed $s>1$, this is the limit of the finite Zipf distribution as its upper endpoint tends to infinity. The log likelihood is strictly concave in $s$. Its score is the sample size times the difference between the model and empirical means of $\log X$. The model mean decreases from infinity to zero, giving one finite maximum whenever some observation exceeds one. Other constant samples also have finite fits.

All-ones samples have an unattained point-mass supremum as $s\to\infty$. As $s\downarrow1$, every fixed count has probability tending to zero, so this endpoint cannot maximize a finite sample likelihood. Holding the exponent evaluates the likelihood directly, including for all-ones samples.

## R notes

A free-exponent fit to all-ones data stops. Supplying `s` holds the exponent.

The probability functions evaluate upper tails directly and use bounded-memory integer inversion. Beyond $2^{53}$, quantiles use the representable double grid, which cannot distinguish every adjacent integer. Quantiles exceeding the largest finite double are returned as infinity. Support starts at one and has no finite upper endpoint.

Finite windows `truncate = c(1, N)` admit every real exponent, since the
normalizer is a finite positive sum. The score is
$n(E_s[\log X]-\overline{\log x})$, strictly decreasing when $N>1$.
Endpoint-only samples have an unattained point-mass supremum in the corresponding
infinite-exponent direction. A singleton window leaves the exponent unidentified.
This differs from the nonnegative-shape finite Zipf model: data favoring an
increasing mass function have a negative exponent, rather than a uniform fit.

Equal multiplicities on every retained integer give the exact exponent MLE
$s=0$. The uniform empirical law attains the multinomial likelihood bound,
providing a closed-form reference for repeated large histograms.

For negative exponents, the finite sums are scaled at the upper endpoint.
Geometric tails control concentrated laws; Euler--Maclaurin with a checked
remainder controls broad laws on large supports. Both the normalizer and the
centered log-count moment are bounded before a score comparison is accepted.
