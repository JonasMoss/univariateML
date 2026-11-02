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
    iterlim: "Maximum number of bisection iterations."
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
relations: {}
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'zeta/README.md, Details; zeta/mle.hpp (public fitter and held overloads); zeta/test.cpp: Strictly monotone
    one-parameter exponential-family or log-shape score gives a bracketed unique root or the documented boundary;
    finite Zipf endpoint and modified zero mass profile exactly.'
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
---

The zeta distribution models unbounded positive integer frequencies following a pure power law. Its exponent $s>1$ controls tail decay, with larger values concentrating probability closer to one.

## Details

For fixed $s>1$, this is the limit of the finite Zipf distribution as its upper endpoint tends to infinity. The log likelihood is strictly concave in $s$. Its score is the sample size times the difference between the model and empirical means of $\log X$. The model mean decreases from infinity to zero, giving one finite maximum whenever some observation exceeds one. Other constant samples also have finite fits.

All-ones samples have an unattained point-mass supremum as $s\to\infty$. As $s\downarrow1$, every fixed count has probability tending to zero, so this endpoint cannot maximize a finite sample likelihood. Holding the exponent evaluates the likelihood directly, including for all-ones samples.

## R notes

A free-exponent fit to all-ones data stops. Supplying `s` holds the exponent.

The probability functions evaluate upper tails directly and use bounded-memory integer inversion. Beyond $2^{53}$, quantiles use the representable double grid, which cannot distinguish every adjacent integer. Quantiles exceeding the largest finite double are returned as infinity. Support starts at one and has no finite upper endpoint.
