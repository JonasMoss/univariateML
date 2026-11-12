---
id: pgw
name: Power generalized Weibull
stage: dpqr
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- {name: scale, symbol: a, role: scale, domain: '(0, inf)', simulate: 2, kind: regular}
- {name: shape, symbol: b, role: shape, domain: '(0, inf)', simulate: 1.5, kind: regular}
- {name: exponent, symbol: c, role: shape, domain: '(0, inf)', simulate: 2.5, kind: regular}
r:
  density: dpgw
  sampler: rpgw
log_density: 'log(shape) + log(exponent) - log(x) + shape*log(x/scale) + (exponent-1)*log(1+(x/scale)**shape) - ((1+(x/scale)**shape)**exponent-1)'
inference_numeric:
- regime: finite positive interior
  available: false
  condition: 'Finite positive scale, shape and exponent; any nonempty free subset.'
  reason: 'Derivatives and numerical covariance have not been implemented or validated.'
  classification: 'dev/research/evaluations/2026-11-11-pgw-theory-report.md#interior-inference'
classification: specialist
classification_reason: Flexible positive lifetime model with distinct early and late hazard powers.
group: gamma and Weibull
construction: [generalisation]
areas: [reliability and survival]
limits:
- name: power-transformed Gompertz
  firmness: proved
  when: '$c\to\infty$, $a=s c^{1/b}$, with $b,s>0$ fixed: $H(x)\to\exp\{(x/s)^b\}-1$.'
- name: lower-threshold conditional Weibull
  firmness: proved
  when: '$a=s$, $b\to\infty$, $c=k/b$, with $s,k>0$ fixed: $H(x)\to0$ below $s$ and $(x/s)^k-1$ above $s$.'
relations: {}
---

The ordinary power generalized Weibull has cumulative hazard
$H(x)=\{1+(x/a)^b\}^c-1$ and survival $\exp\{-H(x)\}$.
It is distinct from the adapted power generalized Weibull.
At $c=1$ it is [Weibull](../weibull/README.md) with shape $b$ and scale $a$;
at $b=c=1$ it is [exponential](../exp/README.md) with rate $1/a$.

## Numerical domain

Quantiles conservatively return NaN when the computed
$|\log((x/a)^b)|<0.2$ and $b<10^{-4}$. In this inverse-cancellation
region, rounding in the near-unit power can be amplified beyond the
quantile accuracy contract by division by the small shape. Rounded zero or
infinity is still returned when both sides of a $10^{-12}$ absolute margin
in the log power imply that same endpoint. Upper log probabilities equal to
$1-2^c$ for integer
$c\le53$ return the exact scale. The inverse sampler inherits unresolved
quantile failures.

## Boundary laws

The power-transformed Gompertz boundary at $b=1$ has Gompertz rate and
shape both $1/s$; an unrestricted Gompertz fit adds a degree of freedom.
The threshold boundary is Weibull conditioned on exceeding its own scale,
not an unrestricted truncated Weibull.
The pointwise endpoint density depends on the path to this law.
These are proved distributional limits, not implemented likelihood escape
detectors. Their reachability as likelihood suprema and exhaustiveness over
joint parameter paths remain open in the
[PGW analysis](../../../../../dev/research/evaluations/2026-11-11-pgw-theory-report.md#proved-boundary-paths).
