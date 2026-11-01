---
id: skellam
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu1 > 0 and mu2 > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Skellam
support: {type: integer, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu1
  symbol: '\mu_1'
  role: positive-count rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: mu2
  symbol: '\mu_2'
  role: negative-count rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dskellam
  sampler: rskellam
  control:
    mu1: "Optional fixed nonnegative finite positive-count rate; NULL estimates it."
    mu2: "Optional fixed nonnegative finite negative-count rate; NULL estimates it."
    reltol: "Relative tolerance for the scalar profile search."
    iterlim: "Maximum iterations for each scalar refinement."
log_density: "-mu1-mu2 + x*log(mu1/mu2)/2 + log(besseli(Abs(x),2*sqrt(mu1*mu2)))"
references:
- Skellam, J. G. (1946). The frequency distribution of the difference between two Poisson variates belonging to
  different populations. Journal of the Royal Statistical Society 109, 296.
- Johnson, N. L. (1959). On an extension of the connexion between Poisson and chi-squared distributions. Biometrika
  46, 352–362.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlskellam())
  mlskellam(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  convolution_of:
  - family: pois
    operation: difference
    components:
    - family: pois
      at:
        lambda: mu1
    - family: pois
      at:
        lambda: mu2
    when: mu1 > 0 && mu2 > 0
fit:
  method: scalar-solve
  certified: partial
  note: 'skellam/README.md, Details; skellam/mle.hpp (public fitter and held overloads); skellam/test.cpp: One held
    positive rate gives a strictly concave bracketed solve; joint scalar profile uses a finite grid without a uniqueness
    certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model differences between independent Poisson event counts.
summary: Skellam provides a model for differences between independent Poisson event counts.
aliases: []
key_references:
- 3
---

The Skellam distribution describes the difference between two independent Poisson counts.

## Details

2026-10-30 (TASK-125): the large-order expression divides before multiplying and uses log coordinates when the order/argument ratio overflows. The independent truth rows at count 1e300 exercise the change.

Zero rates are attained members, including the point mass at zero when both vanish. A positive or negative Poisson boundary may therefore be returned directly. Constant nonzero data need not give a point mass. With one positive rate fixed, the log likelihood is strictly concave in the other rate; a bracketed score solve finds its unique maximum, including the zero endpoint. Holding either or both rates fixed is supported; data outside a fixed zero-rate member's support are rejected.

For a joint interior fit, writing $s=\sqrt{\mu_1\mu_2}$ and $\theta=\log(\mu_1/\mu_2)/2$ gives the score equation $\mu_1-\mu_2=\bar x$. The remaining equation is $n^{-1}\sum_i I'_{|x_i|}(2s)/I_{|x_i|}(2s)=\cosh\theta$, with $I'_v(z)=I_{v+1}(z)+vI_v(z)/z$. The fitter searches the resulting scalar profile and compares every nominated maximum with the exact zero-rate face. Faces win log likelihood ties within $10^{-8}n$. Uniqueness is not assumed. The search uses a finite logarithmic grid and local refinement; this is numerical global-search evidence rather than a proof that every maximum is found.

The mean and variance identify both rates. Likelihoods are bounded by one per observation. Both-sided samples exclude zero-rate faces. A convolution mass is at most the maximal mass of either Poisson factor, which vanishes when that rate diverges. Continuity on the closed quadrant therefore gives a finite maximizer for every finite integer sample, also with fixed rates whenever the support permits the sample. Constant data $k$ have the joint MLE $(k,0)$ for $k>0$, $(0,-k)$ for $k<0$, and $(0,0)$ for zero: condition on the opposite Poisson count and use the decreasing maximal Poisson mass as its integer mode increases. For nonnegative data with positive mean, the inward profile derivative per observation at the Poisson face is $\bar x\,n^{-1}\sum_i 1/(x_i+1)-1$; reflect the data for the other face. This is a local boundary diagnostic, not a sufficient global rule; the fitter compares likelihoods.

## d/p/q/r

Zero rates include the signed Poisson limits; sampling subtracts two independent Poisson variates.

### Boundary log tails (TASK-134)

When one rate is zero, CDF evaluation delegates to the Poisson log-tail
implementation, reflecting the count and reversing the tail for a negative
Poisson law. With both rates positive, probabilities below `1e-280` are summed
in log mass coordinates rather than taking a logarithm of a subnormal CDF.
Skellam masses are log-concave (convolution of a Poisson sequence and a
reflected Poisson sequence). In a descending tail, the next mass ratio thus
bounds subsequent ratios, giving a geometric remainder bound. Summation stops
when that bound is at most `2e-16` times the sum; inability to advance the count
returns NaN. This is a numerical stopping certificate using computed masses,
not a directed-rounding bound on the Bessel approximations.

For a negative count `v`, the positive Bessel series also gives
`I_(n+1)(z)/I_n(z) <= z/(2*(n+1))`, hence the next descending mass ratio
is at most `mu2/(1-v)`. This analytic geometric bound can certify a
single-term tail at enormous counts where the next integer is not
representable. It avoids treating representational stagnation as convergence.
