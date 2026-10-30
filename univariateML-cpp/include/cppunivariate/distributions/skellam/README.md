---
id: skellam
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
  - "Skellam, J. G. (1946). The frequency distribution of the difference between two Poisson variates belonging to different populations. Journal of the Royal Statistical Society 109, 296."
  - "Johnson, N. L. (1959). On an extension of the connexion between Poisson and chi-squared distributions. Biometrika 46, 352–362."
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlskellam())
  mlskellam(x)
---

The Skellam distribution describes the difference between two independent Poisson counts.

## Details

Zero rates are attained members, including the point mass at zero when both vanish. A positive or negative Poisson boundary may therefore be returned directly. Constant nonzero data need not give a point mass. With one positive rate fixed, the log likelihood is strictly concave in the other rate; a bracketed score solve finds its unique maximum, including the zero endpoint. Holding either or both rates fixed is supported; data outside a fixed zero-rate member's support are rejected.

For a joint interior fit, writing $s=\sqrt{\mu_1\mu_2}$ and $\theta=\log(\mu_1/\mu_2)/2$ gives the score equation $\mu_1-\mu_2=\bar x$. The remaining equation is $n^{-1}\sum_i I'_{|x_i|}(2s)/I_{|x_i|}(2s)=\cosh\theta$, with $I'_v(z)=I_{v+1}(z)+vI_v(z)/z$. The fitter searches the resulting scalar profile and compares every nominated maximum with the exact zero-rate face. Faces win log likelihood ties within $10^{-8}n$. Uniqueness is not assumed. The search uses a finite logarithmic grid and local refinement; this is numerical global-search evidence rather than a proof that every maximum is found.

The mean and variance identify both rates. Likelihoods are bounded by one per observation. Both-sided samples exclude zero-rate faces. A convolution mass is at most the maximal mass of either Poisson factor, which vanishes when that rate diverges. Continuity on the closed quadrant therefore gives a finite maximizer for every finite integer sample, also with fixed rates whenever the support permits the sample. Constant data $k$ have the joint MLE $(k,0)$ for $k>0$, $(0,-k)$ for $k<0$, and $(0,0)$ for zero: condition on the opposite Poisson count and use the decreasing maximal Poisson mass as its integer mode increases. For nonnegative data with positive mean, the inward profile derivative per observation at the Poisson face is $\bar x\,n^{-1}\sum_i 1/(x_i+1)-1$; reflect the data for the other face. This is a local boundary diagnostic, not a sufficient global rule; the fitter compares likelihoods.

## d/p/q/r

Zero rates include the signed Poisson limits; sampling subtracts two independent Poisson variates.
