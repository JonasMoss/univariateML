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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
  method: profile-search
  certified: 'yes'
  note: 'skellam/README.md, Details; one held rate has a strictly concave bracketed solve; the joint profile uses
    branch-and-bound with a tail bound, conditional on computed comparisons and a practical likelihood margin.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model differences between independent Poisson event counts.
summary: Skellam provides a model for differences between independent Poisson event counts.
aliases: []
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With both rates free, every nonempty finite integer sample has an
    attained finite MLE in the closed nonnegative quadrant. General joint
    uniqueness is not asserted. A constant sample $k$ has the maximum
    $(\mu_1,\mu_2)=(\max(k,0),\max(-k,0))$. With one rate held,
    the remaining rate has a unique finite MLE whenever the held model
    permits the observations. A held zero rate restricts the observations
    to the appropriate signed Poisson support. Holding both rates leaves
    only likelihood evaluation.
  reason: >-
    A convolution mass is at most the maximal mass of either Poisson
    factor. That bound tends to zero when either rate diverges. Continuity
    on the closed quadrant and a positive likelihood at some finite point
    therefore give compact positive likelihood level sets and attainment.
    With one positive rate held, the conditional paired-count variance
    is strictly less than its mean, giving strict concavity in the free
    rate. With a zero rate held, the problem is an ordinary signed Poisson
    fit. These arguments prove existence and conditional uniqueness,
    not joint uniqueness.
---

The Skellam distribution describes the difference between two independent Poisson counts. The rates $\mu_1$ and $\mu_2$ describe the positive and subtracted counts, respectively. Their difference is the mean of the observed difference.

## Details

Zero rates are attained members, including the point mass at zero when both vanish. A positive or negative Poisson boundary may therefore be returned directly. Constant nonzero data need not give a point mass. With one positive rate fixed, the log likelihood is strictly concave in the other rate; a bracketed score solve finds its unique maximum, including the zero endpoint. Holding either or both rates fixed is supported; data outside a fixed zero-rate member's support are rejected.

The mean and variance identify both rates. Likelihoods are bounded by one per observation. Both-sided samples exclude zero-rate faces. A convolution mass is at most the maximal mass of either Poisson factor, which vanishes when that rate diverges. Continuity on the closed quadrant therefore gives a finite maximizer for every finite integer sample, also with fixed rates whenever the support permits the sample. Constant data $k$ have the joint MLE $(k,0)$ for $k>0$, $(0,-k)$ for $k<0$, and $(0,0)$ for zero: condition on the opposite Poisson count and use the decreasing maximal Poisson mass as its integer mode increases. For nonnegative data with positive mean, the inward profile derivative per observation at the Poisson face is $\bar x\,n^{-1}\sum_i 1/(x_i+1)-1$; reflect the data for the other face. This is a local boundary diagnostic, not a sufficient global rule; the fitter compares likelihoods.

## R notes

Supply `mu1` or `mu2` to hold the corresponding rate. Holding both evaluates the likelihood. Numerical failure stops fitting, and increasing `iterlim` raises the search budget.

## Decision history

2026-10-30 (TASK-125): the large-order expression divides before multiplying and uses log coordinates when the order/argument ratio overflows. The independent truth rows at count 1e300 exercise the change.

### Joint-search certificate (recorded 2026-11-02)

For a joint interior fit, writing $s=\sqrt{\mu_1\mu_2}$ and $\theta=\log(\mu_1/\mu_2)/2$ gives the score equation $\mu_1-\mu_2=\bar x$. The remaining equation is $n^{-1}\sum_i I'_{|x_i|}(2s)/I_{|x_i|}(2s)=\cosh\theta$, with $I'_v(z)=I_{v+1}(z)+vI_v(z)/z$. The fitter searches the resulting scalar profile and compares the exact zero-rate face. Faces win computed log likelihood ties within $0.5\cdot10^{-8}n$. A logarithmic grid and local refinements nominate candidates; a separate branch-and-bound pass bounds the remaining finite interval. The Poisson-mode bound excludes the far tail, and the positive series bounds the interval adjacent to zero. Uniqueness is not assumed. If comparisons are exact, the returned likelihood is within $10^{-8}n$ of the global maximum. In floating point this is a practical margin, conditional on the computed masses, scores and comparisons, not a directed-rounding guarantee. Nonfinite values, representational stagnation or an exhausted search return iteration failure.

The following curvature bound does not require concavity or unimodality.
Put $t=\min(\mu_1,\mu_2)$, $d=|\bar x|$, and $q=t(t+d)$.
Conditional on a difference $k$, the paired count $J$ has weights
proportional to $q^j/[j!(j+|k|)!]$. Its successive ratios satisfy
$(j+1)p_{j+1}/p_j=q/(j+|k|+1)$, which decreases in $j$.
Consequently the size-biased $J-1$ is stochastically dominated by $J$,
so $\operatorname{Var}(J)\leq E J$; also
$E J\leq q/(|k|+1)$. Differentiating the positive series along the
mean-constrained line and discarding nonpositive terms gives
$$
  {d^2\over dt^2}\log p_k(t)\leq {2 E J\over t(t+d)}
  \leq {2\over |k|+1}.
$$
Thus the sum of these bounds supplies a quadratic upper envelope for
branch-and-bound intervals. The midpoint score is computed from the two
neighboring mass ratios. For an interval of half-width $h$, the midpoint
likelihood plus $|\ell'|h+Mh^2/2$ is an upper bound, where $M$ is the sum
of the per-observation curvature bounds. The search bisects until every
bound is within $0.5\cdot10^{-8}n$ of the best candidate, reserving the
same amount for a possible face tie. On $[0,\epsilon]$, the positive
series bounds the profile by $\ell(\epsilon)+2n\epsilon$. Above the
finite search interval, a convolution mass is bounded by the maximal
Poisson mass, at most $1/\sqrt{2\pi\lfloor t\rfloor}$ for $t\ge2$.
This establishes a bound, not profile unimodality. The computed masses
and comparisons are not outward rounded.

### Probability evaluation

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

The joint-rate cover counts curvature-bounded intervals, rather than local optimizer steps. On exhaustion it retains the best feasible pair of rates and a practical likelihood rounding margin; the global upper bound remains infinite.
