---
id: dnorm
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mean and sd > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Discretized normal
support: {type: integer, lower: -inf, upper: inf, closed: [true, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: ddnorm
  sampler: rdnorm
  control:
    mean: "Optional held value; NULL estimates it."
    sd: "Optional positive held value; NULL estimates it."
    reltol: "Tolerance for the Newton decrement."
    iterlim: "Maximum iterations of Newton fitting."
log_density: "log((erf((x + 1 - mean)/(sqrt(2)*sd)) - erf((x - mean)/(sqrt(2)*sd)))/2)"
limits:
  - name: point mass
    firmness: proved
    when: 'Scale satisfies $\sigma\to0$ with the latent normal centre inside one observed cell.'
    detect: 'Both coefficients are free and all observations are equal.'
    status: PointMass
  - model: binom
    firmness: proved
    name: shifted Bernoulli
    when: 'Scale satisfies $\sigma\to0$ and location approaches the shared boundary of two adjacent cells at a rate that preserves their observed proportions.'
    detect: 'Both coefficients are free and the integer sample range is one.'
    status: TwoPointLimit
  - name: zero-scale limit
    firmness: proved
    when: 'Scale satisfies $\sigma\to0$ with location fixed.'
    detect: 'Location is held and every observed cell closure $[x_i,x_i+1]$ contains that location.'
    fixed: [mean]
    status: ScaleLimit
holds: []
examples: |
  x <- diff(datasets::discoveries)
  mldnorm(x)

group: normal and Student
construction:
- discretisation
relations:
  discretisation_of:
  - family: norm
    at:
      mean: mean
      sd: sd
fit:
  method: convex-solve
  certified: 'yes'
  note: 'dnorm/README.md, Details; dnorm/mle.hpp (public fitter and held overloads); dnorm/test.cpp: The grouped-normal
    likelihood is strictly concave in centred mean/sd and inverse sd. Held mean is a linear restriction in these
    coordinates, held sd fixes inverse sd; damped Newton targets the unique maximum.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model rounded Gaussian measurements recorded on the integer lattice.
summary: Discretized normal provides a model for rounded Gaussian measurements recorded on the integer lattice.
aliases:
- "discrete normal"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists exactly when
    the integer sample range is at least two. Constant samples and samples
    on two adjacent integers have unattained suprema. With $\sigma>0$
    held, free location always has a unique finite maximum. With $\mu$
    held, a unique finite scale maximum exists unless every observed
    cell closure $[x_i,x_i+1]$ contains $\mu$; in that case the supremum
    occurs only at zero scale. Holding both coefficients evaluates the
    likelihood without optimization.
  reason: >-
    The grouped-normal likelihood is strictly concave in
    $(\mu/\sigma,1/\sigma)$. Holding either coefficient imposes an affine
    constraint in these coordinates. Away from the stated zero-scale
    exceptions, escaping parameters lose likelihood, so continuity and
    compactness give attainment. The direct boundary argument is given below.
---

The discretized normal distribution is the floor of a continuous normal variable. Each observation records the latent variable in an interval from the observed integer to the next integer. The coefficients $\mu$ and $\sigma$ describe the latent normal location and standard deviation, rather than the moments of the observed integers.

## Details

The [existence analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) and the argument below establish the cell-collapse conditions. Strict concavity proves uniqueness of an attained maximum; a numerical iteration can still fail to reach it within its iteration budget.

With both parameters free, constant samples and samples on two adjacent integers have no finite maximum likelihood estimate. The limiting adjacent-cell probabilities equal their observed proportions.
Both held parameters evaluate the likelihood without fitting. With one parameter held the fitter handles its distinct boundary conditions explicitly.

The grouped normal log likelihood is strictly concave in $(\mu/\sigma,1/\sigma)$; the fitter uses damped Newton iteration in centred versions of these coordinates. With both parameters free, every sample with range at least two has a unique finite maximum. Holding $\sigma$ gives a unique finite location fit even for constant data. Holding $\mu$ gives a finite scale fit unless every observed cell closure contains that mean, in which case the zero-scale limit wins.

Reference: Burridge, J. (1981). A Note on Maximum Likelihood Estimation for Regression Models Using Grouped Data. Journal of the Royal Statistical Society, Series B (Methodological), 43(1), 41–45. https://doi.org/10.1111/j.2517-6161.1981.tb01146.x.

## Existence and identification

Write $p_k=\Phi((k+1-\mu)/\sigma)-\Phi((k-\mu)/\sigma)$.
The right-tail expansion is
$\log p_k=-(k-\mu)^2/(2\sigma^2)-\log k+O(1)$.
Its quadratic and linear coefficients identify scale and mean respectively.
For a sample on two adjacent cells, each finite model has positive mass outside
those cells. The saturated multinomial bound is therefore strict.
If the lower-cell fraction is $p$, take
$\mu=k+1-\sigma\Phi^{-1}(p)$ and let $\sigma\downarrow0$.
The two probabilities tend to $p$ and $1-p$, attaining the bound only in the
closure. The same argument gives a point-mass supremum for constant data.
For seven 3s and three 4s the bound is $7\log(.7)+3\log(.3)=-6.10864302055$.

Conversely, along $\sigma\to\infty$ every fixed-cell probability is at most
$1/(\sigma\sqrt{2\pi})$, and vanishes. With bounded scale, an escaping location
also loses every fixed cell. Along $\sigma\downarrow0$, a positive limiting
likelihood requires the latent centre to lie in the closure of every observed
cell. These closures intersect only for a constant or adjacent-cell sample.
Thus a positive likelihood level set for range at least two is compact inside
the parameter domain. Continuity gives attainment, and strict concavity in
$(\mu/\sigma,1/\sigma)$ gives uniqueness. This is the grouped-normal
concavity result of Burridge (1981); centring the data is an invertible affine
change of these coordinates.

When scale is held, location escapes in either direction lose all cells;
strict concavity gives a unique finite fit. When mean is held, infinite scale
loses all cells, and zero scale loses a cell unless all closures contain that
mean. In the exceptional case each finite-cell probability is strictly below
its zero-scale limit (1 for an interior centre, 1/2 for a boundary centre),
so the supremum is unattained. Otherwise compactness gives attainment.
Neither this discrete likelihood nor its held subsets are unbounded above:
every log probability is nonpositive.

## R notes

For adjacent values `k` and `k + 1`, `mlbinom(x - k, size = 1)` fits the
limiting Bernoulli probabilities. Both held coefficients evaluate the
likelihood without fitting.

## Decision history

2026-10-30 (TASK-125): log interval probabilities use the shared normal log-tail helper, whose Mills expansion starts before the backend tail becomes subnormal. The first omitted term bounds the alternating expansion in the selected large-tail regime; truth rows check the resulting interval differences.

## Stop details

With mean held, a noninteger mean concentrates in its discretization cell. At an integer seam the zero-scale face assigns one half to each adjacent cell; its likelihood need not be the saturated empirical likelihood.
