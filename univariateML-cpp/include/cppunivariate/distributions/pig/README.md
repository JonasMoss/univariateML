---
id: pig
inference_numeric:
  - regime: "unreviewed or unavailable"
    available: false
    condition: "interior joint regime"
    reason: "DQM, finite full-rank information and a valid a.e. Hessian are established; numerical derivative and covariance validation for the expanded subsets is not yet implemented"
    classification: "dev/research/evaluations/2026-11-04-dqm-c-report.md#e-poisson-inverse-gaussian"
name: Poisson-inverse Gaussian
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: mean
  symbol: '\mu'
  role: mean
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape
  symbol: '\lambda'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dpig
  sampler: rpig
  control:
    reltol: "Absolute tolerance on the log parameter in the scalar search."
    iterlim: "Maximum refinement iterations per bracketed scalar peak."
log_density: "log(2*shape/pi)/2 - loggamma(x+1) + shape/mean + (x-1/2)*log(shape/(2+shape/mean**2))/2 + log(besselk(x-1/2, sqrt(shape*(2+shape/mean**2))))"
limits:
  - model: pois
    firmness: proved
    when: 'The concentration $\lambda$ tends to infinity with $\mu>0$ fixed.'
    detect: The Poisson likelihood at the fitted or held mean wins the numerical comparison, including ties within $10^{-8}n$. With both coefficients free and $\bar x>0$, a Poisson supremum occurs exactly when the variance with divisor $n$ is no greater than $\bar x$.
    status: PoissonLimit
    numerical: true
  - model: pois
    firmness: proved
    when: 'The mean $\mu$ tends to zero with positive concentration $\lambda$ held.'
    detect: All observations are zero, $\mu$ is free, and $\lambda>0$ is held.
    fixed: [shape]
    status: ZeroLimit
references:
- 'Willmot, G. E. (1987). The Poisson-inverse Gaussian distribution as an alternative to the negative binomial.
  Scandinavian Actuarial Journal, 1987(3-4), 113-127. DOI: 10.1080/03461238.1987.10413823.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlpig())
  mlpig(x)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: pois
    at:
      lambda: mix
    mixed: lambda
    mixing:
      family: invgauss
      at:
        mean: mean
        shape: shape
    when: shape > 0
fit:
  method: profile-search
  certified: 'no'
  note: "pig/mle.hpp:39-106: The free mean profiles to the sample mean; log-shape meshes and local peak refinements, or a held-shape log-mean search, have no global certificate. Poisson selection uses a numerical likelihood tolerance."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model insurance claim frequencies with inverse-Gaussian heterogeneity in intensity.
summary: Poisson-inverse Gaussian provides a model for insurance claim frequencies with inverse-Gaussian heterogeneity
  in intensity.
aliases:
- "PIG"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free and positive sample mean, a finite MLE
    exists exactly when the variance with divisor $n$ exceeds the sample
    mean. Otherwise the Poisson supremum is unattained. Uniqueness of a
    finite shape maximum is not established. All-zero samples attain their
    maximum at $\lambda=0$, with $\mu$ unidentified. With positive
    $\lambda$ held, every sample containing a positive count has a finite
    mean maximum; all-zero samples instead have an unattained mean-zero
    supremum. With $\mu$ held and a sample containing positive
    counts, a finite MLE exists if and only if some finite shape has
    likelihood at least as large as the Poisson likelihood at that same mean. With $\lambda=0$ held, only
    all-zero data are compatible.
  reason: >-
    Profiling in $t=\lambda/\mu$ at fixed
    $z=\sqrt{\lambda(2+\lambda/\mu^2)}$ gives the unique mean
    $\hat\mu=\bar x$. The reduced shape score is strictly positive
    when the sample variance is no greater than the mean. Under strict
    overdispersion, its inward derivative at the Poisson face is positive,
    while likelihood tends to zero at the shape-zero end. The local
    derivation linked in Details proves these signs and attainment, but
    does not prove uniqueness of the shape maximum.
---

The Poisson-inverse Gaussian distribution mixes a Poisson count with an inverse Gaussian mean. It models overdispersed nonnegative counts.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) derives the stated limits and held-parameter conditions.

The coefficient $\mu$ is the inverse-Gaussian mixing mean and $\lambda$ controls its concentration. For $\lambda>0$ the count mean is $\mu$ and smaller $\lambda$ gives more mixing variation. Either coefficient, or both, can be held fixed. With both free the fitted mean is exactly the sample mean. The fitter searches all bracketed peaks on a broad log-parameter grid and compares the Poisson likelihood; it does not assume a unique peak. A search reaching its range boundary without a winning Poisson face reports failure.

With both parameters free and a positive sample mean, a population-form sample variance no greater than the mean gives the Poisson supremum, including equality. Strict overdispersion guarantees a finite maximum; a gain smaller than the numerical tie tolerance still selects Poisson.

The value $\lambda=0$ is the attained zero point mass. All-zero data with free shape return $\lambda=0$, preserving held $\mu$ or choosing $\mu=1$ when unidentified. With positive $\lambda$ held and mean free, all-zero data still have an unattained mean-zero supremum. With both parameters fixed the likelihood is evaluated directly. The infinite-mean Poisson-Levy face is strictly dominated for every finite sample when mean is free, including with shape fixed.

The [likelihood derivation](../../../../../dev/research/evaluations/2026-10-29-pig/analysis.md)
gives the mean reduction, the global dispersion criterion and the inward
derivative at the infinite-mean face. Its historical reference-density
remarks describe the former external implementation.

## R notes

A winning Poisson comparison stops with advice to use mlpois. Its numerical
tie tolerance is 1e-8 times sample size, so a sufficiently small finite
improvement may also be reported as the Poisson limit. At shape = 0, the
reported mean is the held value or one when mean is unidentified.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
