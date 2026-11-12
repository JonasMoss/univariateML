---
id: cmp
inference_numeric:
  - regime: "geometric member with dispersion known"
    available: true
    condition: "0 < lambda < 1 free; nu = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite lambda > 0 and nu > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Conway--Maxwell--Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: nu
  symbol: '\nu'
  role: dispersion
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dcmp
  sampler: rcmp
  control:
    lambda: "Optional fixed nonnegative finite rate; NULL estimates it."
    nu: "Optional fixed nonnegative finite dispersion; NULL estimates it. At zero lambda must be below one."
    reltol: "Tolerance on moment residuals."
    iterlim: "Maximum number of moment-solving iterations."
log_density: "x*log(lambda)-nu*loggamma(x+1)-log(Sum(lambda**k/factorial(k)**nu,(k,0,oo)))"
limits:
  - name: adjacent two-point law
    firmness: proved
    when: 'As $\nu\to\infty$, adjusting $\lambda$ can retain mass on two adjacent integers.'
    detect: With both coefficients free, both of two adjacent integers are observed. With positive $\lambda$ held and $\nu$ free, all observations lie in $\{0,1\}$, including constant samples.
    status: TwoPointLimit
  - name: point mass
    firmness: proved
    when: 'As $\nu\to\infty$, a suitable path of $\lambda$ concentrates mass at a positive integer.'
    detect: With both coefficients free, all observations equal the same positive integer.
    status: PointMassLimit
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlcmp())
  mlcmp(x)

group: Poisson, binomial and negative binomial
construction:
- base
- generalisation
relations: {}
fit:
  method: convex-solve
  certified: 'yes'
  note: 'cmp/README.md, Moment-error acceptance; cmp/mle.hpp, cmp_domain_mle; cmb/acceptance.hpp, certify: the public domain wrapper checks scalar or joint moment proposals with concave tangent bounds and score half-space coefficient enclosures, including the geometric KKT face. Atomic and held-geometric subsets are exact. The historical special/cmp.hpp solver remains a proposer; its evaluator remainders exclude roundoff.'
areas:
- counts and ecology
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, all-zero samples attain their maximum at
    $\lambda=0$, with $\nu$ unidentified. Positive constants and samples
    supported on two adjacent integers have an unattained supremum.
    Other samples have a unique finite maximum, possibly at the geometric
    boundary $\nu=0$. With finite $\nu$ held, the free rate has a unique
    maximum, including zero for all-zero data. With positive $\lambda$
    held, a unique finite dispersion maximum exists when some observation
    exceeds one. Samples supported on zero and one instead approach their
    supremum as $\nu\to\infty$. Holding $\lambda=0$ requires all-zero
    data and leaves dispersion unidentified. Holding both coefficients
    requires only a valid law and compatible data.
  reason: >-
    The sufficient statistics are $x$ and $\log(x!)$. Their covariance is
    positive definite at positive rate and dispersion, giving strict
    concavity in $(\log\lambda,-\nu)$. At a fixed positive mean, the
    fitted mean log factorial decreases strictly with dispersion, from
    its geometric value to the linear interpolation between the two
    neighboring integer log factorials. Data above that lower hull give
    an interior moment solution or the constrained geometric maximum.
    Data on the hull give the stated exposed-face limits. With rate held,
    the mean log factorial decreases strictly to zero as dispersion grows;
    at its lower domain endpoint it is finite for rate below one and
    diverges otherwise.
classification: specialist
classification_reason: Used to model event counts with either underdispersion or overdispersion relative to Poisson.
summary: Conway--Maxwell--Poisson provides a model for event counts with either underdispersion or overdispersion
  relative to Poisson.
aliases:
- "CMP"
- "COM–Poisson"
references:
- 'Shmueli, G., Minka, T. P., Kadane, J. B., Borle, S. and Boatwright, P. (2005). A useful distribution for fitting
  discrete data: revival of the Conway–Maxwell–Poisson distribution. Journal of the Royal Statistical Society: Series
  C (Applied Statistics), 54, 127–142. doi:10.1111/j.1467-9876.2005.00474.x.'
key_references:
- 1
---


The Conway--Maxwell--Poisson distribution extends Poisson counts with a dispersion coefficient. The rate $\lambda$ is generally not the mean. Dispersion $\nu=1$ gives Poisson, while $\nu=0$ gives a geometric law when $\lambda<1$.

## Details

Either coefficient can be held. With both free, the fit matches the two
sufficient-statistic means when the maximum is interior. Let $m=\bar x>0$
and $t$ be the empirical mean of $\log(x!)$. The geometric law of mean
$m$ has rate $m/(1+m)$. If $t$ is at least its mean log factorial, the
constrained maximum is this geometric member. Otherwise dispersion is
positive, unless the empirical statistic lies on an adjacent-point edge
of its convex support, where only a limiting law attains the supremum.
This is a mathematical classification; finite moment and tail budgets can
still prevent a numerical fit.

Rate zero is the attained point mass at zero. Its free dispersion is
unidentified. When dispersion is zero, the rate must be below one for the
law to normalize. Holding a positive rate leaves a strictly concave
one-dimensional dispersion problem. With rate below one it can attain
the geometric endpoint. With rate at least one the normalizable domain
requires strictly positive dispersion.

The [existence analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md)
discusses the limit paths. The raw shared solver supplies a moment proposal; the public domain wrapper
adds the quantitative acceptance check below.

## R notes

Use `lambda` and `nu` to hold either coefficient. Unidentified free dispersion
at rate zero is represented by one. Unattained adjacent and positive
point-mass limits stop with a named diagnostic.

Probability evaluation uses the certified CMP normalizer, stable centered
log weights and direct tail summation. A calculation exceeding the
normalizer or tail work budget returns `NaN` with a warning. Quantile
search is limited to counts below $9\times10^{15}$. Random generation
uses inversion with R's RNG.

## Decision history

### Moment and tail arithmetic

2026-10-30 (TASK-125): moment sums use compensated accumulation, the saddle-point quotient retains its division residual, and geometric tail bounds can be evaluated in log coordinates. For mode zero or one, the second-count weight uses `exp((2-mode)*eta)*exp2(-nu)`, avoiding amplification of rounding in `log(2)`. The 73-row independent reference grid measures errors; the truncation certificate excludes floating-point roundoff.

## d/p/q/r

The normalizer and both tails are summed directly.

### Poisson probability member

At nu one, the normalizer is exactly the Poisson exponential series. At rate
$10^8$, a fresh numerical normalizer can perturb a $10^{-300}$ tail beyond the
relative probability contract even when its log error is small. This member
therefore retains the Poisson probability evaluator directly.

## Stop details

The public domain entry point retains adjacent-law witnesses, with held lambda fixing the Bernoulli odds. The shared legacy CMP solver is unchanged; the public domain wrapper now certifies its proposals. Attained geometric and zero-count members remain supported.

## Moment-error acceptance

The binding-neutral public route is `cmp_domain_mle`, which retains the shared
`cmp_mle` solver as a proposer. For a joint interior fit it uses coordinates
$(\alpha,\nu)$ with $\alpha=\log\lambda-s\nu$, where $s$ is the proposal's
centered log-factorial slope. This affine change keeps profile covariance and
score calculations from subtracting large nearly equal log-factorial moments.
At every surrounding point the evaluator's centered residuals recover the
likelihood and the expectations of $X$ and $\log(X!)-sX$.
With rate held, only dispersion varies; with dispersion held, only log rate
varies. [The concavity check](../cmb/acceptance.hpp) bounds the global likelihood
and encloses free coefficients using score half-spaces. It transforms the
canonical rectangle through $\lambda=\exp(\alpha+s\nu)$ and requires each
reported free coefficient to lie within $10^{-8}(1+|\hat\theta|)$ of every
endpoint of its enclosure. Failed score surrounds or coefficient checks stop.

At zero dispersion, the geometric rate is analytic. A free-dispersion
geometric proposal must additionally have a strictly nonpositive dispersion
score, separated from zero by $10^{-12}(1+|\overline{\log(X!)}|)$.
Both endpoints of the rate bracket must pass the dispersion sign check.
Since $\operatorname{Cov}(X,\log(X!))>0$, that score is increasing in log rate,
so the sign also holds at the enclosed geometric rate root. The bracket
supplies the free rate enclosure and the supporting planes give a constrained KKT upper bound for all $\nu\ge0$. With rate held,
the same score inequality certifies the exact endpoint directly. An uncertain
seam stops numerically; a near-geometric interior fit is not silently replaced
by an unchecked endpoint. Held zero dispersion uses the analytic geometric
fit. At zero rate any dispersion defines the same atom, so dispersion is
unidentified; for example two zero observations have likelihood one at both
$(\lambda,\nu)=(0,0)$ and $(0,30)$.

Each moment evaluation requests remainder tolerance $2\times10^{-16}$
and requires the evaluator's relative remainder bound below $10^{-12}$.
The shared certificate's likelihood and coordinate margins are specified in
[acceptance.hpp](../cmb/acceptance.hpp); its coefficient inclusion is conditional
on these computed moments, including the protected evaluator's roundoff
qualification.

The wrapper clamps its solver control to `min(iterlim,100000)`. Its acceptance
pass has at most that many refinements, with two or three moment evaluations
per refinement. Those evaluations use the remaining
`100000*min(iterlim,100000)` work allowance after the proposer's reported work.
The protected evaluator's own work ledger and fixed `max_terms` control remain
a separate shared dependency; this wrapper does not revise its internal loops.
Budget stops in the new pass retain feasible finite witnesses where available.
The historical `cmp_mle` entry point itself does not promise this acceptance
certificate; callers needing it must use `cmp_domain_mle`.
