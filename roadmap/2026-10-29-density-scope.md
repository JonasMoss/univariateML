# Scope decisions for extraDistr and actuar densities

**Status:** exclusions decided 2026-10-29. Later the same day the author
chose full coverage of both packages ("cover all of extraDistr and
actuar"), including families whose unrestricted MLE does not exist; see
[Full coverage plan](#full-coverage-plan-2026-10-29), which overrides the
`dbhatt`, `dcat` and `dphtype` rows below. Tiers 1–3 are done (merged 2026-10-29); tier 4 (vector parameters) is not started.

The package fits univariate distributions with a fixed, finite number of
parameters for each specified model. Covering another package does not
require adopting every density it exports. Keep explicit records of
deliberate exclusions, study the useful missing families, and report
nonexistence rather than returning a numerical local optimum as an
unrestricted MLE.

The [coverage inventory](../notes/evaluations/2026-10-29-density-coverage.html)
lists every density name in installed extraDistr 1.10.0.5 and actuar 3.3.7,
including aliases and restrictions of existing fitters. This document
governs inclusion decisions; the inventory records implementation coverage.

## Deliberate exclusions

| Package | Density | Decision and reason |
|---|---|---|
| extraDistr | `dbhatt`, Bhattacharjee or normal–uniform convolution | Skip. Too specialized to justify a dedicated fitter and its maintenance. |
| extraDistr | `dcat`, categorical | Skip. The unrestricted probability vector has a dimension determined by the category set; it does not give the fixed-dimensional parametric family intended here. A fixed category count would define a finite-dimensional model, but adding categorical fitting is still outside the chosen scope. |
| extraDistr | `dbvnorm`, bivariate normal | Skip. Multivariate observations. |
| extraDistr | `dbvpois`, bivariate Poisson | Skip. Multivariate observations. |
| extraDistr | `ddirichlet`, Dirichlet | Skip. Multivariate observations. |
| extraDistr | `ddirmnom`, Dirichlet–multinomial | Skip. Multivariate count vectors. |
| extraDistr | `dmnom`, multinomial | Skip. Multivariate count vectors. |
| extraDistr | `dmvhyper`, multivariate hypergeometric | Skip. Multivariate count vectors. |
| actuar | `discretise`, `discretize` | Exclude from density coverage. These are discretization utilities. |

These are scope exclusions, not claims that their MLEs do not exist.
No error-only fitting functions are needed for out-of-scope distributions.

## Missing extraDistr families to study

| Density | Study scope |
|---|---|
| `dskellam` | Two Poisson rates, with zero-rate boundary models. |
| `ddlaplace` | Discrete Laplace; establish location and boundary conventions. |
| `ddnorm` | Discretized normal; use interval probabilities, with stable log differences and analysis of scale limits. |
| `ddgamma` | Discretized gamma; continuous gamma fitting is not its MLE. |
| `dhcauchy` | Half-Cauchy with endpoint fixed at zero; estimate scale. |
| `dht` | Half-t with endpoint fixed at zero; estimate scale and degrees of freedom, examining the half-normal limit. |
| `dnhyper` | Negative hypergeometric; specify known design parameters and integer parameters to estimate. |
| `dslash` | Location-scale slash; study likelihood, identification and boundary behavior. |
| `dmixpois` | Fixed component count, parameter identification, component weights and boundary reductions. |
| `dmixnorm` | Study as a likelihood-failure case. Unrestricted fitting with at least two components is unbounded through component scale collapse; useful constrained variants require an explicit model definition. |

Acceptance for study does not promise a fitter. Each study should establish
the model, its likelihood failures and limits, a fitting strategy, and the
cost of implementation before recommending inclusion.

## Partial extraDistr coverage to study

| Density | Existing coverage and remaining question |
|---|---|
| `dhnorm` | `mlhnorm` estimates a translation. Add a fixed-zero-endpoint option or separate scale-only fitter; its scale MLE is the root mean square of the observations. |
| `dtpois`, `dtbinom` | Zero truncation exists. Study arbitrary known truncation bounds. Binomial size needs an explicit fixed or estimated policy. |
| `dnsbeta` | Known bounds reduce to `mlbeta` by affine transformation, including the likelihood Jacobian. Free bounds belong in the rejection policy below. |
| `dfrechet` | Known location reduces to `mlinvweibull`; the current GEV domain covers only part of the free-location family. Study wider coverage and its limits separately. |
| `dfatigue` | The current fitter fixes location at zero. Retain the known-location model; reject unrestricted location estimation using the collapse argument in its distribution README. |
| `dgev`, `dgpd`, `dlst` | Retain and document the current shape, threshold and degrees-of-freedom restrictions. Any extension needs its own existence and boundary analysis. |

## Remaining actuar cases

Most actuar density names already have fitters or equivalent models.
The remaining cases are model variants and one larger fitting project.

| Density | Decision |
|---|---|
| `dgenbeta`, generalized beta on a bounded interval | Study with its scale, the upper endpoint, held fixed. This is a genuine missing three-shape family. With scale free, unrestricted likelihood is unbounded when `shape2 < 1`. |
| `dphtype`, phase-type | Accepted for study as a larger project. Observations are scalar, so it is univariate despite its vector and matrix parameters. Specify a fixed phase count and representation; investigate identification, nonunique representations and boundary reductions. |
| `dpareto2`, Pareto II | A known minimum reduces to `mllomax`. Reject unrestricted joint estimation of minimum, shape and scale: a joint endpoint and scale collapse makes likelihood unbounded. Constrained versions would be separate studies. |
| `dpareto3`, Pareto III | A known minimum reduces to `mlllogis`. Reject unrestricted free minimum because shapes below one produce an endpoint singularity. |
| `dpareto4`, Pareto IV | A known minimum reduces to `mlburr`. Reject unrestricted free minimum because `shape2 < 1` produces an endpoint singularity. |
| `dfpareto`, Feller–Pareto | A known minimum reduces to `mlgb2`. Reject unrestricted free minimum because `shape2 * shape3 < 1` produces an endpoint singularity. |

There is no additional missing ordinary actuar density family after these
cases. In particular, `dlgompertz` is an inverse-Weibull alias,
`dpearson6` is a transformed-beta alias in actuar, and `dpoisinvgauss`
duplicates `dpig`. actuar's `dgenpareto` is beta-prime, covered by
`mlbetapr`; it is not extraDistr's threshold generalized Pareto.

## Unbounded likelihood and explicit rejection

Distinguish three outcomes: an out-of-scope distribution, a supported
constrained model, and an unrestricted model with unbounded likelihood.
An explicit fitting error is appropriate for the third. It should identify
the failing free parameter and point to an available fixed-endpoint model.

| Unrestricted model | Failure or useful alternative |
|---|---|
| Shifted Weibull | Choose shape between zero and one and move the lower endpoint toward the sample minimum. Use ordinary Weibull after subtracting a known threshold. Neither audited package exports a shifted-Weibull density directly. |
| Nonstandard beta with free bounds | A shape below one makes density diverge as its corresponding endpoint approaches the sample extreme. Known bounds permit ordinary beta fitting after transformation. |
| Generalized beta with free scale | `shape2 < 1` gives a singularity at the upper endpoint. Study fixed scale. |
| Pareto II with free minimum, shape and scale | Joint collapse, derived below. Known minimum permits Lomax fitting. |
| Pareto III, Pareto IV and Feller–Pareto with free minimum | Their lower-endpoint powers can be negative. Known minimum permits existing fitters. |
| Fatigue with free location | The existing fatigue README gives a joint location, shape and scale collapse path. |
| Normal mixture with free component scales | A component can collapse onto an observation while another retains positive density at the other observations. |

For Pareto II, let the sample minimum be m with multiplicity k in n
observations. Set minimum to m−ε, scale to ε and keep shape α positive.
At each minimum the density is α / (ε 2^(α+1)). At every larger
observation it is asymptotic to a positive constant times ε^α.
Consequently the likelihood is proportional to
ε^(α(n−k)−k). If n > k, choose 0 < α < k/(n−k); the likelihood
diverges as ε decreases to zero. If all observations equal m, it diverges
for any positive α. This corrects the original inventory's classification
of free-minimum Pareto II as merely missing.

The sample (1, 2, 4, 7), shape 0.1, minimum 1−ε and scale ε gives
log likelihoods approximately −9.961, −6.705, −0.258 and 6.190 at
ε = 10^−2, 10^−4, 10^−8 and 10^−12 using installed actuar.
The power argument establishes nonexistence; these values illustrate it.

Error-only coverage should remain distinct from a successful estimator
in metadata, model selection and coverage totals. A family with an MLE
for some samples but not others still requires a real fitter and
sample-specific boundary handling.

## Trigger to revisit

Choose one accepted study candidate and investigate its likelihood and
closure before implementation. Skellam, discrete Laplace and fixed-endpoint
half distributions are natural small projects. Generalized beta with a
known upper endpoint is the main remaining actuar family study;
phase-type and finite mixtures are larger projects.

## Verification

For each candidate, check density parameterization, support, identification,
attained boundaries, escaping limits, and likelihood nonexistence. Verify
any implementation against independent density calculations and an
independent likelihood search. Keep deliberate exclusions in this document
and fitting restrictions in each distribution's README.

Rebuild the complete inventory with
`python3 notes/evaluations/2026-10-29-density-coverage.py`.
The inventory fails if an installed d-prefixed export is unclassified.
Primary density definitions are in the installed package help and the
[extraDistr manual](https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf)
and [actuar manual](https://cran.r-project.org/web/packages/actuar/actuar.pdf).

## Full coverage plan (2026-10-29)

Decided after review of the sections above. Every installed density of
extraDistr and actuar gets a fitter, an equivalence, or a recorded
exclusion. Families whose unrestricted MLE never exists still get a fitter:
the parameter that breaks the likelihood must be held, and the missing-value
message says why (for example, `mlpareto4(x)` explains the collapse at the
sample minimum and suggests `mlpareto4(x, min = 0)`, which is `mlburr`).
Free-threshold fits with some other parameter held can have a bounded
likelihood again; each such subset is a separate existence question, so they
stop with "not supported" until analysed.

Changes to the review above:

- `dbhatt` is included: three parameters, two escapes (normal as
  `a -> 0`, uniform as `sigma -> 0`).
- `dcat` moves to the vector-parameter tier (empirical-frequency MLE).
- `dphtype` is out of scope: a phase-type law has many non-identified
  representations, and general phase-type fitting is a field of its own.
- Tukey lambda was missing from the inventory, which scans only
  `d`-prefixed exports; extraDistr ships only `qtlambda`/`rtlambda`. With
  free `lambda` and location-scale, the likelihood is unbounded as
  `lambda -> Inf` (centre density `2^(lambda-2)/lambda` in range-standardized
  units, elsewhere about `1/(lambda |y|)`; n = 6: log likelihood -7.34 at
  `lambda = 2`, 5.40 at 40, 29.0 at 80). A fitter needs an upper bound.
- Discretized normal (and gamma) escape when the data occupy two adjacent
  integers: seven 3s and three 4s have supremum `7 log .7 + 3 log .3 =
  -6.1086`, approached as `sd -> 0` with mean `4 - sd * qnorm(.7)`, never
  attained ("use `mlbinom(x - 3, size = 1)`").
- Fréchet with free location is GEV with `xi = 1/lambda > 0`; scale collapse
  at the minimum makes the likelihood unbounded for `lambda < k/(n-k)`
  (k = multiplicity of the minimum), so the free-location fit bounds
  `lambda >= 1` and escapes to Gumbel as `lambda -> Inf`.

| Tier | Families | Notes |
|---|---|---|
| 1 | `hcauchy`, `ht`, `hnorm` with `mu` fixable (zero endpoint), `slash`, `bhatt`, `tpois`/`tbinom` with known bounds, `dlaplace` | small; existence clear or a short analysis |
| 2 | `pareto2`, `pareto3`, `pareto4`, `fpareto`, `nsbeta` (held threshold or bounds, delegating to `lomax`, `llogis`, `burr`, `gb2`, `beta`), `frechet` | the held-parameter-with-reason mechanism |
| 3 | `dnorm`/`dgamma` (discretized), `skellam` (no `p`/`q` upstream), `nhyper` (negative binomial limit), `genbeta` with scale held (three shapes; unit-gamma limit, `mllgamma` on `1/x`), Tukey lambda (own d/p) | analysis projects, one job each |
| 4 | `cat`, `mixpois`, `mixnorm` | needs vector parameters of user-chosen length first; mixtures have label switching and local maxima; free normal-mixture scales are unbounded |
| out | 6 multivariate laws, `discretize`, `dphtype`, random-only helpers (`rcompound`, `rmixture`, `rcomppois`, `rcomphierarc`, `rcatlp`, `rsign`) | |
