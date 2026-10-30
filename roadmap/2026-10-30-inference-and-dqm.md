# Inference regularity and derivative support

**Status:** initial source audit and mathematical classification, recorded
2026-10-30 at `c61a4979c6169388dea97c6b821b3bd89e530dfd`. No inference API is
implemented by this plan. The inventory covers all 98 families; 69 remain
unreviewed mathematically, and the other 29 have the conditional classifications
below. These are model statements, not a certification of the current fitters.

Extend the [package and documentation plan](2026-10-30-package-and-documentation.md)
with an inference catalog indexed by free parameters, parameter regime and
hypothesis. A family-wide yes/no label cannot capture regular submodels,
singular information, nonregular nuisance parameters or integer coordinates.
Missing procedures and missing evidence must be visible in the catalog.

## Current and historical implementation

The reproducible [source inventory](../notes/evaluations/2026-10-30-inference-inventory.csv)
records source paths, revision, initial DQM status and inference gaps. Rebuild
with `python3 notes/evaluations/2026-10-30-inference-inventory.py --revision
c61a497`. It examines committed files, not concurrent working-tree changes.

- There is no uniform public per-observation score, OPG, covariance or sandwich
  API in the audited source. R has parametric percentile bootstrap intervals
  through `bootstrapml()` and `confint.univariateML()`.
- Ten family folders contain explicit `void hessian(...)` methods: betapr,
  burr, cauchy, ged, invburr, logis, paralogis, sged, sstd and std. Seven contain
  explicit `void gradient(...)` methods. Other families have scalar scores,
  differently named derivatives or numerical derivatives; these counts do not
  mean that derivatives are wholly absent elsewhere.
- The methods are internal optimizer components, not a validated inference
  interface. For example, betapr and burr contain two-coordinate costs while
  their current models also have scale parameters. Some costs use transformed
  coordinates and divide the objective by sample size. A method's presence is
  not evidence of full free-parameter coverage.
- The parent of `bf5a3ac` contains Hessian methods in 41 family folders. Commit
  `bf5a3ac` removed 33 unused Cost structs, supporting infrastructure and old
  derivative-validation outputs; `161f064` subsequently removed KumarCost.
  Earlier analytical-Hessian work includes `e2e1f3e`, `c81cbca`, `2f0ca37`,
  `e4a0257`, `8f42ab6` and `2670631`. Recover methods as candidates with their
  tests and mathematical meaning; do not restore them as trusted inference code.
- Historical `LaplaceCost::hessian()` returned model Fisher information, not
  the pointwise observed Hessian in the location direction. Historical uniform
  and Pareto costs carried `regular_params()` markers. These distinctions must
  survive recovery; neither a generic Hessian name nor coordinate masking
  establishes an inference theorem.
- `quality/harness/check.py` derives total scores and observed information at
  frozen fits in development tooling. It is not a per-observation production
  implementation, does not form the OPG, and does not establish DQM. Its special
  handling of nonsmooth likelihoods is not an inference classification.

## Classification requirements

For a dominated model, DQM at an interior point means that a square-integrable
score vector s exists with

$$\int\left(\sqrt{p_{\theta+h}}-\sqrt{p_\theta}
  -\tfrac12 h^T s_\theta\sqrt{p_\theta}\right)^2d\mu
  =o(\lVert h\rVert^2).$$

Record DQM, finite information, information rank, likelihood differentiability
and estimator asymptotics separately. DQM can hold with singular information
or without a useful pointwise Hessian. Establishing asymptotic normality of a
particular fitted estimator also needs consistency, appropriate likelihood
conditions and adequate optimization accuracy. Being interior to a constrained
fit domain does not remove moving-support problems.

Each classification needs the model parameterization, known/held quantities,
free coordinates, parameter conditions, evidence or derivation, and exclusions.
For hypothesis tests also record the target, nuisance parameters and null.
Distinguish unsupported implementation, unreviewed theory, failure of DQM,
singular information and a procedure that is known to be invalid.

## Standard models

The normal, exponential, gamma, beta, Poisson and geometric models have DQM
with nonsingular information at ordinary interior parameters. They are smooth
parameterizations of full exponential families: their sufficient statistics
have finite covariance locally and no nonzero linear combination is constant.
This supplies the L2 differentiability and information-rank argument, rather
than relying on pointwise smoothness alone. Binomial probability has the same
property conditional on known integer size at least one.

Lognormal and logit-normal inherit this property from normal through fixed
data transformations. The library's log-gamma model inherits it from gamma
through `log(x)`. Rayleigh inherits it from an exponential model through
`x^2` and a smooth scale reparameterization. These ten full-model classifications
and the conditional binomial classification do not establish that production
scores or covariances are currently available.

## Laplace

For location m and scale b>0, the a.e. scores are

$$s_m(x)=\operatorname{sign}(x-m)/b,\qquad
  s_b(x)=-1/b+|x-m|/b^2.$$

The square root density is absolutely continuous in location with an L2
derivative; including scale gives DQM. Under the Laplace model the score
covariance is `diag(1/b^2, 1/b^2)`, so information is nonsingular. However,
the ordinary pointwise second derivative in location is zero away from the
kink. Averaging that Hessian does not estimate location information.

Under misspecification with a continuous density g positive at its unique
median m, location inference instead uses median asymptotics. The scale
equation estimates mean absolute deviation. The location sensitivity is
`2*g(m)/b`; a robust sandwich needs an estimate of this sensitivity, not a
zero empirical second derivative or the Laplace-model Fisher value. This is
a separate nonsmooth estimating-equation route.

## Moving support

The full three-parameter GEV model is DQM exactly for xi>-1/2. The two-parameter
GPD model with known threshold is DQM for xi>-1/2. These statements use the
library's conventional shape sign; they do not justify estimating the GPD
threshold as another regular coordinate. See Bücher and Segers, Proposition
3.2 and Section 4, [On the maximum likelihood estimator for the Generalized
Extreme-Value distribution](https://arxiv.org/html/1601.05702v3).

Uniform endpoints, Pareto's lower endpoint, the power family's upper endpoint,
and the thresholds of translated half-normal, half-Cauchy, half-t and Pareto II
are non-DQM when free. Their endpoint density is positive: moving the endpoint
by h excludes probability of order |h|, which cannot fit an L2 expansion with
squared remainder o(h^2). This is a support-boundary issue even when the true
endpoint is an interior point of its numerical parameter domain.

Holding those endpoints known can yield regular continuous submodels: examples
are Pareto shape, power shape and translated half-normal scale. The remaining
shape/scale submodels require their own information and derivative checks.
For other moving endpoints, inspect the local density order and support loss;
zero endpoint density by itself does not suffice. If a density behaves as
distance^kappa with a nonzero smooth coefficient, the endpoint score usually
has squared integrand of order distance^(kappa-2). This diagnoses the critical
kappa=1 case but is not a general sufficient theorem for DQM.

## Triangular

For a<c<b, the triangular density vanishes linearly at each endpoint. Free
endpoint directions have divergent squared scores, so the joint family is
not DQM. With a and b known, the mode c is DQM: its score is `-1/(c-a)` to
the left of c and `1/(b-c)` to the right, and the moving kink is continuous
in the density. Integration gives

$$I_{cc}=1/((c-a)(b-c)).$$

The ordinary pointwise second log-density derivative is positive on both
pieces, and its negative expectation is `-I_cc`, not `I_cc`. The missing
contribution comes from the moving kink. Thus even this regular one-parameter
submodel cannot use a naive pointwise Hessian as inference bread. A robust
procedure would need the sensitivity of the expected score, including the
true density at c. Derive and validate it separately.

## Cusps and singularities

For GED with free location, the location score near its cusp has magnitude
proportional to |x-m|^(nu-1). Its square is integrable there exactly when
nu>1/2. Tail integrability is supplied by the exponential-power tail. Scale
and shape scores have integrable local powers/logarithms for nu>0; with
location known those coordinates permit DQM throughout nu>0. These direct
arguments classify the density family; the fitter's nu>=1 restriction and
its attained nu=1 face are a separate question for estimator asymptotics.

The direct Azzalini skew-normal parameterization is DQM for finite shape and
positive scale, but information is singular at alpha=0 when location is free:
the alpha score is `sqrt(2/pi)*z`, proportional to the location score `z/omega`.
Fixing location removes this particular dependence. Tests of alpha=0 with
unknown location need singular-model theory, not an inverse or pseudoinverse
of ordinary information. See Hallin and Ley, [The double sin of the
skew-normal](https://arxiv.org/html/1209.4177).

Audit other special members, including folded-normal symmetry, the normal
face of Bhattacharjee and two-piece/cusp families. Fixed support alone is
not a classification of their information rank or Hessian suitability.

## Discrete coordinates

Integer-valued unknown parameters do not have an open Euclidean local chart.
This affects discrete-uniform endpoints and hypergeometric population counts,
as well as estimated binomial size, discrete-Laplace location and finite-Zipf
cutoff. It is distinct from integer-valued observations: Poisson is regular.

Conditioning on known integer coordinates can leave regular submodels:
discrete-Laplace scale with known location, or Zipf exponent with known N>=2.
Check design-induced identification separately. For example, with binomial
size one, beta-binomial data identify only alpha/(alpha+beta), and
zero-inflated binomial data only (1-pi)*prob. A CMB law with size one cannot
identify its dispersion. Retained singleton supports can also remove all
information in truncated models. Interior probabilities do not fix rank loss.

## Remaining families

The CSV marks 69 families `not_reviewed`; this is not a non-DQM finding.
Complete their classifications by shared arguments where justified, with
separate rows for exceptional parameter values and held subsets. Prioritize
families with existing derivative candidates, then moving support, singular
members, discrete designs, heavy tails and numerically defined normalizers.
Do not infer a theorem from successful differentiation or a finite test grid.

## Scores and covariance contracts

For independent observations define each original-data log likelihood ell_i,
its score s_i, and, where appropriate,

$$H=-\sum_i\nabla^2\ell_i(\hat\theta),\qquad
  J=\sum_i s_i(\hat\theta)s_i(\hat\theta)^T.$$

Under the appropriate smooth M-estimation conditions the covariance estimate
is `H^{-1} J H^{-T}`. These are total sums, so there is no additional 1/n.
Equivalently, with A=H/n and B=J/n it is `A^{-1} B A^{-T}/n`. The total score's
outer product is not J; it is near zero at an interior optimum. Under correct
specification, a justified information estimate can supply model covariance.
Under misspecification, model DQM alone does not supply the needed moments,
identification, sensitivity or estimator expansion. See White, Theorems 3.2
and 3.3, [Maximum Likelihood Estimation of Misspecified
Models](https://faculty.utrgv.edu/diego.escobari/teaching/Econ8370/Papers/White%281982%29Econometrica-Corey.pdf).

The planned C++ interface should expose per-observation scores, total score,
OPG, observed Hessian where meaningful, model Fisher information where
available, and estimating-equation sensitivity where it differs. Each result
identifies parameter names/order, free subset, coordinates, normalization,
method and validity conditions. Allow accumulating J without retaining the
whole score matrix. R wrappers can expose covariance and sandwich integration
without making C++ depend on R or an R package.

Scores transform by the parameter Jacobian; arbitrary-point Hessians also
need the transformation's second-derivative terms. Undo objective scaling and
profile transformations correctly. An optimizer's BFGS inverse-Hessian
approximation is not automatically observed information. Do not manufacture
observation scores by feeding singleton samples to profiled costs without
checking their semantics. Report singularity, ill conditioning and missing
components rather than silently applying a pseudoinverse.

## Hypotheses and nuisance parameters

Catalog procedures for the target hypothesis, not just the fitted family:

| Target and setting | Procedure to evaluate | Current status |
|---|---|---|
| Smooth contrasts of a regular fit | Model Wald, score and profile likelihood procedures; delta method | No general inference API; mathematical and derivative checks required. |
| Smooth contrasts under suitable misspecification conditions | Sandwich Wald; correctly adjusted score procedures | Uniform score/OPG and sensitivity interfaces missing. Ordinary chi-square likelihood-ratio calibration is not automatically robust. |
| Normal mean or variance, exponential rate, known-size binomial probability | Exact pivots or exact test inversion | Theory available; dedicated procedures not implemented here. |
| Pareto shape with unknown lower endpoint | Exact pivot below | Available mathematically despite joint non-DQM; not implemented. |
| Uniform endpoints or spacing targets | Exact order-statistic procedures and nonregular limits | Evaluate target-specific methods; no joint regular Wald covariance. |
| Laplace location | Model Fisher inference, exact sign/order-statistic procedures; density-based median sandwich under misspecification | Historical Fisher candidate only; no general interface. |
| Triangular mode with known endpoints | Model score-information or carefully calibrated likelihood method; sensitivity including the kink for robustness | Mathematical candidate; not implemented or coverage-validated. |
| Skew-normal symmetry with unknown location | Singular-model symmetry procedures or calibrated simulation | Dedicated theory and implementation required. |
| Unknown integer parameters | Discrete likelihood/confidence sets or test inversion | Avoid continuous Wald defaults. |
| Generic fitted family | Existing parametric percentile bootstrap | Mechanically implemented in R; validity is not established for every family/regime. |

For Pareto density `a*b^a*x^(-a-1)` with a,b>0 and n>=2, let
`T=sum(log(X_i/min(X)))`. Log observations shifted by log(b) are independent
exponentials of rate a. The exponential spacings imply

$$2aT\sim\chi^2_{2(n-1)}.$$

This yields an exact model-based confidence interval and tests for shape with
unknown b. If b is known, use `sum(log(X_i/b))` and 2n degrees of freedom.
These are not misspecification-robust claims. Keep the distinction between a
known nuisance parameter and an unknown one estimated at a different rate.

The reproducible [illustrative checks](../notes/evaluations/2026-10-30-inference-examples.py)
integrate the Laplace and triangular scores independently of the package and
check the information calculations above. They also simulate the Pareto pivot:
20,000 samples of size 12, seed 20261030, gave 95.015% coverage for a nominal
95% interval, with Monte Carlo standard error 0.154 percentage points. The
[recorded results](../notes/evaluations/2026-10-30-inference-examples.json)
include dependency versions. These calculations illustrate the derivations;
they do not prove DQM or validate production inference.

For ordinary regular nuisance parameters, inference for a subset uses the
corresponding block of the full covariance or the appropriate efficient score.
Inverting only the target's Hessian block treats nuisance as known. Fixing
nuisance to its fitted value is not profiling it. Nonregular or unidentified
nuisance requires a separate result; regularity of the known-nuisance submodel
does not settle the original hypothesis.

## Bootstrap and maintenance findings

The current R bootstrap simulates from the fitted model and refits, preserving
held coefficients. It is not a misspecification-robust bootstrap. Boundary,
singular, discrete and mixed-rate cases need procedure-specific validity
arguments. Nonparametric bootstrap, subsampling or m-out-of-n methods are
possible alternatives only after their conditions and calibration are checked.
Failed refits currently stop replication; there is no procedure-specific
failure accounting. Silently discarding failures would change the procedure.

Source inspection also found that `confint.univariateML()` passes `Nreps` to
`bootstrapml()` through `...`, while the actual replication argument is `reps`.
The documented Nreps does not set the replication count on that path. Record
and repair this separately with a focused check; this audit changes no R code.

## Implementation and verification sequence

1. Preserve this source inventory and recover historical derivative candidates
   with their validation and semantics. Complete conditional DQM/rank entries
   and identify target hypotheses worth supporting first.
2. Start with a small regular set, such as normal, gamma, beta and Poisson.
   Implement per-observation scores and information in natural coordinates,
   including every supported free subset and nuisance cross term.
3. Validate derivatives against independent calculations, verify that observation
   scores sum to the total score, and check scaling and coordinate transforms.
   Use expected information identities only where their hypotheses hold.
4. Validate covariance and test calibration under the model, then selected
   misspecification alternatives with the required moments. Coverage and type-I
   error checks address inference; likelihood agreement alone does not.
5. Add nonsmooth and nonregular procedures selectively, beginning with cases
   having explicit theory, such as Laplace and the Pareto shape pivot.
6. Publish availability and gaps by regime and hypothesis. Distinguish
   implemented, historically recoverable, mathematically available but missing,
   unreviewed, and invalid for this procedure. Link every positive claim to
   assumptions, derivation/source, implementation and checks.

Resume this implementation after the current density and parameterization
work, alongside the documentation migration. A complete 98-family inference
audit and implementation remain work to do; do not advertise blanket robust
inference from this initial classification.
