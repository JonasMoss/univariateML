# svines one-step bootstrap with coefficient-dependent support

TASK-260 proposes coefficient-scaled central differences, bounded retries, and finite-value checks for the parametric bootstrap. The documented example now passes against both univariateML builds. The patch targets svines `main` at `eb57cd592fa9844249d8b29b6e1916fb06e70801`. The CRAN DESCRIPTION names Thomas Nagler as maintainer and identifies the upstream repository. [svines repository](https://github.com/tnagler/svines)

[Patch](2026-11-25-revdep-pr-svines/svines.patch) · [PR draft](2026-11-25-revdep-pr-svines/pr.md) · [Test results and reproduction](2026-11-25-revdep-pr-svines/results.md)

## Reproduced failure

The example requests a parametric copula and uses svines' default continuous margin panel. On the upcoming univariateML build, the first margin selects GEV with log likelihood 293.4181 and AIC −580.8361. The logistic fit has log likelihood 292.3930 and AIC −580.7860. CRAN 1.5.0 selects logistic for both margins.

The fitted GEV has shape −0.4032494, scale 0.0138846, and upper endpoint 0.0316499. The sample maximum is 0.0282155. The original nested Hessian stencil evaluates the log density with scale reduced by 0.002, which puts the maximum outside the support and gives a log density of `-Inf`. The original marginal and copula scores were finite in this replay. The assembled Hessian had three non-finite entries, and all nine coefficients in each of the ten bootstrap replicates were non-finite. The example stopped when simulation evaluated the GEV support using those coefficients.

## Proposal and alternatives

The patch starts from a step of 0.001 and scales it down in proportion to any nonzero coefficient with magnitude below one. It tries up to ten stencils, halving the steps after an evaluation error or non-finite result. Within a marginal Hessian attempt, the inner steps are fixed on both sides of each outer difference. The mixed block checks the log density before taking CDF differences because a CDF can return a finite zero or one outside the support. If the retries fail, svines reports which margin failed. The bootstrap also checks scores, the assembled Hessian, and each update before constructing a replicate.

| Alternative | Consequence |
|---|---|
| Scale the steps alone | Handles the size mismatch in the example. An observation sufficiently close to a moving endpoint can still fall outside a scaled stencil, as the new near-endpoint fixture demonstrates. |
| Fail on the first non-finite value | Gives a clearer error and ends the documented example before simulation. Retrying permits a centered difference when smaller perturbations are feasible. |
| Exclude margins with coefficient-dependent support | Removes these fits from the one-step bootstrap and requires a cross-version detection rule. CRAN 1.5.0 supplies an `Intervals` support object with a type marker, bounds, and closure. The upcoming build supplies a list with `support$expressions`, including GEV's coefficient-dependent endpoints. The proposal uses density and CDF evaluations directly and retains the full margin panel. |

## Validation and open question

The installed svines suite passed on the workstation against both builds after the patch. The GEV regression ran on the upcoming build, including a test that forces the initial stencil outside the support, and was skipped on CRAN 1.5.0 where the fitter is unavailable. A small-scale normal regression checked analytic scores and Hessian values on both builds. The help example passed with each patched build, and 30 seeds per build gave finite coefficients and simulated quantiles. The patch passed forward and reverse application checks.

**Open.** A finite one-step update can still leave a family's parameter domain. The seeded runs establish that the example completes with finite results. A general admissibility rule would need coefficient-domain information or a reparameterization, plus separate validation of bootstrap inference near parameter-dependent boundaries.
