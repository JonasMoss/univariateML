# Reverse dependencies and the 2.0 package name

Not now: before the 2.0 release (TASK-150.3), decide whether the breaking tree ships as univariateML 2.0 or as a new
package next to the CRAN 1.5.0 one. The deciding evidence is a reverse-dependency check, which has never been run on
this tree (`bindings/r/cran-comments.md` still describes the 1.x release).

## Inventory (CRAN, 2026-11-23, by source grep, not by running checks)

| Package | Field | What it calls |
|---|---|---|
| ale | Imports | `model_select(residuals)` with all models, `rml` |
| autogam | Imports | `model_select` |
| EBcoBART | Imports | `mlinvgamma` |
| kdensity | Imports | at its own build time (`R/starts.R`, top-level code): for each continuous family in `univariateML_metadata`, `estimator = mlX`, `density = eval(parse(text = meta$density))`, `support = meta$support$bounds` (the body-parsing path is a dead fallback for univariateML < 1.5) |
| peppwR | Imports | `model_select(x, models = short_name, na.rm = TRUE)`, `attr(fit, "model")`, `"logLik"` |
| piecenorms | Imports | `model_select(x, c("unif", "power", "norm", "lnorm", "weibull", "pareto", "exp"))` |
| RealSurvSim | Imports | `mlinvgamma`, `rml` |
| svines | Imports | `univariateML_metadata` (support `type` attribute "Z"/"R" to list families), `dml`/`pml`/`qml`, `mlnorm`, `mlstd`, `model_select`, `attr(, "logLik")`, `"model"` |
| bifrost | Suggests | `bootstrapml`, `model_select`, attributes `support`, `n`, `model`, `logLik`, `density` |
| rvinecopulib | Suggests | `univariateML_metadata[[paste0("ml", family)]]$support` and its `type` attribute, `dml`/`pml`/`qml`, `attr(, "model")` |

All exports they name still exist, and every family they name explicitly still exists. Likely breakage points:
kdensity **fails to install**: `meta$density` is now an internal unprefixed name (`core_dnorm`, `dkumar`, ...), which
passes its namespace filter and then fails to evaluate in kdensity's namespace (fix on either side: a callable
`density` in the metadata, or kdensity building the start density from `dml`; kdensity is Jonas's own package), the shape of `univariateML_metadata` (the `support` `type` attribute), attribute values such as
`attr(, "density")` that used to name extraDistr/actuar functions, warnings and errors that callers suppress or match
(ale suppresses `model_select` warnings), removed controls (`reltol`, `<param>0`) and the retired modifier fitters if a
caller lists all models.

## To run before deciding

Build the package from this tree and run `R CMD check` on the 10 packages against it and against CRAN 1.5.0
(`revdepcheck` or `tools::check_packages_in_dir(reverse = ...)`), on simbox (rvinecopulib and ale have heavy
dependency trees). Outcome: per package, unchanged / fixed by a compatibility shim / needs an upstream change.
If most break on deliberate contract changes, a new package name avoids forcing 10 maintainers to move at once.

## Results (2026-11-24, workstation, master 3fe10413 vs CRAN 1.5.0)

`R CMD check --no-manual` of the CRAN sources of all 10 packages, R 4.6.1, `_R_CHECK_FORCE_SUGGESTS_=false`
(ComplexHeatmap, a Bioconductor suggest, not installed), separate libraries for this tree and for 1.5.0.
Script `build/scratch/revdep.sh` (logs under `build/revdep/` of the validate worktree; rvinecopulib rerun after
installing wdm 0.3.0, which its LinkingTo requires).

| Package | 1.5.0 | This tree | Cause | Fix |
|---|---|---|---|---|
| ale, autogam, EBcoBART, peppwR, RealSurvSim | OK (ale 1 NOTE) | same | | none |
| kdensity | OK | install fails | `eval(parse(text = meta$density))` on `core_dbeta` | kdensity side (build from `dml`), or an evaluable `density` in the metadata |
| piecenorms | OK | 2 ERRORs | `sub(".*::d", "", attr(fit, "density"))` recovers the family; `"core_dunif"` is passed back to `model_select` | shim: a parseable `pkg::dX` label in `attr(, "density")`, or upstream use of `attr(, "model")`-independent ids |
| svines | OK | 3 ERRORs | `attr(meta$support, "type")` is NULL: support was an `intervals` object with a `type` slot, now a list with `$type` | shim: also set `attr(support, "type")` |
| rvinecopulib | OK | 1 ERROR (4 tests) | 2 tests: same support `type` attribute (Poisson reported as "c"); 2 tests: match the old non-finite-loglik message where Rayleigh with a zero observation now errors with its own reason | shim for the attribute; upstream test change for the message |
| bifrost | OK | 1 ERROR | test expects Cauchy to fit constant data; neither norm nor cauchy has an MLE there now | upstream test change (deliberate contract) |

Tally: 5 unchanged, 3 fixed or mostly fixed by two metadata shims (support `type` attribute, density label),
kdensity (Jonas's) needs a choice, 2 upstream test changes caused by deliberate contract changes (bifrost,
rvinecopulib's message match). The breakage is mild for the size of the change; it does not by itself argue for a
new package name.

### Rerun with the support `type` shim (2026-11-25, master 19b2fb7b)

Same setup, this tree only (`build/scratch/revdep-new.sh`, logs in `build/revdep/check-new2/`). Unchanged:
ale, autogam, EBcoBART, peppwR, RealSurvSim OK; kdensity install error; piecenorms 2 ERRORs (`core_dunif`); bifrost
1 ERROR (PR proposal TASK-247).

- **svines: tests now pass; one example fails.** With `family_set = "parametric"` svines offers every continuous
  family to `model_select`. On `returns[1:100, 1]` the new `mlgev` wins by AIC (logLik 293.42 against logistic
  292.39; 1.5.0 had no GEV), with $\xi=-0.403$, so the upper support end $0.0316$ lies just above the sample
  maximum $0.0282$. `svine_bootstrap_models()` then takes finite differences of `dml(log = TRUE)` with absolute
  steps of $10^{-3}$ in every coefficient (7% of $\sigma$), twice nested for the Hessian. Perturbed fits put the
  sample maximum outside the support, the Hessian gets $-\infty$ entries, the one-step update is NaN, and the
  bootstrap margins carry NaN coefficients (with the original `certified`/`unique` attributes, copied by
  `with_parameters_mrg`). The NaN then fails in our support expression. Not a univariateML defect: the same
  happens for any selected family whose support depends on its coefficients. Upstream options: relative steps,
  dropping families with coefficient-dependent support from the bootstrap, or a non-finite check. A PR proposal
  would be svines-side; nothing is drafted.
- **rvinecopulib: 3 failures left**, all known: the two Rayleigh message tests (PR proposal TASK-248) and the
  `binom` adapter calling `mlbinom(x)` without `size` (question for Jonas in TASK-248).
