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
| kdensity | Imports | `univariateML_models`, `univariateML_metadata`, the `ml*` functions; parses the **function body** of `ml*` for the `attr(object, "density")` / `"support"` assignments (`R/starts.R:90`) |
| peppwR | Imports | `model_select(x, models = short_name, na.rm = TRUE)`, `attr(fit, "model")`, `"logLik"` |
| piecenorms | Imports | `model_select(x, c("unif", "power", "norm", "lnorm", "weibull", "pareto", "exp"))` |
| RealSurvSim | Imports | `mlinvgamma`, `rml` |
| svines | Imports | `univariateML_metadata` (support `type` attribute "Z"/"R" to list families), `dml`/`pml`/`qml`, `mlnorm`, `mlstd`, `model_select`, `attr(, "logLik")`, `"model"` |
| bifrost | Suggests | `bootstrapml`, `model_select`, attributes `support`, `n`, `model`, `logLik`, `density` |
| rvinecopulib | Suggests | `univariateML_metadata[[paste0("ml", family)]]$support` and its `type` attribute, `dml`/`pml`/`qml`, `attr(, "model")` |

All exports they name still exist, and every family they name explicitly still exists. Likely breakage points:
kdensity's body parsing (generated wrappers no longer assign the density attribute literally; kdensity is Jonas's own
package), the shape of `univariateML_metadata` (the `support` `type` attribute), attribute values such as
`attr(, "density")` that used to name extraDistr/actuar functions, warnings and errors that callers suppress or match
(ale suppresses `model_select` warnings), removed controls (`reltol`, `<param>0`) and the retired modifier fitters if a
caller lists all models.

## To run before deciding

Build the package from this tree and run `R CMD check` on the 10 packages against it and against CRAN 1.5.0
(`revdepcheck` or `tools::check_packages_in_dir(reverse = ...)`), on simbox (rvinecopulib and ale have heavy
dependency trees). Outcome: per package, unchanged / fixed by a compatibility shim / needs an upstream change.
If most break on deliberate contract changes, a new package name avoids forcing 10 maintainers to move at once.
