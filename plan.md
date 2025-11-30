# Plan: reorganize R sources and generation

## Goal
- Keep `R/` for generated artifacts only; move handwritten R code to `r-src/` (structured subdirs).
- Run codegen via Justfile (or scripts) to produce `R/` outputs; keep generators out of `R/`.

## Target layout and naming
- `r-src/ml/`: estimator implementations (`ml*_` functions, no docs/metadata).
- `r-src/util/`: shared helpers (rename sensibly: `helpers.R`, `constructors.R`, `checks.R`, etc.; avoid `util_utility.R`).
- `r-src/generators/`: generator scripts (`gen_wrappers.R`, `gen_metadata.R`, `gen_estimators_from_rsrc.R`).
- `metadata/`: JSON for models/docs.
- `R/` (generated only):
  - `ml_metadata.R` (from models.json)
  - `ml_estimators.R` (concatenated `_` implementations)
  - `ml_wrappers.R` (wrappers with roxygen)
  - existing `zzz.R` (decorator registration) expects generated_models from metadata.

## Steps
1) Move handwritten R helpers from `R/` to `r-src/util/`; adjust generators to read from `r-src` only.
2) Rename generator outputs to `R/ml_metadata.R`, `R/ml_estimators.R`, `R/ml_wrappers.R`; update generator scripts accordingly.
3) Relocate generator scripts into `r-src/generators/` (or keep in `scripts/` but ignored by build); update paths and Justfile targets (`regen`, `regen-wrappers`, `regen-metadata`, `regen-estimators`).
4) Update `.Rbuildignore` to exclude `r-src/` and generator scripts; ensure `R/` contains only generated files plus `zzz.R`.
5) Regenerate outputs and run tests to confirm.

## Notes
- Preserve current functionality/tests; keep `generated_models`/metadata flow intact.
- Ensure load order: metadata → estimators → wrappers → `zzz.R`.
