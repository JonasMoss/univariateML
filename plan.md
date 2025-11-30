# Plan: reorganize R sources and generation

## Goal
- Keep `R/` for generated artifacts only; move handwritten R code to `r-src/` (structured subdirs).
- Run codegen via Justfile (or scripts) to produce `R/` outputs; keep generators out of `R/`.

## Target layout and naming
- `r-src/ml/`: estimator implementations (`ml*_` functions, no docs/metadata).
- `r-src/code/`: helpers/constructors/checks and utilities (e.g., `helpers.R`, `utility.R`, `functions.R`, `generics.R`, `model_select.R`, `probability_plots.R`, `bootstrap.R`, `densities.R`, `misc.R`).
- `r-src/data/`: data docs (`abalone.R`, `corbet.R`, `egypt.R`).
- `r-src/pkg files`: package scaffolding kept at `r-src/` root (`univariateML-package.R`, `zzz.R`) without a nested pkg folder.
- `r-src/generators/`: generator scripts (`gen_wrappers.R`, `gen_metadata.R`, `gen_estimators_from_rsrc.R`, `gen_utils_copy.R`).
- `metadata/`: JSON for models/docs.
- `R/` (generated only):
  - `ml00_metadata.R` (from models.json)
  - `ml01_estimators.R` (concatenated `_` implementations)
  - `ml02_wrappers.R` (wrappers with roxygen)
  - Copies from `r-src/code` prefixed with `code_` (e.g., `code_helpers.R`, `code_model_select.R`, `code_probability_plots.R`, etc.) produced by a copy/rename generator.
  - Data docs copied from `r-src/data` as-is (`data_abalone.R`, `data_corbet.R`, `data_egypt.R`).
  - `pkg_univariateML-package.R` and `pkg_zzz.R` copied from `r-src/` root (load after metadata/estimators/wrappers).

## Steps
1) Move handwritten R helpers from `R/` to `r-src/code/` and data/pkgs to `r-src/data/` and `r-src/` root; adjust generators to read from `r-src` only.
2) Rename generator outputs to `R/ml00_metadata.R`, `R/ml01_estimators.R`, `R/ml02_wrappers.R`; update generator scripts accordingly.
3) Add/update util copy/rename generator to emit `code_*` files from `r-src/code`, data docs from `r-src/data`, and `pkg_*` files from `r-src/` root, preserving load order (metadata → estimators → wrappers → pkg/zzz).
4) Relocate generator scripts into `r-src/generators/` (or keep in `scripts/` but ignored by build); update paths and Justfile targets (`regen`, `regen-wrappers`, `regen-metadata`, `regen-estimators`, `regen-util`).
5) Update `.Rbuildignore` to exclude `r-src/` and generator scripts; ensure `R/` contains only generated files plus the copied util-prefixed files.
5) Regenerate outputs and run tests to confirm.

## Notes
- Preserve current functionality/tests; keep `generated_models`/metadata flow intact.
- Ensure load order: metadata → estimators → wrappers → util pkg/zzz.
