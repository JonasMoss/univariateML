# Plan

## Immediate focus
- Keep current R implementation working and CRAN-clean while we build generators.
- Define machine-readable metadata and docs sources as the single truth for all estimators.

## Artifacts to introduce (near term)
- `metadata/models.json` (or YAML): per-model fields: name/id, params (names, order, defaults), data support (interval bounds + closed flags), density identifier, sampler for tests, start-value hints, per-model validation hooks (e.g., Lomax moment condition), optional optimization options.
- `metadata/docs.json`: per-model text blocks: title, description, details, references, examples (with seeds), seealso, aliases. Shared snippets allowed.
- Justfile targets for generation: `gen-r` (wrappers, roxygen), `gen-bindings` (Rcpp glue stubs), `regen-all` (end-to-end), `check`.

## Pipeline sketch
1) Parser reads metadata+docs → emits:
   - R wrappers `ml<dist>()` as thin decorators around `.Call` bindings.
   - R metadata registration (`univariateML_metadata`) and `zzz` registrar (no filesystem scanning).
   - Roxygen blocks from docs.json.
   - C++ binding stubs that call core C++ APIs (to be added), returning estimates/logLik/iters.
2) Core C++ stays Armadillo-only (no Rcpp types). RcppArmadillo layer only in generated bindings.
3) Tests: generate smoke tests per model (simulate with defaults, fit, recompute logLik via R density), plus A/B checks against legacy R implementations while porting.

## Steps
1) Finish stabilizing current R package (done: tests passing, CRAN notes resolved except env/compiler notes).
2) Draft metadata schema and docs schema; add example entries for 1–2 models (e.g., gumbel, weibull).
3) Write generator scripts + Justfile stubs to produce R wrappers/metadata/zzz and binding skeletons (no core C++ change yet).
4) Implement core C++ for one model using Armadillo-only API; wire generated binding/wrapper; validate vs existing tests.
5) Incrementally port remaining models, removing hand-written R as each is migrated; expand metadata/docs entries.
6) Add minimal GitHub Actions later (R CMD check + codegen sanity).

## Notes
- Maintain portable build flags and optional OpenMP guarded in C++; keep CRAN-safe defaults.
- Plan for standalone C++ by keeping core under `include/univariate_ml/` and isolating Rcpp to bindings.
