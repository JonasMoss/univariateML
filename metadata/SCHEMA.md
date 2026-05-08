# metadata schema

Schemas for `models.json` and `docs.json`. Both are language-agnostic;
the R-side codegen in `scripts/codegen_metadata.R` and
`scripts/codegen_wrappers.R` is the only consumer today, but the same
JSONs are intended to drive a future Python shell.

## models.json

Top-level shape: `{ "models": [ <Model>, ... ] }`. Each `<Model>`:

| field              | type                  | required | consumed by codegen | notes |
|--------------------|-----------------------|----------|---------------------|-------|
| `id`               | string                | yes      | yes                 | wrapper name = `ml<id>`; matches `tests/golden/<id>.json`. |
| `name`             | string                | yes      | yes                 | display name in generated docs. |
| `density`          | string                | yes      | yes                 | callable resolved by `R/code_helpers.R`; bare name or `pkg::fn`. |
| `support`          | `<Support>`           | yes      | yes                 | distribution support. See below. |
| `params`           | array of `<Param>`    | yes      | yes                 | parameter list, in canonical order. |
| `sampler`          | string                | yes      | no (used by goldens)| RNG callable; `pkg::fn` or bare. |
| `start_hints`      | object                | no       | no                  | `{ method, notes }`; informational. |
| `validation_hooks` | array                 | no       | no                  | reserved for future quality work. |
| `legacy_impl`      | string                | no       | no                  | path to the original R-side implementation. |

`<Support>`:

| field    | type                | required | notes |
|----------|---------------------|----------|-------|
| `type`   | `"R"` \| `"Z"`      | yes      | continuous or integer support. |
| `lower`  | number \| `null`    | yes      | `null` ⇒ `-Inf` (or `Inf` for `upper`). |
| `upper`  | number \| `null`    | yes      | as above. |
| `closed` | `[bool, bool]`      | yes      | `[lower-closed, upper-closed]`. |

`<Param>`:

| field     | type                | required | notes |
|-----------|---------------------|----------|-------|
| `name`    | string              | yes      | parameter name as it appears in the fitted object's `coef()`. |
| `default` | number              | yes      | default value used by `simulate_default()` and goldens. |
| `role`    | string \| `null`    | yes      | informational; e.g. `"location"`, `"scale"`. |
| `support` | `<Support>`         | yes      | per-parameter support; same shape as the model `support`. |

## docs.json

Top-level shape: `{ "models": [ <Doc>, ... ] }`. Sparse overlay — entries
present here override the codegen fallbacks; missing IDs get
auto-generated docs from the model record. Each `<Doc>`:

| field         | type            | required | notes |
|---------------|-----------------|----------|-------|
| `id`          | string          | yes      | matches a `<Model>.id`. |
| `title`       | string          | no       | overrides default title. |
| `description` | string          | no       | overrides default `@description`. |
| `details`     | string          | no       | overrides default `@details`. |
| `seealso`     | array<string>   | no       | `@seealso` lines. |
| `references`  | array<string>   | no       | `@references` lines. |
| `examples`    | array<string>   | no       | `@examples` lines (one per element). |

## Cross-shell expectations

- `id` is the cross-shell key. Goldens, wrappers, and (future) Python
  bindings all use it.
- Numeric fields are JSON numbers; consumers parse them as
  double-precision floats.
- `null` for support bounds is the canonical "open at infinity" sentinel.
- Schema changes require:
  - bumping consumers in `scripts/codegen_metadata.R` and
    `scripts/codegen_wrappers.R`,
  - regenerating goldens via `scripts/goldens.R`,
  - updating this file.
