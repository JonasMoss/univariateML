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

## Per-distribution form (dossier-migrated distributions)

A distribution that has been migrated to a dossier lives at
`cppunivariate/include/cppunivariate/distributions/<id>/` and stores
its metadata in a sibling `metadata.json`. The file's structure is
the same as a single `<Model>` entry in `models.json`, with the
following differences:

- `id` is **omitted** — the folder name supplies it.
- per-`<Param>` `role` and `start_hints` are populated (no longer
  `null` placeholders).
- per-`<Param>` gets a new optional field `estimable` (boolean,
  defaults to `true` if absent). The field is read by codegen but
  not yet consumed; it's the metadata seam for a future
  variable-parameter-selection feature (see roadmap).
- Two new top-level keys:

| field            | type            | required | notes |
|------------------|-----------------|----------|-------|
| `docs`           | object          | no       | overlay that replaces what would have been a `<Doc>` entry in `docs.json`. Same shape as `<Doc>` but with `id` omitted. |
| `implementation` | object          | no       | `{ algorithm, optimizer, tol_default, maxit_default }` — short prose summary of the active algorithm. Light replacement for the formal-provenance roadmap entry. |
| `quality_status` | string          | no       | placeholder for the future quality ladder (`experimental` / `ported` / `validated` / `audited` / `stable`). Pilot dossiers set `ported`. |

Codegen rules:

- A given `id` must appear in **exactly one** of `metadata/models.json`
  or a per-distribution `metadata.json`. Duplicate ids are a hard
  error.
- The R-side codegen scripts merge both sources, sort alphabetically
  by `id`, and emit `r/R/ml00_metadata.R` and `r/R/ml02_wrappers.R`
  deterministically.
- Both env vars `UMVL_METADATA_DIR` (legacy aggregate) and
  `UMVL_DOSSIER_DIR` (per-distribution glob root) are honoured by
  every consumer of the metadata.
