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

## Per-distribution form (C++ constexpr)

A distribution that has been migrated to a dossier lives at
`cppunivariate/include/cppunivariate/distributions/<id>/`. Its
canonical metadata is a `metadata.hpp` header of `constexpr`
constants; R-side refs (sampler, density) live in
`univariateML/metadata/<id>.json`.

### `metadata.hpp` (canonical, C++ side)

Required structure (the regex parser in
`univariateML/scripts/parse_cpp_metadata.R` keys on this layout — deviations
are parse errors):

```cpp
#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::<id> {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "<id>";        // matches the folder name
constexpr const char *name = "<Name>";      // display name

constexpr SupportDescriptor support{<TYPE>, <LOWER>, <UPPER>,
                                    <LOWER_CLOSED>, <UPPER_CLOSED>};

constexpr ParamDescriptor params[] = {
  {"<param>", "<role>", <DEFAULT>, <ESTIMABLE>,
    {<TYPE>, <LOWER>, <UPPER>, <LOWER_CLOSED>, <UPPER_CLOSED>}},
  // one row per parameter
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::<id>
```

Field rules:

- `<TYPE>` is `SupportType::Real` or `SupportType::Integer`.
- `<LOWER>` / `<UPPER>` are double literals; use `kInf` /  `-kInf`
  for infinity (the parser recognises these aliases).
- `<LOWER_CLOSED>` / `<UPPER_CLOSED>` are `true` / `false`.
- `<DEFAULT>` is a numeric literal (`1.0`, `kInf`, `-2.5`).
- `<ESTIMABLE>` is `true` (today the parser reads it but the field
  isn't yet consumed — it's the metadata seam for the future
  variable-parameter-selection feature in the roadmap).
- `<role>` is free-form prose (`"rate"`, `"shape"`, `"location"`,
  `"scale"`, …).

Each `ParamDescriptor` row must fit on a single logical line so
the regex matches the entire entry — extending the format requires
extending the parser too.

### `univariateML/metadata/<id>.json` (R-side refs)

```json
{
  "sampler": "<R-callable>",
  "density": "<R-callable>"
}
```

`<R-callable>` is either `pkg::fn` (base R or another package, e.g.
`stats::rexp`) or a bare name (a function exported by univariateML
itself via the Rcpp layer, e.g. `rlomax`). The schema can grow with
additional R-only fields (e.g. doc seealso, examples) when the
human-docs pass happens.

### Codegen rules

- A given `id` must appear in **exactly one** source: legacy
  `metadata/models.json`, an intermediate per-distribution
  `metadata.json`, or a canonical per-distribution `metadata.hpp`.
  Duplicate ids are a hard error.
- The R-side codegen scripts merge all three sources, sort
  alphabetically by `id`, and emit `univariateML/R/ml00_metadata.R` and
  `univariateML/R/ml02_wrappers.R` deterministically.
- Three env vars are honoured: `UMVL_METADATA_DIR` (legacy
  aggregate root), `UMVL_DOSSIER_DIR` (per-distribution glob root
  in the cppunivariate include tree), and `UMVL_R_METADATA_DIR`
  (per-distribution refs root, default `univariateML/metadata`).
