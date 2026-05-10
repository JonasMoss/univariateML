# metadata schema (per-distribution form)

Schema for the canonical per-distribution metadata: a `metadata.hpp`
header in the C++ dossier paired with an `<id>.json` sidecar holding
R-side refs. Both forms are language-agnostic; the R-side codegen in
`scripts/codegen_metadata.R` and `scripts/codegen_wrappers.R` is the
only consumer today, but the same JSONs are intended to drive a
future Python shell.

## `metadata.hpp` (canonical, C++ side)

A distribution lives at
`cppunivariate/include/cppunivariate/distributions/<id>/`. Its
canonical metadata is a `metadata.hpp` header of `constexpr`
constants.

Required structure (the regex parser in
`univariateML/scripts/parse_cpp_metadata.R` keys on this layout —
deviations are parse errors):

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

## `univariateML/metadata/<id>.json` (R-side refs)

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

## Cross-shell expectations

- `id` is the cross-shell key. Goldens, wrappers, and (future)
  Python bindings all use it. The folder name **is** the id.
- Numeric fields are JSON numbers; consumers parse them as
  double-precision floats.
- `kInf` / `-kInf` in `metadata.hpp` are the canonical "open at
  infinity" sentinels.
- Schema changes require:
  - bumping the parser in `scripts/parse_cpp_metadata.R` and
    consumers in `scripts/codegen_metadata.R` /
    `scripts/codegen_wrappers.R`,
  - regenerating goldens via `scripts/goldens.R`,
  - updating this file.

## Codegen rules

- Two env vars are honoured: `UMVL_DOSSIER_DIR` (per-distribution
  glob root in the cppunivariate include tree) and
  `UMVL_R_METADATA_DIR` (per-distribution refs root, default
  `univariateML/metadata`).
- The R-side codegen scripts read every dossier `metadata.hpp`,
  pair it with its `univariateML/metadata/<id>.json` sidecar, sort
  alphabetically by `id`, and emit `univariateML/R/ml00_metadata.R`
  and `univariateML/R/ml02_wrappers.R` deterministically.
