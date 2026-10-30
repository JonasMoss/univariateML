# Distribution dossiers — Closed (2026-05-09)

Landed. Every distribution now lives in a dossier bundle at
`univariateML-cpp/include/cppunivariate/distributions/<id>/` containing
`mle.hpp`, `dpqr.hpp`, `metadata.hpp`, and `dossier.md`. The legacy
flat-file layout (`estimators/<id>.hpp` + `distributions/<id>.hpp`)
is gone.

## Landing commits

- `f636f91` — Dossier Phase 2: Lomax pilot.
- `e5e33a1` — Dossier Phase 3: Exponential pilot (simple distribution).
- `8d3ecb8` — Phase B: Lomax to `metadata.hpp` + R-side `metadata/<id>.json` sidecar.
- `a6a4bf7` — Phase C: Exp restructure + add cppunivariate dpqr.
- `0d9e480` — Sweep: migrate the remaining 38 distributions.

## What this enabled

- Codegen reads per-distribution `metadata.hpp` (parsed via
  `bindings/r/scripts/parse_cpp_metadata.R`) merged with R-side
  refs in `bindings/r/metadata/<id>.json`.
- Each distribution is locally understandable: math, implementation,
  and prose docs sit in one folder.
- `dossier.md` files are stubs today — filling them out is part of
  the [documentation-from-metadata](2026-05-08-documentation-from-metadata.md)
  follow-on.

## What's still open

- The dossier *format* exists; the prose content of `dossier.md`
  files is largely empty. That's a separate roadmap item, not this one.
