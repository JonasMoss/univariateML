# R package scope: ML-only — Closed (2026-05-09)

Landed. The R package now exposes ML wrappers only; d/p/q/r is
delegated to upstream packages (`extraDistr`, `actuar`, `fGarch`,
`logitnorm`, `nakagami`, `sads`) via `Imports:`. The C++ artifact
(`univariateML-cpp/`) keeps its full dpqr surface for downstream
consumers.

## Landing commits

- `c8c93ca` — Expand `Imports:` for canonical density packages.
- `35a61b2` — Route all dpqr through external packages (metadata refs).
- `a5b13b3` — Drop per-distribution dpqr surface from R package.
- `2a86575` — Canonical-names reference; align `ml_exports` `List`
  names with the chosen upstream package conventions.
- `3863d89` — Revert `invgamma` to historic extraDistr `(alpha, beta)`
  parameterization.

## What changed

- `bindings/r/metadata/<id>.json` sidecars point `density` /
  `sampler` at upstream functions (e.g. `extraDistr::dlomax`,
  `actuar::rburr`) where applicable.
- `bindings/r/DESCRIPTION` `Imports:` lists the upstream dpqr
  packages.
- The R package's NAMESPACE no longer re-exports per-distribution
  `d*` / `p*` / `q*` / `r*` functions.

## What's still open

- A user-facing audit of generated examples (do roxygen examples
  reference the upstream functions cleanly?). If they do, this entry
  stays closed; if not, that's a small follow-up but not its own
  roadmap item.
