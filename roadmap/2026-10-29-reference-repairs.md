# Larger reference repairs (held back, needs a decision)

**Status:** proposed 2026-10-29; not merged. The patches are in
`roadmap/patches/`.

The ML-only policy allows small runtime-checked workarounds in
`univariateML_reference_fixes` (`univariateML/R/code_helpers.R`), built from
the reference package's own functions: `sads::qzipf` (inverts
`cumsum(sads::dzipf)`) and `extraDistr::ptriang` (sets the endpoint value).
Three overnight jobs wrote much larger repairs. They were left out because
they reimplement d/p/q/r numerically rather than patch a single value:

| Patch | Functions | Size | Approach | Defects (see `KNOWN_ISSUES.md`) |
|---|---|---|---|---|
| `patches/2026-10-29-trgamma-reference-repairs.patch` | actuar `d/p/q/r` `trgamma` and `invtrgamma` (8 entries) | 164 lines | compose `stats::dgamma/pgamma/qgamma/rgamma` with the power transform; log-CDF anchor below `exp(-700)` | density cancellation at huge `shape1`, `Inf`/0 quantiles, CDF 0/1 at tiny `shape1`, non-finite draws |
| `patches/2026-10-29-tnorm-reference-repairs.patch` | extraDistr `d/p/q` `tnorm` (3 entries) | 103 lines | adaptive integration of the untruncated normal relative to the interval mode | `Inf`/`NaN` density and `Inf` quantile when the interval lies far in a tail |
| (dropped, not saved) | extraDistr `q/p/r` `gev` | 83 lines | Gumbel transform with `expm1`; analytic CDF | `qgev(0.5, xi = 1e-16) = 0`, `qgev(1, xi = -0.5) = Inf`, upper-tail log CDF `-Inf` |

The tnorm repair keys on `extraDistr::dtnorm`, which `mlhnorm` also uses, so it
would change `mlhnorm`'s `dml`/`pml`/`qml` too.

## Options

1. Keep them out (current state): `dml`/`pml`/`qml` can return `Inf`/`NaN`/0
   for fits whose parameters reach the defect regions; `logLik` and fitting
   are unaffected. Report upstream.
2. Merge them as runtime-checked repairs (they switch off when upstream
   fixes the defect). Effectively package-defined evaluation for these
   models, which the ML-only policy was meant to avoid.
3. Merge only the cases fits reach in ordinary use (tnorm deep-tail
   truncation is reachable near the exponential escape; the trgamma cases
   need extreme shapes, near the lognormal and power faces).

## Trigger to revisit

The user's decision on the policy; an upstream fix in actuar or extraDistr.
