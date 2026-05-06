# Math backend: tradeoffs and the Rmath choice

The mlcore estimators need a handful of special functions:

- `digamma(x)`, `trigamma(x)`, `tetragamma(x)`, `psigamma(x, n)` — polygamma family.
- `lgamma(x)`, `lbeta(a, b)` — log-gamma and log-beta. (Currently the `lgamma` we use is `std::lgamma`; only `lbeta` is in `special.hpp`.)
- `dnorm`, `dpois`, `dgeom` — densities, used in a couple of likelihoods.

Today these all flow through `src/ml/special/special.hpp`, which is the single
boundary between mlcore and the math backend. The `dnorm`/`dpois`/`dgeom`
densities are inlined; only `Rf_psigamma` is currently delegated to Rmath.

Swapping backend means rewriting that one file. Nothing else in mlcore
should know which backend is in use.

## Why Rmath today

- Already linked into the package via `LinkingTo: Rcpp` (transitively
  pulls libR which has the symbols).
- Identical to what the R-side test suite exercises, so goldens stay
  bit-for-bit reproducible across the boundary.
- Polygamma in particular is fiddly; relying on R's well-tested
  implementation removes a class of accuracy disputes from the port.

## What Rmath costs

- Couples the C++ tests to a working R install. The standalone-build
  goal in WS3 has to go through Rmath standalone or replace these calls
  outright before `cmake --build && ctest` can run on a non-R container.
- The polygamma symbol resolution is fragile: Rmath.h aliases `digamma`
  → `Rf_digamma` via `#define`, polluting the global namespace. This
  bit us during WS1 (struct fields named `beta`/`gamma` got macro-mangled);
  the workaround is the `#undef` block at the top of `special.hpp`.

## Alternatives considered

### Boost.Math

- **Pro**: Header-only, MIT-style license, well-tested, comprehensive
  set of special functions including all the polygammas, Bessels, etc.
- **Pro**: No global-namespace macros — uses `boost::math::digamma`.
- **Pro**: Same algorithm family as Rmath for digamma/trigamma; ULP-level
  agreement is plausible (worth measuring).
- **Con**: Adds ~30 MB of headers as a build dependency. CRAN-acceptable
  via `BH` package which exposes Boost headers, but pulls in a lot.
- **Con**: Goldens would shift if Boost differs from Rmath even by a
  ULP; refit needed.

### Cephes (vendored)

- **Pro**: Public-domain C source, ~3k LoC for the full polygamma family
  + helpers. Easy to drop into `src/ml/special/cephes/` without
  upstream coordination.
- **Pro**: Zero build-system surface area; just compile the .c files.
- **Con**: Older code; numerical accuracy claims are weaker than
  Rmath/Boost. Likely larger ULP drift, especially at boundaries.
- **Con**: Maintenance falls on us — no upstream to track.

### Hand-rolled

- **Pro**: Total control. Inline what we need, leave the rest. Lowest
  build complexity.
- **Pro**: `dpsifn` is already vendored at `src/ml/special/polygamma.cpp`
  (port of R's `polygamma.c`). The shell of an in-tree polygamma exists
  but isn't wired into the build today.
- **Con**: Owning numerical code is real work — testing, edge cases,
  asymptotic regions. Easy to introduce subtle drift.

## Decision (current)

Stay on Rmath. Document the boundary at `special.hpp`. Revisit when:

1. WS3's standalone build needs `cmake --build && ctest` to pass without
   an R install, AND
2. The minimal vendoring path (compile `polygamma.cpp` + a shim with
   `Rf_d1mach`/`Rf_i1mach` inlined) proves more painful than just
   swapping in Boost.Math or Cephes.

If swapping does happen, the migration is a single-file change in
`special.hpp`. Goldens will need one regen + visual diff to confirm
nothing else moved.

## Rough effort estimates (for the swap)

| Backend     | Files changed | Build deps        | Golden risk     |
|-------------|---------------|-------------------|-----------------|
| Rmath (now) | —             | libR (via LinkTo) | none (baseline) |
| Boost.Math  | special.hpp   | BH                | ULP-level       |
| Cephes      | special.hpp + ~3k LoC vendored | none | ULP-to-1e-12    |
| Hand-roll   | special.hpp + polygamma.cpp wired up | none | depends on porting fidelity |
