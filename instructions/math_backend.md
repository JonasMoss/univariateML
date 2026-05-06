# Math backend: tradeoffs and the Rmath choice

The mlcore estimators need a handful of special functions:

- `digamma(x)`, `trigamma(x)`, `tetragamma(x)`, `psigamma(x, n)` — polygamma family.
- `lgamma(x)`, `lbeta(a, b)` — log-gamma and log-beta. (Currently the `lgamma` we use is `std::lgamma`; only `lbeta` is in `special.hpp`.)
- `dnorm`, `dpois`, `dgeom` — densities, used in a couple of likelihoods.

Today these all flow through `src/ml/special/special.hpp`, which is the single
boundary between mlcore and the math backend. `dnorm`/`dpois`/`dgeom`/`lgamma`/`lbeta`
are inlined on `<cmath>`; the polygamma family routes through `src/ml/special/polygamma.cpp`
(in-tree port of R's `polygamma.c`, see "Decision (current)" below).

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

### Hand-rolled (chosen — see "Decision" below)

- **Pro**: Total control. Inline what we need, leave the rest. Lowest
  build complexity.
- **Pro**: `dpsifn` is already vendored at `src/ml/special/polygamma.cpp`
  (port of R's `polygamma.c`); now wired into the build.
- **Con**: Owning numerical code is real work — testing, edge cases,
  asymptotic regions. Easy to introduce subtle drift.

## Decision (current)

**Vendored hand-roll via `dpsifn`.** mlcore::special::{digamma, trigamma,
tetragamma, psigamma} now delegate to `src/ml/special/polygamma.cpp`, a
literal port of R's `polygamma.c` (Bell Labs / Amos algorithm 610), backed
by tiny libm-only shims in `src/ml/special/rmath_compat.hpp` for the four
helpers it consults (`Rf_d1mach`, `Rf_i1mach`, plus `imin2`/`fmax2`/`fmin2`/
`R_pow_di`).

Concrete consequences:

- The standalone test harness (`tests/cpp/`) no longer links libR, no longer
  needs `find_program(R)`, and `mlcore_tests` shows zero R-related deps in
  `ldd` output.
- The R-package build is unchanged — `LinkingTo: Rcpp,RcppArmadillo` still
  pulls Rmath transitively, and the dpqr cpps in `src/distributions/` keep
  using `Rf_dt`/`Rf_pgamma`/etc. directly.
- Goldens did not move when swapping in `dpsifn`: it's the same algorithm
  R uses internally for `psigamma`, so digamma/trigamma agree at ULP level.

Revisit if: a numerical issue surfaces in the polygamma path that R's
upstream has fixed since the port (sync against `instructions/nmath/polygamma.c`),
or if a future user wants to drop the polygamma dependency entirely (Boost.Math
is still on the table for that).

## Rough effort estimates (for a future swap)

| Backend           | Files changed                                | Build deps        | Golden risk      |
|-------------------|----------------------------------------------|-------------------|------------------|
| Rmath (legacy)    | revert special.hpp                           | libR (via LinkTo) | baseline         |
| Vendored dpsifn   | — (current state)                            | none              | none (baseline)  |
| Boost.Math        | special.hpp                                  | BH                | ULP-level        |
| Cephes            | special.hpp + ~3k LoC vendored               | none              | ULP-to-1e-12     |
