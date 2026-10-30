# TASK-81: p2-extreme

Changed only `README.md`, `mle.hpp`, `adapter.cpp`, `test.R`, and `test.cpp`
in the `gev`, `gpd`, and `frechet` distribution folders, plus this report.
No commits, goldens, shared sources, dpqr files, or board entries changed.

## Holds

- GEV: added `mu` and `sigma`, individually and in every combination with
  `xi`, including all held. Fixed shape profiles the remaining location or
  scale; the held-scale Gumbel location is exact. Free shape profiles these
  scalar fits and compares interior maxima with the current attained bounds
  [-1/2, 1]. Both-held location/scale restrict the search to feasible support.
  A held shape above 1 is accepted with location or scale held when the
  collapse criterion permits. The old default and xi-only paths are retained.
- GEV with mu held at the minimum detects divergent scale collapse, and
  compares half-minimum ties against the finite collapse face. Incompatible
  support, nonfinite holds, and failed searches stop. The collapse diagnostic
  is declared under `limits`, conditional on held mu.
- GPD: added `sigma` with held threshold, with shape free or held.
  Held scale leaves a scalar shape search above its feasible support bound;
  the attained -1/2 floor is compared explicitly. All held evaluates the
  likelihood after checking support. Existing default/held-shape scale
  fitting is unchanged.
- GPD `mu = NULL` remains unsupported by A4, including with sigma held.
  Replaced the broad unsupported-sigma entry with exact `no-mle` entries
  for `[sigma]` and `[sigma, xi]` when mu is free. On the current xi >= -1/2
  domain the likelihood increases with mu. The xi=-1 uniform plateau cases
  from P0 would require the out-of-scope P3 endpoint work.
- Frechet: added `lambda` and `sigma`, in every combination with `mu`.
  With lambda and mu held, sigma has the exact power-mean solution, using
  expm1/log1p to retain the geometric-mean limit at tiny held shape.
  With mu and sigma held, the strictly concave shape score is bisected.
  Free location profiles the exact scale or the concave shape solve.
  With shape free and scale held, lambda >= 1 remains enforced.
  With lambda and sigma held, every positive lambda is accepted.
- Frechet detects fixed-shape collapse when lambda*(n-k) <= k and both
  location and scale are free, and constant-sample shape divergence.
  Those sample-dependent diagnostics are declared under `limits`.
  The existing inverse-Weibull and default GEV/Gumbel paths are retained.
- All successful held coefficients are returned exactly and recorded in
  `fixed`. Every new argument is documented in `r.control` and the prose.
  No `not-implemented` hold entries remain in these three families.

## Evidence and limits

The exact Frechet scale formula, strict concavity of its held-location/scale
shape likelihood, the GPD held-threshold/shape scale root, and the stated
fixed-shape collapse criteria have analytic justification in P0.
The scalar searches enumerate sampled local maxima and refine brackets.
Global coverage of the nested numerical profiles is checked against
multistart reference-density optimizations; it is not a mathematical
certificate. The P0 global-root/exhaustion proof gaps remain open.
Searches work in standardized units and reject unresolved search edges or
iteration exhaustion rather than returning those approximations as fits.

The added R oracle tests cover every new hold subset on n=20 and 100,
ties, n=2, and limit-model samples. Five starts are independent of the
fitter, with an additional density solve at the returned fit. Separate
checks cover constant data, n=1 where admissible, support errors,
nonfinite/invalid holds, collapse, and iteration failures. C++ tests check
held values, free scores, exact solutions, and exceptional cases.

## Validation

Required commands were run through `/tmp/umvl-codex/simbox-run.sh`:

- `just cpp-test`: 420 distribution cases, 418 passed and the two permitted
  failures; 513,752 assertions, 513,750 passed. The other five CTest targets
  passed. Only `betapr/test.cpp:33` and `std/test.cpp:40` failed.
- `just quality gev gpd frechet`: all three passed after the final change.
- `just test`: 10,979 passing checks; only the three permitted failures:
  frozen `sstd` quantiles, golden `bnbinom`, and golden `std`. No warnings
  or skips. No unexpected failures in either language.

Family test inventory and final R family results:

| Family | C++ cases | R test blocks | Passing R checks |
|---|---:|---:|---:|
| gev | 7 | 9 | 319 |
| gpd | 6 | 4 | 164 |
| frechet | 7 | 10 | 194 |

The new oracle blocks exercise 14 new hold subsets on six samples each:
84 fits, with 66 nontrivial oracle comparisons, each using six reference
optimizations (five independent starts plus one result-seeded start).
The final full R run passed all 1,764 generic conformance checks and all
677 checks in these three family files, including the tiny-shape test.
All required commands finished; no task command remains running.

The standalone local C++ hold probe passed 13 new subsets. A second,
redundant local golden probe was cancelled while still waiting for a CPU
slot; no result from it is counted. Golden hold coverage comes from the
completed simbox conformance sweep instead.

Logs are under `/tmp/umvl-codex/p2-extreme/`: `cpp-final2.log`,
`quality-final.log`, and `r-final2.log`. Golden files are unchanged.

## Open questions

Certified global profile search remains open, as recorded in P0.
The xi >= -1 change, endpoint closure, and restriction flags remain P3 work.
