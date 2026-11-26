# Fréchet held scale and the attained shape-one maximum

TASK-299, based on master `2d89fd4a`.

With scale fixed at one, the fit to three zeros, four copies of `1/20`, and
40 returns the unique restricted maximum at shape one. Replacing 40 with 39
gives the unique interior fit at the default controls. Both outcomes are
certified in the native and double-only builds.

## Change

When the held-scale cover refutes an interior search, its feasible witness
is a slab midpoint. The Fréchet fitter now solves the conditional location
at shape one with the existing monotone solver and checks the inward shape
score and local coefficient accuracy. If the edge likelihood is strictly
above the enclosed interior solution, it reruns the cover from the edge
within the original certificate-evaluation allowance. A complete cover
with a shape-one root supplies global optimality and uniqueness. If the
cover is inconclusive and the edge beats the first witness or is on the
same shape-one face, the locally accurate edge is an uncertified estimate
with unknown cardinality. [certificate.hpp:212](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/certificate.hpp#L212)

The public fitter checks restored location accuracy, support, and the
likelihood against the original sample. If the edge cannot be accepted,
a verified refutation still reports both witnesses. The dossier describes
the recovery and cites the held-scale nonuniqueness analysis. The proof
inventory recognizes the existing complete-cover singleton assignment.
[mle.hpp:307](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp#L307)
· [README.md:403](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md#L403)

## Mathematical and numerical evidence

For each fixed shape at least one, the monotone location score gives a
unique maximizing gap. The earlier argument excludes shapes in `[6/5, 10]`
at `r = 40`, using a feasible shape-one likelihood, and excludes shapes
above 10 by the decreasing envelope
`log(shape + 1) - (shape + 1) * log(39) / 8`. This places the winner in
the left interval. To identify the edge, the new replay bounds the profile
derivative strictly below zero on `[1, 11/10]` and excludes `[11/10, 6/5]`
by strict likelihood comparisons. Together these prove that shape one is
the unique global winner for `r = 40`. At `r = 39` the interval checks
exclude `[1, 6/5]` and the tail, placing every global winner in
`(6/5, 10)`. The checker reproduces the exclusions for both exact `1/20`
and its binary64 representation. It uses 50-decimal-digit interval
arithmetic and the conditional root's uniform score signs to enclose
the profile on each slab.
[Earlier math report](2026-11-25-frechet-math-report.md#fréchet-single-holds)
· [Interval replay](2026-11-26-frechet-heldscale-edge-data/reference.py)

The numerical coefficients come from an independent mpmath solve of the
location and shape scores with 80 decimal digits of working precision.
The replay prints 50 significant digits. The C++ and R tests pin 17
significant digits from the exact binary64 sample and check the shape-one
restriction at `r = 40`. The log likelihoods below are totals. The production
global and singleton certificates are exact-arithmetic arguments conditional
on the computed comparisons with practical floating-point margins.

| r | Candidate | Shape | Location | Log likelihood |
|---:|---|---:|---:|---:|
| 39 | Shape-one edge | 1 | −0.473024322278132 | −11.689895952201332 |
| 39 | Interior | 1.574913889527525 | −0.704018719324224 | −11.683597333939747 |
| 40 | Shape-one edge | 1 | −0.473045910746659 | −11.739306349287975 |
| 40 | Interior | 1.543265128752675 | −0.695662179446010 | −11.747142044660334 |

[Reference output](2026-11-26-frechet-heldscale-edge-data/reference.out)
· [Native and double-only fit probe](2026-11-26-frechet-heldscale-edge-data/fit-probe.out)
· [C++ regression, test.cpp:641](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/test.cpp#L641)
· [R regression, test.R:322](../../../bindings/r/distributions/frechet/test.R#L322)

## Route audit

- The fixed-location route delegates to inverse-Weibull fits, and the
  fixed-shape routes solve a monotone location score. The Fréchet all-free
  two-point solver already compares and returns the attaining shape-one
  member when it wins. [mle.hpp:167](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp#L167)
  · [certificate.hpp:67](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/certificate.hpp#L67)
- GEV's single-hold cover converts a better feasible point to
  `SearchContradicted` and checks it again after public-coordinate
  restoration. The joint GEV fallback has another witness-to-stop branch
  after an unresolved cover. The general all-free Fréchet route delegates
  to that joint fallback and propagates the status. The frozen Fréchet
  snapshots contain zero examples of that stop.
  [GEV held_certificate.hpp:520](../../../univariateML-cpp/include/cppunivariate/distributions/gev/held_certificate.hpp#L520)
  · [GEV mle.hpp:446](../../../univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp#L446)
  · [GEV joint_certificate.hpp:726](../../../univariateML-cpp/include/cppunivariate/distributions/gev/joint_certificate.hpp#L726)
  · [Fréchet mle.hpp:383](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp#L383)

## Validation

All final runs used the workstation. The two C++ builds used GCC 15.2.0.
The filtered R suite used R 4.6.1.

| Check | Result |
|---|---|
| Native snapshot vs. stored baseline | 208 identical Fréchet rows |
| Double-only snapshot vs. stored baseline | 208 identical Fréchet rows |
| Modifier native | 0 stored Fréchet rows |
| Modifier double-only | 0 stored Fréchet rows |
| `fit-density.py`, `fit-consistency.py` | Zero new findings in both precision modes |
| `strict-uniqueness.py` | Passed, 218 proof sites |
| `just test-changed --no-long-double` | Fréchet selected. CTest 1/1 in each precision mode. R 398 passed, 7 skipped, 0 warnings or failures. JavaScript build passed, 237/237 tests passed. |
| `just site-check` | Passed, including browser checks and all 86 generated C++ snippets |

The site browser check initially failed because the default Playwright
cache was absent. The passing rerun used the existing Quarto Chromium
headless shell via `BROWSER_EXECUTABLE` and set `EMCC_CORES=1` to correct
the first run's default compiler fan-out under a single-slot reservation.
[Snapshot and strict checks](2026-11-26-frechet-heldscale-edge-data/snapshot-checks.out)
· [Targeted suite](2026-11-26-frechet-heldscale-edge-data/test-changed.out)
· [Site check](2026-11-26-frechet-heldscale-edge-data/site-check.out)

## Open

- A Fréchet comparison in the two frozen modifier snapshot modes would
  need a registry entry and stored baseline rows. Both are absent. The
  runner rejects `--family frechet --modifier-routes` at the registry lookup.
- The shared GEV witness-to-stop paths would need their own attained-edge
  identification and coverage before they could return a maximizer. An
  all-free Fréchet runtime reproducer for the joint path remains to be
  identified.
