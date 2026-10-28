# Known issues in reference packages

The R package takes every model's d/p/q/r from an external package
(`r.density` in `distributions/<id>/README.md`). Fitting and `logLik` never
use them (the fitters are C++); `dml`/`pml`/`qml`/`rml`, plots and
`simulate_default` do. Below are the defects found so far, each with a
one-line reproducer. Workarounds live in `univariateML_reference_fixes`
(`univariateML/R/code_helpers.R`). Each one checks for its defect on every
call, so an upstream fix switches it off without a release here.

VGAM is not used as a reference package: it is unmaintained and its
densities were unreliable (issue #52). It is installed only as a
dependency of sads.

| Package | Defect | Reproducer | Effect here |
|---|---|---|---|
| sads 0.6.5 | `qzipf` is off by one between CDF jumps | `sads::qzipf(0.5, N = 10, s = 1)` is 1, should be 2 | worked around in `qml` (and so in `plot`) |
| sads 0.6.5 | `pzipf` returns 0 for non-integer `q` instead of $F(\lfloor q \rfloor)$ | `sads::pzipf(1.5, 10, 1)` is 0, should be 0.341 | `pml` at non-integer arguments; plots use integers |
| sads 0.6.5 | `dzipf`/`pzipf`/`qzipf` give `NaN` at `s = 0` (the uniform member) | `sads::dzipf(1, 10, 0)` | none seen: on uniform data the fitter returns a tiny positive `s` |
| fGarch 4052.93 | `dstd(log = TRUE)` is `-Inf` where the log density is finite | `fGarch::dstd(1e6, nu = 1e6, log = TRUE)` | `dml(x, mlstd(...), log = TRUE)` far in the tails |
| extraDistr 1.10.0.5 | `dzinb(log = TRUE)` is `-Inf` far in the tail: it takes the log of an underflowed density | `extraDistr::dzinb(10000, 2, 0.5, 0.1, log = TRUE)`, should be -6923.75 | `dml(x, mlzinb(...), log = TRUE)` at huge counts |
| extraDistr 1.10.0.5 | `pzinb(lower.tail = FALSE, log.p = TRUE)` is `-Inf`: it complements the lower CDF | `extraDistr::pzinb(100, 2, 0.5, 0.1, lower.tail = FALSE, log.p = TRUE)`, should be about -66.17 | upper-tail `pml` on the log scale |
| extraDistr 1.10.0.5 | `qzinb(1, ...)` is `NaN` for the point mass at zero (`pi = 1`) | `extraDistr::qzinb(1, 2, 0.5, 1)`, should be 0 | none: fits never return `pi = 1` |
| nakagami 1.1.0 | `pnaka` is positive below the support | `nakagami::pnaka(-1, 2, 3)` is 0.144, should be 0 | `pml` at negative arguments |

Upstream trackers: sads <https://github.com/piLaboratory/sads/issues>;
nakagami is maintained by the univariateML author.
