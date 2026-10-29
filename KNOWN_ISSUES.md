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
| sads 0.6.5 | `qzipf` is off by one between CDF jumps | `sads::qzipf(0.5, N = 10, s = 1)` is 1, should be 2 | repaired at runtime in `univariateML_reference_fixes` |
| sads 0.6.5 | `pzipf` returns 0 for non-integer `q` instead of $F(\lfloor q \rfloor)$ | `sads::pzipf(1.5, 10, 1)` is 0, should be 0.341 | `pml` at non-integer arguments; plots use integers |
| sads 0.6.5 | `dzipf`/`pzipf`/`qzipf` give `NaN` at `s = 0` (the uniform member) | `sads::dzipf(1, 10, 0)` | none seen: on uniform data the fitter returns a tiny positive `s` |
| fGarch 4052.93 | `dstd(log = TRUE)` is `-Inf` where the log density is finite | `fGarch::dstd(1e6, nu = 1e6, log = TRUE)` | `dml(x, mlstd(...), log = TRUE)` far in the tails |
| extraDistr 1.10.0.5 | `dzinb(log = TRUE)` is `-Inf` far in the tail: it takes the log of an underflowed density | `extraDistr::dzinb(10000, 2, 0.5, 0.1, log = TRUE)`, should be -6923.75 | `dml(x, mlzinb(...), log = TRUE)` at huge counts |
| extraDistr 1.10.0.5 | `pzinb(lower.tail = FALSE, log.p = TRUE)` is `-Inf`: it complements the lower CDF | `extraDistr::pzinb(100, 2, 0.5, 0.1, lower.tail = FALSE, log.p = TRUE)`, should be about -66.17 | upper-tail `pml` on the log scale |
| extraDistr 1.10.0.5 | `qzinb(1, ...)` is `NaN` for the point mass at zero (`pi = 1`) | `extraDistr::qzinb(1, 2, 0.5, 1)`, should be 0 | none: fits never return `pi = 1` |
| extraDistr 1.10.0.5 | `qgpd` loses all accuracy for shapes near zero (subtractive `pow(...) - 1`) | `extraDistr::qgpd(0.5, xi = 1e-16)` is 0, should be $\log 2$ | `qml` for `mlgpd` fits with $\lvert\xi\rvert \lesssim 10^{-12}$ |
| extraDistr 1.10.0.5 | `pgpd` underflows for tiny excesses | `extraDistr::pgpd(1e-20)` is 0, should be about `1e-20` | negligible |
| extraDistr 1.10.0.5 | `pgpd(lower.tail = FALSE, log.p = TRUE)` is `-Inf`: it complements the lower CDF | `extraDistr::pgpd(1000, xi = 0, lower.tail = FALSE, log.p = TRUE)`, should be -1000 | upper-tail `pml` on the log scale |
| extraDistr 1.10.0.5 | `ptriang` is `NaN` at the lower endpoint when the mode is there | `extraDistr::ptriang(0, 0, 1, 0)`, should be 0 | repaired at runtime in `univariateML_reference_fixes` |
| extraDistr 1.10.0.5 | `phuber` is `NaN` far in the tails for `epsilon` above about 38 (`exp(epsilon^2/2)` overflows) | `extraDistr::phuber(-60, 0, 1, 40)` | `pml` for `mlhuber(x, epsilon = 40)` far out |
| actuar 3.3-7 | `log = TRUE` densities of the zero-truncated and zero-modified geometric, NB and binomial underflow to `-Inf` (log of an underflowed probability) | `actuar::dztnbinom(10000, 2, 0.4, log = TRUE)` | `dml(..., log = TRUE)` at huge counts |
| actuar 3.3-7 | upper-tail log CDFs of the zero-truncated Poisson, geometric, NB and binomial are `-Inf` (complement of the lower CDF) | `actuar::pztpois(10000, 2, lower.tail = FALSE, log.p = TRUE)` | upper-tail `pml` on the log scale |
| actuar 3.3-7 | `pzmlogarithmic` upper-tail log CDF is wrong by cancellation | `actuar::pzmlogarithmic(10000, 0.4, 0.3, lower.tail = FALSE, log.p = TRUE)` is about -36.4, should be below -9000 | upper-tail `pml` on the log scale |
| actuar 3.3-7 | `qztbinom(1, ...)` and `qzmbinom(1, ...)` are `Inf`, not the upper endpoint `size` | `actuar::qztbinom(1, 10, 0.4)`, should be 10 | `qml(1, ...)`; plots use the finite support |
| extraDistr 1.10.0.5 | `dzib(log = TRUE)` underflows, `pzib` upper-tail log CDF is `-Inf`, `qzib(1, ...)` is `NaN` at `pi = 1` | `extraDistr::dzib(9999, 10000, 0.4, 0.3, log = TRUE)`; `extraDistr::qzib(1, 10, 0.4, 1)` | as for zinb |
| extraDistr 1.10.0.5 | `qgev` and `rgev` lose accuracy for shapes within about `1e-12` of zero; `qgev(1, ...)` is `Inf` at negative shape; `pgev` upper-tail log underflows | `extraDistr::qgev(0.5, xi = 1e-16)` is 0, should be 0.3665; `extraDistr::qgev(1, xi = -0.5)` is `Inf`, should be 2; `extraDistr::pgev(1000, lower.tail = FALSE, log.p = TRUE)` is `-Inf`, should be -1000 | repaired at runtime in `univariateML_reference_fixes` |
| extraDistr 1.10.0.5 | `dtnorm`/`ptnorm`/`qtnorm` fail when the interval lies far in a tail of the parent normal | `extraDistr::dtnorm(10.05, a = 10, log = TRUE)` is `Inf`, should be 1.811; `extraDistr::qtnorm(0.5, a = 40, b = 41)` is `Inf` | repaired at runtime in `univariateML_reference_fixes` |
| actuar 3.3-7 | `trgamma`/`invtrgamma` d/p/q/r lose accuracy at extreme shapes (density cancellation, `Inf`/0 quantiles, 0/1 CDFs, non-finite draws) | `actuar::dtrgamma(1, 1e12, 1, scale = 1e-12, log = TRUE)` is 12.8984, should be 12.8966; `actuar::ptrgamma(0.9, 1e-6, 1e6)` is 0, should be 0.9 | repaired at runtime in `univariateML_reference_fixes` |
| actuar 3.3-7 | `dpoisinvgauss` is wrong near the Poisson limit (unscaled recurrence) | `actuar::dpoisinvgauss(2, mean = 2, shape = 1e20, log = TRUE)` is `log(2)`, a probability of 2; `actuar::dpoisinvgauss(1000, 1000, 1e12, log = TRUE)` is `-Inf` | `dml` for `mlpig` fits with very large shape (barely overdispersed data); grid, figure and a draft upstream patch in `notes/evaluations/2026-10-29-pig/` |
| extraDistr 1.10.0.5 | `pdweibull` does not floor its argument | `extraDistr::pdweibull(0.5, 0.7, 1)` is 0.414, should be `pdweibull(0, 0.7, 1)` = 0.3 | repaired at runtime in `univariateML_reference_fixes` |
| extraDistr 1.10.0.5 | `ddweibull(log = TRUE)` is `-Inf` far in the tail (log of a difference of underflowed powers) | `extraDistr::ddweibull(1000, 0.999, 2, log = TRUE)`, should be -1000.645 | `dml(..., log = TRUE)` far in the tail |
| extraDistr 1.10.0.5 | no `qbbinom`; `dbbinom`/`pbbinom` lose accuracy at huge shapes (large `lbeta` differences) | `extraDistr::dbbinom(5, 10, 1e12, 1e12, log = TRUE)` is -1.40186, should be -1.40204 | `qml` built from `pbbinom` by integer inversion (`univariateML_reference_quantiles`); fits near the binomial face |
| nakagami 1.1.0 | `pnaka` is positive below the support | `nakagami::pnaka(-1, 2, 3)` is 0.144, should be 0 | `pml` at negative arguments |

Upstream trackers: sads <https://github.com/piLaboratory/sads/issues>;
nakagami is maintained by the univariateML author.
