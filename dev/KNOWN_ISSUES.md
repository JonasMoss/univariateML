# Defects found in former reference packages

Until TASK-78 (merged 2026-10-30) the R package took most models' d/p/q/r
from extraDistr and actuar, repairing the defects below at runtime. It now
uses its own d/p/q/r, generated from the C++ core and checked against
high-precision truth tables (`distributions/<id>/truth.json`); none of
these packages is a dependency. The table is kept as a record of what was
found, with a one-line reproducer each. Drafts for upstream reports:
`dev/research/upstream/extraDistr.md` and `dev/research/upstream/actuar.md`.

VGAM was never used as a reference: it is unmaintained and its densities
were unreliable (issue #52). sads, fGarch, logitnorm and nakagami were
dropped on 2026-10-29.

| Package | Defect | Reproducer |
|---|---|---|
| sads 0.6.5 | `qzipf` is off by one between CDF jumps | `sads::qzipf(0.5, N = 10, s = 1)` is 1, should be 2 |
| sads 0.6.5 | `pzipf` returns 0 for non-integer `q` instead of $F(\lfloor q \rfloor)$ | `sads::pzipf(1.5, 10, 1)` is 0, should be 0.341 |
| sads 0.6.5 | `dzipf`/`pzipf`/`qzipf`/`rzipf` give `NaN` at `s = 0` (the uniform member; sads requires `s > 0`) | `sads::dzipf(1, 10, 0)` |
| sads 0.6.5 | `dmzsm` is 0 everywhere for `theta < 1`: the normaliser includes the infinite weight at `y = J` | `sads::dmzsm(1:3, J = 10, theta = 0.5)` is `0 0 0` |
| fGarch 4052.93 | `dstd(log = TRUE)` is `-Inf` where the log density is finite | `fGarch::dstd(1e6, nu = 1e6, log = TRUE)` |
| extraDistr 1.10.0.5 | `dzinb(log = TRUE)` is `-Inf` far in the tail: it takes the log of an underflowed density | `extraDistr::dzinb(10000, 2, 0.5, 0.1, log = TRUE)`, should be -6923.75 |
| extraDistr 1.10.0.5 | `pzinb(lower.tail = FALSE, log.p = TRUE)` is `-Inf`: it complements the lower CDF | `extraDistr::pzinb(100, 2, 0.5, 0.1, lower.tail = FALSE, log.p = TRUE)`, should be about -66.17 |
| extraDistr 1.10.0.5 | `qzinb(1, ...)` is `NaN` for the point mass at zero (`pi = 1`) | `extraDistr::qzinb(1, 2, 0.5, 1)`, should be 0 |
| extraDistr 1.10.0.5 | `qgpd` loses all accuracy for shapes near zero (subtractive `pow(...) - 1`) | `extraDistr::qgpd(0.5, xi = 1e-16)` is 0, should be $\log 2$ |
| extraDistr 1.10.0.5 | `pgpd` underflows for tiny excesses | `extraDistr::pgpd(1e-20)` is 0, should be about `1e-20` |
| extraDistr 1.10.0.5 | `pgpd(lower.tail = FALSE, log.p = TRUE)` is `-Inf`: it complements the lower CDF | `extraDistr::pgpd(1000, xi = 0, lower.tail = FALSE, log.p = TRUE)`, should be -1000 |
| extraDistr 1.10.0.5 | `ptriang` is `NaN` at the lower endpoint when the mode is there | `extraDistr::ptriang(0, 0, 1, 0)`, should be 0 |
| extraDistr 1.10.0.5 | `phuber` is `NaN` far in the tails for `epsilon` above about 38 (`exp(epsilon^2/2)` overflows) | `extraDistr::phuber(-60, 0, 1, 40)` |
| actuar 3.3-7 | `log = TRUE` densities of the zero-truncated and zero-modified geometric, NB and binomial underflow to `-Inf` (log of an underflowed probability) | `actuar::dztnbinom(10000, 2, 0.4, log = TRUE)` |
| actuar 3.3-7 | upper-tail log CDFs of the zero-truncated Poisson, geometric, NB and binomial are `-Inf` (complement of the lower CDF) | `actuar::pztpois(10000, 2, lower.tail = FALSE, log.p = TRUE)` |
| actuar 3.3-7 | `pzmlogarithmic` upper-tail log CDF is wrong by cancellation | `actuar::pzmlogarithmic(10000, 0.4, 0.3, lower.tail = FALSE, log.p = TRUE)` is about -36.4, should be below -9000 |
| actuar 3.3-7 | `qztbinom(1, ...)` and `qzmbinom(1, ...)` are `Inf`, not the upper endpoint `size` | `actuar::qztbinom(1, 10, 0.4)`, should be 10 |
| extraDistr 1.10.0.5 | `dzib(log = TRUE)` underflows, `pzib` upper-tail log CDF is `-Inf`, `qzib(1, ...)` is `NaN` at `pi = 1` | `extraDistr::dzib(9999, 10000, 0.4, 0.3, log = TRUE)`; `extraDistr::qzib(1, 10, 0.4, 1)` |
| extraDistr 1.10.0.5 | `qgev` and `rgev` lose accuracy for shapes within about `1e-12` of zero; `qgev(1, ...)` is `Inf` at negative shape; `pgev` upper-tail log underflows | `extraDistr::qgev(0.5, xi = 1e-16)` is 0, should be 0.3665; `extraDistr::qgev(1, xi = -0.5)` is `Inf`, should be 2; `extraDistr::pgev(1000, lower.tail = FALSE, log.p = TRUE)` is `-Inf`, should be -1000 |
| extraDistr 1.10.0.5 | `dtnorm`/`ptnorm`/`qtnorm` fail when the interval lies far in a tail of the parent normal | `extraDistr::dtnorm(10.05, a = 10, log = TRUE)` is `Inf`, should be 1.811; `extraDistr::qtnorm(0.5, a = 40, b = 41)` is `Inf` |
| actuar 3.3-7 | `trgamma`/`invtrgamma` d/p/q/r lose accuracy at extreme shapes (density cancellation, `Inf`/0 quantiles, 0/1 CDFs, non-finite draws) | `actuar::dtrgamma(1, 1e12, 1, scale = 1e-12, log = TRUE)` is 12.8984, should be 12.8966; `actuar::ptrgamma(0.9, 1e-6, 1e6)` is 0, should be 0.9 |
| extraDistr 1.10.0.5 | `dbnbinom(log = TRUE)` loses accuracy at huge shapes (log-gamma cancellation) | `extraDistr::dbnbinom(0:3, 2, 4e14, 1e14, log = TRUE)` is (-0.469, -1.406, -2.563, -3.906), should be about `dnbinom(0:3, 2, 0.8, log = TRUE)` = (-0.446, -1.363, -2.567, -3.888) |
| extraDistr 1.10.0.5 | `pbnbinom` exceeds one at huge shapes | `extraDistr::pbnbinom(0:3, 2, 4e14, 1e14)` is about (2.80, 3.18, 3.24, 3.24) |
| extraDistr 1.10.0.5 | no `qbnbinom`; `pbnbinom` costs O(q) | |
| actuar 3.3-7 | `ptrbeta`/`qtrbeta` saturate in the tails (inversion through a beta value rounded to 0 or 1) | `actuar::ptrbeta(.4, .003, 1000, .002)` is 0, true about 0.0960; `actuar::ptrbeta(1e300, 2, 5, 3, lower.tail = FALSE, log.p = TRUE)` is `-Inf`, about -6905.96; `actuar::qtrbeta(.5, .01, 1, 1)` is `Inf`, should be `expm1(log(2)/.01)`; `actuar::qtrbeta(.001, .06, 50, .04, lower.tail = FALSE)` is `Inf`, about 7.4 |
| actuar 3.3-7 | `dpoisinvgauss` is wrong near the Poisson limit (unscaled recurrence) | `actuar::dpoisinvgauss(2, mean = 2, shape = 1e20, log = TRUE)` is `log(2)`, a probability of 2; `actuar::dpoisinvgauss(1000, 1000, 1e12, log = TRUE)` is `-Inf` |
| extraDistr 1.10.0.5 | `pdweibull` does not floor its argument | `extraDistr::pdweibull(0.5, 0.7, 1)` is 0.414, should be `pdweibull(0, 0.7, 1)` = 0.3 |
| extraDistr 1.10.0.5 | `ddweibull(log = TRUE)` is `-Inf` far in the tail (log of a difference of underflowed powers) | `extraDistr::ddweibull(1000, 0.999, 2, log = TRUE)`, should be -1000.645 |
| extraDistr 1.10.0.5 | no `qbbinom`; `dbbinom`/`pbbinom` lose accuracy at huge shapes (large `lbeta` differences) | `extraDistr::dbbinom(5, 10, 1e12, 1e12, log = TRUE)` is -1.40186, should be -1.40204 |
| extraDistr 1.10.0.5 | `dhcauchy(log = TRUE)` and `dht(log = TRUE)` overflow the squared observation/scale ratio | `extraDistr::dhcauchy(1e200, log = TRUE)` is `-Inf`, should be about -921.486 |
| extraDistr 1.10.0.5 | `phcauchy`, `pht`, `phnorm` upper-tail log CDFs are `-Inf` (complement of the lower CDF) | `extraDistr::phcauchy(1e20, lower.tail = FALSE, log.p = TRUE)`, should be about -46.503 |
| extraDistr 1.10.0.5 | `qhcauchy`, `qht`, `qhnorm` round extreme upper probabilities away | `extraDistr::qht(-100, 3, lower.tail = FALSE, log.p = TRUE)` is `Inf`, should be about 3.90e14 |
| extraDistr 1.10.0.5 | truncated Poisson and binomial d/p/q/r subtract rounded parent CDFs: infinite densities, NaN CDFs, quantiles at the wrong bound under severe truncation; `q(0)` returns the excluded lower bound | `extraDistr::dtpois(101, 2, 100, 110, log = TRUE)` is `Inf`, should be about -0.0198; `extraDistr::qtbinom(.5, 100, .1, 90, 100)` is 100, should be 91 |
| extraDistr 1.10.0.5 | `pdlaplace` picks its branch from `q` instead of `q - location` (negative probabilities) and loses the upper log tail; no `qdlaplace` | `extraDistr::pdlaplace(3, 5, .5)` is -1/3, should be 1/6 |
| extraDistr 1.10.0.5 | `ddlaplace` with noninteger location checks the observation's integrality, while `rdlaplace` samples on `location + Z` | `extraDistr::ddlaplace(0, .5, .4)` is positive |
| extraDistr 1.10.0.5 | `dfrechet`/`pfrechet`/`qfrechet` lose accuracy at large shapes near the Gumbel limit: the rounded ratio `(x - mu)/sigma` is raised to the power `lambda`, and `qfrechet` cancels `mu + sigma * w` | `extraDistr::dfrechet(-1, 1e12, -1e12, 1e12, log = TRUE)` is -1.718243817774324, should be -1.718281828458904; `extraDistr::qfrechet(0.5, 1e12, -1e12, 1e12)` is 0.3665771484375, should be 0.3665129205817315 |
| extraDistr 1.10.0.5 | `dnhyper`/`pnhyper`/`qnhyper`/`rnhyper` convert counts to 32-bit integers, take logs of underflowed masses, complement the lower CDF for upper tails, and crash for an urn with no black balls | `extraDistr::dnhyper(1003, 1000, 1000, 3, log = TRUE)` is `-Inf`, should be about -1369.14; `extraDistr::dnhyper(0, 0, 0, 0)` segfaults |
| extraDistr 1.10.0.5 | `ddnorm`/`ddgamma(log = TRUE)` subtract ordinary CDFs and take the log afterwards: `-Inf` in both tails | `extraDistr::ddnorm(c(-40, 10), log = TRUE)` is `c(-Inf, -Inf)`; no `qdnorm`/`qdgamma` |
| extraDistr 1.10.0.5 | `dskellam(log = TRUE)` overflows its unscaled Bessel function and is `NaN` at zero rates; there is no `pskellam`/`qskellam` | `extraDistr::dskellam(0, 1000, 1000, log = TRUE)` is `NaN`, should be -4.71933 |
| extraDistr 1.10.0.5 | `qfrechet`/`pfrechet` upper log tails round through `1 - p` (`Inf` quantiles, `-Inf` log survival) | `extraDistr::qfrechet(-40, 2, lower.tail = FALSE, log.p = TRUE)` is `Inf`, should be about 4.85e8 |
| actuar 3.3-7 | `qfpareto` upper log tails lose the complementary beta probability | `actuar::qfpareto(-40, 0, .3, .7, .4, lower.tail = FALSE, log.p = TRUE)` is `Inf`, should be about 6.89e81 |
| extraDistr 1.10.0.5 | `qtlambda`/`rtlambda` lose digits at tiny nonzero shape (subtracting nearly equal powers) and round upper log tails to `Inf` | `extraDistr::qtlambda(-100, 0, lower.tail = FALSE, log.p = TRUE)` is `Inf`, should be 100 |
| actuar 3.3-7 | `dgenbeta(log = TRUE)` loses precision near the unit-gamma limit; `pgenbeta(log.p = TRUE)` underflows; `qgenbeta`/`rgenbeta` saturate an intermediate beta quantile | `actuar::qgenbeta(.5, .0001, 2, 100)` is about 8.27e-4, should be 7.81e-31 |
| extraDistr 1.10.0.5 | `dslash` cancels near its centre and overflows in the far tails; `pslash` has twice the right slope near the centre and loses both tails; no `qslash` | `extraDistr::pslash(1e-10) - .5` is about 3.99e-11, should be 1.99e-11 |
| extraDistr 1.10.0.5 | `dbhatt`/`pbhatt` subtract rounded normal CDFs (log density `-Inf`, tails lost); no `qbhatt`; the help shows a CDF that is zero at `mu` | `extraDistr::dbhatt(10, log = TRUE)` is `-Inf`, should be -44.3213 |
| nakagami 1.1.0 | `pnaka` is positive below the support | `nakagami::pnaka(-1, 2, 3)` is 0.144, should be 0 |

Upstream trackers:
nakagami is maintained by the univariateML author.

## Platform precision (TASK-125)

The Bhattacharjee golden fit moves with the intermediate precision: in the
x86-64 double-width proxy its three coefficients differed from the native
80-bit golden by at most 1.61e-8 relatively. The score norm was 5.73e-6
against the family's 1e-3 gate. The C++ golden check permits 5e-8 relative
under `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` or where `long double` has
no more precision than `double`; x86-64 keeps 1e-12. The R golden test uses
5e-8 for bhatt on every platform. This is one tested dataset, not a bound on other
fits or platforms. Not yet run on such hardware. See `dev/research/evaluations/2026-10-30-long-double-report.md`.

For an underflowed gamma upper tail, the asymptotic helper returns NaN if its
remainder criterion cannot be met within one million terms. Platforms with a
wider exponent range may instead evaluate such a tail directly. The double-width
regression at shape `1e14`, argument `1e14 + 4e8` pins the NaN failure. There
is no uniform accuracy or availability guarantee outside the tested parameter grid.

Beta-coordinate quantile inversion reports NaN when it cannot represent a
finite logit bracket or reduce it to adjacent floating-point values within its
iteration budget. For example, log probability -1e308 and beta shape 1e-308
have a logit outside the finite double range. It does not return a capped
interior estimate in this case.
