# Prentice generalized gamma d/p/q/r

TASK-176 adds standalone C++ density, distribution and quantile functions and an independent-draw sampler in `univariateML-cpp/include/cppunivariate/distributions/gengamma/`. The parameters are log-location `mu`, positive log-scale `sigma`, and signed shape `Q`. There is no fitter or R/JavaScript model yet; that is the separate TASK-177. No existing family's d/p/q/r, truth table, default fit or golden was changed.

## Calculation and numerical evidence

Put `w = (log(x) - mu)/sigma`. For nonzero `Q`, `a = Q^-2` and

```text
R(a) = log Gamma(a) - (a - 1/2) log(a) + a - log(2*pi)/2,
log f(x) = -log(sigma) - log(x) - log(2*pi)/2 - R(a)
           - (exp(Q*w) - 1 - Q*w)/Q^2.
```

This identity is exact. The exponential remainder is evaluated as a series near zero; at `Q = 0` the normal/lognormal limit is explicit. For `|Q| <= 0.25`, the Stirling remainder uses terms through `-Q^14/1680`; the first omitted Stirling term bounds the exact-arithmetic truncation by `Q^18/1188`, at most `1.23e-14` in that range. The zero endpoint is handled according to the sign of `Q*sigma - 1` for positive `Q`; negative `Q` and the lognormal limit have zero density there.

For small nonzero `Q`, the code integrates the smaller tail of the standardized log-density. It scales distance by `max(1, |(exp(Q*w)-1)/Q|)`. The density decreases away from its mode at `w=0`. In exact arithmetic, the fourth-derivative bound used by composite Simpson quadrature controls its absolute error by `5e-14` on that scaled integral; log-concavity gives a tangent bound below `2.5e-14` on the omitted tail. The integral and the Stirling correction are combined on the log scale. Quantile inversion brackets a monotone log tail and checks its resulting representable interval; a failed quadrature or unresolved interval returns `NaN`. These bounds are conditional on the computed comparisons: arithmetic is not outward rounded, and they are not interval certificates of floating-point error.

For intermediate `|Q|`, gamma tails reverse for negative `Q`. In the small-argument gamma regime, the family uses `lgamma1p(a)` so that `1+a` does not round to one. The leading small-gamma quantile retains the first correction `exp(L)/(a+1)` separately through the `sigma/Q` transformation, where `L` is the leading log-gamma quantile. For `a < 16` and `L <= -36`, the omitted inverse-series terms are bounded in exact arithmetic by `2*exp(2*L)`; if that bound can change the scaled answer beyond the practical `1e-10` relative margin, the function returns `NaN`. For `|Q| > 1e10`, `a < 1e-20`: a small-gamma-shape route retains logarithmic tails using the exponential integral `E1`, including the small-argument log-hazard form, even when `a` itself underflows. That asymptotic branch and its floating-point margins are supported by the comparisons below, not by an outward-rounded uniform error certificate. The implementation also preserves finite log tails when the intermediate `Q*w` overflows.

The small-`Q` sampler uses a Marsaglia–Tsang gamma rejection proposal but retains `log(1+t)` and the standardized fluctuation before rounding the gamma variate. Its acceptance calculation uses the same exponential remainder. At zero it uses a normal source. Away from the seam it uses a gamma source when safe and inverse transformation for subunit gamma shapes, avoiding the loss of draws whose gamma variate underflows before the power transformation.

The checked-in `truth.py` and `truth.json` contain 13 parameter points: `Q=0`, both signs at `1e-8`, `1e-6`, `1e-4`, and `1e-2`, and `Q=±2, ±10`. There are 267 density/tail rows and 572 quantile rows, with no exclusions. The 80-digit mpmath route checks normalization and antiderivative derivatives against the dossier density; it uses standardized numerical integration near zero and incomplete gamma elsewhere. The C++ truth test compares log density, both log and natural tails, both quantile tails and log probabilities, including inputs down to `-10000`. Tests also check continuity from both sides of zero, reciprocal reflection, parameter transport, the `|Q|=0.25` switch, very large shapes, overflowed coordinates, endpoints and fixed-seed sampler log moments and empirical CDF landmarks.

The separate [evaluation data](2026-11-10-gg-dpqr-data/) retain reproducible checks. A 301-point comparison of the `E1` helper with 60-digit mpmath had worst scaled log error `1.84e-15`. A 48-point independent small-gamma comparison, for shapes `1e4` through `1e200` and log gamma arguments from `-50` to `7`, had worst scaled log-tail error `1.13e-14` and worst relative quantile error `2.5e-12`. These are numerical comparisons, not proofs over all floating-point inputs. The earlier diagnostic documents why direct use of the existing gamma backend is insufficient near `Q=0`.

## Staging until a fitter exists

The dossier carries `stage: dpqr` and has no fabricated `golden.json`. The truth harness uses the dossier simulation parameters as its two sampler selectors for this staged case; they are not presented as a fitted golden. R metadata loading and fixture assembly, JavaScript generation, the inference inventory, and site data, source preparation and content loading exclude the fitter-free family. The site exclusion is derived from the missing fitter header. When TASK-177 adds `mle.hpp`, an adapter and a genuine golden, its card must complete the dossier and remove the staging marker. The existing 99 fitted families remain in those pipelines.

## Validation

All final local build and suite commands below ran on the workstation, one at a time through `/tmp/umvl-cpu/run4`, with the repository's thread controls and `nice -n 10`:

| Check | Result |
|---|---|
| `just cpp-test` | 107/107 development shards passed |
| `just cpp-test-no-long-double` | 131/131 full CTest shards passed |
| `just test` | 23,031 passed, 0 failed, 0 warnings, 154 skipped |
| `just test-full` | 45,846 passed, 0 failed, 0 warnings, 0 skipped |
| `just js-build` | passed |
| `just js-test` | 601/601 passed |
| `just site-check` | passed; 99 fitted families generated |

The R, JavaScript and site checks validate the staging changes; they do not exercise the new family through a binding. Their green runs preceded the final standalone-header quantile safeguard and sampler-moment test, which are excluded from those pipelines. The two C++ suites were rerun after those changes. A focused R code-generation test also passed 17/17 after the staging filter was corrected. `just relations` passed all 157 declared edges. The truth generator was run through the single-slot lock with a 7,200-second budget and generated the full table successfully; its input fingerprint was checked afterwards.

**Open — second machine:** `simbox-run` could not reach simbox and returned 75 on 2026-11-11. The prescribed local `run4` fallback passed 131/131; there is no GCC 13 simbox result for this revision. **Open — universal floating-point error:** the exact-arithmetic quadrature and truncation arguments are conditional on computed comparisons, and the sampled high-precision checks do not establish a uniform error bound for every finite combination of `mu`, `sigma`, `Q` and tail probability. Extreme cancellation under scale/location transformation remains subject to that practical floating-point limitation.
