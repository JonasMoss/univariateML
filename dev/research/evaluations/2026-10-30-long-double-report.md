# Long-double portability (TASK-125), 2026-10-30

Status: complete for the tested workloads, with the limitations below. Source base: `1bd8c44e1d7650aacb864a338a0f9be0f11f2f0d`; the working tree contains the precision seam change. This inventory describes the original sites; purposes overlap. The stated risks are analytical possibilities, not measured failures unless explicitly reported below.

| Purpose | Original sites | Role and double-width consequence |
|---|---|---|
| Tail exponent range | `special/special.hpp`, `logitnorm/dpqr.hpp`, `naka/dpqr.hpp` | Boost normal/gamma tails retain values past double underflow; loss of range or subnormal relative precision can corrupt log tails and inverse tails. |
| Tail cancellation and range | `bhatt/dpqr.hpp`, `emg/dpqr.hpp`, `bnbinom/dpqr.hpp`, `cmp/dpqr.hpp`, `cmb/dpqr.hpp`, `hyper/dpqr.hpp`, `skellam/dpqr.hpp`, `sn/dpqr.hpp`, `yulesimon/dpqr.hpp`, `special/cmp.hpp`, `special/tails.hpp` | Wider intermediates protect differences, series, or sums and products; double may lose digits or overflow. Sites vary in their own scaling and stopping rules. |
| Centering and accumulation | `exp`, `gamma`, `gompertz`, `invgauss`, `laplace`, `lnorm`, `logitnorm`, `naka`, `norm`, `pareto`, `pig`, `pois`, `power`, `rayleigh`, `skellam`, `trgamma`, `weibull` fitters | Means, moments, log likelihoods, ratios and scores; double can accumulate rounding or overflow in intermediate sums. |
| Optimizer/profile scores and objectives | `beta`, `betapr`, `bhatt`, `bnbinom`, `emg`, `fatigue`, `gb2`, `ged`, `genbeta`, `gpd`, `gumbel`, `huber`, `llogis`, `lomax` fitters | Accumulation, cancellation, and range affect objective comparisons or score signs; double may change a branch or convergence decision. |
| Search, normalization and comparisons | `binom`, `hyper`, `nhyper`, `snorm` fitters; `nhyper/dpqr.hpp` | Integer bounds, polynomial roots, ties and normalized moments use the extra significand; double can alter discrete decisions or collapse nearby values. |
| Coordinate range | `cauchy`, `logis`, `slash`, `sstd`, `std`, `unif` fitters; `emg/dpqr.hpp`, `sn/dpqr.hpp` | Wider subtraction and scaling avoid intermediate overflow or preserve low bits; double can overflow even for finite endpoints. |

Each family name in the fitter rows denotes its `mle.hpp`. No universal error bounds have been established for these sites; existing tests sample only part of their input domains.

## Seam and native comparison

`core/precision.hpp` supplies one type spelling, `CPPUNIV_EXTENDED_REAL`, and a literal selector: native tokens are `long double`/long-double literals, while `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` selects `double` and double literals. `cppunivariate::ExtendedReal` is a compatibility typedef of that same type for the special-function backend. Emscripten sets the same flag before this header selects the type. The separate `just cpp-test-no-long-double` build uses ccache and the flag. It is an arithmetic/Boost proxy, not a cross-platform ABI or libm test.

Before numerical changes, all 98 distribution test translation units were compiled from frozen HEAD and seam-only snapshots at `-O2` using GCC 15.2, BH 1.90, fixed compiler seed and normalized source paths: **98/98 object files were byte-identical**. The Bhattacharjee overloads needed distinct names when the types coincide; the Huber fixture was adapted to the alias. `build/scratch/native-seam-exact-final.log` and the two object directories retain the ignored local evidence. Native `just cpp-test` also passed 6/6 at HEAD and after the original seam-only change.

## Initial double-width failures and remedies

The initial full proxy run failed 12/922 distribution cases (57 assertions) and 1/8 CMP cases (30 assertions); other targets passed. Truth and golden files are unchanged. The final source contains these changes:

| Cases | Method and evidence |
|---|---|
| beta, betapr, nsbeta tails | Evaluate the incomplete-beta prefactor in log coordinates and its continued fraction for subnormal input or probability below 1e-290, rather than logging an inaccurate backend result. The original affected mpmath truth rows pass in the targeted run, including the smallest subnormal. This is measured coverage, not a uniform bound. |
| betapr held shape | For identical observations and exactly one held shape, the profiled scale likelihood is increasing in the free shape: `psi(a+b)-psi(b) > log((a+b)/b)`. Return the appropriate gamma face. Both held-face tests passed. |
| dnorm and logitnorm | Use the normal log-tail helper, switching to a 12-term Mills expansion when the backend result becomes subnormal. In this regime its alternating first omitted term bounds truncation; normal interval and logit-normal truth and round-trip tests passed. Logit-normal and shared extreme-tail inversions require a valid bracket and adjacent floating-point endpoints, otherwise returning NaN. |
| dgamma | Switch to the gamma upper-tail expansion before backend subnormal loss, and require a geometric or alternating remainder criterion. On nonconvergence after one million terms, return NaN; a double-width test pins this at shape 1e14 and argument 1e14 + 4e8, where the backend upper probability is zero. The affected truth rows pass; an added high-shape row uses an independent mpmath value. This does not bound floating-point cancellation in the prefactor. |
| gamma, lnorm held parameters | Compute the gamma scale from `sum(x/n)` on total overflow; scale lognormal residuals before squaring. Both existing overflow tests passed. |
| skellam | Divide the large-order exponent before multiplication and evaluate the large order/argument ratio in log coordinates when it overflows. All 1e300 truth rows passed; other extremes have not been exhaustively covered. |
| CMP moments and work limit | Compensate sums, preserve the saddle quotient residual, evaluate normalized geometric bounds in log coordinates, and use `exp((2-mode)*eta)*exp2(-nu)` for the second-count weight where safe. The independent 73-row reference grid passed with worst relative error 9.51e-16. Its certificate controls truncation/quadrature, not roundoff. |
| bhatt golden | Double-width fit: mu `-0.0038000681586891905` vs `-0.0038000682197418395` (relative `1.6066e-8`); sigma `1.0578973465863588` vs `1.057897350270679` (`3.4827e-9`); a `1.7336058647695651` vs `1.7336058787117143` (`8.0423e-9`). Score norm `5.7293e-6` passes the existing `1e-3` gate. A macro-only 5e-8 relative golden tolerance covers this measured case; the native comparison remains unchanged. An independent SciPy total negative log likelihood in `(mu, log sigma, log a)` gives finite-difference Hessian eigenvalues 25.80, 240.10 and 588.26; eight Nelder–Mead starts converge to objective 896.3104598272384–896.3104598272386. These numerical checks support a local maximum but do not prove global optimality. |

## Remaining differences and validation

The Bhattacharjee fit is precision-dependent within the measured tolerance, and the gamma upper-tail helper returns NaN when its bound cannot be certified; see `dev/KNOWN_ISSUES.md`. The beta logit quantile returns NaN when a finite bracket cannot be represented or the bisection cannot certify its endpoint. The continued fraction also returns NaN if it does not converge. The inventory above identifies further unbounded input-domain and search-decision gaps: the test grid does not prove universal platform independence.

Final runs, all through `/tmp/umvl-cpu/run4` (`nice -n 10`, four build slots; BLAS/OpenMP one thread):

| Command | Result |
|---|---|
| `just cpp-test-no-long-double` | 6/6 passed, 136.46 s; includes the gamma failure regression and quantile checks. |
| `just cpp-test` | 6/6 passed, 157.88 s; native goldens retain their original tolerances. |
| `NOT_CRAN=true just test` | FAIL 0, WARN 0, SKIP 1, PASS 39040; optional `sn` is not installed. |
| `just quality` | 98/98 families passed. |

The R and quality runs preceded the last C++-only conditional test addition; library code was unchanged afterward. C++ truth checks normally use log/probability tolerance 1e-10 and quantile tolerance 1e-9, subject to per-family overrides; sampling defaults to 20000 draws with seed `20261029 + point`. These numerical checks are not proofs for untested inputs. The Emscripten preprocessor selection was compile-checked with `-D__EMSCRIPTEN__`, not with an actual WebAssembly toolchain.

Environment: x86-64 Linux, GCC 15.2, BH 1.90, R 4.6.1, Python 3.14.5; `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` set to `build/tmp`. Base revision `1bd8c44e1d7650aacb864a338a0f9be0f11f2f0d`; uncommitted tracked C++/Justfile patch SHA-256 (`git diff --binary -- Justfile univariateML-cpp`) `46eb53bba19250b58f96f9b827b4ac7246ab18f5415abdee7dfa913556dda31e`; untracked precision header SHA-256 `592be80dea12fb6feff33c2bd338853399040616e34fdc54fc7d97adde747a72`. Full command logs and the independent Bhattacharjee probe are under ignored `build/scratch/`.
