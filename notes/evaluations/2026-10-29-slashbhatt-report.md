# slashbhatt implementation report

No commits, git writes, dependency installs, board edits, or other worktree changes.

## Files

New family files (each under `cppunivariate/include/cppunivariate/distributions/`):

- `slash/README.md`, `slash/mle.hpp`, `slash/dpqr.hpp`, `slash/adapter.cpp`, `slash/test.cpp`, `slash/test.R`, `slash/golden.json`, `slash/existence-proof.py`.
- `bhatt/README.md`, `bhatt/mle.hpp`, `bhatt/dpqr.hpp`, `bhatt/adapter.cpp`, `bhatt/test.cpp`, `bhatt/test.R`, `bhatt/golden.json`, `bhatt/escape-study.R`.

Shared files, individually:

- `cppunivariate/include/cppunivariate/cppunivariate.hpp`: two fitter includes.
- `univariateML/CANONICAL_NAMES.md`: two parameter registrations.
- `univariateML/R/code_helpers.R`: continuous reference quantiles and four runtime-checked reference repairs.
- `univariateML/tests/testthat/test_input_checks.R`: held scales for the generic successful Bhattacharjee fit contract.
- `codex-report.md`: this report.

## Mathematics — proved directly

Slash has density `c/sigma * (1-exp(-z^2/2))/z^2`, continuously extended to `c/(2*sigma)` at zero, where `c=1/sqrt(2*pi)`.
The free-scale collapse at multiplicity k is proportional to `sigma^(n-2*k)`.
More than half gives unbounded likelihood; strictly less than half guarantees attainment because all escaping paths give zero likelihood.
For exactly half, collapse at v has likelihood `B_v=c^n*2^(-n/2)*prod_{x!=v}|x-v|^(-2)`.
Every nonzero collapse limit is at most B_v: a displacement of order sigma multiplies B_v by `[2*(1-exp(-t^2/2))/t^2]^(n/2) <= 1`.
For every other location the scale score `sum(1-2*t/expm1(t))` is strictly decreasing in sigma and has one root.
Thus the exact half-mass existence criterion is whether any interior location profile reaches or exceeds every B_v. This is a variational criterion, not a multiplicity-only classification.
If `sum_{x!=v}1/(x-v)` is nonzero, displacement proportional to sigma squared improves over B_v, proving an interior fit (with the other half-mass point, if present, also compared).
A zero reciprocal sum is not sufficient for collapse: `c(rep(0,10),-1,rep(9,9))` has `L(4,3)/B > 1.45605`.
Free-scale multimodality is proved for `c(rep(0,10),-1,rep(10,9))`: `L(-1/625,1/5)/B > 1.00012` and `L(9/2,7/2)/B > 1.06613`. At location zero all finite-scale values are below B, so each side contains an attained finite maximum.
These inequalities were certified with exact rational arithmetic: `1-exp(-t) >= sum_{j=1}^{12}(-1)^(j+1)*t^j/j!`, plus Bernoulli bounds for the negative-location candidate. Reproducer: `python3 cppunivariate/include/cppunivariate/distributions/slash/existence-proof.py` (five exact-rational assertions).
Location lies in the sample range; fixed positive sigma removes collapse and guarantees attainment. Symmetry and a quantile width identify both parameters.
Multimodality is proved for held sigma: at sigma 1 on `c(-100,100)`, both observation neighbourhoods have higher likelihood than the midpoint; symmetry forces separated maxima.

Bhattacharjee identification: mean is mu, fourth cumulant is `-2*a^4/15`, variance is `sigma^2+a^2/3`.
Density bounds `f <= 1/(2*a)` and `f <= 1/(sqrt(2*pi)*sigma)` rule out any nonzero likelihood limit with either scale diverging, including joint divergence.
Escaping location with bounded scales gives zero likelihood. Joint scale collapse gives unbounded likelihood only for constant samples.
For nonconstant samples the nonzero boundary models are normal (a -> 0) and uniform (sigma -> 0).
Normal face: sample mean and ML residual standard deviation, respecting a held sigma.
Uniform face with a free: midrange and half-range; expand the endpoints by an amount vanishing more slowly than sigma to recover full endpoint density.
Uniform face with a held: zero likelihood if range > 2a, `(2a)^(-n)` if range < 2a; at equality multiply by `(k/(k+l))^k*(l/(k+l))^l` for endpoint multiplicities k,l.
With a held and sigma free, range <= 2a always escapes to uniform: the pointwise density bound, and complementary endpoint bounds at equality, dominate every interior likelihood. Range > 2a guarantees an attained fit.
For sample half-range r, endpoint interval masses are bounded by complementary shifted normal CDFs. Log concavity of Phi gives their product at most `Phi((a-r)/sigma)^2`. Together with the normal and uniform peak-density bounds this yields rigorous likelihood upper bounds before location profiling. Candidates screened below the best exact face cannot be an interior winner; final face comparison always replaces screened proxy values.
The unrestricted uniform face is always locally maximizing on the likelihood closure: for sample half-range r, `a>=r` is excluded by the peak bound, and `r*4^(-1/n)<=a<r` by the extreme-pair bound. It need not be global: the normal candidate on `c(-10,rep(0,20),10)` exceeds the uniform face. This proves boundary multimodality; multiple finite scale modes are not analytically classified.
For fixed positive scales, strict log concavity gives a unique location maximum. The normal-uniform convolution identity gives the stable normal interval density and integrated normal survival CDF used by the repairs.

## Mathematics — checked numerically

Both fitters compare boundary log likelihoods with tolerance `1e-8*n`.
Slash profiles scale and refines all grid-local location maxima, including observation gaps and logarithmic neighbourhoods of half-mass points.
Bhattacharjee profiles the location score and searches both log scales from a 4-by-4 start grid (reduced for held coordinates).
The independent R oracles use 21 varied slash samples, 10 varied Bhattacharjee samples, and two conditional Bhattacharjee searches; numerical integrals independently check both CDFs.
Ten additional n=1000 uniform samples (R seeds 1:10), searched from endpoint-gap starts, all agreed with the fitter; the maximum oracle log-likelihood improvement was zero.
Oracle densities implement the documented reference formulas in base R using expm1 and log normal probabilities, so raw extraDistr cancellation cannot produce spurious oracle maxima. A held-scale Bhattacharjee likelihood is also compared directly with extraDistr::dbhatt at moderate arguments.
Only sigma is fixable for slash; both sigma and a are fixable for Bhattacharjee. Holding mu is explicitly unsupported by the generated unused-argument check.
The small-a/sigma location approximation uses the normal mean below ratio `1e-7`; its omitted location correction is fourth order in that ratio.
Convolution evaluation uses the normal limit when `b*(1+abs(z)) < 1e-8`, where the log-density/CDF correction is at double rounding scale; unresolvable far-tail intervals retain their finite normal log leading term.
Bhattacharjee evaluation preserves the reference normal limit at a=0, while fitting keeps a positive. Near-one log probabilities use log1p and are tested by relative error down to input log probability -1e-100.
Quantile inversion checks representable support edges before solving: unrepresentable slash quantiles return infinity, while scale `1e-300` with upper log probability -1000 gives a finite quantile.

## Mathematics — assumed / not proved

Completeness of the nominated grid modes and simplex basins is a numerical assumption, supported by the independent oracles, not a certified global theorem.
The implementation does not claim an exhaustive analytic classification of Bhattacharjee scale modes or a simpler closed slash half-mass sample condition.

## Validation

`just cpp-test`: 5/5 ctest targets; 350 doctest cases and 517375 assertions passed (distribution target: 332 cases, 506475 assertions). New families add 6 C++ cases and 57 assertions, including finite-difference score gates.
`just test`: FAIL 0 / WARN 0 / SKIP 1 / PASS 6767 (the existing missing-{sn} skip).
`just quality slash bhatt`: 2/2 passed (symbolic log likelihood, regular score/information, independent eight-start oracle).
`UMVL_GOLDEN_IDS="slash bhatt" just goldens`: 2 goldens written; all pre-existing golden files unchanged. The performance optimization moved the new Bhattacharjee golden estimates by about 3e-8 and log likelihood by 1e-13; slash was unchanged.
`slash/existence-proof.py`: all 5 exact-rational assertions passed. The resumable `bhatt/escape-study.R` reproducer was smoke-tested on 3 seeds/cases, matching the study records.
Earlier R failures were corrected: first pass FAIL 2 / WARN 0 / SKIP 1 / PASS 6702 (normal-quantile fixture and a named expected scalar); second pass FAIL 1 / WARN 0 / SKIP 1 / PASS 6747 (endpoint interval refinement in held-scale slash fitting).
The 100-point uniform-face timing fixture took 105.28 s before exact bound screening and 0.805 s afterwards, with identical face parameters and log likelihood.
Artifact display was attempted, but the viewer returned 403 because this worktree is outside its registered readable roots; no viewer configuration was changed.
All long computations used `/tmp/umvl-cpu/run`, one thread per worker, no parallel builds. Scratch logs, oracle code and study CSVs are in `/tmp/umvl-codex/slashbhatt/`.

## Bhattacharjee escape catalogue

100 independent samples per cell; entries are counts (also percentages). All 900 distinct samples completed with zero fitting failures. Counts use the fitter's `1e-8*n` face preference; worst-case Monte Carlo standard error is 0.05.
Generators: normal N(0,1); uniform on [-sqrt(3),sqrt(3)]; intermediate normal(0,1) plus uniform on [-2,2]. C++ `StdRng<>`/mt19937_64 seeds are `20261029+n*1000+model*100+rep`, model=0/1/2 respectively and rep=0:99. Reproduce with `bhatt/escape-study.R` (standard installed Rcpp/BH only).

| Generator | n | Normal face | Uniform face | Interior |
|---|---:|---:|---:|---:|
| Normal | 20 | 21 | 73 | 6 |
| Uniform | 20 | 1 | 99 | 0 |
| Intermediate | 20 | 12 | 77 | 11 |
| Normal | 100 | 40 | 1 | 59 |
| Uniform | 100 | 0 | 100 | 0 |
| Intermediate | 100 | 6 | 3 | 91 |
| Normal | 1000 | 46 | 0 | 54 |
| Uniform | 1000 | 0 | 100 | 0 |
| Intermediate | 1000 | 0 | 0 | 100 |

All 168 pre-optimization sample statuses agreed with the optimized run. Eleven overlapping worker repeats were identical, and were counted once.

## Ready-to-paste KNOWN_ISSUES rows

| Package | Defect | Reproducer | Effect here |
|---|---|---|---|
| extraDistr 1.10.0.5 | `dslash` cancels near its centre and overflows its squared standardized argument in extreme tails | `extraDistr::dslash(c(1e-10,1e200), log=TRUE)` gives two `-Inf` values, should be about -1.612086 and -921.952976 | runtime-checked stable exponential difference and log-tail density |
| extraDistr 1.10.0.5 | `pslash` has a doubled numerical CDF slope near its centre | `extraDistr::pslash(1e-10)-.5` is about 3.98942e-11, should be 1.99471e-11 | same runtime-checked stable slash CDF repair |
| extraDistr 1.10.0.5 | `pslash` loses upper and lower tails by cancellation | `extraDistr::pslash(1e20, lower.tail=FALSE, log.p=TRUE)` is `-Inf`, should be about -46.97064 | runtime-checked direct smaller-tail evaluation; continuous inverse uses log probabilities |
| extraDistr 1.10.0.5 | `dbhatt` subtracts rounded normal CDFs, then logs an underflowed density | `extraDistr::dbhatt(10, log=TRUE)` is `-Inf`, should be -44.3212963 | runtime-checked log normal interval probabilities |
| extraDistr 1.10.0.5 | `pbhatt` loses normal convolution tails, sometimes returning the wrong endpoint | `extraDistr::pbhatt(c(-10,10), lower.tail=FALSE, log.p=TRUE)` gives 0 and `-Inf`, rather than tiny negative and -46.5420885 | runtime-checked integrated normal survival probabilities |
| extraDistr 1.10.0.5 documentation | Bhattacharjee help displays a CDF that is zero at mu, and describes an uncentred Uniform(0,1) convolution | `extraDistr::pbhatt(0)` is 0.5, whereas the displayed formula is zero | documentation defect; runtime CDF is correct centrally |

The references have no quantile functions. Both are registered with the continuous reference inverse; future upstream quantile exports take precedence automatically.
The inverse recycles parameters and probabilities, handles endpoints exactly, switches to the smaller tail using `expm1`, expands brackets in signed log-distance coordinates, and uses safeguarded bisection.

## Sources and open questions

Density parameterizations: [extraDistr Slash manual](https://search.r-project.org/CRAN/refmans/extraDistr/html/Slash.html) and [extraDistr Bhattacharjee manual](https://search.r-project.org/CRAN/refmans/extraDistr/html/Bhattacharjee.html).
Five web searches used; no external likelihood theorem was relied upon. Secondary search results disagree about Kafadar dates and were not used as theorem evidence.
Existing `mlnorm`/`mlunif` alternatives cannot preserve held sigma/a; conditional-face error messages explicitly say the suggested alternative is unrestricted.
Open mathematical question: a simpler closed sample criterion for the slash half-mass case (the exact profile criterion above is implemented numerically).
Performance follow-up: regular n=1000 global fits can still take tens of seconds on this workstation.
Open numerical question: certified exclusion of arbitrarily narrow location/shape modes beyond the current search grids.
