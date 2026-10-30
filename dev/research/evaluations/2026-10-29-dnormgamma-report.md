# dnorm / dgamma implementation report

Branch: `codex/dnormgamma`. No commits or Git-writing commands run.

## Files

New family files (each under `univariateML-cpp/include/cppunivariate/distributions/`):

- `dnorm/README.md`: model, metadata, fixable parameters, limits, existence proof.
- `dnorm/mle.hpp`: stable cell likelihood, analytic damped Newton, held subsets.
- `dnorm/dpqr.hpp`: stable scalar d/p/q/r.
- `dnorm/adapter.cpp`: R fitting adapter, statuses and fixed names.
- `dnorm/test.cpp`: golden, score, boundaries, tails, held subsets and quantiles.
- `dnorm/test.R`: reference oracles, held subsets, repairs, quantiles and RNG.
- `dnorm/golden.json`: new 500-observation reference-seeded regression sample.
- `dgamma/README.md`: model, metadata, limits, identification and existence proof.
- `dgamma/mle.hpp`: exact inner log-rate profile; shape grid with local refinement.
- `dgamma/dpqr.hpp`: stable scalar d/p/q/r.
- `dgamma/adapter.cpp`: R fitting adapter, statuses and fixed names.
- `dgamma/test.cpp`: golden, score, boundaries, profile and quantiles.
- `dgamma/test.R`: reference oracles, held subsets, repairs, quantiles and RNG.
- `dgamma/spec.py`: independent positive-series symbolic quality oracle, remainder bound.
- `dgamma/golden.json`: new 500-observation reference-seeded regression sample.

Shared files, listed separately:

- `univariateML-cpp/include/cppunivariate/cppunivariate.hpp`: two includes.
- `bindings/r/CANONICAL_NAMES.md`: two rows; extraDistr coefficient names.
- `bindings/r/R/code_helpers.R`: runtime-checked density repairs; two missing-q
  registrations; extend integer inversion to a support unbounded below.
- `codex-report.md`: this report.

## Mathematics: proved (arguments in the family READMEs)

- Both laws are floors: `P(X=k)=F(k+1)-F(k)`. The reference C++ sources confirm it.
- Identifiability: normal tail quadratic/linear terms identify sd/mean;
  gamma tail exponential/power terms identify rate/shape.
- Both unrestricted MLEs exist iff sample range is at least two. Constants
  approach point masses; adjacent cells approach the saturated two-point law,
  strictly beating every finite fit. The error recommends shifted `mlbinom`.
- Normal adjacent path: `mean=k+1-sd*qnorm(p)`, lower-cell fraction p.
  Gamma path: `m=k+1-s*qnorm(p)`, `shape=m^2/s^2`, `rate=m/s^2`.
  Seven 3s and three 4s give supremum `-6.108643020548935`.
- Normal converse: escaping location or infinite scale loses fixed cells;
  zero scale retains only cells with a common closure point. Positive level
  sets are compact for range >= 2. Burridge concavity gives a unique MLE.
- Gamma converse: for k >= 1, uniformly over rate,
  `p_k <= a^a*exp(-a)/Gamma(a)*log(1+1/k) -> 0` as shape a -> 0.
  Bounded positive shape with rate escaping loses a positive observed cell.
  Shape -> infinity concentrates relative to its mean; bounded means can
  retain only one or two adjacent cells, unbounded means lose fixed cells.
  Thus positive likelihood level sets are compact for range >= 2.
- No additional proper small-shape count limit retains a positive cell;
  mass at zero plus mass escaping to infinity cannot maximize such samples.
- Held normal sd: finite unique location fit for every nonempty sample.
  Held normal mean: zero-scale escape iff every observed cell closure
  contains it; otherwise attainment. Both held: evaluate, including constants.
- Held gamma shape or rate: attainment iff a positive observation is present;
  all-zero samples escape. Both held: evaluate, including all-zero samples.
- Gamma log-rate with shape held is a concave grouped location likelihood
  for log(Y); its unique score root supplies the exact inner profile.
- Discrete log likelihoods are bounded above by zero: no unbounded likelihood.
  Faces are classified exactly; there are no finite competing limit faces
  requiring approximate `1e-8*n` likelihood comparisons.

## Mathematics: checked numerically

- Quality: both symbolic log likelihoods, regular scores, observed Hessians and
  eight-start scipy searches pass. Normal standardized score 2.05e-11;
  gamma 2.85e-8. Oracle log-likelihood gaps 1.14e-13 and 1.02e-12.
- Independent R checks cover ten varied samples with nine starts each and
  eleven stress samples with twenty starts each: 310 R optimizer starts.
  Stress samples include zero-heavy data, outliers, shape .05 and shape 1000;
  largest absolute stress-oracle gap 5.2e-13. Twelve independent scalar oracles
  also check held subsets. These are numerical evidence, not gamma uniqueness.
- Normal concavity uses Burridge (1981), [doi:10.1111/j.2517-6161.1981.tb01146.x](https://doi.org/10.1111/j.2517-6161.1981.tb01146.x).
- Reference sources: [normal](https://github.com/twolodzko/extraDistr/blob/master/src/discrete-normal-distribution.cpp),
  [gamma](https://github.com/twolodzko/extraDistr/blob/master/src/discrete-gamma-distribution.cpp).
  The existing reference CDFs already floor q and retain stable upper log tails;
  no CDF or RNG repair was needed. Web searches used: 2 of the budget of 6.

## Mathematics: assumed / unproved

- Returning a gamma fit relies on numerical coverage of the shape profile.
  The 41-point scan, improving-edge expansion and local refinements are not
  certified to find arbitrarily narrow or distant modes. Uniqueness is unproved.
- Normal concavity and the gamma CLT are used as established external theorems;
  the compactness and escape arguments above are separate from numerical checks.

## Validation

- Scoped `UMVL_GOLDEN_IDS="dnorm dgamma" just goldens`: 2/2 generated;
  existing goldens unchanged.
- Final `just cpp-test`: 5/5 ctest targets; 351 cases and 517,386 assertions,
  all passing (distribution target: 333 cases, 506,486 assertions).
- Final `just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 6749 ]`.
  Standalone family tests: 8 blocks, 188 passing expectations (94 per family).
- All eleven stress samples passed again against the rebuilt final package.
- `just quality dnorm dgamma`: 2/2, with no skipped derivative checks.
- Earlier setup runs hit missing new goldens, an overlapping package assembly
  I caused while rearranging the queue, and incorrect test API calls; corrected
  and rerun sequentially. These were not accepted as validation results.
- Count magnitudes are restricted below 2^52 to preserve half-cell centring;
  extremely narrow cells use normal Taylor / gamma midpoint evaluations.

## Escape frequencies (catalogue item)

10,000 independent samples per row, seed 20261029. Entries are counts
`constant / adjacent / finite`; divide by 100 for percentages. Both unrestricted
fitters use the same range criterion (gamma requires nonnegative observations).

| Generating model | n=20 | n=100 | n=1000 |
|---|---|---|---|
| floor N(0,1) | 0 / 7 / 9993 | 0 / 0 / 10000 | 0 / 0 / 10000 |
| floor N(4,.2^2) | 0 / 10000 / 0 | 0 / 9999 / 1 | 0 / 9990 / 10 |
| floor Gamma(shape=2,rate=1) | 0 / 0 / 10000 | 0 / 0 / 10000 | 0 / 0 / 10000 |
| floor Gamma(shape=400,rate=100) | 0 / 9999 / 1 | 0 / 9997 / 3 | 0 / 9985 / 15 |
| round N(3.5,.2^2) | 0 / 10000 / 0 | 0 / 9999 / 1 | 0 / 9995 / 5 |
| Poisson(.1) | 1295 / 7811 / 894 | 1 / 6252 / 3747 | 0 / 90 / 9910 |

Reproducer and CSV: `/tmp/umvl-codex/dnormgamma/sweep.R`, `escape-frequency.csv`.
The gamma adjacent path at s=.1,.03,.01,.003 gives lower-cell probabilities
.7021281,.7006326,.7002103,.7000630, converging to .7.

## Ready-to-paste KNOWN_ISSUES rows

| extraDistr 1.10.0.5 | `ddnorm` ordinary-CDF subtraction loses upper tails and underflows before taking logs in both tails. | `extraDistr::ddnorm(c(-40,10), log=TRUE)` returns `c(-Inf,-Inf)`, although both log probabilities are finite. | Runtime `broken()` check; log-CDF difference on the smaller-tail side. |
| extraDistr 1.10.0.5 | `ddgamma` ordinary-CDF subtraction cancels upper tails and underflows lower tails before taking logs. | `extraDistr::ddgamma(c(0,1000),c(1000,2),rate=c(1e-8,1),log=TRUE)` returns two `-Inf` values although both logarithms are finite. | Runtime `broken()` check; log-CDF difference on the smaller-tail side. |

## Open questions

- Can gamma's shape profile be proved unimodal, or can a certified global
  search replace the grid? Current existence proofs do not settle uniqueness.
