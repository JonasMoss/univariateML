# halfdists review report

Implemented `mlhcauchy`, `mlht`, and an exact held endpoint for `mlhnorm`.
No commits or Git writes. Existing goldens, including hnorm, are unchanged.

## Files changed

New `cppunivariate/include/cppunivariate/distributions/hcauchy/`:
README.md, mle.hpp, dpqr.hpp, adapter.cpp, test.cpp, test.R, golden.json.
New `cppunivariate/include/cppunivariate/distributions/ht/`:
README.md, mle.hpp, dpqr.hpp, adapter.cpp, test.cpp, test.R, golden.json.
Changed `cppunivariate/include/cppunivariate/distributions/hnorm/`:
README.md, mle.hpp, adapter.cpp, test.cpp, test.R; dpqr.hpp/golden.json unchanged.
Shared files (each listed separately):
- `cppunivariate/include/cppunivariate/cppunivariate.hpp`: two includes.
- `cppunivariate/include/cppunivariate/distributions/README_SCHEMA.md`:
  document the already-supported combination `bound: lower` + `fixable: true`.
- `univariateML/CANONICAL_NAMES.md`: two family rows.
- `univariateML/R/code_helpers.R`: runtime-checked reference repairs;
  held-endpoint hnorm dispatches shifted extraDistr half-normal d/p/q/r.
- `univariateML/tests/testthat/test_input_checks.R`: hold nu=3 in the
  generic successful-fit contract, avoiding a small-sample normal escape.
- `codex-report.md`: this report.

## Mathematics: proved

- Finite half-t parameters are identified by the tail exponent and mode density;
  half-Cauchy scale is identified by its mode density.
- Half-Cauchy log-scale score is n - sum(2*sigma^2/(sigma^2+x^2)).
  With k zeros and at least one positive observation it strictly decreases
  from n-2k to -n. Thus a finite unique MLE exists iff 2k<n.
  At equality it is negative everywhere: the finite scale-zero supremum
  is unattained. For 2k>n (also all-zero data) likelihood is unbounded.
  Single positive and constant positive samples are fittable.
- Held-df half-t log-scale score decreases strictly, with limits
  nu*(n-k)-k and -n. The unique scale root exists iff nu*(n-k)>k;
  equality gives a finite unattained supremum, strict reverse inequality
  unboundedness. Any zero makes the both-free model unbounded: choose
  0<nu<k/(n-k) and let scale tend to zero; all-zero data collapse for any nu.
- Held scale removes the zero-collapse path; all fixed subsets are implemented.
  With both parameters held there is only likelihood evaluation.
- For x>0 the individual density maximum over scale occurs at sigma=x,
  bounding f(x) by B(nu)/x, B(nu)=2*t_nu(0)*(1+1/nu)^(-(nu+1)/2).
  B(nu) tends to zero with nu, excluding every nu->0 escape on positive data.
  It increases in nu: the kernel factor does by log(1+u)<u; the mode does
  since digamma((nu+1)/2)-digamma(nu/2)>1/nu (integral representation).
  Hence the lower-grid bound applies to all smaller df, not only its endpoint.
  With held scale, densities at zero also tend to zero.
- nu->infinity is the half-normal face. Compare its exact RMS scale,
  or the held scale, with finite candidates; face wins ties within 1e-8*n.
  Error names `mlhnorm(x, mu = 0)`. With sigma held it explicitly states
  that the suggested call estimates sigma; on all-zero data that free-scale
  half-normal fit itself has no MLE.
- Held-mu half-normal scale is RMS(x-mu); below-endpoint observations fail,
  all-at-endpoint samples have unbounded likelihood as sigma->0.
  Single observations above a held endpoint are allowed. The default
  translated fitter and its one-ULP endpoint convention remain unchanged.
- A freely translated half-t with unrestricted positive df is unbounded:
  put the endpoint at the minimum and use the same zero-collapse argument.

## Mathematics: checked numerically

- A 1,200-sample scan (seed 8461; n=3..40; log-spread and tied clusters),
  401 log-df grid points per sample, found 32 profiles with multiple local
  maxima; 1,003 finite fits and 197 half-normal escapes, no other statuses.
- The five-point example in ht/README.md has two independently verified
  maxima: nu=.41508, logLik=-11.72979; nu=1.17074, logLik=-11.98253;
  the half-normal face has logLik=-12.59859.
- Independent multistart R optim on extraDistr density checks varied samples,
  including a multiple-peak sample; held-df scale uses R optimize independently.
- Symbolic quality checks and goldens: final counts below.

## Assumptions / numerical limits

No unimodality assumption. Log-df grid spacing <=0.125 refines every sampled
local maximum after monotone log-scale profiling. This is a numerical global
search, not a proof against arbitrarily narrow missed peaks. The large-df
search ends at 1e8; numerical ties select the exact normal face, an unresolved
winning edge errors. Nonrepresentable positive scales error rather than fit.
Tail repairs use stable folded t/Cauchy/normal probabilities and log kernels;
central tiny-tail approximations retain double-precision accuracy in tested cases.

## Validation

- `/tmp/umvl-cpu/run just cpp-test`: 5/5 ctest targets passed; 351 doctest
  cases (333 distribution, 6 BFGS, 1 sampling, 3 no-throw, 8 CMP).
  Seven distribution cases are new in this job.
- `/tmp/umvl-cpu/run just test`: FAIL 0, WARN 0, SKIP 1, PASS 6683.
  The skip is the baseline optional `{sn}` reference package.
- `/tmp/umvl-cpu/run just quality`: 81/81 passed, including hcauchy/ht/hnorm.
- `/tmp/umvl-cpu/run env UMVL_GOLDEN_IDS="hcauchy ht" just goldens`:
  exactly two new 500-observation goldens; no existing golden changed.
- Generated package adapters, headers, helpers and ht tests match final sources.
- `git diff --check`: clean.
- Logs and numerical scan sources: `/tmp/umvl-codex/halfdists/`.
- An extra standalone vectorization probe was canceled while waiting for a
  shared CPU slot; required reference tails and seeded RNG passed in testthat.

## Ready-to-paste KNOWN_ISSUES.md rows

| extraDistr 1.10.0.5 | `dhcauchy(log=TRUE)` overflows its squared ratio | `extraDistr::dhcauchy(1e200, log=TRUE)` returns -Inf; should be about -921.48562 | runtime log-kernel repair for half-Cauchy `dml` |
| extraDistr 1.10.0.5 | `dht(log=TRUE)` overflows the observation/scale ratio | `extraDistr::dht(1e200, 3, 1e-200, log=TRUE)` returns -Inf although its log density is finite | runtime log-ratio repair for half-t `dml` |
| extraDistr 1.10.0.5 | `phcauchy` loses extreme upper tails | `extraDistr::phcauchy(1e20, lower.tail=FALSE, log.p=TRUE)` returns -Inf; should be about -46.50328 | runtime folded-Cauchy tail repair |
| extraDistr 1.10.0.5 | `pht` loses extreme upper tails | `extraDistr::pht(1e20, 3, lower.tail=FALSE, log.p=TRUE)` returns -Inf; should be about -137.36423 | runtime folded-t tail repair |
| extraDistr 1.10.0.5 | `phnorm` loses extreme upper tails | `extraDistr::phnorm(40, lower.tail=FALSE, log.p=TRUE)` returns -Inf; should be about -803.91529 | runtime folded-normal tail repair for held-mu hnorm |
| extraDistr 1.10.0.5 | `qhcauchy` rounds extreme upper probabilities away | `extraDistr::qhcauchy(-100, lower.tail=FALSE, log.p=TRUE)` returns about 1.63e16 instead of 1.71e43 | runtime folded-Cauchy quantile repair |
| extraDistr 1.10.0.5 | `qht` rounds extreme upper probabilities away | `extraDistr::qht(-100, 3, lower.tail=FALSE, log.p=TRUE)` returns Inf instead of about 3.90e14 | runtime folded-t quantile repair |
| extraDistr 1.10.0.5 | `qhnorm` rounds extreme upper probabilities away | `extraDistr::qhnorm(-100, lower.tail=FALSE, log.p=TRUE)` returns Inf instead of about 13.938 | runtime folded-normal quantile repair for held-mu hnorm |

## sstd and open questions

No sstd changes. Its half-t faces have a translated endpoint and nu>=2,
whereas mlht has endpoint zero and unrestricted positive nu. A direct
`use mlht(x)` is not a correct replacement. Subtracting the minimum (or
reflecting about the maximum) creates zeros, so free-df mlht then correctly
rejects that sample as unbounded. A scale-only fit with the face df held,
on the transformed data, is appropriate when nu*(n-k)>k; it does not solve
the same constrained df search as sstd. No automatic pointer was added.

Reviewer decision: whether a later release should add a df lower bound
for a translated half-t is outside this task. No mathematical claim of
certified global optimality is made for the finite-grid df search.
