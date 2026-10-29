# sgomp implementation report

Status: complete; C++, R, oracle, golden and full quality checks pass.
No commits or writing git commands. No board edits or sub-agents.

## Files

New `cppunivariate/include/cppunivariate/distributions/sgomp/`:
`README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `test.cpp`,
`test.R`, `golden.json` (500 draws, seed 12895).
Root report: `codex-report.md`.
Shared file: `cppunivariate/include/cppunivariate/cppunivariate.hpp`
(one include registration). Generated package/build artifacts are ignored.
No other distributions' goldens changed.

## Domain and behaviour

Rate b > 0; shape eta >= 0. The eta = 0 exponential is an attained member,
so there is no D1 exponential-limit error or Gumbel escape declaration.
Both parameters can be held fixed. Shape-zero candidates are explicitly
compared, preferring the face within 1e-8*n log-likelihood units.
Zeros are valid data; constant samples with both parameters free, and
all-zero samples with b free, report unbounded likelihood.
Invalid input and numerical/iteration failures have separate status enums.

## Mathematics: proved

Writing v_i = exp(-b*x_i), a_i = 1-v_i gives
ell = n*log(b)-b*sum(x)-eta*sum(v)+sum(log1p(eta*a)).
At fixed b the eta score is -sum(v)+sum(a/(1+eta*a)); its derivative is
-sum(a^2/(1+eta*a)^2). Thus eta=0 is optimal iff n-2*sum(v)<=0;
otherwise there is a unique positive root, bounded above by n/sum(v).
All-zero data have their unique shape optimum at zero.

Scale observations by max(x). For normalized data y in [0,1], b<=1
implies sum(v)>=n/e, hence ell <= n*log(b)+n after maximizing over eta.
For all b, ell <= n*log(b)+n*log(n)-b*(sum(y)-n*min(y)).
For fixed eta the alternative upper bound is
ell <= n*log(b)+n*log1p(eta)-b*sum(y).
These give finite low/high rate exclusions for nonconstant samples;
the high bound decreases after n/(sum(y)-n*min(y)) (or n/sum(y)).
Strictly positive constant data collapse along eta=exp(b*x), b->infinity,
with f(x) asymptotic to b/e. All-zero data collapse as b->infinity.
With eta fixed and some positive data, both rate ends lose likelihood.
With b fixed, the shape optimum is finite, including constant samples.

The direct survival decomposition is
S = v + (1-v)*(1-exp(-eta*v)). Log-sum-exp and expm1 preserve its tails.
The CDF is (1-v)*exp(-eta*v). Quantiles bisect log(b*x), selecting the
smaller tail without forming 1-F. Sampling is the maximum of independent
Exp(b) and Gumbel(log(eta)/b, 1/b), with an exponential-only eta=0 branch.

## Mathematics: checked numerically

Ordinary d/p values agree with extraDistr on six parameter combinations.
Log tails and quantiles were checked at tiny x, log probabilities down to
-1000, upper log probability -1e300, and eta=1e308 with lower log p=-1e300.
The exponential member agrees with stats; fixed-coordinate fits agree with
scalar optimization. R RNG reproducibility and the max construction pass.
Fitted d/p/q/r, plot, bootstrap with held parameters, and model_select pass.
An independent direct-formula, five-start Nelder-Mead oracle on 60 samples
(three rates, four shapes, five replicates, some including zero) differs
by at most 1.232081e-11 log-likelihood units. Quality independently checks
the symbolic likelihood (difference -1.82e-12), score/observed Hessian
(standardized score 3.77e-7), and an eight-start scipy oracle.

## Mathematics: assumed / limitations

The finite interval is justified analytically. A log-rate grid with spacing
at most 1/16 brackets local maxima, each refined by golden-section search.
There is no proof that an arbitrarily narrow extra maximum cannot be missed;
no joint uniqueness claim or certified stationary-point enumeration is made.
No nonpositive inward-score shortcut is used as a global existence test.
Parameters/likelihoods outside finite double representation report failure;
near-collapse samples can also exhaust scalar-search precision.

## Exact validation counts

- `just cpp-test`: 5/5 ctest targets, 311 cases, 506189 assertions passed.
- sgomp-only C++: 4/4 cases, 55/55 assertions passed.
- First R run: 4927 passes, 1 failure (support-error wording in a test);
  expectation corrected.
- Final `just test`: 4931 passes, 0 failures, 0 warnings, 0 skips.
- Independent oracle probe: 60/60 samples passed.
- `just quality sgomp`: 1/1; `just quality`: 72/72 passed.
- `UMVL_GOLDEN_IDS=sgomp just goldens`: exactly 1 new golden.
All builds/tests/computations used `/tmp/umvl-cpu/run`; no parallel make.
Logs and probe scripts: /tmp/umvl-codex/sgomp/.

## Open questions

No policy decision required: this implementation deliberately includes eta=0.
A certified global root enumeration would require further analysis beyond
the bounded grid/refinement search and independent oracle validation.

Artifact viewer: opening the report was attempted, but its configured readable
roots exclude this worktree (HTTP 403). The report remains in the worktree.
