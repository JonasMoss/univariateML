# Job zetays

Implemented `mlzeta` and `mlyulesimon`, plus package-defined `d/p/q/r` for
both families. No commits, Git writes, board edits, installs or sub-agents.
All builds, tests and substantial probes use `/tmp/umvl-cpu/run`.

## Files changed

- New `cppunivariate/include/cppunivariate/distributions/zeta/`:
  `README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `test.cpp`,
  `test.R`, `golden.json`, `spec.py`.
- New `cppunivariate/include/cppunivariate/distributions/yulesimon/`:
  the same files except `spec.py`.
- Shared: `cppunivariate/include/cppunivariate/cppunivariate.hpp`
  (two aggregate includes).
- Shared: `cppunivariate/include/cppunivariate/special/special.hpp`
  (Boost zeta include and no-throw wrapper).
- Existing family: `distributions/zipf/README.md` (infinite-support face).
- This report. Generated R package/build artifacts remain ignored.

## Mathematics proved

- Zeta score: `n(E_s log X - mean(log x))`; its derivative is
  `-n Var_s(log X) < 0`. The model mean decreases continuously from infinity
  at `s -> 1+` to zero at infinity. Thus every sample containing a value
  above one has a unique finite MLE. All ones have an unattained point-mass
  supremum at one. At `s -> 1+` the finite-sample likelihood tends to zero;
  this endpoint never supplies the supremum.
- Yule--Simon score in `eta=log(rho)`:
  `n - sum_i sum_{j=1}^{x_i} rho/(rho+j)`, strictly decreasing from `n`
  to `n-sum(x)`. This gives the same finite-MLE/all-ones distinction.
  At `rho -> 0+` the likelihood tends to zero and mass escapes to infinity,
  with no proper probability limit.
- Yule--Simon survival: `Gamma(k+1) Gamma(rho+1)/Gamma(k+rho+1)`.
  It equals beta negative binomial with size=1, alpha=rho, beta=1,
  shifted by one.
- For fixed `s>1`, finite Zipf converges to zeta as N tends to infinity
  by convergence of the positive normalizing series.

## Implementation and numerical checks

Both fitters bracket and bisect their strictly monotone scalar scores.
Zeta extends Zipf's 1024-term Euler--Maclaurin log moments to infinity;
normalization uses Boost zeta (log1p series for large shape). Direct scaled
Euler--Maclaurin tails and finite partial sums avoid cancellation near s=1.
Yule--Simon caches distinct counts, uses digamma score differences, and
reuses beta-NB's stable long-double log-gamma increment for direct log tails.
Small-count recurrences, a local digamma expansion for tiny gamma increments,
and a small-shape log-gamma expansion preserve
extreme shapes. Inversion compares the requested probability scale/tail.
R wrappers follow shared recycling, missing values, warnings and RNG policy.

- `UMVL_GOLDEN_IDS="zeta yulesimon" just goldens`: exactly two new
  500-observation goldens; existing goldens unchanged.
- Final `just test`: 5,016 passed expectations, zero failures/warnings/skips.
  Targeted new R files: 154 expectations (76 zeta, 78 Yule--Simon), all pass.
  Seeded 10,000-draw chi-square checks pass for each family; model generics,
  bootstrap and plotting pass. Independent scalar fit oracles cover four
  samples per family, including constant, sparse and billion-count data.
- `just quality zeta yulesimon`: 2/2; full `just quality`: 73/73.
  Both new scores/Hessians and eight-start likelihood oracles pass.
- Final `just cpp-test`: 5/5 ctest targets, zero failures, including the
  added extreme-tail regression. Earlier targeted new cases: 4/4 cases,
  94/94 assertions; the final suite also includes two added regression
  assertions in a fifth new case.
- Independent 750-digit mpmath probe: 96 zeta and 132 Yule--Simon grid points,
  456 density/survival log comparisons, all pass. Maximum relative log-tail
  errors: 2.61e-13 and 3.29e-15 respectively. Grid includes shapes from
  1+1e-12 to 1000 (zeta), 1e-300 to 1e308 (Yule--Simon), counts to 1e300.
- Checked eight exported probability-function names: no base/stats clashes.
- The first R run had 5,015 passes and one failing extreme-tail regression.
  The mpmath probe exposed cancellation in the reused gamma increment;
  the local digamma expansion fixed it. The final R/probe runs above pass.
- `git diff --check` passes. Scratch logs/scripts: `/tmp/umvl-codex/zetays/`.

## Assumptions and limits

- Euler--Maclaurin truncation and the reused gamma-increment expansion are
  numerical approximations; no new uniform floating-point error theorem
  over every finite double parameter is claimed.
- Zeta's quality `spec.py` uses a separate 64-term/four-correction symbolic
  approximation because the harness cannot lambdify zeta derivatives.
  Its agreement with the Boost-normalized likelihood is checked numerically.
- Above 2^53, quantiles use the representable double grid. Adjacent integers
  cannot be distinguished; quantiles above DBL_MAX return Inf. This also
  applies to inverse-transform random draws in extremely heavy tails.
- Fitter input follows the existing integer-count R/C++ interface.

## Open questions

None requiring an author decision. Reviewer-owned NEWS/behaviour notes and
board updates were deliberately left untouched.
