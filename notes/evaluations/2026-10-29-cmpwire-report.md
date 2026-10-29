# cmpwire report

No commits or Git write operations. No board edits, sub-agents or dependency installs.

## Files

- New `cppunivariate/include/cppunivariate/distributions/cmp/`:
  `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `README.md`,
  `test.R`, `test.cpp`, `reference.py`, `reference.inc`, `spec.py`, `golden.json`.
- Shared file: `cppunivariate/include/cppunivariate/cppunivariate.hpp`:
  one CMP include registration.
- Root `codex-report.md` (this report).
- Assembly/codegen output stays in ignored `univariateML/package/`.

## Mathematics

Proved algebraically:
- pmf is lambda^k/(k!)^nu/Z; centered evaluation uses the existing
  residual identity, avoiding subtraction of large log normalizers.
- Consecutive weight ratios are lambda/(k+1)^nu. For nu>=0 the
  outward ratios decrease on either side of the mode, so a geometric
  series bounds the omitted tail. Stop at relative remainder <=1e-18.
- Lower and upper tails sum their own weights. The larger probability
  uses a two-branch log1p/expm1 complement to preserve tiny log tails.
- Geometric nu=0 has P(X>q)=lambda^(floor(q)+1).
- Binary search returns the smallest integer satisfying the requested
  lower or upper tail inequality. R sampling inverts R's uniform RNG.

Checked numerically (final results below):
- Independent 160-digit mpmath d/p fixtures, including extreme lambda,
  concentrated dispersion, geometric, and tiny-rate members.
- Poisson/geometric identities, inverse inequalities including log p=-1000.
- Slow R finite-series likelihood oracle; existing C++ fitter tests unchanged.
- Fixed coordinates, face errors and fitted-model generics.

Assumed / limitations:
- Existing CMP normalizer's certified truncation/quadrature bounds and
  fitter mathematics are reused unchanged; floating-point roundoff is not
  interval-certified. No new claim of uniform error over all parameters.
- Tail summation has a 1,000,000-step budget; quantile bracketing stops
  above 9e15. Work failures return NaN (R warns).
- Quality oracle independently truncates at k=127; its applicability is
  checked for the golden, not asserted throughout the parameter space.

## Validation

- Initial `just test`: 4,850 passing expectations, 0 failures/warnings/skips.
  This assembly preceded the new per-CMP test file.
- Final `just test`: 4,962 passing expectations, 0 failures/warnings/skips.
- Final `just cpp-test`: 5/5 ctest targets pass. Verbose ctest rerun:
  310 cases / 506,164 assertions, all pass (0 skipped).
  bfgs: 6/21; sample: 1/10,204; distributions: 292/495,264;
  no_throw: 3/56; existing cmp: 8/619 (cases/assertions).
  New CMP distribution tests: 3 cases / 30 assertions.
- `just quality cmp`: all checks pass, 8 starts / 16 feasible optimizer
  results; oracle gap 2.2737367544323206e-13. Standardised score 3.9e-15.
- 7 mpmath fixtures test 21 density/lower/upper log probabilities.
- Independent R ppois probes verify both inverse inequalities at lambda=1e6,
  p=1-1e-12, where qpois differs from CMP by one count in both tails.
- Intermediate new-test failures were corrected: qpois extreme-tail identity
  replaced by inverse inequalities; bootstrap test now checks raw draws
  (2 parameters x 3 replicates), including the fixed dispersion row.
- Initial quality oracle could not evaluate large symbolic integer constants
  in NumPy; evaluating constant log-gammas at 40 digits fixes its oracle.
- `git diff --check` passes.
- Existing golden files are unchanged; only CMP was generated.

## Open questions

None requiring a user decision; numerical work limits are documented.
