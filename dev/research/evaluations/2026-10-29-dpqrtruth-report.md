# TASK-78.2 — independent d/p/q truth

Implemented the generator, checked-in tables, C++ consumer and fix queue.
**94/98 families have verified tables. Four explicit oracle gaps remain.**
No commits or git-writing commands were run.

- `python -m quality truth [ids]` / `just dpqr-truth [ids]`; run through
  `/tmp/umvl-cpu/run`. Computation uses mpmath, not C++, R, stats, extraDistr,
  actuar or scipy probability functions.
- `dev/validation/quality/density.py` shares the existing README/spec parser. The harness
  keeps its previous spec preference. Baseline/current norm and zip quality
  result dictionaries are identical; all 19 Python tests pass.
- All 98 folders contain a local `truth.py` route or an explicit gap reason;
  94 contain `truth.json`. Shared generator code has no family registry.
- Per-family results and failures are in
  [summary.csv](2026-10-29-dpqr-truth/summary.csv).
  The 98 status JSONs distinguish generated tables from gaps.
- [Methods and family lists](2026-10-29-dpqr-truth/README.md)
  document the parameter rule, special points, errors, limits and probe comparison.

Numerical checks:

- 544 parameter points, 30,778 x rows, 23,936 quantile rows.
- At least 80 decimal digits; up to 600 where finite endpoint cancellation
  needs it. Normalization checked at every point to 1e-25; worst residual
  2e-32. No NaN table cells. Density hashes match the unchanged READMEs.
- Continuous normalization uses density integration, split at independent
  quantiles/kinks, or a documented density change of variable. Discrete
  sums have finite, telescoping, special-function or explicit remainder control.
- CMP uses an adaptive positive normalizer with a geometric remainder;
  Tukey lambda uses independent log-tail inversion and its implicit density.
  Existing golden-only spec approximations are not reused as general truth.
- Small-tail direct forms avoid subtracting one or cancelling half probabilities.
  Regression checks cover half-t/half-Cauchy agreement, half-normal tiny
  quantiles, folded normal, shifted Gompertz, and enormous discrete counts.
- Both natural and log quantile inputs are rounded to binary64 first.
  Stored adjacent-double bounds and declared overflow/underflow limits are
  independent of C++ p(q). Endpoint densities use one-sided limits.
- The standard stress rule uses sampler/fit values, then one-coordinate
  0.1/10 distance changes or bounded-edge moves, capped at eight points.
  GEV/GPD xi and Tukey lambda include -5e-10, 0, +5e-10.
- Local parameter exceptions: triangular a/b and Frechet mu are held endpoints;
  zmnbinom/ztnbinom size remains continuous despite the metadata trials label.
- 27 broad support declarations differ from the actual first-point support;
  [support-corrections.json](2026-10-29-dpqr-truth/support-corrections.json)
  records them. README front matter was not edited.

C++ wiring and audit:

- Numeric matrix loader and `mltest::check_dpqr_truth` check log d, both p
  scales/tails and both q input scales/tails at the requested tolerances.
  Test-only no-throw assertions record unexpected Boost exceptions and continue.
- 89 explicit signature adapters: **25 strict passing cases, 61 may_fail
  cases, three skipped oracle gaps** (bnbinom, skellam, sn).
  Doctest 2.4.11 semantics were checked: may_fail also permits a future pass.
- The nine density-only families' test.cpp files remain untouched. Their
  eight available tables were separately audited through `check_density_truth`.
- Standalone audit: 95 cases including the loader, 445,914 assertions;
  9,795 numerical mismatches plus two unexpected beta Boost exceptions.
  62 families fail (61 full APIs and density-only gb2); 32 pass.
- All sampler checks passed: 490 points, **9,686,000 draws**. Counts/timings:
  [sampling.csv](2026-10-29-dpqr-truth/sampling.csv).
  CMP uses 1,000 draws/point after its 20,000-draw audit exceeded five minutes;
  its six-point sampler run took 7.368 s. Total sampler time: 13.453 s.
- Probe comparison: 37 failing families overlap, 25 additional families fail
  on the larger grid/tighter tolerances. Half-Cauchy extreme-lower oracle
  false positives were removed; its remaining CDF and endpoint failures are real.

Validation:

- Baseline fab96c1: ctest **5/5**, 172.16 s;
  R **[ FAIL 0 | WARN 0 | SKIP 1 | PASS 8402 ]**.
- Final: ctest **5/5**, 68.54 s;
  R **[ FAIL 0 | WARN 0 | SKIP 1 | PASS 8402 ]**; Python **19 passed**.
  The R skip is the unavailable optional sn package.
- Distribution target: **468 passed, 0 failed, three skipped**;
  953,643 assertions, including 9,791 failures permitted by the 61 may_fail cases.
  Other targets: bfgs 6 cases/21 assertions; sampling 1/10,204;
  no-throw 3/56; CMP 8/619. All passed.
- [validation.json](2026-10-29-dpqr-truth/validation.json)
  records exact counts. `git diff --check` passes; goldens are unchanged.

Open gaps: bnbinom needs a uniform 3F2 heavy-tail remainder; pig a controlled
Bessel-K count tail; skellam a controlled two-sided Bessel-I count tail; sn
stable Gaussian-tail integration with an error estimate. These have no stored
truth tables and are explicitly listed in the queue, not counted as successes.
No normalization failure was accepted for any generated family.

Changed: Justfile; quality parser/CLI/generator/primitives/tests/report assembler;
98 family-local routes, 94 tables, 89 test.cpp adapters; test_support.hpp,
json_loader.hpp and the loader regression case; evaluation report/CSVs/statuses.
No dpqr.hpp, mle.hpp, golden.json, R sources/codegen, DESCRIPTION, README front
matter, protected dev/research/status files or work board were changed.
Scratch audit sources, commands and logs remain in `/tmp/umvl-codex/dpqrtruth/`.

Viewer limitation: the local artifact pane rejected this worktree as outside
its configured readable roots. The report and queue remain in this worktree.
