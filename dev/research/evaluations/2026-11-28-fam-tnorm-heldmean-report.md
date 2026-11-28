# Held-mean truncated normal on a tiny window

TASK-485, worktree `codex/fam-tnorm-heldmean`, base `62d65023`.

## Reproduction and fix

Archive case `007677` (`55cf0f3629de5282`) has 12 observations and a held parent mean of `-0.062005588372027076`. The fitted window is `[-2.1573937203803818e-150, 7.587636626597624e-151]`. A native replay of `62d65023` through `norm +truncate=tnorm` returned `IterationFailure` (status 10), with no candidates. The face comparison already came before Newton. Subtracting the held mean rounded both bounds and all observations to the same residual, so the standardized uniform second moment was exactly one. The strict comparison added by TASK-459 was inconclusive, and the scale search received coincident standardized bounds.

Let $w=b-a$, $u=(x-a)/w$, and $S=\max(|a-\mu|,w)$. Expanding $(x-\mu)^2$ and cancelling the constant gives the empirical-minus-uniform second-moment gap divided by $wS$ as

$$
\frac{2(a-\mu)}{S}(\bar u-1/2)+\frac{w}{S}(\overline{u^2}-1/3).
$$

At zero quadratic tilt this gap has the sign of the likelihood derivative. A strictly positive derivative and concavity give a strictly lower likelihood at every negative tilt, proving the uniform limit in exact arithmetic. The new comparison runs when the existing moment comparison is inconclusive. It uses unit-window coordinates and the TASK-459 binary64 margin, $(128+8n)\epsilon$, multiplied by the sum of the absolute coefficients. The unit-window first and second moments are bounded by one, so the summation allowance controls their absolute errors even when either centered moment is small. The remaining allowance covers rounding in scaling, centering, and combining the terms. The certificate is conditional on the computed sign clearing that margin. Exact rational evaluation of the archived binary64 inputs gives a scaled gap of `0.1811585636995257`, against a margin of `9.947598300641403e-14`. The unscaled gap is about `3.2756739011705266e-152`.

A strict uniform win returns `UniformBoundary` before Newton. Other results from the supplementary comparison fall through to the existing interior search. In the free branch, the mean-matched exponential face is evaluated before joint Newton. With scale held, only the linear tilt can vary. An interior sample mean gives a finite location root, and the existing exact endpoint check identifies unbounded location before Newton. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp) · [Norm dossier](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md)

## Verification

Both native and double-only modifier-route replays used `fit-snapshot.py --modifier-routes --family tnorm` on the workstation with GCC 15.2. Each replay covered 285 cases. Case `007677` returned certified `UniformBoundary` (status 9), model `unif`, with one candidate. Its bounds and limit log likelihood (`0x1.023cf59ee08c2p+12`) match the stored native baseline. A direct comparison to the pre-fix native replay found exactly one changed row, case `007677`. The diagnostic text follows TASK-459. [Archive inputs](2026-11-07-fit-audit-data/cases.json.gz) · [Stored native route snapshot](2026-11-15-fit-snapshot-data/routes-native.jsonl.gz)

The appended modifier tests check the archived uniform payload and zero Newton iterations, and check a concentrated sample on the same window with the parent mean held at the midpoint. The latter test requires a certified finite fit, agreement with the analytic symmetric truncated-normal variance, and log likelihood above the uniform limit. Existing tests, pinned values and goldens had no edits. [Regression tests](../../../univariateML-cpp/tests/modifiers/tnorm.cpp)

`/tmp/umvl-cpu/run4 just test-changed --no-long-double` passed on the workstation. The runner classifies the modifier test file as shared, so it ran the full native C++ development selection (136/136), the double-only C++ selection (181/181), the R development suite (38,424 passed, 157 skipped, zero failures or warnings), the JavaScript build, and the JavaScript tests (601/601). R `dist-norm` had 144 passes and one skip. `git diff --check` passed.

## Open

`just site-check` reached the Astro build on the workstation and exited 127 because `astro` was absent. The worktree was provisioned without `site/node_modules`. The remaining verification requires rerunning the recipe in this worktree once its site dependencies are provisioned.
