# Modifier and sampler evaluation gaps

TASK-219, worktree `codex/mod-gaps`, based on `7a57d011`. The three cases came from the [qtie-2 evaluation](2026-11-13-qtie-2-report.md).

## Negative binomial with size 1e8 and probability 0.5

For the closed windows `[2, 12]` and `[0, 5]`, the parent interval logs are about −69.3 million. Their natural-scale probabilities underflow, and the R modifier's resolution guard rejects subtraction on that absolute log scale. Both windows have positive probability under the parent law documented in the [negative-binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#dpqr).

When the parent-tail calculation is unresolved, the R evaluator can enumerate a short, finite negative-binomial window. It anchors the largest term with the tail-accurate log PMF and forms relative log masses using the exact adjacent-mass ratio `(size + k) * (1 - prob) / (k + 1)`. A shifted log-sum-exp adds the masses, and conditional evaluation cancels the common anchor before binary64 rounding can erase their differences. The CDF sums each side directly on the relative scale, and the discrete quantile inverts those conditional tails. Enumeration covers up to 256 representable counts and requires a finite log-space normalizer. [R window evaluator, modifiers.R:230](../../../bindings/r/R/modifiers.R#L230)

An independent 100-digit mpmath sum of log-gamma PMFs gives log window masses of −69314525.312805366 and −69314634.205818257, respectively. The compiled R replay had a maximum absolute error of `1.43e-14` in log PMFs and lower log CDFs and a maximum relative error of `1.13e-14` in upper log CDFs. All 48 tested quantiles matched across the two windows, both tails and both probability scales. [Reference generator](2026-11-22-mod-gaps-data/reference.py) · [Reference values](2026-11-22-mod-gaps-data/reference.json) · [Compiled R replay](2026-11-22-mod-gaps-data/evaluation.txt)

The new family test checks the exact requested inputs against the independent values. The shared four-mode round-trip test now exercises both windows, and the guard test checks both successful normalization of a short upper-tail window and refusal of a wider unresolved window. [nbinom test.R:215](../../../bindings/r/distributions/nbinom/test.R#L215) · [test_modifiers.R:212](../../../bindings/r/tests/testthat/test_modifiers.R#L212) · [test_modifiers.R:456](../../../bindings/r/tests/testthat/test_modifiers.R#L456)

## Frozen logarithmic series with theta zero

The dossier excludes the boundary from fitting, including held fits, and identifies the limit as an atom at one. Indeed, the mass at one is `theta / -log1p(-theta)`, which tends to one as theta decreases to zero. At the starting revision, the shared C++ kernel represented this atom, while the base C++ entry points and the R constructor rejected zero. The frozen constructor now admits the limit and the C++ base evaluators expose it. The degenerate quantile endpoints are both one. [Logarithmic-series dossier, README.md:135](../../../univariateML-cpp/include/cppunivariate/distributions/lgser/README.md#details) · [frozen.R:74](../../../bindings/r/R/frozen.R#L74) · [lgser dpqr.hpp:19](../../../univariateML-cpp/include/cppunivariate/distributions/lgser/dpqr.hpp#L19)

The exact atom is the reference for the R and C++ evaluation tests. The R test covers sampling, held and free all-one fit refusals, and quantile endpoints. The compiled replay returned one on all 512 draws. [R test.R:100](../../../bindings/r/distributions/lgser/test.R#L100) · [C++ test.cpp:161](../../../univariateML-cpp/include/cppunivariate/distributions/lgser/test.cpp#L161) · [Compiled R replay](2026-11-22-mod-gaps-data/evaluation.txt)

## Negative binomial with size zero

The dossier treats size zero as an attained point mass at zero, consistent with `P(X = 0) = prob^size` tending to one at fixed positive probability. The R sampler delegated this case to `stats::rnbinom`, which returned NaNs in the inherited replay. The R sampler now returns a zero vector using the bindings' standard sample-length handling, with no random variates consumed. [Negative-binomial dossier, README.md:277](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#details) · [code_helpers.R:271](../../../bindings/r/R/code_helpers.R#L271)

The R test uses `size = 0, prob = .5` on frozen and fitted objects. It checks d/p/q against the exact atom, checks 512 zero draws and an intact RNG state, and compares a seeded positive-size draw with `stats::rnbinom`. The compiled replay also returned zero on all 512 draws. [nbinom test.R:253](../../../bindings/r/distributions/nbinom/test.R#L253) · [Compiled R replay](2026-11-22-mod-gaps-data/evaluation.txt)

## Validation

All final checks ran on the workstation (`JonasBI`, GCC 15.2.0, R 4.6.1). Simbox returned exit 75, so the full suite ran locally. The only C++ header edited was `distributions/lgser/dpqr.hpp`.

| Check | Result |
| --- | --- |
| Native and double-only C++ stages of `just test-changed --no-long-double` | 134/134 native, 179/179 double-only |
| `just truth-check` via `just test-full` | 36/36 |
| `just test-full` | 66,168 passed, zero failures or warnings, one skip |
| WASM build via `just site-check`, then `just js-test` | 587/587 |
| `just site-check` | Passed, including all 85 family pages and C++ snippets |
| Modifier-route snapshots against the stored baselines | 1,950 exact matches per mode. [Native](2026-11-22-mod-gaps-data/routes-native.txt) · [Double-only](2026-11-22-mod-gaps-data/routes-double.txt) |

The initial changed-family run stopped in its R stage on the old expectation that a short upper-tail negative-binomial window must refuse. The updated guard test passed in isolation and the full R suite. The JavaScript stage ran separately against the WASM build produced by the site check.

## Open

None for the three reported inputs. A rare window beyond the bounded enumeration can still reach the parent-tail resolution guard.
