# Face fixes (TASK-214.32): gompertz huge, pig and cmp all-zero uniqueness

Source of the findings: the Failures section of
[the replay-faces report](2026-11-15-replay-faces-report.md); predicates in
`dev/validation/cert_replay_faces.py`. Branch `claude/face-fixes`, uncommitted.

## Findings

### 1. gompertz, double-only build, class `huge`

- Cause: `gompertz_mle_impl` standardizes by the mean, `total / n`. With data
  near DBL_MAX the sum is `inf` in a double-only build (long double in native
  builds holds it), so no standardization ran; `sum_x` and `sum_xsq` then
  overflowed, `inf >= inf` selected the exponential limit, although
  $nQ - 2S^2 < 0$ (finite MLE). Native decided finite.
- Fix: `distributions/gompertz/mle.hpp` lines 67-80. When the sum is not finite
  but the data are, the mean is formed from maximum-scaled terms
  (`xmax * mean(x / xmax)`). The data are then standardized as in the native
  build, so the unchanged comparison `sum_xsq / n >= 2 mean^2` runs on
  well-scaled values. No padding or changed margin; the criterion is the same.
- Evidence: standalone program on the test sample
  `{1.33e307, 1.5e308, 1.7e308, 1.6e308, 1.4e308, 1.45e308}`, built with
  `-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`: before the fix status 1
  (Overdispersed, a = b = nan); after, status 0 with a = 1.73127e-310,
  b = 3.55158e-308, identical to the native build. Regression test
  `gompertz: exponential-limit decision survives sums that overflow binary64`
  (`gompertz/test.cpp`) checks Ok, estimate outcome, a > 0, b > 0; it fails
  without the fix in the double-only build. Snapshot: double `class/huge`
  changes from limit exp (rate 0x0.8a93f1b6051e4p-1022) to estimate
  a = 5.2051791108271714e-309, b = 1.200158248042536e-308.

### 2. pig, all-zero counts, both free

- Cause: the branch `average == 0 && !fs` returned a plain Ok result, which the
  outcome mapped to `estimate(true)` with `unique = true`; $(\mu, 0)$ attains
  the maximum for every $\mu$.
- Fix: `PigResult::unique` (`pig/mle.hpp` line 23, default true), set to `fm`
  (mean held) in that branch (line 378), read by `outcome()` (line 419).
  Coefficients unchanged.
- Evidence: test `pig: all-zero counts with both coefficients free are not a
  unique maximum` (`pig/test.cpp`): mean 1, shape 0, estimate, `unique` false;
  with mean held at 2.5 `unique` true. Replay predicate `unique` holds on
  `class/zeros` in both modes.

### 3. cmp, all-zero counts, both free

- Cause: same; $\lambda = 0$ attains the maximum for every $\nu$.
- Fix: `CmpResult::unique` (`special/cmp.hpp` line 402), set at line 467
  (`unique = !(!fl && !fn)`, so false only with both free); both
  `outcome()` overloads (`cmp/mle.hpp` lines 132, 147) pass it to
  `estimate(true, r.unique)`.
- Evidence: test `CMP all-zero counts with both coefficients free leave nu
  unidentified` (`cmp/test.cpp`): Ok, nu 1, `unique` false; with `nu` held at 2
  `unique` true. Replay `unique` holds on `class/zeros` in both modes.

### Bindings

R (`bindings/r/scripts/common_fit.h` line 56, attribute `unique`) and JS
(`bindings/js/bindings.cpp` lines 60 and 69) read `fit.outcome.unique`
generically; `bindings/common/fit.hpp` line 316 takes the first listed maximizer
for a non-unique estimate. No binding change needed. The dossiers (pig and cmp
existence sections) already state the unidentified mean and dispersion; no
dossier change.

### Observed, not changed

The same unidentification is documented for hold subsets: pig with shape held at
0 and mean free, and cmp with $\lambda$ held at 0 and $\nu$ free, still carry
`unique = true`. Changing them would alter archive hold cases outside the
allowed snapshot differences; they need their own card.

## Snapshot diff (gompertz, pig, cmp; 270 cases per mode)

Baselines: `2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`.

- native: 2 of 270 differ: cmp `class/zeros` and pig `class/zeros`,
  `unique` true to false.
- double: 3 of 270 differ: the same two, and gompertz `class/huge` (limit to
  estimate, as above).
- No other field changed; `tie3` cases are in the baseline and did not differ.

## Known list (`dev/validation/strict-known.json`, `--prune` only)

Removed, six entries:

- `mode|both|gompertz|class|huge` (TASK-214.13), by fit-judge
- `cert-replay:moment|double|gompertz|class|huge` (TASK-214.32)
- `cert-replay:unique|double|cmp|class|zeros` (TASK-214.32)
- `cert-replay:unique|double|pig|class|zeros` (TASK-214.32)
- `cert-replay:unique|native|cmp|class|zeros` (TASK-214.32)
- `cert-replay:unique|native|pig|class|zeros` (TASK-214.32)

fit-judge and cert-replay: no failures, no new ones. fit-consistency: two known
`phenomenon:near-constant` failures remain, none new, none pruned.

## Tests (each through `/tmp/umvl-cpu/run4`, `nice -n 10`)

- `just cpp-test`: 110 of 110 passed.
- `just cpp-test-no-long-double`: 154 of 154 passed.
- `just test-changed` (85 families): cpp-test exit 0, `just test` exit 0 (testthat
  FAIL 0, PASS 36354), js-build exit 0, js-test exit 0 (586 pass, 0 fail).

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/gompertz/{mle.hpp,test.cpp}`
- `univariateML-cpp/include/cppunivariate/distributions/pig/{mle.hpp,test.cpp}`
- `univariateML-cpp/include/cppunivariate/distributions/cmp/{mle.hpp,test.cpp}`
- `univariateML-cpp/include/cppunivariate/special/cmp.hpp`
- `dev/validation/strict-known.json`
- `dev/research/evaluations/2026-11-15-face-fixes-report.md`
