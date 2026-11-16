# Equivariance batches D and E: lst, std, emg on near-constant data (TASK-214.42)

Branch `claude/equiv-de`, from master `704c846e`. Not committed. Source:
[equivariance report](2026-11-15-equivariance-report.md) (sections 3.4 and 3.5), procedure as in
[batch A](2026-11-15-equiv-a-report.md). Comparison data: [2026-11-16-equiv-de-data](2026-11-16-equiv-de-data/)
(`classify.py`, `classify-native.txt`, `classify-double.txt`).

## Cause and fix per family

### lst and std (batch D), `lst/certificate.hpp`

Cause, as in the report: the interval allowance in `t_cert_detail::data` was
`eps*(1+|v/unit|+|center/unit|+|value|)`, which is 266 (lst) and 364 (std) on the near-constant sample, against
standardized values near 1. The variance enclosure then contained 0 and `data()` returned false.

Fix:

- `certificate.hpp:140` the allowance is `eps*(1+|value|)`. Claim checked: the exactness of the subtraction is not needed.
  `value = fl(fl(v - center)/unit)`; each of the two roundings has error at most half an ulp of its own result, so
  `value` differs from the real number `(v - center)/unit` by at most about `DBL_EPSILON*|value|`, whatever the
  operands (`center` and `unit` are fixed doubles). `eps` is `64 DBL_EPSILON`, so `eps*(1+|value|)` encloses it with a
  large margin.
- `certificate.hpp:414-424` (`t_certificate`): the collapse rule `mass > floor*(n - mass)` is evaluated before `data()`,
  on a sorted copy and exact equalities only, and returns `Unbounded` with `loglik_lower = loglik_upper = inf`.
  The later test at line 444 is unchanged.

std needed no change: its normal-limit coefficients are the limit coefficients of the certificate result. The named
risk (later coefficient-restoration enclosures) did not occur: the near-constant sample ends `L norm` through
`student_t_mle_certified`, and no gate on a coefficient enclosure is reached in the normal branch.

Side effect, measured: the old allowance also made the normal-limit enclosures of ordinary offset samples too wide.
std archive 7199 (n = 12, mean 1.0e8, sd 0.493): the old limit sd was off by 6.6e-11 relative to the 50-digit
population sd, the new one is within 6e-18.

### emg (batch E), `emg/certificate.hpp`

Cause, as in the report: the certificate evaluates `z = b v - u` on raw data with the allowance
`64 eps (|b v| + |u|)`; at the near-constant sample `u` is 2.0e16 (ulp 4) against a true `z` spread of 2.45, so the
computed `z` are all 0 and `profile(start)` fails.

Fix (`certificate.hpp:300-376`): `outer_certificate` (the only entry of the global search, called at
`mle.hpp:70`) now fits `y = (x - x_min)/2^e` and maps back; the old body is `outer_certificate_raw`. The transform is
used only when it is exact in binary64: spread below `2^-16` of the largest magnitude, `2^e` the power of two at or
above the spread, every `x_i - x_min` and a held `mu - x_min` representable (error-free TwoSum residual,
`emg_exact_difference`, line 313), and the scaled holds `sigma/2^e`, `lambda*2^e` normal and exactly invertible.
Otherwise the raw path runs unchanged. With the transform exact the sample has the same likelihood up to the
constant `n log 2^e`: every log likelihood and bound (result, candidates, `limit_loglik`, `loglik_lower`,
`loglik_upper`, `segment_loglik_upper`) is shifted by `-n log 2^e` and padded by `16 DBL_EPSILON (|v| + |shift|)`;
coefficients are mapped by model (`mu = fma(2^e, mu', x_min)`, `sigma = 2^e sigma'`, `lambda = lambda'/2^e`; normal
face `(mean, sd)` like `(mu, sigma)`; translated exponential `(mu, rate/2^e)`).

Differs from the report's proposal in two points. The transform is applied at the call of the certificate, not at the
entry of `emg_accuracy_search`: the earlier branches (constant samples, the normal limit with `mu >= max`, all-held
evaluation) use exact data comparisons and raw formulas and are unchanged; the location-only slice stays raw as
intended. The origin is the minimum and the scale a power of two (not midrange and half-range), so the map is exact
and ordinary samples are not touched (threshold above).

`emg/README.md`: one paragraph after the endpoint-multiplicity sentence states the threshold and the exactness
condition.

## Tests

C++ (appended, none changed or removed), data from `native.jsonl.gz`:

- `lst/test.cpp` "lst near-constant and tied samples end in scale collapse": near-constant and `tie3`, status
  `Unbounded`, `loglik_lower = inf`.
- `std/test.cpp` "std near-constant sample reaches the normal limit as its tie3 preimage does": both samples
  `NormalBoundary`, limit sd positive and below 1e-17.
- `emg/test.cpp` "emg near-constant and tied samples reach the translated exponential limit": both
  `TranslatedExponentialLimit`; near-constant `limit_coef` is `(0.9120912193716119, 1.5 * 2^54)`.

Before the fix (headers reverted to `HEAD`, tests kept; built and run): lst near-constant status 11
(`NumericalFailure`) instead of `Unbounded`, std status 11 instead of `NormalBoundary` (and an empty `limit_coef`),
emg status 6 instead of `TranslatedExponentialLimit`; the `tie3` halves passed. After: all three pass.

No existing test was changed.

## Snapshot diff

Callers of the changed code, from `git grep`: `lst`, `std` (`t_certificate`), `sstd` (`student_t_mle_certified`,
`sstd/mle.hpp:723`), `emg`; `st` includes `lst/mle.hpp` and was run as well. Command:
`--family lst,std,emg,sstd,st`, native and `--no-long-double`, dirs kept in `build/snapshots/{native,double}`.
Baselines `2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`.

| family | cases | identical (native / double) | changed (native / double) | kind changes | `Ok` to stop |
|---|---|---|---|---|---|
| lst | 173 | 130 / 130 | 43 / 43 | 2 | 0 |
| std | 173 | 127 / 127 | 46 / 46 | 2 | 0 |
| emg | 213 | 185 / 174 | 28 / 39 | 12 | 0 |
| sstd | 285 | 285 / 285 | 0 | 0 | 0 |
| st | 285 | 285 / 285 | 0 | 0 | 0 |

No `Ok` becomes a stop. Kind changes (native and double agree except the emg 1311 row):

- lst `class near-constant`: numerical failure (evaluation) to `nomle` unbounded (certified).
- lst `class offset`: numerical failure to limit norm (certified).
- std `class near-constant` and `class offset`: numerical failure to limit norm (certified).
- emg `class near-constant`: numerical failure to limit translated exponential (certified).
- emg `class offset`: numerical failure (evaluation) to `unresolved` ("normal face unresolved: no verified finite
  winner beats it"). Both are stops; the offset sample has spread 3.5 at magnitude 1.7e15, so the transform applies and
  the search now runs to its end. Not a regression; it is a more specific stop.
- emg archive 1272, 1273, 1275, 1276, 1279, 1281, 10263, 10264: numerical failure to limit translated exponential
  (certified).
- emg archive 1311: native numerical failure (double: timeout) to `Ok`; archive 1343: numerical failure to `Ok`.

Other changes in the same cases:

- lst, std: 2 archive limit rows each go from uncertified (core 10) to certified (lst core 4, std core 3);
  lst 15 and std 10 `Ok` to `Ok` estimate moves, max 1.8e-15 (lst) and
  1.1e-15 (std); none above 1e-13. The allowance change shrinks every enclosure, so the Newton centre of ordinary
  fits moves in the last digits.
- lst `Ok` log likelihood move above 1e-12: resample n7-s2 (a Cauchy-boundary fit with the same coefficients), 4.45e-12
  relative. Against a 50-digit evaluation at the returned coefficients (-7.9211338463575), reported old
  -7.92113384625587, new -7.92113384629111: the new value is closer.
- std stopped cases: limit estimate moves up to 3.3e-11 (archive 7199, the 6.6e-11 error described above, now
  correct); other limit estimates move at most 9.4e-16. Bounds `loglik_*` narrow.
- emg `Ok` to `Ok`: native 2 moves of 1.55e-16 (lambda), double 13 moves, max 7.0e-15 (archive 10272, lambda);
  none above 1e-13. Log likelihood moves above 1e-12: none.
- sstd and st: identical in every case, both modes.

## Strict entries removed

`dev/validation/strict-known.json`, 15 entries left (commands with `--prune`, never `--add`):

- TASK-214.33, `phenomenon:near-constant|{native,double}|{lst,std,emg}|class|near-constant` (6).
- TASK-214.33, `phenomenon:near-constant-tie3|{native,double}|{lst,std,emg}|class|near-constant` (6).
- TASK-214.33, `phenomenon:tie-collapse|{native,double}|lst|class|near-constant` (2; the `collapse-not-reported` entries).
- TASK-214.5, `terminates|double|emg|archive|001311` (1; the case that was a CPU timeout in double mode now ends `Ok`).

New failures: none, in `fit-judge.py`, `fit-density.py` (0 failures), `fit-consistency.py` (its 4 listed
`phenomenon:two-point` entries remain), `fit-refine.py --family lst,std,emg,sstd,st` (178 cases judged, no failure).
`cert-replay.py` has no replay for lst, std, emg, sstd or st, so it was not run for them. `CERT_REPLAY_COVERS_CACHE`
was set to `build/cert-replay-covers.jsonl.gz` (not used). `fit-refine.py` ran on the changed cases; `strict-refine.jsonl.gz`
is unchanged in the tree.

## Suites

All through `/tmp/umvl-cpu/run4`, one at a time, `nice -n 10`, BLAS and OpenMP pinned to 1.

- `just cpp-test`: 110 of 110 passed.
- `just cpp-test-no-long-double`: 154 of 154 passed.
- `just test-changed` (85 families, 3030 configurations): exit 0 for `just cpp-test` (263 s), `just test` (R, 579 s),
  `just js-build` and `just js-test` (175 s, 586 passed).

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/emg/certificate.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/emg/README.md` (one paragraph)
- `univariateML-cpp/include/cppunivariate/distributions/{lst,std,emg}/test.cpp`
- `dev/validation/strict-known.json`
- `dev/research/evaluations/2026-11-16-equiv-de-report.md` and
  `dev/research/evaluations/2026-11-16-equiv-de-data/{classify.py,classify-native.txt,classify-double.txt}`
