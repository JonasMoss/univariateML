# Equivariance batches B and C: trgamma, fpareto, gengamma on near-constant data (TASK-214.42)

Branch `claude/equiv-bc`, from master `6849644d`. Not committed. Source: [equivariance report](2026-11-15-equivariance-report.md)
(sections 3.2, 3.3), procedure as in [batch A](2026-11-15-equiv-a-report.md) and [batches D, E](2026-11-16-equiv-de-report.md).
Comparison data: [2026-11-16-equiv-bc-data](2026-11-16-equiv-bc-data/) (`classify.py`, `classify-native.txt`, `classify-double.txt`).

## Cause and fix per family

Shared change, `trgamma/mle.hpp`. `LogData` gets `relative`, `anchor`, `rmean`, `scale_at(s)` and `centre_of(b)`.
`relative_log_regime(x)` is true when `0 < (max-min)/min < 1e-6`: rounding each log moves it by about 1e-16, which distorts
the log spread by the default tolerance 1e-10 at that relative spread (the threshold used for `invburr` in batch A).
`standardize_relative(x, d)` builds `u` from `transformed_detail::logs` (`log1p` of the exact difference from `x[0]`),
standardizes the relative logs, and keeps the absolute `mean` and `sum` as the two-word values of the anchor logs.
`scale_at` returns `x0*expm1(rmean+s) + x0` (|q|<0.5), so the public scale rounds once. Samples outside the regime take
the unchanged path.

### trgamma (batch B)

Cause as in the report: `lost_log_distinctions` fired because the two logs are the same binary64 number.
Fix, `trgamma/mle.hpp`: `trgamma_mle` (about lines 925-960) builds the relative `LogData` in the regime and runs
`lost_log_distinctions` on the relative logs; `trgamma_fit_log` takes an optional prepared `LogData`; `make_result` and
`face_power` take the scale from `scale_at`; `trgamma_faces_data(d)` is split out of `trgamma_faces_log`. Result: L power,
`alpha` = sample maximum, `beta` 1.3366841215615552e16, as in the report. `check_represented` still uses the absolute logs
(only reached by `Ok`/numerical results; no near-constant case reaches it in the snapshots).

### fpareto (batch B)

Cause as in the report: GB2 concave gate with `dv` = 39.7. The "1" in `dv = 8 eps (1 + ...)/sd` is also 20 when
`sd` is 1e-16, so removing the absolute `|mean|`, `|logb|` terms alone is not enough.
Fix:
- `gb2/mle.hpp` (about 280-290, 335, 368, 381-402): `relative_concave` (p and q held, power or scale free, in the regime)
  skips the log-collapse gates, uses `standardize_relative`, and after `gb2_concave_detail::fit` re-evaluates `logLik` at the
  returned (rounded) coefficients from `log_ratio(x, scale)` in extended precision, per the decision "logLik is the log
  likelihood at the returned coefficients". `loglik_lower` keeps its error width, `loglik_upper` keeps the supremum bound.
- `gb2/concave_certificate.hpp` lines 10, 18-22, 36-37: offset via `d.centre_of(b)`, scale via `d.scale_at`, and for relative
  data `dv = 8 eps (1 + |z| + (|rmean| + |logb_rel|)/sd)`.

Result for the near-constant sample: `Ok`, certified, shape2 6.3828980049954096e16, scale 1.4170184014157163,
logLik 106.17345192245736 (60-digit value at the returned coefficients 106.173451922457372; supremum 106.2314);
`loglik_lower/upper` 106.17345192245328 / 106.2313734206586. The `fpareto_transport_accuracy` question: it is not reached
(`!r.certified` is false for a certified GB2 result), so it neither accepts nor rejects the 0.058 loss.
The five TASK-214.44 archive fits (1674, 1675, 10736, 10745, 1752) are not in the regime; unaffected (that branch is not in
this tree, so I checked the data only: relative spreads are 1e3 to 1e300 or 4e-6).

### gengamma (batch C)

Cause as in the report: `log_error = 8 eps |log x|` 1.45e-16 against a spread of 5.9e-17 inflates the face certificate.
Fix, `gengamma/mle.hpp`: the old body is `gengamma_mle_core(..., relative)` taking optional relative logs in place of
`log(x)`; the new `gengamma_mle` uses relative logs when `mu` is free and the sample is in the regime and shifts the result
back (`mu + log x0` as a two-word sum, `logLik`, `limit_loglik`, `loglik_lower/upper`, `segment_loglik_upper`, candidate
`mu` and bounds by `-n log x0`). `face_certificate.hpp`, `local.hpp`, `held*.hpp` needed no edit: they receive the relative
logs through `logs`, so `log_error` is `8 eps |delta|` and `data_error/data_scale` is O(eps). Held `mu` keeps binary64 logs
(its paths compare logs with `mu`; not changed). Result: L pareto, `a` 2.4899936518791200e16 (50-digit 2.48999365187911995e16),
`b` = sample minimum, logLik 110.50623857503564 (50-digit 110.50623857503563), certified.
Not reached here and unchanged in risk: a finite fit on relative logs has `mu` rounded to double against a `sigma` of 1e-16,
and `attainable_tol` scales use `max(|mu_rel|, sigma)`.

Dossiers: one sentence each in `trgamma/README.md`, `gengamma/README.md`, `gb2/README.md` (threshold, and for gb2 the
logLik re-evaluation).

## Tests

Added (`near-constant` and `tie3` samples from `native.jsonl.gz`):
- `trgamma/test.cpp` "trgamma near-constant and tied samples end at the power face": both `PowerBoundary`, `limit_coef[0]` = max.
- `gengamma/test.cpp` "gengamma near-constant and tied samples end at the Pareto face": both `ParetoBoundary`, model Pareto.
- `fpareto/test.cpp` "fpareto near-constant sample ends like its tie3 preimage": tie3 `Ok`; near-constant `Ok`,
  `shape2 > 1e16`, logLik within 1e-10 of 106.17345192245737, outcome estimate.

Before the fix (headers at `HEAD`, tests kept): trgamma status 6 (`IterationFailure`) instead of `PowerBoundary`;
gengamma status 4 instead of `ParetoBoundary`; fpareto near-constant status 15 (`NumericalFailure`) instead of `Ok`.
The tie3 halves passed. After: all pass.

Changed expectations of existing tests (each pinned the old stop on a sample that is near-constant by the regime;
new values checked against 50 to 80-digit references):
1. `trgamma/test.cpp` "distinct observations collapsed by logarithms stop numerically": `trgamma_mle` on
   `{1e300, nextafter(1e300)}` is now `PowerBoundary` (was `IterationFailure`). The other three checks (fixed scale, invtrgamma)
   are unchanged.
2. `gb2/test.cpp` "GB2 held beta shapes do not classify log collapse as constant data": same sample, q=2, p=3 held,
   now `Ok` with shape2 12786359414679470 (80-digit root 12786359414679473.79, relative 3e-16) and
   logLik -1310.3714282287533 at the returned point (supremum -1310.178; the rounded scale loses 0.19).
3. `gengamma/test.cpp` "log-input accuracy is checked against fitted scale": `exp(100 + 1e-10 z)` with Q=0 is now `Ok`,
   sigma 1.4142390049674224e-10 and logLik -393.69839592006673 (both equal the 60-digit values). I added a spread-1e-5
   sample (outside the regime) that still ends `IterationFailure`, so the stop itself stays tested.
4. `bindings/r/distributions/trgamma/test.R` "partial logarithmic ties stop as numerical failures": `mltrgamma` on
   `7-(0:15)*4*eps` is now a `univariateML_limit` (message names `mllnorm`); `mlinvtrgamma` still stops numerically.
5. `bindings/js/test.mjs` "trgamma partial logarithmic ties stop numerically": same split (trgamma `limit`, model `lnorm`;
   invtrgamma `numerical`).

## Snapshot diff

Callers (`git grep`): `trgamma`, `invtrgamma` (uses `trgamma_fit_log`), `gb2`, `fpareto` (via `gb2_mle`), `gengamma`;
all five snapshotted, native and `--no-long-double`, dirs in `build/snapshots/{native,double}`, baselines
`2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`. Native and double agree in every kind change below.

| family | cases | identical (native) | changed | kind changes | `Ok` to stop |
|---|---|---|---|---|---|
| trgamma | 204 | 182 | 22 | 2 | 0 |
| fpareto | 572 | 552 | 20 | 8 | 0 |
| gb2 | 572 | 554 | 18 | 6 | 0 |
| gengamma | 28 | 26 | 2 | 2 | 0 |
| invtrgamma | 180 | 180 | 0 | 0 | 0 |

(double mode: trgamma 23 changed, others as above.) No `Ok` becomes a stop. Kind changes:
- trgamma `class near-constant`, `class offset`: numerical (evaluation) to limit power (uncertified, as its tie3 preimage).
- gengamma `class near-constant`, `class offset`: numerical (no-convergence) to limit Pareto (certified).
- fpareto `class near-constant`, `class offset`, archive 1669, 1670, 1672, 1673, 10314, 10315: numerical to `Ok` (certified).
- gb2 archive 2153, 2154, 2156, 2157, 10398, 10399: numerical to `Ok` (certified).

Other changes:
- `Ok` to `Ok` estimate moves: fpareto 6 (max 3.3e-14, shape2), gb2 5 (max 2.1e-12: archive 2306 and 2309, held scale and
  shape1 and shape3; 80-digit reference of 2306 is 1.745440644656763, old 1.745440644656753 (6e-15), new 1.745440644653063
  (2.1e-12): both inside tol, the new one accepts one Newton step earlier because the gate no longer fails on the inflated
  `dv`), trgamma 5 (max 1.7e-16). No `Ok` logLik moves above 1e-12.
- trgamma stopped cases: lognormal-limit estimates move at most 3.3e-17; candidate bounds change in the last digits (15 cases).
- Remaining stops in the regime, unchanged in kind: fpareto archive 1699, 1700, 1702, 1703, 10320, 10321 and gb2 archive
  2191, 2192, 2194, 2195, 10404, 10405 (shape2 held with spread about 1e-8; the radius `target/(4(1+1/A+...))` with
  `A = a sd` about 2e-8 sets `m*radius/2` near 1e-18 against a gradient margin near 1e-7, so the gate cannot pass: a
  different defect, not the log-error model); only candidate digits changed.

## Strict entries removed

`dev/validation/strict-known.json`, 12 entries (60 lines) removed with `--prune` (never `--add`), all TASK-214.33,
`class|near-constant`, native and double, for trgamma, fpareto and gengamma:
- `phenomenon:near-constant|{native,double}|{trgamma,fpareto,gengamma}` (6),
- `phenomenon:near-constant-tie3|{native,double}|{trgamma,fpareto,gengamma}` (6).

New failures: none. `fit-judge.py`: 0 failures. `fit-density.py`: 0 failures (5 limit rows per mode have no C++ density).
`fit-consistency.py`: 2 listed `phenomenon:two-point` entries remain. `fit-refine.py --family trgamma,invtrgamma,gb2,fpareto,gengamma`:
130 cases judged, no failure. `cert-replay.py`: no replay exists for these families ("no replay for trgamma"); run with
`CERT_REPLAY_COVERS_CACHE=build/cert-replay-covers.jsonl.gz` (not used).

## Suites

All through `/tmp/umvl-cpu/run4`, one at a time, `nice -n 10`, BLAS and OpenMP pinned to 1.
- `just cpp-test`: 110 of 110 passed.
- `just cpp-test-no-long-double`: 154 of 154 passed.
- `just test-changed` (85 families, 3030 configurations): `just cpp-test` 110 of 110, R `FAIL 0 | PASS 36335`, JS 586 passed.
  The first run failed two R expectations (`test-dist-trgamma.R:264-265`, item 4 above) and the JS test is item 5; both
  changed and the run repeated green.

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/trgamma/mle.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/gb2/concave_certificate.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/gengamma/mle.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/{trgamma,gengamma,gb2}/README.md` (one paragraph each)
- `univariateML-cpp/include/cppunivariate/distributions/{trgamma,gb2,gengamma,fpareto}/test.cpp`
- `bindings/r/distributions/trgamma/test.R`, `bindings/js/test.mjs`
- `dev/validation/strict-known.json` (12 entries removed)
- `dev/research/evaluations/2026-11-16-equiv-bc-data/` (`classify.py` and outputs), this report
