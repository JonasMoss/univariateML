# Equivariance batch F: sn, huber, triang on near-constant data (TASK-214.42)

Branch `claude/equiv-f`, from master `6849644d`. Not committed. Source:
[equivariance report](2026-11-15-equivariance-report.md) section 3.6; procedure as in
[batch A](2026-11-15-equiv-a-report.md) and [batches D, E](2026-11-16-equiv-de-report.md). Comparison data:
[2026-11-16-equiv-f-data](2026-11-16-equiv-f-data/) (`classify.py`, `classify-native.txt`, `classify-double.txt`).

## Cause, fix and certification per family

Before the fixes, a probe (`sn_mle`, `huber_mle`, `triang_mle` on the snapshot samples) gave: sn near-constant
`IterationFailure`, huber `IterationFailure`, triang `NumericalFailure`; the three `tie3` samples were as in the baseline.

### sn, `sn/mle.hpp`

Cause, confirmed: the stop at the public-coordinate check (old line 447) is the one reached. The snapshot candidate list
holds exactly the finite witness (`alpha = 2^31`, `omega = 1.28e-16`) that this branch pushes; the budget branch
would also list the faces.

Fix (`mle.hpp:438`, `:446-459`, `:495-500`, `:504`): a finite candidate whose public likelihood differs from the
standardized one is kept as a witness (`public_gap`) when `alpha` is free and the sample is not a held-location
constant, instead of returning. The face comparison then uses the witness's public likelihood
(`finite_reference = original_best_ll + n log unit`) in place of the standardized one, in both the tie test and the
win test. If no face wins, the stop stands (`if(public_gap) return bad`): that path is not turned into an `Ok` fit,
because `sn_mle`'s polish cannot reach `tol` at a rounded `xi`. No sample in the snapshot or tests reaches that path.

Result: `L hnorm`, `mu = c`, `sigma = 6.4099e-17`, limit log likelihood 109.68, as expected.
Certification: the outcome is the half-normal limit with `certified = false`, the same as the `tie3` preimage
(baseline `tie3` is an uncertified limit). It is not certified here because `limit_detail` for the sn faces is a
comparison of computed values within the `1e-8 n` margin (dossier note), not a proof; the face loses nothing to
rounding (`< 1e-32`, mapped.json), so no rounding loss enters.

### huber, `huber/mle.hpp`

Cause, as in the report: `huber_nuisance_upper` at the rounded `mu` has `k = g - move = 0`, bound infinite.

Fix: `HuberResult` carries the standardized optimum (`std_center`, `std_unit`, `std_m`, `std_s`, lines 22-24, set at
`:192`). `huber_nuisance_upper` is split into `huber_nuisance_upper_at` (`:591`, the old body on explicit
`mu, sigma, logLik`) and a wrapper (`:621`). `huber_standardized_gap` (`:632`) evaluates the same bound in standardized
coordinates at `(std_m, std_s)` and returns bound minus the standardized log likelihood. `huber_mle` uses it only
when the public check fails (`:713-721`): the status stays `Ok` if that gap is within the same allowance and the public
`mu` and `sigma` equal the standardized optimum mapped back to within 8 ulp of the public coordinates. `logLik` stays
the value at the returned coefficients.

Result: `E`, `mu = c`, `sigma = 3.8887e-19`, `logLik = 122.1419`, as expected. Independent check (mpmath, 60 digits,
Newton on the exact score from the returned point): supremum 122.5941, loss 0.452, `mu` off by 2.6e-19 (0.3 ulp).
Certification: `certified = true` (the outcome constructor for `Ok`), and the claim it makes is about the
unrounded optimum: its likelihood is within the allowance of the global bound, so the coefficients are the
correctly rounded maximizer, with the steep-maximizer loss declared and reported through `logLik`
(contract decision 2026-11-14, same as `norm` at 0.75). The certificate bounds the coefficients to 8 ulp, not
the likelihood at them.

### triang, `triang/mle.hpp`

Cause, as in the report: `b` rounds to `c + 2 ulp`; the old acceptance required the global upper bound to be within
`tol` of the public likelihood (`global_upper - actual`), which is 0.52.

Fix (`mle.hpp:316-326`): the public point is accepted when `rounded_endpoint` (unchanged: signed scores and `log1p`
differences prove it is the best public neighbour) holds and the global bound is within `tol` of the continuous
optimum it was rounded from (`certificate_upper - best`, in standardized units). `out.logLik` was already the summed
`dtriang`. Dossier paragraph (`triang/README.md`) rewritten to say this.

Result: `E`, `a = c = 0.4665625208523124`, `b = c + 2 ulp`, `logLik = 111.5967`, as expected.
Certification: `certified = true`; the certificate bounds the continuous maximizer and `rounded_endpoint` proves the
public `b` is the best of the two neighbours; the 0.523 rounding loss is not certified away, it is the declared loss.

## Tests

Added (data from `native.jsonl.gz`, `strict_common.load`):

- `sn/test.cpp` "sn near-constant sample reaches the half-normal face as its tie3 preimage does": both samples
  `RightHalfNormalBoundary`, `xi` = sample minimum, `logLik` equals the summed face density.
- `huber/test.cpp` "huber near-constant sample is an estimate at the rounded optimum, as its tie3 preimage is": `Ok`,
  `mu = c`, `sigma = 3.889e-19`, `logLik = 122.14` equal to `huber_point_loglik`.
- `triang/test.cpp` "triang near-constant sample is an estimate at the best public endpoint, as its tie3 preimage is":
  `Ok`, `a = c = c0`, `b = c0 + 2 ulp`, `logLik = 111.60`.

Before the fix (probe on `HEAD` headers with the same calls): sn status 2 (`IterationFailure`), huber 3
(`IterationFailure`), triang 5 (`NumericalFailure`) on the near-constant samples; the `tie3` halves passed.

Expectations changed (each pinned the old numerical stop, on samples whose spread is a few ulp of their offset; changed
only from the stop to `Ok`, the coefficient and `logLik` checks beside them kept and now exercised):

- `triang/test.cpp`, six `triang: audit-support case N public coordinates` (8173, 8194, 11117, 11118, 11119, 11122): `REQUIRE`
  status `NumericalFailure` to `Ok`.
- `triang/test.cpp` "endpoint representation certificates against 400-digit profiles": `fit.status` for 8173, 8194, 11117,
  11118, 11119, 11122 and every `strict` (tol `1e-14`) status of the ten subcases, `NumericalFailure` to `Ok` (15 lines). The
  pinned coefficients and `logLik` are unchanged. The subcases 8165, 8168, 8172, 8213 passed at `tol = 1e-10` before and
  failed at `1e-14`; they now pass at both, because the rule no longer scales the rounding loss with `tol`.
- `bindings/r/distributions/triang/test.R`: six `audit-support` tests `expect_true(inherits(fit,"error"))` to
  `expect_false`; in "triangular endpoint representation agrees with 400-digit profiles" every `expect_error(...,
  class="univariateML_numerical")` (16 calls) to "does not error".
- `bindings/js/test.mjs` "triangular public rounding loss is a numerical failure" (case 11119): renamed and now expects
  `Ok` with `a = 0.036270735335580465`, `b = 0.03627073533558054`, `c = 0.03627073533558047`.

## Snapshot diff

Families: sn, huber, triang, and `st` (it includes `sn/mle.hpp`); `lst`, `std`, `sstd` do not include these headers.
Command as given, `--family sn,huber,triang,st`, native and `--no-long-double`, dirs kept in `build/snapshots/{native,double}`.
Native and double agree in every kind change.

| family | cases | identical | changed | kind changes | `Ok` to stop |
|---|---|---|---|---|---|
| huber | 173 | 169 | 4 | 4 | 0 |
| sn | 189 | 179 | 10 | 7 | 0 |
| triang | 196 | 185 | 11 | 11 | 0 |
| st | 285 | 285 | 0 | 0 | 0 |

Changed cases (old to new):

- huber, failure `no-convergence` to certified estimate: `class near-constant`, `class offset`, archive 3397, 3400.
  Independent 60-digit check of the new points (mpmath, Newton from the returned point): loss against the supremum
  0.452 (near-constant), 0.0098 (offset, `mu = 1.7e15`), 1.7e-10 (3397), 5.3e-13 (3400); `logLik` is the
  value at the returned point in all four.
- sn, failure `no-convergence` to uncertified limit `hnorm`: `class near-constant`, `class offset`, archive 6425, 6427, 6428, 6431, 6463.
  Three more cases change only in their stop candidates and `loglik_lower` (`class subnormal`, `class subnormal-signed`,
  archive 6464): still failure `no-convergence`.
- triang, failure `evaluation` to certified estimate: archive 8165, 8172, 8173, 8194, 11117, 11118, 11119, 11122,
  `class near-constant`, `class offset`, `class subnormal`. `class subnormal` ends with `unique = false`; coefficients
  are denormal multiples (`a = c = 1`, `b = 4` units of `2^-1074`). Finding: `fit-refine` cannot judge triang
  (36 `ZeroDivisionError` open cases, same count on the stored baseline), so this estimate rests on `rounded_endpoint`
  and `fit-density` only.
- No `Ok` to `Ok` coefficient or `logLik` move anywhere. `st`: identical in both modes.

## Strict entries removed

`dev/validation/strict-known.json`, 122 to 110 entries; run with `--prune`, never `--add`. Removed, all TASK-214.33,
native and double:

- `phenomenon:near-constant|*|{sn,huber,triang}|class|near-constant` (6)
- `phenomenon:near-constant-tie3|*|{sn,huber,triang}|class|near-constant` (6)

New failures: none. `fit-judge.py` (843 cases, only the listed `mode` entry), `fit-density.py` (0 failures; max
relative difference reported vs summed density 3.3e-16 for sn, 6.1e-16 triang, 4.4e-16 huber, 7e-12 st),
`fit-consistency.py` (its 2 listed two-point entries remain), `fit-refine.py --family sn,huber,triang,st` (160 cases judged,
0 failures). `cert-replay.py --family triang`: 208 cases, 0 failures; sn and huber have no replay. `CERT_REPLAY_COVERS_CACHE`
set to `build/cert-replay-covers.jsonl.gz`.

## Suites

All through `/tmp/umvl-cpu/run4` (js-test through `run`), `nice -n 10`, BLAS and OpenMP pinned to 1.

- `just cpp-test`: first run 109 of 110 (triang, the pinned stops above); after the test changes 110 of 110.
- `just cpp-test-no-long-double`: 154 of 154.
- `just test-changed`: first run failed in the R tier (`test-dist-triang.R`, the pinned stops); second run: `cpp-test` 110 of
  110, `just test` 0 failures (36335 passed), `js-build` ok, `js-test` 585 passed, 1 failed (the triang JS test above).
  After changing that test, `just js-test` alone: 586 passed, 0 failed. `test-changed` was not rerun in full after that edit.

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp`, `huber/mle.hpp`, `triang/mle.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/{sn,huber,triang}/README.md` (one paragraph each)
- `univariateML-cpp/include/cppunivariate/distributions/{sn,huber,triang}/test.cpp`
- `bindings/r/distributions/triang/test.R`, `bindings/js/test.mjs`
- `dev/validation/strict-known.json`
- `dev/research/evaluations/2026-11-16-equiv-f-report.md`, `2026-11-16-equiv-f-data/`
