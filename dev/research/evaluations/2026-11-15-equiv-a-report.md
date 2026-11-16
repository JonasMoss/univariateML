# Equivariance batch A: Burr certificate on relative logs (TASK-214.42)

Branch `claude/equiv-a`, from master `ef464c2e`. Not committed. Source reports:
[equivariance report](2026-11-15-equivariance-report.md) (burr, pareto4, invburr, batch A) and
[refine causes report](2026-11-15-refine-causes-report.md) (burr 638, 639). Snapshot comparison
data: [2026-11-15-equiv-a-data](2026-11-15-equiv-a-data/) (`classify.py`, `classify-native.txt`,
`classify-double.txt`).

## Cause and fix per family

### burr (`burr/certificate.hpp`, `burr/mle.hpp`)

Cause, as in the reports: `data()` formed `log(v)` in absolute terms and padded every log by
`64 eps (1 + |log v|)` (about 1.4e-14), while the logs of near-constant data spread by about
1e-16. The standardized box half-width became 531, `distances` contained 0, and the Pareto bound
at the old line 303 was invalid, so `fit()` returned at the old line 304. The root-neighbourhood
coordinates and every `exp(center - ...)` scale used the same absolute logs.

Fix:

- `certificate.hpp:63-71` `rel_log(v, a, err)`: `log1p((v - a)/a)` when the ratio is within 1/2
  (the subtraction is exact there), otherwise `log v - log a`; returns a bound on its own
  absolute error (8 ulp of the value, or of the absolute logs in the second branch).
- `certificate.hpp:72-112` `Data` and `data()`: anchor `x0` is the median; logs are relative to
  it, then centred at their mean, standardized, and the box is `y +- e` with
  `e = 64 eps (1 + |y|) + 2 (log error + centring error)/unit`. The padding is now relative to
  the spread, not an absolute 1.4e-14. The jacobian is `-(n log x0 + sum rel) - n log unit`. The
  anchor enters the scale only through `Data::scale(q) = anchor * exp(q)`, which replaces
  `exp(center - ...)` at the five places that formed a scale.
- `certificate.hpp:390-402` `fit()` root neighbourhood: the proposed scale `s` is a double; `original`
  now holds `log(v/s)` relative to `s` with tight boxes, and the shift between the two
  coordinate systems is the relative log `log(s/x0)`. No absolute log is formed.
- `certificate.hpp:238-250` `numeric()` and `isolate` (`:269`), the Newton centre: `numeric` accumulates in
  `long double` and `isolate` applies the last Newton step before stopping. Without this the change of rounding in
  the centre moved ordinary fits by 1.5e-14 to 6e-15 (golden tolerances are 2e-14, 5e-15, 1e-15
  for burr, pareto4, invburr); with it the goldens are within 2.4e-16 (burr), 1e-16 and 5e-16
  (pareto4 shape1 and scale) and 6e-16 (invburr shape1) of 50-digit references. This is an
  accuracy change in the shared header, not a requirement of the near-constant fix.
- `burr/mle.hpp:383-391, 437-446` `burr_mle_holds` (shape1 held, archive 638 and 639): `l = log x` and its mean are
  replaced by `r_i = rel_log(x_i, x0)` and `rmean` for every score, the location solve and the
  scale (`s = x0 exp(rmean + location/b)`); `-l_i` stays absolute only in the final `ll`, which
  is overwritten by `burr_local_loglik` for `Ok`. The Lomax branch (shape1 and scale free of
  holds on `b` only) is unchanged.
- `burr/mle.hpp:253-300` `burr_mle_certified(..., bool reflect=false)`: new trailing argument, see invburr.

Result on the stored cases: archive 638 shape2 87511530.540359885 (reference 87511530.54035975,
relative error 1.5e-15, was 7.2e-10) and 639 shape2 875130348796.00476 (reference
875130348796.0063, 1.8e-15, was 4.9e-6). References are 60-digit solutions of the two score
equations at the held shape1.

### pareto4 (no change to `pareto4/mle.hpp`)

Cause: the two logs of the near-constant sample are equal in binary64, so `data()` failed (old
line 297). Pareto4 with `min = 0` passes `y = x - min` to `burr_mle`, so the `data()` fix is the
whole fix: the near-constant sample now returns the Pareto face, the tie3 sample the Pareto face
with its exact value. The shifted subtraction guard stays.

### invburr (`invburr/mle.hpp`, `burr/certificate.hpp`, `burr/mle.hpp`)

Cause: `invburr_mle` formed `1/v` and passed the reciprocal vector. Rounding each reciprocal
moves each log by 1e-16, which distorts a log spread of 1e-16 by 11.6%, and can merge distinct
values into artificial ties.

Fix: when the observations lie within relative distance 1e-6 of the first (`invburr/mle.hpp:179-181`),
the certificate runs on `-log x` relative to an anchor without the reciprocal vector:
`Data::reflect` (`certificate.hpp:72-79`) takes `-rel_log(v, x0)`, the anchor of the reciprocal
sample is `1/x0` and enters only the scale, and `fit()` counts the minimum multiplicity on the
original sample. `burr_mle_certified(..., reflect=true)` runs the legacy fallback on a reciprocal
vector only when no artificial ties exist (`burr/mle.hpp:292-300`). The inverse Weibull and power face
likelihoods are evaluated at the returned coefficients on the original sample
(`invburr/mle.hpp:216-227`, `:240`) with `log_ratio`; the burr-side evaluation on rounded reciprocals is
not used. Spread above 1e-6 keeps the previous reciprocal path unchanged, including the
artificial-tie guard. The threshold is where reciprocal rounding (1e-16 relative to the spread)
reaches the default tolerance 1e-10. One dossier sentence records it
(`invburr/README.md`, after the artificial-tie paragraph).

Open decision, not taken: always reflecting would remove the reciprocal vector entirely, but
the existing test "invburr: transformed certificate workloads and guarded outcomes" compares
`invburr` with `burr` on the reciprocals bit for bit on every fixture, and "reciprocal overflow
and artificial ties stop numerically" pins the guard. Both would have to be weakened, so the
reflected path is limited to the small-spread region. Inputs with spread above 1e-6 are
unchanged, including the equivariance defect of the reciprocal in that region (error
`1e-16 / spread`, at most 1e-10).

## Tests

C++ (appended, none removed):

- `burr/test.cpp`: "burr near-constant and tied samples reach the Pareto face" (near-constant and
  `tie3` from `native.jsonl.gz`, against 60-digit values of alpha and the log likelihood);
  "burr shape1 held keeps the log spread of near-constant data" (archive 638 and 639, shape2 and
  scale within 1e-12 of the references).
- `pareto4/test.cpp`: "pareto4 near-constant and tied samples reach the Pareto face".
- `invburr/test.cpp`: "invburr near-constant and tied samples reach the inverse Weibull face"
  (shape2, scale, and the log likelihood at the rounded returned coefficients, 106.3798004017
  for near-constant).
- Before the fix (headers reverted to `HEAD`, tests kept): all four fail
  (near-constant burr, pareto4 and invburr: `REQUIRE` status 9 instead of the face; 638 and 639:
  shape2 relative errors 7.17e-10 and 4.92e-06). After: all pass.

Changed expectations (a decision for review; each pinned the defect):

- `burr/test.cpp` "burr: certified retained workloads and guarded stops": fixture
  `nearly-constant` (log spread 2e-6) expected a guarded `NumericalFailure` with refutation
  witnesses. It now expects `WeibullBoundary`, the same status for the preimage `x^1e5`, and
  `shape2/1e5` equal to the preimage's `shape2` within 1e-7 (measured: the status is Weibull for
  powers 1, 1e2, 1e4, 1e5, 3e5 and `shape2` scales as 1/p). The witness check function and the
  other fixtures are unchanged.
- R, `bindings/r/distributions/{burr,invburr,pareto4}/test.R` (five tests) and `bindings/js/stop-test.mjs` (one test), pinned numerical
  stops for `exp(seq(-1e-6, 1e-6, length.out = 20))` and the 20-point `nearly-constant` samples.
  They now expect `univariateML_limit` with model `weibull` or `invweibull`, and keep the
  diagnostic checks; the single assertion `nom$shape1 > 1e100` (the witness of the old
  refusal) is dropped. Two titles said "retains an uncertified finite search" and now say
  "reaches its Weibull limit" and "reaches its inverse Weibull limit".

## Snapshot diff

Callers of the changed code, from `git grep`: `burr`, `pareto4` and `invburr`
(`burr_mle_certified`), `gb2`, `paralogis` and `pareto4` (`burr_mle_holds`); `paralogis` also
includes `burr/certificate.hpp` for the interval primitives, which are unchanged. `pareto2`,
`pareto3`, `trgamma` include `burr/mle.hpp` for helper functions only and were not run. Command:
`--family burr,pareto4,invburr,gb2,paralogis`, native and `--no-long-double`, dirs kept in
`build/snapshots/{native,double}`. Baselines: `2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`.
`classify.py` produced the lists; native and double agree except where noted.

| family | cases | identical | changed | kind changes | `Ok` to stop |
|---|---|---|---|---|---|
| burr | 220 | 134 | 86 | 2 | 0 |
| pareto4 | 204 | 133 | 71 | 2 | 0 |
| invburr | 220 | 171 | 49 | 2 | 0 |
| gb2 | 572 | 571 (native), 572 (double) | 1 native | 0 | 0 |
| paralogis | 104 | 95 | 9 | 0 | 0 |

No `Ok` becomes a stop.

Kind changes (identical in native and double), old kind to new kind:

- `class near-constant`: burr, pareto4, invburr: `numerical` / failure / evaluation to `limit`
  proved (certified): burr and pareto4 Pareto, invburr inverse Weibull.
- `class offset`: the same three families, `numerical` failure to a proved limit (burr and
  pareto4 Pareto, invburr inverse Weibull).
- `tie3` and `near-constant-tie3`: no kind change (stop before and after, a limit); the limit
  coefficients and bounds moved, below.

Certificate status changes inside `limit` (native and double alike): invburr 3 cases and
pareto4 2 cases go from an unproved comparison tie (core status 7, certified false) to a proved
limit (core status 1 or 2, certified true). Burr has no such case besides its two kind changes.

ok to ok coefficient moves (relative, largest first):

- burr: 45 cases, max 4.92e-06 (archive 636 and 639, shape2, the shape1-held near-constant data),
  7.17e-10 (archive 635 and 638, same data), 9.0e-13 (647), 1.7e-13 (634, 637), then at most
  3.1e-14 (scale at 1e+-150 magnitudes) and the golden at 1.95e-14. Four cases above 1e-10,
  all the stored 638 and 639 data (cases 635, 636, 638, 639), all toward the 60-digit
  references. Log likelihood moves above 1e-12: archive 636 and 639 only, 2.43e-12 (higher).
- pareto4: 33 cases, max 3.1e-14; none above 1e-13.
- invburr: 13 cases, max 5.4e-14; none above 1e-13.
- paralogis: 9 cases (scale only), max 1.8e-14. gb2: 1 case (golden, native only), 8.5e-15.

Stopped cases with the same kind (limit): burr 39, invburr 34, pareto4 36 cases changed in
`candidates`, `limit_estimates` or bounds. Largest moves of the limit coefficient: burr 4.9e-06
(archive 617 and 620, the same data as 638 and 639), 2.4e-09, 1.5e-11; invburr 7.7e-06
(3569, 3572), 1.1e-06 (10480), 1.9e-09; pareto4 6.9e-05 (5651, 5654), 7.1e-09, 1.6e-10. Six
invburr and five pareto4 and four burr stopped cases move the limit coefficient by more than
1e-10, all in the archive near-constant series (burr 610 to 628, invburr 3563 to 3580, pareto4 5644
to 5658, five points each). `loglik_*` bounds move by up to 1.4e-3
relative in the same cases (burr 617 and 620) and some `limit_loglik` values change from NaN
to a finite value (invburr, pareto4). 

## Strict entries removed

`dev/validation/strict-known.json` 173 entries to 157. Exactly these 16 left (the commands
used `--prune` only, never `--add`):

- TASK-214.33: `phenomenon:near-constant|{native,double}|{burr,pareto4,invburr}|class|near-constant`
  (6) and `phenomenon:near-constant-tie3|{native,double}|{burr,pareto4,invburr}|class|near-constant`
  (6).
- TASK-214.5: `refine|{native,double}|burr|archive|000638` and `...|000639` (4).

New failures: none, in `fit-judge.py`, `fit-density.py`, `fit-consistency.py`, `fit-refine.py`
and `cert-replay.py --family burr` (there is no replay for pareto4 and invburr). Entries left
for the five families: the burr scale-held 688, 689, 9424, 9884 refine entries (8, not in this
batch) and `near-constant` of paralogis and invparalogis (4, not in this batch).
`dev/validation/strict-refine.jsonl.gz` changed (cache entries added by `fit-refine.py` for
two cases). `CERT_REPLAY_COVERS_CACHE=build/cert-replay-covers.jsonl.gz`.

## Suite results

All through `/tmp/umvl-cpu/run4`, one at a time, `nice -n 10`, BLAS and OpenMP pinned to 1.

- `just cpp-test`: 110 of 110 tests passed (after the final code change).
- `just cpp-test-no-long-double`: 154 of 154 tests passed.
- `just test-changed` (85 families, 3030 configurations): exit 0 in the final run, with
  `just cpp-test` (262 s), `just test` (R, 337 s, 0 failures, 36337 passed), `just js-build`
  and `just js-test` (175 s, 586 passed). The first run had 5 R failures and the second one
  JS failure (`bindings/js/stop-test.mjs`, "Burr and reciprocal nearly-constant uncertified
  fallback"): six tests pinned the near-constant numerical stops, listed above under changed
  expectations (the JS test now expects kind `limit`, is renamed "reach their Weibull limits",
  and drops `nomination.shape1 > 1e100`).

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/burr/certificate.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp`
- `univariateML-cpp/include/cppunivariate/distributions/invburr/README.md` (one paragraph)
- `univariateML-cpp/include/cppunivariate/distributions/{burr,pareto4,invburr}/test.cpp`
- `bindings/r/distributions/{burr,pareto4,invburr}/test.R`, `bindings/js/stop-test.mjs`
- `dev/validation/strict-known.json`, `dev/validation/strict-refine.jsonl.gz`
- `dev/research/evaluations/2026-11-15-equiv-a-report.md` and
  `dev/research/evaluations/2026-11-15-equiv-a-data/{classify.py,classify-native.txt,classify-double.txt}`
