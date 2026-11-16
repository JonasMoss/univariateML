# Near-constant maximizers of paralogis, invparalogis and fatigue (TASK-214.39 part 1)

Branch `claude/near-constant-fix`, from master `2c6015a6`. Not committed. Source analysis:
[near-constant report](2026-11-15-near-constant-report.md) (TASK-214.38). Data and scripts:
[2026-11-16-near-constant-fix-data](2026-11-16-near-constant-fix-data/) (`reference.py`, `verify.py`, `classify.py`, and their outputs).
Decision followed: logLik is the log likelihood at the returned coefficients ([decisions](../../roadmap/decisions.md)).

## Cause and fix per family

Common cause (the report's "What the fit cannot do in doubles"): the likelihood varies on the scale of one ulp of the scale
coefficient (`alpha * ulp(theta)` is about 2 for paralogis), so a fitter that evaluates doubles in absolute coordinates cannot reach
the maximizer. Before the fix, the existing polisher `paralogis_local_accuracy` (offset coordinates, `paralogis/accuracy.hpp`) was
never reached because the BFGS proposal had already failed, and where it was reached its Newton stop is relative `1e-10` in `theta`,
which is `alpha * 1e-10` in the offset coordinate: the scale it returned was one ulp off the rounded maximizer (log likelihood 73.98
against 107.23 at the correctly rounded point on the stored paralogis sample). So a new solver was written; the old polisher is unchanged.

### paralogis and invparalogis, `paralogis/offset.hpp` (new, 181 lines), shared

Method. With `c = min(x)`, `r_i = sigma log1p((x_i - c)/c)` (`sigma = +1` paralogis, `-1` inverse; the subtraction is exact for
`max/min <= 2`) and `w_i = alpha (r_i - mu)`, `theta = c exp(sigma mu)`: the log density is
`2 log(alpha) + w_i - log x_i - (alpha + 1) log1p(e^{w_i})` in both families (the inverse differs only by the sign of `w`).
The solve is in `(alpha, z)`, `w_i = alpha (r_i - mean r) - z`, which is O(1).

- `solve_z` (`:68`): for fixed alpha the likelihood is concave in `z` (score `sum p_i = n/(alpha+1)`, `p_i` logistic of `w_i`): one
  root, safeguarded Newton on `log(sum p_i)` with a bisection bracket.
- `fit` (`:102`): geometric grid `alpha * range` in `1.5^k`, `k = -8..30`, the grid maximum bracketed by neighbours with opposite
  signs of the profile score, bisection of the profile score on `alpha` (not on `log alpha`, so `alpha` is resolved to the last bit),
  check that the profile is concave at the root. Sums, `exp`, `log1p` run in `ExtendedReal` (long double; double under
  `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`); `z` is kept in extended precision until it is converted to the scale.
- The scale is rounded once: `theta = fma(c, expm1(sigma mu), c)`. `alpha` is the maximizer's, not re-optimized at the rounded
  scale. `logLik` is evaluated at the returned `(alpha, theta)` from `log1p((theta - c)/c)`.
- Route gate: `max/min - 1 <= 1e-6` (`kMaxSpread`, `:29`). At that spread `alpha` is about `1e6` and one ulp of `theta` moves the
  conditional `alpha` by about `1e-10`, the default `tol`. If the solver fails (grid maximum at an edge, no sign change, profile
  not concave) it returns nothing and the previous path runs unchanged.
- `paralogis/mle.hpp:321-332` (`paralogis_mle`, before `paralogis_mle_impl`) and `invparalogis/mle.hpp:59-70`
  (`invparalogis_mle`, before the reciprocal vector is formed; the reciprocal rounding is the same equivariance defect as in invburr).
  Held-scale and held-shape routes are unchanged. `shape0` is ignored on the route.

Certification: uncertified (`certified = false`, outcome `estimate`, flagged). The argument bounding the coefficients is numerical:
a grid scan finds the profile maximum, a score root is bisected, the profile is concave there. It does not exclude another
maximum outside the grid, and uniqueness is not proved (dossier: unknown). The existing compact-domain certificate does not close
in this regime (the original stop stays reachable if the solver fails). `stop_detail` carries the point as a candidate with
`loglik_lower = loglik_upper = logLik` and `loglik_upper` of the global bound `INFINITY`.

Result on the stored samples (verified at 100 digits, `verify.py`; `reference.py` for the supremum):

| | paralogis | invparalogis |
|---|---|---|
| returned shape | `0x1.06001630c25d2p+54` = 18436634801379144 | `0x1.58ecca37b168ap+54` = 24271936312597032 |
| 100-digit shape | 18436634801379144.73 | 24271936312597031.36 |
| scale | `0x1.ef3095fc55f2ap-1` (maximizer rounded once, 18.28 ulp above c) | `0x1.b024c564354b8p-1` (11.70 ulp below c) |
| logLik at returned point | 107.2277 | 107.6098 |
| supremum | 107.8750 | 108.6557 |
| loss | 0.647 | 1.046 |

In double-only mode the shape is on the stored near-constant samples within `1.7e-14` (paralogis) and `4.0e-14` (invparalogis) of the maximizer (native `4e-17`, `3e-17`), same scale.

### fatigue (mu held), `fatigue/certificate.hpp:14-54` `offset_fit`, called at `:87`; `fatigue/mle.hpp:33,64`

Cause confirmed: `fatigue_certificate::fit` returns at the branch "A/H can round together" with the witness `value(A)`, which has the
best alpha at `beta = A`.

Method. With `c = min(x)`, `l_i = log1p((x_i - c)/c)`, `beta = c e^u`, `e_i = l_i - u`:
`log L = -sum log x_i + sum log cosh(e_i/2) - n log alpha - (n/2) log 2 pi - sum 2 sinh^2(e_i/2)/alpha^2`,
`alpha^2 = sum 4 sinh^2(e_i/2)/n` at the maximum, profile score `-sum tanh(e_i/2)/2 + n sum sinh(e_i)/sum 4 sinh^2(e_i/2)`,
positive at `u = min l`, negative at `u = max l` (checked; no root bracket otherwise returns nothing). The root is bisected
to the last bit; `beta = fma(c, expm1(u), c)`; `alpha` is the maximizer's; `logLik` is evaluated at the returned coefficients.
Taken only in the branch where the old code stopped (both coefficients free, `A` and `H` within `128 eps`), and only if
`max <= 2 min`; otherwise the old stop remains.

Certification: uncertified. `FatigueResult` gained `bool certified = true` (`mle.hpp:33`), `outcome()` maps `Ok` to
`estimate(r.certified)` (`:64`); the offset solve sets it false. The argument is numerical (bisection of a monotone-sign-changing
score on the bracket `[min l, max l]`, which contains `[H, A]` by the dossier proof; uniqueness is not proved).

Result on the stored sample: `alpha = 0x1.cf8db108c016cp-54` = 1.00517138574689838e-16 (100-digit reference identical to all printed digits),
`beta = 0x1.0a95766f46a5ep+1 = c` (the maximizer is `c + (c+ - c)/3`), `logLik = 103.3008` at the returned point, supremum 104.0508,
loss 0.75. Old candidate alpha was 22% too large.

## Tests

Added (none removed), values from `native.jsonl.gz` via `strict_common.load` (`reference.py` does the loading) and 120-digit maximizers:

- `paralogis/test.cpp` "paralogis near-constant sample returns the rounded maximizer, uncertified": `Ok`, `certified == false`, outcome `estimate`,
  shape and scale within `1e-10` relative, scale equal to the rounded maximizer, logLik 107.2277 at the returned point.
- `invparalogis/test.cpp` "invparalogis near-constant sample returns the rounded maximizer, uncertified": same, logLik 107.6098.
- `fatigue/test.cpp` "fatigue near-constant sample with mu held returns the rounded maximizer, uncertified": alpha within `1e-10`, `beta == c`, logLik 103.3008.

Before the fix (headers stashed, tests kept): paralogis `REQUIRE status == Ok` failed (status 1, BfgsFailure); invparalogis the same;
fatigue status 3 (SearchFailure) from a probe on the same call (the test file did not compile before the fix because `FatigueResult::certified` did not exist).
After: all pass, both precision modes.

Expectations changed (each pinned the old numerical stop of a near-constant sample; changed only to the new correct outcome,
verified against 100-digit maximizers: shape within `2e-17` and scale within 0.41 ulp for the pair, exact for the five points):

- `invparalogis/test.cpp` "invparalogis: rounded reciprocal ties do not imply nonexistence" (`x = {1.9, nextafter(1.9)}`): `NumericalFailure` and empty
  candidates became `Ok`, uncertified, shape `2.0530914653470480e16`, scale `1.8999999999999965` (references from `chk` at 100 digits), the
  `CHECK_FALSE(isinf(loglik_lower))` kept.
- `fatigue/test.cpp` "fatigue certificate: nearly coincident mean bounds stop numerically" (`x_i = 1 + k eps`, k = 0..4), renamed "... are solved in an offset
  coordinate": `SearchFailure` and one candidate became `Ok`, uncertified, `alpha = sqrt(2) eps`, `beta = 1 + 2 eps`; the
  `comparison_tie` and `budget_exhausted` checks kept, the single-candidate check kept (the offset result lists itself).
- `bindings/r/distributions/fatigue/test.R` ("fatigue handles ties and extreme scales without invalid estimates"): `expect_error(..., class = "univariateML_numerical")`
  for that sample became estimates of `alpha` and `beta` as above.
- `bindings/r/distributions/invparalogis/test.R` ("reciprocal rounding ties ..."): numerical-stop expectation became the estimate for the pair above;
  the title now reads "are neither nonexistence nor a numerical stop".
- `bindings/js/test.mjs` "fatigue certificate stops when rounding hides alpha resolution": now expects the estimates.

## Snapshot diff

Families: paralogis, invparalogis, fatigue (the only includers of the changed headers besides `cppunivariate.hpp` and the adapters).
Command as given, native and `--no-long-double`, kept in `build/snapshots/{native,double}`. Baselines: `2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`.
`classify.py` output in `classify-{native,double}.txt`, per-case check at 100 digits in `verify-{native,double}.txt`.
Native and double agree in every kind change.

| family | cases | identical | changed | kind changes | `Ok` to stop |
|---|---|---|---|---|---|
| paralogis | 104 | 98 | 6 | 4 | 0 |
| invparalogis | 116 | 100 native / 98 double | 16 native / 18 double | 7 | 0 |
| fatigue | 116 | 109 | 7 | 7 | 0 |

Changed cases (old to new). Every kind change is `numerical` failure `no-convergence` to an uncertified `estimate`:

- paralogis: `class near-constant`, `class offset`, archive 5389, 5392. Ok to Ok: archive 5388 (shape moved `2.0e-13`; old was `2.0e-13` from the
  maximizer, new `3e-17`), 5391 (old `8e-16`, new `1.7e-16` from the maximizer).
- invparalogis: `class near-constant`, `class offset`, archive 3914, 3915, 3917, 3918, 10534. Ok to Ok on the route: archive 10533 (old shape `2.0e-17` from the
  maximizer, new `1.9e-16` native and `6.1e-15` double-only; all within `tol`). Held-shape cases that differ from the stored baseline and are not on the route:
  archive 3927, 3938 to 3944 (scale only, up to `3.1e-15`) and, in double-only mode, 3934 and 3937 (one ulp of `logLik`; held shape `1.0292870573482298`).
  For the eight native cases, a probe with the master headers (`git archive HEAD`) returns the new values bit for bit, so the stored baseline differs from master
  there and this change does not cause it. The two double-only ones were not probed; they use the same held-shape code.
- fatigue: `class near-constant`, `class offset`, archive 1438, 1439, 1441, 1442, 1449.

All 21 estimates on the route (both modes) are within `2.3e-16` (native) and `4.0e-14` (double-only, invparalogis near-constant) relative of the 100-digit
maximizer in shape/alpha and within `1.1e-16` in scale/beta; their losses against the supremum are the rounding loss (near-constant 0.647, 1.046, 0.75;
offset 0.0023, 0.0026, 0.036; archive cases `1e-8` to `1e-18`). An `Ok` that becomes a stop: none. No stopped case moved.
`fit-density`: reported against summed logLik differ by at most `1.2e-13` (paralogis native), `4.7e-14` (fatigue).

## Strict layer

From `dev/validation`, snapshots as above, `--prune` only, `CERT_REPLAY_COVERS_CACHE=build/covers-cache.json`.

- `fit-judge.py`: 336 cases in two modes, no failures.
- `fit-density.py`: no failures.
- `fit-refine.py --family paralogis,invparalogis,fatigue`: 120 cases judged, no failures.
- `fit-consistency.py`: no failures; six listed entries passed and were pruned.

Removed from `strict-known.json` (the TASK-214.33 near-constant entries of these families, native and double):
`phenomenon:near-constant|{native,double}|{paralogis,invparalogis,fatigue}|class|near-constant`.

- `cert-replay.py --family paralogis,invparalogis,fatigue`: **18 new failures**, all `cert-replay:cover`, none listed (I did not `--add`). The cases:
  paralogis 5389, 5392, `class near-constant`, `class offset`; invparalogis 3915, 3918, 10534, `class near-constant`, `class offset`; each in both modes.
  They are the new uncertified estimates whose rounding loss exceeds the cover check's slack: the check demands `L(a, .) < L(returned)` for every `a`
  outside a window of relative half width `1e-6` around the returned shape, with `L(returned)` the log likelihood at the *rounded* point, so a profile
  maximum at the window edge exceeds it by the rounding loss (near-constant paralogis: 107.7749 against target 107.1276; archive 5389: `1.7e-9`).
  The fits are flagged uncertified and the check says it is an independent check. The same check passes for the other changed cases
  (invparalogis 3914, 3917, 10533; paralogis 5388, 5391) whose rounding loss is below `1e-17`. Fatigue: all offset results are uncertified `ok`,
  which the replay does not judge ("no certified outcome"); five fatigue archive cover cases listed as open (1458, 1459, 1461, 1462, 9817) are unchanged from before.
  Decision for review: list the 18 under a card, or let the cover check compare against the profile at the rounded maximizer.

### Review decision: cover target (coordinator, 2026-11-16)

The cover target for both coefficients free is now `max(L(returned), P(a_hat))`, the larger of the log
likelihood at the returned point and the lower enclosure of the profile at the returned shape
(`dev/validation/cert_replay_covers.py`, `replay_paralogis`; `VERSIONS` paralogis and invparalogis 2).
The profile value is attained inside the excluded window (at the unrounded log scale), so "every shape
outside the window lies below it" still places the global profile maximum inside the window; it is a
tighter attained value than `L(returned)`, not a looser bound. On ordinary fits the two agree to the
enclosure width; on near-constant fits they differ by the rounding loss of the scale (decision "logLik
is the log likelihood at the returned coefficients"). The held-scale branch keeps `L(returned)`.

Rerun on this branch's snapshots (`cert-replay.py --family paralogis,invparalogis,fatigue`, both modes,
456 cases): no failures. The 18 cover cases above now hold, except invparalogis `class near-constant`
(both modes), which is open on the evaluation budget: the profile is flat to `2e-12` just outside the
window (largest value seen 108.147030537474, target 108.147030537476). The five fatigue archive cover
cases stay open as before.

## Suites

One at a time through `/tmp/umvl-cpu/run4`, `nice -n 10`, `CMAKE_BUILD_PARALLEL_LEVEL=4`:

- `just cpp-test`: 110 of 110 passed.
- `just cpp-test-no-long-double`: 154 of 154 passed.
- `just test-changed` (selected 85 families because of shared headers; runs `cpp-test` and `test`): first run, `cpp-test` passed, R `test`: 2 failures,
  `test-dist-fatigue.R:28` and `test-dist-invparalogis.R:140`, the two R tests listed above that pinned the old stops. After updating them: see the end of this section.

Second run, after the two R expectations (and the JS one) were changed: exit 0. Its stages: `cpp-test` passed, R `test` passed, `js-build` and `js-test` passed (the JS test of the fatigue sample ran).

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/paralogis/offset.hpp` (new)
- `univariateML-cpp/include/cppunivariate/distributions/paralogis/mle.hpp`, `invparalogis/mle.hpp`, `fatigue/mle.hpp`, `fatigue/certificate.hpp`
- `paralogis/README.md`, `invparalogis/README.md`, `fatigue/README.md` (the proposed `existence.statement` text: the near-constant finite maximizer, the loss at rounded
  coefficients, the offset solve and the uncertified flag; the rounding of `beta` for fatigue)
- `paralogis/test.cpp`, `invparalogis/test.cpp`, `fatigue/test.cpp`, `bindings/r/distributions/fatigue/test.R`, `bindings/r/distributions/invparalogis/test.R`, `bindings/js/test.mjs`
- `dev/validation/strict-known.json` (six entries removed)
- `dev/research/evaluations/2026-11-16-near-constant-fix-report.md` and `2026-11-16-near-constant-fix-data/`

## Not done

- No certificate: the grid scan and the score root bound the maximizer numerically only; uniqueness of the joint maximizer is not proved for any of the three.
- Only both coefficients free (paralogis, invparalogis) and mu held with alpha, beta free (fatigue) are routed. Held-scale and held-shape routes and fatigue with alpha held are unchanged.
- Spread above `1e-6` (paralogis, invparalogis) and above `max = 2 min` or outside the A/H collapse (fatigue) keep the previous paths.
