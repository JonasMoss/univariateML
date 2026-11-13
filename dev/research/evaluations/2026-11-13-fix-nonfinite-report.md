# TASK-214.3: fits that returned `Ok` with a wrong or non-finite value (fix-nonfinite)

Job `fix-nonfinite`, worktree branch `claude/fix-nonfinite` from master
`bbe1b13a`; no commits. Families changed: gamma, norm, logis, weibull, gumbel,
gompertz, cauchy (lnorm shares no fitting code with norm and is unchanged).
Evidence: [2026-11-13-fix-nonfinite-data](2026-11-13-fix-nonfinite-data/).

All nine defects, and the coordinator's addition (gompertz invalid samples,
item 10), are fixed in the core. Every case that returned `Ok` with a
wrong or non-finite value now returns either the correct estimate or a non-`Ok`
status whose outcome names the reason. The snapshots of the eight families
differ from the baselines only in the defect cases, plus three norm cases and
one held-scale logis case that have the same causes, and the gompertz
invalid-sample classes of item 10. Every changed estimate
was checked against a 50-digit reference. The outcome audit has no
disagreements and no adapter overrides left. The strict judge reports no new
failures and three fixed ones, which were pruned from the known list.

## Defects

`ref` is a 50-digit reference from the README density or the closed-form MLE
equations ([references.py](2026-11-13-fix-nonfinite-data/references.py),
[references.json](2026-11-13-fix-nonfinite-data/references.json)).
Baseline behaviour against the fix on the same inputs, in both builds:
[before-after.py](2026-11-13-fix-nonfinite-data/before-after.py) and
[before-after.txt](2026-11-13-fix-nonfinite-data/before-after.txt). The new
`test.cpp` cases fail on the baseline headers:
[new-tests-on-baseline.txt](2026-11-13-fix-nonfinite-data/new-tests-on-baseline.txt).

| # | case | before | after | outcome after |
|---|---|---|---|---|
| 1 | gamma, `(1,2,3)*denorm_min`, free | `Ok`, scale 0, logLik NaN | `TransformFailure` + tolerance detail, `attainable_tol` 2.69 | failure / tolerance |
| 2 | norm, `(1, 1, 1+2^-52)` | `Ok`, sd 1.282e-16 | `Ok`, sd 1.0467283057891836e-16 (ref ...835e-16) | estimate |
| 3a | logis, `(1e-8, 1, 1e8)`, tol 1e-10 | `IterationFailure` after 100 iterations | `Ok`, location and scale within 1.6e-14 and 4e-15 of ref | estimate |
| 3b | logis, constant data (and one observation; held location equal to every observation) | `InvalidInput` | `Unbounded` | nonexistence / unbounded |
| 4 | weibull, shape held at `denorm_min`, `(0.5,1,2,4)`, double-only | `Ok`, scale 1.4715177646857691 | `Ok`, scale `sqrt(2)` = 1.4142135623730951 | estimate |
| 5 | weibull, archive 8443, both held | `Ok`, logLik -Inf (NaN double-only) | `NumericalFailure`, logLik NaN (ref -1.83e438) | failure / evaluation |
| 6 | gumbel, archive 3008, both held | `Ok`, logLik -Inf | `NumericalFailure` (ref about -10^(3.66e149)) | failure / evaluation |
| 7 | gompertz, `(1,2,3)*denorm_min`, free | `Ok`, a = b = Inf | `NumericalFailure` (ref a 1.45e322, b 2.48e323) | failure / evaluation |
| 8 | cauchy, empty sample | `NoMLE` | `InvalidInput` | failure / invalid-input |
| 9 | gamma class `huge`, double-only | `IterationFailure` | `Ok`, as native to 3 ulps | estimate |
| 10 | gompertz, empty, NaN, ±Inf or negative sample | `IterationFailure` (no-convergence); ±Inf `Overdispersed` | `InvalidInput` | failure / invalid-input |

### 1. gamma on subnormal data

Cause. The direct branch solved the shape score with `s = log(mean) - mean(log x)`
and returned `rate = shape/mean`, `scale = 1/rate` without checks. The mean of
`(1,2,3)*denorm_min` is subnormal, the rate overflowed, the scale became 0 and
the log likelihood NaN. A second, silent problem: a subnormal sample mean is
rounded to a multiple of `denorm_min` (`(1,1,2)*denorm_min` has mean 4/3 of it,
stored as 1), so `s`, and with it the shape, was wrong even when the scale
survived.

Fix (`gamma/mle.hpp`). The direct branch runs only for a mean in the normal
range; otherwise `s` comes from relative deviations about the stored mean
(`robust_s`, exact to rounding for any centre). When the rate is not a normal
double, a new `gamma_detail::finish` forms the scale as `mean/shape` by
power-of-two scaling, with the mean `c(1+m)` taken from relative deviations,
and the log likelihood from `s` and `mean(log x)` alone. A scale below the
normal range is accepted if its relative spacing `denorm_min/scale` is at most
`min(tol, 1e-10)`. Otherwise the fit stops with `TransformFailure` and
`stop_detail.accuracy_stop = Tolerance`, `attainable_tol = denorm_min/scale`.
A scale above the range stops with `TransformFailure` alone. `GammaResult`
gained `StopDetail stop_detail`. `outcome()` maps the tolerance detail to
failure/tolerance before the status switch, so `TransformFailure` stays
failure/evaluation otherwise.

`TransformFailure` is reused, not a new enumerator: `GammaStatus` is shared
with naka and invgamma, whose `outcome()` switches have no default and are not
this job's files. The adapter renders it as "Gamma transformed observations
exceed double precision." (plain error, no override).

Answer. The scale MLE is `mean/shape = 0.37208*denorm_min` (ref
1.8383e-324): below half the smallest subnormal, so it rounds to zero, and the
nearest positive double is 2.69 times the value. That it is below half is
proved without numerics. Alzer's bound `1/(2k) < log k - psi(k) = s` gives
`k > 1/(2s)` with `s = log 2 - log(6)/3 = 0.0959`, so
`scale/denorm_min = 2/k < 4s = 0.384 < 1/2`. The 50-digit root only sharpens
this to 0.372. The fit reports
tolerance with `attainable_tol` 2.6876. Shape 5.3752094836907656 (native) and
5.3752094836907567 (double-only), against ref 5.375209483690757399 (1.5e-15 and
1.3e-16). logLik 2229.7015289366736 (ref 2229.70152893667367). Tests:
`gamma: subnormal data, scale below binary64 resolution, overflowing sums`. It
covers this case, the rounded subnormal mean `(1,1,2)*denorm_min` (shape equal
to that of `(1,1,2)` to 1e-12), and a subnormal scale still resolved to 1e-10
(`(1,2,3)*2^-1036`: `Ok`, scale `2*2^-1036/shape` to 2e-11).

### 2. norm near ties

Cause. `norm_detail::moments` profiles the sd at the rounded mean it returns.
Exactly, `sum (x - m_r)^2 = sum (x - xbar)^2 + n*delta^2` with `delta = xbar -
m_r`, so the sd was `sqrt(sd^2 + delta^2)`. On `(1, 1, 1+2^-52)` the mean rounds
to 1, `delta = 2^-52/3`, and the sd is `2^-52/sqrt(3)` instead of
`2^-52*sqrt(2)/3`.

Fix (`norm/mle.hpp`). `norm_detail::mean_rounding` computes `delta` from
residuals split exactly (TwoSum) and summed with compensation, and
`exact_center_rms` recomputes the residual RMS about `m_r + delta` when
`|delta| > 2^-27 * sd_r`. Below that the relative effect `delta^2/(2 sd_r^2)` is
under `2^-55` and the old value stands bit for bit. `norm_mle` uses it for the
free fit. The held-sd, free-mean likelihood uses it too, so its maximized value
is not lowered by the rounded mean. `moments()` itself is unchanged: sged, ged,
std, bhatt, sstd, logis and gumbel call it for starts, scaling and certificates.

Answer. sd 1.0467283057891836e-16 against ref 1.046728305789183373e-16;
logLik 106.13026066290186 (ref 106.13026066290186115). The identity is exact
algebra; the computed `delta` is accurate to rounding when the residuals are
exact. That holds by Sterbenz whenever the correction triggers: then `sd_r <
2^27 |delta| <= 2^-26 |mean|`, so every observation lies within a factor two
of the mean for any `n` below about `10^15`. Test: `norm: the sd is profiled at the
exact mean, not the rounded one`, which also covers ties offset at 1e6 and
bit-identity on ordinary data.

### 3. logis

3a. Cause. The joint Newton stops on an absolute step `max(|dmu|, |dtheta|) <
tol`. Near `mu = 2.6e7` the rounding noise of the location step (a few spacings
of the data's magnitude) exceeds 1e-10, so the target was never met and the
fit ended in `IterationFailure` after 100 iterations. The likelihood is jointly
concave in `(mu/s, 1/s)` and the MLE is unique (dossier proof). With the
adapter's default `abstol = 1.220703e-4` the same data converge; the defect
shows at small `tol`.

Fix (`logis/mle.hpp`). The Newton loop moved unchanged into
`logistic_detail::newton`. `logistic_mle` already standardized to
`(x - mean)/sd` for extreme `sd` or a large offset. It now also does so when
`tol < 2^-40 * max(|mean|, sd)`, that is when the absolute target is within
`2^12` spacings of the location scale. In standardized coordinates the same
target bounds the step relative to sd, which is the accuracy contract's bound
(location floor = scale). Refactoring removed the recursive call, so the new
trigger cannot recurse. With the default `abstol` the trigger needs
`max(|mean|, sd) > 1.3e8` on a sample not already standardized; no snapshot
case reaches it (no logis estimate changed).

Answer. `Ok`; location 26275335.368361574 (native) and 26275335.368361995
(double-only) against ref 26275335.36836199440 (1.6e-14, 2.3e-17); scale within
4e-15; logLik -57.411482623420589 (ref -57.411482623420589). The Newton
convergence is numerical (quadratic in standardized units); uniqueness is
proved in the dossier.

3b. Constant data, including one observation, returned `InvalidInput`, while
norm and the other location-scale families report the collapse. A new status
`LogisticStatus::Unbounded` (appended, raw value 3) maps to
nonexistence/unbounded. It covers constant data and, with location held, every
observation equal to that location. The dossier's `limits` entry now names
`Unbounded`. The adapter already rendered these cases as `nomle`; only
`core_status` changes in the snapshot.

3c. Same cause, held scale (archive 4719, offset 1e8, scale held at 1.008).
The location bisection stopped at width `1e-12*s`, which is below the spacing
near 1e8, so it stalled into `IterationFailure`. When the bracket reaches
adjacent doubles first, the fit now accepts it only if the computed scores at
both ends exceed the rounding bound `4 n^2 eps`. It looks outward up to four
spacings; otherwise the fit fails as before. This differs from the old code
only where the old loop could no longer move. Answer: `Ok`, location
100000000.11620703 (ref 100000000.116207034, 1e-17). Archive 4718 (spread
1e150, scale 1) still fails, by design: see Open.

Tests: `logis: an absolute tol below the location's resolution; constant data
collapse` and `logis: held scale, location bracket at adjacent doubles`.

### 4. weibull, held shape `denorm_min`, double-only

Cause. With shape held, `log scale = top + log1p(mean(expm1(k*(log x - top))))/k`.
In a double-only build the products `k*(log x - top)` are subnormal at `k =
denorm_min` and keep only absolute accuracy; dividing by `k` magnifies the
error (about `denorm_min/(2k)` = 0.5 in log scale). Native builds form the
products in long double and were correct.

Fix (`weibull/mle.hpp`). For `k < 2*denorm_min/epsilon` of the working type
(4.45e-308 for double; never for long double), `k` is divided out analytically:
`log scale = top + A*log1p(kA)/(kA)` with `A = mean(d*expm1(kd)/(kd))`,
`d = log x - top`. Both ratios are evaluated by series near zero. The form is
algebraically identical to the old one.

Answer. Scale `sqrt(2)` = 1.4142135623730951 in both builds; the 400-digit
reference equals `sqrt(2)` to 4e-79 relative. logLik -2983.1465820466447 (ref
-2983.146582046644940). Test: `weibull: subnormal held shape; unrepresentable
log likelihood at held values` (shapes `denorm_min`, 1e-310, 1e-300, 1e-20).

### 5, 6. weibull and gumbel, log likelihood beyond binary64 at held values

Cause. The held branches returned the summed log likelihood without a
finiteness check. In archive 8443 `(x/scale)^shape` is about `e^1006`; in
archive 3008 `exp(-z)` has `z` near `-8.4e149`.

Fix. A new status `NumericalFailure` (appended: weibull 5, gumbel 5) maps to
failure/evaluation. It is returned with the coefficients kept and logLik NaN
whenever the log likelihood at the returned or held coefficients is not finite.
This applies to every held branch and to the free fits (`fit_gumbel_newton`;
weibull maps gumbel's `NumericalFailure` and checks its own transformed
logLik).

Answer. The log likelihoods are -1.830e438 and about `-10^(3.66e149)` (references):
both below `-DBL_MAX`, so they are unrepresentable and this is a numerical
failure. The adapters' generic guards render it as before ("Weibull MLE
failed", "Gumbel MLE failed"), now with the core's outcome and no override.
Tests: the weibull case above; `gumbel: unrepresentable log likelihood at held
values` (with a synthetic `(-1000, 0, 1000)` pair of held values, and a finite
control).

### 7. gompertz on subnormal data

Cause. The free fit runs on `x/unit` with `unit` the sample mean and returns
`a/unit, b/unit`; with `unit = 2*denorm_min` both overflowed to Inf while the
log likelihood stayed finite, and the status stayed `Ok`.

Fix (`gompertz/mle.hpp`). After back-scaling: a coefficient that overflows (or
a non-finite logLik) gives a new `NumericalFailure` (appended, 4) with NaN
coefficients, mapping to failure/evaluation. A coefficient whose relative
spacing, computed in log space, exceeds `min(tol, 1e-10)` gives
`NumericalFailure` with a tolerance detail, mapping to failure/tolerance. The
native `huge` class keeps its subnormal but accurate coefficients (5.2e-309,
relative spacing 1e-15).

Answer. a = 1.4508e322 and b = 2.4760e323 by scale equivariance and a 50-digit
profile root (numerical): above binary64. The margin is wide, since overflow
needs only `a_z, b_z > DBL_MAX * unit = 1.8e-15` for the rescaled fit
(`a_z = 0.143`, `b_z = 2.45`). Test: `gompertz: coefficients that leave binary64
after rescaling`, which also checks exact equivariance at 1e-300.

### 8. cauchy, empty sample

Cause. `n < 2` returned `NoMLE` before the empty and finiteness checks. Fix:
an empty sample is `InvalidInput`, finiteness is checked next, and one finite
observation stays `NoMLE` (an atom with all the mass). One NaN observation now
reports `InvalidInput` instead of `NoMLE`. The dossier already restricts its
existence statement to nonempty samples.

### 9. gamma class `huge`, double-only

Cause. The sum of six values near 1e308 overflows. The fallback summed in
`CPPUNIV_EXTENDED_REAL`, which is double in a double-only build, so the mean
was Inf, `s` Inf, the relative-deviation `s` NaN, and the fit
`IterationFailure`. Fix: the fallback sums the data scaled by `2^-k` with
`2^k >= 2n`. In long double the scaling is exact, so native results are
bit-identical. Answer: `Ok` in both builds, shape 2.6999293645745452 (native)
and ...466 (double-only), within 3.8e-14 of ref 2.69992936457444438; that
residual is the legacy absolute shape step (`abstol` 1.22e-4), the same in
native. The strict judge's mode entry for this case is fixed and pruned.

### 10. gompertz invalid samples (coordinator addition)

Cause. The free fit had no data check. An empty sample reached the internal
`n == 0` guard (`IterationFailure`). NaN reached Newton and failed there. A
negative value reached the held overload's combined check, or Newton, and also
failed with `IterationFailure`, all mapped to failure/no-convergence. ±Inf
passed the moment test and returned `Overdispersed`, a claimed exponential
limit, which the adapter re-labelled invalid-input.

Fix (`gompertz/mle.hpp`). A new status `GompertzStatus::InvalidInput`
(appended, raw value 5) maps to failure/invalid-input. Both public
overloads return it for an empty sample or any observation that is not
finite or is negative (outside the support `x >= 0`). Negative data go
slightly beyond the request "empty and non-finite"; they are the same
category and ended in the same no-convergence status. Invalid held values or
controls keep `IterationFailure` in the core; the adapter rejects them before
the call. All-zero data are still `Unbounded`. Test: `gompertz: empty,
non-finite and negative samples are invalid input` (free, `a` held, `b`
held).

## Changes to existing tests

Two existing assertions encoded defects the brief lists, and were narrowed
rather than deleted:

- `logis: invalid inputs never report success` expected `InvalidInput` for
  `{1}` and `{1,1}`. These now assert `Unbounded`; `{}` and `{0, Inf}` keep
  `InvalidInput`.
- `cauchy: three observations and nonexistent MLE` expected `NoMLE` for `{}`.
  The loop now covers the nonempty inputs only; the empty sample is asserted
  as `InvalidInput` in the appended case.

All other new cases are appended at the end of each `test.cpp`.

## Snapshot differences

Commands as in the brief, workstation, GCC 15.2.0
([native provenance](2026-11-13-fix-nonfinite-data/native-provenance.json),
[double provenance](2026-11-13-fix-nonfinite-data/double-provenance.json)).
Native: 17 of 896 cases differ; double-only: 18 of 896
([native diff](2026-11-13-fix-nonfinite-data/native-diff.log),
[double diff](2026-11-13-fix-nonfinite-data/double-diff.log)).

| family | case | modes | change | reason |
|---|---|---|---|---|
| cauchy | class `empty` | both | `nomle` -> error, nonexistence -> failure/invalid-input | defect 8 |
| gamma | class `subnormal` | both | status 0 -> 4, outcome reason evaluation -> tolerance, override cleared; message "Gamma MLE failed" -> "Gamma transformed observations exceed double precision." | defect 1 |
| gamma | class `huge` | double | error -> `Ok` | defect 9 |
| gompertz | class `subnormal` | both | status 0 -> 4, override cleared (kind and message unchanged) | defect 7 |
| gompertz | class `empty`, `nan`, `negative` | both | status 2 -> 5, outcome reason no-convergence -> invalid-input | item 10 |
| gompertz | class `inf`, `neginf` | both | status 1 -> 5; message "escapes to an exponential ... Use `mlexp` instead." -> "Gompertz MLE failed to converge or data exceed double precision."; outcome unchanged (invalid-input) | item 10 |
| gumbel | archive 3008 | both | status 0 -> 5, override cleared | defect 6 |
| weibull | archive 8443 | both | status 0 -> 5, override cleared | defect 5 |
| logis | class `constant`, `one`, `zeros` | both | `core_status` 1 -> 3 only | defect 3b |
| logis | archive 4719 | both | error -> `Ok` | 3c, same cause as 3a |
| norm | class `near-constant` | both | sd 2.0031e-18 -> 1.6355e-18 | defect 2 |
| norm | class `offset` (data near 1.3e15) | both | sd 0.84779 -> 0.84677 | defect 2: mean spacing 0.25, `delta` 0.042 |
| norm | archive 5268 (`constant-1-1e-12`) | both | sd relative change 8.3e-11 | defect 2 |

Every changed estimate and log likelihood was checked against 50 digits
([check-snapshot-diffs.py](2026-11-13-fix-nonfinite-data/check-snapshot-diffs.py),
[snapshot-diff-check.json](2026-11-13-fix-nonfinite-data/snapshot-diff-check.json)):
new relative errors are at most 4e-16, except gamma `huge` (3.8e-14, legacy
step tolerance). Baseline norm sd errors were 0.22 (near-constant),
1.2e-3 (offset) and 8.3e-11 (5268).

Dependants. std, bhatt, emg, lst, huber, betapr, genbeta, lgamma, expweibull,
burr, invweibull, naka, invgamma and gb2 were snapshotted in both builds
against the same baselines: 2 of 2611 cases differ natively, 3 double-only
([native](2026-11-13-fix-nonfinite-data/dependants-native-diff.log),
[double](2026-11-13-fix-nonfinite-data/dependants-double-diff.log)).

- huber archive 3333 and 3336 (both builds): only the normal-limit candidate
  inside a stop that is unchanged (`Huber MLE failed to converge`): its sd
  moves to the exact-mean MLE (relative 4e-10 and 8e-11), and the candidate's
  log likelihood and `loglik_lower` by rounding only.
- betapr class `huge` (double-only): `loglik_lower` -4260.62 -> -4258.54, now
  equal to the native value, because the gamma face fits in the double-only
  build. The stop is unchanged.

## Outcome audit and strict judge

`--outcomes` on both snapshots: all 8 families report an outcome on every
case; 0 disagreements; no adapter overrides
([native](2026-11-13-fix-nonfinite-data/native-outcomes.txt),
[double](2026-11-13-fix-nonfinite-data/double-outcomes.txt)). The baseline had
one override each in gamma, gompertz, gumbel and weibull.

`fit-judge.py build/snap/native build/snap/double`: 0 new failures; 3 listed
failures pass and were pruned from `dev/validation/strict-known.json`
(`invalid-input` cauchy `empty` in both builds, `mode` gamma `huge`;
[before prune](2026-11-13-fix-nonfinite-data/judge-before-prune.log),
[after](2026-11-13-fix-nonfinite-data/judge.log)). The one remaining failure
for these families is the listed `mode|both|gompertz|class|huge`
(TASK-214.13).

## Suites

All runs on the workstation (GCC 15.2.0), through the slot locks; simbox was
not used (unreachable, per the job instructions)
([suites.log](2026-11-13-fix-nonfinite-data/suites.log)). The counts equal
those `dev/STATUS.md` records for master `c167a975` after the 16-family
removal; the brief's figures (136, 31515, 641) predate that removal.

| command | result |
|---|---|
| `just test-changed master --no-long-double` (unfiltered, since `strict-known.json` is shared) | all five recipes exit 0 |
| `just cpp-test` | 109/109 |
| `just cpp-test-no-long-double` (workstation, GCC 15) | 153/153 |
| `just test` (R dev tier) | `[ FAIL 0 \| WARN 0 \| SKIP 157 \| PASS 28842 ]` |
| `just js-build`, `just js-test` | built; 578/578 |
| `just test-full` (R full tier) | `[ FAIL 0 \| WARN 0 \| SKIP 1 \| PASS 57977 ]` |
| `just quality gamma norm lnorm logis weibull gumbel gompertz cauchy` | all 8 pass ([quality.log](2026-11-13-fix-nonfinite-data/quality.log)) |
| `just site-check` | pass ([site-check.log](2026-11-13-fix-nonfinite-data/site-check.log)) |
| `python3 dev/tools/check_links.py` | 0 new broken links |
| goldens | unchanged (no `golden.json` touched; golden fits identical in the snapshots) |

The scratch doctest binaries of the eight families (all cases, slow ones
included) pass in both builds: 185/185 cases before the gompertz addition.
After it, gompertz's 19/19 cases pass in both builds. The gompertz change
came after `just cpp-test` had compiled. Its doctest shard was rebuilt and
rerun in both builds, together with the eight native shards whose
translation units include it (fatigue, beta, burr, ged, invburr, kumar, snorm,
std): all pass. The R dev tier and the JS build ran after the change, since
their assembly copies the headers then. So did `just test-full`. Only
comments in `gumbel/mle.hpp` and `gumbel/test.cpp` changed after that run.
The final snapshots, outcome audits and judge run were retaken on the final
sources and are identical to the previous run.

## Adapter notes (for the render job)

No adapter was edited.

- gamma: the final guard (`overridden(failure("Gamma MLE failed"))`) is no
  longer reached by any snapshot case. Every non-finite core result now
  carries a non-`Ok` status. The below-resolution scale arrives as
  `TransformFailure` with `stop_detail.accuracy_stop = Tolerance`; a rendered
  tolerance stop would read `GammaResult::stop_detail` (new).
- weibull, gumbel, gompertz: `NumericalFailure` has no branch of its own. It
  reaches the generic non-finite guards ("Weibull MLE failed", "Gumbel MLE
  failed", "Gompertz MLE failed to converge or data exceed double
  precision."), which now render a core failure instead of overriding a core
  success. In gompertz, a tolerance detail can accompany it.
- gompertz: an empty sample is still rendered as "The Gompertz likelihood is
  unbounded for all-zero data with a free.", because the adapter's all-zero
  test is true for an empty range. The outcome is now invalid-input from the
  core. Infinite data no longer get the `mlexp` advice. The adapter guard that
  turned `Overdispersed` on invalid data into an invalid-input outcome is no
  longer reached for these classes.
- logis: the adapter's constant-data and held-location collapse checks now
  duplicate the core's `Unbounded`. The `done` conversion "`nomle` without a
  nonexistence outcome" is no longer needed for these inputs.
- cauchy: the empty sample now gets "Cauchy fit requires finite data, positive
  tolerance and iterlim, and valid held coefficients."

## Open

- logis, held scale, archive 4718 (data spread 1e150, scale 1): still
  `IterationFailure`, which is correct but not an answer. Every tanh term
  saturates, so the computed score is exactly zero between the 6th and 7th
  order statistics. The MLE lies near their midpoint, to `O(s)`. Finding it
  needs a sign test in log space with exact (double-double) differences
  `x_i - mu`. Relaxing the stopping rule without that would return the edge
  of the flat region, a wrong `Ok`. The sign certificate prevents this.
- gompertz class `huge`, double-only build (TASK-214.13, left listed): the
  same mechanism as defect 9. The double-only sum overflows, the rescaling is
  skipped, and the moment test selects the exponential face. The fix pattern
  is the same scaled sum. Not done here: it is outside this brief and listed
  under TASK-214.13.
- bhatt: `normal_member_certificate` takes `norm_detail::moments(x).sd` as
  the free-location normal member's sd, which is the rounded-mean profile of
  defect 2 (the sd is too large near ties or at large offsets). It is not
  this job's file. Switching it to `norm_detail::exact_center_rms` would
  align it with norm.
- gamma held overloads on subnormal data still stop with `IterationFailure`
  when the rate overflows. That is honest, but its reason is NoConvergence,
  not tolerance.
- The logis trigger constant `2^12` spacings is a noise margin, not a proof:
  the Newton step noise is a few spacings of `max|x - mu|` near the
  solution. The absolute-step semantics of `abstol` itself belong to step 5c
  (uniform `tol`).
