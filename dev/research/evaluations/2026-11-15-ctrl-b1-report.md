# Step 5c job 2a: B-abstol families get `tol = 1e-10` (2026-11-15)

Card TASK-150.1.2, consolidation step 5c, job 2 of the
[controls inventory](2026-11-14-controls-inventory.md) (section 5), minus binom.
Families: cauchy, gamma, invgamma, lgamma, llogis, logis, naka. Branch
`claude/ctrl-b1`, uncommitted. Causes and fixes follow the
[refine-causes report](2026-11-15-refine-causes-report.md); job 1 is the
[ctrl-a report](2026-11-15-ctrl-a-report.md). Scripts and outputs for the
snapshot diff are in [the data folder](2026-11-15-ctrl-b1-data/).

## Per family

Every family: `abstol = 1.220703e-4` becomes `tol = 1e-10` in the dossier
`r.control` (README.md), `bindings/common/distributions/<id>/fit.hpp`,
`bindings/r/distributions/<id>/adapter.cpp`, the R tests, and the generated
`bindings/js/family-types.d.ts` (`just gen`). The `mle.hpp` parameters were
already named `tol`. Rules below say "old" for master at 318b2fd3.

| family | old rule | new rule | default |
|---|---|---|---|
| gamma | `gamma/mle.hpp:187` `newton_raphson_1d_abs` (`core/newton.hpp:51`): absolute step below `abstol` after applying it | `gamma/mle.hpp:193` `newton_raphson_1d` (the existing relative helper): $\lvert\text{step}\rvert < tol\,k$, step applied. Bracket path unchanged | `1e-10` |
| logis | `logis/mle.hpp:181`: $\max(\lvert d\mu\rvert,\lvert d\theta\rvert) < abstol$ after the damped step; strict cost test `cost(cand) <= old` | `logis/mle.hpp:200`: stop on an accepted, undamped step in the Newton direction with $\max(\lvert d\mu\rvert/\max(\lvert\mu\rvert,s),\lvert d\theta\rvert)\le tol$, step applied. Acceptance `cost(cand) <= old + 64 eps max(\lvert old\rvert, n)` | `1e-10` |
| cauchy | `cauchy/mle.hpp:201`: same as logis | `cauchy/mle.hpp:220`: same as logis | `1e-10` |
| invgamma, lgamma, naka | via `gamma/transformed.hpp:99,116`: relative bracket $hi/lo-1\le\min(tol/8, 4\varepsilon)$ (log bracket $hi-lo\le\min(tol/8,2\cdot10^{-13})$ with the rate held) | unchanged (already relative; the default tol only reaches the `restored` and mean-error gates, `:152-155`) | `1e-10` |
| llogis | via `core/transformed_location.hpp:25`: bracket $hi-lo\le\min(tol/16,4\varepsilon)$ in standardized log coordinates | unchanged; default reaches the `restored` and coefficient-round-trip gates | `1e-10` |

Justification, as written in the code comments:
- gamma: the score $\log k-\psi(k)-s$ is strictly decreasing and convex, so
  the Newton step estimates $k-k^*$ to first order and the error after the
  applied step is $O(\text{step}^2)$; $k>0$, so a relative rule is the
  contract's $tol\,\lvert k^*\rvert$. The scale is the closed form
  $k/\bar x$.
- logis, cauchy: the Newton direction is used only when $H_{mm}>0$ and
  $\det>0$ (positive definite observed information), so $H^{-1}g$ estimates
  the error of the iterate before the step, to second order; after the
  applied step the error is of order $\text{full}^2$. The location is
  measured against $\max(\lvert\mu\rvert, s)$ and the log-scale step is the
  relative scale step: the contract's $s_i$ floor for a location is the
  fitted scale. A damped or fallback (preconditioned-score) step never stops
  the loop.
- bracket families: the root lies in the bracket by computed signs; the
  midpoint is within the relative width. Added as comments in
  `gamma/transformed.hpp` and `core/transformed_location.hpp`.

Step-acceptance defect (cauchy and logis): fixed as in the refine-causes
report: noise-tolerant acceptance and convergence tested on the undamped step.
The cauchy case 000764 (class c: the damped step falsely signalled
convergence) is fixed, see the strict entries.

The standardization threshold in `logistic_mle` (`tol < 2^-40 max(|mean|, sd)`)
is unchanged; with the default $10^{-10}$ it now fires when the location scale
exceeds about 110, so more fits run in standardized coordinates. Its comment
was reworded. With the location step measured against $\max(\lvert\mu\rvert,s)$
the original reason (location noise vs an absolute target) no longer applies;
the standardization is harmless but could be removed (not done).

## Snapshot moves

Native and `--no-long-double` snapshots of the seven families (798 cases per
mode) against `2026-11-15-fit-snapshot-data/`. No case is added or lost.
Output: `2026-11-15-ctrl-b1-data/diff-native.txt`, `diff-double.txt` (script
`snapshot-diff.py`). Max relative change per coefficient over ok to ok cases
(a location is measured against $\max(\lvert\mu\rvert, s)$); native and double
agree to the digits shown except where noted.

| family | changed / cases | max rel change | worst case | max rel change in logLik | kind changes |
|---|---|---|---|---|---|
| gamma | 28 / 124 | shape 4.64e-6, scale 4.64e-6 | wide | 2.7e-12 | none |
| cauchy | 33 / 101 | location 1.8e-8, scale 9.03e-8 | huge | 5.5e-14 | none |
| logis | 40 / 101 | location 6.67e-9, scale 3.31e-9 | n7-s2, 004678 | 1e-15 | none |
| lgamma | 29 / 108 | shapelog 2.7e-13, ratelog 2.7e-13 | tie3 | 3.2e-14 native, 2.5e-14 double | none |
| llogis | 7 / 116 | 0 | | 0 | 1 |
| naka | 1 / 124 | 0 | | 0 | 1 |
| invgamma | 0 / 124 | 0 | | 0 | none |

Kind changes (both modes): `llogis|class|offset` and `naka|class|offset`:
`ok`/`estimate`, certified `true` becomes certified `false`. The estimates and
the log likelihood are bit-identical. Both samples are six points at offset
$10^{15}$ spread; the fitted shape is $\approx10^{15}$ (llogis) and $10^{29}$
(naka). The flag comes from `restored(ideal, actual, tol, ...)`
(`core/transformed_data.hpp:81`, called at `gamma/transformed.hpp:152` and
`core/transformed_location.hpp:90`), which compares the likelihood cost of
rounding the public coefficients with `tol`: at `tol = 1.22e-4` it passed,
at `1e-10` it does not. The `Ok` is kept and now carries the uncertified flag,
as the code comments for that gate intend. I read this from the code; I did not
measure the likelihood gap. No case turned `Ok` into a stop.

The gamma move (4.6e-6) is where the old absolute-step rule stopped early on a
large shape; the others are at or below $10^{-7}$.

## Strict layer

Run on the two snapshot dirs (`--prune` only): `fit-judge` 0 failures, 0 known;
`fit-density` 0 failures (max $4.3\cdot10^{-14}$ relative in any family);
`fit-consistency` 0 failures; `fit-refine` 294 cases judged, 0 new failures, 10
listed entries passing; `cert-replay` (all seven families) 980 cases, 22 known,
0 new, 0 listed but passing.

Entries that left `strict-known.json` (all TASK-214.5, all `refine`, 10 = 5 per
mode):
- `refine|{native,double}|gamma|archive|001977`
- `refine|{native,double}|gamma|archive|001991`
- `refine|{native,double}|logis|archive|004689`
- `refine|{native,double}|cauchy|archive|010216`
- `refine|{native,double}|cauchy|archive|000764`

`fit-refine` leaves open (not failures) 6 gamma, 2 invgamma, 2 lgamma, 2 naka
cases ("scaled line search failed"); these are reference-side limits.
`cert-replay` reports 6 OPEN logis `root` cases per mode (hp "matrix is
numerically singular": archive 004685, 004686, class huge, subnormal,
subnormal-signed, tiny). The same 6 appear with the old `cert_replay_roots.py`
on the new snapshot; I did not run the old code on the old snapshot.

`dev/validation/cert_replay_roots.py` was updated: default `TOL` of the six
families is now 1e-10 (with new `fit.hpp` names), the gamma Newton claim is a
relative bracket $k(1\pm tol)$, and the logis claim measures $\mu$ against
$\max(\lvert\mu^*\rvert,s^*)$ with `tol`. Claim strings and source line
references were updated for gamma and logis.

## Tests

All through `/tmp/umvl-cpu/run4`:

| suite | result |
|---|---|
| `just quality cauchy gamma invgamma lgamma llogis logis naka` | all pass |
| `just cpp-test` | 110/110 (after the two test edits below) |
| `just cpp-test-no-long-double` | 154/154 |
| `just test-changed` | cpp-test 110/110; R `[ FAIL 0 \| WARN 0 \| SKIP 157 \| PASS 36337 ]`; `js-build` ok; JS 586/586 |

First `just cpp-test` run: `distribution_gamma` and `distribution_lgamma` failed.
They passed `mltest::kFitTol` (the shared CRAN-era 1.220703e-4, used by 70 call
sites) and compared against the frozen golden at $10^{-15}$. The golden is the
true MLE (its 50-digit reference agrees to $10^{-16}$), so the failure was
the old loose rule stopping to the golden by luck and the relative 1.22e-4
rule not. No test deleted or weakened: gamma/test.cpp:13 and
lgamma/test.cpp:18,115,116 now pass `1e-10` explicitly (same tolerances in
the expectations). `kFitTol` itself is unchanged; other families' tests use it.

Renamed in other families' tests: `abstol` to `tol` in the invtrgamma
(`mlinvgamma`), trgamma (`mlgamma`) R tests, `test-stops-cont.R`
(`mlgamma(abstol=0)`), `bindings/js/stop-test.mjs`, `bindings/js/test.mjs`
(`llogis`). Tests of the seven families themselves: `abstol=` replaced by `tol=`
(same values). Calls to burr/invburr `abstol` are other families and untouched.

## Files changed

- C++: `core/newton.hpp` (comment only), `core/transformed_location.hpp`
  (comment only), `distributions/{gamma/mle.hpp, gamma/transformed.hpp
  (comment only), logis/mle.hpp, cauchy/mle.hpp}`, README.md dossiers of the seven families,
  `gamma/test.cpp`, `lgamma/test.cpp`.
- Bindings: `bindings/common/distributions/<7>/fit.hpp`,
  `bindings/r/distributions/<7>/adapter.cpp`, the R tests of the seven families,
  `bindings/js/family-types.d.ts` (generated), `bindings/js/{test,stop-test}.mjs`,
  `bindings/r/distributions/{invtrgamma,trgamma}/test.R`,
  `bindings/r/tests/testthat/test-stops-cont.R`.
- Validation: `dev/validation/cert_replay_roots.py`,
  `dev/validation/strict-known.json` (10 entries removed).
- New: this report and `2026-11-15-ctrl-b1-data/` (`snapshot-diff.py`,
  `diff-native.txt`, `diff-double.txt`).
- Not edited: `diagnostics-audit.cpp` (job 4), the modifier routes (job 3),
  `bindings/js/benchmark.cpp` (passes a literal to `gamma_mle`; it now means a
  relative step 1.22e-4).

## Shared helpers

- `newton_raphson_1d_abs` is not changed. `git grep` of callers: only
  `gamma/mle.hpp`, which now calls `newton_raphson_1d` (relative, unchanged).
  The abs helper has no caller left in the library (it remains in
  `dev/validation/step7_measure.py`).
- Callers of `gamma_mle` outside the seven families: lgamma (own `tol`, wrapper
  check), betapr `joint_certificate.hpp:333` (1e-12), genbeta `mle.hpp:167`
  (1e-10), `gamma/score.hpp:43-64` (1e-11, score-test fits). All pass explicit
  tolerances, so none changed default; each now gets a relative instead of an
  absolute Newton step at the same number, which is tighter for shape above 1 and
  looser below 1. I did not snapshot these families (outside scope); their C++
  and R tests pass in `just cpp-test` and `just test-changed`.
- `transformed_location.hpp` and `gamma/transformed.hpp` are shared with invburr,
  invweibull, st and others (location_fit callers); no code change there, comments only.

## Callers of `gamma_mle` outside the scope (checked by the coordinating session)

betapr and genbeta call `gamma_mle` with explicit tolerances and now get its
relative stop. Fit snapshots of both, in both modes (432 cases per mode),
against the 2026-11-15 baselines: no estimate changes; two betapr archive
cases change payload only. Case 000170 is a budget stop whose candidate list
changes. Case 000167 is an uncertified unresolved fit ("global certificate
cannot separate the finite profile from the gamma and inverse-gamma limits")
whose limit coefficients move in the last digits; in double-only mode its
best candidate changes from the gamma to the inverse-gamma face. The two
faces differ by about $10^{-17}$ there (compare the betapr verdict in the
[near-constant report](2026-11-15-near-constant-report.md)), so the label
follows rounding; the outcome stays uncertified.
