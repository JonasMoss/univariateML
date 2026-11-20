# Step 5c job 2d · Location-scale and remaining fitter controls

TASK-150.1.5. Branch `codex/ctrl-d`, based on `d5ca6052`. The iterative public fitters in this job take `tol = 1e-10` and their existing family-specific `iterlim`. Power takes its two possible holds and has a closed-form fit. Huber’s `epsilon` remains its required design coefficient. [Controls inventory](2026-11-14-controls-inventory.md) · [Accuracy contract](../../design/accuracy-contract.md)

## Per family

The paths in this table are relative to `univariateML-cpp/include/cppunivariate/distributions/`. Global likelihood comparisons and local coefficient accuracy have different evidence. The signed enclosures are conditional on computed signs and the practical arithmetic margins in the dossiers. Newton checks use observed information and contraction of the applied corrections.

| Family | Previous control and rule | Rule with `tol` |
|---|---|---|
| ged | `gradtol = 1.220703e-4`. BFGS proposals and a separate global shape enclosure | A successful finite shape representative also passes analytic Newton refinement. With shape above one and scale held, the location solve brackets the strictly decreasing signed residual-power score, with residuals normalized by sample radius and width checked in public location units. The median and knot routes retain their exact locations. `ged/mle.hpp:267`, `:521` and `ged/accuracy.hpp:15` |
| lst | `gradtol = 1.220703e-4`. Global shape and nuisance enclosure or fixed-shape EM nomination | The global enclosure identifies the basin at width `1e-6`. A positive-definite information correction checks location, scale and df. The fixed-shape route applies the correction after EM and root inclusion, retaining an accurate local fit with the uncertified flag when inclusion fails. `lst/mle.hpp:210`, `:544` |
| std | `gradtol = 1.220703e-4`. Global enclosure including the transformed sd box | The same basin and correction sequence as LST, with the error evaluated in the standardized-t parameters. Fixed df delegates to the LST nuisance solve and maps ordinary t scale to sd. `std/mle.hpp:198`, `:355` and `lst/accuracy.hpp:9` |
| sged | `gradtol = 1e-10`. Interior observed-information polish and held-moment enclosures | Existing convergence check retained and the held-moment parameter renamed. Interior log-shape increments are conservative for relative error in shape. The held shape-two delegation passes the requested tolerance through the skew-normal certificate, capped at `1e-10`. `sged/mle.hpp:446`, `sged/held_moment_certificate.hpp:105` and `sged/skew_certificate.hpp:4` |
| sstd | `gradtol = 1e-10`. Interior observed-information polish | The existing final check maps the correction for log excess df to the public relative df error, and measures location in public location/scale units. The held symmetric route uses STD. `sstd/mle.hpp:679`, `:704` |
| huber | `gradtol = 1e-10`. Standardized location and scale score checks and a free-shape score gate | The finite representative additionally passes analytic observed-information refinement in scale-standardized location, log sigma and log epsilon. The exact midpoint of a flat location interval remains eligible if the interval survives the corrected scale and shape. The certificate for global likelihood value still uses its own margins. `huber/mle.hpp:931`, `:971` |
| bnbinom | `gradtol = 1e-10`. Log-coordinate observed-information correction and tolerance-dependent enclosures | The existing relative coefficient rule is now exposed as `tol`. Positive free coefficients are checked in log coordinates. `bnbinom/mle.hpp:607`, `:893` |
| slash | `reltol = 1e-10`. Observed-information polish or held-location scale bracket | The dossier and binding now agree on `tol`. The existing final correction measures the mean on the public location/scale floor and the scale in log coordinates. `slash/mle.hpp:61`, `:425` |
| dweibull | `reltol = 1e-12`. Brent bracket in log shape2 | Default `1e-10`. For each fixed raw scale coordinate, the positive-count contributions to its score decrease with shape2. Consequently the profiled shape1 increases with shape2. Both ends of the signed log-shape2 bracket therefore also bound log shape1. `dweibull/mle.hpp:274`, `:474` |
| gpd | `reltol = 1e-12`. Scale bisection and scalar profile polishing | Default `1e-10`. Retain the signed root enclosure and require a contracting curvature correction mapped to both free coordinates. The scale derivative follows from the integral identity `D'(b) = -P`. The shape floor is one at and around zero. `gpd/mle.hpp:47`, `gpd/certificate.hpp:83` |
| triang | `reltol = 1e-12`. Newton-step length in reciprocal endpoint distances | Default `1e-10`. Check negative-definite objective curvature and require a contracting correction after mapping both endpoint steps to support-width units. The free mode is chosen by finite observation enumeration. `triang/mle.hpp:87`, `:106`, `:253` |
| snorm | `iterlim` exposed. Root isolation used an internal absolute width | `tol` is exposed in C++, R and JS. The selected finite joint root bracket propagates to three public coefficient widths using the monotone side-residual sums. The conditional algebraic route checks coefficient widths on the selected finite result after carrying its likelihood bounds through the comparison. `snorm/mle.hpp:198`, `snorm/certificate.hpp:455` |
| power | Ignored compatibility `epsilon` | Closed-form endpoint and shape. The free endpoint is exactly the sample maximum. `power/mle.hpp:41` |
| binom | `abstol = 1.220703e-4`. Exact integer size bracketing, including held-probability search | With probability held at `p` in (0, 1), every summand of the adjacent log-likelihood ratio is nonpositive once `k + 1 >= max(x) / p`. The search pads this tail ceiling by `tol`, checks the computed sign and still searches to adjacent integers. With size held, the success probability has its closed form. `binom/mle.hpp:139` |

A precision regression check sends the same five ordinary samples to their default controls and to `tol = 1e-24`. Each default call fits. Each tight request returns a tolerance stop with a finite candidate and a method floor of four binary64 epsilons in both precision modes. This floor describes the supported method resolution. [Probe source](2026-11-20-ctrl-d-data/tight-controls.cpp) · [Probe output](2026-11-20-ctrl-d-data/tight-controls.txt)

## Interfaces and calls

- The dossier controls, common bindings, R adapters and generated JS types use the new names and defaults. No user start arguments existed in this family set. C++ location-scale entry points now provide the corresponding default where their free-fit signature previously required an explicit tolerance.
- `snorm_mle` passes `tol` to its joint and conditional helpers. The positional callers of the common `fit_snorm` signature are the R adapter, the generated JS bridge and the SGED accuracy replay. The SGED shape-two delegate also forwards its tolerance to C++.
- `power_mle(x)` and `power_mle(x, alpha, beta)` replace the variants that carried the ignored control. The two GB2 call lines, the genbeta and trgamma C++ test calls, and the invburr and trgamma R test calls were updated. The diagnostics audit’s Power call drops the argument and its skew-normal C++ call uses `1e-10`. Its copied route signatures belong to the route migration.
- `just test-changed --dry-run` selected all 85 families because the JS test and diagnostics-audit changes are shared effects. The snapshot census includes the direct header dependants and further delegates exercised by the changed calls.

## Snapshot comparisons

Each mode paired 5,323 cases against the stored baseline, with zero unpaired cases and zero final outcome transitions. The figures include any serialized change. Relative location moves use the baseline scale as a floor. Other reported relative moves use the absolute baseline coefficient. [Native comparison](2026-11-20-ctrl-d-data/summary-native.txt) · [Double-only comparison](2026-11-20-ctrl-d-data/summary-double.txt) · [Comparison code](2026-11-20-ctrl-d-data/compare.py)

| Family | Cases per mode | Changed native / double-only | Maximum relative move in either mode |
|---|---:|---:|---|
| bbinom | 148 | 0 / 0 | 0 |
| betapr | 212 | 0 / 0 | 0 |
| binom | 87 | 0 / 0 | 0 |
| bnbinom | 135 | 2 / 2 | 0 |
| cmb | 135 | 0 / 0 | 0 |
| dweibull | 79 | 9 / 9 | `1.54231e-11`, archive 001212 shape2 |
| fpareto | 572 | 0 / 0 | 0 |
| gb2 | 572 | 0 / 0 | 0 |
| ged | 173 | 34 / 33 | `6.34510e-11`, archive 002407 nu |
| genbeta | 220 | 0 / 0 | 0 |
| gengamma | 28 | 0 / 0 | 0 |
| gpd | 108 | 26 / 26 | `3.37394e-11`, archive 002912 sigma |
| ht | 228 | 0 / 0 | 0 |
| huber | 173 | 64 / 57 | `7.70394e-11`, archive 003393 sigma |
| invburr | 220 | 0 / 0 | 0 |
| invtrgamma | 180 | 0 / 0 | 0 |
| lst | 173 | 28 / 28 | `2.92443e-13`, resample n7-s3 df |
| pois | 51 | 0 / 0 | 0 |
| power | 112 | 0 / 0 | 0 |
| sged | 285 | 0 / 0 | 0 |
| slash | 93 | 9 / 9 | 0 |
| snorm | 157 | 0 / 0 | 0 |
| sstd | 285 | 0 / 0 | 0 |
| st | 285 | 0 / 0 | 0 |
| std | 173 | 18 / 18 | `2.61028e-13`, golden nu |
| trgamma | 204 | 0 / 0 | 0 |
| triang | 196 | 72 / 72 | `1.37814e-10`, class base6 a |
| yulesimon | 39 | 0 / 0 | 0 |
| **Total** | **5,323** | **262 / 254** | |

All BNBINOM and Slash changes are message text. In the triangular stopping rule’s support-width units the largest endpoint move is `1.38243e-11` in archive 008236. The initial double-only run turned GED 002463 (data around `1e-150)) and 002466 (a large outlier) from estimates into numerical stops. Both fix shape and scale. The radius-normalized bracket restored the finite estimates without scaling the derivatives by the held sd. Their location errors against the independent 50-digit roots are `2.10e-165` and `4.38e-16` in public location units, respectively, in both modes. [Initial transitions](2026-11-20-ctrl-d-data/ged-initial-double-transitions.txt) · [Native root comparison](2026-11-20-ctrl-d-data/ged-location-reference-native.txt) · [Double-only root comparison](2026-11-20-ctrl-d-data/ged-location-reference-double.txt)

Golden files were preserved for all families. The 50-digit reference checks for GED, LST, STD, Huber, discrete Weibull, GPD, triangular and skew-normal agreed with their frozen goldens, with maximum relative differences from `2.3e-17` to `9.7e-17`. The reference log identifies the held and kink coordinates and whether the high-precision stationary-point check carries an independent global argument. [Reference results](2026-11-20-ctrl-d-data/golden-reference.txt) · [Quality results](2026-11-20-ctrl-d-data/quality.txt)

## Test changes and runs

- R fitter tests use `tol` in BNBINOM (2 lines), discrete Weibull (1), GED (6), Huber (3), LST (3), SGED (35), SSTD (1), STD (3) and triangular (36). The independent `optim` and `nlminb` option names are preserved. Power’s removed-control test now expects an unknown argument and still checks the exact endpoint. Invburr and trgamma each update one Power call.
- C++ golden calls for GED, LST, STD, SGED, SSTD and skew-normal exercise `1e-10`. Power’s tests, the genbeta delegate test and the two trgamma tests use the current Power signature. JS updates the triangular control and the SGED delegate check. No numerical-stop expectation was loosened.
- The first R dev run exposed three shallow Student-t failures with the unnecessarily narrow global basin gate. The retained `1e-6` basin gate allows the same peaks to reach the separate coefficient correction. The full R run and the targeted development rerun pass those tests. The independent R oracle option regressions were corrected in the source tests, including the full-tier triangular `nlminb` rerun.
- Both 27-family snapshots ran on the workstation through `run4`. Late reruns refreshed the affected families in place. The provenance files record the initial runs and the selected precision, GED and Student-t replays. The small reference checks used one slot. [Evidence directory](2026-11-20-ctrl-d-data/)

| Run | Machine | Result |
|---|---|---|
| `just cpp-test` | Workstation, GCC 15.2 | 110/110 |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2 | 154/154 |
| Late targeted C++ | Workstation | 367 native cases after the precision and GED fixes, then 161 Student-t cases in each precision mode after the basin fix |
| `just test` | Workstation | First run had 3 shallow Student-t failures and 81 warnings. The late development rerun below verified the affected files |
| `just test-full` | Workstation | 0 failures, 65,737 passes, 1 skip. Its 1,391 warnings included 1,362 from the triangular oracle’s misspelled `nlminb` option, verified with the corrected targeted full-tier rerun below |
| Late targeted R dev | Workstation | 0 failures, 0 warnings, 23 skips, 1,606 passes across nine affected files |
| Late full-tier `dist-triang` | Workstation | 0 failures, 0 warnings, 0 skips, 296 passes with `rel.tol` restored |
| `just quality` | Workstation | All 14 families passed |
| `just js-build` and `just js-test` | Workstation | Build passed, 586/586 tests |
| `just site-check` | Workstation | Passed, including 85 generated C++17 snippets |
| `python3 dev/tools/check_links.py` | Workstation | 0 new broken links, 45 inherited |

Simbox returned 75 for each attempted C++ and R suite. The final suite results are from the required workstation fallback. The fixes were verified with targeted reruns for the affected families.

## Open

- TASK-214.13 remains reproducible. Skew-normal archives 006576 and 006590 select a half-normal limit in native arithmetic and an evaluation failure in double-only arithmetic. Archive 006587 retains the native finite-versus-reflected-half-normal comparison rendered as a reflected-half-normal limit, and an evaluation failure in double-only mode. The `huge` class still selects the native half-normal limit and a double-only representability failure. Resolving these requires tighter arithmetic or endpoint comparisons for the existing certificates. Giving the finite root solve a public tolerance leaves these dispositions intact.
- TASK-214.33 remains reproducible for SSTD `class/two`. Both modes still return the translated half-normal limit. The fixture is below the dossier’s declared minimum sample size and its disposition comes from the face comparison. It requires resolution of that sample-size and limit policy independently of coefficient refinement.
