# Step 5c job 2c — relative stopping rules for the Burr cluster

TASK-150.1.4 on `codex/ctrl-c`, based on `d5ca6052`. The eight families expose
`tol = 1e-10` and their existing `iterlim` defaults. The implementation,
snapshots, and fixtures follow the accuracy contract and the step 5c inventory.
[Accuracy contract](../../design/accuracy-contract.md) · [Controls inventory](2026-11-14-controls-inventory.md)

## Per family

Paths in the table are relative to `univariateML-cpp/include/cppunivariate/distributions`.
The contractions and signed brackets use the repository's practical
floating-point margins. Where a wider inclusion establishes globality,
observed-information refinement checks the coefficients separately.

| Family | Prior rule | Rule in force and justification | `iterlim` |
|---|---|---|---:|
| burr | BFGS proposal with separate gradient and objective tolerances. Held-scale Newton could override the bisected root, and the certificate allowed coefficient widths of $10^{-6}$. | The held-scale fit returns the bisected root and its profiled shape. Strict profile concavity and enclosed endpoint signs identify a unique root, and both mapped coefficient widths must meet `tol` (`burr/scale_certificate.hpp:115`). The all-free fit checks its mapped contraction box or applies an observed-information correction at the globally enclosed maximum (`burr/mle.hpp:260`). An inconclusive cover can still return a locally polished search estimate with `certified=false`. | 1000 |
| invburr | Reciprocal Burr with separate controls and a second held-scale refinement. | Burr's all-free enclosure and correction carry through the reciprocal map. With scale held, the shared signed profile bracket uses negated log ratios at the original held scale (`invburr/mle.hpp:270`). This keeps endpoint ties exact and avoids reciprocal rounding before the accuracy check. A rounded-reciprocal fallback is polished on the original sample before it can return an uncertified estimate. | 1000 |
| betapr | BFGS proposal controls, wider joint and conditional enclosures, and a fixed-scale beta delegate. | The fixed-scale route sends `tol` to its beta shape solve. The joint or one-held-shape cover can identify the winning stationary point with a wider enclosure. The compensated observed-information correction checks coefficient accuracy at that point (`betapr/fallback.hpp:103,154`). For the joint fit, the corrected coefficients must stay in the winning box. Gamma and inverse-gamma delegates check their reported face coefficients. Requests below the joint inclusion's arithmetic padding use the locally checked uncertified fallback. | 2000 |
| paralogis | Gradient-norm proposal stop and fixed-width inclusion. | Safeguarded Newton nominates the held-scale root using the shape correction. A tolerance-sized contraction checks shape and mapped scale or, after a wider global enclosure, an observed-information correction checks both (`paralogis/certificate.hpp:155`, `paralogis/mle.hpp:310`). The offset route checks the signed profile bracket and uses `iterlim` for its root loops (`paralogis/offset.hpp:157`). | 100 |
| invparalogis | Reciprocal paralogistic fit with a gradient tolerance and optional start. | The reciprocal fit receives the same coefficient tolerance, with the original-data polish available to an uncertified estimate. The near-constant fit checks the original log-offset profile in the inverse direction (`invparalogis/mle.hpp:62`). | 100 |
| gb2 | The joint Newton correction usually governed acceptance. A gradient-only branch could also yield `Ok`. | The joint analytic observed information checks and applies the step in local coordinates. The scale component is mapped to relative error at the public scale. A small gradient can nominate a point for polish, and acceptance requires that polish or the existing held-power, held-scale beta accuracy check (`gb2/mle.hpp:677,717,725`). The largest replayed coefficient move was $2.22\times10^{-11}$. | 300 |
| genbeta | Existing coefficient inclusions, scalar roots, and Newton correction under `gradtol`. | The name is `tol`. The existing applied Newton correction in product coordinates and the scalar inclusions continue to govern accuracy (`genbeta/mle.hpp:535`). | 300 |
| pareto2 | The dossier, adapter, and binding used the `reltol` name for the Lomax delegate. | The name is `tol` throughout. The Lomax computation receives the same value as before (`pareto2/mle.hpp:14`). | 1000 |

## Signatures and callers

- Burr, inverse Burr, and beta prime dropped `shape10`, `shape20`, and
  `scale0`. The Burr and inverse Burr all-free signatures now take
  `(x, tol, maxit)`. The beta prime joint entry point has the same control
  signature. Internal Burr certificate test hooks still accept work and
  reflection arguments. The legacy Burr search keeps its internal BFGS
  proposal controls and automatic seeds.
- Paralogistic and inverse paralogistic dropped `shape0` from the all-free
  and fixed-scale entry points and their bindings. The fixed-scale proposal
  selects its seed from the bracket. The reciprocal dispatchers forward
  `tol` and `maxit` directly.
- The current shared callers include the inverse fits, the GB2 seeds and
  held-subset routes, beta prime's internal fallback and conditional
  delegates, and the paralogistic held-shape Burr delegate. Pareto IV's Burr
  and GB2's proposal calls already used the retained three-argument form.
  Pareto II, Pareto III, F Pareto, and the transitive header dependants were
  included in the replay.
- Updated the eight dossiers and current common and R adapters, regenerated
  `bindings/js/family-types.d.ts`, and updated the positional C++ calls in
  `dev/validation/diagnostics-audit.cpp`. The audit's own copied route
  signatures are reserved for job 4.

## Snapshot comparison

GCC 15.2.0 on the workstation, 3,361 matched cases per mode from 16 families.
“Changed” counts any serialized result difference, including iterations and
message text. Relative moves are over estimate-to-estimate cases. The
snapshots share the same source fingerprint.
[Comparison script](2026-11-20-ctrl-c-data/check.py) · [Native results](2026-11-20-ctrl-c-data/diff-native.txt) · [Double-only results](2026-11-20-ctrl-c-data/diff-double.txt)

| Family | Cases per mode | Changed native / double-only | Max relative move native / double-only |
|---|---:|---:|---:|
| betapr | 212 | 49 / 50 | 4.54e-10 / 4.55e-10 |
| bhatt | 173 | 0 / 0 | 0 / 0 |
| burr | 220 | 26 / 26 | 4.41e-9 / 4.41e-9 |
| fpareto | 572 | 15 / 13 | 2.31e-11 / 2.31e-11 |
| gb2 | 572 | 14 / 14 | 2.22e-11 / 2.22e-11 |
| genbeta | 220 | 0 / 0 | 0 / 0 |
| gengamma | 28 | 0 / 0 | 0 / 0 |
| invburr | 220 | 34 / 34 | 1.75e-13 / 1.61e-13 |
| invparalogis | 116 | 42 / 42 | 3.41e-14 / 3.53e-14 |
| invpareto | 116 | 0 / 0 | 0 / 0 |
| invtrgamma | 180 | 0 / 0 | 0 / 0 |
| paralogis | 104 | 42 / 41 | 3.97e-13 / 4.04e-13 |
| pareto2 | 116 | 0 / 0 | 0 / 0 |
| pareto3 | 104 | 0 / 0 | 0 / 0 |
| pareto4 | 204 | 14 / 14 | 2.15e-14 / 2.15e-14 |
| trgamma | 204 | 0 / 0 | 0 / 0 |

- Burr's largest movement is held-scale archive `009884`. The new shape2 is
  within $2.06\times10^{-14}$ of the 80-digit root in both modes.
- Beta prime's largest move is the all-free `class/ties` scale. The 80-digit
  joint score gives old relative errors of $2.02\times10^{-10}$,
  $2.39\times10^{-10}$, and $4.54\text{--}4.55\times10^{-10}$ in the
  two shapes and scale. The new errors are all below $5.4\times10^{-17}$.
- Every baseline estimate remained an estimate. The two kind changes were
  native F Pareto archive `001625` and `001628`. They share five observations
  with relative spread $5\times10^{-12}$, `min=0`, and
  `shape2=1.9802932530684678`. The GB2 candidate has both free shapes
  around $1.57\times10^{16}$. The newly required polish cannot establish
  its coefficient accuracy, so the existing `unresolved` stops become
  `numerical/evaluation` with a candidate. The double-only kinds and the
  certification flags in both modes match the baseline.

## Strict entries

All eight named entries pass direct replay. The checker reconstructs the
exact binary64 observations and held scale in mpmath and solves the strict
concave scalar profile at 80 digits. Both modes returned certified fits
with the same relative errors.

| Held-scale Burr archive case | Shape1 error | Shape2 error |
|---|---:|---:|
| 000688 | 3.01e-16 | 4.60e-16 |
| 000689 | 5.08e-15 | 1.26e-14 |
| 009424 | 1.19e-15 | 1.04e-14 |
| 009884 | 2.43e-15 | 2.05e-14 |

The merge-time strict layer owns the ratchet file and pruning.
[Refine causes](2026-11-15-refine-causes-report.md)

## Tests and fixtures

- Renamed the old named arguments in the R tests for all eight families,
  the Pareto IV R tests that call Burr, and the JS tests that supply a
  cluster control. Independent R `optim` controls retain their optimizer names.
- The Burr, inverse Burr, beta prime, and paralogistic R start tests now check
  agreement across tolerances and rejection of removed arguments where those
  tests exercised validation. The paralogistic C++ start agreement test
  compares two tolerances. The inverse Burr C++ validation loop now exercises
  invalid held scales, and positional C++ certificate tests use the reduced
  signatures.
- Appended a Burr default-tolerance regression for held-scale archive
  `000688`. The old fixed-scale Burr fixture pinned a historical Newton
  estimate whose shape2 error was about $1.15\times10^{-9}$. Replaced just
  that fixture's estimates and likelihood with the rounded independent
  80-digit reference. Its C++ and R tests read the same reference at their
  existing tolerances. The frozen data and all canonical `golden.json`
  files were retained. The inverse Burr fixed-scale reference agreed with
  its stored test at the existing tolerance.
  [Fixed-scale reference](2026-11-20-ctrl-c-data/fixed-scale-reference.txt) · [Canonical 50-digit checks](2026-11-20-ctrl-c-data/default-golden-reference.txt)

| Run | Machine | Result |
|---|---|---|
| `just cpp-test` and targeted native rerun | Workstation | Initial 108/110. Both failures were corrected: held-scale Burr's fixture and beta prime's explicit $10^{-14}$ uncertified fallback. Targeted rerun 14/14, including both cases and header dependants. |
| `just cpp-test-no-long-double` | Workstation | 154/154 |
| `just test` | Workstation | 0 failures, 29 warnings, 157 skips, 36,337 passes |
| `just test-full` | Workstation | 0 failures, 29 warnings, 1 skip, 65,737 passes |
| `just js-build` and `just js-test` | Workstation | Build passed, 586/586 JS tests |
| `just site-check` | Workstation | Passed |
| `python3 dev/tools/check_links.py` | Workstation | Passed, 45 inherited broken links and zero new ones |
| `just quality burr invburr betapr paralogis invparalogis gb2 genbeta pareto2` | Workstation | All eight passed |

Simbox returned exit 75 for the requested C++ and R suite attempts. The
workstation runs used the shared slot lock. Subsequent SSH probes also timed out.

## Open

- The centered-log follow-up owns held-shape Burr archive `000638` and
  `000639`. They were outside the held-scale fix.
- F Pareto's near-constant cases `001625` and `001628` need a better
  conditioned GB2 parameterization or a limit proof to resolve the retained
  candidates. TASK-214.44 covers the separate F Pareto delegate stops.
- The simbox compiler comparison remains unavailable until SSH access returns.
