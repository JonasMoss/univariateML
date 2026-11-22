# Certificate replay for Lomax transfers and scalar covers

TASK-214.34, step 8d. Baseline `b6a0087141ceb753324e1d46441d8f320271042e`, 22 November 2026, workstation GCC 15.2.

The replay added 932 passing comparisons and judged 7,638 records. All 7,152 pre-existing records matched master in order and in every comparison field after removing measured time and the added comparisons. The full run had 44 OPEN comparisons and zero failures. Twenty OPEN comparisons belong to the families in this evaluation. All 44 came from the master replay. [Record diff](2026-11-22-replay-covers-data/master-records.diff), [full replay log](2026-11-22-replay-covers-data/replay.log)

The four fresh snapshots matched all 27,710 stored fit records, including the modifier routes in each precision mode. The replay used reconstructed data with the recorded input digests and the public coefficients, in the coordinates of each certificate. Interval and exact comparisons support their inequalities for these inputs. Numerical comparisons give 50-digit evidence, including those using special functions or a numerical nuisance solve. The re-derived cells are independent of the fitter's subdivision order. [Snapshot comparison](2026-11-22-replay-covers-data/snapshot-diff.log), [replay design](../../design/certificate-replay.md)

## Family results

Counts combine native and double-only records. A record can contain several comparisons. The table includes older support and root checks carried by the same families. The mechanism ledger gives separate counts for each mechanism and the comparison ledger records the arithmetic level. CPU seconds sum the times of the recorded cases for each family. [Mechanism ledger](2026-11-22-replay-covers-data/mechanisms.tsv), [comparison ledger](2026-11-22-replay-covers-data/comparisons.tsv), [case records](2026-11-22-replay-covers-data/records.jsonl.gz)

| Family | Scope | Records | Holds | OPEN | Failures | CPU s |
|---|---|---:|---:|---:|---:|---:|
| pareto2 | E/F | 132 | 350 | 0 | 0 | 10.058 |
| invpareto | E/F | 130 | 264 | 0 | 0 | 12.722 |
| betapr | E/F | 0 | 0 | 0 | 0 | 0.000 |
| burr | E/F | 48 | 156 | 0 | 0 | 5.851 |
| gpd | F | 142 | 266 | 0 | 0 | 1315.766 |
| ht | E/F | 232 | 288 | 2 | 0 | 6301.338 |
| skellam | F | 56 | 52 | 4 | 0 | 10.131 |
| slash | F | 134 | 376 | 0 | 0 | 2188.207 |
| sgomp | F | 148 | 316 | 0 | 0 | 519.984 |
| dgamma | F | 120 | 228 | 2 | 0 | 777.018 |
| paralogis | E/F | 156 | 346 | 0 | 0 | 1085.708 |
| invparalogis | E/F | 174 | 380 | 2 | 0 | 1296.527 |
| foldnorm | F | 186 | 270 | 0 | 0 | 23.640 |
| fatigue | F | 126 | 242 | 10 | 0 | 19.268 |
| cauchy | F | 154 | 224 | 0 | 0 | 1.403 |
| trgamma | E/F | 112 | 164 | 0 | 0 | 19.308 |
| invtrgamma | E/F | 90 | 120 | 0 | 0 | 16.808 |
| huber | E/F | 116 | 172 | 0 | 0 | 54.905 |

The 42 informational records for pareto2 are counted separately in the evidence. Their inherited `mapping` text still describes the older certification flag. The new exact `transfer-flag` comparison verifies the current `certified=false` exponential limit for pareto2 and inverse Pareto. The stored records reflect the current mapping in both precision modes. [Recorded comparisons](2026-11-22-replay-covers-data/records.jsonl.gz)

## Claims and replay

| Family | Certificate comparison |
|---|---|
| pareto2 | Transfer the held-minimum likelihood to Lomax on the binary64 excesses. Use interval root, curvature, shape inclusion, global cover and exponential margin on the Lomax scale. Check model and certification flag exactly. |
| invpareto | Transfer to Lomax on binary64 reciprocals, with the constant Jacobian in the likelihood conversion. Check the Lomax intervals and the inverse-exponential model and flag. |
| betapr | A held first shape of one gives Lomax directly. A held second shape of one gives Lomax on reciprocals with reversed scale conversion. Use the Lomax root and global comparisons and its strict face classification when applicable. |
| burr | A held power gives Lomax on the powered, normalized observations. Check the root, global comparison and the strict exponential face before transport to Weibull. Unit power uses this same route. |
| gpd | Compare the standardized excess profile to its attained boundary and tails. Interval score and curvature bounds close the compact subdivision and check the scale conversion. |
| ht | Numerically enclose the inner scale scores and the log-shape slope on a cell. The added normal-face route checks the omitted tails and bounds the full profile by the face plus `2.5e-9` per observation, the quarter-margin in `ht/cert.hpp`. Its large-shape scale floor uses the upper three quarters of the positive excesses. The implementation also handles the equal-mass neighborhood of the scale-zero face. |
| skellam | Evaluate the Bessel profile numerically on the empirical-mean line. The cell curvature and the Poisson-mode and near-zero tail bounds check the `1e-8 n` value allowance. |
| slash | Interval scale roots and location curvature close held-scale and profiled-location cells. The replay also checks the half-mass comparison where exercised and uses numerical Newton steps for local accuracy. |
| sgomp | Enclose the conditional shape root at each rate. Its monotonicity bounds shape over a rate cell. Use the profile curvature bound and the two analytic rate tails for the value comparison. Exclude rate cells outside the `1e-7` relative window at the unpadded likelihood, then bound shape by the remaining rate root window. The shape tolerance is `1e-7 max(1, eta)`. |
| dgamma | The new joint replay profiles the log-rate root by concavity, uses its tangent allowance, and bounds the log-shape slope with conditional log-cell integral secants. The cutoffs use the small-shape mass bound and separated-cell Chernoff bounds. The check is numerical with a `1e-8` per-observation allowance. The held-rate cover uses the existing numerical incomplete-gamma evaluation. |
| paralogis | Enclose the monotone intercept root and the shape profile on cells in log shape. Test analytic tails against the returned value and use a signed root window. |
| invparalogis | Replay the same shape cover on reciprocal observations. Match the binary64 transform for a certified fit and use interval reciprocals when replaying an uncertified result polished in the original data. |
| foldnorm | Bound the profile in `mu/sigma^2` by interval scores and values. The derivative tail and the `1e-10` per-observation compact allowance cover the half-normal member and interior points. |
| fatigue | Use the interval beta score and the analytic bounds on its search range at held shift. Recheck a signed bracket and exclude competing beta cells. Margin-tied uncertified returns are listed as OPEN. |
| cauchy | At held scale, the sample hull contains the location maximum. Interval slopes, curvature and likelihood bounds check the root and global cover. Exact ties have a separate comparison. |
| trgamma | Numerically invert digamma in the standardized power profile. Convex-conjugate secants and their endpoint tangents bound the cells. The fitted normal seam and analytic power tail close the omitted ranges. The held-scale profile uses the conjugate of log gamma and its own tails. Finite fits use a signed concave basin and the `1e-8` per-observation face margin where a face is available. |
| invtrgamma | Apply the same power, seam and face checks on negated data logs and reciprocal scale. The Jacobian is common to the compared values. |
| huber | Refit the location and scale nuisance problem at each epsilon endpoint and calculate its supporting-plane upper bound. Add the log-normalizer chord error to bound each cell. Use the Gaussian tail above its largest residual and the Laplace or held-scale bound at small epsilon. Compute the Gaussian and Laplace face values under the same holds. |

The shifted Gompertz coefficient comparison uses an interval curvature check to narrow the proven rate window before transporting it to shape. For discretized gamma the conditional rate proposal uses analytic CDF endpoint derivatives, with the bracket and the tangent allowance controlling acceptance. The selected archive case took 30.1 CPU seconds with the safeguarded rate solve, down from 99.6 with bisection. The inverse transformed-gamma near-seam resample `n7-s3` in both modes required a narrower signed concavity interval, which closed the targeted rerun. [Code](../../validation/cert_replay_covers.py), [shifted Gompertz](../../validation/cert_replay_sgomp.py), [transformed gamma](../../validation/cert_replay_trgamma.py), [Huber](../../validation/cert_replay_huber.py), [rate-solve check](2026-11-22-replay-covers-data/dgamma-rate-solve.log), [near-seam check](2026-11-22-replay-covers-data/trgamma-tight-basin.log)

## OPEN and failures

The 20 OPEN comparisons in the requested families appear below. Each listed case occurs in both precision modes. The ledger gives every case key, claim, numerical value and reason. The full log also lists the 24 inherited OPEN comparisons in other families. [OPEN ledger](2026-11-22-replay-covers-data/open.tsv), [full log](2026-11-22-replay-covers-data/replay.log)

- **dgamma**, archive `001025`. The held-rate large-shape cell probability loses relative accuracy in the difference of lower incomplete-gamma tails at 50 digits. A stable evaluation of the complementary tail is needed.
- **fatigue**, archive `001458`, `001459`, `001461`, `001462` and `009817`. The returned fits explicitly flag lack of uniqueness and certification. The alternate beta modes differ in total likelihood by roughly `1.7e-26` to `1.5e-24`, within the fitter's comparison margin.
- **ht**, class `wide`. The inherited finite-fit replay uses a lower scale floor that gives a log-shape cutoff of about 142.22, beyond its evaluated log-shape range of 90. The new face replay uses the sharper subset scale floor for face outcomes.
- **invparalogis**, class `near-constant`. The fit is uncertified. The independent log-shape cover used its 2,000 evaluations with a largest lower bound about `2e-12` below the target. The competing cell bounds did not close.
- **skellam**, archive `006309` and class `large`. The series replay stops above order 5,000 or rate `1e5`. These cases require a high-order Bessel evaluation with a usable error bound.

There were zero failures on the snapshots. Twelve synthetic controls exercised both unit-shape beta-prime orientations and Burr unit power, perturbed the held-shape roots, and checked rejection of inferior shifted Gompertz, discretized-gamma, transformed-gamma and Huber witnesses. [Failure ledger](2026-11-22-replay-covers-data/fails.tsv), [controls](2026-11-22-replay-covers-data/controls.py), [control results](2026-11-22-replay-covers-data/controls.log)

## Open scope

- The snapshots contain no beta-prime unit-shape hold and no dedicated Burr unit-power hold. The synthetic records exercise those branches. Snapshot coverage requires those hold values in the input ledger. Burr's held-first-shape concave log-location and power solve and closed-form held subsets have no subdivision to replay.
- No free-shape half-t snapshot returned the equal-mass scale-zero face. The replay handles that neighborhood, but its integration evidence here is for normal-face and finite-fit outcomes. The inherited finite-fit replay leaves the equal-mass finite branch outside its scope.
- There are no certified free-epsilon finite Huber estimates in the snapshots. The 116 epsilon covers are face outcomes. A snapshot with a certified finite epsilon would exercise the finite-winner allowance. The original nuisance points and their standardized supporting-plane details are transient.
- Original subdivision endpoints, retained root boxes, competing root witnesses and practical floating-point pads are generally discarded on successful fits. This affects the exact runner-up comparisons in held-scale Cauchy and fatigue, the original narrow root boxes in transformed gamma and shifted Gompertz, and the exact finite-cell comparisons in the other covers. Replaying those particular machine comparisons would require a trace of the deciding bound, target and coordinates. The independent profiles above establish the stated re-derived comparisons.
- Held single-root or closed-form subsets outside the cover are recorded only where an existing replay handles them. In particular the separate held-rate Skellam and held-scale or held-location folded-normal solves do not enter these global covers.

## Timing and checks

All runs used the workstation. Wall time in the table starts after the slot reservation. CPU time includes children and is user plus system time.

| Check | Wall s | CPU s | Result |
|---|---:|---:|---|
| Master replay with four workers | 3,967.70 | 14,727.13 | 7,152 records, 44 OPEN, zero failures |
| Extended replay with four workers | 4,800.29 | 17,998.04 | 7,638 records, 44 OPEN, zero failures |
| Changed-family recipe, full tier | 2,779.89 | 10,558.90 | 179/179 native, 179/179 double-only, R 65,860 pass, 29 warnings, 1 skip, zero failures, JS 587/587 |
| Truth checks | 43.12 | 43.15 | 36 passed |

The replay cache had zero hits in the reported extended run. The changed-family selector classified the Python validation modules as shared and selected all 85 families. `UMVL_TEST_TIER=full just test-changed --no-long-double` ran the R full tier once, and `just truth-check` ran the truth prerequisite separately. The master replay finished its ratchet and timing output. The surrounding zsh command could not save its exit code because it used the reserved variable `status`. The extended replay and both test commands recorded exit zero. [Master log](2026-11-22-replay-covers-data/master-replay.log), [extended log](2026-11-22-replay-covers-data/replay.log), [test results](2026-11-22-replay-covers-data/tests.log)

The Markdown link checker exited zero with inherited entries only. [Link check](2026-11-22-replay-covers-data/links.log)

Rebuild each baseline with `/tmp/umvl-cpu/run4 python3 dev/validation/fit-snapshot.py --output build/snapshots/MODE --jobs 4`. Supply `--no-long-double` for the two double-only modes and `--modifier-routes` for the two route modes. Run the master and current drivers with the same arguments below, each with its own record file. For master, extract the replay scripts, `strict_common.py` and `fit-snapshot.py` with `git show b6a00871:<path>` into a scratch projection of `dev/validation/` and link the static inputs within the worktree.

```sh
/tmp/umvl-cpu/run4 python3 dev/validation/cert-replay.py \
  build/snapshots/native,build/snapshots/routes-native \
  build/snapshots/double,build/snapshots/routes-double \
  --jobs 4 --records build/scratch/replay-final.jsonl
```

Use the evidence summarizer with the master and extended record files to reproduce the payload diff and tables. [Summarizer](2026-11-22-replay-covers-data/summarize.py), [native provenance](2026-11-22-replay-covers-data/native.provenance.json), [double-only provenance](2026-11-22-replay-covers-data/double.provenance.json), [native route provenance](2026-11-22-replay-covers-data/routes-native.provenance.json), [double-only route provenance](2026-11-22-replay-covers-data/routes-double.provenance.json)
