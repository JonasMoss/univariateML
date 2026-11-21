# Exponentiated Weibull free fits at limit faces

TASK-217, branch `codex/ew-limits`, base `67ff1d73`. Both precision modes matched the 19 predictions of the [limit analysis](2026-11-15-expweibull-limits-report.md). Thirteen fits selected power, four selected inverse Weibull, and `ties` and `offset` returned finite estimates. All previously accepted fits in the frozen TASK-180 archive remained estimates in their original precision mode. The [replay evidence](2026-11-21-ew-limits-data/) contains the case-level results, decision checks, and reproduction scripts.

## Per family

| Family | Fit and outcome changes | Starts and callers |
|---|---|---|
| expweibull | Compare the two free faces before the finite search. Return a flagged limit with its model coefficients and represented likelihood when the face wins the numerical comparison. Restore the rounded scale on steep finite fits and evaluate its public likelihood. Diagnose when a held shape forces exponent overflow. | Retain all quantile starts. Add the inverse-Weibull path start at $T=\log c=16$ when $A>0$. C++ `expweibull_mle`, common `fit_expweibull`, and the R and JS signatures are unchanged. The R adapter, JS generator, umbrella header and diagnostics sweep continue to use those signatures. |

No other distribution includes the expweibull headers directly. The umbrella header and the expweibull tests include them. The targeted runner therefore selected expweibull alone.

### Face comparison and restoration

- The free power face has endpoint $x_{\max}$ and shape $n/\sum_i\log(x_{\max}/x_i)$. Its endpoint approaches from above, so observations tied there retain the closed-endpoint power density. A held scale at that same endpoint still incurs its separate $\log(e-1)$ penalty per tied observation.
- The inverse fit delegates to `invweibull_mle` for the shape and reconstructs the profiled location from relative logs. If the delegate cannot restore a public scale, the fitter uses the same delegate on exponentiated normalized log data. It compares the continuous face values before restoring a rounded scale. For example, the continuous inverse-Weibull likelihood for `near-constant` is $104.4968473$, while the payload's named binary64 coefficients give $104.3003589$.
- The comparison uses mean likelihood in normalized log coordinates. With $F=\max(P,I)$ in those coordinates, the pad is $256\epsilon_{64}(1+n)(1+|F|)$. The factor allows for accumulated rounding in the face and finite likelihood evaluations without amplifying the pad by a common change of units. A polished point within the pad of $F$ retains its estimate. A finite witness above $F$ by the pad also prevents a limit decision if polishing fails. Otherwise the higher face is selected when both face evaluations are available. Within one pad of each other, both faces appear in the diagnostic and the larger computed value remains the primary model.
- The free limit decisions carry `certified = FALSE` because the finite search supplies no global interior bound. The existing held-scale decisions retain their proofs. Relative logs preserve small differences and common scales. The likelihood kernel uses `log1p(-exp(-t))` in the upper range, where forming $1-e^{-t}$ first erases the tail.
- The rounded offset scale is $1659950389748864$. At 70 digits the continuous scale is $1659950389748864.040161$, and the optimum log likelihood $-8.9446793681$ exceeds both faces. The public likelihood $-8.9489149493$ includes the cost of rounding the steep scale. The largest relative coefficient errors were $1.43\times10^{-15}$ native and $2.58\times10^{-13}$ double-only. [Native reference](2026-11-21-ew-limits-data/check-native.txt), [double-only reference](2026-11-21-ew-limits-data/check-double.txt).

### Held shape and exponent range

For Weibull shape $b$ and hazard coordinate $t=(x/a)^b$, the log-data density divided by $b$ is $ct e^{-t}(1-e^{-t})^{c-1}$. At $t\le712$, optimizing over real $c>0$ gives an upper bound $(1+t)/e$. At $t>712$, $1\le c\le\mathrm{DBL\_MAX}$ gives an upper bound $\mathrm{DBL\_MAX}\,t e^{-t}<713/e$, and $c<1$ gives an upper bound of 1. Thus every representable exponent lies below the mean log-data likelihood bound $\log b+\log(713)-1$.

The fitter sets $t_{\min}=1424$ and computes a witness in log exponent at $c\,\overline{e^{-t}}=1$. The log density uses the upper bound $-\log(1-e^{-t})\le e^{-t}/(1-e^{-t})$. When the witness beats the bound by $10^{-9}$ per observation, the fit reports exponent overflow. For the archived held-shape `near_constant` sample the witness gap is $0.6917402$ per observation. The separate 70-digit stationary profile places $\log\hat c$ at approximately $471775.685$. The same bound resolves all nine held-shape tie archive failures. [Witnesses and stationary profiles](2026-11-21-ew-limits-data/check-native.txt).

## Snapshot moves

| Family | Mode | Compared | Changed records | Largest relative coefficient move, estimate to estimate | Kind changes |
|---|---|---:|---:|---:|---:|
| expweibull | Native | 28 | 20 | $3.80\times10^{-16}$, golden exponent | 18 |
| expweibull | Double-only | 28 | 20 | $2.66\times10^{-15}$, golden exponent | 18 |

The same cases changed kind in both modes. The two other changed records are the surviving golden and `ties` estimates. Their certification flags stayed false. The stored golden file stayed byte-identical. [Native diff](2026-11-21-ew-limits-data/diff-native.txt), [double-only diff](2026-11-21-ew-limits-data/diff-double.txt).

| Cases | Move | Evidence |
|---|---|---|
| `base6`, `tiny`, `huge` | Numerical stop to power | Three scalings of the same case, with $P-I\approx1.519$. The profiled grid stays below power. |
| `two`, `half-ties` | Numerical stop to power | Power beats inverse Weibull by about $0.823$ and $2.331$, respectively, and no finite candidate closes the gap. |
| `subnormal`, `wide` | Numerical stop to power | The normalized comparison gives $P-I\approx1.144$ and $0.699$. The independent profile stays below power in each magnitude regime. |
| `n3-s0`, `n3-s1`, `n3-s2`, `n7-s0`, `n7-s1`, `n7-s2` | Numerical stop to power | Every $P-I$ is positive. The finite bump visible for `n7-s0` lies below power. |
| `tie3`, `near-constant` | Numerical stop to inverse Weibull | The same two-value likelihood after a power-scale map. $I-P\approx0.229$ in each, and the high-precision profile approaches inverse Weibull from below. |
| `n3-s3`, `n7-s3` | Numerical stop to inverse Weibull | $I-P\approx0.148$ and $0.602$. Both high-precision profiles reach their largest values near the inverse-Weibull face. |
| `offset` | Numerical stop to finite estimate | The independent finite root exceeds both faces. Restoring the rounded public scale and its likelihood resolves the old evaluation stop. |

The high-precision searches in the preceding analysis supply the numerical profile evidence. The replay checks all 19 expected decisions, including the unchanged finite outcome of `ties`, whose interior likelihood exceeds power by about $0.065$. [Prior profiles](2026-11-15-expweibull-limits-report.md#5-verdict-per-sample-n), [replay checks](2026-11-21-ew-limits-data/check-native.txt).

## Frozen ew-fit archive

All 41 samples and eight hold subsets were replayed in each mode. The 12 held-study samples are an exact prefix of the archive input. The 21 free numerical stops that moved to power are `two_point`, `below_endpoint`, `endpoint_ties`, `wide`, the two random `c0.05` cases with shapes $0.3$ and $1.5$, the random `c0.2` case with shape $8$, all nine endpoint-tie samples, and the five rescalings. A 70-digit comparison confirms $P>I$ in all 21. The finite search did not find a contender within the comparison margin. The ten held-shape overflow outcomes were previously generic iteration failures.

| Mode | Fits | Estimates retained | Limits, all hold subsets | Unbounded | Held-shape overflow | Other numerical stops | Largest relative move among retained estimates |
|---|---:|---:|---:|---:|---:|---:|---:|
| Native | 328 | 265 | 39 | 12 | 10 | 2 | $1.93\times10^{-12}$ |
| Double-only | 328 | 263 | 39 | 12 | 10 | 4 | $7.26\times10^{-13}$ |

The 39 limits include 15 existing strict held-scale limits and three existing held-scale tied envelopes. The native snapshot preceded the final refinement of the held-shape overflow witness. All its inputs are free fits. The held-mask archive and targeted suites cover the final witness in both modes. [Archive diff](2026-11-21-ew-limits-data/archive-diff.txt), [prior fit report](2026-11-12-ew-fit-report.md).

## Strict entries and tests

`fit-consistency.py` reported four listed entries passing and zero new findings. The `near-constant` and `two` entries pass in both modes. `fit-density.py` checked 20 payloads per mode with no findings. The maximum scaled likelihood discrepancy was $4.4\times10^{-16}$ native and $3.1\times10^{-16}$ double-only. The strict inventory was left for the batch merge. [Consistency output](2026-11-21-ew-limits-data/strict-consistency.txt), [density output](2026-11-21-ew-limits-data/strict-density.txt).

Appended tests cover the following cases.

- C++ power payload with repeated upper endpoints and inverse-Weibull payloads for ordinary and adjacent observations.
- C++ held-shape exponent overflow.
- C++ steep offset fit with the rounded scale and recomputed public likelihood.
- R flagged named limit conditions, including advice and equality with the named model's density sum.
- R held-shape exponent range diagnostic.

| Check | Machine | Result |
|---|---|---|
| `just test-changed --no-long-double` | Workstation, GCC 15.2 | Native and double-only expweibull shards both passed. R dev filter had 335 passes, 9 skips, no warnings or failures. JS build passed and all 230 JS tests passed. |
| `just quality expweibull` | Workstation | Passed |
| `just site-check` | Workstation | Passed, including 85 family pages and 286 inference scopes |
| `python3 dev/tools/check_links.py` | Workstation | Passed, with 47 inherited broken destinations and zero new ones |
| Fit snapshots, archived held-mask replay and direct strict checks | Workstation, GCC 15.2 | Both precision modes passed the case comparisons above |

There was no shared-code change requiring a full suite. The R build automatically regenerated the expweibull limit counts in the tracked capabilities CSV. That generated diff was removed from this family-only worktree for regeneration at merge. [Check summaries](2026-11-21-ew-limits-data/test-results.txt).

## Open

- Certifying free limit selections requires a global upper bound over the finite interior, or an exhaustive stationary-point comparison with bounds on both approach regions. The search and the archived high-precision profiles leave that open. Uniqueness for the general two-free held-shape and held-scale cases also remains open.
- The two pre-existing double-only free conditioning stops in the archive remain. `near_constant` and `random_b0.300000_c50.000000` need a more accurate score evaluation and refinement along the shallow inverse-Weibull direction to verify all coefficients at the default tolerance. The native fits and the earlier 100-digit roots give reference points. [Prior root comparison](2026-11-12-ew-fit-data/double-failed-accuracy.json).
- `rescaled_6.000000` and `rescaled_100.000000` with scale and shape held still report no convergence. Here $t_{\min}$ is about $8.94\times10^7$ and $8.94\times10^{148}$, respectively. The closed exponent profile gives $\log\hat c\ge t_{\min}-\log(1+4\epsilon_{64})$. This held-scale-and-shape route needs a log-domain overflow diagnostic of its own.
- On subnormal observations the original inverse-Weibull delegate can fail to restore its fitted scale. In this job the normalized delegate supplies the comparison and power wins the stored subnormal case. Any change to the standalone inverse-Weibull reporting for an unrepresentable scale belongs in that family.

## Reproduction

Set `TMPDIR`, `JUST_TEMPDIR`, and `XDG_RUNTIME_DIR` to `$PWD/build/tmp` and create that directory. Run the two snapshot commands through `/tmp/umvl-cpu/run4` with `--family expweibull --jobs 4`, using `build/snapshots/native` and `build/snapshots/double` as outputs and adding `--no-long-double` for the latter. Use [diff_baseline.py](2026-11-21-ew-limits-data/diff_baseline.py) with the [stored baselines](2026-11-15-fit-snapshot-data/) and the respective snapshot directories. Run [replay.sh](2026-11-21-ew-limits-data/replay.sh) through `/tmp/umvl-cpu/run` for the archive. Run [check-replay.py](2026-11-21-ew-limits-data/check-replay.py) through the same wrapper with the mode and snapshot directory to reproduce the face, offset and exponent-range numerical checks.
