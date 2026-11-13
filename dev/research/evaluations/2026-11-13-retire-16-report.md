# Retirement of 16 public families (TASK-191)

Completed in `codex/retire-16`, starting at `1e0f5185`, on the workstation only. No commits, board edits, external publication, dependency installations or other worktrees were used. The public library now has **86 families: 85 fitted and PGW evaluation-only**, down from 102/101. The sixteen retired IDs have no public wrappers, aliases, redirects, dossier folders, WASM fit registrations, public JS types, or site routes.

The retained kernels were moved without numerical changes. Snapshot evidence establishes exact equality of every recorded modifier-route result before and after retirement in both precision modes. The supplied older surviving-family double baseline has one pre-existing EMG timeout discrepancy; it is explicitly open below rather than reported as a zero-difference pass.

## Kernel and binding locations

Paths in this table are relative to `univariateML-cpp/include/cppunivariate/distributions/`. Every retained header's exact old/new path, plus test, fixture and binding moves, is in [moves.json](2026-11-13-retire-16-data/moves.json). The 63 retained headers match the starting revision after ignoring include directives and removing the obsolete Zipf optimizer and unused `fit_tbinom` forwarding entry point. [Reproduction and full header inventory](2026-11-13-retire-16-data/check-kernel-moves.py).

| Old directory | Surviving kernel location | Modifier |
|---|---|---|
| `tbinom/` | `binom/tbinom_*.hpp` | `window` |
| `tnorm/` | `norm/tnorm_*.hpp` | `window` |
| `tpois/` | `pois/tpois_*.hpp` | `window` |
| `zipf/` | `zeta/finite_harmonic.hpp`, `zeta/zipf_dpqr.hpp` | `finite window` |
| `zib/` | `binom/zib_*.hpp` | `zero="inflated"` |
| `zinb/` | `nbinom/zinb_*.hpp` | `zero="inflated"` |
| `zip/` | `pois/zip_*.hpp` | `zero="inflated"` |
| `zmbinom/` | `binom/zmbinom_*.hpp` | `zero="modified"` |
| `zmgeom/` | `geom/zmgeom_*.hpp` | `zero="modified"` |
| `zmlogarithmic/` | `lgser/zmlogarithmic_*.hpp` | `zero="modified"` |
| `zmnbinom/` | `nbinom/zmnbinom_*.hpp` | `zero="modified"` |
| `zmpois/` | `pois/zmpois_*.hpp` | `zero="modified"` |
| `ztbinom/` | `binom/ztbinom_*.hpp` | `zero="truncated"` |
| `ztgeom/` | `geom/ztgeom_*.hpp` | `zero="truncated"` |
| `ztnbinom/` | `nbinom/ztnbinom_*.hpp` | `zero="truncated"` |
| `ztpois/` | `pois/ztpois_*.hpp` | `zero="truncated"` |

The existing public `zero.hpp` and `truncation.hpp` trait seams include these private headers. Shared zero-modification machinery remains in `core/zero_modified*.hpp`. Common adapters moved to `bindings/common/distributions/<base>/<oldid>_fit.hpp`; their modifier seam entry points remain private implementation details. The R kernel adapters are in `bindings/r/modifier-adapters/`, outside the public family adapter registry. Zipf needs no private fitter adapter: finite zeta already provides the approved wider-domain fit.

The removed Zipf optimizer imposed the old nonnegative exponent domain. Its still-used harmonic implementation and evaluation helpers survive. The unused five-argument binomial-window forwarding shim was deleted; the controlled kernel entry point used by the seam remains. Norm's shared outcome wiring and the other jobs' 75 family fitters/adapters were left untouched.

## Tests and frozen evidence

All sixteen C++ family test files moved to `univariateML-cpp/tests/modifiers/<oldid>.cpp`. Frozen fixtures are under `univariateML-cpp/tests/modifiers/fixtures/<oldid>/`, inside the standalone core tree. **48 golden/truth files are byte-identical** to the starting revision. The helper resolves ordinary family dossiers first and private modifier fixture directories second. CMake retains the development and slow full shards.

All sixteen R test files moved to `bindings/r/modifier-tests/<oldid>.R`; assembly includes them as private modifier-kernel regressions. Test-only helpers preserve old coefficient coordinates when checking retained kernels. Public-route equivalence and evaluation assertions use base-family modifiers. JS facade regressions use the test-only route coordinate map. Private kernel constructors/evaluators are absent from model selection, bootstrap, plotting and inference public registries.

The exact removed assertions and reasons are preserved in [dropped-public-assertions.json](2026-11-13-retire-16-data/dropped-public-assertions.json):

- Four fragments in the normal inference-dispatch test queried retired public family IDs: normal windows, binomial/point-mass singularities, Poisson inflation/modification faces, and modified count/logarithmic singular faces. The public dispatches were removed; modifier inference is unavailable. Surviving-family inference assertions remain.
- C++ `zipf`, `zipf: endpoint, shape and joint holds`, and `zipf: structured stop payloads` tested the removed nonnegative-shape public optimizer. Harmonic, density, tail, quantile and random-generation assertions remain; the approved finite-zeta route has a new golden regression and frozen seam snapshots.
- R's `Zipf returns the attained uniform face at the count ceiling` asserted the removed exponent restriction. Finite zeta permits negative exponents and an infinite-exponent supremum.
- Three old Zipf rejection clauses (`N=4.5`, `N=NA_real_`, `s=-1`) conflict with approved closed-window rounding, estimated endpoints and the wider exponent domain. Other hold/oracle assertions were retargeted.

No tolerance was relaxed. Warning and NaN checks both remain; the helper stores the value separately from testthat's warning-condition return. Finite-zeta log-tail comparisons use a stable independent finite sum. The one-coefficient bootstrap assertion now checks its two-quantile vector because the window endpoint is an attribute. Model-selection filtering uses zeta as its discrete candidate. The late JS quantile regression now calls the public inflated-binomial route, retaining all three assertions.

## Fit snapshots

The snapshot harness gained `--modifier-routes` before any old folder was removed. [Frozen route inputs](2026-11-13-retire-16-data/route-inputs.json) retain each old family's archive rows, golden, magnitude classes, holds and resamples. Results retain their route IDs and original input provenance. [Commands](2026-11-13-retire-16-data/commands.txt) and per-mode provenance record the compiler and flags.

| Comparison | Native | Double-only |
|---|---:|---:|
| Modifier seam before vs after, all recorded fields | **0 / 1,934 differ** | **0 / 1,934 differ** |
| 85 surviving fitters vs supplied older baseline | **0 / 11,820 differ** | **1 / 11,820 differ** (EMG; open below) |
| EMG rebuilt at starting commit vs final tree | — | **0 / 212 differ** |

The survivor diff ignores only `outcome,core_status,outcome_override`, as requested. Modifier diffs ignore nothing. The required before snapshots are `modifier-routes-native.jsonl.gz` and `modifier-routes-double.jsonl.gz`; the after records have the `modifier-routes-after-` prefix. [Complete comparison output](2026-11-13-retire-16-data/final-snapshots.txt).

Comparing the approved modifier routes to old wrapper baselines through their coefficient maps, at the modifiers-2 audit tolerance, gives the same classification in each mode:

| Reason class | Cases |
|---|---:|
| Equal | 1,912 |
| Widened finite-zeta exponent domain | 18 |
| Zeta accuracy refinement | 1 |
| Held-normal-window density improvement | 1 |
| Unclassified binomial-window finding | 2 |

These are old-wrapper/replacement differences, not before/after retirement changes. [Every differing case and field](2026-11-13-retire-16-data/old-route-differences.csv), [comparison script](2026-11-13-retire-16-data/compare-old-routes.py) and [summary](2026-11-13-retire-16-data/old-route-summary.json) are retained.

## Configured relations

All **34 affected edges** were retained as relations between configured surviving laws, with coefficient maps. The minimal schema extension adds `configuration` and `source_configuration` (zero mode or closed window), with maps in public base coordinates. A private `kernel` selector identifies the symbolic density used for independent validation; it is not a callable family alias. Validation checks the base/configuration/map pairing before evaluating that symbolic law. The schema is documented in `README_SCHEMA.md`.

The size-zero positive-conditional negative-binomial face maps to logarithmic theta correctly; geometric shifting retains its shift and window; finite-zeta uniformity is `s=0` on the configured window; the normal unbounded-window edge retains infinite bounds. Configured edges are not deduplicated as unconfigured self-relations. Both source and target settings appear on site relation rows.

[Before edges](2026-11-13-retire-16-data/relations-before.json) and [after edges](2026-11-13-retire-16-data/relations-after.json) contain every expression and map. The following is an inventory; the JSON is authoritative for coefficients.

| Before | After source → target | Kind |
|---|---|---|
| `nbinom → zinb` | `nbinom → nbinom {"zero":"inflated"}` | `special_case_of` |
| `binom → zib` | `binom → binom {"zero":"inflated"}` | `special_case_of` |
| `binom → tbinom` | `binom → binom {"truncate":["max(0, floor(-1)+1)","floor(size)"]}` | `special_case_of` |
| `lgser → zmlogarithmic` | `lgser → lgser {"zero":"modified"}` | `special_case_of` |
| `lgser → ztnbinom` | `lgser → nbinom {"zero":"truncated"}` | `special_case_of` |
| `dunif → zipf` | `dunif → zeta {"truncate":[1,"max-min+1"]}` | `transform_of` |
| `geom → zmgeom` | `geom → geom {"zero":"modified"}` | `special_case_of` |
| `norm → tnorm` | `norm → norm {"truncate":["-Inf","Inf"]}` | `special_case_of` |
| `tbinom → binom` | `binom {"truncate":["max(0, floor(a)+1)","floor(b)"]} → binom` | `truncation_of` |
| `tnorm → norm` | `norm {"truncate":["a","b"]} → norm` | `truncation_of` |
| `tpois → pois` | `pois {"truncate":["max(0, floor(a)+1)","floor(b)"]} → pois` | `truncation_of` |
| `zipf → zeta` | `zeta {"truncate":[1,"N"]} → zeta` | `truncation_of` |
| `zib → binom` | `binom {"zero":"inflated"} → binom` | `modification_of` |
| `zinb → nbinom` | `nbinom {"zero":"inflated"} → nbinom` | `modification_of` |
| `zinb → zmnbinom` | `nbinom {"zero":"inflated"} → nbinom {"zero":"modified"}` | `special_case_of` |
| `zip → pois` | `pois {"zero":"inflated"} → pois` | `modification_of` |
| `zmbinom → binom` | `binom {"zero":"modified"} → binom` | `modification_of` |
| `zmgeom → geom` | `geom {"zero":"modified"} → geom` | `modification_of` |
| `zmgeom → zmnbinom` | `geom {"zero":"modified"} → nbinom {"zero":"modified"}` | `special_case_of` |
| `zmlogarithmic → lgser` | `lgser {"zero":"modified"} → lgser` | `modification_of` |
| `zmlogarithmic → zmnbinom` | `lgser {"zero":"modified"} → nbinom {"zero":"modified"}` | `special_case_of` |
| `zmnbinom → nbinom` | `nbinom {"zero":"modified"} → nbinom` | `modification_of` |
| `zmpois → pois` | `pois {"zero":"modified"} → pois` | `modification_of` |
| `ztbinom → binom` | `binom {"zero":"truncated"} → binom` | `modification_of` |
| `ztbinom → zmbinom` | `binom {"zero":"truncated"} → binom {"zero":"modified"}` | `special_case_of` |
| `ztgeom → geom` | `geom {"zero":"truncated"} → geom` | `modification_of` |
| `ztgeom → ztnbinom` | `geom {"zero":"truncated"} → nbinom {"zero":"truncated"}` | `special_case_of` |
| `ztgeom → zmgeom` | `geom {"zero":"truncated"} → geom {"zero":"modified"}` | `special_case_of` |
| `ztgeom → geom` | `geom {"zero":"truncated"} → geom` | `transform_of` |
| `ztgeom → dlaplace` | `geom {"zero":"truncated"} → dlaplace` | `truncation_of` |
| `ztnbinom → nbinom` | `nbinom {"zero":"truncated"} → nbinom` | `modification_of` |
| `ztnbinom → zmnbinom` | `nbinom {"zero":"truncated"} → nbinom {"zero":"modified"}` | `special_case_of` |
| `ztpois → pois` | `pois {"zero":"truncated"} → pois` | `modification_of` |
| `ztpois → zmpois` | `pois {"zero":"truncated"} → pois {"zero":"modified"}` | `special_case_of` |

## Public surface, references and migration

R assembly, wrappers/help/exports, JS metadata, capabilities, family reductions, inference availability and site data were regenerated. Public-surface assertions confirm 86 evaluation families, 85 fitters, no retired R exports/help, no raw retired WASM registrations or public facade types, and no retired catalogue nodes or any of the 32 old site routes. [Machine-readable result](2026-11-13-retire-16-data/public-surface-check.json).

The NEWS migration entry gives all sixteen replacement calls, coefficient/attribute changes and the integer endpoint map `(a,b] → [max(0,floor(a)+1),floor(b)]`, with free endpoints and binomial size spelled out. It explicitly distinguishes the finite-zeta full-real exponent domain from the removed Zipf uniform-boundary restriction.

[remaining-references.csv](2026-11-13-retire-16-data/remaining-references.csv) lists **every remaining literal grep hit**, file, line, matched IDs, text and reason outside `dev/research/` and `dev/roadmap/`, including substring coincidences. No hit remains marked for review. The inventory honors source ignore rules, excluding generated build products. Dated evaluations and roadmap history were preserved. The protected status files and project instructions were not edited. Remaining real names identify private kernels, frozen fixtures, test coordinate maps, migration history or exact stop payloads; none register a public family. Internal stop-model IDs were preserved to keep the snapshots identical. Runtime messages do not recommend a removed wrapper. The measured test-tier inventory keeps original test labels for historical timing attribution.

Reason totals from the complete line inventory:

| Reason | Lines |
|---|---:|
| Protected project/status instructions retained under the job scope | 2 |
| Substring coincidence (for example logitnorm, gzip or Python zip); no retired family reference | 947 |
| Mathematical kernel terminology or linked migration/design context | 3 |
| Python zip/gzip operation; no retired family reference | 88 |
| Validation/historical provenance or internal kernel coordinate label, not a public family registration | 348 |
| Private modifier binding/evaluation coordinates; absent from the public family registry | 135 |
| gzip size measurement; no retired family reference | 1 |
| Retained kernel regression or test-only map to a public modifier route | 168 |
| Retained internal modifier kernel, helper or kernel include | 88 |
| Retained modifier kernel, binding adapter or frozen fixture at its new path | 2061 |
| Frozen measured test-tier baseline retains original test labels for attribution | 131 |
| Generated modifier capabilities include private kernel provenance; all public registry IDs survive | 1 |
| Generator handling private modifier evaluation kernels | 14 |
| Frozen internal stop-model payload identifier; retained for exact snapshot invariance | 8 |
| Shared modifier machinery or frozen internal stop coordinates; no retired public family registration | 11 |
| Migration entry or earlier release history | 29 |
| Modifier regression asserts a frozen internal stop-model code, not a public wrapper | 1 |

The link checker verifies 35 retired historical citations against `1e0f5185` instead of modifying those reports. Final source check: **0 new broken links**, with 45 inherited missing/parser destinations listed in the log. Site checks separately resolve all built site links and fragments.

## Final validation

All runs used the **workstation**, with `/tmp/umvl-cpu/run` or `run4`, pinned library threads and `nice -n 10`. No simbox run was attempted. Slot waits were allowed to finish. Logs are retained beside this report as `<check>.log.gz`.

| Check | Result |
|---|---|
| `just cpp-test` | **109/109 passed** |
| `just cpp-test-no-long-double` (GCC 15 workstation) | **136/136 passed** |
| `just test` (R dev) | 28,492 passes, 157 skips, no warnings; one missed retired test input corrected; targeted model-selection rerun **13/13 passed** |
| `just test-full` after that correction | **56,516 passes, 0 failures, 0 warnings**, one no-reference-values skip |
| `just js-build` | Passed |
| `just js-test` | 576 passed; one missed raw retired quantile call corrected; targeted replacement-route rerun **1/1 passed**, all three assertions retained |
| `just site-check` | Passed: 85 pages, 286 inference scopes, 264 rendered graph edges, browser checks, 85 compilable C++ snippets, MathML and links |
| `just relations` | **158 edges, 0 failed** |
| `python3 dev/tools/check_links.py` | **0 new broken links** |
| Public-surface guard | Passed |
| Frozen fixtures / moved header checks | **48/48** byte-identical files; **63/63** headers match under documented move normalization |

The first C++ dev run exposed two test-wiring mistakes (evaluation-only PGW fixture lookup and unresolved zeta window endpoint); both were fixed before the final complete C++ runs above. After late test-only R and JS fixes, only their targeted checks were repeated, as required. Full R already ran after the R correction. No numerical implementation was changed to make tests pass. The baseline snapshot discrepancy below is not counted as a passing zero-difference comparison.

## Open findings

1. **Supplied double survivor baseline: EMG archive `001311`.** The supplied older record stops numerically; this workstation replay reaches the unchanged 300-CPU-second process guard and records timeout. The sample has 12 observations and held `sigma=1.0518147225573364`. Rebuilding starting commit `1e0f5185` independently and replaying all 212 EMG cases gives exactly the final-tree records, including the timeout: **0/212 differ**. Thus retirement did not introduce it, but the requested **0 differences against the supplied double baseline is not met** (one case). Missing: resolution of the pre-existing baseline/runtime-budget discrepancy in the EMG job or an author-approved reproducible baseline. No timeout was hidden, budget altered or supplied record rewritten. [Starting-tree replay](2026-11-13-retire-16-data/replay-starting-emg.sh), [comparison](2026-11-13-retire-16-data/starting-emg-retirement-diff.txt), [older-baseline discrepancy](2026-11-13-retire-16-data/starting-emg-baseline-diff.txt).

2. **Unclassified old-wrapper/replacement differences: binomial windows `007339`, `007366`.** Both modes return different uncertified finite-size coefficients/log likelihoods between the old wrapper and approved route. The old wrapper's default work allowance and the seam's allowance differ (512 vs 200 in this comparison). Before/after retirement routes remain bit-identical. Missing: a separate numerical/work-budget audit to classify these two cases under the modifiers-2 taxonomy; no numerical fix belongs in this layout/interface job. [Exact input records](2026-11-13-retire-16-data/open-case-inputs.json) and the per-field comparison above preserve the finding.

3. **Inherited diagnostic generator gap.** Full legacy diagnostics generation encounters the existing CMP parser limitation. The explicit `diagnostics.py --retire-existing` projection removes retired routes from the checked-in prior sweep while retaining surviving snippets verbatim. Existing absent st/gengamma/expweibull scopes are not silently filled or claimed covered. Missing: an independent diagnostics-generator repair; retirement leaves no retired public dispatch in that sweep.

The report was sent to the artifact viewer with the prescribed `bin/show` command. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains available at its worktree path; no viewer settings or other worktrees were changed.

No additional numerical fix, explicit modifier-formal change or outcome-type change is included.
