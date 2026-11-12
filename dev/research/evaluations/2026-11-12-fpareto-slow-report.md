# TASK-205: fpareto fits exceeding 120 seconds

The runtime repair and Claude's review round are complete. Cases 1601 and 10072 again return their original **Pareto limit stop with exactly the same limit likelihood**. The three original runtime cases remain below 0.08 seconds in the final review replays. All **544 fpareto and 544 GB2 archive rows** were replayed before and after in **both arithmetic modes**. All 548 returned fits per mode retain text-identical coefficients, likelihood, status and certification flag; no original named stop or limit likelihood degrades. The requested targeted C++ checks in both modes, full-tier R files, and targeted JS checks are green.

Baseline: `bb46078757dad3538bfb263086d4e72cde163330`. Worktree: `fpareto-slow`, branch `codex/fpareto-slow`. All computation and final validation used the local workstation, GCC 15.2.0; simbox was explicitly off. Commands used the local single-slot or four-slot wrappers, with their nice priority and single-thread BLAS/OpenMP settings. No commits, board edits, sub-agents, dependency installations, pushes or publication were performed.

The review replay uses the binding defaults: fpareto `tol=1e-7, maxit=1000`, and GB2 `tol=1e-10, maxit=300`. The three runtime cases have `min=0` held and all other coefficients free. Before and after executables use the same two-family driver at `-O2`. The original baseline is reconstructed from Git; the uncommitted pre-review state is reconstructed from that commit plus the retained [pre-review delta](2026-11-12-fpareto-slow-data/review-before.patch). No source tree is archived.

The original seam audit used `-O1` and reported all three cases open at 120 seconds. Timing and timeout sets differ between that audit, the initial single-family replay, and this fresh two-family replay. An external guard is a measurement result, never a fitter status. The table below uses only the fresh review records; each seconds column is native / double-only.

| Case | Original seconds, N / D | Final seconds, N / D | Final profile evaluations, N / D | Final outcome, both modes |
|---|---:|---:|---:|---|
| 1599 | >120 (guard) / 104.522 | 0.072 / 0.023 | 18995 / 19327 | Double-Pareto |
| 1600 | 59.387 / 73.533 | 0.072 / 0.061 | 20077 / 20080 | Double-Pareto |
| 10717 | >120 (guard) / 98.488 | 0.054 / 0.048 | 12962 / 12978 | Power |

The outcomes are reported limit stops, not finite fpareto estimates. The corresponding original completed runs return the same named model and limit likelihood. All final runs retain finite likelihood witnesses.

The time went into GB2's repeated conditional power/scale solves. Each individual Newton and backtracking loop was capped, but every outer trial and every grid start restarted those allowances. Near the double-Pareto and endpoint seams, growing power and shrinking beta shapes make the conditional Hessian nearly singular. Tiny rounding differences after changing units could therefore send otherwise equivalent samples through very long sequences of unsuccessful refinements. Per-evaluation cost remains proportional to the number of observations; there is no support-size enumeration or count-magnitude sum.

The [direct profiling driver](2026-11-12-fpareto-slow-data/profile.py) reconstructs the two changed baseline headers with read-only `git show` and calls the C++ fitter without R. Eight-second `gprof` samples recorded 4,379,380 evaluations for case 1599, 3,865,767 for case 1600, and 12,946,385 for case 10717. `gb2_detail::evaluate` accounts for 88.62%, 84.79% and 79.18% of attributed self time respectively; `logbeta` supplies another substantial share. The full call graphs are [1599](2026-11-12-fpareto-slow-data/profile-1599.txt), [1600](2026-11-12-fpareto-slow-data/profile-1600.txt) and [10717](2026-11-12-fpareto-slow-data/profile-10717.txt). These interrupted profiles establish the hotspot, not a successful fit or a full-fit work count.

The review regression was bookkeeping, not exhaustion of the shared ledger or a smaller face budget. Native case 1601 used 29,036 of 128,000 allowed evaluations, and case 10072 used 30,113. Both had valid completed outward grid refinements, while the later selected basin refinements consumed their individual 1,000-evaluation allowances. `explored` recorded completion only in the latter pass. The best nomination was inside the first shape box, so the outer loop stopped and forgot the completed grid evidence; the final Pareto comparison was then bypassed.

The review fix records `candidate.outer_done` from valid all-free grid candidates in `explored` as well. Later incomplete refinements can no longer withdraw that evidence. The existing face comparison, limit likelihood calculations, starts, solver steps, tolerances and budgets are unchanged. A conditional-continuation experiment was discarded; it is not part of the final change. The [1601 trace](2026-11-12-fpareto-slow-data/review-trace-1601.txt) and [10072 trace](2026-11-12-fpareto-slow-data/review-trace-10072.txt) show the completed first-pass points and the unexhausted shared ledger.

The [GB2 change](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp) gives each all-free refinement one solver allowance shared by its nested likelihood evaluations, including inner Newton solves and backtracking. All starts and box expansions also share a 64-bit ledger of `128 * min(maxit, 100000)` evaluations. A failed refinement leaves work for subsequent starts. The ledger checks before spending, so it cannot overspend; the default shared ceiling is 128,000. Nonfinite Newton directions stop their unsuccessful conditional solve immediately. Held-parameter searches retain their existing allowances.

A successful, locally checked nomination survives an exhausted outer search as uncertified. If the search has not supplied an acceptable result, exhaustion produces a numerical stop with the budget flag and its finite witness when representable. The existing local accuracy checks remain in place. The new `profile_evaluations` field exposes the search ledger in both C++ result types, and [fpareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp) propagates it.

Witness likelihoods are recomputed at the literal physical coefficients, using compensated summation. Their practical floating-point margin is `128 * epsilon * M`, where `M` includes the absolute likelihood terms and a log-coordinate sensitivity allowance. The global upper bound remains infinite: a feasible witness is not a proof of a global maximum. The final diagnostic correction restricts these newly attached witnesses to the all-free route, preserving baseline held-parameter stop payloads.

The exact-arithmetic ledger bound is a work guarantee. The remaining mathematical evidence is numerical: independent 100-digit, exact-binary64-input calculations place all six native/double witness likelihoods inside their recorded bounds. Independent closed-form fits within the reported double-Pareto and power limit models reproduce the reported likelihoods within `5e-13`. Those checks validate the witnesses and the within-face optima; they do not supply a new global exterior certificate for the four-free GB2 family. Limit selection retains the existing numerical GB2 search/face-comparison rule. See the [witness checks](2026-11-12-fpareto-slow-data/witness-check.json), [double-only checks](2026-11-12-fpareto-slow-data/witness-check-double.json), and [face calculations](2026-11-12-fpareto-slow-data/face-check.json).

The [complete review comparison](2026-11-12-fpareto-slow-data/review-comparison.json) compares all 1,088 rows against both the original commit and the pre-review implementation, in each arithmetic mode. There are no returned-fit losses, coefficient/likelihood changes in returned fits, certification-flag losses, named-model changes, or named-limit likelihood changes. All 548 returned rows per mode are text-identical in their 17-digit coefficient, likelihood, status and certification fields. Final named-stop counts are 173 for fpareto and 176 for GB2 in each mode. No pre-review or final row times out.

Every change in those semantic result fields is listed below. A single label applies to both modes; differing labels are native / double. `Guard` is an external timeout and `Unresolved` is the fitter's `SearchUnresolved` stop. The 12 rows account for all 34 comparison/mode changes: 14 original-to-final changes and 20 pre-review-to-final changes. The linked JSON additionally lists every diagnostic-only row difference (iteration/work counters or stopped-candidate payloads), with its reason and complete before/after records; timing differences are excluded from semantic comparisons.

| Family and case | Original N / D | Pre-review N / D | Final N / D | Reason |
|---|---|---|---|---|
| fpareto 1599 | Guard / Double-Pareto | Double-Pareto | Double-Pareto | Bounded runtime; original native guard expired |
| fpareto 1601 | Pareto | Unresolved | Pareto | Restores original named decision |
| gb2 2054 | Power | Unresolved | Power | Restores original named decision |
| gb2 2056 | Unresolved | Unresolved | Power | Retained grid evidence completes the existing face comparison |
| gb2 2069 | Unresolved | Unresolved | Pareto | Retained grid evidence completes the existing face comparison |
| gb2 9484 | Unresolved | Unresolved | Pareto | Retained grid evidence completes the existing face comparison |
| gb2 9500 | Unresolved | Unresolved | Power | Retained grid evidence completes the existing face comparison |
| fpareto 9580 | Unresolved | Unresolved | Pareto | Retained grid evidence completes the existing face comparison |
| fpareto 9596 | Unresolved | Unresolved | Power | Retained grid evidence completes the existing face comparison |
| gb2 9976 | Pareto | Unresolved | Pareto | Restores original named decision |
| fpareto 10072 | Pareto | Unresolved | Pareto | Restores original named decision |
| fpareto 10717 | Guard / Power | Power | Power | Bounded runtime; original native guard expired |

Specifically, cases **1601** and **10072** retain Pareto likelihoods **-29.102002422134035** and **77.219355967752222** exactly in both modes. The review also restores the original named Power and Pareto stops for GB2 cases 2054 and 9976. Six other previously unresolved rows improve to named limits in each mode. No successful fit becomes a refusal and no named stop becomes unresolved.

The original [golden comparison](2026-11-12-fpareto-slow-data/golden-check.json) remains applicable: the review fix changes no estimator calculation, and both families' golden checks also pass in the rerun C++/R files. The initial supplementary `tol=1e-10` fpareto comparison preserved all 270 baseline returned certified rows; its before pass used a 10-second guard and is historical supplementary evidence, not the fresh default-control census above. Golden files were not changed.

The appended fpareto regressions use the three exact archived samples and assert positive work, the shared work ceiling, a terminal stop, and a finite translated witness with ordered likelihood bounds. Two appended GB2 tests force an exhausted one-evaluation ledger and verify that an unsuccessful start leaves allowance for the next start. There are no timing assertions. The initial [scope audit](2026-11-12-fpareto-slow-data/scope-check.json) checked that both original test files remain byte-identical prefixes and that the six owned fitter, test and dossier files are the only tracked changes. No d/p/q/r, special-function, binding, code-generation, NEWS, status or roadmap source was edited.

The review appended cases 1601 and 10072 to both owned C++ test files. They require the Pareto stop, exact equality to the closed-form Pareto likelihood in that arithmetic mode, agreement with the recorded original likelihood, bounded positive work, and no exhausted-budget flag. Existing tests remain byte-identical prefixes; dossiers, fpareto's wrapper, controls and numerical solves are unchanged. The [review scope audit](2026-11-12-fpareto-slow-data/review-scope.json) records these checks and final source hashes.

The requested review checks all passed on the local workstation:

| Review check | Result |
|---|---|
| GB2/fpareto C++ full-label shards, native | 2/2 passed |
| GB2/fpareto C++ full-label shards, double-only | 2/2 passed |
| GB2/fpareto R files, full tier | PASS 2,492; FAIL 0; WARN 0; SKIP 4 |
| Targeted WASM rebuild and JS tests | 209/209 passed |
| Both-family, both-mode replay comparison | No degradation; all 4,352 original/pre-review comparisons checked |

The [review validation record](2026-11-12-fpareto-slow-data/review-validation.json) and compressed `review-check-*.log.gz` files retain the commands and results. No complete suite was repeated for this narrowly scoped review fix, as requested. The following completed full-suite results are from the initial job, before the review correction:

All initial-job final commands below finished with exit zero on the workstation. The complete C++ checks use the project's default CMake configuration; the no-long-double check was explicitly run under GCC 15, not delegated to simbox.

| Final validation | Result |
|---|---|
| `just cpp-test` | 108/108 shards |
| `UMVL_TEST_TIER=full just cpp-test` | 135/135 shards |
| `just cpp-test-no-long-double` | 135/135 shards |
| `just test` | PASS 25,816; FAIL 0; WARN 0; SKIP 154 |
| `just test-full` | PASS 52,574; FAIL 0; WARN 0; SKIP 0 |
| `just site-check` | Passed catalogue, browser and numerical checks |
| Post-correction GB2/fpareto native C++ checks, full labels | 2/2 shards |
| Post-correction GB2/fpareto R checks, full tier | PASS 2,492; FAIL 0; WARN 0; SKIP 4 |
| Post-correction WASM rebuild and targeted JS checks | 209/209 tests |
| Retained numerical evidence checks | 5/5 commands |

The complete native C++ and R suites preceded the final, owned diagnostic-scope guard. Following the brief's late-fix rule, their affected family checks were rerun instead of repeating those complete suites. The complete double-only suite used the guarded C++ source. The site check passed after the dossier updates, and the final WASM rebuild and targeted tests followed the guard. The earlier changed-family iteration also passed the three dependency-selected C++ shards, selected R checks, and 213 targeted JS tests. All builds/tests were foreground commands and were waited for. Records are [C++/site validation](2026-11-12-fpareto-slow-data/validation.json), [R validation](2026-11-12-fpareto-slow-data/validation-r.json), [post-correction validation](2026-11-12-fpareto-slow-data/validation-late.json) and [numerical checks](2026-11-12-fpareto-slow-data/evidence-validation.json); corresponding compressed logs are retained alongside them.

An additional optimized-build experiment exposed an **open baseline tail defect outside this job's authorized scope**. With GCC 15, the unchanged Burr call `pburr(1e100, 1e-308, 1e308, 1, false, true)` and inverse-Burr counterpart return `-Inf` at `-O1`/`-O2`, rather than approximately `-230.2585092994`. The standalone reproducer also fails in optimized double-only mode and succeeds at `-O0` or `-O2 -fno-inline`. Its entire local header dependency closure is byte-identical to the baseline. The optional optimized C++ dev attempt therefore failed those two existing tail tests; the final project-default run passed them. The [optimization matrix and baseline hashes](2026-11-12-fpareto-slow-data/baseline-tail-check.json) and [failed attempt log](2026-11-12-fpareto-slow-data/optimized-ctest.log.gz) retain this evidence. A separately authorized repair must investigate/harden the inlined overflow handling around `special::log_softplus_product` and validate Burr/inverse-Burr tails under optimized GCC. The brief explicitly excludes changes to `special/` and `dpqr.hpp`, so no such repair or test rewrite was made here.

To reproduce, set the worktree-local temporary directory, then run the retained drivers through the appropriate slot wrapper. Use a fresh output filename for each archive replay; completed output files are resumable and are not silently replaced. Baseline reconstruction uses Git, not an archived source tree.

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
study=dev/research/evaluations/2026-11-12-fpareto-slow-data
/tmp/umvl-cpu/run python3 "$study/archive.py" --baseline --output build/scratch/fpareto-before.jsonl
/tmp/umvl-cpu/run python3 "$study/archive.py" --output build/scratch/fpareto-after.jsonl
/tmp/umvl-cpu/run python3 "$study/archive.py" --double --output build/scratch/fpareto-double.jsonl
/tmp/umvl-cpu/run python3 "$study/profile.py"
/tmp/umvl-cpu/run python3 "$study/check_evidence.py"
/tmp/umvl-cpu/run4 python3 "$study/validate.py"
/tmp/umvl-cpu/run4 python3 "$study/validate_r.py"
/tmp/umvl-cpu/run4 python3 "$study/validate_late.py"
```

The review replay and targeted checks are reproducible with the same temporary-directory setup:

```sh
study=dev/research/evaluations/2026-11-12-fpareto-slow-data
for revision in original pre-review final; do
  /tmp/umvl-cpu/run python3 "$study/review_replay.py" --version "$revision"
  /tmp/umvl-cpu/run python3 "$study/review_replay.py" --version "$revision" --double
done
/tmp/umvl-cpu/run python3 "$study/review_compare.py"
/tmp/umvl-cpu/run4 python3 "$study/review_validate.py"
```

Completed replay files are resumable; use new filenames or move the retained files before requesting a fresh measurement. Source hashes, controls and the 120-second external guard are recorded beside each replay. The review makes no change to the separately filed optimized-build pburr finding.

The requested artifact-pane display was attempted with `codex-math-web/bin/show`, but the viewer returned HTTP 403: this worktree is outside its configured readable roots. Display remains open pending viewer root configuration or registration of this existing thread/worktree. No viewer configuration was edited, no agent was spawned, and no report was copied into another checkout. The local report and all its relative links were validated.

The runtime repair, archive preservation checks, regressions, required final validation and this report are complete. No started build or test remains running.

Merge review (Claude): an exhausted all-free ledger no longer skips the face comparison. With completed grid evidence the existing face decision applies unchanged; without it, a best interior point that does not beat a computed limit face is a numerical stop with the budget flag and its witness, not an uncertified `Ok`. No archive row reaches this branch (no all-free row exhausts the ledger in either mode), so the replay tables above are unaffected.
