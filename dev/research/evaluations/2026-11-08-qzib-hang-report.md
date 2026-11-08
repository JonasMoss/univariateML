# TASK-153: discrete quantile termination

Completed in `codex/qzib-hang`, against HEAD `cfb7c4b37d13d1f23ca8ceb546333eb87290b1f6`. No commits, board edits, dependency installations or changes to fitter implementations, dossier metadata, NEWS, status files or roadmap files were made.

The original call `mlzib(0, size=1e308, prob=.9, pi=0)` now terminates with a clear median-evaluation error. With data, it preserves the native numerical condition and its witnesses. The underlying `qzib(.5,1e308,.9,0)` returns NaN. A deterministic huge model, `size=1e308, prob=1, pi=0`, retains its exact median `1e308`.

All required final suites passed. All 297 canonical golden, truth-table and truth-generator files remain byte-identical to HEAD.

## Search contract and termination argument

The common implementation is in [special/tails.hpp](../../../univariateML-cpp/include/cppunivariate/special/tails.hpp). A comparison has three outcomes: below the jump, at/above the jump, or evaluation failure. Invalid tails and NaNs cannot become successful comparisons.

For an interior target, bisection maintains `F(lo) < p <= F(hi)`, expressed in the selected log tail. It returns only after the bracket contains one integer step. Midpoints use `lo + (hi-lo)/2` when the difference is finite and `lo/2 + hi/2` otherwise, followed by flooring. A midpoint that cannot lie strictly inside the bracket produces NaN; an adjacent pair of representable doubles whose distance exceeds one does not identify an integer quantile.

In exact arithmetic, each step leaves width at most `ceil(width/2)`. Any finite binary64 bracket has width less than `2^(max_exponent+1)`. The structural bound `max_exponent+3` therefore suffices; exhaustion returns NaN. Nonnegative bracket growth starts at least at one, doubles, and evaluates a final clamped `DBL_MAX`, within `max_exponent+1` evaluations. Two-sided searches have at most `max_exponent+2` expansions per direction, finite clamping and explicit progress checks.

These are mathematical termination and rank arguments conditional on the computed CDF comparisons and the family's CDF monotonicity. They do not establish outward-rounded CDF bounds. Existing truth tolerances and declared adjacent-lattice alternatives remain unchanged for finite results. No new probability tolerance was introduced.

The million-term guards added to CDF recurrences and the logarithmic-series sampler are evaluator guards. Exhaustion returns NaN, never a truncated tail or unchecked quantile. These pure d/p/q/r operations have no fitting `iterlim`; the evaluator exception in [work budgets](../../design/work-budgets.md) applies.

## Complete discrete-route inventory

The scan covers all 99 primary `dpqr.hpp` files, both shared `dpqr_core.hpp` files and all six `special/` headers: 107 source files. The [machine inventory](2026-11-08-qzib-hang-data/audit.json) records every nominated `for`/`while` line and source SHA-256. The table below accounts for all 35 discrete families. Paths in this table are relative to `univariateML-cpp/include/cppunivariate/distributions/`.

| Family | Quantile loop or route checked | Verdict and action |
|---|---|---|
| bbinom | `dpqr.hpp::qbbinom`, integer bisection | Size is capped at `10^7`, so midpoint arithmetic and strict integer progress are safe. Added NaN-tail rejection. |
| binom | `qbinom` → shared inversion | Shared overflow, stagnation, NaN and growth guards; also repaired its descending CDF recurrence and backend quantile seam. |
| bnbinom | `qbnbinom` → shared inversion | Failed beta-mixture tails now propagate. Repaired the shared gamma-ratio recurrence that could stall inside this CDF. |
| cmb | `Table::quantile`; geometric branch → shared inversion | Integer table bracket is bounded by `10^7`. Added NaN rejection and preserved failed normalization through cumulative tables. |
| cmp | `Law::quantile`, expansion and bisection | Replaced with bounded inversion; removed the arbitrary `9e15` search cutoff. Normalizer failure remains NaN. |
| dgamma | `qdgamma`, two upward and two downward corrections | Already bounded; added final inverse inequalities and representable-predecessor checks. Explicit probability endpoints bypass interior certification. |
| dlaplace | `qdlaplace`, left/right growth and bisection | Replaced both growth loops and bisection with guarded two-sided inversion; removed false infinities from the old step cutoff. |
| dnorm | `qdnorm`, two upward and two downward corrections | Already bounded; finite answers now require both inverse inequalities and a representable predecessor. |
| dunif | `qdunif` → shared inversion | Overflowing signed widths and unrepresentable integer jumps now produce a certified bracket result or NaN. |
| dweibull | Closed-form hazard inversion, two conditional corrections | No quantile loop. No termination change needed. |
| geom | `qgeom` → shared inversion | Bounded growth, NaN propagation and a numerical stop beyond integer resolution. |
| hyper | `qhyper`, bounded support enumeration | Safe under `N <= 2^53-1`: every unit increment, including the final exit increment, is representable. Valid-count PMF arithmetic stays in finite scalar ranges. No nontermination defect found. |
| lgser | `qlgser` → zero-detail shared inversion; `rlgser` inverse recurrence | Shared quantile guards. Sampler now rejects zero/nonfinite mass, NaN uniform input, lost unit progress and evaluator exhaustion. Ordinary recurrence arithmetic is retained. |
| nbinom | `qnbinom` → shared inversion | Shared guards, descending CDF recurrence protection and bounded backend seam. |
| nhyper | `qnhyper`, inclusive integer bisection | Count domain gives at most 53 halvings; `mid+1` progresses exactly. Added NaN-CDF rejection. |
| pig | `qpig` → shared inversion | Existing CDF exhaustion is now a numerical stop, rather than an apparent jump. Regression uses mean and shape `1e8`. |
| pois | `qpois` → shared inversion | Shared guards, descending CDF recurrence protection and bounded backend seam. |
| skellam | `qskellam`, two growth loops and bisection | Guarded signed inversion; initial scale uses `hypot(sqrt(mu1),sqrt(mu2))`, avoiding overflow of `mu1+mu2`. CDF descent is also bounded and checked. |
| tbinom | `truncated_detail::quantile` | Guarded finite-support inversion and NaN propagation. |
| tpois | Same shared truncation quantile, including unbounded growth | Guarded growth replaces `2*right+1` and the `9e15` false-infinity cutoff. |
| yulesimon | `qyulesimon`, expansion and bisection | Bounded search retaining the supplied probability scale/tail comparisons; failed CDFs and unresolved integer spacing return NaN. |
| zeta | `qzeta`, expansion and bisection | Same guarded search; no unchecked adjacent-double answer or interior false infinity. |
| zib | `qzib`, finite-support bisection | Safe common search; NaN handling, strict progress, width-derived cap, exact deterministic shortcut and canonical discrete log-tail conversion. |
| zinb | `qzinb` → shared inversion | Failed base NB tail propagates instead of becoming a jump. |
| zip | `qzip` → shared inversion | Failed base Poisson tail propagates instead of becoming a jump. |
| zipf | `qzipf`, finite bisection | `N <= INT_MAX` already prevented midpoint/stagnation defects. Moved to the common guarded search to reject NaN CDFs. |
| zmbinom | `zero_detail::quantile`, kind 3 | Shared search and binomial CDF guards. |
| zmgeom | Same, kind 1 | Shared search; unrepresentable-rank regression. |
| zmlogarithmic | Same, kind 4 | Shared search; failed series CDF now returns NaN. |
| zmnbinom | Same, kind 2 | Shared search and NB CDF guards. |
| zmpois | Same, kind 0 | Shared search and Poisson CDF guards. |
| ztbinom | Same, kind 3 | Shared search and binomial CDF guards. |
| ztgeom | Same, kind 1 | Shared search; unrepresentable-rank regression. |
| ztnbinom | Same, kind 2 | Shared search and NB CDF guards. |
| ztpois | Same, kind 0 | Shared search and Poisson CDF guards. |

New family cases were appended to the existing C++ `test.cpp` files. The two routes without a termination defect, hyper and dweibull, retain their existing tests. Regression cases cover each other family, and shared-helper tests inject failed comparisons at bracket endpoints and inside bisection.

## Supporting loops and delegated searches

| Source/function | Loops checked and verdict |
|---|---|
| `special/tails.hpp::discrete_bisect`, `discrete_search`, `discrete_signed_search` | One bisection, one nonnegative expansion and two signed expansions. All have the structural bounds and failure rules above. |
| `special/special.hpp`, binomial/NB/Poisson quantile wrappers | Removed delegation to Boost discrete quantiles. The installed Boost `inv_discrete_quantile.hpp` has rounding loops outside the root-solver counter, without explicit NaN checks. The seam now uses no-throw CDFs and bounded integer inversion; it does not rely on those backend loops. Zero-rate/zero-size atoms are handled explicitly. This is a source-audit finding, not a claim that a backend hang was reproduced. |
| `special/tails.hpp::log_gamma_ratio` | Its small-argument shift loop could stall when `z+h` cancelled to zero while `z+1==z`. Added finite/domain checks, progress rejection and a 17-shift guard; positive exact arguments need at most 16 shifts. Direct and bnbinom regressions cover the failure. |
| `special/cmp.hpp::residual` | Both nearby-integer sum loops now require representable unit steps at both endpoints. Otherwise they use the existing analytic residual path. Tested at the precision-dependent loss of unit spacing. |
| `special/cmp.hpp` normalizer | Direct enumeration consumes the existing ledger; Euler–Maclaurin panels and correction sums are bounded. No additional unbounded quantile search found. |
| `special/tails.hpp::log_series` | Existing million-term cap and `next==k` handling; already terminates with a remainder-based result or NaN. No change. |
| `binom`, `pois`, `nbinom` descending CDF sums | Added million-term guards, lost-unit-step rejection and nonfinite accumulation checks. Boost NaNs are rejected before fallback selection. |
| `ztpois/dpqr_core.hpp::positive_tails` | Prefix enumeration now has a million-term cap, progress and NaN checks; failed upper-tail evaluation propagates. Applies to all ten zero-modified/truncated routes above. |
| `tpois/profile.hpp::mass` | Both directional sums already have two-million-step guards and propagate failed normalization through `Mass.ok`. No change. |
| bbinom PMF/table/log-sum loops; CMB table/cumulative loops | Integer counts bounded by `10^7`; no overflow/stagnation. Added CMB normalization-failure propagation and quantile NaN gates. |
| hyper/nhyper CDF enumeration | Exact unit spacing throughout the declared count domain. Hyper's `bd0` subseries is bounded at 1,000 iterations. No unbounded quantile dependency found. |
| zeta/Yule–Simon/Zipf CDF prefixes | Fixed small prefixes (including 1,024-term sums), or the finite Zipf count domain. The scalar gamma-increment shifts operate below 16 and progress. No unbounded integer search in these evaluators. |
| PIG mass/CDF streams | Integer indices remain below the existing `10^7` evaluator limit; tail exhaustion already returns NaN. The inversion caller now respects that failure. |
| Skellam mass/CDF sums | The small-argument descending Bessel sum has bounded order; its ascending sum already has a million-step guard. Added a cap, stagnation and NaN rejection to CDF descent. |
| dnorm/dgamma corrections and discrete-gamma lower-tail series | Corrections are fixed at two per direction; gamma series is bounded at 100,000. Added final certification for the quantile corrections. |
| `special/special.hpp::fs_log_quantile` | Continuous seed fallback used by dnorm/dgamma: growth is bounded by its `DBL_MAX/2` check, and bisection by 1,100 steps plus stagnation/NaN checks. Discrete callers now certify the resulting integer. |
| Remaining continuous d/p/q/r routines, `beta_tails.hpp`, `skewed_densities.hpp` | No additional discrete integer-inversion loop. Continuous quantile algorithms are outside this audit's scope. |

The unused `zipf_cdf` table helper is accounted for separately below; the public `qzipf` and `rzipf` routes do not call it.

## Exact log-half regression

`qzib` now uses `discrete_log_tails`, including its established half-probability correction. For `size=10, prob=1, pi=.5`, the CDF is exactly one half at integers 0 through 9. Plain input `.5` therefore returns 0. The binary64 log input `-0.6931471805599453` represents a probability strictly above one half and must return 10.

The independent [100-digit Decimal calculation](2026-11-08-qzib-hang-data/log-half.json), generated by [log-half.py](2026-11-08-qzib-hang-data/log-half.py), gives `exp(input) = 0.5000000000000000115952340692314982...`. C++, R and JS regressions check this distinction. No existing truth-table cell was edited.

## R construction and explicit numerical stops

`ml_build_fixed_model` evaluated a median before computing the held likelihood. The old C++ loop never returned, so no R error handler could run. Its median guard also accepted infinity, and its likelihood path accepted a nonfinite sum of otherwise finite log densities.

The constructor now rejects a nonfinite median with `median quantile evaluation failed: invalid held coefficients or numerical failure`, and rejects a nonfinite likelihood sum. With data, existing adapter diagnostics remain available. Native numerical conditions retain their class and witnesses, with median-failure context added; invalid-input diagnostics are preserved. Frozen models without data report the evaluation failure directly. CMP regressions verify that evaluator failures remain numerical and do not acquire fitting-budget advice.

An integer search cannot certify a unit-spaced jump once its bracket is wider than one and contains no representable interior point. C++, R and JS truth consumers now permit an explicit NaN only for **interior discrete** targets whose reference quantile exceeds `2^53`. Endpoints, ordinary counts, continuous families and inaccurate finite answers still fail their original checks. The unchanged oracle values are not replaced by NaNs.

The final JS truth run recorded **244,598 checks, zero discrepancies and 134 explicit numerical stops**: bnbinom 64, Yule–Simon 36 and zeta 34. These stopped cells are not claimed as successfully computed quantiles. See [js-truth-results.json](2026-11-08-qzib-hang-data/js-truth-results.json).

For the R zeta and Yule–Simon inverse samplers, the full consumer replays the exact seeded uniforms, checks equality to the direct quantile call, and permits a stopped draw only when that uniform exceeds `F(2^53)`. Other sampler failures remain failures. This handles the heavy zeta full-tier point without silently discarding arbitrary NaNs.

## Validation

All jobs ran in the foreground under the requested slot wrappers or simbox runner, with at most four cores in total and BLAS/OpenMP pinned to one thread. One simbox outage triggered the prescribed local fallback; the completed final R runs used simbox. Workstation C++ used GCC 15.2; simbox standalone C++ used GCC 13.3. Simbox R used its configured GCC 15.2.

| Check | Machine | Final result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 dev shards |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129; 278.14 s |
| `just cpp-test-no-long-double` | simbox | 129/129; 212.58 s |
| `just cpp-test-no-long-double` | workstation | 129/129; 304.33 s |
| Rebuilt no-long-double `distribution_zib` | both machines | 1/1 each, including the final log-half case |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21,857 |
| Full-tier generated-dpqr file | simbox | FAIL 0, WARN 0, SKIP 0, PASS 12,602 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44,692; elapsed 324.093 s |
| `just js-build` | workstation | exit 0 |
| `just js-test` | workstation | 537/537; 232.744 s |

The final log-half case was added after the full precision runs, so its rebuilt family shard was checked on both machines. The final normal C++ full run, JS suite and R full run also include it. The final R full run includes the sampler replay checks added after development-tier validation.

Required JS testing also exposed a stale assertion expecting `unresolved` for four binomial-relative examples. A read-only HEAD header snapshot and the working core produced byte-identical displayed classifications and bounds: `ztpois`, `zmpois`, `zip` and `tpois` limits. The relevant fitter and common-binding files are unchanged. The JS test now checks those specific models, finite bounds and retained candidates. This is baseline-consistency evidence, not a new proof of their likelihood classifications. Reproducers and captures: [fit-stops.cpp](2026-11-08-qzib-hang-data/fit-stops.cpp), [HEAD](2026-11-08-qzib-hang-data/fit-stops-head.txt), [working core](2026-11-08-qzib-hang-data/fit-stops-current.txt), [WASM](2026-11-08-qzib-hang-data/wasm-stops.txt), and [snapshot helper](2026-11-08-qzib-hang-data/snapshot_headers.py).

Compact final-run records are retained in [final-runs.json](2026-11-08-qzib-hang-data/final-runs.json). The core stress calls and results are in [quantile-probe.cpp](2026-11-08-qzib-hang-data/quantile-probe.cpp) and [quantile-probe.txt](2026-11-08-qzib-hang-data/quantile-probe.txt).

The optional repository-wide Markdown link scan reported six destinations in four unchanged sources: two absent inference screenshots, the absent boundary-points PDF, the `1+m/r` expression parsed as a link in the nbinom dossier, and two `/inference/` site-route references. Each affected source is byte-identical to HEAD. The new report's own destinations pass the same destination checker; details are in [link-check.json](2026-11-08-qzib-hang-data/link-check.json). These pre-existing documentation findings do not affect the required numerical suites.

No dossier or public fitter signature changed, so the site-check trigger does not apply.

## Fixture integrity and reproduction

[audit.py](2026-11-08-qzib-hang-data/audit.py) compares the exact bytes of all 99 `golden.json`, 99 `truth.json` and 99 `truth.py` files to HEAD. There are **zero changes**. Their aggregate SHA-256, including ordered paths and separators, is:

`7d6868570e0d008679d5340fe9dd1bcf445f9d38968ed963d298b4d1ab1943cc`

Run the check from the repository root through `/tmp/umvl-cpu/run`:

```sh
python3 dev/research/evaluations/2026-11-08-qzib-hang-data/audit.py
python3 dev/research/evaluations/2026-11-08-qzib-hang-data/log-half.py
```

Compile the quantile probe as C++17 with `-IunivariateML-cpp/include` and the existing BH include directory returned by `Rscript -e 'cat(system.file("include",package="BH"))'`. No R runtime is used by the executable. The baseline fit probe additionally needs `-Ibindings` and was compiled with `-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`; use `snapshot_headers.py REVISION DESTINATION` to materialize HEAD headers inside scratch, then select that include root for the baseline build. No Git state is written.

## Separate source findings

No assigned quantile-termination or fixed-constructor item remains open. Two adjacent helpers need separate follow-up, and were not changed under this quantile repair:

- `zib/dpqr.hpp::dzib` narrows a positive double observation to `int` before `bin_logpmf`. Valid large counts such as `x=size=1e308, prob=1, pi=0` exceed that conversion's range. It needs a double-count density route or an explicit numerical failure before conversion. R's fitting-data integer limit does not protect direct core density evaluation.
- `zipf/dpqr.hpp::zipf_cdf` builds a table with `for (int k=1; k<=N; ++k)`. At `N=INT_MAX`, the final increment overflows a signed integer. This table helper needs a safe index type or a tighter validated bound. The repaired `qzipf` and its inverse sampler do not use this helper.

These are source-level range findings, not measured alternative density values or changes to the frozen references.

## Artifact delivery

The requested `bin/show` call returned HTTP 403 because this worktree is absent from the viewer's readable roots. The report remains at its requested worktree path. Display requires registering this worktree with the viewer; no new agent thread or external viewer configuration was created.
