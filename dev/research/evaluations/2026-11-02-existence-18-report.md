# existence-18: discretized gamma existence and prose

Completed 2026-11-02 for TASK-101.6.4.1 in `codex/existence-18`.
Tested source: `8b261c7669ac20f25342c959e8c04d3cde076f12` plus the uncommitted dossier changes described here. No commits were made.

## dgamma

**Statement:** With both coefficients free, a finite MLE exists exactly for integer sample range at least two; with either coefficient held, it exists exactly when a positive count occurs, and the held-shape maximum is unique; with both held, only likelihood evaluation remains.

**Grades:** Existence and the declared escapes are **proved** by the dossier's boundary arguments. Held-shape uniqueness is **proved**. Joint uniqueness and uniqueness with rate held remain **unknown**, so the new metadata uses `uniqueness: unknown`.

For small shape, the positive-cell bound
$p_j \le a^a e^{-a}\log(1+1/j)/\Gamma(a)$ tends to zero uniformly over rate. For large shape, an escaping latent mean loses all fixed cells, while a bounded mean gives vanishing variance and permits positive limiting mass in at most two adjacent cells. With shape bounded away from zero and infinity, escaping rate loses an occupied cell. Thus positive likelihood level sets are compact for sample range at least two. Continuity gives attainment. The same boundary analysis gives attainment for either held coefficient whenever a positive count occurs. Constant and adjacent-cell free samples approach their saturated multinomial bound through gamma concentration, but every finite gamma assigns positive probability outside the observed cells and strictly misses that bound. All-zero samples with exactly one free coefficient approach probability one only at an excluded boundary.

With shape held, integration of the log-concave log-gamma location density over fixed log-cell intervals gives a concave log likelihood in log rate. It is analytic and tends to minus infinity at both ends when a positive count occurs. Two maximizers would force a constant interval and, by analyticity, a constant function, contradicting those tails. This supplies a checkable uniqueness argument without a new external citation. No web searches or new bibliographic references were used.

**Changes:** Added the existence statement and reason. Rewrote all three limit paths and data conditions as sentences using the coefficient symbols. Specified a fixed mean strictly inside the cell for the point-mass concentration path. Explained the latent coefficients and the effect of holding either one. Kept the existence, identification and search-certificate derivations. Moved the shifted Bernoulli R call into `R notes`, and dated implementation/validation material into `Decision history`. Put display-math delimiters on separate lines. Clarified that the conditional covariance is nonnegative and its negative is the rate derivative.

**Statuses and other metadata:** No status changes. All limit firmness flags, held sets, bounds, fit metadata, inference metadata, density definitions, relations, examples and catalog fields remain unchanged. `holds: []` remains appropriate: no hold subset has structural nonexistence for every sample. There are no advice or fit-domain reasons to rewrite. No matching adapter messages or R test regexes needed changes. Fitters, d/p/q/r, goldens and truth files were untouched.

**Disagreements:** No disagreement was found among the existence regimes, proved limit records, empty structural hold list and fitter branches. The old blanket sentence that uniqueness had not been established was too broad given the existing held-shape argument; it now explicitly applies to joint and held-rate fits. The old covariance wording was ambiguous about the sign of the derivative; the monotonicity and implementation were already consistent.

**Open:** Joint uniqueness and held-rate uniqueness require a proof excluding distinct maxima, or a counterexample. Identification from tail probabilities does not provide that proof. The current shape search deliberately does not assume unimodality. Its existing certificate is conditional on computed comparisons with a practical $10^{-8}n$ likelihood allowance; a uniform special-function error guarantee remains unavailable. No numerical code change or new claim of outward-rounded certification is made here. Existing bound-check scripts and their historical revision, seeds and tolerances remain linked in the dossier; they were not rerun for this prose change.

## Checks

All runs were local on the workstation, through `/tmp/umvl-cpu/run` or `run4`, under the lock wrappers' `nice -n 10`. The wrappers pin OpenMP and BLAS to one thread and reserve at most four slots. Temporary/runtime files used `build/tmp`; logs used `build/scratch`. Environment: R 4.6.1 (2026-06-24), Python 3.14.5. No simbox, package installs, board edits or Git writes were used.

| Check | Result |
| --- | --- |
| `/tmp/umvl-cpu/run4 just regen` | Passed. Also rerun as part of final `just test` after the final prose edits. |
| Targeted `testthat::test_local('.', filter = 'dist-(dgamma)\|conformance\|restricted\|generated-dpqr\|parameter', stop_on_failure = TRUE)` from `univariateML-r`, with `UMVL_DISTRIBUTIONS_DIR` set, under `run4` | FAIL 0, WARN 0, SKIP 1, PASS 7700. The skipped dgamma independent-optimizer test requires the full tier. |
| `/tmp/umvl-cpu/run python3 site/scripts/data.py` | Passed, 99 families. Repeated after the final dossier edit. |
| `/tmp/umvl-cpu/run just relations` | Passed, 157 edges, 0 failed. |
| `/tmp/umvl-cpu/run4 just cpp-test` | Passed, 106/106 development shards, 0 failed. |
| `/tmp/umvl-cpu/run4 just test` | Passed, FAIL 0, WARN 0, SKIP 152, PASS 20493. Development-tier skips are reported, not passes. |
| All `man/ml*.Rd` checked with `tools::checkRd`, excluding Non-ASCII diagnostics | No output, including `mldgamma.Rd`. |
| README control-character scan using the requested `grep -lP` pattern | No output. |
| `git diff --check` | Passed. |

The generated `mldgamma.Rd` was inspected: it contains the existence statement/reason, full limit sentences, coefficient explanation and R usage guidance. These checks establish generation consistency and regression compatibility; they do not turn an unknown uniqueness result into a numerical or mathematical proof. The full test tier was not required because fitting, d/p/q/r and test infrastructure were not changed.
