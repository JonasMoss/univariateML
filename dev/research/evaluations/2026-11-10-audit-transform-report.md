# TASK-101.7.5: information loss in transformed-data fitters

All 68 ranked silent cases were reproduced at baseline `c9c073e22be560db48c33fc1f8d8711bcef4b860` and resolved. Sixty-seven now meet the requested `1e-10` relative coefficient and log-likelihood comparisons against independent raw-model references. Inverse-Weibull case 4283 correctly reports numerical failure: its fully held log-likelihood is approximately `-1.0656757388606e385`, outside finite double range. It previously returned `Ok` with `-Inf`. The additional logit-normal case 9806 also passes. All required workstation suites finished green.

The baseline and repaired replay cover **all 824 frozen cases** for the eight families. The original archived `reproduce.py` was restored under `build/scratch/fit-audit`, with its root relocated, and run for every ranked case before and after the changes. No baseline evidence was regenerated from repaired code. The [provenance](2026-11-10-audit-transform-data/provenance.json) records the base commit and hashes of the frozen harness, case set and audit references.

| Family | All cases | Ranked cases | Successful statuses, before → after | Reported/raw likelihood misses, before → after |
|---|---:|---:|---:|---:|
| invgamma | 96 | 6 | 96 → 96 | 3 → 0 |
| lgamma | 80 | 6 | 80 → 80 | 2 → 0 |
| naka | 96 | 10 | 93 → 93 | 2 → 0 |
| lnorm | 100 | 5 | 100 → 100 | 3 → 0 |
| logitnorm | 88 | 4 | 88 → 88 | 2 → 0 |
| llogis | 88 | 10 | 88 → 88 | 2 → 0 |
| invweibull | 84 | 17 | 84 → 83 | 2 → 0 |
| invburr | 192 | 10 | 144 → 144 | 3 → 0 |
| **Total** | **824** | **68** | **773 → 772** | **19 → 0** |

Case 4283 is the only status transition. All other existing non-success statuses are unchanged. The 68 ranked failures include coefficient errors, so their count differs from the 19 reported-versus-returned-point likelihood mismatches. The latter use `abs(reported - raw) / max(1, abs(raw)) > 1e-10`.

The [transition table](2026-11-10-audit-transform-data/transitions.csv), [full comparison](2026-11-10-audit-transform-data/comparison.json), [baseline CSV](2026-11-10-audit-transform-data/before.csv), [repaired CSV](2026-11-10-audit-transform-data/after.csv), and original-reproducer [before](2026-11-10-audit-transform-data/before-reproduce.log) and [after](2026-11-10-audit-transform-data/after-reproduce.log) logs retain individual outcomes.

The changes compute statistics from raw observations rather than rounding a transformed sample before fitting:

| Route | Implementation |
|---|---|
| invgamma, naka, lgamma | Raw reciprocal, square and log gamma statistics in `gamma/transformed.hpp`; normalized means and centered Jensen gaps; explicit restoration of inverse-gamma beta, Nakagami second moment and log-gamma rate. |
| lnorm, logitnorm | Shared centered log/logit statistics and closed normal formulas; a two-component absolute log mean preserves small nonzero means and held-mean residuals. |
| llogis, invweibull, invburr with first shape held | Shared logistic/Burr/Gumbel score brackets in standardized raw log differences, followed by checked restoration of public scale. |
| invburr with scale held and both shapes free | Retained existing Burr global certificate, then refined its scalar score using raw log ratios. Failure of this tighter refinement retains the successful search result explicitly uncertified, with its raw likelihood recomputed. |

The three shared headers are [transformed_data.hpp](../../../univariateML-cpp/include/cppunivariate/core/transformed_data.hpp), [transformed_location.hpp](../../../univariateML-cpp/include/cppunivariate/core/transformed_location.hpp), and [gamma/transformed.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/transformed.hpp). Each public family selects its route explicitly. The existing inverse-Burr all-free global search and other held configurations retain their existing certificate machinery; this job does not introduce a new global theorem for that model. Ordinary log-gamma delegation is retained only after the raw-statistic solve checks its coefficients and likelihood at the tighter of the requested tolerance and `1e-12`.

Centered differences use `log1p((x - x0) / x0)` near the anchor. Compensated sums and two-component products/divisions retain small mean displacements. An FMA-based, two-component log anchor avoids relying on extended `long double`; range reduction and 32 atanh terms have an exact-arithmetic truncation bound below `2e-50`. Normal residual sums are scaled before squaring. Broad gamma samples accumulate positive normalized observations directly, avoiding subtraction from one. Product association permits a finite Nakagami mean when the largest individual square overflows, and finite inverse-gamma coefficients when an individual reciprocal overflows. Scale restoration uses `expm1` near the raw anchor and log coordinates over wide ranges.

The mathematical certificates concern the underlying real likelihood, conditional on computed comparisons. The normal statistics give closed-form conditional maxima. For gamma, the centered Jensen gap gives the unique positive root of `log(a) - digamma(a) = s`; monotonicity supplies a bracket, and safeguarded Newton steps retain signed endpoints. With a fixed rate, the digamma score is monotone. For logistic, fixed-first-shape Burr and Gumbel log-data likelihoods, strict concavity in power and power times location identifies the maximum; held scale restricts these coordinates to a line. Inner location scores have unique roots. Constant-data nonexistence is distinguished from a rounded transform becoming constant.

These are exact-arithmetic certificates **conditional on the computed signs**, with practical floating-point margins. Restoration checks include a `128 * epsilon` magnitude allowance in addition to tolerance. They are not outward-rounded interval proofs. Solver caps use the shared `min(iterlim, 100000)` policy; the inverse-Burr certificate refinement uses its certificate-evaluation allowance. A failed search does not return a capped iterate.

Finite coefficient restoration can lower a highly concentrated likelihood visibly even after the continuous root is resolved. Those results are retained with C++ `certified = false`, R `attr(fit, 'certified') = FALSE`, and JavaScript `certified: 0`. Their reported likelihood is evaluated at the actual returned coefficients. This flag propagates through Frechet, Pareto3, GB2 and Fpareto reductions. Ordinary certified R/JS results retain their existing output shape. No generated R d/p/q/r or codegen scripts were edited.

The [69 independent references](2026-11-10-audit-transform-data/reference.json) use exact binary64 observations and held coefficients, raw model equations, and mpmath roots at 180 and 360 digits. The two precisions agree within `1e-90` at the recorded precision. This is high-precision numerical evidence, supported by uniqueness arguments for the relevant routes. Among the 68 successful referenced cases, the largest coefficient relative error is `3.88e-14` and the largest **strict relative** log-likelihood error is `4.26e-11`. All 772 successful replay results also pass raw returned-point likelihood checks; the largest normalized discrepancy is `4.96e-14`.

The [18 additional restoration/range checks](2026-11-10-audit-transform-data/restoration-reference.json) compare continuous-root coefficients and the raw likelihood of the returned point at both precisions. They include ulp-wide samples, parent reductions, scales `1e-150` and `1e150`, held power `1e-20`, a six-hundred-order sample range, an overflowing reference square/reciprocal, and a 100,001-observation broad sample. Twelve fits remain explicitly uncertified because public coefficient rounding prevents transfer of the continuous likelihood certificate. Their likelihood comparisons are to the returned point, not a claim that this point attains the continuous maximum. The largest coefficient error in these checks is `2.19e-13`; the largest normalized returned-point likelihood error is `1.20e-15`.

C++ and R regressions were appended for every ranked case and case 9806, with further restoration, budget/status transport, overflow and parent-flag regressions. JavaScript adds concentrated-reference and flag tests. The eight family dossiers and the Frechet delegation note were updated. One existing lognormal assertion was corrected from nonexistence to **finite success**: for `x = {2,2,2}` with binary64 `meanlog = log(2)` held, the true residual is `2.3190468138462995584e-17`, so the conditional standard deviation MLE is positive. Independent high-precision evidence verifies its likelihood `108.57211386594349`. No existing success expectation was weakened to permit a stop, and no goldens or default tolerances were changed.

Final validation ran on the workstation with GCC 15.2.0, R 4.6.1, BH 1.90.0.1, Python 3.14.5 and mpmath 1.3.0. The four-slot launcher applies nice level 10 and one BLAS/OpenMP thread per worker. Native final runs started from a clean build with ccache disabled after an earlier build overlapped an edit; intermediate passes are not used as final evidence. All [62 source hashes](2026-11-10-audit-transform-data/source-sha256.json) still match the tested snapshot. The generated R package's critical copied headers were also compared with their workspace sources.

| Final command | Machine | Result |
|---|---|---|
| `just test` | Workstation | FAIL 0, WARN 0, SKIP 154, PASS 22851 |
| `just cpp-test` | Workstation | 106/106 |
| `UMVL_TEST_TIER=full just cpp-test` | Workstation | 129/129 |
| `just cpp-test-no-long-double` | Workstation | 129/129 |
| `just test-full` | Workstation | FAIL 0, WARN 0, SKIP 0, PASS 45686 |
| `just js-build` | Workstation | Exit 0 |
| `just js-test` | Workstation | 586/586, no skips |
| `just site-check` | Workstation | Exit 0; 99 family pages and 99 C++ snippets, browser checks green |

The [validation manifest](2026-11-10-audit-transform-data/validation.json) links compressed final logs. The [combined log](2026-11-10-audit-transform-data/validation-frozen.log.gz) records each command's zero exit status; the [postflight log](2026-11-10-audit-transform-data/postflight-final.log.gz) retains the final independent checks. Site-check regenerated an unrelated pre-existing screenshot, which was restored byte-for-byte to its baseline version. No test or build process remains running from this job.

**Open — GCC 13 machine check.** Four `simbox-run` attempts exited 75 because simbox was unreachable, including a final-source attempt. The missing evidence is a successful `just cpp-test-no-long-double` run on simbox/GCC 13 once it is available. The workstation/GCC 15 run above is green. The [final connection log](2026-11-10-audit-transform-data/simbox-no-long-double-frozen.log.gz) and preceding attempt logs are retained; no simbox test pass is claimed.

**Open — protected density evaluation.** The [density seam table](2026-11-10-audit-transform-data/density-seams.csv) identifies 24 successful referenced fits whose unchanged density-kernel sum still disagrees with the corrected fitted likelihood at the audit threshold: invgamma 6, lgamma 5, naka 5, lnorm 3, logitnorm 2, llogis 2 and invweibull 1. Invgamma case 3759, for example, has fitted log-likelihood about `126.415564` while its density sum is `-4563402752`. Gamma-derived evaluators need stable normalizers and centered raw residuals; logarithmic location-scale evaluators need raw relative log/logit residuals. The stats-backed lognormal path also needs a separate evaluation decision: for the held-log(2) example, `stats::dlnorm` gives `110.07211386594349`, a difference of 1.5, as recorded in the [held-normal check](2026-11-10-audit-transform-data/held-normal-density.csv). R fully held models use their density-evaluation shortcut, so this limitation also applies there. These kernels, `special/`, truth tables and generated R density functions were left untouched as instructed. The repaired fitter likelihoods use the stable raw route.

**Open — pre-existing Markdown inventory.** The additional repository-wide `check_links.py` run reports 41 diagnostics in five files that are byte-identical to HEAD: an older boundary-literature report, two frozen audit inventories, the nbinom dossier and an inference note. These include missing historical targets and function/math notation parsed as Markdown links. They were left unchanged. All links in this job's ten Markdown files resolve; the [scoped check](2026-11-10-audit-transform-data/task-links-final.log.gz) and [global diagnostics](2026-11-10-audit-transform-data/links-final.log.gz) retain the distinction.


To replay the retained checks from this worktree, with its existing dependencies:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
evidence=dev/research/evaluations/2026-11-10-audit-transform-data
bash "$evidence/build-harness.sh"  # reserves its own single slot
/tmp/umvl-cpu/run python3 "$evidence/replay.py" after
/tmp/umvl-cpu/run python3 "$evidence/compare.py"
/tmp/umvl-cpu/run bash "$evidence/build-restoration.sh"
/tmp/umvl-cpu/run bash "$evidence/build-density.sh"
/tmp/umvl-cpu/run python3 "$evidence/density-check.py"
/tmp/umvl-cpu/run4 bash "$evidence/validate-local.sh"
```

The frozen before replay, references and transition evidence remain in the data directory. [Evidence hashes](2026-11-10-audit-transform-data/evidence-sha256.json) identify the retained result files. Work stayed in this worktree; there were no commits, Git writes, board edits, sub-agents, installs, pushes or publication. Protected project-status, roadmap and NEWS files were not edited.
