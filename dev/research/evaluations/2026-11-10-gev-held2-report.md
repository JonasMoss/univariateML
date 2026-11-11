# TASK-174: GEV and Fréchet held parameters and uncertified fallback

The held-scale Fréchet target is closed: all five formerly uncertified `n=500` cases now certify at `iterlim=100`, and all 40 Fréchet fits in the unchanged held study certify. Across both families, certification rises from **103/160 to 138/160 at the default budget**, with **zero stops**. At `iterlim=1000`, **148/160** certify; the remaining 12 GEV fits return successful numerical searches marked uncertified.

All-free certificate incompleteness now preserves numerically accepted results with the per-fit flag cleared. Ten focused cases that previously stopped numerically now return uncertified fits. Proved Gumbel limits, analytic nonexistence/collapse and ties, strict counterexamples, and failed searches retain their stops. Both dossiers retain `fit.certified: partial`.

Worktree: `codex/gev-held2`, baseline `af295963493ce0b73f678d1316e5c7355914aa89`. Changes are uncommitted. The [validation record](2026-11-10-gev-held2-data/validation.json) records source and fixture hashes, machines, checks, and generated-package agreement.

## Certificate changes and evidence

The [proof note](2026-11-10-gev-held2-data/proof.md) gives the arguments and floating-point margins. Certification follows the package standard: exact-arithmetic coverage conditional on computed comparisons, with practical floating-point padding. There is no claim of outward-rounded interval arithmetic. Numerical studies and regression/oracle checks supply operational evidence separately from those arguments.

- **Fréchet held scale:** the outer cover subdivides shape, enclosing the unique location root on each slab with uniform score signs and interval Newton. Quarter-point signs permit contraction when a midpoint straddles the root and the initial nuisance curvature changes sign. Envelope-theorem shape scores and value/Taylor bounds eliminate slabs without assuming profile unimodality. The existing infinite-shape exclusion and the `lambda=1` boundary of the `lambda>=1` fit are retained.
- **GEV held scale:** nonnegative shape uses the same profile cover with the proved location-root region and a tighter support-compatible upper bound. A separate value bound handles negative-shape boxes touching the Gumbel seam: for a minimum observation below location, `u=-L >= q/(1+a*q)`, where `q=(mu-min(x))/sigma` and `a=-xi`. For `u>=4`, the average likelihood is at most `-log(sigma)-u*u/(4*n)`. This excludes remote location boxes even when the jets overflow. In the [before/after probe](2026-11-10-gev-held2-data/tail-bound-before-after.csv), the old upper bound was approximately zero; the new bound is `-247.808`, while the scalar-density mean remains finite at approximately `-5.27e305`. Eight formerly stalled held-scale cases now complete in 1,988–6,160 evaluations at the default budget.
- **Active `xi=-1` support faces:** one-sided value bounds replace jets through a zero residual. The held-scale isolator covers every feasible location on its shape strip. The held-location construction covers its feasible corner rectangle when `A=2*n-sum((M-x)/(M-mu))>0`. Certified face coefficients are restored from the original sample maximum and hold. This closes the previously selected location-held samples 10, 20, 60, and 80; the C++, R, and JS regressions include their exact data.
- **Location held, `2j<=n`:** `j` counts observations differing from the hold. New small-scale cutoffs cover data straddling the hold and data with observations below it but none above. Together with the earlier minimum-tie classifiers, they close the compactness gap in exact arithmetic. They do not establish general inner scale-root uniqueness.

For held scale, location is strictly concave for `-1<xi<=0`, so its inner root is unique. Uniform, support-aware enclosures near `xi=-1` remain difficult. The broader [exploratory profile runs](2026-11-10-gev-held2-data/exploratory-profile-100.csv) also profiled additional held-location regions and lost certifications within the allowance. Production retains the two-coordinate cover in those regions. For held location below every observation and nonnegative shape, the proof note establishes a unique inner scale root; a general mixed-sign result remains open.

The implementation is in [GEV held certificates](../../../univariateML-cpp/include/cppunivariate/distributions/gev/held_certificate.hpp) and [Fréchet certificates](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/certificate.hpp). The [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md) and [Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) describe the resulting coverage.

## All-free fallback

The [joint-certificate wrapper](../../../univariateML-cpp/include/cppunivariate/distributions/gev/joint_certificate.hpp) first attempts the existing certificate. On incompleteness it runs the existing scalar profile search with its convergence check, successful inner fits, finite likelihood, and the R6 total shape-score gate. When a value-only scan selects the open Fréchet seam, existing finite Newton proposals can be retained after fresh inner solves, score checks, and a negative numerical profile-curvature check. These establish numerical acceptance only. A zero-shape seam is never converted into a capped finite Fréchet fit.

A strictly better certificate witness refutes the search. That ordering is checked again with feasible public coordinates and fresh likelihoods; loss of strict ordering within the margin preserves the successful search uncertified. Public support and representation checks remain. Nonconverged scalar Gumbel solves and genuinely failed searches still stop.

The two-point reductions also preserve normal, finite analytic members when coefficient inclusion is defeated by location cancellation or a comparison remains unordered at arithmetic stagnation. Subnormal, unrepresentable, and excessively tight-tolerance precision stops remain.

C++ exposes `result.certified`; R attaches logical `certified`; the common/JS binding exposes zero or one for these all-free and held-nuisance routes. Completed all-free GEV certificates now populate the field, which was previously left unset.

Exactly three existing ordinary-stop assertions changed, all for `c(-9,rep(1,9))`:

1. [GEV C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/gev/test.cpp): `NumericalFailure` becomes `Ok` with `certified=false`.
2. [GEV R tests](../../../bindings/r/distributions/gev/test.R): expected `univariateML_numerical` becomes coefficients `(0,1,-1)` with `certified=FALSE`.
3. [Main JS tests](../../../bindings/js/test.mjs): the numerical throw becomes the same estimates with `certified=0`.

All other C++ additions are appended. Existing tests were retained, including search-failure and proved-stop cases.

## Unchanged held study: before and after

The [study source](2026-11-10-gev-held2-data/study.cpp) preserves seed `17020261110`, sampling order and grid: GEV generating shapes `-0.9,-0.5,0,0.5,1,2`; sample sizes `3,10,50,100,500`; two replicates; holds `mu=0` or `sigma=1`. The Fréchet generating blocks are `lambda=2,2,1,0.5`, with independently drawn samples in the two `lambda=2` blocks and scale held at one. There are 60 GEV datasets fitted twice and 40 Fréchet datasets fitted once. C++ tolerance is `1e-10`; only `iterlim` changes between the two after runs. Existing admissible parameter ranges and controls are unchanged.

Entries below are **certified / uncertified / stops**.

| Subset (fits) | Before, 100 | After, 100 | After, 1000 |
| --- | --- | --- | --- |
| GEV, location held (60) | 33 / 27 / 0 | 44 / 16 / 0 | 52 / 8 / 0 |
| GEV, scale held (60) | 35 / 25 / 0 | 54 / 6 / 0 | 56 / 4 / 0 |
| Fréchet, scale held (40) | 35 / 5 / 0 | 40 / 0 / 0 | 40 / 0 / 0 |
| Total (160) | 103 / 57 / 0 | 138 / 22 / 0 | 148 / 12 / 0 |

No baseline certification was lost, and none was lost on raising the budget. The five F1 target IDs are 37, 39, 57, 59, and 77. Raising `iterlim` closes ten further GEV fits: location held at IDs **6, 7, 72, 74, 76, 86, 90, 94**, and scale held at **6, 7**.

| Subset | Run / iterlim | Median ms | p95 ms | Maximum ms |
| --- | --- | --- | --- | --- |
| GEV, location held | Before, 100 | 395.83 | 8,138.14 | 8,800.97 |
| GEV, location held | After, 100 | 416.71 | 6,741.06 | 8,573.35 |
| GEV, location held | After, 1000 | 446.15 | 80,073.05 | 91,246.57 |
| GEV, scale held | Before, 100 | 270.72 | 11,089.01 | 11,591.95 |
| GEV, scale held | After, 100 | 156.68 | 3,403.98 | 6,169.37 |
| GEV, scale held | After, 1000 | 181.78 | 4,736.43 | 50,503.86 |
| Fréchet, scale held | Before, 100 | 10.40 | 1,391.05 | 1,995.82 |
| Fréchet, scale held | After, 100 | 6.67 | 213.38 | 219.20 |
| Fréchet, scale held | After, 1000 | 7.72 | 239.42 | 313.71 |

These are single-pass fit-call timings on the shared workstation. They exclude compilation, slot waits and auxiliary diagnostic reconstruction. The p95 is the upper order statistic used by the [summary script](2026-11-10-gev-held2-data/summarize.py); timings are descriptive. Higher allowance closes ten fits but raises the recorded maximum held-fit time from 8.57 s to 91.25 s.

Primary records: [before](2026-11-10-gev-held2-data/held-before.csv), [after 100](2026-11-10-gev-held2-data/study-local-100.csv), [after 1000](2026-11-10-gev-held2-data/study-local-1000.csv), and [unrounded summary](2026-11-10-gev-held2-data/summary.json).

## All-free grid and focused fallback cases

The [all-free grid](2026-11-10-gev-held2-data/allfree.cpp) uses the same generated datasets with no holds. Before-run GEV certification counts follow the old successful certificate path: the old per-fit boolean was unpopulated, and the old algorithm returned success only after certificate completion. That historical raw field is preserved in the CSV.

| Family (fits) | Before, 100 | After, 100 | After, 1000 |
| --- | --- | --- | --- |
| GEV (60) | 60 / 0 / 0 | 60 / 0 / 0 | 60 / 0 / 0 |
| Fréchet (40) | 38 / 0 / 2 | 38 / 0 / 2 | 38 / 0 / 2 |
| Total (100) | 98 / 0 / 2 | 98 / 0 / 2 | 98 / 0 / 2 |

The two stops are Fréchet IDs 23 and 43, both proved Gumbel limits, at both budgets.

| Family | Run / iterlim | Median ms | p95 ms | Maximum ms |
| --- | --- | --- | --- | --- |
| GEV | Before, 100 | 13.74 | 316.38 | 376.29 |
| GEV | After, 100 | 8.19 | 222.65 | 261.68 |
| GEV | After, 1000 | 9.03 | 485.98 | 813.85 |
| Fréchet | Before, 100 | 10.30 | 149.41 | 169.58 |
| Fréchet | After, 100 | 7.66 | 162.59 | 172.11 |
| Fréchet | After, 1000 | 5.49 | 85.07 | 114.77 |

The regular grid already certified its finite successes, so focused cases exercise the new fallback:

| Cases | Before, 100 | After, 100 |
| --- | --- | --- |
| Two-point cancellation, both families at units `1e-50,1,1e50` (6) | 0 / 0 / 6 | 0 / 6 / 0 |
| Four finite near-Gumbel Fréchet proposals (4) | 0 / 0 / 4 | 0 / 4 / 0 |

The six cancellation cases retain the same numerical coefficients and likelihoods. The four near-Gumbel cases return finite shapes approximately `1.44e6`–`1.44e9`, with fresh numerical score checks, and remain uncertified at `iterlim=1000`. Their median fit time is 18.14 ms before versus 32.59 ms after at 100; after at 1000 it is 27.99 ms. The cancellation fixtures were checked at the default budget.

Across the regular grid and these ten focused checks, the default outcome changes from **98 certified / 0 uncertified / 12 stops** to **98 / 10 / 2**, preserving the two proved limits.

Records and reproduction: [all-free before](2026-11-10-gev-held2-data/allfree-before.csv), [after 100](2026-11-10-gev-held2-data/allfree-local-100.csv), [after 1000](2026-11-10-gev-held2-data/allfree-local-1000.csv), [cancellation before](2026-11-10-gev-held2-data/edges-before.csv), [cancellation after](2026-11-10-gev-held2-data/edges-after.csv), [near-seam before](2026-11-10-gev-held2-data/seam-before.csv), [near-seam after](2026-11-10-gev-held2-data/seam-after-rescue.csv), [near-seam after 1000](2026-11-10-gev-held2-data/seam-after-1000.csv).

## Open GEV proof cases

The [final instrumented trace](2026-11-10-gev-held2-data/phase-final-1000.csv) matches exactly the 12 public uncertified fits at 1000. It also records the eight remote-location cases that now complete.

| Remaining class | Fits | What is missing |
| --- | --- | --- |
| Local inclusion | location held: 4; scale held: 4, 8 | A valid local root/coefficient inclusion around the numerical proposals. These return after 3, 3, and 14 evaluations at both budgets; more allowance alone does not close them. |
| Active-face transition | scale held: 5 | The face isolator succeeds, but a machine-width box near `xi=-0.999999999999882` remains outside its excluded region. Its value bound does not separate from the face within the practical margin. A sharper overlapping face bound or stable enclosure is needed; both budgets stagnate after 714 evaluations. |
| Outer coverage | location held: 8, 9, 78, 88, 92, 96, 98; scale held: 9 | Local inclusion succeeds, but the two-coordinate cover exhausts 128,000 evaluations. Tighter global bounds or efficient support-aware profile enclosures are still needed. |

Beyond this grid, general mixed-sign inner scale-root uniqueness is unproved, and the held-location one-sided isolator has no general `A=0` extension. Rounded normalization can also obstruct inclusion. These are certificate gaps; the successful numerical searches remain available with the flag cleared. Both catalogue entries therefore remain partial.

## Final validation and scope

Every requested build/test run finished green on the workstation, using reserved slots, one BLAS/OpenMP thread per worker, and nice priority. After the shared helper gained two concurrent four-slot groups, this job's validation trees were pinned to the same four CPUs. Final software is GCC 15.2.0, R 4.6.1, and Node 22.22.1.

| Check | Final result | Machine |
| --- | --- | --- |
| `just test` (fresh dev run) | 23,030 passes; 0 failures, 0 warnings; 154 tier skips | Workstation |
| `just test-full` | 45,845 passes; 0 failures, 0 warnings, 0 skips | Workstation |
| `UMVL_TEST_TIER=dev just cpp-test` | 106/106 shards | Workstation |
| `UMVL_TEST_TIER=full just cpp-test` | 130/130 shards | Workstation |
| `UMVL_TEST_TIER=full just cpp-test-no-long-double` | 130/130 shards | Workstation |
| Standalone GEV shard, native and double-width | 59 cases and 33,736 assertions in each mode | Workstation |
| `just js-build` | Pass | Workstation |
| `just js-test` | 599/599 tests; 0 failures or skips | Workstation |
| `node --v8-pool-size=1 --test bindings/js/fit-test.mjs` | 307/307 tests; 0 failures or skips | Workstation |
| `just site-check` | 99 snippets compile/link as C++17; HTML and browser checks pass | Workstation |
| `just quality gev frechet` | Both families pass | Workstation |

Long R/C++ stages attempted `/tmp/umvl-cpu/simbox-run`; the final attempts returned exit 75 after filesystem/unreachability failures and used the prescribed local fallback. **GCC 13 on simbox remains unverified.** The final workstation no-long-double run includes the final fitting code and strengthened regression; the earlier preliminary run is not used as final coverage.

The [validation audit](2026-11-10-gev-held2-data/validation.json) verifies all 15 changed tracked files against the frozen source hashes, original baseline headers, frozen goldens/d/p/q/r/truth files, C++ append-only tests apart from the authorized fallback assertion, generated R-package header copies, study sizes, and the remaining-case trace. Controls, code generation, settled numerical evaluation files, NEWS, STATUS, KNOWN_ISSUES and roadmap files are unchanged. The site check regenerated one unrelated older screenshot; it was restored from HEAD.

Documentation check: this job's links pass. The repository-wide scanner reports **44 unchanged findings in files identical to HEAD**, including unavailable private-PDF/site-route targets and C++/math text interpreted as links. They remain outside this job; the [link audit](2026-11-10-gev-held2-data/link-validation.json) records every finding and the unchanged-file hashes.

Reproduction uses [reproduce-local.sh](2026-11-10-gev-held2-data/reproduce-local.sh), [check-edges.sh](2026-11-10-gev-held2-data/check-edges.sh), [check-seam.sh](2026-11-10-gev-held2-data/check-seam.sh), [check-phases.sh](2026-11-10-gev-held2-data/check-phases.sh), and [check-tail-bound.sh](2026-11-10-gev-held2-data/check-tail-bound.sh). The exact five original fitter headers are retained in [before-headers.zip](2026-11-10-gev-held2-data/before-headers.zip). Files named `exploratory-*`, `*-before-tail-*` or `phase-before-corners.*` are intermediate evidence; the primary after results are `study-local-*`, `allfree-local-*`, `summary.json` and the final trace.
