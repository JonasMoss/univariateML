# TASK-174: GEV and Fréchet held parameters and uncertified fallback

The held-scale Fréchet target is closed: all five formerly uncertified `n=500` cases now certify at `iterlim=100`, and all 40 Fréchet fits in the unchanged held study certify. Across both families, certification rises from **103/160 to 141/160 at the default budget**, with **zero stops**. At `iterlim=1000`, **148/160** certify; the remaining 12 GEV fits return successful numerical searches marked uncertified.

All-free certificate incompleteness now preserves numerically accepted results with the per-fit flag cleared. Ten focused cases that previously stopped numerically now return uncertified fits. Proved Gumbel limits, analytic nonexistence/collapse and ties, strict counterexamples, and failed searches retain their stops. Both dossiers retain `fit.certified: partial`.

Worktree: `codex/gev-held2`. The original implementation was committed as `53a8e256`; this integration follows master merge `1b5b6cce`. Integration changes are uncommitted. Original before measurements use `af295963`. The [combination audit](2026-11-10-gev-held2-data/combination-audit.json), [source manifest](2026-11-10-gev-held2-data/combination-sources.json) and [run ledger](2026-11-10-gev-held2-data/combination-validation.csv) record the final evidence.

## Certificate changes and evidence

The [proof note](2026-11-10-gev-held2-data/proof.md) gives the arguments and floating-point margins. Certification follows the package standard: exact-arithmetic coverage conditional on computed comparisons, with practical floating-point padding. There is no claim of outward-rounded interval arithmetic. Numerical studies and regression/oracle checks supply operational evidence separately from those arguments.

- **Fréchet held scale:** the outer cover subdivides shape, enclosing the unique location root on each slab with uniform score signs and interval Newton. Quarter-point signs permit contraction when a midpoint straddles the root and the initial nuisance curvature changes sign. Envelope-theorem shape scores and value/Taylor bounds eliminate slabs without assuming profile unimodality. The existing infinite-shape exclusion and the `lambda=1` boundary of the `lambda>=1` fit are retained.
- **GEV held scale:** nonnegative shape uses the same profile cover with the proved location-root region and a tighter support-compatible upper bound. A separate value bound handles negative-shape boxes touching the Gumbel seam: for a minimum observation below location, `u=-L >= q/(1+a*q)`, where `q=(mu-min(x))/sigma` and `a=-xi`. For `u>=4`, the average likelihood is at most `-log(sigma)-u*u/(4*n)`. This excludes remote location boxes even when the jets overflow. In the [before/after probe](2026-11-10-gev-held2-data/tail-bound-before-after.csv), the old upper bound was approximately zero; the new bound is `-247.808`, while the scalar-density mean remains finite at approximately `-5.27e305`. Eight formerly stalled held-scale cases complete at the default budget.
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

| Subset (fits) | Original before, 100 | Prior branch, 100 | Combined, 100 | Combined, 1000 |
| --- | --- | --- | --- | --- |
| GEV, location held (60) | 33 / 27 / 0 | 44 / 16 / 0 | 46 / 14 / 0 | 52 / 8 / 0 |
| GEV, scale held (60) | 35 / 25 / 0 | 54 / 6 / 0 | 55 / 5 / 0 | 56 / 4 / 0 |
| Fréchet, scale held (40) | 35 / 5 / 0 | 40 / 0 / 0 | 40 / 0 / 0 | 40 / 0 / 0 |
| Total (160) | 103 / 57 / 0 | 138 / 22 / 0 | 141 / 19 / 0 | 148 / 12 / 0 |

No previous certificate is lost and no status changes at either budget. The five F1 target IDs remain certified: 37, 39, 57, 59 and 77. Relative to the prior branch, the combined default cover additionally certifies location-held IDs **7, 74** and scale-held ID **7**. Raising the combined budget closes seven further fits: location-held **6, 72, 76, 86, 90, 94** and scale-held **6**. Neither budget closes any of the requested eight outer-coverage cases.

| Subset | Run / iterlim | Median ms | p95 ms | Maximum ms |
| --- | --- | --- | --- | --- |
| GEV, location held | Original before, 100 | 395.83 | 7,072.87 | 8,800.97 |
| GEV, location held | Prior branch, 100 | 416.71 | 5,435.94 | 8,573.35 |
| GEV, location held | Combined, 100 | 394.98 | 7,714.38 | 8,994.66 |
| GEV, location held | Combined, 1000 | 201.75 | 48,795.69 | 70,139.32 |
| GEV, scale held | Original before, 100 | 270.72 | 8,905.48 | 11,591.95 |
| GEV, scale held | Prior branch, 100 | 156.68 | 2,749.52 | 6,169.37 |
| GEV, scale held | Combined, 100 | 183.56 | 3,348.17 | 5,536.93 |
| GEV, scale held | Combined, 1000 | 99.23 | 1,864.47 | 48,889.00 |
| Fréchet, scale held | Original before, 100 | 10.40 | 1,289.40 | 1,995.82 |
| Fréchet, scale held | Prior branch, 100 | 6.67 | 213.24 | 219.20 |
| Fréchet, scale held | Combined, 100 | 8.18 | 201.35 | 209.49 |
| Fréchet, scale held | Combined, 1000 | 4.01 | 99.78 | 205.11 |

These are single-pass wall-clock fit timings on the shared workstation, excluding compilation, slot waits and diagnostic reconstruction. The p95 uses linear interpolation between adjacent order statistics. CPU contention and frequency scaling prevent a controlled speedup claim. [Raw combined results at 100](2026-11-10-gev-held2-data/combination-policy-100.csv), [at 1000](2026-11-10-gev-held2-data/combination-policy-1000.csv), [prior branch](2026-11-10-gev-held2-data/study-local-100.csv), [original before](2026-11-10-gev-held2-data/held-before.csv), and [summary script/results](2026-11-10-gev-held2-data/combination-summary.json) retain the measurements. The [study driver](2026-11-10-gev-held2-data/combination-study.cpp) preserves the original seed, sample generation and grid; auxiliary diagnostic refits are disabled for timing.

## Combination with TASK-172

The piecewise negative-Hessian enlargement is valid for a smooth included root: the four rings checked by `holds` cover the enlarged rectangle outside the existing basin. An attained smooth shape edge instead requires the uniform outward shape score and negative nuisance curvature. All smooth pieces must lie within support. The merged edge flags and radii now have initialized outer scope; **a one-sided support-face basin never enters smooth enlargement**. Its exclusion still rests solely on its one-sided value proof. Profile slabs retain the whole nuisance range initially, including slabs over the basin's shape interval; they are discarded as basin-contained only after enclosing every possible inner root. This prevents an unchecked exclusion at a support corner or in profile mode. The [proof addendum](2026-11-10-gev-held2-data/proof.md) spells out these conditions.

Fréchet uses enlargement for interior roots and its original outward-score basin at the shape-one edge. All three policies certify 40/40, but their evaluation counts differ:

| Policy | 13 shape-one fits | 27 interior fits | Total | Median total CPU ms, three repeats |
| --- | --- | --- | --- | --- |
| Selected: interior enlargement | 3,544 | 31,326 | 34,870 | 887.92 |
| Enlargement everywhere | 9,781 | 31,326 | 41,107 | 1,301.37 |
| No enlargement | 3,544 | 37,593 | 41,137 | 964.21 |

Thus edge enlargement is disabled because it spends evaluations without improving coverage. Interior enlargement reduces cover work. [Instrumented policy comparison](2026-11-10-gev-held2-data/combination-frechet-modes.sh) uses scratch header copies and preserves production behavior apart from counters. GEV retains smooth enlargement in both box and profile modes; the three new default certificates are its measured coverage benefit.

## Fréchet sample-size growth

The [size driver](2026-11-10-gev-held2-data/combination-large.cpp) uses the same deterministic quantiles as TASK-172's `large.cpp`, extended to n=5000, with scale held at one. Every cell below certifies at **iterlim=100**. Entries are certificate evaluations / CPU ms.

| n | Generating shape 1.25 | Shape 2 | Shape 4 |
| --- | --- | --- | --- |
| 500 | 1,212 / 205.4 | 1,292 / 216.8 | 1,353 / 226.2 |
| 1,000 | 1,215 / 337.9 | 1,329 / 427.4 | 1,317 / 209.7 |
| 2,000 | 1,238 / 389.7 | 1,369 / 429.8 | 1,337 / 421.0 |
| 5,000 | 1,225 / 975.1 | 1,384 / 1089.1 | 1,404 / 1098.6 |

Evaluation counts remain approximately constant (1,212–1,404); per-evaluation observation work grows with n. Timing is broadly linear with sample size, subject to the shared workstation's varying CPU frequency. TASK-172's box-cover measurements needed iterlim 200 at n=1000 and 400 at n=2000; the combined profile cover needs no raised budget through n=5000 on these samples. This is measured behavior, not a uniform complexity theorem. [Raw scaling](2026-11-10-gev-held2-data/combination-scaling-growth.csv) and [reproduction](2026-11-10-gev-held2-data/combination-scaling.sh) are retained. Both dossiers describe this final policy and remain `fit.certified: partial`.

## Local accuracy of uncertified fits

Global certificate completion and local numerical accuracy are separate requirements. Held and all-free successful fallbacks now undergo analytic-jet Newton refinement using `core/likelihood_polish.hpp`: positive definite observed information, the full undamped correction `H^-1 g`, and two small corrections or quadratic contraction to the rounding floor. Coordinates scale the coefficient error to the requested `tol * max(abs(theta), s)`; acceptance uses a factor-16 margin. Attained shape edges refine free nuisance coordinates and check the outward shape score. Analytically coupled active support corners retain their exact conditional formulas; an inactive reverse-exponential edge uses smooth refinement. Failed local accuracy stops numerically even when the global cover is merely incomplete.

Held-scale Fréchet uses log shape and log location gap. Its log ratio uses `u + log1p(d/exp(u))`, or `log(d)+log1p(exp(u)/d)` when d is larger; the quotient is bounded by one. This avoids both lost small gaps and overflow. All-free Fréchet near the Gumbel seam refines the original observations with a compensated small-shape score and stable fitter likelihood, preserving relative accuracy of the large transformed coefficients. Public-unit projection checks reject rounding beyond tolerance, including subnormal scales for which an absolute error bound can underflow. The [projection probe](2026-11-10-gev-held2-data/combination-projection-after.csv) records genuine representation failures rather than uncertified successes.

Independent roots use mpmath at **70-digit working precision**, exact rational conversion of each binary64 observation, and direct score formulas independent of the production jets and covers. Error below is coefficient error divided by its contract scale.

| Check | Fit-instances | Maximum coefficient error |
| --- | --- | --- |
| All uncertified held results, both budgets | 31 | 2.254e-14 |
| Three-coordinate GEV local refinement checks | 4 | 1.002e-13 |
| Actual all-free Fréchet seam fits, native | 4 | 2.433e-16 |
| Actual all-free Fréchet seam fits, double-only | 4 | 1.204e-16 |
| Actual held-scale Fréchet large-shape fits, native | 4 | 5.6e-13 |
| Actual held-scale Fréchet large-shape fits, double-only | 4 | 5.6e-13 |

The 31 held checks cover every uncertified fit: 19 at 100 and 12 at 1000. The GEV three-coordinate checks perturb successful public solutions and test the common fallback refinement engine; they are local component checks, not extra global certificates. The actual all-free Fréchet cases remain uncertified at finite shapes approximately 1.44e6–1.44e9. Four additional private held-scale large-shape refinements per arithmetic mode have maximum error 1.608e-15. A double-only large-shape regression caught a former 1.968e-9 coefficient error; the stable log-ratio resolves it. These comparisons support local accuracy numerically and make no claim of global uniqueness. [Independent root code](2026-11-10-gev-held2-data/combination-reference.py) and its JSONL records are retained; the [audit](2026-11-10-gev-held2-data/combination-audit.json) checks coverage of every uncertified study instance. Nine C++ cases and one frozen-input JavaScript regression were appended during integration; existing tests were preserved.

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
| GEV | Original before, 100 | 13.74 | 301.40 | 376.29 |
| GEV | Combined, 100 | 8.26 | 236.95 | 319.16 |
| GEV | Combined, 1000 | 8.52 | 323.50 | 528.79 |
| Fréchet | Original before, 100 | 10.30 | 138.11 | 169.58 |
| Fréchet | Combined, 100 | 6.16 | 84.25 | 93.86 |
| Fréchet | Combined, 1000 | 6.10 | 88.53 | 99.54 |

Final grid records: [100](2026-11-10-gev-held2-data/combination-allfree-100.csv) and [1000](2026-11-10-gev-held2-data/combination-allfree-1000.csv).

The regular grid already certified its finite successes, so focused cases exercise the new fallback:

| Cases | Before, 100 | After, 100 |
| --- | --- | --- |
| Two-point cancellation, both families at units `1e-50,1,1e50` (6) | 0 / 0 / 6 | 0 / 6 / 0 |
| Four finite near-Gumbel Fréchet proposals (4) | 0 / 0 / 4 | 0 / 4 / 0 |

The six cancellation cases retain the same analytic members, with local precision and public-coordinate checks. The four near-Gumbel cases return finite shapes approximately `1.44e6`–`1.44e9`, with fresh numerical score checks, and remain uncertified at `iterlim=1000`. The final native and double-only seam probes at both budgets retain all four successful uncertified fits; [native at 100](2026-11-10-gev-held2-data/combination-seam-native-100.csv) and [1000](2026-11-10-gev-held2-data/combination-seam-native-1000.csv) record final timings. The six cancellation fixtures were rerun at both budgets in [the combined probe](2026-11-10-gev-held2-data/combination-edges-100.csv).

Across the regular grid and these ten focused checks, the default outcome changes from **98 certified / 0 uncertified / 12 stops** to **98 / 10 / 2**, preserving the two proved limits.

Historical original-job records: [all-free before](2026-11-10-gev-held2-data/allfree-before.csv), [after 100](2026-11-10-gev-held2-data/allfree-local-100.csv), [after 1000](2026-11-10-gev-held2-data/allfree-local-1000.csv), [cancellation before](2026-11-10-gev-held2-data/edges-before.csv), [cancellation after](2026-11-10-gev-held2-data/edges-after.csv), [near-seam before](2026-11-10-gev-held2-data/seam-before.csv), [near-seam after](2026-11-10-gev-held2-data/seam-after-rescue.csv), [near-seam after 1000](2026-11-10-gev-held2-data/seam-after-1000.csv).

## Open GEV proof cases

The [combined instrumented trace](2026-11-10-gev-held2-data/combination-phase-1000.csv) matches exactly the 12 public uncertified fits at 1000. It also records the eight remote-location cases that now complete.

| Remaining class | Fits | What is missing |
| --- | --- | --- |
| Local inclusion | location held: 4; scale held: 4, 8 | A valid local root/coefficient inclusion around the numerical proposals. These return after 3, 3, and 12 evaluations at both budgets; more allowance alone does not close them. |
| Active-face transition | scale held: 5 | The face isolator succeeds, but a machine-width box near `xi=-0.999999999999882` remains outside its excluded region. Its value bound does not separate from the face within the practical margin. A sharper overlapping face bound or stable enclosure is needed; both budgets stagnate after 714 evaluations. |
| Outer coverage | location held: 8, 9, 78, 88, 92, 96, 98; scale held: 9 | Local inclusion succeeds, but the two-coordinate cover exhausts 128,000 evaluations. Tighter global bounds or efficient support-aware profile enclosures are still needed. |

Beyond this grid, general mixed-sign inner scale-root uniqueness is unproved, and the held-location one-sided isolator has no general `A=0` extension. Rounded normalization can also obstruct inclusion. These are certificate gaps; the successful numerical searches remain available with the flag cleared. Both catalogue entries therefore remain partial.

## Open density-evaluation follow-up

**D1 (outside authorized files):** `frechet/dpqr.hpp` still subtracts nearly equal logarithms near the Gumbel limit. On the four finite seam fits, its summed log density differs from a 70-digit reference by as much as **1.04725e-5** at shape approximately 1.44e9; the stable fitter likelihood error there is **5.5e-14**, and at most **1.447e-13** over the four cases. [Raw density comparison](2026-11-10-gev-held2-data/combination-density-check.csv) separates these quantities. A follow-up must apply a stable central log-ratio to the public density, validate its truth cases, and regenerate the bindings. The task explicitly protects d/p/q/r, special functions, truth files and code generation, so those sources were not changed. Distribution/CDF/quantile/random accuracy was not independently audited here.

## Final validation and scope

All required final runs completed green **locally on the workstation**, through the shared slot wrappers with one BLAS/OpenMP thread per worker and nice priority. Simbox is unavailable with a read-only filesystem; this integration made no remote attempt. The temporary local allocation was ten cores. The run ledger records exits; final logs use the `combination-*` names below.

| Check | Final result | Machine |
| --- | --- | --- |
| `just cpp-test` (dev) | 106/106 shards | Workstation |
| `UMVL_TEST_TIER=full just cpp-test` | 133/133 shards | Workstation |
| `just cpp-test-no-long-double` | 133/133 shards | Workstation |
| `just test` | 23,147 passes; 0 failures, 0 warnings; 154 tier skips | Workstation |
| `just test-full` | 45,962 passes; 0 failures, 0 warnings, 0 skips | Workstation |
| `just js-build` | Pass | Workstation |
| `just js-test` | 612/612 tests; 0 failures or skips | Workstation |
| `just site-check` | 99 C++17 snippets plus HTML/browser checks pass | Workstation |
| `just quality gev frechet` | Both families pass | Workstation |

The [final source/fixture audit](2026-11-10-gev-held2-data/combination-audit.json) checks frozen source hashes, unchanged settled files, append-only C++/JS tests, generated R headers including the new accuracy header, unchanged study outcomes and complete independent-root coverage. Controls and default admissible parameter ranges are unchanged. No new golden, d/p/q/r, special-function, truth, code-generation, NEWS, STATUS, KNOWN_ISSUES or roadmap changes were made. The site-generated unrelated screenshot was restored from HEAD. No commits or board edits were made during integration.

This job's documentation links pass. The repository-wide scanner's 45 unchanged baseline findings remain outside this job; [job link validation](2026-11-10-gev-held2-data/link-validation.json) identifies the checked documents and baseline findings. Original job scripts and before-header archive remain available. `combination-*` records supersede earlier `study-local-*` after measurements for this integrated source; files named `preliminary`, `before-logratio`, `before-bounded` or `exploratory` are intermediate evidence. [Combination validation driver](2026-11-10-gev-held2-data/combination-validate.sh), [held-grid reproduction](2026-11-10-gev-held2-data/combination-held.sh), [all-free reproduction](2026-11-10-gev-held2-data/combination-allfree.sh), [scaling reproduction](2026-11-10-gev-held2-data/combination-scaling.sh) and the source manifest make the final checks reproducible.
