# Family taxonomy: first version (TASK-101.6.5)

2026-10-31. Source revision: `1d7fd22d6eb12c207002cdeb152a1292a6e4f22f` plus the uncommitted taxonomy changes. No fitters, goldens, density definitions, dossier prose or existing classifications changed. No commits, installs, board edits or other worktrees were used.

## Schema and deviations

- All 98 dossiers have group, construction, relations, fit and areas; the 88 missing classifications/reasons are filled. Existing ten are preserved. Vocabularies and expression maps are validated by the R loader without adding these fields to generated R help. Legacy minimal codegen inputs may omit the entire taxonomy block; a partial block is rejected. This preserves the existing synthetic codegen fixture.
- Fit method starts from the dated inventory; explicit profile searches are labelled profile-search. Discrete Laplace is corrected from integer-search to closed-form from its median/scale formula. The method vocabulary adds convex-solve for dnorm: its dossier documents joint damped Newton in concave natural coordinates, so the inventory multistart label would be misleading.
- Power and shift transforms have explicit argument expressions. Maps cover every parent coefficient. Abs means folding a full-real parent and sums both branches; other transforms use the absolute Jacobian. Continuous supports agree modulo null endpoints; discrete endpoint inclusion must agree exactly.
- Existence tags are additive. Named proper faces include unnamed-in-package half families, shifted laws and two-point laws. Unclassified failure records use partial instead of an unjustified always; Cauchy and slash tie-collapse records are not assumed constant-sample failures. Always means no declared obstacle, not a theorem. Degenerate-only requires explicit degenerate-sample records with no other obstacle. Conditional metadata is summarised at family level; tags do not claim the same story for every hold subset.
- Positive denotes a lower-bounded continuous shape, including translated half-lines; dependent flags coefficient bounds. Finite-counts includes coefficient-bounded supports even where a held infinite bound can remove truncation. These are catalog shapes, not assertions about every coefficient value.
- The catalog has separate facets for coefficient count/kinds and support shape/dependence, in addition to all written dimensions and declared relation kinds. Multi-selection is OR within a facet, AND across facets. Counts ignore the current facet selection and respect all others.

## Counts per tag value

- group: Pareto and Lomax **7**; Poisson, binomial and negative binomial **14**; beta and generalised beta **8**; bounded continuous **6**; discrete heavy-tailed **4**; extreme value **4**; finite sampling **3**; gamma and Weibull **11**; modified counts **14**; normal and Student **7**; other positive **7**; skewed and robust location-scale **13**.
- construction: base **47**; compounding **6**; discretisation **3**; folding **4**; generalisation **16**; location-scale **14**; modification **8**; skewing **4**; transformation **9**; truncation **7**.
- classification: classical **23**; specialist **68**; uncommon **7**.
- areas: actuarial **19**; counts and ecology **28**; economics and income **14**; extremes and environment **4**; finance **4**; measurement and errors **11**; proportions **7**; rank-frequency **4**; reliability and survival **18**; robust modelling **9**; sampling and quality control **3**; waiting times and queues **6**.
- support_tags: bounded **9**; counts **23**; dependent **31**; finite-counts **12**; positive **35**; real **16**; unit **3**.
- coefficient_tags: 1 **10**; 2 **47**; 3 **32**; 4 **8**; 5 **1**; design **13**; endpoint **6**; regular **96**; threshold **11**.
- existence_tags: always **16**; degenerate-only **23**; escapes **44**; needs-held **8**; partial **11**; restricted **8**.
- support bounds: dependent **31**; fixed **67**.
- relation kinds (families): none **66**; special_case_of **22**; transform_of **16**.
- fit.method: closed-form **13**; convex-solve **1**; delegation **16**; integer-search **3**; multistart **42**; profile-search **10**; scalar-solve **13**.
- fit.certified: no **28**; partial **6**; unknown **63**; yes **1**.

## Relations evidence

- 39 edges: verified numerically: 30; verified symbolically: 9.
- Every edge runs support/domain checks and 24 random interior log-density comparisons (seed 20261031; relative tolerance 2e-9; 35-digit evaluation), including symbolically simplified edges. Symbolic status refers to density algebra; mapped support bounds are also compared numerically. Numerical-only results are regression evidence, not identities proved for all parameters.
- Full per-edge machine output: `build/quality/relations.json`; reproducible command: `just relations`. The following edges are numerical-only:
  - betapr → gb2 (special_case_of).
  - burr → gb2 (special_case_of).
  - exp → gamma (special_case_of).
  - exp → weibull (special_case_of).
  - foldnorm → norm (transform_of).
  - frechet → invweibull (transform_of).
  - gamma → trgamma (special_case_of).
  - invburr → burr (transform_of).
  - invexp → exp (transform_of).
  - invgamma → gamma (transform_of).
  - invpareto → invburr (special_case_of).
  - invtrgamma → trgamma (transform_of).
  - invweibull → invtrgamma (special_case_of).
  - invweibull → weibull (transform_of).
  - kumar → genbeta (special_case_of).
  - lgamma → gamma (transform_of).
  - llogis → burr (special_case_of).
  - llogis → logis (transform_of).
  - lnorm → norm (transform_of).
  - logitnorm → norm (transform_of).
  - lomax → burr (special_case_of).
  - naka → gamma (transform_of).
  - paralogis → burr (special_case_of).
  - pareto2 → pareto4 (special_case_of).
  - pareto3 → pareto4 (special_case_of).
  - pareto3 → llogis (transform_of).
  - pareto4 → fpareto (special_case_of).
  - power → genbeta (special_case_of).
  - rayleigh → weibull (special_case_of).
  - weibull → trgamma (special_case_of).
- Final failed edges: none.
- Limit records are displayed, not validated by this new checker. No numerical fitter certification follows from density agreement.

## Uncertainty and gaps visible in the first version

- All new prevalence classifications are editorial, not measured popularity; specialist is the conservative default. Especially tentative uncommon assignments: bhatt, fpareto, invparalogis, invtrgamma, lgamma, nsbeta, paralogis. Most specialist reasons describe a modelling role rather than documenting adoption.
- Application areas are broad and nonexclusive starting points. Inverse/transformed gamma actuarial use overlaps reliability; bounded laws include interval measurements beyond proportions; modified normal belongs with normal rather than modified counts. EMG is grouped with skewed location-scale because its support is full real; foldnorm and hcauchy remain in other-positive. These primary groups should be revised visibly rather than treated as disjoint mathematical classes.
- Sparse edges are deliberate: no new limits, truncation/conditioning relation type, or general affine/composed transform is introduced. Half families with translated endpoints, nsbeta standardisation, and more skew/dispersion special cases remain useful additions. Inverse relations already implied by chains need not all be duplicated.
- Certification unknown is retained unless Details or the inventory states strength. No claims about DQM, Fisher information or inference are added. Existing numerical-search caveats remain; none was repaired by this metadata task.
- Three web searches checked application/construction context: [actuar manual](https://stat.ethz.ch/CRAN/web/packages/actuar/actuar.pdf) (loss families), [extraDistr manual](https://www.stat.ethz.ch/CRAN/web/packages/extraDistr/refman/extraDistr.html) (normal-uniform Bhattacharjee convolution), and [McDonald issue record](https://www.jstor.org/stable/i332767) (income modelling). Density relations themselves are checked against local coordinates, not copied from external parameterisations.

## Validation

- All required final commands ran in the foreground under `/tmp/umvl-cpu/run4`, with single-thread BLAS/OpenMP, nice priority and worktree build/tmp. Existing site/node_modules was copied with cp -a; no packages were installed.
- Initial full R run exposed the synthetic-fixture compatibility issue; corrected before the final rerun.
- `NOT_CRAN=true just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 39040 ]`; optional `{sn}` absent. Log: `build/scratch/r-test-final.log`.
- `just site-check`: PASS. All 98 family pages contain display MathML; every declared/inverted relation and limit link is rendered and resolves; generated C++17 snippets compile and link. Log: `build/scratch/site-check-final.log`.
- `just relations`: all declared edges pass. Log: `build/scratch/relations-final.log`. Relation regression tests cover incorrect parameter maps, missing coefficients, missing Jacobian/support mismatches and discrete endpoint inclusion.
- R loader: all 98 accepted; eight malformed vocabulary/map cases rejected. Source-field audit confirms all dossier bodies and pre-existing non-taxonomy fields unchanged; existing classifications preserved. `git diff --check` passed.
- Environment: Linux-7.0.0-38-generic-x86_64-with-glibc2.43; Python 3.14.5. See final logs for R/compiler versions. Numerical checks cover random seed neighbourhoods, not the entire parameter domains. No separate C++ numerical suite is needed for metadata/UI-only changes.

- Visual inspection caught global details margins stretching collapsed filters; scoped margins and grid alignment were corrected, followed by a green site/browser rerun.
- Browser acceptance passed: hydrated counts, coefficient count/kind conjunction, query/reset, mobile width, related-family navigation; no browser errors. Screenshots: `build/scratch/taxonomy-browser/catalog-{desktop,mobile}-final.png`.
- Artifact-pane display was attempted with bin/show; the viewer returned HTTP 403 because this worktree is outside its configured readable roots. No viewer settings or other checkout was changed; the report and final screenshots remain in this worktree.
