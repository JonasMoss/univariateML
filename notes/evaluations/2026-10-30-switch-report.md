# TASK-78.9 — internal d/p/q/r switch

No commit, compilation, installation or test suite was run; no sub-agents or board edits.
Artifacts and audit scripts are in this worktree or `/tmp/umvl-codex/switch/`.
## Files changed

- All 84 non-stats distribution `README.md` and `test.R` files under
  `cppunivariate/include/cppunivariate/distributions/`.
- Distribution `README_SCHEMA.md`; betapr/boundary-profile.R,
  genbeta/escape-probe.R, skellam/validate-profile.R, burr/boundaries.md.
- univariateML: DESCRIPTION, NAMESPACE, NEWS.md, CANONICAL_NAMES.md,
  README.Rmd, README.md, vignettes/distributions.Rmd.
- R/code_helpers.R, R/code_misc.R, R/univariateML-package.R.
- scripts/codegen_dpqr.R, codegen_wrappers.R, load_distributions.R, goldens.R;
  deleted scripts/test_export_schema.R.
- tests/testthat/test-generated-dpqr.R, test_fixed.R, test_generics.R,
  test_golden_regression.R, test_restricted.R; this report.
- No C++ files, truth/golden JSON, notes/, roadmap/, CLAUDE.md,
  KNOWN_ISSUES.md or STATUS.md changed.
## Changes by step

1. All 66 former external families select bare internal d/r names; the
   existing 18 own families remain internal selections. Fourteen stats
   families retain stats. Removed hnorm's sole argument map: dhnorm takes
   mu and sigma directly. Removed the folded/hnorm repair hardcode with the
   repair registry; the resolver now resolves its selected function directly.
   Existing generic r.args/r.shift infrastructure remains.
2. Deleted reference-fix and missing-quantile registries, defect probes,
   discrete/continuous R inversion and runtime repair implementations.
3. Replaced external sampling/likelihood calls with internal functions,
   naming coefficients explicitly and supplying formerly implicit defaults.
   Optimizer-versus-likelihood, fitting attributes, holds, and limit tests
   remain. Pure former-reference value comparisons are superseded by truth
   tables. Generic truth tests additionally check dml/pml/qml dispatch.
4. Removed all 18 export mappings, export validation/schema tests, codegen
   roxygen topics and coefficient defaults. Retained short family-specific
   facts in README d/p/q/r prose, included by fitter-documentation codegen.
   Internal densities link to dml rather than nonexistent density help.
   Source NAMESPACE contained 116 stale per-family exports (29 historical
   re-exported families), rather than the generated 72 own-family exports;
   removed all 116, preserving dml/pml/qml/rml and plotting exports.
   No tracked per-family Rd pages existed: man/ contains figures only.
   Assembly will no longer generate the 18 distribution help topics.
5. Imports is Rcpp only. simulate_default and golden-generation tooling
   resolve bare sampler names inside the namespace, including unexported ones.
   README tables/examples, canonical names and vignette now describe internal
   evaluation. No pkgdown config or CI dependency mentions needed changes.
6. Development NEWS records dependency removal, internal evaluation checked
   against high-precision truth, changed rml streams, and the unreleased
   eighteen-family APIs becoming internal; obsolete repair claims removed.
## Tests removed or converted

- bbinom: removed generic-inversion registry/veto test; retained lattice
  inversion inequalities, endpoints, invalid-probability and NA checks.
- bnbinom: removed obsolete reference-inversion work-budget/plot failures;
  retained quantile inequalities, log/upper-tail consistency and endpoints.
- dweibull: removed fresh-runtime-probe selection and probe assertions;
  flooring regression now uses the closed-form survival probability.
- triang: removed repair detection/formal-equivalence/value comparisons;
  retained pml corner-mode/endpoints and independent fitting checks.
- tnorm, gev: replaced repair/probe/reference-stream blocks with normal
  log-tail identities and tiny-shape Gumbel/log-tail round trips.
- trgamma, invtrgamma: removed repair-detection/ordinary-reference/seed-probe
  blocks; retained gamma-quantile and extreme-power closed-form regressions.
- hnorm: removed repair-registry/map test; retained half-normal closed forms,
  endpoints, translations, p/q checks and bootstrap. RNG comparisons use rhnorm.
- genbeta: removed RNG-defect detection; retained extreme analytic limits,
  density/CDF/quantile regressions and set.seed reproducibility.
- bhatt, slash, dnorm, dgamma, frechet: retained edge/mixture/tail checks
  using generated functions instead of repair entries or inversion helpers.
- tlambda, sgomp, skellam: deleted pure external-value comparisons; retained
  special members, tail, reflection, flooring and recycling checks.
- Golden regression fits stored g$data, preserving frozen numerical targets
  without replaying removed external samplers. No JSON or draws regenerated.
- test_fixed model-selection test checks the selected set and each family's
  parameter count, rather than freezing AIC row order from an old RNG stream.
  No other frozen numeric draw targets were changed; former fGarch fixtures remain.
## P4 overlap / merge notes

- invexp, invweibull, llogis retain current rate coefficients; hnorm retains
  mu/sigma in its generated API. hcauchy/ht retain their current unshifted
  coefficients. nhyper retains n/m/r and sample-size formal nn.
- gamma and hyper remain unchanged stats references. Merge all nine P4
  families with TASK-83 before regenerating wrappers/metadata and checking
  references, shifts, parameter names and tests. No P4 parametrization applied.
## Remaining extraDistr/actuar mentions

- univariateML/NEWS.md: removal announcement and historical API/provenance
  notes; cran-comments.md: old CRAN submission output.
- Family README references: actuar JSS citations, HalfCauchy/
  HalfT sources, slash/Bhattacharjee definition links; genbeta/lgamma/llogis/
  invweibull describe inherited parameterisation conventions.
- C++ comments and test labels, oracle-fixtures.inc, fixed-scale/golden JSON
  sampler fields, truth.py provenance: historical origins/oracle failures;
  left untouched. pig/dpqr.hpp still has an obsolete R-shell comment, left
  untouched under the no-header-change rule.
- dweibull/adapter.cpp error text cites the inherited parameterisation;
  burr/boundaries.md and invburr test comments explain reciprocal conventions;
  kumar/pig test comments cite historical defects. No dependency calls remain.
- quality/truth_report.py says its truth generator does not use those packages.
## Static validation and reviewer checks

- Parsed 144 R files; loaded all 98 YAML READMEs; generated 336 R functions
  and 84 C++ adapters in scratch, including README/core signature checks.
  Static direct-call matching found no missing coefficients/invalid names.
- git diff --check clean. Dependency/helper/export searches clean except
  historical/provenance mentions listed above. No compiled artifacts created.
- Optional attribute/fitter-doc generation was cancelled while queued for a
  machine slot; it never started. Reviewer must regenerate/roxygenise and
  verify no distribution topics/exports or broken internal help links remain.
- Run generated truth/dispatch tests, all R/C++ suites and package check;
  inspect changed-stream fitting/escape tests, bootstrap and model selection.
  Compare against known std/bnbinom golden and sstd fixture baseline failures.
- No blocking design question. Numerical and rendered-help validation remain
  with the reviewer under the requested no-compilation/no-suite speed rule.

Pane display was attempted; the viewer returned 403 for this worktree root.
