# switchmerge (TASK-78.9)

Resolved all 48 conflict hunks in 23 files; index remains unmerged for reviewer staging.
No git writes, board edits, dependency installs, additional fitter edits or truth regeneration.

Conflict resolutions (distribution paths relative to cppunivariate/include/cppunivariate/distributions):

- `README_SCHEMA.md`: Retained internal-only evaluation schema; clarified direct mu and retained optional resolver maps.
- `bbinom/README.md`: Kept master free-size limitations; removed obsolete external quantile/repair prose.
- `cmb/README.md`: Combined master integer-size holds with internal evaluation notes.
- `hcauchy/README.md`: Kept translated parameters/support/docs; internal functions receive mu directly.
- `hcauchy/test.R`: Kept master endpoint/hold/bootstrap/oracle tests using internal translated functions.
- `hnorm/README.md`: Kept master endpoint semantics with internal translated references and examples.
- `hnorm/test.R`: Combined analytic/endpoint tests; internal RNG; asserted no redundant shift map.
- `ht/README.md`: Kept master translated/restricted parametrization and holds; internal references.
- `ht/test.R`: Kept master translation/tie/restriction/oracle tests with required internal coefficients.
- `invexp/test.R`: Kept scale tests, reciprocal identity and switch optimizer-domain guard.
- `invweibull/test.R`: Kept scale names/tests; translated old sampler rate to reciprocal scale.
- `llogis/test.R`: Kept scale names/tests and outlier oracle using internal functions.
- `nhyper/README.md`: Kept m,N,r coefficients/docs; removed external argument map; updated sampler example.
- `nhyper/test.R`: Kept master design tests; converted internal calls and overrides to m,N,r.
- `sged/README.md`: Combined master holds/endpoint prose with internal evaluation notes.
- `sstd/README.md`: Combined master holds/collapse prose with internal evaluation notes.
- `tbinom/README.md`: Kept master free-size support/docs; internal example; removed obsolete hold exclusions.
- `univariateML/CANONICAL_NAMES.md`: Kept master names/coefficient order with switch internal function names.
- `univariateML/R/code_helpers.R`: Kept expression-map/resolver implementation; clarified direct internal mu contract.
- `univariateML/R/univariateML-package.R`: Kept switch truth-table evaluation documentation; omitted deleted repair registries.
- `univariateML/README.Rmd`: Kept master display names/docs with switch evaluation-package column.
- `univariateML/README.md`: Kept master display names/docs with switch evaluation-package column.
- `univariateML/tests/testthat/test_golden_regression.R`: Kept master tolerances and switch fitting of stored golden datasets.

Auto-merge audit: converted added bbinom/tbinom external calls; fixed frechet/pareto3
reciprocal scale references and nhyper positional overrides in test_closure.R.
Check-driven docs repair: quote literal intervals in binom/gev/zib/zip/zmbinom/ztbinom
README controls/prose; correct hyper See also link to mlbinom(). No domain changes.
All 98 golden.json and all 98 truth.json files match master byte-for-byte.
CLAUDE.md, KNOWN_ISSUES.md and STATUS.md match master; notes/roadmap untouched by this job.
All 98 READMEs load; codegen signature check passes for all four core APIs.
Static scan: 139 R source/test files plus README examples, zero direct-call argument issues.
Forbidden-reference scan: only bibliography, NEWS/CRAN history, JSON sampler provenance,
and historical C++/oracle/convention comments remain; no executable extraDistr/actuar calls,
r.export or repair registries. Six changed historical comment lines come from master.

Verification on simbox (nice 10; BLAS/OpenMP one thread):

- just cpp-test: 6/6 CTest targets pass (206.34 s); 906 cases, 1,210,994 assertions.
  distribution_tests alone: 884 cases, 1,199,947 assertions; zero failed/skipped.
- Full uncapped R suite after just regen: [ FAIL 0 | WARN 0 | SKIP 0 | PASS 34117 ] (407.0 s).
- just check (TMPDIR=$HOME/umvl/tmp; UMVL_DISTRIBUTIONS_DIR set): Status: OK;
  zero errors, warnings or notes; installed tests and examples pass.

Open questions: none. Reviewer stages and commits the merge.
Logs: /tmp/umvl-codex/switch/merge-cpp.log, merge-{regen,r}-final.log, merge-check-final.log.
Pane display attempted; viewer returned 403 because this worktree is outside its readable roots.
