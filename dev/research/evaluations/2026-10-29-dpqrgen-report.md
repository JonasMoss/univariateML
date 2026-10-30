# TASK-78.8: generated internal d/p/q/r

## Design

- Assembly invokes `scripts/codegen_dpqr.R` before Rcpp attribute compilation.
  All 98 core signatures are checked against README coefficient names/order.
  The 84 non-stats families receive 336 R functions and Rcpp entrypoints in
  the ignored assembled package. Stats references remain on stats.
- New `r.export` boolean defaults to false. Only the eighteen established
  own families set it true. No external-family reference was switched.
- Existing exported formals (including defaults and logitnorm's `...`) live
  in `scripts/dpqr_api.R`. Documentation text was retained verbatim in
  `R/dpqr_documentation.R`; the old eighteen `dpqr.R` files were removed.
- Generated numeric calls use the existing `dist_impl::dpqr_apply` policy.
  RNG wrappers apply the base-R length convention and recycle coefficients
  to exactly that length. `src/rng_r.h` provides uniform, normal, gamma, t
  and Poisson primitives under Rcpp's RNGScope, calling core sampler templates.
  EMG uses an R-host specialization in `src/rng_emg.h` with R's exponential
  primitive: this preserves its frozen R stream and exact sample arithmetic;
  an exponential/uniform round trip shifts its finite-difference fit.
  EMG data, logLik and estimates replay exactly; the core sampler is unchanged.
  Density validation rejects invalid sampler parameters before drawing.
- Codegen rejects names in base/stats/utils/graphics/grDevices/methods
  namespaces, independently of which packages are attached.
- `r.args`, `r.shift`, fitting adapter functions, fitting behavior, goldens and truth
  data remain unchanged. Only old d/p/q/r sections were removed from adapters.

## Signature changes

- betapr d/p/q: scale now precedes flags; old omitted-scale callers explicitly
  pass 1.0. The sampler retains its scale default.
- tbinom d/p/q/r: coefficient order is prob, a, b, size (README order).
- hnorm: dhalfnorm/phalfnorm/qhalfnorm/rhalfnorm renamed to d/p/q/rhnorm.
- unif: runif_ renamed runif; min_/max_ argument names now min/max.
- hyper d/p/q/r: m,n,k replaces N,m,k; N=m+n is derived internally.
  Callers convert N,m to m,N-m; truth adapters directly pass README values.
- cmb and zipf samplers take double size/N, avoiding R-side truncation to int.
- Argument-name-only normalization (no expression changes beyond renaming):
  bhatt, cmb, cmp, dgamma, dnorm, emg, ged, gev, gpd, huber, invexp,
  invparalogis, invpareto, logitnorm, naka, sged, skellam, slash, snorm,
  sstd, std, tlambda, triang: CDF input q and/or lower_tail/log_p flags;
  skellam also uses README names mu1/mu2 for p/q/r.
- Existing callers and per-family C++ truth lambdas were updated. No MLE
  implementation needed a signature-fallout edit.

## Files changed

- Core: signatures/argument identifiers in the families listed above;
  caller updates in betapr, hnorm, hyper, tbinom and unif `test.cpp`.
- Eighteen own-family READMEs: export key only; their adapter.cpp files:
  removal of handwritten d/p/q/r sections only; their dpqr.R files removed.
- Schema: `distributions/README_SCHEMA.md` documents export and generation.
- R sources/scripts: assembly, loader, new dpqr codegen, compatibility
  formals, preserved documentation, RNG adapter and EMG host specialization.
- New generic R test: `tests/testthat/test-generated-dpqr.R`.

## Validation

All compilation and test suites run through `/tmp/umvl-simbox/run`.

- Baseline/final C++ `distribution_tests`: 836 cases / 1,198,189 assertions;
  835 cases pass, one known std golden assertion fails. The other five CTest
  targets pass. The known betapr slow fixture passes on this run.
- Baseline R: 22,724 passing expectations, three failures (sstd frozen
  reference, bnbinom golden, std golden), no warnings/skips.
- Targeted generated-layer R test: 6,247 expectations pass; no warnings/skips.
  All 84 families / 506 parameter points are covered: 2,530 value rows,
  6,072 quantile rows, 16,192 draws reproduced twice under set.seed.
- Baseline check: one existing Rd cross-reference warning, no notes;
  tests fail, including missing-dossier JSON errors without the env variable.
  Final check supplies UMVL_DISTRIBUTIONS_DIR explicitly.
- Exported-formal (72 functions), NAMESPACE, help-topic (18) equivalence,
  determinism (85 generated files), and both rejection audits pass.
- `just quality betapr cmb hnorm hyper tbinom unif zipf`: all seven pass.
- Final `just test`: 28,971 passing expectations, the same three baseline
  failures, no warnings/skips. EMG golden replay passes unchanged.
- Final `just check`: 28,958 passing expectations, the same three known
  failures, one CRAN-only skip (`test_special.R`), no test warnings. One ERROR from
  those known failures, one pre-existing Rd cross-reference WARNING, no NOTEs.
  The warning text is identical to baseline; there are no added warnings/notes.

The generic truth test selects five evenly spaced value rows per parameter
point, three evenly spaced quantile rows per tail/scale group, and 32 draws
per point. It uses the C++ truth defaults: log tolerance 1e-10, natural-tail
relative tolerance 1e-10 above 1e-300, quantile relative tolerance 1e-9 or
oracle bracket inclusion. Deliberately fractional density rows suppress the
expected non-integer warning; a separate shell-policy check asserts it.
Missing dossier directories or truth tables skip the truth test.

Logs are retained under `/tmp/umvl-codex/dpqrgen/`: `baseline-*.log`,
`final-{gen,regen,cpp,r,check,quality}.log`, `targeted-r.log` and
`audit-codegen.log`. The isolated EMG replay also verifies zero data, logLik
and estimate differences from the unchanged golden.

## Open questions

- No blocking questions. nhyper has a coefficient named n; its generated
  sampler uses nn for sample size, preserving n for the resolver (the
  extraDistr convention). This choice was raised during implementation.
- TASK-78.9 should replace hnorm's external dtnorm argument map when
  selecting dhnorm; this task deliberately leaves that README reference intact.
- No commit was made; the reviewer owns committing.

Artifact-pane display was attempted with the required `show` helper, but the
viewer returned HTTP 403: this worktree is outside its configured readable
roots. The report remains in the requested worktree and is linked in chat.
