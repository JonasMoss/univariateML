# TASK-82: P3 domains

The requested green R state is blocked by three stale assertions in the
excluded binom/test.R file. The prepared three-line patch awaits a scope
exception; that file has not been edited. All started commands have finished.

No commits, git writes, board edits, sub-agents, installs, or protected shared
source edits. All paths below are relative to
`univariateML-cpp/include/cppunivariate/distributions/` unless stated otherwise.

Files changed:
- `nbinom`, `zinb`, `pig`, `cmp`, `cmb`: README.md, mle.hpp, adapter.cpp,
  test.cpp and test.R.
- `bhatt`: test.cpp and test.R; its P2 implementation already included a=0.
- `ztnbinom`: mle.hpp and test.R; `zmnbinom`: test.R.
- Domain strings only: `zip`, `hyper`, `nhyper`, `nsbeta` README.md.
- Root `codex-report.md`.

Boundary members:
- NB: size zero is accepted with compatible zero data, under every hold.
  Free size on all-zero data is zero; unidentified free prob is 0.5.
  Positive held size still estimates prob one. All boundaries are silent.
  The adapter now delegates all subsets to the core, retaining its explicit
  diagnostic when held size makes a positive-data probability round to one.
- ZINB: size zero is accepted and returned on all-zero data with free size;
  unidentified free prob/pi are 0.5/0. Supplied coordinates are preserved.
  Positive held size uses free prob one, or free pi one if prob is held
  below one. Fully held nonmaximal parameters still evaluate their likelihood.
- PIG: shape zero is accepted for zero data and returned with free shape.
  Unidentified free mean is one; held mean is preserved. With positive shape
  held and mean free the mean-zero escape remains. Infinite mean stays excluded.
- CMP: the family entry `cmp_domain_mle` handles lambda zero, returning
  free nu one, and converts the legacy geometric-face result to attained Ok.
  nu zero still requires lambda below one. R calls the family entry.
  Adjacent two-point and positive point-mass divergence remain escapes.
- CMB: prob zero/one and size zero are accepted. Free endpoint fits preserve
  held dispersion, or use nu one when unidentified. Size zero uses free
  prob 0.5 and nu one. Bernoulli endpoint fits are silent; the existing
  unidentified-dispersion warning for interior Bernoulli fits is retained.
- Bhatt: regression checks confirm the existing free fit returns normal a=0
  and the held normal member preserves mu/sigma and its normal likelihood.
- Removed attained members from limits; narrowed PIG's zero escape to its
  positive-held-shape scope. No other family fitter was added to the scope.

Domain audit:
Reviewed all 98 README parameter lists against finite proper-law R7 and the
P0 group domain probes. Closed NB/ZINB size, PIG shape, CMP lambda/nu and CMB
prob; capped CMB integer size at the reference's 10000000. Bhatt already
has its correct closed a endpoint. ZIP lambda is strictly positive because
its reference returns NaN at zero. Added joint-order constraints to nsbeta
min/max, hyper k<=m+n and nhyper r<=m. Integer flags remain in force.
Continuous scale/shape-zero point masses, infinite parameters and improper
zero-rate/endpoint limits remain excluded. Zero-truncated/modified NB's
existing size-zero logarithmic members remain included, with no domain edit.
Discrete Laplace keeps integer location: the reference's real-location
expression is not normalized on the integer lattice, so real locations
fail R7 despite numerical acceptance by the reference.

R6/A6:
ZINB now diagnoses exhausted scalar refinement on the default free-size path,
as well as the existing held paths, and propagates refinement failure.
Truncated NB rejects an insufficient default scalar-refinement budget;
modified NB inherits the status through its existing conditional solver.
PIG fails if no finite objective was found. Its existing iteration status,
Bhatt's simplex status and CMP's iteration/work status stop at the adapter;
CMB already verifies returned moment scores. NB retains its converged
bracketed-root fallback. No failed optimizer is newly rescued without a
score certificate; stopping conservatively is allowed by R6.

Evidence:
Zero laws attain likelihood one, the universal upper bound for count data.
CMP's geometric-face criterion follows exponential-family concavity and its
one-sided sufficient-statistic score (P0 discrete2). CMB endpoints have exact
likelihood one; Bhatt a=0 uses the conditional normal formula. These are
analytic reductions. Existing multistart/grid searches and likelihood-tie
comparisons remain numerical, without global certification.

Goldens: none changed. Default interior fits are preserved. Default zero-only
NB/ZINB/PIG/CMP/CMB fits and free geometric CMP fits change as required by R7.
Bhatt's attained normal behavior was already present in P2.

Validation (final simbox runs):
- `just cpp-test`: all six CTest targets finished; five passed outright.
  Distribution suite: 638 cases, 636 passed, exactly two permitted failures
  (betapr/test.cpp:33 and std/test.cpp:40), three skipped. Raw assertions:
  957842 total, 948893 passed, 8949 failed; 8947 failures are protected
  may_fail truth-table assertions, and only two are non-may_fail failures.
  Across all targets: 660 cases (658 pass, two known failures), 968889
  assertions (959940 pass, 8949 raw failures as classified above).
  Runtime 249.85 seconds. No unexpected failure.
- `just quality nbinom zinb ztnbinom zmnbinom pig cmp cmb bhatt zip hyper
  nhyper nsbeta`: 12/12 pass, exit zero.
- Final `just test`: 22406 passing expectations, six failures, zero
  warnings and zero skips. Three failures are exactly the accepted frozen
  sstd quantiles and bnbinom/std goldens. Three are stale cross-family NB
  assertions in excluded binom/test.R (warnings and old representative).
  There are no other failures. All 2601 conformance checks pass.
  Runtime 391.7 seconds.
- Corrected owned R test files additionally pass directly on simbox:
  CMP 127, truncated NB 105, modified NB 210 assertions (442 total).
All full commands used `/tmp/umvl-codex/simbox-run.sh` from the worktree root.
The full R/C++ commands exit nonzero for the failures classified above.
Local focused C++ check: 36 cases and 423 assertions passed (truth cases
excluded from this additional focused check). R test files parse.

Changes needed outside owned files:
- `binom/test.R:41`: three cross-family NB assertions require expect_silent
  and the new free representative (0,0.5). This is the only conflict with
  the requested final green R run; changing it needs a scope exception.
- `special/cmp.hpp` still exports the legacy `cmp_mle` semantics. Its owner
  should migrate/forward that API to the family boundary implementation;
  protected shared tests intentionally retain the old expectations meanwhile.
- NB/ZINB C++ dpqr and special::dnbinom still reject size zero. TASK-78 needs
  zero-size d/p/q/r handling. Existing positive-size density tests are retained;
  new boundary tests check fits and the R reference evaluations.
- Untouched restrict-job corrections: lst df, frechet lambda and ged nu
  should be (0, inf); gev/gpd xi and tlambda lambda should be (-inf, inf).
  ht nu and sged nu already have their correct positive marginal domains.
  R5 fitting bounds belong only in fit_domain.
- Untouched closure-job domains: tnorm a/b should follow finite R7 and a<b;
  tpois additionally needs a nonempty conditioning event of positive parent
  probability. Power/hnorm need no marginal-domain correction. Endpoint
  support closure remains that job's responsibility.
- P0 records extraDistr nhyper crashes at n=0. Its natural proper completion
  needs the shared reference/DPQR repair; no crash-prone probe was repeated.

Open questions: certify existing global profile searches; resolve CMB free-size
fitting (P2 holds remain declared). No unresolved choice for the owned boundary
fits. Logs: `/tmp/umvl-codex/p3-domains/`.

Artifact pane: show was attempted, but its configured readable roots exclude
this worktree. The report is present here; no viewer configuration was changed.
