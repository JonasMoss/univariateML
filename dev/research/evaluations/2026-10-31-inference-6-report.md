# TASK-102.6: inference availability by family, regime and hypothesis

Completed 2026-10-31 in `codex/inference-6`, based on
`d257f4a6f8c2b3dfa1ed6732b4fd50cde97a8db3` plus the uncommitted changes
identified in this report. The availability data, site page, acceptance checks
and desktop/mobile inspection are complete. No commit, board edit, package
installation, sub-agent, simbox run, numerical-source edit or golden change
was made.

## Canonical data model and provenance

The canonical procedure availability artifact is
[`dev/validation/inference-availability.json`](../../validation/inference-availability.json),
reproduced by
[`dev/validation/inference_availability.py`](../../validation/inference_availability.py).
Run `python3 dev/validation/inference_availability.py` through a CPU reservation
to regenerate the checked-in artifact. The site imports the same generator;
its acceptance check requires the serialized artifact and every family's
attached rows to agree with that generator exactly.

The generator projects all 118 family/regime rows of the
[existing inventory](2026-10-30-inference-inventory.csv), then adds 39 explicitly
reviewed procedure/hypothesis scopes from the
[TASK-102.1](2026-10-31-inference-1-report.md),
[TASK-102.4](2026-10-31-inference-4-report.md) and
[TASK-102.5](2026-10-31-inference-5-report.md) reports and their implementation
headers. The inventory and its original historical generator are unchanged.
Their dated derivative-API flags are not reused as live availability claims.

Each row has family, regime, target/hypothesis scope, status, procedure(s),
theory grade and rate, assumptions, DQM/information/rank descriptions,
derivation/source anchor, calibration-table anchor, implementation header and
function names, check paths and an explicit gap. Procedure-specific targets
separate, for example, a normal mean exact interval from a model-based mean
test or a mean at a KL projection. The key is `(family, regime, target)`;
acceptance rejects duplicate keys. Exact and asymptotic qualifications coexist
with a separate model-only qualification in the theory text; root-n and rate n
are estimator scalings, not claimed coverage-error rates.

The 118 inventory rows retain the target label “Other hypotheses: not reviewed”.
They preserve every inventory regime without claiming that DQM, finite
information or historical derivatives establish an estimator limit or a
procedure. Thus a family can have both an unreviewed remainder and implemented
specific targets. Counts are procedure/hypothesis scopes, not distinct methods,
parameters or calibrated Monte Carlo cells.

| Status | Rows |
|---|---:|
| Implemented | 28 |
| Historically recoverable | 0 |
| Available but missing | 4 |
| Unreviewed | 120 |
| Invalid for the procedure | 5 |
| Total | 157 |

All 98 families have at least one row. Implemented scopes occur in nine
families: normal, gamma, beta, Poisson, Laplace, Pareto, exponential, uniform
and triangular. Historical derivative names and symbolic-grid outcomes remain
available inside the unreviewed rows. Zero historically recoverable procedures
is deliberate: the supplied historical scan explicitly does not validate
current coordinates, normalization, observation-score semantics, estimator
limits or inference calibration. It would be misleading to promote those
records into a positive procedure category.

Positive rows link their assumptions and source derivation to existing code.
For “available but missing”, implementation links identify supporting pointwise
statistics rather than a completed endpoint interface. The gap field states
what remains missing. Every implemented row additionally has a calibration
anchor and links to family C++ cases and its production calibration driver.
This job reuses the reports' analytical and numerical evidence; it neither
reruns their Monte Carlo studies nor proves additional statistical theorems.

## Site and checks

The new static route is `/inference/`, authored in
[`site/src/pages/inference/index.astro`](../../../site/src/pages/inference/index.astro),
with a main-header link. [`data.py`](../../../site/scripts/data.py) attaches
`inference` rows to each family. The page provides combined status, family
and regime-substring filters, a row count, empty state and reset. All 157 rows
are present in the generated HTML; assumptions, sources, calibration,
implementation and gaps use native disclosures. With JavaScript disabled,
filters are hidden and every row and disclosure remains usable.

The guide explains the five categories, DQM, information rank, Hessian
suitability, nuisance handling and separate robust assumptions. It links the
canonical roadmap and reports by repository paths, states that inference is
C++ only, and does not treat the R parametric percentile bootstrap as audited.
Repository links point at master; the page states that unpublished worktree
paths may not yet exist there. Their source files and evidence anchors are
validated locally, rather than claiming a remote publication.

There are no TeX delimiters or equations requiring conversion on this page;
the existing build-time MathML pipeline remains in force throughout the site.
The site check verifies MathML on all family pages, absence of runtime math
engines and external runtime assets, and resolving local site links.

[`check.py`](../../../site/scripts/check.py) now verifies family coverage,
unique scope keys, canonical/generated data agreement, allowed statuses,
nonempty assumptions/theory/procedures, Markdown evidence anchors, named
implementation symbols, calibration and check paths. It checks that the full
table is statically rendered and invokes
[`inference-browser.mjs`](../../../site/scripts/inference-browser.mjs).
That browser test uses the existing installed Playwright/Chromium tooling,
serves the built site locally, tests every status filter, combined filters,
reset, empty state and disclosures, checks mobile overflow and browser errors,
and opens a separate JavaScript-disabled context to verify all 157 rows and
an expanded Pareto implementation link. The server and browser close before
the test returns.

An initial acceptance attempt correctly stopped on a faulty new row-count
assertion: a raw substring count included the inline filter script's selector
as well as the 157 table rows. The assertion now counts actual `<tr>` elements.
The final complete `just site-check`, including the integrated browser check,
is green; no assertion or numerical test was disabled.

## Screenshots and visual inspection

All four final captures were opened and visually inspected. Desktop uses
1280 × 1100; mobile uses 390 × 1000. The desktop table keeps regime, target and
theory in separate columns. Mobile stacks each row, and expanded assumptions,
implementation names and gaps remain readable. Neither mobile state has
horizontal document overflow. The default captures show the page introduction
and filters; the evidence captures show a gamma misspecification scope and
triangular interior-mode inference, respectively.

| Capture | Worktree path |
|---|---|
| Desktop | `build/scratch/inference-6/desktop.png` |
| Desktop with evidence | `build/scratch/inference-6/desktop-evidence.png` |
| Mobile | `build/scratch/inference-6/mobile.png` |
| Mobile with evidence | `build/scratch/inference-6/mobile-evidence.png` |

![Desktop availability page with filters and the beginning of the complete table.](../../../build/scratch/inference-6/desktop.png)

![Mobile triangular scope with assumptions, calibration, implementation links and limitations expanded.](../../../build/scratch/inference-6/mobile-evidence.png)

The captures, browser result JSON, source hashes and full command logs are
retained under `build/scratch/`; these are ignored build artifacts and can be
regenerated. The generator, canonical data, browser test and this report are
repository deliverables.

## Final validation

All builds and tests ran as foreground commands through the shared CPU locks.
`run4` reserves four slots and pins BLAS/OpenMP to one thread; its wrapper also
sets nice level 10. No reservation wrapped a waiting script or another slot
wrapper. `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` were set to
`$PWD/build/tmp`. `site/node_modules` was copied with `cp -a` from the main
checkout as authorized; the main checkout was not modified.

| Final command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just site-check` | PASS; 157 unique scopes; 98 families; implementation/evidence/calibration checks; integrated browser test; 98 C++17 examples compile and link; existing static checks green |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 151, PASS 19843 |
| `/tmp/umvl-cpu/run site/node_modules/.bin/node site/scripts/inference-browser.mjs` | PASS; also rerun inside the final site check |
| `git diff --check` | PASS |

The R skips are the existing dev-tier exclusions and optional comparison
coverage, not new permitted failures. No R or C++ numerical source or numerical
test infrastructure changed, so the final requested R dev tier was used;
no new full numerical or calibration run is asserted. Build logs contain
existing shell startup messages about a missing cargo environment file and
Pagefind's UI notice; testthat's warning count is zero. The final logs are
`build/scratch/inference-6-site-check.log`,
`build/scratch/inference-6-test.log` and
`build/scratch/inference-6-browser.log`; the integrated browser result is also
in `build/scratch/inference-6/browser.json`.

Environment: Python 3.14.5, SymPy 1.14.0, R 4.6.1 (2026-06-24),
g++ 15.2.0, site Node 22.23.4, Astro 7.4.4, Playwright Core 1.64.1,
Chromium headless shell cached revision 1234. Boost headers were resolved to
`/home/jonas/R/x86_64-pc-linux-gnu-library/4.5/BH/include` by the existing site
check. Source identities for the tested deliverables are recorded in
`build/scratch/inference-6/source-identities.json`.

## Explicit open availability entries

These are published scientific or interface gaps from the source reports,
not unfinished site work.

- **Gamma mean, gamma shape and beta mean full Rao-set endpoint interfaces:**
  pointwise tests exist. Gamma mean has a remote unbounded accepted tail;
  complete component representation is missing. Gamma shape and beta mean
  still need certified topology or global root isolation, plus conditioning
  decisions. Profile-LR concavity does not certify Rao-set topology.
- **Triangular exact score inversion:** the null count has the stated binomial
  law, but an exact inversion API and independent calibration are missing.
  The supporting production score statistic is linked without claiming that
  it already supplies exact intervals.
- **Triangular simulated-LR confidence sets:** null-specific simulated testing
  is calibrated in the retained driver, not exposed as a general confidence-set
  API. All components, null-specific simulation and controlled Monte Carlo
  error need handling. A forced connected LR interval is disproved.
- **Triangular boundary modes, fitted endpoints and robust mode inference:**
  separate boundary theory, kink sensitivity, identification, density estimation
  and calibration are missing. They remain explicit unreviewed scopes.
- **Other family/regime hypotheses and historical recovery:** the inventory's
  conditional DQM/rank arguments do not establish every estimator limit or
  procedure. Source-level recovery must verify current coordinates, objective
  scaling and derivative meaning before any historical procedure is advertised.
- **Small-sample and misspecification behavior:** implemented is not an
  adequacy recommendation. The page preserves gamma/beta small-n limitations,
  gamma sandwich undercoverage under the lognormal alternative, Laplace cusp
  behavior, triangular Wald undercoverage and Poisson boundary unavailability.
  The linked calibration grids do not imply uniform coverage.
- **R/JS integration and bootstrap validity:** no inference bindings exist yet;
  the R percentile bootstrap needs procedure-specific validity analysis.
  No changes to settled d/p/q/r, special functions, truth fixtures or codegen
  are needed for this publication task.

## Artifact display

The requested `codex-math-web/bin/show` command was attempted after completion.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots (the main checkout is registered, but this sibling worktree is
not). No files were copied outside the worktree and no viewer configuration
was changed. The report and captures remain available at the paths above;
this display-service restriction does not affect the green site/browser tests.
