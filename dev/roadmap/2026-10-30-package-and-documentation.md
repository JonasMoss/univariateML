# Package structure and mathematical documentation

**Status:** banked design direction; implementation waits for the current
density, parameterization and MLE checking work to land. The directory layout
and site tooling below are proposals within that direction.

univariateML should have a light repository landing page, a fully R-independent
C++ library, a generated R package and a custom documentation website covering
both mathematics and use. Preserve the reasons for algorithmic and numerical
choices before simplifying implementations. Documentation should make that
knowledge available without reconstructing agent sessions or Git history.

## Repository structure

Keep one monorepo initially. A proposed target is:

```text
bindings/r/
  README.md                    # short introduction, installation and links
  LICENSE
  AGENTS.md
  Justfile
  univariateML-cpp/             # independently usable C++ library
    CMakeLists.txt
    include/
    tests/
    examples/
    docs/                      # algorithm and numerical explanations
  bindings/r/                  # authored R sources, adapters and generation
  univariateML-r/               # generated installable R package
  web/                         # custom website sources
  dev/
    STATUS.md
    dev/roadmap/
    research/
    validation/
    tools/
  .github/
```

The root README introduces the project and links to installation, C++ and R
usage, and the distribution catalog. Optional contribution boilerplate can go.
Retain licensing and attribution required by each distributed artifact.
`AGENTS.md` stays at the root. Status, TODOs, deferred plans and historical work
reports belong under `dev/`; active assignments remain on the external board.
Do not create a second task tracker in the repository.

`bindings/r/` holds editable inputs, including ordinary handwritten R methods,
plots, tests and package metadata. `univariateML-r/` is a disposable generated
package directory, normally ignored by Git and absent until generation. The
installed package remains named `univariateML`. Generate repetitive wrappers,
exports, metadata and help; do not require every useful R function to be codegen.
The existing `univariateML-r/` assembly is the starting point.

Move R adapters and R tests out of the C++ source tree. Retain one canonical
distribution dossier for definitions, parameterization and mathematical facts;
language bindings and the site consume it. Initially rename directories without
also renaming the C++ namespace and public include paths.

## Complete R independence

The C++ library must build, test, fit, evaluate and sample with a C++ compiler
and its declared Boost dependency, on a machine without R. Its distributed
source must not need sibling monorepo directories or an R-based generation
step. Frozen reference fixtures can have been produced using R, but executing
the standalone tests must not require R.

Move statistical behavior currently implemented in adapters into C++, including
the log-gamma fitter's transformation and likelihood adjustment. Audit all
families for equivalent gaps. C++ owns all supported d/p/q/r operations,
including familiar distributions otherwise supplied by R's stats package.

Samplers accept an explicit RNG object. Supply a standalone adapter using
`<random>` and keep the R-backed adapter and RNG-state handling in the R
bindings, so R sampling respects `set.seed()`. Do not introduce hidden global
C++ RNG state or promise identical streams across R and standalone backends.

Standalone CMake discovers Boost directly, without searching an R installation
for BH. The R package may continue to obtain Boost headers through BH.

## Distribution catalog and website

Build a custom static site with a searchable, filterable catalog and stable
pages for individual distributions, with custom styling and MathML rendered
at build time. Tooling, the JavaScript binding and the page structure are
decided in [site-and-js-binding](2026-10-30-site-and-js-binding.md) (Astro,
WebAssembly from the C++ core; Quarto was the initial preference). GitHub Pages can host
the site; `univariateml.jonasmoss.com` is the proposed custom domain. These are
future hosting choices, not instructions to publish or change DNS now.

Each distribution page should cover:

- Definition, aliases, parameterization, domains, support class and endpoints.
- Origin and attribution, distinguishing established history from uncertain
  attribution, with references to original work where available.
- Applications and an explained editorial classification such as classical,
  specialist or uncommon; no unsupported numerical popularity score.
- MLE existence, identification, uniqueness, consistency and asymptotic
  results, with assumptions and unknowns stated separately.
- Algorithm, held-parameter cases, restrictions, boundary and limit behavior,
  numerical accuracy, measured runtime and known limitations.
- C++ and R usage, related families, derivations and references.

Keep existence, statistical consistency, numerical reliability, global-search
guarantees and speed separate. Tests do not establish consistency or certify
a global optimum. Benchmarks identify the workload, environment and version.
"Not established" and "not checked" are useful catalog entries.

Extend the existing per-distribution README metadata and prose rather than
maintaining a duplicate catalog. Generate factual tables, binding metadata and
concise R help from those dossiers. Author derivations and history as prose;
select a short description for R help rather than copying a whole mathematical
chapter. Put general theory in shared articles. Render the same canonical
algorithm and numerical notes into the website where useful.

## Algorithm choices and their history

Review algorithmic choices as well as low-level arithmetic. For each fitter,
establish what problem it solves: the parameterization, held parameters,
fitting restrictions, parameter-space closure and intended handling of
nonexistence. Then explain the selected method and the strength of its result.

Documentation depth follows complexity:

| Case | Expected explanation |
|---|---|
| Straightforward closed form | Formula or citation, conditions, boundary cases and links to checks; usually a short paragraph. |
| Scalar or profiled solve | Objective or score, reduction, bracket, monotonicity or uniqueness argument where available, stopping rule and failure behavior. |
| Involved fit | Profile structure, starts and search strategy, competing modes and faces, conditioning, fallback paths, computational cost, alternatives investigated and unresolved guarantees. |

For involved fits, retain why a grid, multistart method, particular optimizer,
initialization, tolerance or restriction was chosen. Distinguish mathematical
necessity, numerical expedience, compatibility and performance tradeoffs.
Record rejected or reverted approaches when their failure explains the current
design, with a reproducer or benchmark where possible. An optimizer's successful
termination does not establish that the statistical MLE exists or is global.

Keep the current explanation concise and authoritative. Add dated decision
history for material changes, linked to commits, evidence and superseded
decisions; do not turn it into a transcript of every experiment. Shared methods
get shared notes inside the standalone C++ component, linked from the relevant
distribution dossiers. Family-specific arguments stay with their family.

Recover lasting conclusions from the existing evaluation reports, research
notes, tests and commits before retiring older implementations. Preserve the
original reports under `dev/` and retain links to them. If a rationale cannot
be recovered, record that it is unknown and investigate before replacing code.

## Numerical choices and Boost

Retain Boost.Math provisionally and assess each proposed replacement on its
requirements and evidence. An overlap with Boost does not establish that a
custom implementation is redundant. Log probabilities, support endpoints,
overflow, underflow, rounding compatibility and optimizer behavior can justify
specialized paths.

Inventory the actual call paths, separating Boost wrappers, standard-library
functions and our own numerical algorithms. Candidate reviews include Lambert
W, repeated scalar solvers and overlapping distribution/tail helpers. Evaluate
accuracy, applicable range, no-throw behavior, convergence diagnostics and
performance before selecting a replacement. Keep directory moves separate
from numerical changes.

Each substantial custom numerical routine needs a durable explanation of the
problem, a concrete example, applicable ranges, fallback conditions, sources,
error bounds or empirical evidence, and remaining limitations. Link it to
regression tests and reproducible checks. Shared beta-tail machinery currently
under GB2 is one example to consider relocating after documenting its users.

## Validation documentation and agent handoff

Create one validation guide explaining commands, inputs, reference provenance,
tolerances, expected outcomes and limits of each check. Cover golden regression,
symbolic likelihood and derivative checks, numerical optimization comparisons,
independent high-precision d/p/q values, sampling checks, boundaries, held
parameters and scale behavior. Identify which checks run in ordinary tests,
which are extended audits and which require optional tools.

Separate regression agreement, internal identities, independent numerical
evidence and mathematical proof. A round trip can agree with itself while
being wrong; a frozen output from our own fitter is a regression fixture, not
independent truth; a multistart optimization comparison is not a proof of
global optimality. Reference implementations can also be defective.

Run records identify the source commit, commands, relevant environment and
dependencies, workload or seed, tolerances and results. Report skipped checks,
permitted failures, missing oracle coverage and untested regions explicitly.
Existing reports and status summaries are historical evidence, not live claims
about a later commit. Preserve important reproducers outside temporary storage.

During implementation, add an `AGENTS.md` completion rule: substantive changes
to fitting algorithms or numerical methods must update the canonical rationale
and provide a reproducible check, or identify a concrete validation gap. Keep
unresolved experimental conclusions and rejected alternatives when they matter
to future choices. Brief documentation is sufficient for straightforward work.

Current evidence entry points to consolidate include:

- `univariateML-cpp/include/cppunivariate/distributions/<id>/README.md`, tests,
  `golden.json`, `truth.py` and `truth.json`.
- `univariateML-cpp/tests/test_support.hpp` for shared tolerances and truth checks.
- `dev/validation/quality/harness/check.py` and `dev/validation/quality/truth.py` for validation methodology.
- `dev/research/evaluations/2026-10-29-dpqr-truth/README.md` and dated repair reports,
  particularly `2026-10-29-fixD-report.md` for numerical rationale examples.
- `dev/research/ml-behaviour.tex`, the failure gallery and performance reports.
- `build/quality/` for generated results, not permanent methodological history.

## Migration sequence

1. Let current density, parameterization and MLE work land. Record a baseline
   that includes failures, skips and incomplete coverage.
2. Inventory algorithm and numerical decisions and their evidence. Pilot the
   documentation schema on contrasting families, including a closed form, a
   scalar solve and an involved search. Capture rationale before replacement.
3. Move directories and update build paths without changing numerical behavior.
4. Complete C++ independence and test a standalone source export without R.
5. Finish reproducible R package assembly and check the resulting source package
   without access to the monorepo.
6. Build the catalog, shared theory articles and generated documentation;
   progressively fill missing dossiers and decision history.
7. Review numerical simplification candidates individually, using their
   documented contracts and comparisons. Preserve evidence for each decision.
8. Review release artifacts, the landing page and hosting before publishing.

## Trigger to revisit

Resume after the current density implementation and ML checking work has
landed. The first implementation work is documenting and testing the contracts
needed for the migration, not replacing algorithms wholesale.

## Verification

- A standalone C++ export builds and runs its tests and all supported family
  operations without R or sibling repository directories.
- The generated R source package builds and checks independently; repeating
  generation from the same inputs produces the same substantive content.
- C++ and R cover the intended families with documented parameter conventions.
- Distribution tables and repeated documentation derive from canonical sources.
- Nontrivial fitters and custom numerical routines link their rationale,
  material decision history, reproducible checks and known limitations.
- Validation reports distinguish passes, failures, permitted failures, skips
  and gaps, with the tested revision and environment recorded.
- Site mathematics renders as MathML, links resolve, and the root remains light.

## Related plans

- [Python package for teaching and simulation](2026-10-30-python-teaching-and-simulation.md):
  a later consumer of the independent core and shared dossiers, with Python API,
  RNG, simulation and packaging choices to refine after the foundational work.
- [Inference regularity and derivative support](2026-10-30-inference-and-dqm.md):
  conditional DQM, information rank, derivative history, robust covariance
  requirements and procedures for parameter and hypothesis subsets.
- [Monorepo layout](2026-05-08-monorepo-layout.md).
- [Documentation from metadata](2026-05-08-documentation-from-metadata.md).
- [Distribution dossiers](2026-05-08-distribution-dossiers.md).
- [Density-first validation](2026-05-11-density-first-ai-validation.md).
- [R package scope revisited](2026-10-28-r-package-scope-revisited.md).
