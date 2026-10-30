# univariateML agent instructions

Read [CLAUDE.md](CLAUDE.md) for the current architecture, engineering conventions
and build workflow. The current directory layout matches the migration plan; later steps
cover C++ independence and documentation. Do not push or publish without an explicit
user request.

## Banked plans

- [Package structure and mathematical documentation](dev/roadmap/2026-10-30-package-and-documentation.md):
  light landing page, standalone `univariateML-cpp`, generated `univariateML-r`,
  custom mathematical documentation site, and preservation of algorithm choices
  and validation history.
- [Inference regularity and derivative support](dev/roadmap/2026-10-30-inference-and-dqm.md):
  conditional DQM and information classification, historical derivatives,
  robust covariance requirements and procedures for subsets of hypotheses.
- [Python package for teaching and simulation](dev/roadmap/2026-10-30-python-teaching-and-simulation.md):
  deferred Python bindings, distribution and fit objects, NumPy RNG integration,
  simulation outcomes and installation for teaching; refine after the foundational
  work lands and coordinate implementation with the user's orchestrators.

These are the canonical plans. Link to them rather than copying their full
contents into agent or orchestrator instructions. The cleanup follows the
current density, parameterization and MLE checking work; coordinate its start
with the user's orchestrators. A plan entry is not an instruction to start an
unrelated migration or implement inference during another task.

The target C++ component must build, test, fit, evaluate and sample without R
or sibling repository directories. RNG adapters keep R state handling in the
bindings. The root stays light; status, TODOs, roadmaps and historical
development reports belong under `dev/`, while `AGENTS.md` remains at the root.

## Algorithm and numerical documentation

For substantive algorithmic or numerical changes, update the canonical
distribution dossier or shared algorithm note and leave a reproducible check.
Keep documentation proportional to complexity: a formula, conditions and
reference may suffice for a closed form. An involved fitter needs its objective,
profile or search structure, initialization, convergence and failure behavior,
boundary comparisons, computational tradeoffs and unresolved guarantees.

Preserve the reasons for material choices and changes. Retain significant
rejected or reverted approaches when they explain the current design, with
dated decisions and links to commits, derivations, reproducers or benchmarks.
Recover lasting conclusions from existing reports into current documentation;
keep the original evidence accessible. Important findings must not exist only
in chat, task summaries, ignored build output or temporary files.

An apparent overlap with Boost or another implementation is not evidence that
custom code is redundant. Before replacing it, establish its contract, protected
cases, rationale and comparison evidence. Record unknown rationale as unknown.
Keep layout moves separate from changes to numerical behavior.

## Inference requirements

Classify the model at the relevant parameter value and free subset. Record DQM,
finite information, information rank, pointwise Hessian suitability and estimator
asymptotics separately. Interior parameters and fixed support do not by themselves
establish regular inference. For hypothesis tests, identify the target, null and
nuisance parameters; known nuisance and fitted nuisance are different settings.

Inventory per-observation scores, their cross-product sum, observed Hessians,
model Fisher information and estimating-equation sensitivity separately. Record
parameter order, coordinates, objective normalization and supported free subsets.
Do not substitute the outer product of the total score for the observation-level
cross-product sum, or an optimizer's inverse-Hessian approximation for validated
information. Nonsmooth cases can require a different sensitivity calculation.

Historical derivative code is a recovery candidate, not an inference guarantee.
Check its mathematical meaning, current parameterization and validation before
reuse. In particular, a method named `hessian()` may return Fisher information,
cover only a profiled subproblem, or use transformed coordinates.

Document and mark procedure availability by regime and hypothesis: implemented
and checked, historically recoverable, mathematically available but missing,
unreviewed, or invalid for that procedure. Evaluate regular submodels, exact
pivots and tests, and appropriate nonregular procedures even when joint Wald
inference fails. Model-based inference and inference robust to misspecification
need distinct assumptions and checks. Bootstrap availability alone establishes
neither validity nor robustness.

The [initial 98-family inventory](dev/research/evaluations/2026-10-30-inference-inventory.csv)
is tied to the revision recorded in it. Its 29 initial conditional classifications
and 69 unreviewed families are a historical starting point, not live coverage
claims. Review the source and evidence before updating availability labels.

## Validation and coordination

Record the tested source revision, commands, environment, workloads or seeds,
tolerances and results. Distinguish passes, failures, permitted failures, skips
and missing coverage. Golden regression, internal identities, independent
numerical comparisons and mathematical proofs support different claims.
Inference additionally needs checks of coverage or test calibration under its
stated assumptions; likelihood agreement alone is insufficient.

Use the shared board through `/home/jonas/.local/bin/board`, check active tasks
before touching overlapping files, and keep its task records current. Across
concurrent workstation jobs use at most four cores, pin BLAS/OpenMP to one thread
in parallel workers, and run long computations under `nice -n 10`. Use simbox
only when the task or user calls for it.
