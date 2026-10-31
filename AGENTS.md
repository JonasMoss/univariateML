# univariateML agent instructions

Read [CLAUDE.md](CLAUDE.md) for the current architecture, engineering conventions
and build workflow. Do not push or publish without an explicit user request.

## Banked plans

- [Useful density proposals](dev/roadmap/2026-10-31-useful-density-proposals.md):
  exponentiated Weibull, GIG, PGW, sinh–arcsinh and unified Prentice gamma; priorities and preliminary ML/dpqr risks.

- [Package structure and mathematical documentation](dev/roadmap/2026-10-30-package-and-documentation.md):
  standalone C++, generated R package, custom mathematical website and algorithm history.
- [Inference regularity and derivative support](dev/roadmap/2026-10-30-inference-and-dqm.md):
  conditional regularity, derivatives and inference by regime and hypothesis.
- [Python package for teaching and simulation](dev/roadmap/2026-10-30-python-teaching-and-simulation.md):
  deferred bindings, NumPy integration, simulation workflows and student installation.

Link to these canonical plans rather than duplicating them. Coordinate planned
work through the shared board and the user's orchestrators; a plan entry does
not authorize unrelated implementation during another task.

The C++ component must build, test, fit, evaluate and sample without R or sibling
repository directories. Keep R RNG state handling in the bindings.

## Algorithm and numerical documentation

For substantive algorithmic or numerical changes, update the canonical
distribution dossier or shared algorithm note and leave a reproducible check,
or name the concrete validation gap. See the [validation guide](dev/validation/README.md).
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

Read the linked inference plan before inference work. Classify DQM, finite
information, rank, Hessian suitability and estimator asymptotics separately at
the relevant parameter value and free subset. Interior parameters alone do not
establish regularity. State the hypothesis and whether nuisance parameters are
known or fitted.

Distinguish per-observation score cross-products, observed Hessians, model Fisher
information and estimating-equation sensitivity; record coordinates and objective
normalization. The total-score outer product and an optimizer's inverse Hessian
are not substitutes. Nonsmooth sensitivity and historical derivatives need
validation of their mathematical meaning and current parameterization.

Mark procedure availability and gaps by regime and hypothesis. Consider regular
submodels, exact and nonregular procedures when joint Wald inference fails.
Model-based and misspecification-robust inference need distinct assumptions;
bootstrap availability establishes neither validity nor robustness. Dated
inventories are historical evidence, not live coverage claims.

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
