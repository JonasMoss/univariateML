# Python package for teaching and simulation

**Status:** banked direction; refine and implement after the current cleanup
and foundational C++ work have landed. Coordinate the start with the user's
orchestrators. Saving this plan does not start binding implementation.

The Python package should make it pleasant to simulate univariate data, fit
distributions and study estimator behavior in notebooks and teaching material.
It consumes the same C++ algorithms and distribution dossiers as R. Broad family
coverage is a goal; keep the interface small and consistent as new families arrive.

## Scope

Provide distribution construction, density and log-density evaluation, CDFs and
tail probabilities, quantiles, sampling and maximum-likelihood fitting. Support
the held parameters and restrictions available in the core, with their documented
limitations. Useful workflows include comparing fits, examining likelihoods and
repeating estimation experiments across samples and sample sizes.

NumPy is the intended array interface. Keep plotting libraries, notebook tools
and SciPy optional where possible; examples and independent checks can use them
without making them the implementation of our statistical methods. A general
simulation framework, regression, Bayesian inference, GPU support and automatic
differentiation are outside the initial scope.

Python may expose distribution evaluation directly even though the current R
package exports it through model objects. This does not change R's scope.

## Architecture and source ownership

Follow the accepted monorepo migration layout:

```text
univariateML-cpp/                 # standalone algorithms and family dossiers
bindings/python/
  pyproject.toml                 # package and build configuration
  CMakeLists.txt
  src/                           # Python conveniences and compiled adapters
  distributions/                 # family-specific adapters if needed
  tests/
  examples/                      # teaching scripts or notebooks
web/                             # shared documentation and teaching articles
dev/                             # plans, decisions and validation evidence
```

These are target paths. The current `python/` directory is a placeholder; settle
its relocation with the migration rather than create parallel binding trees.
Generated wrappers, wheels and source archives are disposable artifacts. Python
sources and tests remain authored inputs; a generated package directory is optional.

C++ owns fitting, likelihoods, d/p/q/r, parameter restrictions and mathematical
failure or boundary behavior. Python handles arrays, objects and teaching
conveniences. Move any statistical logic confined to R adapters into C++ before
exposing the corresponding Python capability. Building and running the Python
package must require neither R nor an R-based generator.

Generate repetitive exports, signatures, parameter descriptions, type hints and
capability listings from canonical family metadata. Keep useful Python methods
handwritten. Extend shared metadata where a binding needs information currently
implicit in an R adapter; avoid brittle parsing of C++ signatures or a duplicate
handwritten catalog.

Nanobind with scikit-build-core is the initial tooling preference; pybind11
remains an alternative. Choose after a small build and packaging pilot and record
material reasons. Nanobind documents a CMake and wheel workflow in its
[packaging guide](https://nanobind.readthedocs.io/en/latest/packaging.html).
The binding dependency stays outside the standalone C++ component.

## Proposed public interface

Distinguish a distribution with specified parameters from a fit to data. A frozen
distribution evaluates and samples without observations. A fit carries both a
distribution and estimation information. Omitted constructor parameters can use
canonical user-facing defaults; omitted fit parameters are free unless the model
requires a known design or restriction.

The following interface is illustrative. Package and import names have not been
selected or checked for availability.

```python
import numpy as np
import univariateml as uml

rng = np.random.default_rng(42)
model = uml.gamma(shape=2, scale=0.5)
x = model.sample(1000, rng=rng)

fit = uml.gamma.fit(x)
held_fit = uml.gamma.fit(x, fixed={"shape": 2})

fit.params
fit.loglik
fit.status
fit.distribution.cdf(1.0)
```

Select names such as `pdf` versus `density`, `pmf` for discrete families, and
`sample` versus `rvs` together. Provide direct log-density, survival and log-tail
operations so callers need not form inaccurate complements or exponentiate tiny
probabilities. Specify discrete quantile conventions. Keep canonical parameter
names and coordinates and document differences from SciPy and R aliases.

Start with scalar parameters and evaluation over arrays of observations. Define
scalar return types, sample shapes and dtype rules before broadening parameter
broadcasting. Fitting initially takes one univariate sample; later batch fitting
needs an explicit sample axis rather than silently flattening data. Specify copies,
noncontiguous arrays, empty input, missing values, infinities and noninteger data
for discrete families. Avoid silent dropping or rounding that changes the problem.

## Fit results and failure policy

Use a common result interface with named estimates, free and held parameters,
log likelihood, sample size, restrictions, status and available diagnostics.
Preserve family-specific status information behind a common vocabulary. Include
iteration counts and convergence measures only where the core provides them.

Keep invalid input, unsupported holds, numerical failure, nonexistence, attained
boundaries and escape to another model distinguishable. Nonconvergence is not
proof that an MLE does not exist. Do not silently substitute a limit model or
present a failed candidate as successful. Convergence alone does not establish
global optimality.

Choose the default exception policy during the pilot. Notebook calls need clear
explanations; repeated experiments need to retain individual outcomes without
aborting the run. An explicit option such as `on_error="return"` is one candidate.
Failed results must not expose an apparently usable fitted distribution. Keep
programming errors visible rather than swallowing every exception.

Inference follows the [inference plan](2026-10-30-inference-and-dqm.md). Expose
standard errors, covariance matrices, intervals and tests only for supported
procedures, parameter regimes and hypotheses. Do not manufacture covariance from
an optimizer approximation. Fitting and sampling can ship before inference is
ready; unavailable procedures need an explicit availability message.

## Random numbers and repeated experiments

Prefer explicit `numpy.random.Generator` integration. NumPy provides interfaces
for compiled extensions to access its random infrastructure; see
[extending NumPy random](https://numpy.org/doc/stable/reference/random/extending.html).
Prototype the adapter against the core's uniform, normal, gamma, t and Poisson
requirements. Check correctness, state advancement, overhead, threading and
packaging implications before fixing the contract.

Keep RNG state handling in the binding. Avoid Python callbacks per draw, hidden
global core state, reseeding each call or a second stream that ignores the supplied
generator. If a core-owned RNG is initially necessary, expose it explicitly.
Document reproducibility by algorithm, backend and relevant versions; matching R,
SciPy or another platform's bit stream is not a requirement.

Repeated studies should record seeds or stream derivation and associate each
repetition with its stream. Do not share a mutable generator concurrently without
an explicit synchronization contract. Begin with clear serial examples. Retain
one outcome per repetition and report failure counts and denominators alongside
bias or coverage. Separate generation and fitting to allow misspecification.

Add compiled batching or parallel execution when workloads establish a need.
Release the interpreter lock around safe C++ work after defining buffer ownership,
mutation and RNG access rules.

## Packaging and installation

A teaching release should install on supported student systems without requiring
students to compile C++ or install Boost. Select the Python, NumPy, operating-system
and architecture support matrix before release and build tested wheels for it.
Boost headers are build inputs; wheel installation should not discover a local
Boost or R installation.

Build a source archive containing the core headers, metadata-derived inputs and
licenses needed for an isolated build. It must work outside the monorepo without
sibling directories, R or network retrieval of project source. Reuse the standalone
core export rather than maintain a manually synchronized copy. Settle whether
generation happens before packaging or during the build, with reproducible inputs
and declared build dependencies.

Identify the core revision and metadata used by each artifact. Package naming,
PyPI publication and release timing remain later decisions; publication requires
a separate user request.

## Teaching material and browser support

Create small examples for simulation and recovery, held-parameter fitting,
likelihood profiles, bias versus sample size, and boundary or nonexistence behavior.
Add coverage exercises only for procedures with checked assumptions and calibration.
Link canonical mathematical explanations rather than repeating them in docstrings.
Notebook displays should keep restrictions and consequential statuses visible.

JavaScript and WebAssembly widgets are a separate consumer of the core. Python is
not a prerequisite for interactive documentation. Executable Python exercises
through Pyodide are a later possibility requiring their own extension build and
validation; see the [Pyodide build guide](https://pyodide-build.readthedocs.io/en/latest/tutorials/quickstart.html).
Keep that work outside the initial native package.

## Implementation sequence

1. After the foundational work lands, inspect the actual core, metadata and
   artifact layout. Refine this plan against teaching needs, record the source
   revision and coordinate the start on the shared board.
2. Pilot normal, gamma, Poisson, Laplace and GEV. These exercise simple fits,
   iterative fitting, discrete data, nonsmooth likelihoods and support or boundary
   behavior. Include ordinary cases and deliberate failure cases.
3. Refine objects, array rules, holds, statuses and RNG handling using teaching
   examples. Settle consequential API choices before generating the full catalog.
4. Prove packaging early: install a wheel and build an isolated source archive,
   including a representative student platform beyond the developer's workstation.
   Expand the support matrix before a teaching release.
5. Generate adapters and documentation for remaining supported families. Derive
   the capability listing from current source; mark missing operations and holds.
6. Add the simulation and teaching collection, measure representative workloads
   and introduce batching where justified. Reuse validated inference as available.
7. Review API stability, installation, provenance and platforms before a separately
   authorized release.

## Decisions to refine

- Package and import names, method vocabulary and result representation.
- Default exception policy and how simulations retain individual outcomes.
- Array shapes, conversion rules, broadcasting and batch-fitting scope.
- NumPy RNG integration, optional core RNG and reproducibility policy.
- Binding tool, generation boundaries, supported versions and wheel matrix.
- Initial teaching examples and validated inference procedures to expose.

## Trigger to revisit

Resume when the current cleanup, parameter and MLE work, and standalone C++
foundation have landed and the user coordinates a start with the orchestrators.
Python need not wait for every future family or the whole inference catalog.
Begin from a documented baseline with explicit capability gaps.

## Verification

- Install wheels in clean supported environments and build and test the source
  archive without R or access to sibling repository directories.
- Check parameters, holds, restrictions, log likelihood and statuses against core
  fixtures. Golden agreement checks binding fidelity, not independent mathematical
  correctness. Use established per-check tolerances rather than a blanket promise.
- Run independent d/p/q truth checks, tails, endpoints, discrete quantiles and
  appropriate sampling checks. Test RNG state and reproducibility separately.
- Exercise failed fits, unsupported holds and nonexistence through individual
  calls and repeated experiments; preserve counts and reasons.
- Check shapes, dtypes, buffer lifetimes and copies. Run teaching examples against
  installed artifacts rather than importing from the source tree.
- Record revisions, commands, environments, workloads, seeds, tolerances, passes,
  failures, skips and missing coverage. Inference also needs coverage or
  test-calibration checks under its stated assumptions.

## Related plans

- [Package structure and mathematical documentation](2026-10-30-package-and-documentation.md).
- [Inference regularity and derivative support](2026-10-30-inference-and-dqm.md).
- [Frozen distributions](2026-10-30-frozen-distributions.md).
