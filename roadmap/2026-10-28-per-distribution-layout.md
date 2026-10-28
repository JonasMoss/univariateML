# One folder per distribution

**Status:** landed 2026-10-28. Decided option A with `README.md` front
matter (D2); piloted on lomax + paralogis (dd3ace4), the other 40
migrated in four Codex batches (merges 7a73cad, 5acd604, a748203,
ac23f6a), legacy paths removed (da8eff1). Harness rework landed:
README/golden checks replace Cost validation; unused
Costs removed except in the protected kumar/std folders. Measured
compile cost: a distribution adapter compiles in ~1.3 s (the old
all-in-one `ml_exports.cpp` took ~6 s), so per-distribution translation
units are cheap.

## Problem

Everything about one distribution is spread over 13+ places. For lomax
today:

| What | Where |
|---|---|
| fitter, Cost | `cppunivariate/include/cppunivariate/distributions/lomax/mle.hpp` |
| d/p/q/r | `.../lomax/dpqr.hpp` |
| metadata | `.../lomax/metadata.hpp` (regex-parsed by R) |
| prose | `.../lomax/dossier.md` |
| R refs | `univariateML/metadata/lomax.json` |
| Rcpp adapter | `univariateML/src/ml_exports.cpp` (all 41 in one file) |
| R tests | `univariateML/tests/testthat/test_mllomax.R` |
| C++ tests | `cppunivariate/tests/test_estimators.cpp`, `test_hessians.cpp`, `test_validation.cpp`, `test_sampling.cpp` (all 42 in each) |
| aggregator | `cppunivariate/include/cppunivariate/cppunivariate.hpp` |
| sympy spec | `quality/math/lomax.py` |
| harness output | `quality/derived/lomax/` (6 committed generated files) |
| golden | `quality/golden/lomax.json` |

Parameter facts exist three times (`metadata.hpp`, `quality/math/<id>.py`,
the JSON sidecar), and 84 of the 87 parameter domains in `metadata.hpp` are
placeholders.

## Proposal

```
cppunivariate/include/cppunivariate/distributions/lomax/
  README.md      the one declarative file: YAML front matter + prose
  mle.hpp        fitter; a Cost struct only if the fitter optimises it
  dpqr.hpp       d/p/q/r
  test.cpp       doctest: golden, status cases, limit-model data
  golden.json    golden data + estimates
  adapter.cpp    Rcpp adapter (mllomax_)
  test.R         testthat: oracle fit, messages
  spec.py        optional, only when log_density does not fit in a string
```

Shared code stays shared: `core/`, `special/`, `tests/` helpers (doctest,
JSON loader), the R package's generic code (`univariateML/R/`), the harness
engine (`quality/harness/`). Nothing shared lists distributions by hand;
everything globs `distributions/*/`.

### The declarative file (`README.md`)

```markdown
---
id: lomax
name: Lomax
support: {type: real, lower: 0, upper: inf}
params:
  - {name: lambda, role: rate,  domain: "(0, inf)", default: 1}
  - {name: kappa,  role: shape, domain: "(0, inf)", default: 2}
r:
  density: extraDistr::dlomax
  sampler: extraDistr::rlomax
log_density: log(lambda) + log(kappa) - (kappa + 1)*log(1 + lambda*x)
limits:
  - model: exp
    when: kappa -> inf, lambda*kappa fixed
    detect: mean(x^2) < 2*mean(x)^2
    status: MeanVarianceCondition
references:
  - Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in
    Economics and Actuarial Sciences. Wiley.
---

The Lomax (Pareto type II) distribution is a heavy-tailed distribution on
the positive half-line.
```

- Front matter replaces `metadata.hpp`, the JSON sidecar, most of
  `quality/math/<id>.py` and the dossier schema's structured parts. The
  body is prose only (what cannot be read off the code).
- `limits` makes the Hellinger-closure work in `notes/ml-behaviour.tex`
  machine-readable: the R docs generate the edge-case paragraph from it, and
  the tests generate limit-model data and check the declared status fires.
  Families that are closed have no `limits` entry and need almost no prose.
- `README.md` renders when browsing the folder on GitHub/Gitea.

### What consumes it

- **R assembly** (one script replacing `codegen_metadata.R`,
  `codegen_wrappers.R`, `parse_cpp_metadata.R`): writes one
  `R/ml<id>.R` per distribution into the gitignored `package/` (wrapper,
  roxygen from front matter + body, metadata entry), copies
  `adapter.cpp` to `src/<id>.cpp` and `test.R` to
  `tests/testthat/test-ml<id>.R`, syncs headers (`*.hpp` only).
- **CMake**: globs `distributions/*/test.cpp` into one doctest executable;
  install rule ships `*.hpp` only.
- **Harness**: reads `log_density` (or `spec.py`), `params`, `limits`.

### Harness, reworked (still an experiment)

Checks the real fitter, not a test-only Cost:

1. At the fitted estimate (from `golden.json`): symbolic
   score near zero (unless the MLE is on a boundary), Hessian negative
   definite, reported logLik equal to the symbolic sum.
2. Slow oracle: multi-start optimiser on the symbolic density; flag when it
   beats the fitter (this is what catches silent escapes).
3. Limits: list README entries; folder tests own status checks.

Outputs go to the build directory; nothing generated is committed.

## Deleted

- `metadata.hpp` x42, `univariateML/metadata/*.json` x42, `SCHEMA.md`,
  `parse_cpp_metadata.R`, `DOSSIER_SCHEMA.md`
  (replaced by a short schema for the front matter).
- `quality/math/`, `quality/derived/`, old derive/grid/validate/spec modules.
- 33 unused Cost structs outside protected `kumar/` and `std/` folders;
  their dead derivative tests, `test_validation.cpp`, `eval_io.hpp`.
- Unreferenced `core/newton_md.hpp`, `core/cholesky.hpp`,
  `core/concept_traits.hpp`, `test_newton_md.cpp`, and associated targets.
- The aggregator and existing R codegen scripts remain in use.

## Options and trade-offs

1. **Folder location.**
   - (A, recommended) Inside the include tree, as above. No include-path
     change; one folder. Cost: the include tree holds non-headers, so the two
     consumers (CMake install, R header sync) must filter to `*.hpp`.
   - (B) Top-level `distributions/<id>/` with everything, headers too, and
     the include tree assembled at build time. Cleaner include tree, but every
     C++ consumer needs a build step and the relative includes change.
   - (C) Headers in the include tree, everything else in a top-level
     `distributions/<id>/`. Clean include tree, but two folders per
     distribution, which is what we are trying to get away from.
2. **File format.** YAML front matter (R `yaml` is installed; Python needs
   `pyyaml`, a dev-only dependency) vs TOML (Python has `tomllib`; R would
   need `RcppTOML`) vs keeping `metadata.hpp` canonical (no new parser, but
   the regex parser and the three copies of parameter facts stay).
3. **File name.** `README.md` (renders in the browser) vs `<id>.md` vs
   `distribution.md`.
4. **R package.** Keep it generated and gitignored (recommended: assembly
   is needed anyway once adapters and tests live per distribution) vs
   commit the assembled package (installable from git, reviewable doc diffs;
   duplicates the headers in `inst/include`).
5. **Compile time.** One adapter file per distribution means 42 translation
   units that each parse Boost, instead of one. Slower serial R builds;
   parallel builds are faster. Measure in the pilot.
6. **Analytic Hessians.** Deleting the unused Cost structs loses
   hand-derived Hessians that analytic standard errors could use later. They
   stay in git history, and sympy can regenerate them.

## Pilot

lomax (1-D Newton, has a limit) and paralogis (BFGS, uses its Cost, closed
family). std is the natural BFGS pilot but is being changed by the
escape-detection job; it joins the sweep afterwards.

Verification: both pilots pass `just cpp-test` and `just test` with
unchanged goldens; the generated `?mllomax` page has the limit paragraph;
adding a third distribution needs no edit outside its folder.
