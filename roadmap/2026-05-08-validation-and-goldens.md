# Validation and goldens (deferred)

**Status:** deferred — extension of the existing test strategy.

## The idea

Keep golden regression tests, but make them one layer in a broader validation
system.

## Why it matters

Goldens detect accidental numerical drift. They do not prove an estimator is
correct. A wrong implementation can reproduce its own goldens forever.

The project also needs tests that ask whether the score is close to zero, the
Hessian is plausible, simulations recover parameters, and edge cases fail in
documented ways.

## Design when revisited

Use several validation layers:

- golden regression for accidental behavior changes
- reference comparisons for `d/p/q/r` and estimators where reliable references
  exist
- score residual checks at regular MLEs
- analytical Hessian versus finite-difference Hessian checks
- closed-form Fisher information checks where known
- simulation recovery across parameter regimes
- edge-case tests for boundaries, degeneracies, and invalid data
- cross-language consistency once Python exists

Current assets already include golden JSON fixtures, R golden regression tests,
standalone C++ estimator tests, C++ Hessian tests, and reference-package
`d/p/q/r` checks.

## Trigger to revisit

Take this off the shelf before changing the golden generation process, adding a
quality dashboard, or promoting any distribution beyond `ported`.

## Verification when revisited

- Existing tests are categorized by validation layer.
- Dossier pilots list passed and missing validation layers.
- Golden regeneration has an explicit diff-review step.
