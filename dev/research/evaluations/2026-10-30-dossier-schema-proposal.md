# Dossier schema proposal

This is a proposal for review, not a change to the current schema. The
pilot dossiers are [exp](../../../univariateML-cpp/include/cppunivariate/distributions/exp/README.md),
[gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/README.md)
and [gb2](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md).
The current generator includes the `Details` section in R help; it does not
include the separate `Decision history` section. That separation keeps the
R-facing description concise, but means the history is available only in the
source dossier until the generator is deliberately extended.

## Proposed sections

- **Details**: only facts useful to a reader that cannot be inferred safely
  from the code: existence conditions, supported holds, limitations and
  alternatives on non-existence. Keep front matter as the machine-readable
  source for domains, structural hold exceptions and declared limits.
- **Fitting method** (or a compact paragraph within Details): explain the
  method, the strength of its result, relevant face comparisons, failure
  behavior, and links to evidence. Avoid repeating the density and
  parameter table.
- **Decision history**: dated material choices with commit and report
  links, including superseded approaches when their failure explains the
  present design. State unknown rationale and what would resolve it.

Depth varies with the problem. For a closed form, give the condition,
boundary and a check. For a scalar or profiled solve, give the score or
objective, reduction, bracket and any monotonicity/uniqueness argument,
stopping and failure. For an involved fit, add initialization, competing
basins and faces, conditioning, fallback, cost and rejected alternatives;
distinguish a numerical search from a certificate. Keep shared optimizer and
tail contracts in shared notes under the standalone C++ component; retain
family-specific existence and limit arguments in the family dossier.

For evidence, link source tests, derivations and run reports; label goldens,
independent numerical comparisons and proofs separately. Keep historic
reports accessible. A green generated-doc test confirms rendering and
packaging, not the mathematical correctness of the prose. The pilot's
separate history headings are not propagated by the current generator; a
future schema decision should decide explicitly whether they belong in R help.
