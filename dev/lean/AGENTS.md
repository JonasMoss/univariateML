# AGENTS.md (dev/lean)

Lean 4 formalization of univariateML's likelihood mathematics. The repository `AGENTS.md` still applies.

## Layers

- **A. Outcome semantics**: what an outcome claims (an estimate is a global maximizer, a limit outcome means the
  supremum is approached along a named path and not attained, nonexistence, uniqueness).
- **B. Generic lemmas**: profiling, strict concavity implies uniqueness, compactness plus upper semicontinuity gives
  attainment, escape classification.
- **C. Family mathematics**: one module per family under `UnivariateML/Family/`, stating the dossier's existence,
  limit and uniqueness claims (`univariateML-cpp/include/cppunivariate/distributions/<id>/README.md`).
- **D. Per-input certificates** (later): soundness of the C++ certificates' acceptance conditions.

## Rules

- Toolchain: `/home/jonas/.elan/bin/lake` (not on PATH). Check a file with
  `nice -n 10 /home/jonas/.elan/bin/lake env lean <file>` from `dev/lean/`, through `/tmp/umvl-cpu/run`.
  Do not run `lake update`, `lake exe cache get`, or anything that rebuilds Mathlib; `.lake/packages` is hard-linked
  from another project and must not be modified. Find lemmas by grepping `.lake/packages/mathlib/Mathlib/`.
- Edit only the files your task names, plus `UnivariateML.lean` to import new modules. Do not edit `lakefile.toml`,
  `lake-manifest.json`, `lean-toolchain` or `.lake/`.
- Fidelity: formalize the dossier's statement. If it needs extra hypotheses, is false as written, or you prove a
  weaker or special case, say so in the module docstring under "Deviations" and in your report. Never weaken a
  statement silently; a precisely stated partial result is fine. A mathematical error found in a dossier is a result:
  report it prominently.
- No `sorry`, `admit`, `axiom` or `native_decide` in a finished result. If time runs out, leave remaining goals as
  `sorry` in clearly named lemmas, mark the main theorem `/-- TARGET: ... -/`, and report what is left.
- Names: each file in its own namespace (`UnivariateML.<Family>` or `UnivariateML.<Topic>`).
- Prefer Mathlib definitions (`Real.log`, `Real.exp`, `Filter.Tendsto`, `StrictConcaveOn`, `IsMaxOn`).
- Use the dossier's symbols and parameterization; state the sample as `Fin n → ℝ` with positivity hypotheses.
