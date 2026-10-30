# P5 parameter-rules documentation (2026-10-30)

- `README_SCHEMA.md`: parameter kinds and defaults, simulation and advice,
  structural holds, fitting restrictions, conditional limits, support and
  endpoint expressions, and translation/argument mappings; removed P1/P3/P4
  statements superseded by implementation. Conformance is a golden-data
  check, not a global existence proof.
- `CLAUDE.md`: new-distribution guidance and the half-normal reference mapping.
- `bindings/r/NEWS.md`: consolidated the parameter-rules changes, preserving
  the existing hold-count sentence; updated the shape-bound descriptions.
- `dev/research/ml-behaviour.tex`: R5 mechanisms and scope, P2b conditional searches,
  and corrections to superseded status statements. Analytic results and
  numerical comparisons are distinguished.
- `dev/research/evaluations/2026-10-29-p0b.md`: dated corrections with links to both
  P2b reports, covering sstd, mean versus mode, CMB limits and reflection.
- `tlambda/README.md`: finite support `mu +/- sigma/lambda` for positive
  lambda, infinite otherwise, with the appropriate inside endpoint densities.

## Support inventory

Of the 27 inventory entries, 22 in-scope entries were already corrected in the
base: binom, dunif, fpareto, frechet, genbeta, gev, gpd, hnorm, nsbeta,
pareto, pareto2, pareto3, pareto4, power, tbinom, tpois, triang, unif, zib,
zipf, zmbinom and ztbinom. Tukey lambda was corrected here. The four excluded
families bbinom, cmb, hyper and nhyper were not edited; their declarations
should be checked by their owners against the inventory.

## Validation and unresolved work

- `just note-build ml-behaviour` passed locally through the CPU slot. Two
  additional LaTeX passes showed no undefined references; no em-dashes occur.
  The artifact viewer rejected the worktree path as outside its readable roots.
- `just test` on simbox: 30,743 passed, zero failures, warnings or skips.
- `just quality` on simbox: 97/98 passed; tlambda failed before checking its
  fit because the harness passes the R coefficient `lambda` unescaped to
  SymPy, where it is a Python keyword. The support expressions are valid R.
  The harness needs to substitute safe symbolic names, as `dev/validation/quality/density.py`
  already does, before parsing support bounds; this is outside the assigned
  files and requires authorization. TASK-78 owns d/p/q/r and codegen, so no
  changes were made there.
- P2b skew global comparisons and truncated-binomial free-size searches remain
  numerical rather than globally certified. P2b's sstd collapse equality and
  SGED exact uniform endpoint rate remain unresolved; see the phase reports.
