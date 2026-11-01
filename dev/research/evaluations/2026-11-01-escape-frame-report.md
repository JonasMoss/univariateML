# Escape frame and firmness grades — 2026-11-01

Tested source: `90ec2153ea2a4272bb47259bb6091d2602a176e6` plus the uncommitted changes in this worktree. No fitting or density code or goldens were changed.

## Results

The [research note](../ml-behaviour.tex) defines the joint upper-semicontinuous likelihood envelope at the sample and proves that, for a dense family in a compact metrizable closure, its maximum equals the original likelihood supremum. It states the subsequence conclusion and distinguishes compactness of the likelihood closure from tightness of laws. Identifying the envelope with the likelihood of a named limit law is an additional, sample-specific compatibility condition: separate pointwise limsups can have incompatible paths, and convergence of laws does not determine density values at isolated observations. Tightness of maximizing sequences is assumed, not inferred from the other hypotheses. The 98-family inventory marks the full set of hypotheses as assumed, with individual path evidence left to the dossiers and earlier tables.

The note gives the Hellinger and total variation inequalities, distinguishes weak continuous point-mass collapse from discrete Hellinger limits, and states the KL projection and Wald consistency interpretation with its additional assumptions. The [limits article](../../../univariateML-cpp/notes/limits.md) uses the same frame and links to the proof.

The dossier schema accepts optional `firmness: proved|argued|numerical|conjecture`; matching records were seeded from the historical argument and probe classifications. Code and tests alone are numerical evidence, not a proof; unassessed records retain no grade. `numerical: true` remains a separate detection-algorithm flag, preserving the existing cautious R help. A proved or argued boundary can have numerical detection. The R loader and site generator validate both fields; the family existence display, lab page and limits catalogue render the grade.

Inverse Gaussian declares its Lévy boundary as a `closure_point` relation, with the fixed-positive-shape and finite-positive-sample scope of the non-reachability argument. The finite sample mean uniquely maximizes the likelihood in the mean. Relations validation checks this record's structure only; it does not independently prove its limiting law or non-reachability. External closure points are excluded from the graph of package-family edges.

## Validation

Local commands used the worktree `build/tmp` for `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` and the shared CPU locks, which set one BLAS/OpenMP thread and `nice -n 10`. Site dependencies were copied from the specified local checkout; no packages were installed. This is the existing Linux workstation R, C++, Python, Node, browser and LaTeX environment. The relation checker uses seed `20261031`, 24 interior samples and relative tolerance `2e-9` (`2e-8` for specified constructions).

- `/tmp/umvl-cpu/run python3 -m unittest dev.validation.quality.test_relations`: 41 tests passed. The loader integration was rerun with the added valid and invalid firmness, numerical-flag and closure-point probes: 1 test passed.
- `/tmp/umvl-cpu/run4 just test`: **19,865 passed, 151 skipped, 0 failed, 0 warnings**.
- `/tmp/umvl-cpu/run4 just test-full`: **41,311 passed, 1 skipped, 0 failed, 0 warnings**. The skip was the unavailable `sn` package.
- `/tmp/umvl-cpu/run4 just cpp-test`: **105/105** development shards passed.
- `/tmp/umvl-cpu/run4 just relations`: **156 records, 0 failed**, including 155 density edges and one structurally checked closure-point record. Numerical agreement is not proof.
- `/tmp/umvl-cpu/run4 just site-check`: passed, including visible firmness, numerical-detection qualifier, closure-point and dossier-catalogue checks.
- `/tmp/umvl-cpu/run4 just note-build ml-behaviour`: two LaTeX passes succeeded; extracted text contains no `??`. The PDF is an untracked build artifact.
- `git diff --check`: passed.

The artifact-pane `show` command was attempted for the PDF and this report, but returned HTTP 403 because this worktree is outside the viewer's configured readable roots. No file was copied into another checkout or outside the worktree.

## Open evidence

- Compactness, likelihood-envelope compatibility and tightness have not been verified for each family and held subset. The inventory is explicitly conditional; it does not prove closure completeness or exclude all defective limits. Individual topology, endpoint and path analyses are needed.
- Many limit records have no corresponding grade in the historical tables and remain unassessed. A record-by-record mathematical and literature audit is needed before assigning grades; historical code, arguments and probes do not automatically certify global optimization or every held subset.
- The relation check validates the Lévy record's structure, not its convergence or reachability claims. The likelihood comparison is stated in the inverse-Gaussian dossier.
- The viewer configuration must include this worktree before the PDF or report can be displayed in the artifact pane.
