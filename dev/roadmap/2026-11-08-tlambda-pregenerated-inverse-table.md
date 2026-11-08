# Pre-generated constants for the tlambda inverse seed table

Decided 2026-11-08 (Jonas): the fitter's inverse-quantile seed table over
(eta, lambda), lambda in [-1,1], is built at first use from the solver
itself (function-local static, about 1.6 ms once per process). Generated
literal constants are acceptable and may replace it later.

When to switch: if first-use cost shows up in WASM or R startup profiles,
or when `dtlambda` adopts the same seed and scalar calls make the build
cost visible.

What a switch needs: a generator script under `dev/tools/` that calls the
same solver, a header of literals in the tlambda dossier folder, a test
that rebuilds the table and compares it with the literals, and a check
that the stored values seed identically on the no-long-double and WASM
builds. Design and measurements:
[speed report](../research/evaluations/2026-11-08-tlambda-speed-report.md#one-static-table-for-the-fitted-branch).
