# Portability-2: second Windows R check

2026-11-01 · TASK-129

See [Second run](2026-11-01-portability-report.md#second-run) in the canonical
portability report for the fixes, three-platform drift table, final passing
test/check results and open native-Windows confirmation. No goldens changed.

## Addendum: st (2026-11-01, after the skew-t merge)

CI on cpp-core ff0cbb26 failed the R golden check for `st` on all three
platforms because `st` had no entry and fell back to 1e-12. Observed largest
coefficient drift: macOS arm64 1.17e-7 relative (df 4.8315, difference
5.65e-7), Windows 1.0e-9, Linux below that. The new entry is `st = 5e-7`
(four times the macOS drift, rounded up) against stopping tolerance
`reltol = 1e-6`, within the cap 10 * sqrt(1e-6) = 1e-2. The C++ golden check
keeps its own 1e-5 coefficient tolerance (st dossier).
