# Fit snapshot of master `7e3c9779`: rendered baselines (2026-11-14)

Recorded after step 5a (every fit rendered from its shared outcome), the
fixes of TASK-214.3, the norm `logLik` decision and the strict layer. The
probe now records the rendered result (classed stop kinds, `certified` and
`unique` flags, comparison outcomes). Earlier baselines:
[2026-11-13](2026-11-13-fit-snapshot-report.md). The differences between the
two are classified in the step-5a reports
([render](2026-11-13-render-report.md), [render-2](2026-11-14-render-2-report.md))
and in the fix-nonfinite report.

Files: [native](2026-11-14-fit-snapshot-data/native.jsonl.gz),
[double-only](2026-11-14-fit-snapshot-data/double.jsonl.gz),
[routes, native](2026-11-14-fit-snapshot-data/routes-native.jsonl.gz),
[routes, double-only](2026-11-14-fit-snapshot-data/routes-double.jsonl.gz),
each with its `provenance.json`; 11,820 family and 1,934 route cases per
mode. All 85 families and 16 routes report an outcome on every case with no
disagreement.

`just strict` on these files: every failure is on the known list after two
edits made with this record: 22 entries that now pass were removed (binom,
gompertz, lgamma and cauchy invalid-data cases), and the double-only EMG
archive case 1311, already listed as reaching the CPU guard, was added for
the seam check too (load-dependent). The list has 318 entries. Suites on
the same commit: C++ 110/110, double-only 154/154, R dev 36,209, R full
65,348, JS 583/583, relations, site check.
