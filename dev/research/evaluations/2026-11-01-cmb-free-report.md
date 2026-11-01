# CMB free size and free dispersion

2026-11-01. Tested source revision `7d6fe5361668c0ed49cf99b6d4198a933cb6bb1d` plus the uncommitted changes in this worktree. No goldens, `limits` fields, or generated d/p/q/r code were changed. No commit was made.

## Constant samples

For free probability and dispersion, a constant sample at k attains likelihood one at size k and probability one (or probability zero when k is zero). With an interior held probability, zero attains likelihood one at size zero. For positive k at most 5,000,000, size 2k is within the supported size range and its unique central mode is k: as dispersion tends to positive infinity, the likelihood tends to one. No finite parameter value with an interior held probability assigns mass one, so the supremum is not attained. Held probability one attains a positive constant at size k; held probability zero is incompatible with it. Positive constants above 5,000,000 with interior held probability remain unimplemented because size 2k exceeds the supported cap. These claims are analytical; C++ and R tests check representative cases.

The two `holds` exceptions were removed because they previously made generated R wrappers reject these subsets before inspecting the sample. The missing general implementation is a sample-dependent limitation, recorded in the dossier rather than as a structural hold exception. The R and JavaScript conformance checks now recognize the explicit sample-dependent stop for the CMB golden sample. The generated R package was rebuilt by the test recipes.

## Open

The general procedure for both free-dispersion subsets is not implemented. The superlevel bound in the CMB dossier does not by itself provide a checked global upper bound for the truncated two-coordinate profile, classify fixed-size dispersion-limit faces, or compare the held-odds face. The existing fixed-size moment solve does not establish those comparisons. Nonconstant samples, including adjacent-support samples, consequently stop as not implemented rather than returning an unchecked estimate. The requested certified-fit, face, tie, budget and seeded brute-force comparison tests for those subsets remain missing.

## Validation

Simbox runs used `/tmp/umvl-cpu/simbox-run`, R 4.6.1, and GCC 13.3.0 unless specified otherwise. Local runs used `/tmp/umvl-cpu/run4`, with one BLAS/OpenMP thread per worker and existing site dependencies. Logs are under ignored `build/scratch/`.

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox, GCC 13.3 | Pass, 105/105 development shards. |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | **Fail**, 125/126: `distribution_bhatt`, golden mu relative discrepancy `2.27338e-7` versus tolerance `5e-8`. CMB passed. The same failure occurred in two full runs. |
| `just cpp-test-no-long-double` | simbox, GCC 15.2 | **Fail**, 125/126: the same Bhattacharjee discrepancy and tolerance; CMB passed. The compiler and existing BH include path were set explicitly in the CMake build configuration. |
| `just test` | simbox | Pass: 19,950 passed, 0 failed, 0 warnings, 151 development-tier skips. |
| `just test-full` | simbox | Pass: 41,418 passed, 0 failed, 0 warnings, 0 skips. |
| `just js-build` | local | Pass. |
| `just js-test` | local | Pass, 400/400. |
| `just site-check` | local | Pass. |

An earlier R development run failed because `holds` caused the generated wrapper to reject the constant-sample fit before the adapter. The metadata and conformance changes above fixed that failure, and both R tiers were rerun successfully. An earlier R full run was interrupted when the disk filled and is not counted. The Bhattacharjee failure is outside the CMB files touched by this job; no unrelated golden or tolerance was modified. The required no-long-double suite is therefore not green on simbox, despite the CMB shard passing.
