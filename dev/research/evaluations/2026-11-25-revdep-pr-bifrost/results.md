# Bifrost test results

All runs used the workstation, R 4.6.1 and testthat 3.3.2. Bifrost 0.2.3 was
cloned at `6df4b209e7014b815db2b9455292469df6ccc4e4` and installed before and
after the patch with `R CMD INSTALL --install-tests` into the corresponding
`build/revdep-pr/lib-old` or `lib-new`. Dependency search used the matching
read-only univariateML library first and the shared `lib-deps` next. The
upcoming build still identifies itself as 1.5.0 in DESCRIPTION. The
reverse-dependency roadmap identifies its source as `3fe10413`.

| Tests | univariateML | Original | Patched |
| --- | --- | --- | --- |
| Installed `test-shift-distributions.R` | CRAN 1.5.0 | Pass (4.17 s) | Pass (4.49 s) |
| Installed `test-shift-distributions.R` | Upcoming | One error (5.15 s) | Pass (4.10 s) |
| Full installed testthat suite | CRAN 1.5.0 | Pass (239.02 s) | Pass (218.06 s) |
| Full installed testthat suite | Upcoming | One error (213.54 s) | Pass (202.03 s) |
| Source `test-shift-distributions.R` with empirical fixtures | Upcoming | One error and one assertion failure (117.19 s) | One assertion failure in the full source run |
| Full source testthat suite with empirical fixtures | CRAN 1.5.0 | — | Pass (418.24 s) |
| Full source testthat suite with empirical fixtures | Upcoming | — | One assertion failure (570.49 s) |

The installed runs used `NOT_CRAN=false` and called
`testthat::test_check("bifrost", reporter = "summary")` from the installed tests
directory. The targeted calls added `filter = "^shift-distributions$"`. The
targeted runs had two empirical-fixture skips each. The full installed runs
had 139 skips each and zero warnings. The patched spelling runner completed
with each build in CRAN mode with `skip_on_cran = TRUE`.

The expanded source runs used `NOT_CRAN=true`, the same installed package, the
clone's `tests/` directory, and `BIFROST_ARTIFACT_DIR` pointing to the clone's
`data-remote/`. Both full source runs skipped the absent paired-tuning inputs
and the optional ComplexHeatmap test. Each emitted 28 warnings when worker
tests requested two R processes on the one pinned core. The original-source
replay restored the original test file for the run. The installed production
code is identical for both revisions.

The original error was the constant-data rate-fit fallback at line 1125.
The additional source-only failure was the avian-skeleton Gumbel bootstrap
assertion at original line 1893. It reproduced with and without the patch.
With 1,000 replicates and seed 2026, the unrounded upper bound was
`0.1421887609` on CRAN 1.5.0 and `0.1442560083` on the upcoming build.
Using `extraDistr::rgumbel` to generate replicates and the upcoming
`mlgumbel` to refit them gave `0.1421887605`. At fixed parameters the
CRAN sampler matched `extraDistr::rgumbel` for the same seed, while the
upcoming sampler generated different draws.

Raw workstation logs are in `build/scratch/bifrost-*.log`.
