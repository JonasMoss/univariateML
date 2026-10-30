# TASK-103: confint bootstrap repetitions

- Source: master `5f11fb9e878219ac47a9a372e985eb69da6198e3` plus the uncommitted TASK-103 changes; 2026-10-30.
- Cause: `confint.univariateML()` forwarded `Nreps` unchanged; `bootstrapml()` expects `reps`, so `Nreps` reached the reducer through `...` while 1,000 draws were used.
- Fix: forward `reps = reps`, with a named-only `reps = Nreps` alias. Document that explicit `reps` takes precedence over `Nreps`; retain the 1,000-draw default.
- Added one bug-fix NEWS entry. No fitter, golden, density, or numerical algorithm changes.
- Checked all repository `bootstrapml()` callers: the confint wrapper, bootstrap examples, overview vignette, shared testthat tests, and per-distribution `test.R` files. Others use `reps` or the default; no other mismatches or TASK-78 changes needed.
- Regression: normal fit on `c(-2, -1, 0, 1, 3)`, seed 103; compare percentile intervals and exact RNG state against `bootstrapml(reps = 20)`, inspect the 2-by-20 draws, and check alias precedence with seven draws. Interval comparisons use testthat's default tolerance; RNG and draw-count checks are exact.
- Local preparation: `TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run just regen` passed (R 4.6.1, GCC 15.2.0, C++17).
- Local regression command: `TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run Rscript build/scratch/test-confint-nreps.R bindings/r/R/code_generics.R`; loads the assembled package, sources the current generic, and runs `test_confint_nreps.R`.
- Local result: 2 test blocks, 5 assertions passed; 0 failures, warnings, or skips.
- Same check sourcing the saved original generic: 3 assertion failures, 2 passes, 0 warnings/skips; explicitly observed 1,000 rather than 20 draws.
- Simbox: `/tmp/umvl-codex/simbox-run.sh just test` passed, exit 0; 37,718 assertions passed, 0 failures/warnings/skips; test duration 622.3 s (R 4.6.1, GCC 15.2.0).
- Simbox includes all five new assertions; its RNG assertion used `expect_identical`, subsequently expressed locally as `expect_true(identical(...))` to avoid testthat's integer-overflow formatter on the intentional pre-fix failure.
- Builds/tests ran under the shared CPU locks, with one-thread settings and nice 10. Logs and the before/after runner are in ignored `build/scratch/`.
- `git diff --check` passed. No commits, board edits, or publication performed.
