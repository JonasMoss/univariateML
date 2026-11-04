# Pareto IV switch reproduction

Run from the worktree root; no installations are required.

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
# The standard selector is UMVL_GOLDEN_IDS=pareto4 just goldens.
# It resamples using current defaults (1, 1), unlike the frozen (0.3, 0.7)
# actuar sample, so this correction instead replays the original observations.
git show 0460ccc8:univariateML-cpp/include/cppunivariate/distributions/pareto4/golden.json > build/scratch/pareto4-original.json
/tmp/umvl-cpu/run4 nice -n 10 just regen
/tmp/umvl-cpu/run nice -n 10 Rscript dev/research/evaluations/2026-11-04-pareto4-switch-data/refit.R
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-04-pareto4-switch-data/verify.py
/tmp/umvl-cpu/run4 nice -n 10 just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 just cpp-test-no-long-double
/tmp/umvl-cpu/run4 nice -n 10 just test
/tmp/umvl-cpu/run4 nice -n 10 env UMVL_TEST_TIER=full just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 just test-full
```

The refit changes only three fitted coefficient lines; the computed log
likelihood rounds to the existing value. Data, historical sampler metadata,
seed, holds and fitting controls are preserved. `verify.py` independently
solves analytic total likelihood scores at 80 decimal digits, starting from
the corrected golden. Coordinates are log(shape1), log(shape2), log(scale),
with no objective normalization. Binary64 input values are converted exactly.
Acceptance limits: relative coefficient error < 1e-10, absolute likelihood
error < 1e-8, total-score norm < 1e-7. This is numerical corroboration, not
the global certificate proof. The latter is inherited from Burr.

`verification.json` retains the numerical results. `validation.json` identifies
the tested base and modified sources and hashes the worktree-local logs.
C++ dev/full passed 106/106 and 127/127; no-long-double passed 127/127.
R dev: FAIL 0, WARN 0, SKIP 154, PASS 20551.
R full: FAIL 0, WARN 0, SKIP 0, PASS 43366.
All commands completed locally; long runs used nice scheduling and the locks
pinned BLAS/OpenMP to one thread. Environment: Linux 7.0.0-38 x86-64,
GCC 15.2.0, R 4.6.1, Python 3.14.5, mpmath 1.3.0.
