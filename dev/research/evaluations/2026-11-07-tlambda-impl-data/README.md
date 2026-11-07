# Reproducing TASK-116.27

Run from the worktree root with its existing compiler, R, Boost, doctest, Python/mpmath and Node dependencies. No installation is required. `source-manifest.sha256` identifies the tested numerical sources and fixtures; `validation.json` and `final-run-summary.txt` retain final results and machines. Environment versions are in `environment-*.{json,txt}`. All original large build logs remain under ignored `build/scratch/`.

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
validation_data=dev/research/evaluations/2026-11-07-tlambda-impl-data
/tmp/umvl-cpu/run bash "$validation_data/native-complete.sh"
bash "$validation_data/run-suite.sh" remote-final bash "$validation_data/remote-final.sh"
bash "$validation_data/run-suite.sh" cpp-complete bash "$validation_data/cpp-release.sh"
bash "$validation_data/binding-site-final.sh"
```

Run those commands serially: simultaneous simbox mirrors to the same remote worktree can collide. `run-suite.sh` uses simbox, waits for completion, and falls back locally through run4 on exit 75. The actual final ordinary C++ run used this fallback after a mirror collision; the final no-long-double and R runs used one serial remote mirror. `binding-site-final.sh` reserves slots per computation and redirects temporary browser captures into build/scratch.

`native-complete.sh` builds the focused doctest, independent 100-digit comparisons (`checks.cpp`, `check.py`), frozen fixture comparisons (`golden-check.*`), diagnostic multidimensional box probes (`box-probe.cpp`), and timings (`timing.cpp`). It excludes the slow held-subset golden case from the focused binary only; that case runs in the full C++ suite and the explicit golden checker. JSON files are retained outcomes, not additional fixtures. The box probes are experiments, not production fitters.

Timing CSV columns are n, held-mask (mu=1, sigma=2, lambda=4), elapsed seconds, status, mu, sigma, lambda, log likelihood. `timing-before.csv` was built against clean source 017b9f8e before edits; the identical driver and -O2/C++17 settings produced `timing-after.csv` against the final hashes in `timing-source.sha256`. Workload: deterministic tlambda(0,1,.14) quantiles at (i+.5)/n; tol=1e-7, iterlim=1000. Generation is outside the timed region. The pilot timing is retained as evidence of timing variability, not used for a speedup claim. The independent golden checker uses the stored n=500 fixture and tol=1e-8, iterlim=2000.

All computations used nice +10 and slot-lock thread limits locally, or the simbox wrapper remotely. The production certificate proofs and unresolved regimes are described in the sibling implementation report and family dossier. Independent grids and regression agreement are numerical evidence, not globality proofs.
