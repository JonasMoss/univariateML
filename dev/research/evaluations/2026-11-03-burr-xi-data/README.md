# Burr XII seam-coordinate evidence

The [report](../2026-11-03-burr-xi-report.md) states the outcome and open
certificate requirements. Production fitting and metadata are unchanged.

* [derivation.md](derivation.md) proves the quantitative signed seam bound,
  nuisance containment and a profile-curvature upper bound.
* [study.py](study.py) replays the 39 retained burr-cert samples and 11
  closure workloads. It imports the earlier fixed-k primal/dual solver
  read-only and writes [samples.json](samples.json),
  [diagnostics.csv](diagnostics.csv), [tail-checks.csv](tail-checks.csv),
  [roots.csv](roots.csv) and [summary.json](summary.json).
* [verify.py](verify.py) uses the earlier independent mpmath conditional
  solver at 75 digits on seven workloads; results are in
  [high-precision.csv](high-precision.csv).
* [exact.py](exact.py) uses the earlier exact rational exp/log enclosures
  to prove the signed tail and positive seam curvature for the exact
  negative-slope counterexample. Its assertions use rational endpoints;
  [exact.json](exact.json) contains decimal summaries and exact root brackets.
* [replay.py](replay.py) runs the unchanged C++ fitter on all 50 samples;
  [existing-fitter.csv](existing-fitter.csv) retains its outputs.

No row in the numerical diagnostics or root reconnaissance is a returned
certified estimate. The finite mesh can miss stationary roots. The exact
counterexample input is rational log data; exponentiating it in binary64
produces nearby proxy samples for the numerical/C++ replay, not exact copies.
The numerical equal-face and zero-slope roots are likewise proxies. The
earlier exact certificates, rerun by study.py, justify the existence and
sign-change statements for the underlying continuous sample families.
The `finite_lower` column uses centered, standardized log coordinates with
the constant Jacobian omitted. Gaps and seam derivatives are invariant under
that omission. `existing-fitter.csv` instead records full original-data log
likelihoods; those absolute likelihood columns must not be compared directly.

From the worktree root, using existing dependencies:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-03-burr-xi-data/run.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-xi-data/exact.py
```

The script builds the previous research probe under `build/scratch/` with
the shared ccache and existing BH headers. It writes no previous study
outputs. The CPU runner supplies `nice -n 10` and thread limits. Scratch
executables/logs are disposable; lasting evidence is in this directory.
