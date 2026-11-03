# Burr XII closure: retained research evidence

The [report](../2026-11-03-burr-closure-report.md) contains the proofs and
explains the distinction between weak limits and endpoint density traces.
No production fitter, binding, dossier, golden, or test is changed.

- [check.py](check.py): deterministic seed 138; 75-digit mpmath conditional
  solves and limit-expansion checks. It writes [summary.json](summary.json),
  [checks.csv](checks.csv) and [profiles.csv](profiles.csv).
- [cert.py](cert.py): exact rational exponential/logarithm enclosures for the
  finite witness, the equal-face crossing interval, the positive-slope
  nonexistence example, the zero-slope/positive-second-order example, and
  the conservative `k >= 10^100` exclusion for the first witness, and
  the uniform `k >= 10^106` exclusion used in the finite/Pareto tie proof.
  Assertions use rational endpoints; JSON displays
  are rounded floating-point summaries of those endpoints.
- [plot.py](plot.py): plots the retained profiles as [profiles.png](profiles.png).
- [render.py](render.py): checks local report links and generates the
  script-free [MathML preview](preview.html).

From the worktree root, with temporary directories under `build/tmp`:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-closure-data/check.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-closure-data/plot.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-closure-data/render.py
```

Dependencies were already installed. The plot grid is not a global search:
the mathematical existence claims use closure and the exact finite witnesses.
No last grid point, capped search, or optimizer output is exposed as an MLE.
`__pycache__` is disposable ignored output.
