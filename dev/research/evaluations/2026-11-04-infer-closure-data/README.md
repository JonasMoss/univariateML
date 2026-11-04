# TASK-102.12 evidence

The [report](../2026-11-04-infer-closure-report.md) records the tested revision,
results, final regression runs, interpretation and open calibration work.

| File | Purpose |
|---|---|
| [gaussian-experiments.md](gaussian-experiments.md) | Scores, nuisance information, DQM, integer mesh, corner LAN, Gaussian confidence inversion |
| [bb-localization.md](bb-localization.md) | Empirical-KL localization and fourth-order expansion for the unrestricted BB mathematical LR |
| [pareto-experiment.md](pareto-experiment.md) | Burr Hellinger speed, logistic-intensity PPP, known/fitted endpoint tests |
| [burr-localization.md](burr-localization.md) | Density-envelope consistency and order-statistic localization for the unrestricted Burr mathematical LR |
| [classification.md](classification.md) | Invariance and a counterexample to identifying exact face-selection behavior from a first local experiment |
| [literature.md](literature.md) | Primary-source metadata, inspected locations, search protocol and access limits |
| [symbolic.py](symbolic.py), [symbolic.json](symbolic.json) | Independent exact fourth-order BB algebra and normalization |
| [check.py](check.py), [results.json](results.json) | Numerical information, likelihood approximation, score calibration, Gaussian confidence experiment and PPP diagnostics |
| [pareto-witness.csv](pareto-witness.csv) | All 1,000 log observations for the feasible Burr/Pareto likelihood comparison |
| [run.sh](run.sh) | Sequential one-slot reproducer; contains no fit/search optimizer |
| [validate.py](validate.py), [validation.json](validation.json) | Source/whitespace/JSON/local-link checks, final run markers and evidence hashes |

Run from the worktree root, with the existing Python environment:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-04-infer-closure-data/run.sh
```

Seed 1021220261104 drives the research simulation; the Gaussian confidence
illustration uses seed+1 independently so it cannot change the retained
sample. Histograms represent large iid count samples exactly, apart from
the explicitly bounded Poisson tail beyond count 60. There is one recorded
support failure in the smallest BB comparison, reported rather than replaced.
No production estimator or likelihood-ratio optimizer is called. Grid
quantiles are diagnostic values on a specified finite grid with a separately
bounded PPP truncation error; they are **not** quantiles of a certified
global LR. The finite-data witness evaluates the full density at a feasible
point and is independently replayed at 55 digits; this is numerical agreement,
not an outward-rounded arithmetic enclosure.
