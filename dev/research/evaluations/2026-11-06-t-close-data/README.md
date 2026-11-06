# TASK-140.1 evidence

The [report](../2026-11-06-t-close-report.md) describes final outcomes and
validation. [derivation.md](derivation.md) records the mathematical bounds,
search structure, practical arithmetic scope, fallbacks and work limits.

Run from the worktree root. Set worktree temporary paths before local commands:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-06-t-close-data/native.sh
```

This compiles the native replay and independent arithmetic harness, replays
the frozen 31 samples plus two multimodal examples and the 4000-point quantile
sample, differentiates an independent 80-digit objective, solves the quantile
root independently and checks retained root/endpoint boxes and golden widths.
It writes replay.json, arithmetic.json, quantile-root.json and independent.json.
Original frozen inputs and their generation seed 14020261104 are in
[the retained input fixture](../2026-11-04-t-cert-data/inputs.json).
All new arithmetic boxes and corners are deterministic.

For paired baseline/final timings:

```sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-06-t-close-data/benchmark.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-t-close-data/benchmark.py --final
```

The second command uses the final executable built by native.sh and retains
baseline measurements from benchmark.json. benchmark-final.json is the final
comparison. search-history.json preserves the earlier tail-bound bottleneck
and corrected harness attempts; benchmark.json also retains the exploratory
comparison before the final neighborhood proposal ordering.

The final suites were run with these foreground launchers:

```sh
/tmp/umvl-cpu/simbox-run sh dev/research/evaluations/2026-11-06-t-close-data/final-simbox.sh
sh dev/research/evaluations/2026-11-06-t-close-data/final-local.sh
```

final-local.sh acquires and releases the workstation four-slot reservation for
each command; do not wrap it in another reservation. final-simbox.sh sets four
workers and worktree temporary paths. Exit 75 from simbox-run requires rerunning
locally through run4; no fallback was needed in this job. targeted.sh supplies
a smaller two-family C++ iteration check.

environment.py reads the mirror and checks source identities, recording machine
and package versions. validation.py freezes final command results and hashes
after every suite finishes; raw logs remain under build/scratch. validation.json
is the durable final record. final-checks.json records document/source checks
and the artifact-pane display attempt.
