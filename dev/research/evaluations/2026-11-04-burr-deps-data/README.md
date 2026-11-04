# TASK-116 reproduction and retained evidence

Run from the `burr-deps` worktree root. No dependencies are installed by these
commands. The existing BH installation supplies Boost headers to standalone
C++ compilation; R is used only to locate that include directory.

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
/tmp/umvl-cpu/run nice -n 10 sh dev/research/evaluations/2026-11-04-burr-deps-data/verify.sh
/tmp/umvl-cpu/run nice -n 10 sh dev/research/evaluations/2026-11-04-burr-deps-data/run.sh
```

`verify.sh` compiles `roots.cpp` and writes `roots.txt`, then runs `verify.py`.
The Python check converts original golden inputs and printed outputs to exact
binary64 values before 80-digit score/root/likelihood evaluation. It writes
[verification.json](verification.json). Tolerances and the total-score
coordinates are explicit in the source and [report](../2026-11-04-burr-deps-report.md).
This is independent numerical corroboration; globality comes from the Burr
certificate and the invertible transformations.

`run.sh` compiles `timing.cpp`, reads [inputs.txt](inputs.txt), and writes
[timing.txt](timing.txt). Both inputs are unchanged 500-observation family
goldens; Pareto IV has held threshold zero. `legacy-invburr.inc` retains the
original public InvBurr function from base revision
`54b0d06a3e447f944940fc0dd4b5b87235d2acc2`, renamed `invburr_before`.
The baseline includes the old optional seed solve. The Pareto IV baseline is
its unchanged public function. The candidate timing includes its complete
entrypoint and guards. Each output row is family, repetition, mode (0 legacy,
1 certified), milliseconds and log likelihood. The final family row reports
the two seven-repetition medians and their ratio. One warmup pair is discarded.

[compatibility.txt](compatibility.txt) preserves the rejected Pareto IV
default-switch failures under unchanged native and R golden tolerances.
The final source keeps that public default on the legacy engine and exercises
the certified candidate explicitly in C++.

[validation.json](validation.json) records final run counts, machine,
environment, source/input hashes and complete temporary-log hashes. The five
final logs are under `build/scratch/`: `cpp-dev-final.log`,
`cpp-no-long-double-final.log`, `r-dev-final.log`, `cpp-full.log`, `r-full.log`.
They are not needed by any report link. To re-record after rerunning those
commands under the locks:

```sh
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-04-burr-deps-data/validation.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-04-burr-deps-data/final_checks.py
```

The native transformation identities use the existing 51 binary64 workloads
in Burr's `certificate-fixtures.inc`; that file is hashed in the validation
record. Existing independent oracle and exhaustive held-subset tests run in
the full tiers. Refusal diagnostics, arithmetic overflow and artificial-tie
cases are regression checks, rather than new global existence proofs.
