# Count acceptance evidence

The adjacent report describes the results and mathematical scope.
`source.sha256` records the files actually fingerprinted by the final simbox
run; `source-verification.txt` verifies those bytes against the workstation.
`source-base.txt` records the unchanged Git base. No fixtures were regenerated.

Final logs:

- `native-final.log.gz`: C++ dev/full, no-long-double, R dev/full, and source fingerprints.
- `web-final.log.gz`: workstation WASM build, JS tests and site checks.
- `relations-final.log.gz`: 157 relation checks.
- `validation-summary.json`: final command results and machine assignments.

`cpp-dev-development-failures.log.gz` is an earlier failed development run,
resolved before final validation. `r-target-development.log.gz` is the earlier
979-pass focused R run, before the final budget test was added; it is not a
substitute for the final full suite.

From the worktree root, reproduce the native/R suites with:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/simbox-run bash dev/research/evaluations/2026-11-07-count-accept-data/run-final.sh
result=$?
if [ "$result" -eq 75 ]; then
  /tmp/umvl-cpu/run4 nice -n 10 bash dev/research/evaluations/2026-11-07-count-accept-data/run-final.sh
elif [ "$result" -ne 0 ]; then
  exit "$result"
fi
```

The local commands used for binding/site checks were:

```sh
/tmp/umvl-cpu/run4 nice -n 10 bash -c 'set -e; just js-build; just js-test; just site-check'
/tmp/umvl-cpu/run nice -n 10 just relations
```

Build and run the deterministic golden/refinement checker with:

```sh
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
/tmp/umvl-cpu/run nice -n 10 c++ -O2 -std=c++17 \
  -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  dev/research/evaluations/2026-11-07-count-accept-data/check.cpp \
  -o build/scratch/count-accept-check
/tmp/umvl-cpu/run nice -n 10 build/scratch/count-accept-check
```

`check.txt` is its recorded output. `budget-probe.txt` records the low-budget
CMP prepass work; the final family tests additionally check recovery at a
higher iteration allowance.

`timing.cpp` uses the same deterministic eight observations for all three
fitters and prints seconds per fit over 100 repetitions. Compile it with the
same `-O2 -std=c++17` and library/Boost include flags. The before executable was
built against the base before editing; the after executable was rebuilt against
the final headers. Five outputs from each are in `timing-before.txt` and
`timing-after.txt`; `timing-summary.json` records their medians in microseconds.
The runs used one local CPU slot. `local-environment.txt` records the compiler
and architecture. All timings are wall-clock smoke measurements on a shared
workstation.
