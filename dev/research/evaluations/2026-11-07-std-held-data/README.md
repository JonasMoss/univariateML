# std/lst held-subset evidence

Source baseline: 017b9f8e; branch codex/std-held, uncommitted working-tree
changes. `source-manifest.json` records the final implementation hashes.
No golden or truth file is regenerated.

- `derivation.md`: proofs and exact scope; concrete remaining mathematics.
- `check.py`, `checks.json`: independent deterministic 80-digit algebra and
  actual-hold normal-seam checks. Run under `/tmp/umvl-cpu/run` with one
  BLAS/OpenMP thread and `nice -n 10`.
- `held_check.cpp`: standalone assertions for the uniform held-scale shape proof, certified Cauchy endpoint, physical candidate likelihoods, solver stops and extreme-coordinate precision guards.
- `timing.cpp`, `timing-before.txt`, `timing-after.txt`: deterministic
  n=500 workload, one fit per family for every nontrivial held mask. Bits
  hold location, scale and shape respectively. Columns: mask, lst status,
  std status, combined elapsed seconds. Compile C++17 with -O2, the core
  include path and BH's Boost include directory, through the CPU lock.
  The before executable was compiled before implementation edits. The final
  after timings retain the restored numerical free-shape searches;
  `timing-refusal-superseded.txt` records the withdrawn refusal implementation
  and is not a successful-fit speed comparison.
- `validation.json` and compressed logs: final commands, machines, results
  and elapsed suite times. No-long-double runs on the workstation.
  Intermediate failed runs and earlier refusal-route validation are
  identified separately in `development-events.json`.

Timing is a single paired wall-time observation on a shared workstation,
not a controlled statistical speed comparison. It does not establish a
performance guarantee. Header-only source means an executable retains the
implementation with which it was compiled; the before executable was not
recompiled after the change.
