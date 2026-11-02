# Skew-normal shape search checks

`check_bounds.py` uses seed 116 to spot-check feasible likelihoods against
finite-interval and tail upper bounds. It is a numerical check, not a proof.

`compare.cpp` generates 600 samples of size 100 with `std::mt19937` seed
20261102, using shapes -100, -20, -5, -1, -0.001, 0, 0.001, 1, 5, 20,
100 and 1e8 in rotation. Compile once normally and once with `-DOLD_SN`,
after placing the original `mle.hpp` at `build/scratch/sn_old.hpp` and
adjusting its two relative includes to the corresponding `cppunivariate/`
include paths. Run each binary into `build/scratch/sn_new.txt` and
`build/scratch/sn_old.txt`, respectively; then run `summarize.py` on those
files in old/new order. The driver depends on the standard library's normal
distribution implementation, so exact samples can vary across implementations.
The output files are regenerable and are not kept here.

For the binding replay, compile `compare.cpp` with both `-DOLD_SN` and
`-DEMIT_DATA` to `build/scratch/sn_emit_old`, then run `compare-r.R` from
the worktree root after assembling the generated package. It streams each
old fit and its observations to R, checks finite likelihoods within 1e-6,
and checks both half-normal error directions. No sample table is retained.

`benchmark.cpp` reads the golden data as one observation per line from
standard input. Compile the original and new headers as for `compare.cpp`,
with and without `-DOLD_SN`. The timed region is one core fit; process startup,
compilation and input parsing are outside it. Extract `data` from the golden
JSON to a scratch file and alternate the two binaries on that input.
