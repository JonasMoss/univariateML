# Fit-speed trial patches (2026-10-31)

These are preserved experiments, **not patches to apply together**. The
[report](../../research/evaluations/2026-10-31-fit-speed-report.md) records their
measurements and rejection reasons. All diffs except the GB2 and lst caches are against
source revision 68428d91bb027e99dbbe2972b4e17c78807769f1; the GB2 and lst cache diffs are
against the final fitter with member-seed subsampling / normalizer hoisting.

- `fit-speed-warm-tlambda.patch`: sorted, neighboring-root warm inversion;
  checked residual with fallback to the original inversion after six steps.
- `fit-speed-double-erfc.patch`: double erfc calls inside the extended central
  Bhattacharjee primitive; coordinate arithmetic remains extended precision.
- `fit-speed-bhatt-cache.patch`: bounded exact full-profile cache.
- `fit-speed-emg-cache.patch`: bounded exact full-profile cache and array score.
- `fit-speed-emg-hoist.patch`: likelihood invariants hoisted while retaining
  per-observation double rounding, plus array score.
- `fit-speed-lst-cache.patch`: exact refinement-point memoization beyond the
  existing grid cache.
- `fit-speed-gb2-cache.patch`: bounded exact shape-profile cache across starts,
  preserving the requested evaluation count.

Use isolated copies under `build/scratch/` for reproductions. Compile the paired
[driver](../fit-speed-comparison.cpp) with `-O2 -std=c++17`, the selected include
tree and the installed BH include path. Never apply these experiments to the
settled `dpqr.hpp` or fitter sources as a group.
