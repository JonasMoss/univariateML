# Four finite hypergeometric grids (TASK-98)

Source: branch `codex/hyper-grids`, based on `c578ac2`; uncommitted changes
in the two distribution dossiers, fitters, adapters and tests. No default
fit or golden changes. The five unbounded structural exceptions remain.

## Searches and numerical policy

Write `a=min(x)`, `b=max(x)`. The algebraic bounds and attainment arguments
are in [the design report](2026-10-30-holds-hyper-report.md) and
[the follow-up](2026-10-30-holds-hyper2-report.md).

- Hyper `{N}`: enumerate `b <= m <= N-b+a`, `b <= k <= N-m+a`.
  These are the common-support constraints, including `m+k <= N+a`.
- Nhyper `{N}`: positive data enumerate `1 <= r <= a`,
  `r <= m <= N-b+r`. Zero-only data select `(m,r)=(0,0)`.
- Nhyper `{m,r}`: positive `r` enumerates `m+b-r <= N <=
  max(m+b-r,floor(m*b/r))`. Beyond that bound each population-ratio
  factor is below one, so no larger population can improve the maximum.
  At `r=0`, zero-only data select `N=m`; positive data are infeasible.
- Nhyper `{m}`: enumerate `1 <= r <= min(m,a)`, with the preceding
  population range for each line. Zero-only data select `(N,r)=(m,0)`;
  mixed zero and positive data are infeasible.

No joint unimodality is assumed: every candidate inside the bounds is
evaluated. This favors transparent coverage over speed on very large grids.
All new grids sum log-gamma differences in compensated long double sums,
using observed-value multiplicities. First find the numerical maximum,
then select the lexicographically smallest `(m,N,k)` or `(m,N,r)` within
`64 * double epsilon * max(abs(ll),abs(maxll))` of it. This prevents
chained approximate ties. Joint-grid ties are resolved silently. Counts
and held values remain exact doubles below 2^53; `fixed` records holds.

The budget is 10^7 candidate likelihood evaluations, counting both passes.
Grid cardinalities are checked before searching; oversized grids, bounds
outside the representable count range and nonfinite evaluations return
`NumericalFailure`, and adapters stop without a partial maximum. Population
quotients are corrected with compensated integer product differences.
Attainment and the tail bound are mathematical results; floating-point
ordering is numerical under the agreed tie policy, not an exact certificate.

## Validation

R whole-grid oracles use 26 samples per subset (104 fits), including zeros,
constants, near-support boundaries, repeated observations and n up to 100.
Hyper uses `stats::dhyper`; nhyper uses `extraDistr::dnhyper`, with analytic
point masses at r=0 and m=N. The installed extraDistr segfaults at n=0
(no black balls), so that endpoint must not be called. Tests check complete
coefficient vectors, likelihoods and fixed attributes, plus budget failures
and infeasible data. Appended doctests enumerate independent small grids.

Final simbox commands (via `/tmp/umvl-codex/simbox-run.sh`, nice 10,
one thread under the shared slot lock):
- `just cpp-test`: 6/6 CTest targets; 927 cases, 1,226,316 assertions total.
  Distribution target: 905 cases, 1,215,269 assertions; no failures or skips.
- `just test`: 34,567 passes, 0 failures, 0 warnings, 0 skips (441.6 s).
  Family files: hyper 276, nhyper 568 assertions, all passing.
- `just quality hyper nhyper`: 2/2 pass.
Local targeted grid run: 4 cases, 15,156 assertions passed. R likelihood
oracle tolerance is 1e-10; appended C++ checks use 1e-11 (hyper), 1e-10
(nhyper). New workloads are deterministic. Simbox used C++17/GNU 13.3,
BH headers, R 4.6.1 and g++ 15 for R adapters. Final `git diff --check` passed.

Initial full R verification aborted at the unguarded extraDistr endpoint;
initial C++ verification found one obsolete NaN-k invalid-input assertion.
That list now excludes NaN, which requests the newly supported fit; all new
C++ cases are appended. Both complete suites were rerun successfully.
Tested distribution-tree diff from c578ac2 has SHA-256:
`408945c18849e4047ec08d2321bc20e635f08b71b5411c2aa1501b3c58d15a54`.

No TASK-78-owned file was changed; no change to those files is required.
