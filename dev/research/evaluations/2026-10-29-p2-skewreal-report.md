# TASK-81: P2 skewreal

## Changes

Only these distribution files changed, plus this report:

- `snorm/{README.md,mle.hpp,adapter.cpp,test.R,test.cpp}`
- `sstd/{README.md,mle.hpp,adapter.cpp,test.R,test.cpp}`
- `sged/{README.md,mle.hpp,adapter.cpp,test.R,test.cpp}`
- `tnorm/{README.md,adapter.cpp}`

No shared files, density implementations, goldens, board records, or Git
history were changed. `sn` and `emg` already supported every hold and needed
no implementation changes.

## Holds

- **snorm:** all six proper nonempty subsets of `{mean,sd,xi}`, plus
  all-held evaluation. Nested conditional location/scale minimization and
  a scanned/refined skew profile retain held values exactly. Holding
  `xi=1` uses the analytic normal estimates. Both half-normal faces retain
  the held moment constraints; with both moments held, observations at a
  limiting endpoint retain the shrinking-side density penalty. Constant
  data at the held/free mean with free scale report unbounded likelihood.
- **sstd:** holding `{mean,nu,xi}` estimates scale; holding `{sd,nu,xi}`
  estimates location; holding all four evaluates likelihood. The searches
  scan/refine the remaining scalar parameter and include location modes
  at translated observations. For scale estimation, k observations equal
  to the held mean give unbounded likelihood if `k > nu*(n-k)`;
  equality reports the uncertified collapse boundary.
- **sged:** the same two triple holds and all-held evaluation. Constant
  observations at the held mean with free scale report unbounded
  likelihood. Positive held shapes below one are accepted for these
  conditional fits; the estimated-shape restriction remains unchanged.
- **tnorm:** every combination of held/free parent mean and sd with fixed
  bounds was already implemented. Added argument documentation and marked
  all 12 subsets estimating a bound as needing R7a (P3).
- **sn/emg:** existing complete hold interfaces retained and tested.

Each newly accepted adapter argument defaults to NULL, has an exact
coefficient name, is documented, and contributes to `fixed` when supplied.
Failed default skew-t/GED optimizers now require the R6 score gate before
an adapter can return their result. Successful default paths are unchanged.

## Remaining exceptions and mathematics

No in-scope subset was converted to `no-mle`: P0 proves no such structural
exception here. The 12 other proper nonempty subsets of each of `sstd`
and `sged` remain `not-implemented`, with the explicit reason that their
existence remains unresolved in P0. They stop rather than return an
uncertified fit. This is a remaining limitation, not a no-MLE claim.
For each family the retained subsets are `{mean}`, `{sd}`, `{nu}`, `{xi}`,
`{mean,sd}`, `{mean,nu}`, `{mean,xi}`, `{sd,nu}`, `{sd,xi}`, `{nu,xi}`,
`{mean,sd,nu}`, and `{mean,sd,xi}`. All have that same unresolved-P0 reason.

P0 proves fixed-scale location attainment and the stated scale-tail /
collapse conditions. The half-normal moment constraints and endpoint
penalty follow algebraically from the two-piece normal parameterization.
The finite scans, their tail guards, and independent multistart comparisons
are numerical evidence, not global optimization certificates. Student-t
scale-collapse equality and the remaining skew-t/GED hold classifications
remain open. No shared change is required by the implemented cases.

## Validation

All final commands ran on simbox through `/tmp/umvl-codex/simbox-run.sh`
and completed. Their exit codes remain nonzero for the explicitly allowed
machine-dependent failures; no other failure was ignored.

- `just cpp-test`: six CTest targets; five passed and `distribution_tests`
  failed only the two known fixtures. Distribution doctest counts:
  **420 cases, 418 passed, 2 known failures; 513,738 assertions,
  513,736 passed, 2 known failures**. Failures were exactly
  `betapr/test.cpp:33` and `std/test.cpp:40`.
- `just test`: **11,043 passing expectations, 3 known failures,
  0 warnings, 0 skips**. Failures were exactly frozen `qsstd` in
  `test-dist-sstd.R:237` and the `bnbinom`/`std` golden comparisons.
  Generic parameter/hold conformance passed **1,776 expectations**.
  Family-file passing counts: sn 209, snorm 609, sstd 266 (plus the known
  quantile failure), sged 250, emg 126, tnorm 113.
- `just quality sn snorm sstd sged emg tnorm`: **6/6 passed**.
- Added four C++ test cases and ten R test blocks. The new independent
  oracle sweep attempts **104 sample/subset combinations** (56 snorm,
  24 sstd, 24 sged), including n=20/100, ties, small n and limit samples;
  returned partial fits are compared with three R optimizer starts.
  Separate cases cover held domains, collapse/equality, the exact normal
  member, and both reflected held-moment endpoint limits.
- A prior local `just cpp-test` passed all six targets. The redundant
  focused local rerun was interrupted while waiting for a CPU slot.
- Logs: `/tmp/umvl-codex/p2-skewreal/simbox-{cpp-final3,r-final,quality-final}.log`.

All six family goldens passed R regression, and all golden files are
unchanged. `git diff --check` passed. No process started for this job
remains running. No commit was made.

The artifact-pane `show` command was attempted but the viewer returned HTTP
403 because this worktree is outside its configured readable roots. The
report remains at the worktree root; viewer configuration was not changed.
