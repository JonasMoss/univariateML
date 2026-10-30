# TASK-93 / p2b-skew: conditional skew-t and skew-GED fits

All 12 previously unimplemented subsets in each family now have fitting
paths. Their `holds:` exceptions are removed. The default fitters, previous
triple holds, all-held evaluations and golden files are unchanged.

## Algorithms by subset

M/S/N/X denote mean/sd/nu/xi. B denotes multistart BFGS on dimensionless
free coordinates, with a converged simplex rescue for nonsmooth cases;
G denotes scanned and refined bounded scalar search. Every SGED search
with free N also compares the attained N=1 restriction explicitly.

| Held | Interior | sstd competing faces | sged competing faces |
|---|---|---|---|
| M | B(S,N,X) | normal, half-t/normal, df-2 | uniform, half-GED |
| S | B(M,N,X) | normal, half-t/normal | uniform, half-GED |
| N | B(M,S,X) | fixed-N half-t | fixed-N half-GED |
| X | B(M,S,N) | fixed-X normal, df-2 | uniform |
| M,S | B(N,X) | fixed-moment normal and half faces | fixed-moment uniform and half faces |
| M,N | G(X) around a scalar S profile | fixed-N, fixed-mean half-t | fixed-N, fixed-mean half-GED |
| M,X | B(S,N) | fixed-mean/X normal, df-2 | fixed-mean uniform |
| S,N | B(M,X) | fixed-scale/N half-t | fixed-scale/N half-GED |
| S,X | B(M,N) | fixed-scale/X normal | fixed-scale uniform |
| N,X | exact scale profile, G(mode) | none | none |
| M,S,N | G(log X) | forced half-t | forced half-GED |
| M,S,X | G(transformed N), plus N=1 for SGED | fixed-moment/X normal | fixed-moment uniform |

The new family-local `sstd/hold_search.hpp` serves only these two families.
It uses ordinary t scales and exp(-abs(z)^nu) GED scales. For fixed N,X,
the GED scale profile is explicit; the t scale score is strictly monotone
and solved by bisection. Location searches include every observation
interval and the observation kinks. Other scalar searches scan before
refining competing brackets. Higher-dimensional interiors use 12 starts.
With M,N held, scale is profiled before skew is scanned. For GED N>=1
the scale objective is strictly convex in inverse scale; other shapes
retain scanned scale profiles. Conditional Laplace fits also use exact
weighted sample quantiles and, at fixed skew, an explicit scale profile.
Other held-mean Laplace fits enumerate observation kinks.
Searches reaching their finite numerical guards stop instead of returning
an unchecked iterate. Failed BFGS fits require a total free-coordinate
score norm below 1e-3, or their independent rescue must converge.

For half faces, mean=c+sign*m*t and sd=t*sqrt(v-m^2), where m and v
are the ordinary base absolute first and second moments. Held moments
therefore force endpoint and/or scale. Free endpoints are the appropriate
sample extremes; free scales have an exact profile when mean is free,
and a support-constrained scalar search when mean is held. Both signs
are checked. Fixed-moment observations at a limiting endpoint retain
the shrinking-side density penalty. Normal faces reuse conditional snorm;
uniform faces retain the moment constraints. The df-2 face retains
mean=c+sqrt(2)*s*(xi-1/xi), rather than pinning the mode to the held mean.

Held values are restored exactly and adapters retain the `fixed` names.
Estimated SGED N=1 sets bit 16 and emits the existing restriction warning;
a positive held shape below one remains accepted without a restriction flag.
Reachable escapes are declared with `fixed:` in each README. Half-t escapes
recommend `mlht` on signed distances from the endpoint. Default diagnostic
wording is preserved; conditional messages describe constrained endpoints.

## Proofs, numerical evidence, and qualifications

The tie-collapse exponent N*(n-k)-k and the fixed-shape scale profiles are
analytic. Constant SGED samples with free scale collapse. The restriction
N>=1 blocks the free-shape SGED spike for every subset. Equality in the
Student-t collapse criterion remains an explicitly uncertified boundary.
The remaining global comparisons are numerical, not existence or global
optimality certificates; this includes the two joint N,X searches with
both moments held. No subset is asserted to be structurally no-MLE.

P0b's statement that held sd blocks all sstd collapse is incorrect: with
N free, N->2 shrinks the effective ordinary-t scale even at fixed sd.
Its own heavy-tie experiment exhibits this path. The implementation checks
k>2*(n-k) (at held mean, or the largest tie with free mean) even with sd
held. Only holding both sd and N prevents that mechanism. Also, holding mean is not the same
as holding the mode: their affine translation depends on scale and shapes
and must be retained on every face.

For SGED with both moments pinned, an observation exactly at a uniform
endpoint retains the exp(-EulerGamma) log-density penalty. If a held
uniform width equals the sample range exactly while mean is free, the
coupled endpoint-rate optimization remains unresolved: a winning face
stops with `ScaleBoundary`. This is a data-dependent diagnostic, not a
retained structural hold exception. Very extreme held GED shapes can
still exceed floating-point moment/density range and stop numerically.

## Tests and final verification

- `just cpp-test`: **6/6 CTest targets passed** (136.77 seconds).
- New C++ subset tests: **7/7 cases, 146/146 assertions passed**.
- Targeted R files: **sstd 706, sged 705 passing expectations**, zero
  failures, warnings or skips; all 240 new sample/subset combinations ran.
- `just test`: **23,748 passes, 0 failures, 0 warnings, 1 existing skip**
  (`sn` is not installed).
- `just quality sstd sged`: **2/2 families passed**.
- Logs: `/tmp/umvl-codex/p2b-skew/{cpp-final,r-final,quality-final}.log`.

The new R sweep covers 240 sample/subset combinations: each of the 24
interfaces on ten samples, including n=20/100 normal data, ties, n=3,
exponential/reflected exponential, half-t, uniform, and two independent
family draws at n=20/100. Independent
README densities use R t/normal primitives and gamma functions. Each
interior is compared against three starts with both BFGS and Nelder-Mead
(or a scalar Brent search). Every interface must return at least one fit.
Reported faces are independently approached in R; uniform values are
checked directly. Explicit tests cover collapse, equality, restrictions,
held sub-one shapes, and both reflected forced half faces. C++ tests also
check all subsets at golden holds and independent half/df-2 likelihoods.

`git diff --check` passed; both golden files are unchanged. No process
started for this job remains running.

All builds, tests and numerical probes use `/tmp/umvl-cpu/run`. An initial
R debug build and a C++ rebuild hit temporary-file quota exhaustion; final R runs
use scratch compiler settings (-O1 -g0), no pkgbuild debug additions,
and a private cache/temp directory. Compiler and cache preprocessing
temporaries are under this worktree to avoid the /tmp quota. No user
compiler configuration changed.

## Open items and scope

- The artifact-pane opener was attempted, but its viewer returned HTTP 403
  ("That directory is never served") for this hidden worktree path.
- Global optimality and exhaustive joint boundary-rate certification are
  numerical limitations, especially for M,S holds and exact uniform endpoints.
- Correct the contradictory held-sd statements and mean/mode wording in P0b
  outside this job's files. No shared core or density change is required.
- No dpqr, special, truth, codegen source, golden, board or Git-history
  changes were made. No commit was created.
