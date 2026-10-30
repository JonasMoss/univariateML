# Remaining hypergeometric holds: partial investigation

2026-10-30. **Incomplete:** no new fitter subset has been implemented. The
nine requested subsets remain `not-implemented`. The only code-tree changes
are more precise structural-hold reasons and unsupported-fit diagnostics.
The existing fitters and their rounding-based tie handling are unchanged.

## Existence and search

Write `a = min(x)`, `b = max(x)` and `h = length(x)`.

* Hyper `{N}`: the feasible `(m,k)` grid is finite; a maximum exists.
* Nhyper `{N}`: the feasible `(m,r)` grid is finite; a maximum exists.
* Nhyper `{m,r}`: for positive `r`, a maximum lies at or below
  `max(m+b-r, floor(m*b/r))`, by the population ratio in the preceding
  report. At `r=0`, only zero observations are feasible and all `N>=m`
  maximize.
* Nhyper `{m}`: for positive observations, `1 <= r <= min(m,a)`;
  applying the same bound for each `r` gives a finite search. Zero-only
  observations admit `r=0`; mixed zeros and positives are infeasible.

These bounds are algebraic proofs, not numerical extrapolations. The missing
implementation must compare candidates reliably, enforce an explicit work
budget, and return numerical failure if it cannot certify a comparison or
finish the grid. The inherited compensated long-double ratio and 64-ulp
rounding tie heuristic are not a proof of exact integer ordering. No budget,
new tie rule, or new oracle tests were added in this job.

* Hyper `{m}`: as `N,k` diverge with `k/N -> p`, the face is
  `Binomial(m,p)`; its supremum uses `p = mean(x)/m` for positive `m`
  (the `m=0` case is degenerate). A finite point strictly above this face
  entails an attained finite maximum, since escaping sequences have no
  larger limiting likelihood. Equality is itself an attained maximum if
  no other finite point exceeds it. A computable global tail certificate
  to decide this comparison is missing.
* Hyper `{k}`: the symmetric face is `Binomial(k,p)`, with
  `p = mean(x)/k` for positive `k`. The same certificate is missing.
* Hyper `{}`: in addition to finite-size binomial faces, allowing both
  `m` and `k` to diverge with `m*k/N -> lambda` gives a Poisson face.
  Its supremum occurs at `lambda = mean(x)`. A certified global comparison
  over all finite-size faces, the Poisson face and the interior is missing.
* Nhyper `{r}`: for positive fixed `r`, `x-r` tends to a negative binomial
  with size `r` and success probability `r/mean(x)`; a certified tail
  comparison is missing. For `r=0`, the model is identically zero.
* Nhyper `{}`: positive data bound `r` by `a`; the boundary includes the
  negative-binomial face for each admissible positive integer `r`. A
  certified comparison over these faces and the finite interior is missing.

Constant samples give finite maximizers in the unrestricted hyper family:
set `N=m=k=x[1]`, obtaining probability one. Thus a universal `no-mle`
label would be incorrect. The previous report contains the detailed ratio
proofs and numerical corroboration; these do not certify the new searches.

## Tests and handoff

Two R assertions cover the fully free unsupported-fit diagnostics; no new oracle or doctest cases were added. The requested nine implementations,
finite-grid budget/error-path tests, escape tests, and the corresponding
independent reference enumerations remain open. No TASK-78-owned file was
changed; no specific TASK-78 change is required for this partial investigation.

Final simbox checks: `just cpp-test` passed 6/6 CTest targets; `just test`
passed 34,190 assertions, with 0 failures, 0 warnings and 0 skips; and
`just quality hyper nhyper` passed 2/2. An initial R run had four diagnostic
message mismatches (34,186 passes); the README reasons were corrected before
the successful full rerun. These checks do not validate unimplemented holds.
