# Work budgets (decided 2026-11-06)

`iterlim` (C++ `maxit`) is the single user control of the work a fit may do.
Every loop cap in a fit is

    cap = K[unit] * min(iterlim, 100000)

computed in 64-bit integers, for the work unit the loop counts. No cap is a
fixed constant that `iterlim` cannot raise, and `iterlim = 1` must still stop
quickly (tests use it to force stops).

| unit | K | one unit is |
|---|---|---|
| solver iteration | 1 | one step of a local solve (Newton, BFGS, bracketing pass, profile inner solve) |
| certificate evaluation | 128 | one segment, box or profile point evaluated by a global certificate search |
| enumeration cell | 100000 | one O(1) cell of an exact discrete enumeration or integer-range scan |

Loops nested inside a certificate evaluation (mean solves, proposer
refinements) are solver loops with their own `K = 1` caps.

## What exhausting a cap means

A solver cap is non-convergence: the existing iteration-failure status, and R6
applies (`dev/roadmap/2026-10-29-parameter-rules.md`). A certificate or
enumeration cap means the sample needs more work than allowed, not that the
fit failed. It is a `budget` stop (`budget_exhausted`, R class
`univariateML_budget`) that keeps its witnesses
(`dev/design/structured-stop-details.md`), and its message says that raising
`iterlim` may resolve it.

## Defaults

`K` is the same for every family. Differences in per-unit cost between
families go into the family's default `iterlim`, not into a family-specific
`K`. Raise a family's default `iterlim` when ordinary samples (goldens,
seeded test samples) would otherwise stop on budget. Raising it only loosens
solver caps, which converged solves never reach. The target is that budget
stops are rare on ordinary samples and a default-budget stop at n = 100000
takes a few seconds at most.

Families without an `iterlim` argument (exact or closed-form fitters) keep a
fixed cap, stated in the README. Add `iterlim` to one only if realistic
samples hit that cap.
