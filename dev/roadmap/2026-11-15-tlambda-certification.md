# tlambda: remaining certification and speed

**Status:** banked 2026-11-15 (card TASK-151 archived in the board cleanup). Jonas, 2026-11-07:
noted, not to be worked on; ML for the symmetric Tukey lambda is rarely used.

Open items, from the [speed report](../research/evaluations/2026-11-08-tlambda-speed-report.md): a
certificate for free shape with a free nuisance, and for negative held shape with both nuisances
free (box bounds exhausted at n = 31); the certified shape-only fit takes about 115 s. The strict
layer still lists tlambda tie and two-point cases (TASK-214.17). See also the
[inverse-table idea](2026-11-08-tlambda-pregenerated-inverse-table.md).

**2026-11-25:** the [TASK-240 math report](../research/evaluations/2026-11-25-tl-math-report.md) settles the U-branch
shape rule (junction status when $J$ wins), the exactly-half-tie comparison (box $K$, cutoff $\tau$) and
several uniqueness regimes, and proposes a cheaper shared-scale envelope; its "What this changes for the fitter"
section is the brief for any future tlambda implementation.
