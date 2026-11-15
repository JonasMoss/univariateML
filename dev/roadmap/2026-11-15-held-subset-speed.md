# Speed of slow held-subset fits

**Status:** banked 2026-11-15 (card TASK-144 archived in the board cleanup); not scheduled.

The census of 2026-11-06 ([report](../research/evaluations/2026-11-06-slow-held-report.md),
`census-baseline.tsv` in its data dir; n = 500 golden, default controls) found st free 15.5 s,
st with xi or omega held 3 to 10 s, std with sd held 7.7 s, slash free 6.3 s, lst with sigma held
5.2 s, tlambda free 3.0 s. The timings predate several accuracy and speed jobs (acc-allfree,
acc-fb-t, acc-fb-cont, dpq-skew), so re-measure first; a new card starts from the new census.
Related: [tlambda inverse table](2026-11-08-tlambda-pregenerated-inverse-table.md).
