# pgw (power generalized Weibull) as a fitted family

**Status:** banked 2026-11-15 (card TASK-183 archived in the board cleanup); not scheduled.

Theory and d/p/q are done:
[theory](../research/evaluations/2026-11-11-pgw-theory-report.md),
[d/p/q](../research/evaluations/2026-11-12-pgw-dpqr-report.md). Remaining: `mle.hpp` for the free
fit and every hold subset with certificates and named escapes, the README dossier, `test.cpp`,
goldens with 50-digit references, `just quality`, the R wrapper through `just regen`, and the
inference classification entry. Schedule after the consolidation plan (dev/design/consolidation.md),
so the fitter is written against the shared outcome type and the strict layer from the start.
