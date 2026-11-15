# qgauss (symmetric q-Gaussian) d/p/q and fitter

**Status:** banked 2026-11-15 (cards TASK-202, TASK-203 archived in the board cleanup); not scheduled.
The skewed variant is a separate idea: [skewed q-Gaussian](2026-11-11-skewed-q-gaussian.md).

Parametrization, restricted shape domain and escape analysis are done (TASK-201):
[literature](../research/evaluations/2026-11-10-qgauss-lit-report.md),
[theory](../research/evaluations/2026-11-12-qgauss-theory-report.md). Remaining:

- `dpqr.hpp`, continuous through $\eta = 0$ from both sides: log density via
  $\log(1+\eta z^2)/\eta$; normalizing constant through the normal without lgamma-difference
  cancellation; CDF by incomplete beta on each side (Student t for $\eta > 0$, symmetric beta on
  the support for $\eta < 0$), tail-accurate as $\eta \to 0$; mpmath truth table.
- `mle.hpp` on the decided shape domain and every hold subset with certificates, delegating to
  norm, lst and unif at the hold values; named escapes and restriction reporting; dossier with
  `inference_numeric` regimes; tests, goldens, `just quality`, R wrapper.

Schedule after the consolidation plan, like [pgw](2026-11-15-pgw-family.md).
