# Paper idea: a catalogue of maximum-likelihood difficulties (later)

Raised by Jonas on 2026-11-02, for after the migration. Target: a review in the
style of Statistical Science, not a theorem paper.

## Thesis

For exponential families the existence theory is complete: the MLE exists iff
the mean sufficient statistic lies in the interior of the convex support, and a
face of the support identifies the limit (Barndorff-Nielsen 1978; Geyer 2009,
directions of recession; Csiszár and Matúš, closures of exponential families;
citations to be checked). Outside exponential families the same picture mostly
survives as "compactify the model, classify where the supremum goes", checked
family by family. A fitter can certify its answer or name the limit it escaped
to (the two-outcome procedure).

## Failure modes seen in the 99 families

1. Escape to a limit model: supremum attained only in the closure (Lomax to
   exponential, sn to half-normal, Burr to Weibull/Pareto, bbinom to NB/Poisson).
2. Collapse: unbounded likelihood at atoms or ties (multiplicity conditions such
   as 2k < n for Cauchy; tnorm endpoint collapse).
3. Faces of the convex support (cmb at fixed size: support polygon of
   (x, log choose(N, x))).
4. Integer parameters: size searches with tail cutoffs and block bounds (hyper,
   nhyper, cmb, bbinom, tbinom).
5. Multimodality and non-identifiability (slash, Cauchy with scale held,
   genbeta ridge).

## Material already in the repository

- Existence statements and graded limits in all 99 dossiers (TASK-101.6.4.1),
  `dev/research/ml-behaviour.tex` (closure, firmness grades).
- Certificates (TASK-116, TASK-113, TASK-114, TASK-121): skellam, sn, dgamma,
  slash, ht, cauchy (geodesic convexity), cmb block bounds, hyper Poisson
  envelope.
- Non-maximizers returned by the previous fitters and fixed under rule R6
  (evaluation reports 2026-11-02: burr-collapse, norm-collapse, cauchy-bracket,
  bracket-fixes, zib-zero, gompertz-extreme).

## Thin spots

No general existence theorem outside exponential families; joint uniqueness is
"unknown" for most families.
