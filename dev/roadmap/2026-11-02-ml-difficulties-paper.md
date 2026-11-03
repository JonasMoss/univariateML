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

## Seams (added 2026-11-03)

"Seam", "cusp" and "corner" are provisional working labels, not established
terms ("cusp" clashes with singularity theory). Candidates for the paper:
removable versus essential boundary. Related literature: boundary-parameter
tests (Chernoff 1954; Self and Liang 1987), one-sided score tests for
overdispersion (Dean and Lawless 1989; Tarone 1979), sign-extending
reparametrizations (Prentice 1974; the Katz family; GPD), nonregular
threshold problems (Smith 1985), singular information (Rotnitzky et al.
2000). Citations to be checked.

Many "escapes" are not boundaries but seams: a limit model that sits inside
a larger smooth family, with the fitted family on one side. Reparametrize the
escape coordinate so the limit is at zero and let it change sign.

- `kappa = 1/N`: binomial (`kappa > 0`), Poisson (0), NB of shape `-1/kappa`.
  The first-order slope at Poisson is `-h(v - mu)/2 kappa`; the binomial-size
  and NB-shape existence results are its two signs.
- BB, NB, beta-negative-binomial, by the same substitution in the
  hypergeometric ratio. The BNB side is local geometry only, not a BB limit.
- GPD in `xi`: Lomax (`xi > 0`), exponential (0), bounded support (`xi < 0`).
  The slope at the exponential is `h(m2/(2 xbar^2) - 1)`, i.e. CV > 1,
  while the Lomax dossier's exact criterion is a profile condition.
- Burr XII is the GPD on `x^c` (`xi = 1/k`, `sigma = s^c/k`), so its
  Weibull limit is the same seam; the other side has bounded support.
- Generalized gamma in Prentice's parametrization: lognormal at zero.

The recurring pattern: the first-order seam slope is a dispersion-type
statistic and gives a sufficient condition; the exact existence criterion is
global and differs. Where two seams meet (the Poisson corner of BB, and
presumably the lognormal corner of GB2), the first-order scores are collinear,
the information is singular, and the answer is decided at second order.

Not every face is a seam. Burr's Pareto limit is a cusp: the profile is
`H(k) = P + n k log k + O(k)`, so it is a local maximum of the extended likelihood for every sample and
the first-order test is replaced by a global one (at least half the sample
at its minimum: no finite MLE). Burr also has exact finite/face ties and a
face crossing `P = W` with a strictly better finite fit
(`2026-11-03-burr-closure-report.md`).

The bbinom reports (2026-11-02 map; 2026-11-03 closure and cert) work this out
in full for one family; see `2026-11-03-bbinom-free-size.md`.

## Thin spots

No general existence theorem outside exponential families; joint uniqueness is
"unknown" for most families.
