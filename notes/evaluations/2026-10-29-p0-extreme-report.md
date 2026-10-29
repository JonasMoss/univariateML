# TASK-79 P0 extreme-value analysis

All 21 hold subsets have table entries. The task remains **incomplete**:
several face-comparison criteria are marked † because I have not proved that
all noncompact sequences are exhausted, and the numerical multistart does
not certify global roots. The proposed YAML is a draft because `while_free`
cannot express the necessary conditional/disjunctive restrictions.

Proved and checked:

- With the actual `extraDistr` densities the GPD and GEV upper-endpoint
  density is zero at xi=-1. The uniform GPD / reversed-exponential GEV
  endpoint faces are therefore often unattained. C2 must specify whether to
  adopt a different boundary-density convention.
- A free GPD threshold still lacks an MLE under -1<=xi<=1: its likelihood
  increases towards the excluded sample minimum, or the uniform interval
  shrinks to excluded endpoints. Holding sigma can allow a uniform plateau.
- GEV diverges with sigma held and mu free as xi→infinity; it also diverges
  with mu held above the minimum, xi→infinity and sigma growing. Thus the
  upper shape restriction remains relevant when either parameter is held.
- Fréchet diverges with sigma held as lambda→0 and mu approaches the
  minimum, so its shape restriction is also relevant in that subset. With
  both lambda and sigma held, free mu has an MLE even when lambda<1.
- The tied minimum, rather than every tied value, drives positive-shape
  collapse; master rejects some samples whose fixed-shape MLE exists.
- For held GPD threshold and shape xi>-1 the scale MLE is unique; several
  other one-free-parameter and Fréchet hold cases are proved in the note.

Fixed-seed simbox studies cover all subsets, default samples at n=20,100,
1000, n=2,3,5, ties, constants and limit samples. Among 212 successful
master fits, every log likelihood agrees with the independent reference-
density multistart within 1e-6*n (largest difference/n 2.92e-8). The
`[-1,-1/2]` profile sweep shows endpoint, interior and upper-grid maxima;
its optimizer and mesh cannot certify all roots. Path probes confirm the
listed divergences and reference endpoint values.

The detailed [analysis](notes/evaluations/2026-10-29-p0-extreme.md) and
scripts/results in `notes/evaluations/2026-10-29-p0-extreme/` record the
remaining gaps and exact comparisons. README shape domains differ from the
reference domains as documented there. No package, README, test or golden
files, board, or commits were changed.
