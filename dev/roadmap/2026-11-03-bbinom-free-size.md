# bbinom with size and both shapes free (later; TASK-113)

Parked 2026-11-03 after four research rounds; Jonas wants to return to it
("exactly the kind of problem we do want to solve"). The fitter stops as
not-implemented for this subset; held-alpha and held-beta free-size fits are
implemented.

## Where things stand

- [Map](../research/evaluations/2026-11-02-bbinom-map-report.md): 2,202
  samples; dispersion does not decide the winner; local slopes at the faces
  are necessary, not sufficient.
- [Closure](../research/evaluations/2026-11-03-bbinom-closure-report.md),
  proved: the proper closure is finite-size BB, binomial and endpoint laws
  plus NB and Poisson (defective limits only `q delta_0`); `Q_N -> F`; a
  finite MLE exists iff some finite BB reaches the active face (best
  binomial, Poisson or NB by dispersion); joint second-order expansion at
  the Poisson corner (first-order scores in `1/a` and `1/N` collinear);
  equidispersed sufficient condition `F3 < mu^3`; exact counterexamples,
  including a strict local Poisson maximum that loses globally.
- [Certificate attempt](../research/evaluations/2026-11-03-bbinom-cert-report.md),
  proved but loose: joint tail envelope `sup L(N,.) <= L_* + O(1/N)` from a
  binomial/Poisson kernel bound and a beta/gamma NB coupling. It clears the
  tail at size 1,001 for 72 of 2,202 samples; six hard samples fail even
  at `10^12`.

## What is missing

1. A sharp tail bound near the NB seam and the Poisson corner, presumably
   in `kappa = 1/N` with explicit curvature constants, sectored around the
   corner in `(1/a, 1/N)` where the second-order (third-moment) term rules.
2. Exclusion of intervals of sizes without fitting every profile.
3. A certified upper bound for the NB face (its fitted value is numerical).

The hard cases have likelihood gaps down to `6e-4`, so the bounds must be
close to sharp, not merely convergent.
