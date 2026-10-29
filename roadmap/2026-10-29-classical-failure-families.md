# Classical ML-failure families, and a failure gallery

**Status:** recorded 2026-10-29. Skew-normal in progress (Codex job `sn`);
the rest deferred.

The univariate families most famous for failing maximum likelihood, which
the package does not have yet:

| Family | Failure | Literature | Reference / d-p-q-r |
|---|---|---|---|
| Azzalini skew-normal | shape estimate infinite with positive probability (escape to the translated half-normal); the profile likelihood has a stationary point at shape 0 for every sample, where the Fisher information is singular | Azzalini (1985); Azzalini & Capitanio (1999); Pewsey (2000); Sartori (2006); Azzalini & Arellano-Valle (2013, penalised ML) | own d/p/q/r (CDF via Owen's T, in Boost) |
| Three-parameter Weibull, gamma, lognormal (threshold) | likelihood unbounded as the threshold approaches the minimum when the shape is below one; Smith's regimes (regular for shape > 2, non-regular local maximum for 1 < shape < 2, no local maximum below 1) | Smith (1985); Cheng & Amin (1983, maximum product of spacings); Cheng & Iles (1987); Hill (1963) | extraDistr/actuar/stats with a location shift, or own |
| Skew-t | skew-normal problems plus the Student-t tie unboundedness with free df | Azzalini & Capitanio (2003); Azzalini & Arellano-Valle (2013) | own d/p/q/r |

Related, as illustrations of "removable" escapes (see the taxonomy below):

- Prentice's extended generalized gamma $(\mu,\sigma,Q)$, $Q\in\mathbb{R}$:
  `trgamma` ($Q>0$), lognormal ($Q=0$) and `invtrgamma` ($Q<0$) in one
  smooth family, so the lognormal escape becomes an interior point.
- Student t continued through the normal into Pearson type II (symmetric
  beta) with $\eta = 1/\nu < 0$; spikes appear for $-1 < \eta < 0$ exactly
  as for the generalized Pareto with $\xi < -1$.

## Failure gallery

One entry per phenomenon, across all families: failure type, minimal
reproducer sample, exact condition where known, profile-likelihood figure.
Material: `notes/ml-behaviour.tex`, `notes/evaluations/2026-10-2*`.

Taxonomy (working version):

1. Escape to a nondegenerate limit outside the family (Lomax, t, NB,
   binomial, Burr, GG, GB2, skew-normal). Removable: reparametrise so the
   limit is a boundary point (compactify; the MLE exists but sits on the
   boundary with positive probability, non-regular inference), or continue
   analytically through it (GPD, Prentice GG, Pearson VII/II; the boundary
   becomes interior). Corners reached along many paths (GB2's lognormal)
   need a blow-up rather than a single added point.
2. Unbounded likelihood (spikes towards degenerate limits: threshold
   families, GED/SGED, GEV, free-df t with ties, three-parameter
   Birnbaum–Saunders). Not removable by reparametrisation; fixed only by
   changing the model (parameter bounds) or the estimator (grouped or
   rounded likelihood, maximum product of spacings, penalisation, a local
   maximum).
3. Non-identifiability (beta negative binomial size/beta mirror, folded
   normal sign): quotient the parameter space.
4. Coordinate singularities (skew-normal at shape 0): the MLE exists, the
   information is singular; a different parametrisation (centred) removes it.
5. Non-unique maxima (Cauchy balanced pair, triangular, Huber location):
   inherent; needs a selection rule.

## Trigger to revisit

After the skew-normal lands; when the author wants the gallery.

## Verification

Each family: README limits, oracle tests, a reproducer per failure; the
gallery builds from scripts with fixed seeds.
