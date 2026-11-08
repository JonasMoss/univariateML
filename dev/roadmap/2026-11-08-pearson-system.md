# The Pearson system as one family

**Status:** banked idea, 2026-11-08 (Jonas: "the ultimate univariateML
project"). Not scheduled; no implementation authorized by this entry.

## Idea

Fit the whole Pearson system as a single four-parameter model rather than
as separate types. The density solves

$$
\frac{f'(x)}{f(x)}=-\frac{x-a}{c_0+c_1x+c_2x^2},
$$

and the types are the partial-fraction cases of integrating that rational
function. Real roots of the quadratic become support endpoints with power
behaviour: type I (beta) between the roots, type VI (beta prime) beyond them.
A double root gives an endpoint with an essential singularity (type V,
inverse gamma). A complex pair gives full support with an arctan skew term
(type IV), or Student t (type VII) when the mode is at the pair's centre.
c2 = 0 gives a gamma with threshold (type III), and c1 = c2 = 0 the normal.
Pearson's kappa = c1^2/(4 c0 c2) is the discriminant in disguise
(kappa - 1 = discriminant/(4 c0 c2)). Type boundaries are root collisions:
a real pair merges and leaves the real line, or a root runs to infinity
(c2 -> 0). For c2 > 0 the tails decay like |x|^(-1/c2), so the sign of c2
picks heavy, exponential or bounded tails, as xi does for the GPD and lambda
for `tlambda`.

## Why it is the showcase

One model exhibits nearly every catalogued ML phenomenon:

* free thresholds with infinite endpoint density, an unbounded likelihood
  for every sample (types I, III, VI with an exponent below zero), where
  maximum product of spacings applies; `nsbeta` refuses free endpoints for
  this reason, and `gamma`, `invgamma` and `betapr` fix the threshold at 0;
* an essential-singularity endpoint (type V);
* moving endpoints with zero density and Smith-type rates;
* type changes as faces and continuations; Student t through the normal into
  type II is already in the
  [classical failure-family roadmap](2026-10-29-classical-failure-families.md);
* heavy tails with Cauchy-like multiple maxima (types IV and VII).

Existing members: `beta`, `nsbeta` (I, II), `gamma` (III), `invgamma` (V),
`betapr` (VI), `std`, `lst` (VII), `norm`. New: type IV, free thresholds,
and the joint parameter space.

## Open questions

* Coordinates. The ODE coefficients (a, c0, c1, c2), with location and scale
  split off, give one chart over all types, subject to normalisability.
  Pearson's moment coordinates (mean, sd, beta1, beta2) are another;
  per-type charts are a third. Which coordinates keep the likelihood
  continuous across kappa = 1 and c2 = 0?
* Type IV normaliser: |Gamma(m + i nu/2)/Gamma(m)|^2 needs a complex
  log-gamma, which Boost does not provide. Its CDF needs a hypergeometric
  function with complex arguments, or quadrature (Heinrich, Fermilab note,
  2004).
* Which subsets have attained maxima once thresholds are free, and whether
  spacings or a structured unbounded stop is the right contract.

## Trigger to revisit

After the [quantile-defined families](2026-11-08-quantile-defined-families.md),
or when the [ML-difficulties paper](2026-11-02-ml-difficulties-paper.md)
wants one model exhibiting all failure types. Pearson type IV alone could
come earlier as an ordinary family (ion-implantation profiles, skewed
heavy-tailed returns).

## Verification

Per type, an existence and escape inventory, then the joint type-boundary
analysis. 50-digit truth tables, including type IV. Independent comparison
with the R package `PearsonDS` (moment and ML fitters). Spacings against ML
on free-threshold samples. The applications and the package named here are
from memory; verify them before citing.
