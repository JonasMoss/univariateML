# Neutral-theory abundance models: MZSM and Volkov

**Status:** banked at low priority, 2026-11-12 (Jonas: "add dmzsm to low
priority"), from the [sads survey](../research/evaluations/2026-11-12-sads-survey.md).
Volkov is recorded here for context only and is not proposed.

## MZSM (metacommunity zero-sum multinomial)

sads (`dmzsm`) uses Alonso and McKane's (2004) large-$J$ form

$$
p(y)\propto\frac{\theta}{y}\Bigl(1-\frac{y}{J}\Bigr)^{\theta-1},
\qquad y=1,\dots,J,
$$

with community size $J$ known. That is a one-parameter exponential family in
$\theta$ (statistic $\sum\log(1-y_i/J)$), so the fit is concave and easy. The
exact finite-$J$ expected abundance distribution of a neutral sample
(Ewens) is

$$
p(n)\propto\frac{\theta}{n}\,\frac{J!}{(J-n)!}\,
\frac{\Gamma(J-n+\theta)}{\Gamma(J+\theta)},
$$

which the approximation replaces by $(1-n/J)^{\theta-1}$. The approximation
fails at $y=J$, where the weight is $0$ for $\theta>1$ and infinite for
$\theta<1$; that is the source of the sads defect recorded in
[KNOWN_ISSUES](../KNOWN_ISSUES.md). If implemented, prefer the exact form and
check its concavity in $\theta$ separately (it is not an exponential family).

**Caveat for any iid fit.** The $S$ species abundances of one sample sum to
$J$, so treating them as iid draws from $p$ is a composite likelihood. The
exact likelihood is the Ewens sampling formula, under which the number of
species $S$ is sufficient for $\theta$. A dossier must say which one is fitted.

## Volkov (neutral local community with immigration)

Volkov, Banavar, Hubbell and Maritan (2003, *Nature* 424:1035) give the
expected number of species with abundance $n$ in a local community of size
$J$ receiving immigrants with probability $m$ from a metacommunity with
biodiversity number $\theta$:

$$
\langle\phi_n\rangle=\theta\binom{J}{n}\frac{\Gamma(\gamma)}{\Gamma(J+\gamma)}
\int_0^\gamma\frac{\Gamma(n+y)}{\Gamma(1+y)}
\frac{\Gamma(J-n+\gamma-y)}{\Gamma(\gamma-y)}e^{-y\theta/\gamma}\,dy,
\qquad\gamma=\frac{m(J-1)}{1-m}.
$$

`sads::dvolkov` normalises this over $n=1,\dots,J$ (with a fixed 96-point
quadrature in C) and fits $(\theta,m)$ with $J$ held. Reasons not to add it:

- Every pmf evaluation needs $J$ integrals with large-argument gamma ratios;
  $J$ is around $2\times10^4$ for the BCI plot. Tail accuracy and a
  certified stopping rule would be real work.
- Its $m\to1$ face should be the metacommunity law, but on 2026-11-12
  `dvolkov(c(1,2,5,50), theta = 5, m = 0.99999, J = 200)` gave
  0.26266, 0.12814, 0.047612, 0.00157 against exact Ewens
  0.25702, 0.12598, 0.047442, 0.0016864 and sads `dmzsm`
  0.25779, 0.12632, 0.047535, 0.0016643. Whether sads's quadrature is
  inaccurate at large $\gamma$ or the limit is something else is unresolved.
- The same composite-likelihood caveat applies, more strongly: Etienne's
  (2005) sampling formula is the exact likelihood, and GUILDS and untb
  implement it for the users who want this model.
