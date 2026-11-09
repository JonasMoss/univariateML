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

## Geometry of the system (working notes, 2026-11-09)

From a conversation with Jonas (TASK-166). Labels: **[algebra]** direct
computation, checked symbolically; **[numeric]** high-precision check in the
[geometry data](../research/evaluations/2026-11-09-pearson-geometry-data/README.md);
**[lit]** standard result; **[sketch]** argued, not proved.
$H^2(f,g)=\int(\sqrt f-\sqrt g)^2$.

**Coordinates.**

* Write $-f'/f=(b_0+b_1x)/(c_0+c_1x+c_2x^2)=P/Q$. Only the ratio
  matters, so the coefficients are a point of $\mathbb P^4$; Pearson's form
  is the affine chart $b_1=1$. That chart puts members at infinity:
  Beta($\alpha,\beta$) has $b_1=\alpha+\beta-2$, so every beta with
  $\alpha+\beta=2$, the uniform included, has infinite Pearson
  coefficients, and bell-shaped and U- or J-shaped betas meet only there.
  [algebra]
* A member is a coefficient point plus an arc: the support runs between
  consecutive points of {real roots of Q, infinity} on the real projective
  line. With real roots and endpoint exponents $p_1,p_2>-1$,
  $p_1+p_2<-1$, one coefficient point carries three members: a U-shaped
  beta inside and type VI on each outer half-line. [algebra]
* The endpoint exponent at a root $r$ is $p=-(b_0+b_1r)/Q'(r)$. A root
  shared by P and Q gives $p=0$, a finite positive endpoint density (Lomax,
  Pareto, exponential with threshold, uniform). Keeping that root in Q keeps
  such members at finite coordinates: Lomax is $[0:\alpha+1:0:\lambda:1]$
  and tends to the exponential $[0:r:0:1:0]$ as $\alpha/\lambda\to r$,
  the case where the usual Lomax chart puts a Hellinger limit at infinity.
  [algebra]

**Structure.**

* At fixed Q the fibre is a two-parameter exponential family,
  $\log f=-b_0T_0-b_1T_1$ with $T_j=\int x^j/Q$: Q = 1 normal, x gamma,
  x(1-x) beta, x^2 inverse gamma, 1+x^2 type IV. The log-likelihood is
  concave in the numerator at fixed Q for every type; type IV's concavity in
  (m, nu) at fixed location and scale is a special case. A fitter can search
  over Q up to scale (two coordinates) with a concave inner fit. [algebra]
* The types are root configurations of Q, a conjugation-symmetric root pair
  on the Riemann sphere: complex pair IV (VII when the mode is at the pair's
  centre), two real roots I or VI (by arc), double finite root V, one root at
  infinity III, double root at infinity normal. Modulo location and scale
  each configuration is one point, so the shapes live in the fibre.
  [algebra]
* The type III quadratics ($c_2=0$) form the tangent line to the
  double-root conic $c_1^2=4c_0c_2$ at the normal. In signed skewness both
  type curves are $\beta_2=3+k\gamma_1^2$, with k = 3/2 (III) and 15/8 (V),
  so type VI is a horn at the normal. The cusp is in the type labels, not in
  the model. [algebra]

**Smoothness of the embedding in the Hellinger sphere.**

* Type IV interior: scores bounded or logarithmic, information positive
  definite (the location-scale block has determinant proportional to
  $-(4m^2+\nu^2)$). [algebra]
* Across type V, IV and VI share one expansion in
  $\delta=1/\nu^2$ (IV, with $\alpha\nu=\beta$ fixed) and
  $\delta=-\gamma^2/\beta^2$ (VI, roots $\lambda\pm\gamma$):
  $\log f=\log f_V+\delta\,(s(y)-\mathbb E_Vs)+O(\delta^2)$ with
  $s=-m\beta^2/y^2+\beta^3/(3y^3)$, $y=\lambda-x$. Type V is a smooth
  hypersurface of the joint model, not an edge, and the $y^{-3}$ term keeps
  the information full rank. [numeric, first order on both sides] DQM
  through V, the type III face, and $C^\infty$ but not analytic across the
  boundaries (flat terms such as type IV's $e^{-\pi\nu}$ mass beyond the
  type V threshold): [sketch]. At the normal, Jarque and Bera derived their
  test as the score test of normality within the Pearson family. [lit]
* Free endpoint with exponent p: $H^2(h)\asymp h^{p+1}$ for $p<1$,
  $\tfrac c4h^2\log(1/h)$ at $p=1$ (density $\sim c\,y$),
  $\tfrac14Ih^2$ for $p>1$. The endpoint curve in $L^2$ behaves like
  fractional Brownian motion with Hurst index $\min((p+1)/2,1)$: chords
  over disjoint parameter intervals are orthogonal at p = 0 (Halmos's
  crinkled arc) and parallel for p > 1. For $p\le1$ the curve has infinite
  length, so no reparametrisation is bi-Lipschitz there. The inverse modulus
  is $\omega(t)\asymp t^{2/(p+1)}$ and $\omega(n^{-1/2})=n^{-1/(p+1)}$ is
  Smith's (1985) rate. This non-regular part is an open region (types I,
  III, VI with some free endpoint exponent below 1), not a lower-dimensional
  set. [numeric for gamma at p = -0.5, 0, 0.5, 1, 1.5; lit]
* What "fractional Brownian" means here; nothing is random. Translations act
  isometrically on $\gamma(\tau)=\sqrt{f_\tau}$, so the curve's geometry is
  fixed by $\varphi(h)=H^2(h)$, and polarization gives every chord inner
  product,
  $\langle\gamma(b)-\gamma(a),\gamma(d)-\gamma(c)\rangle=\tfrac12[\varphi(d-a)+\varphi(c-b)-\varphi(c-a)-\varphi(d-b)]$.
  A centred Gaussian process is a curve in $L^2(\Omega)$, and fBm is the curve
  with $\varphi(h)=|h|^{2H}$ (Kolmogorov's "Wiener spirals", 1940; H = 1/2 is
  Halmos's crinkled arc $t\mapsto\mathbf 1_{[0,t]}$). Near the endpoint
  $\sqrt{f_\tau(x)}\approx(x-\tau)_+^{p/2}g(x-\tau)$ with g smooth and
  g(0) > 0: the Mandelbrot-Van Ness kernel with $H-\tfrac12=p/2$. The Wiener
  isometry $X_\tau=\int\sqrt{f_\tau}\,dW$ gives a stationary Gaussian process
  with $\mathbb E(X_\tau-X_\sigma)^2=H^2(f_\tau,f_\sigma)$ whose small-scale
  increments are fBm's; for the uniform, $X_\tau=W(\tau+1)-W(\tau)$. Blowing
  the curve up at a point gives a line where DQM holds and a Wiener spiral for
  p < 1. [algebra; numeric through the chord cosines]
* Spectral form, with $\hat g(\omega)=\int g(x)e^{-i\omega x}dx$ and
  $g=\sqrt f$:
  $\varphi(h)=\frac1\pi\int(1-\cos\omega h)\,|\hat g(\omega)|^2d\omega$ and
  $I=4\int(g')^2=\frac2\pi\int\omega^2|\hat g(\omega)|^2d\omega$. A power
  endpoint gives $|\hat g(\omega)|^2\sim|\omega|^{-(p+2)}$, fBm's exponent
  $2H+1$, and finite information iff p > 1: Hajek's location-family DQM
  criterion read in frequency space. For the gamma,
  $|\hat g(\omega)|^2=\Gamma(p/2+1)^2/(\Gamma(p+1)(1/4+\omega^2)^{p/2+1})$
  exactly. The parameter rate solves $n\,\varphi(h_n)\asymp1$.
  [algebra; numeric: spectral and direct $\varphi$ agree to 8-10 digits,
  spectral I = 2 at p = 1.5]
* Limit experiments, from memory and unverified. For a two-sided cusp
  ($f\approx a-b|x|^\kappa$) the local log-likelihood ratio converges to
  $B_H(u)-\tfrac12|u|^{2H}$, literally fBm (Ibragimov-Has'minskii). At a
  one-sided endpoint the support moves too: observations in the uncovered
  sliver matter, and the limit is driven by a Poisson process near the
  endpoint with intensity proportional to $x^p$. Its Hellinger geometry is
  the same spiral, since Poisson processes have affinity
  $\exp(-\tfrac12\int(\sqrt{\lambda_u}-\sqrt{\lambda_v})^2)$: one geometry,
  realised with Gaussian or with Poisson noise.

**Closure, injectivity and the inverse modulus.** The property wanted is
"Hellinger-close implies parameter-close", $d(\theta,\theta')\le\omega(H)$.
It fails globally when parameters escape to infinity while the densities
converge, as for Lomax with the exponential appended.

* $\mathbb P^4$ is compact and P/Q with the arc determines the member, so the
  map is injective (no crossings). Closure lemma, unproved: a sequence whose
  coefficients tend to a non-normalisable point is not Hellinger-Cauchy.
  Given the lemma, the system is closed in the Hellinger sphere, the inverse
  is uniformly continuous away from the non-normalisable points, and there is
  no boundary and no corner: every edge is either glued (IV|V|VI, I|III|VI,
  the normal, bell|U beta through $b_1=0$) or at infinite distance.
  Conjecture: a connected topological 4-manifold. Subfamilies with held
  parameters do have boundaries (type IV alone has the type V face).
  [sketch]
* The heavy-tail end is a cusp. Type VII with $d=2m-1\to0$ has location
  information $d(d+1)/((d+3)\alpha^2)$ while the information in d grows like
  $1/d^2$. At a location gap $\Delta=\alpha$, $H^2\approx0.077d$; for large
  gaps $H^2=2d\log(\Delta/\alpha)+O(d)$. Members with distant locations are
  Hellinger-close, and no chart removes this. [numeric] Endpoint exponents
  tending to -1 give the same kind of end; scale to 0 or infinity and
  location to infinity give hyperbolic-type ends. [sketch]

**Consequences.**

* In the full system the known escapes (to V, III, the normal, the uniform
  and exponential) are interior points, so a whole-system fitter might need
  no escape statuses; what would remain are degenerate paths (a free endpoint
  with p < 0 makes the likelihood unbounded; collapse at ties). This needs
  the likelihood along non-Cauchy paths to tend to plus or minus infinity
  rather than to a finite supremum. [sketch]
* Testing type V is a regular chi-square(1) problem in the joint IV, V, VI
  model and a boundary problem (1/2 chi-square(0) + 1/2 chi-square(1)) within
  type IV alone. [sketch]

## Open questions

* Coordinates. Partly answered by the geometry notes above: the projective
  chart with an arc label covers every member, with the type boundaries
  smooth inside it. Open: a practical atlas for a fitter, and the proofs
  marked [sketch].
* Type IV normaliser: |Gamma(m + i nu/2)/Gamma(m)|^2 needs a complex
  log-gamma, which Boost does not provide. Its CDF needs a hypergeometric
  function with complex arguments, or quadrature (Heinrich, Fermilab note,
  2004).
* Which subsets have attained maxima once thresholds are free, and whether
  spacings or a structured unbounded stop is the right contract.

Literature on type IV (ML, computation, software, the type V boundary):
[TASK-160 literature check](../research/evaluations/2026-11-08-pearson4-lit-report.md).

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
from memory; verify them before citing. For the geometry notes: prove the
closure lemma, DQM through types V and III and the normal, and full-rank
information on the U-shaped and type VI sheets; check the heavy-tail cusp for
type IV with nu != 0.
