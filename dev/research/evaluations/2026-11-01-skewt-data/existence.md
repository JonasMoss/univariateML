# Azzalini skew-t: existence and limiting faces (2026-11-01)

This is a research dossier, not a registered or implemented family. `st` is
unused at revision d8a62683. Grades below distinguish proofs from open work.
Let observations be finite, n >= 1, and write

\[
 f(x)=\frac{2}{\omega}t_\nu(z)T_{\nu+1}\!\left(\alpha z
 \sqrt{\frac{\nu+1}{\nu+z^2}}\right),\quad
 z=(x-\xi)/\omega,\quad \omega,\nu>0,\quad \alpha,\xi\in\mathbb R.
\]

Here t and T are the central Student density and CDF. The parameter domain is
finite; limits are not members. This is the univariate member of Azzalini and
Capitanio (2003), DOI [10.1111/1467-9868.00391](https://doi.org/10.1111/1467-9868.00391).
The existing `sstd` is a different skew-t construction.

## Collapse and restrictions

**Proved, fixed finite nu and alpha.** Let k observations equal the collapse
location xi, and all other residuals be nonzero. As omega tends to zero the
likelihood is asymptotic to a strictly positive finite constant times
omega^[nu(n-k)-k]. This also holds for alpha=0, including k=n. For nonzero
residuals the CDF argument tends to signed alpha sqrt(nu+1); it never vanishes
for finite alpha. Thus the exponent being negative proves divergence; equality
gives a finite positive path limit, not by itself a nonexistence result.

**Proved, unrestricted all-free fit:** every nonempty finite sample has
unbounded likelihood. Choose xi at any observation (k >= 1), alpha=0, and a
fixed positive nu < k/(n-k) when k<n; when k=n choose any nu>0. Collapse
omega. In particular, a singleton need not have a repeated value: ordinary
non-tied samples are already affected. With xi held the same proof applies if
and only if a datum equals that held xi; the converse here concerns this
construction, not a complete classification of all joint paths.

**Proved, R5 candidate:** nu >= 1 is the weakest constant lower bound that
prevents this fixed-nu collapse divergence for *all* samples whose largest tie
count k satisfies 2k<n. Indeed nu(n-k)-k > 0 then. Any lower bound below 1
fails for sufficiently large even n with k=n/2-1. A sample with 2k>n still
diverges at nu=1; with 2k=n the nu=1 path has a finite nonzero limit, requiring
separate comparison. Restricting nu alone cannot ensure an attained maximum.
The same bound remains appropriate under R5's scope when nu is free and xi is
free, including with omega held, absent a proved exemption. For held xi and
free omega an exemption is a candidate, not established here: a tie at xi gives
unboundedness with unrestricted nu, while absence of such a tie gives the
boundedness result below. Hold-specific existence is still open.

**Proved, boundedness for nu >= 1 and 2k_max <= n.** Uniformly in nu >= 1,
t_nu(z) <= C/(1+z²), for a finite constant C. To see this compare with the
Cauchy density: their ratio is maximized at |z|=1 (or is constant for nu=1),
and the normalizing constants are bounded. Hence f(x) <= 2C omega /
(omega²+(x-xi)²), regardless of alpha. If xi is close to one distinct sample
value, that value has at most k_max observations and all remaining residuals
are bounded away from zero. As omega -> 0 their product is O(omega^(n-2k))
and is bounded. Away from the finitely many sample values all residuals are
bounded away from zero. For omega bounded away from zero all individual
densities are bounded; for omega -> infinity they vanish uniformly. These
bounds also cover xi escaping to infinity. This proves bounded likelihood,
not an attained MLE or a reliable search; at equality collapse can compete.

**Proved, selected held cases.** With omega held positive, f <= 2 t_nu(0)/omega,
uniformly in xi, alpha and nu>0, and t_nu(0) is bounded; hence no unbounded
likelihood, even for ties. With xi held away from all data, maximizing the
individual symmetric t density over omega gives a finite bound proportional
to 1/|x-xi|, uniformly over nu>0; the skew factor is at most 2. Thus the
likelihood is bounded there as well. Neither bound establishes attainment.
All-held parameters give a finite positive likelihood. Other holds, their
exemptions and the boundary attainment questions are open.

## Limits and joint paths

## Candidate positive-integral CDF route (round 2)

For a >= 0 and t >= 0, the lower thin tail at z=-t has the representation

\[
 F(-t;a,\nu)=\frac1\pi\int_0^{\arctan(1/a)}
 \left(1+\frac{t^2}{\nu\sin^2 r}\right)^{-\nu/2}\,dr,
\]

with the upper endpoint pi/2 when a=0. This follows by conditioning the
skew-normal Gaussian wedge on an independent chi-square scaling variable and
using its Laplace transform. The integrand is nonnegative, avoiding a
subtraction of nearly equal CDFs on the thin side. Symmetry of the skewing
factor gives `1-F(t) = 2 T_nu(-t)-F(-t)`, where the subtraction is well
conditioned because the last term is at most `T_nu(-t)` for a >= 0. For
negative a, reflect both the observation and tail. This identity does not by
itself provide a tail-accurate floating-point quadrature algorithm, stable
extreme quantiles, or an implementation error bound.

The accompanying `cdf_wedge.py` compares this integral with independent
integration of the skew-t density in SciPy on 300 seeded parameter points;
seed 20261101, t in [0.01,8], a in [0.05,12], and nu in [0.5,50], drawn
log-uniformly. With absolute quadrature tolerance 1e-100 and relative
tolerances 1e-12 and 1e-11 respectively, the largest observed relative
difference was 8.16e-14. This is a finite numerical comparison, not a test
of a C++ implementation or of extreme tails. A first run using an absolute
tolerance of 1e-13 failed its 1e-7 relative threshold on a probability of
6.5e-14; the tolerance was too coarse to resolve relative error there.

For a numerical fitter, write `h = alpha*z*sqrt((nu+1)/(nu+z*z))`
and `R = t_(nu+1)(h)/T_(nu+1)(h)`. Differentiating the log density at
finite parameters gives

\[
g_z=-\frac{(\nu+1)z}{\nu+z^2}
 +R\frac{\alpha\nu\sqrt{\nu+1}}{(\nu+z^2)^{3/2}},\qquad
s_\xi=-g_z/\omega,\quad s_{\log\omega}=-1-zg_z,\quad
s_\alpha=Rz\sqrt{\frac{\nu+1}{\nu+z^2}}.
\]

These are pointwise algebraic derivatives, not a validated implementation;
the df score also differentiates the Student CDF with respect to its df.
A numerical score check must account for all free coordinates and for the
one-sided condition at the df=1 fitting restriction.

**Proved, fixed xi, omega, nu:** alpha -> +infinity converges weakly to a
translated right half-t, density 2 t_nu(z)/omega for x>xi and zero for x<xi;
alpha -> -infinity gives its reflection. At x=xi every finite-alpha density
equals t_nu(0)/omega, whereas the closed half-t density convention takes twice
that value. Weak convergence does not prescribe a density at a point. At an
observation exactly at xi, comparing the endpoint density of `mlht` without
analyzing the joint xi/alpha approach can overstate the actual path limit.
The right limit is `mlht`; the reflected one is `mlht` on -x, with transformed
location. Held parameters must be mapped accordingly.

**Proved, fixed xi, omega, alpha:** nu -> infinity gives the Azzalini
skew-normal density 2 phi(z) Phi(alpha z)/omega, and weak convergence follows
by Scheffe's lemma. This is the `mlsn` face. With alpha=0, the model is `mllst`.
If nu -> infinity and alpha -> either signed infinity with fixed xi, omega,
the weak limit is the corresponding translated half-normal, again by pointwise
convergence away from xi and Scheffe. The order of the two limits does not
settle paths with moving xi, omega, or observations at the moving boundary.

**Proved, nonuniform endpoint layer:** for nu -> nu0>0, alpha -> +infinity,
z -> 0 with alpha z -> c, the skewing factor tends to
2 T_(nu0+1)(c sqrt((nu0+1)/nu0)), which ranges strictly between 0 and 2.
Thus endpoint pointwise values and moving-boundary likelihoods depend on the
joint rate. Simultaneous nu -> 0, nu -> infinity with moving location/scale,
and all likelihood-relevant joint degeneracies remain open; a list of the
three named faces is not a closure proof.

## Information at symmetry

The prompt's analogy to skew-normal needs qualification. **Proved:** at
alpha=0 and finite nu, the xi and alpha scores, apart from nonzero constants,
are z/(nu+z²) and z/sqrt(nu+z²), respectively. They are not proportional as
functions of z; both have finite second moments under t_nu, so their 2-by-2
information block is positive definite. At the normal limit they become
proportional and the skew-normal direct-parameter information is singular.
This does not assert full four-parameter DQM, rank or MLE asymptotics. See also
Ley and Paindaveine's [generalized skew-elliptical information study](https://orbilu.uni.lu/handle/10993/57896).

## Decision and open work

No fitter is shipped. A finite grid, local optimization, score gate or a
comparison to the existing `sn`/`lst` numerical fits does not certify the
global maximum or exclude a competing joint escape. This is material under
the task's explicit certification rule; it is not a proof that certification
is impossible. A reliable implementation needs (1) a closure theorem for all
joint paths, including nu down to its applicable bound, alpha infinity and
scale collapse; (2) correct endpoint-layer likelihood suprema; (3) a global
upper-bound or monotonicity/unimodality certificate with numerical error
control; (4) hold-by-hold existence and R5 exemption proofs; and (5) stable
tail-certified CDF/quantile evaluation and independent truth tests. The
existing half-t, skew-normal and Student-t fitters' numerical searches are not
such certificates. Penalized likelihood is a different objective, not a
substitute: Azzalini and Arellano-Valle (2013),
[DOI 10.1016/j.jspi.2012.06.022](https://doi.org/10.1016/j.jspi.2012.06.022),
discuss divergent skewness and penalized estimation.
