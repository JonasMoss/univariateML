# Poisson–lognormal counts

**Status:** banked idea, 2026-11-11. Not scheduled; no implementation
authorized by this entry. The density looks settled in principle; the open
work is the ML existence study. Zero-truncated fits, which ecologists use,
come from the [zero and truncation modifiers](2026-11-11-zero-and-truncation-modifiers.md).

## Model

$X\mid T\sim\mathrm{Pois}(e^T)$, $T\sim N(\mu,\sigma^2)$, so

$$
p_k=\frac{1}{k!\,\sigma\sqrt{2\pi}}\int_{-\infty}^{\infty}
 \exp\Bigl(kt-e^t-\frac{(t-\mu)^2}{2\sigma^2}\Bigr)\,dt .
$$

With $m=e^{\mu+\sigma^2/2}$, $\operatorname{Var}X=m+m^2(e^{\sigma^2}-1)$:
the negative binomial's quadratic form with $e^{\sigma^2}-1$ for $1/r$, but
a lognormal upper tail. Two coefficients, $\mu\in\mathbb R$, $\sigma\ge0$;
$\sigma=0$ is the Poisson member.

## Why it belongs here

- The most used overdispersed count model the package lacks. Species
  abundance (Preston's lognormal, Bulmer 1974), Poisson GLMMs with an
  observation-level random effect, and the marginal of the multivariate
  PLN used for microbiome and RNA-seq counts.
- No Bessel functions; the special-function seam needs only Lambert W.
- An honest open existence and uniqueness question for the
  [ML-difficulties paper](2026-11-02-ml-difficulties-paper.md).

## Density: what is known (2026-11-11 checks)

Evidence: high-precision comparison against adaptive quadrature in double
precision, scripts in
[2026-11-11-pln-quadrature-data](../research/evaluations/2026-11-11-pln-quadrature-data/).
Not yet a proof or an mpmath truth table.

**No route besides quadrature.** The factorial-moment series
$p_k=\sum_j(-1)^jE[\Lambda^{k+j}]/(k!\,j!)$ diverges, since
$E[\Lambda^r]=e^{r\mu+r^2\sigma^2/2}$. Integration by parts does not close into
a recurrence in $k$ (each step creates $\int t(\cdots)$). Large-$k$ Laplace and
small-$\sigma$ expansions miss the small-count, moderate-$\sigma$ regime that
ecological data occupy. The pmf is a derivative of the lognormal Laplace
transform, which has no closed form.

**Peak.** The log integrand has second derivative $-e^t-1/\sigma^2<0$, so it is
strongly log-concave with a closed-form mode

$$
t^\ast=\mu+k\sigma^2-W\bigl(\sigma^2e^{\mu+k\sigma^2}\bigr),
$$

and right-of-mode curvature at least $e^{t^\ast}+1/\sigma^2$. For large
$\mu+k\sigma^2$ the argument overflows (observed at $k=5000$, $\mu=\sigma=2$);
solve $u+\log u=\log\sigma^2+\mu+k\sigma^2$ instead.

**Rule.** 40-node Gauss–Hermite centred at $t^\ast$ with scale
$h=(e^{t^\ast}+1/\sigma^2)^{-1/2}$ stalls at about $2$–$4\times10^{-10}$ in
log pmf for small $k$ and moderate or large $\sigma$ (skewed integrand). The
plain trapezoid rule on $[t^\ast-9\sigma,\ t^\ast+9h]$ reaches
$10^{-15}$ with 80–160 points in all five tested cases, including those two.

**Certificate sketch (not written out).** The integrand is entire and decays
on both sides, so the trapezoid rule converges geometrically
(Trefethen and Weideman 2014). On the strip $\lvert\operatorname{Im}t\rvert<a$,
$\lvert e^{-e^{t+ia}}\rvert=e^{-e^t\cos a}$ needs $a<\pi/2$, and the Gaussian
factor costs $e^{a^2/(2\sigma^2)}$; for large $k$ the factor $(\cos a)^{-k}$
suggests $a$ of order $\min(\pi/4,\sigma,k^{-1/2})$. Truncation is bounded by
the Gaussian tails implied by the curvature bounds above. Together these could
give an a priori pmf error bound with no library quadrature (and no Boost
exception policy) involved.

**CDF.** $P(X\le k)=E[\text{Poisson CDF at }k\text{ with rate }e^T]$, again
one smooth integral; tail accuracy needs the complementary regularized gamma.

## Derivatives (heat equation)

$p_k$ is $e^{kt-e^t}/k!$ smoothed in $\mu$ by a Gaussian of variance
$\sigma^2$, so it solves the heat equation in $(\mu,\sigma^2)$:

$$
\begin{aligned}
\partial_\mu p_k&=k\,p_k-(k+1)\,p_{k+1},\\
\partial_{\sigma^2}p_k&=\tfrac12\partial_\mu^2p_k
 =\tfrac12\bigl[k^2p_k-(2k+1)(k+1)\,p_{k+1}+(k+1)(k+2)\,p_{k+2}\bigr].
\end{aligned}
$$

Both identities hold to about $10^{-10}$ under central differences at
$k=4$, $\mu=0.5$, $\sigma=0.9$; they are also a two-line derivation. Score and
Hessian need only $p_k,\dots,p_{k+4}$. At $\sigma=0$,
$\partial_{\sigma^2}\log p_k=\tfrac12[(k-m)^2-k]$ with $m=e^\mu$, so at
$m=\bar x$ the boundary derivative of the likelihood is $n(v-\bar x)/2$
($v$ with divisor $n$): the same local Poisson criterion as `nbinom` and `pig`.

## ML: open questions

1. **Global existence.** Is $v>\bar x$ necessary and sufficient for a finite
   MLE, as for `nbinom` and `pig`? Only the local direction is shown. Equality
   needs higher-order terms.
2. **Uniqueness.** For fixed $\sigma$, $\ell$ is strictly concave in $\mu$
   (each $p_k$ is a convolution of log-concave functions, Prékopa), so each
   $\mu$-profile has one maximum. Joint concavity, or concavity of the
   $\sigma$-profile in some coordinate, is unknown.
3. **No mean reduction.** The fitted mean is not $\bar x$: on
   $x=(0,0,0,1,1,2,3,3,5,9,14)$, $\bar x=3.4545$, $v=17.70$, the exact fit is
   $\mu=0.626753$, $\sigma=1.173365$, mean $3.7253$, log likelihood
   $-26.18438$; `poilog::poilogMLE` agrees to about six digits. The problem
   stays two-dimensional.
4. **Other faces.** All-zero data send $\mu\to-\infty$ (point mass at zero).
   As $\sigma\to\infty$, $p_k\approx1/(k\sigma\sqrt{2\pi})$ for $k\ge1$, so
   samples with a positive count should not escape there (heuristic).
5. **Inference.** $\sigma^2=0$ is a boundary: the Poisson-versus-PLN LR test
   has a 50:50 chi-bar-squared null. Relevant to the
   [inference plan](2026-10-30-inference-and-dqm.md).

## Oracles

- `poilog::dpoilog` / `poilogMLE` (exact, univariate): independent, about
  six-digit agreement here; not a golden source.
- mpmath truth tables with the Lambert-W centred integral.
- `PLNmodels` is **not** an oracle: it maximizes a variational lower bound
  (the reported `loglik` is the ELBO). A univariate ELBO fit of the sample
  above gives $\mu=0.6404$, $\sigma=1.1427$, ELBO $-26.3225$.

## Trigger to revisit

A free slot for a new count family, or the ML-difficulties paper wanting a
two-dimensional count model without an exact mean reduction.

## Verification when resumed

- Write the trapezoid error bound and test it against mpmath at extreme
  $(k,\mu,\sigma)$, including $k\ge10^5$ and $\sigma\to0$.
- Existence study: random and adversarial samples near $v=\bar x$, profile
  scans in $\sigma$, comparison with the Poisson face.
- Derivative identities against mpmath differentiation.

## References (from memory; bibliographic identities not yet verified)

- Preston, F. W. (1948). The commonness, and rarity, of species. Ecology.
- Bulmer, M. G. (1974). On fitting the Poisson lognormal distribution to
  species-abundance data. Biometrics.
- Aitchison, J. and Ho, C. H. (1989). The multivariate Poisson-log normal
  distribution. Biometrika.
- Asmussen, S., Jensen, J. L. and Rojas-Nandayapa, L. (2016). On the Laplace
  transform of the lognormal distribution. Methodology and Computing in
  Applied Probability.
- Trefethen, L. N. and Weideman, J. A. C. (2014). The exponentially
  convergent trapezoidal rule. SIAM Review.
- Grøtan, V. and Engen, S. R package `poilog`.
