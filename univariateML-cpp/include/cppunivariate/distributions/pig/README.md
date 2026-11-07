---
id: pig
inference_numeric:
  - regime: "regular finite interior"
    available: true
    condition: "finite mean > 0 and finite shape > 0; any free subset; zero mass and Poisson faces excluded; analytic piecewise natural-coordinate derivatives; successful interior fit and nonsingular information required"
    classification: "dev/research/evaluations/2026-11-04-dqm-c-report.md#e-poisson-inverse-gaussian"
name: Poisson-inverse Gaussian
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: mean
  symbol: '\mu'
  role: mean
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape
  symbol: '\lambda'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dpig
  sampler: rpig
  control:
    reltol: "Requested log-parameter tolerance; the general shape certificate uses width max(reltol, 2e-7)."
    iterlim: "Solver work limit; count recurrences are limited to 100000 times min(iterlim, 100000) cells."
log_density: "log(2*shape/pi)/2 - loggamma(x+1) + shape/mean + (x-1/2)*log(shape/(2+shape/mean**2))/2 + log(besselk(x-1/2, sqrt(shape*(2+shape/mean**2))))"
limits:
  - model: pois
    firmness: proved
    when: 'The concentration $\lambda$ tends to infinity with $\mu>0$ fixed.'
    detect: 'For a sample containing a positive count, $v+(\bar x-\mu)^2\le\bar x$, with $v=n^{-1}\sum_i(x_i-\bar x)^2$ and $\mu$ the held mean or $\bar x$ when free. Equality selects the Poisson supremum; an unresolved floating-point sign stops as a tie.'
    status: PoissonLimit
  - model: pois
    firmness: proved
    when: 'The mean $\mu$ tends to zero with positive concentration $\lambda$ held.'
    detect: All observations are zero, $\mu$ is free, and $\lambda>0$ is held.
    fixed: [shape]
    status: ZeroLimit
references:
- 'Willmot, G. E. (1987). The Poisson-inverse Gaussian distribution as an alternative to the negative binomial.
  Scandinavian Actuarial Journal, 1987(3-4), 113-127. DOI: 10.1080/03461238.1987.10413823.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlpig())
  mlpig(x)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: pois
    at:
      lambda: mix
    mixed: lambda
    mixing:
      family: invgauss
      at:
        mean: mean
        shape: shape
    when: shape > 0
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'Every free subset has a global certificate: strict compact-coordinate shape concavity, a monotone held-shape mean score, or direct evaluation. Padded signs enclose the unique finite root or select the proved Poisson face; unresolved arithmetic, representability and work budgets stop with witnesses. Exact-arithmetic certificate conditional on computed comparisons with stated practical floating-point margins.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model insurance claim frequencies with inverse-Gaussian heterogeneity in intensity.
summary: Poisson-inverse Gaussian provides a model for insurance claim frequencies with inverse-Gaussian heterogeneity
  in intensity.
aliases:
- "PIG"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free and positive sample mean, a finite MLE
    exists and is unique exactly when the variance with divisor $n$ exceeds
    the sample mean. Otherwise the Poisson supremum is unattained.
    With positive mean $\mu$ held and any positive count, the finite shape
    maximum exists and is unique exactly when
    $v+(\bar x-\mu)^2>\bar x$; equality also selects the unattained Poisson
    supremum. With positive shape held and any positive count, the mean
    maximum always exists and is unique. All-zero data with free shape
    attain the zero mass at $\lambda=0$, leaving a free mean unidentified;
    positive held shape instead gives an unattained mean-zero supremum.
    With $\lambda=0$ held only all-zero data are compatible, and a free
    mean is unidentified. Holding both coefficients leaves evaluation.
  reason: >-
    At fixed $z=\sqrt{2\lambda+\lambda^2/\mu^2}$ the unique maximizing
    mean is $\bar x$. The Bessel-polynomial degree variance satisfies
    $V_k<E_k(k^2-E_k^2)/(k^2+E_k^2)$, proving strict shape-profile
    concavity in the compact coordinate $w$. Its inward Poisson score
    gives the existence criteria. The weaker bound $V_k\le E_k$ proves
    monotonicity and uniqueness of the held-shape mean score. The
    derivations and floating-point contract are given below.
---

The Poisson-inverse Gaussian distribution mixes a Poisson count with an inverse Gaussian mean. The coefficient $\mu$ is the mixing mean; smaller positive $\lambda$ gives more mixing variation. With both coefficients free, the fitted mean is exactly the sample mean.

## Zero member and limit faces

Shape zero is the attained zero point mass. All-zero data with free shape return $\lambda=0$, preserving held $\mu$ or choosing $\mu=1$ when it is unidentified. This supplies a counterexample to joint uniqueness: $(\mu,\lambda)=(1,0)$ and $(2,0)$ have the same maximum. With positive shape held and mean free, all-zero data instead have the unattained mean-zero supremum. With both coefficients held the likelihood is evaluated directly.

For positive samples the Poisson face wins exactly under the criterion in the existence statement, including equality. The infinite-mean Poisson–Levy face cannot win when mean is free: at fixed $z$, writing $t=\lambda/\mu$ and $\lambda=(z^2-t^2)/2$ leaves $nt+S\log\lambda$ as the $t$-dependent likelihood, with positive inward derivative at $t=0$ and a unique maximum giving $\mu=\bar x$. With positive shape held its inward derivative is also positive. These reductions and the mixing limits are derived in the [original likelihood analysis](../../../../../dev/research/evaluations/2026-10-29-pig/analysis.md); its historical numerical-search and external-density remarks are superseded here.

## Shape-profile certificate

For $k\ge1$ define the positive half-integer Bessel polynomial

$$
Q_{k-1}(y)=\sum_{j=0}^{k-1}
 \frac{(k-1+j)!}{j!(k-1-j)!2^j}y^j.
$$

Let $E_k(z)$ and $V_k(z)$ be the mean and variance of $J$ under weights proportional to the terms of $Q_{k-1}(1/z)$; set $E_0=V_0=0$. For $k\ge2$, $c=k(k-1)$, the polynomial differential equation gives

$$
\frac{dE_k}{d\log z}=-V_k,\qquad
V_k=c-(2z+1)E_k-E_k^2.
$$

The sharper variance bound needed here is

$$
V_k<b(E_k),\qquad b(E)=\frac{E(k^2-E^2)}{k^2+E^2}.
$$

To prove it, put $f=V_k-b(E_k)$. At any zero of $f$,

$$
z f'(z)=\frac{2E_k k(E_k+k)^2
 (E_k^3+E_k^2k-E_k k^2+k^3)}{(E_k^2+k^2)^3}>0,
$$

because $0<E_k<k-1<k$. But the finite polynomial expansion gives
$f=-c/(2z^2)+O(z^{-3})<0$ at infinity. A crossing back to negative would be impossible, so $f<0$ everywhere.

With mean fixed, write

$$
w=\frac{\sqrt{1+2\mu^2/\lambda}-1}
        {\sqrt{1+2\mu^2/\lambda}+1},\qquad
z=\frac{\mu(1-w^2)}{2w},\qquad 0<w<1.
$$

For $k\ge1$ the log mass is $-\mu(1-w)+k\log(\mu(1-w)/(1+w))+\log Q_{k-1}(1/z)-\log(k!)$. For $k\ge2$ the variance bound implies, with $r=E_k/k$,

$$
\frac{(1-w^2)^2}{k}\frac{d^2\log p_k}{dw^2}
<\frac{2(r-w)^2(rw^2-r-2w)}{w^2(1+r^2)}\le0.
$$

For $k=1$ the scaled second derivative is $-4w<0$, and for $k=0$ it is zero. Thus the total log likelihood is strictly concave whenever a count is positive. Its one-sided score at $w=0$ is

$$
n\mu-2S+\frac{\sum_i x_i(x_i-1)}{\mu},\qquad S=\sum_i x_i.
$$

The score tends to minus infinity at $w=1$, where the likelihood also tends to minus infinity. These signs prove the stated existence and uniqueness criteria, and exclude every competing peak and both unsearched tails. With both coefficients free, the fixed-$z$ mean reduction completes the joint certificate. The [expanded proof and symbolic checks](../../../../../dev/research/evaluations/2026-11-06-pig-cert-data/concavity-proof.md) retain the crossing argument and factorization.

The implementation uses $\log\lambda$, starting at $2\log\mu$ clipped to representable positive shapes. It expands a bracket with doubling log-steps, then bisects signed score points until the log-width is at most $\max(\mathtt{reltol},2\times10^{-7})$. A point inside that verified box is polished to the requested solver resolution without leaving the enclosure. The global uncertainty remains the certified box width; the polished point preserves the existing golden accuracy. Raw score signs only choose a point within the verified box; they do not replace its global certificate. Solver exhaustion during polishing stops. A small residual alone never supplies the global certificate.

Put $q=(1-w)/(1+w)$ and $\bar E=n^{-1}\sum_i E_{x_i}$. The evaluated score has the sign of

$$
F=\frac{2\mu q}{(1+q)^2}-\bar x+
  \frac{1+q^2}{1-q^2}\bar E
 =\frac{1-w^2}{2n}\ell'(w).
$$

For $q\ge1/2$ the implementation evaluates $\mu F$, using $z\bar E$ instead of dividing a small $\bar E$ by $1-q^2$. The positive ratio recurrence $Q_m/Q_{m-1}=(2m-1)/z+Q_{m-2}/Q_{m-1}$ streams both moments without large coefficients or logarithm subtraction. The scaled moment retains its nonzero Poisson limit even when $1/z$ underflows. For $q<1/2$, products involving a very large mean are evaluated through logarithms. Aggregated moments and likelihoods use compensated sums; count multiplicities use 64-bit integers.

## Held-positive-shape mean certificate

For fixed $\lambda>0$ and a positive sample, put $t=\sqrt{z^2-2\lambda}$, $q=t/z$ and $D(z)=S+\sum_i E_{x_i}(z)$. Apart from constants the likelihood is $n(t-z)-S\log z+\sum_i\log Q_{x_i-1}(1/z)$, with zeros contributing no polynomial term. Its stationary equation is

$$
zD(z)=\frac{2n\lambda}{q(1+q)},\qquad
\mu=\frac{D(z)}n\frac{1+q}{2}.
$$

The right side of the first equation strictly decreases with $z$; the left side strictly increases. Indeed $V_k\le E_k$: the successive factorial-weight ratio $(j+1)a_{j+1}/a_j=(m+j+1)(m-j)/2$, $m=k-1$, decreases with $j$, so its covariance with $J$ is nonpositive and $E[J(J-1)]\le E[J]^2$. Thus $d(zD)/dz=D-\sum V_k\ge S>0$. Endpoint signs give exactly one crossing and a unique maximum. Since $S\le D<2S$, its mean lies strictly between $\bar x/2$ and $2\bar x$.

The solve bisects $[\bar x/4,2\bar x]$ in $\log\mu$, widening the lower endpoint to protect tiny-shape fits. It requires a signed root enclosure of width at most `reltol`.

For binary counts with held mean the shape solve has a closed form: $\ell'(w)=n\mu-2S/(1-w^2)$, so $\mu\le2\bar x$ selects Poisson and otherwise $w=\sqrt{1-2\bar x/\mu}$, $\hat\lambda=2\bar x^2/[w(1+w)^2]$. An FMA product residual resolves rounded equality and preserves a small positive numerator at this boundary. This exact-product comparison requires $n\le2^{52}$; larger binary-count samples stop numerically.

## Numerical contract and work

These are exact-arithmetic certificates conditional on computed comparisons, with practical floating-point margins; no outward-rounded intervals are claimed. A shape score comparison uses $256\epsilon(1+\sqrt{\max x})$ times the sum of the absolute evaluated contributions plus the smallest positive normal double. The held-shape mean score uses $256\epsilon(1+|\log(\bar x/4)|+|\log(2\bar x)|+\max x)$. An uncertain midpoint requires two separately signed nearby points, or causes a numerical stop. The Poisson-boundary sign uses exact nonnegative integer products when they fit the stated binary64 range, or a $256\epsilon$ scaled allowance; uncertain boundary signs stop as ties with the Poisson witness. No likelihood-gain tolerance selects a face, so small proved finite improvements are retained.

Each iterative solve uses at most `min(iterlim, 100000)` solver iterations, including shape bracketing and bisection. A needed count recurrence exceeding `100000 * min(iterlim, 100000)` cells stops on budget before scanning the range. Analytic Poisson faces need no count recurrence. The largest representable integer count stops numerically if an interior mass recurrence is required, protecting its existing integer endpoint; a Poisson face can still be evaluated there. Each moment evaluation costs $O(\max x)$ time and constant additional storage. The final likelihood retains the settled log-mass recurrence and its $O(\max x)$ storage; Poisson faces use the existing stable Poisson log-pmf helper.

Core results retain candidates and global bounds on limit, tie and budget stops. Numerical or iteration stops retain any evaluated finite witness. Bounds on stopped finite candidates describe evaluated likelihoods, not a claim that those candidates maximize the model. An unclosed global bound remains infinite. Both R and JavaScript transport the pig-specific payload, with held-mean Poisson advice distinguishing the constrained face from an unrestricted `mlpois` fit.
