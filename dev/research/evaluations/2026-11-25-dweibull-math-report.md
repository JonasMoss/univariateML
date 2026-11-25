# Discrete Weibull existence, uniqueness and faces

TASK-345 · 2026-11-25 · Base `f25d97e2`

**Proved.** The existing sample predicates exhaust attainment and multiplicity for every hold mask. The geometric law is attained at shape one. On integer observations, the sign of an exact product comparison determines whether the unrestricted optimum is geometric or on either side of shape one. An exact geometric fit is a single maximizer in the larger model. All-zero observations with the probability held give the only non-singleton set of finite maximizers.

**Numerical.** For the exact sample $(8,10)$, refitting shape at the nearest binary64 probability moves shape by about $6.87\times10^{-7}$ relative to the joint MLE. This exceeds the default $10^{-10}$ coefficient tolerance. The calculation concerns the probability-rounding and refit step in isolation.

## Attainment and maximizer sets

Write $t=-\log q$, $\alpha=\log t$, and let $n_k$ be the sample counts, $n=\sum_k n_k$, $m=\min x$, and $M=\max x$. The model has $0<q<1$ and $\beta>0$. Write $S_k=\Pr(X\ge k)=\exp(-t k^\beta)$ for $k\ge1$, with $S_0=1$. Then $p_k=S_k-S_{k+1}$. The results concern nonempty samples of nonnegative integers and holds inside the stated domains.

**Proved.** The dossier's negative-definite Hessian for each positive cell, together with the concave zero-cell term, gives strict concavity of the joint log likelihood in $(\alpha,\beta)$ as soon as $M>0$. A hold on either parameter fixes one of these coordinates. The prior existence work establishes the predicates below. The quantitative bounds in the next section also give a direct way to localize every attained non-flat optimum. [Dossier proof](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md#why-a-single-profile-bracket-is-global) · [Original existence census](2026-10-29-p0-discrete2.md#dweibull) · [Existence follow-up](2026-11-02-existence-5-report.md#discrete-weibull) · [Transfer analysis](2026-11-25-transfer-math-report.md)

| Held coefficients | Exact sample predicate | Maximizer or supremum and intended outcome |
|---|---|---|
| None | $M-m\ge2$ | Unique finite pair. Return a checked estimate. Example $(0,2)$. |
| None | $M=m$ | Point mass at $m$, with supremum log likelihood $0$. Certified excluded limit, `PointMass`. Examples $(0,0)$ and $(2,2)$. |
| None | $M=m+1$ | The law on $\{m,m+1\}$ with upper mass $\pi=n_{m+1}/n$. Certified excluded limit, `BernoulliLimit` for $m=0$ and `TwoPointLimit` for $m\ge1$. Examples $(0,0,1)$ and $(2,2,3)$, both with $\pi=1/3$. |
| $\beta$ | $M>0$ | Unique finite $q$ for every valid held shape. Return a checked estimate. Example $(2,2)$ at $\beta=1$. |
| $\beta$ | $M=0$ | $q\downarrow0$ gives the point mass at zero and log likelihood $0$. Certified excluded limit, `PointMass`. Example $(0,0)$ at $\beta=1$. This case is attained in the closed `mlgeom` domain at success probability one. |
| $q$ | $M\ge2$ | Unique finite $\beta$ for every valid held probability. Return a checked estimate. Example $(2,2)$ at $q=1/2$. |
| $q$ | $M=1$ | Bernoulli with success probability $q$ and supremum log likelihood $n_0\log(1-q)+n_1\log q$. Certified excluded limit, `HeldBernoulliLimit`. Example $(0,1,1)$ at $q=1/4$. |
| $q$ | $M=0$ | Exact flat likelihood $n\log(1-q)$. Certified non-unique finite estimate, `ShapeUnidentified`, with representative $(q,1)$ and complete set $\{q\}\times(0,\infty)$. Example $(0,0)$ at $q=1/2$. |
| $q,\beta$ | Every valid sample | The singleton consisting of the held pair. Evaluate the log likelihood. |

**Proved face values and strictness.** Every finite model assigns positive probability to every count. A constant sample therefore has finite log likelihood strictly below $0$. For positive common count $m$, taking $t=c^{-\beta}$ for $m<c<m+1$ gives $p_m\uparrow1$ as $\beta\uparrow\infty$. For $m=0$, take $q\downarrow0$ at any fixed $\beta$. For two adjacent occupied cells, write $d=p_m+p_{m+1}<1$ and $w=p_{m+1}/d$. The finite likelihood is
$$
d^n(1-w)^{n_m}w^{n_{m+1}}
<
(1-\pi)^{n_m}\pi^{n_{m+1}}.
$$
The right side is reachable in the limit by setting $t=-\log(\pi)/(m+1)^\beta$ and sending $\beta$ to infinity. Along this path $S_{m+1}=\pi$, $S_m\to1$, and $S_{m+2}\to0$. The limiting log likelihood is $n_m\log(1-\pi)+n_{m+1}\log\pi$. At held $q$ and $M=1$, $p_1=q-\exp(-t2^\beta)$ increases strictly to $q$ and $p_0=1-q$ is fixed. The held Bernoulli success probability is therefore the supplied $q$, even when the observed proportion differs. For free $q$ and $m=0$, the alternate fit is `mlbinom(x, size = 1)`. At $m>0$ the limiting law retains its support offset.

**Proved boundary comparison.** The proper all-free boundary laws are exactly the point masses and the adjacent two-point laws above. For $\beta\to\infty$, two nonadjacent cells $m$ and $M\ge m+2$ cannot both have positive limiting masses. In fact $p_m\le t(m+1)^\beta$ and $p_M\le\exp(-tM^\beta)$ imply that if $p_m$ stays positive, $p_M$ tends to zero. When $\beta\to0$, positive masses vanish uniformly by the bound below. At bounded positive shapes, escaping $t$ tends on subsequences to zero or infinity, which gives loss of mass to infinity or the point mass at zero, respectively. The exhibited paths realize all the proper laws. In the positive-count finite-fit cases every escaping sequence has likelihood tending to zero, by the level-set bounds below. On all zeros at held $q$, the proper Bernoulli limit at $\beta\to\infty$ ties the flat finite maximum because it has the same zero mass. At $\beta\downarrow0$ the pointwise limit also has that zero mass, with the remaining probability escaping to infinity. These facts leave the finite maximizing set exactly the open shape interval.

## Quantitative localization and signed brackets

**Proved.** A finite reference log likelihood $\ell_0<0$ gives an explicit superlevel box. Set $c=\exp(\ell_0)$, so that $\ell\ge\ell_0$ implies $p_k\ge c$ for every occupied cell. For any occupied $k>0$, the interval between $\alpha+\beta\log k$ and $\alpha+\beta\log(k+1)$ has length $\beta\log(1+1/k)$, and its mass equals the integral of $y\mapsto \exp(y)\exp(-\exp(y))$ there. The integrand is bounded by $1/e$, so
$$
\beta\ge\frac{ec}{\log(1+1/k)}.
$$
The two bounds used for the boundary comparison also give
$$
\frac{c}{(m+1)^\beta}\le t\le\frac{-\log c}{M^\beta}
\qquad(M>0).
$$
If $M\ge m+2$, it follows that
$$
\beta\le\frac{\log((-\log c)/c)}{\log(M/(m+1))}.
$$
With $t$ held and $M\ge2$, the upper bound is instead $\beta\le\log((-\log c)/t)/\log M$. A held $\beta$ needs only the bounds on $t$. In a free $\beta$ case these bounds keep $\beta$ in a closed positive interval. Substitution into the bounds on $t$ gives a closed positive interval for $t$ as well. In each finite-fit case the superlevel set is therefore compact and nonempty. The proof allows tighter cell-specific bounds $p_k\ge\exp(\ell_0/n_k)$ and logarithmic evaluation of the formulas.

**Proved score enclosure.** For one positive cell use $A=tk^\beta$, $D=t((k+1)^\beta-k^\beta)$, and $r(D)=D/\operatorname{expm1}(D)$. The contributions to the $\alpha$ and $\beta$ scores in the raw coordinates are
$$
r(D)-A,\qquad
r(D)\frac{(k+1)^\beta\log(k+1)-k^\beta\log k}{(k+1)^\beta-k^\beta}
-A\log k.
$$
The zero cell contributes $r(t)$ and $0$, respectively. Since $r$ decreases strictly, the raw $\beta$ score at each fixed $\beta$ decreases strictly with $t$ when there is a positive count. Evaluating it at both endpoints of a signed inner $t$ bracket therefore encloses the profile score. At fixed $t$, every positive cell's $\alpha$ score decreases strictly with $\beta$ because $D$ increases strictly and $A$ is nondecreasing. Thus the fitted $t(\beta)$ decreases and $q(\beta)$ increases, as in the prior accuracy work. The dossier's negative Schur complement makes the profile score strictly decreasing. A signed outer bracket plus signed inner brackets at its endpoints encloses both public coefficients. A held-coordinate solve uses its corresponding scalar score bracket. These arguments concern signed enclosures in exact arithmetic. [Prior accuracy work](2026-11-20-ctrl-d-report.md)

## Exact decision at the geometric member

**Proved.** At $\beta=1$, $p_k=(1-q)q^k$ for every $k$. Let $T=\sum k n_k$. For $T>0$, maximizing over $q$ on this line gives $q_G=T/(T+n)$ and `mlgeom` success probability $1-q_G$. For held $q$, use that value in the following comparison. Define
$$
B=\sum_{k\ge1}n_k(k+1)\log(k+1),\qquad
A_G=\sum_{k\ge1}n_k k\log k.
$$
Differentiating $S_k-S_{k+1}$ at $\beta=1$ gives the shape score
$$
\partial_\beta\ell(q,1)=\frac{-\log q}{1-q}(qB-A_G).
$$
For the joint fit evaluated at $q_G$, this is the profile derivative. At an attained unique fit with $\beta$ free, its sign is the sign of $\widehat\beta-1$, by strict decrease of the scalar or profile score. At equality the finite maximum occurs at shape one. At either strict sign, movement from the geometric fit in the indicated direction strictly improves its likelihood. Thus equality in optimized geometric and unrestricted likelihood occurs exactly at the same maximizing pair on the geometric line, with no distinct tied fiber. For all-zero data at held $q$, the geometric representative belongs to the flat set. For held $\beta=1$, the scalar geometric fit is exact when $T>0$, and the excluded point-mass result applies when $T=0$.

For rational $q=a/b$ in lowest terms, the sign of $qB-A_G$ is the sign of the comparison
$$
\prod_{k\ge1}(k+1)^{a n_k(k+1)}
\quad\text{versus}\quad
\prod_{k\ge1}k^{b n_k k}.
$$
The rule applies to every exact binary64 hold and to the exact rational $q_G$ from integer data. Compare products exactly, or compare the integer exponent of each prime for equality and then order the remaining products. For arbitrary real holds and positive counts, the only possible contact is $q=A_G/B$, and the score sign increases across that threshold. On the excluded limit cases with a positive count, the finite geometric fit loses strictly by the face proofs.

**Proved examples.** The sample with histogram $(n_0,n_1,n_2,n_3)=(52,12,4,1)$ has $T=23$, $n=69$ and $q_G=1/4$. Here $B=32\log2+12\log3$ and $A_G=8\log2+3\log3$. The joint maximizer is precisely $(1/4,1)$. Changing only the zero multiplicity to $51$ or $53$ gives strictly larger or smaller fitted shape, respectively. With histogram $(n_1,n_2,n_3)=(12,4,1)$ and $q$ held, $1/4$ gives the unique shape-one fit, and its immediate binary64 neighbors give opposite strict signs. The sign statements use $B>0$ and the rational identity, so they also decide the cases where floating-point subtraction nearly cancels.

## Numerical evidence and coefficient restoration

**Numerical.** The workstation replay used mpmath at 90 decimal digits, with signed nested bisection for the finite fits. It checked the three geometric histograms above and the two binary64 neighbors and exact center at held $q$. The fitted shapes for zero multiplicities $51,52,53$ were approximately $1.00693307979613$, $1$, and $0.993262808936063$. The held shape scores at the lower and upper neighbors were about $-1.81\times10^{-15}$ and $3.63\times10^{-15}$. It also checked the strict likelihood gaps along the Bernoulli path at $\beta=2,4,6$, the shifted adjacent path on $(2,2,3)$ at $\beta=2,8,32,128$, and the point-mass path on $(2,2,2)$ at $\beta=4,16,64$. The held comparison used $(0,1,1)$ at $q=1/3$, and the flat-likelihood check used $(0,0,0)$ at the same hold.

**Numerical.** The restoration grid used pairs $(k,k+2)$ for $k=5,6,7,8,9,10,12,15$. For $(8,10)$ the continuous MLE has $t\approx2.73552921524930\times10^{-12}$ and $\beta\approx11.5970191669438$. Rounding $q$ gives `0x1.fffffffff9fc1p-1`. Maximizing at this fixed probability gives $\beta\approx11.5970271326355$. The relative shape displacement is $6.86874\times10^{-7}$, despite the very small relative probability error. For $(12,14)$ and $(15,17)$ the joint probability rounds to one. The current source rounds $q$ and conditionally refits shape when $t<10^{-6}$, so a certificate for the joint root does not automatically certify the refitted public pair. The replay evaluates this rule on high-precision roots and provides no runtime trace of the C++ solver. [Fitter source](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/mle.hpp)

The final Python replay exited zero on the workstation with one CPU slot. [Replay script](2026-11-25-dweibull-math-data/check.py)

```sh
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-25-dweibull-math-data/check.py
```

## What this changes for the fitter

- Keep the exact range and hold dispatch and its certified limits. Use the empirical upper mass for free adjacent fits and the supplied probability for the held Bernoulli fit. Retain the full open shape interval as the typed maximizer set for the held-probability, all-zero case. A shape-one finite maximum is an ordinary discrete Weibull estimate.
- An exact geometric test can supply a shape-one return or the sign of the profile at zero log shape when useful. Retain the signed bracket on ordinary cases where the exact product comparison would exceed the budget. Never use an approximate zero to declare exact geometric membership.
- For coefficient certification, carry signed inner brackets into the outer score and use monotone endpoint propagation. Use the bounded superlevel set as an analytic alternative to an arbitrary maximum search radius. Compare the **returned** public vector against the resulting coefficient endpoints. A small step, a rounded score zero, or a refit at rounded $q$ is insufficient on its own. The $(8,10)$ case tests the refit check at the default tolerance.
- Use `certified = TRUE, unique = TRUE` for a verified singleton and `certified = TRUE, unique = FALSE` for the proved flat set. If certification runs out of budget, retain an otherwise successful checked search fit as uncertified under the fallback rule. If a returned pair is proved outside tolerance, seek a public pair meeting the coefficient check or report the applicable accuracy failure. Distinguish this from the proved sample-based limit statuses and from an actual failed search. [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

## Open

- Implement and measure floating-point score margins for nested brackets and public-coordinate restoration, including rounded score zeros, exact geometric contact and small $t$. The bounds give the exact-arithmetic checks. The replay gives examples for calibration, with no uniform floating-point error bound.
- For a production geometric fast path, benchmark exact equality and strict sign on large counts and dyadic holds with large denominators. Direct products decide both but can require enormous integers. A practical path can cancel common factors and use guarded log comparisons for the strict sign.
- Decide the behavior when the exact $q$ rounds to an excluded endpoint. An interior neighboring double may meet a relative error check on $q$ while badly perturbing $-\log q$ and the likelihood. A production resolution needs an explicit restoration policy and checks on the returned likelihood, or a separate public parametrization. This is a representation question on proved finite-fit samples.
