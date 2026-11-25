# Poisson–inverse Gaussian MLE existence and uniqueness

TASK-343 · 2026-11-25 · Base `f25d97e2`

**Proved.** With both coefficients free and at least one positive count, a finite MLE exists exactly when the variance with divisor $n$ exceeds the sample mean. It is unique whenever it exists. Equality gives an unattained Poisson supremum. The only nonunique finite maximizer set in the supported masks is the shape-zero ray on all-zero data with a free mean. There is no positive-count finite/Poisson tie.

The [October likelihood analysis](2026-10-29-pig/analysis.md) proved the fixed-Bessel-argument mean reduction and the all-free Poisson decision. The [November certificate](2026-11-06-pig-cert-report.md) already established strict shape concavity, the held-mean criterion and held-positive-shape uniqueness. The [expanded proof](2026-11-06-pig-cert-data/concavity-proof.md) and [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md) give the underlying inequalities. The results below use those proofs and add a joint boundary classification, exact predicates on represented holds and an arithmetic seam example. The domain is the ordinary untruncated count law with $0<\mu<\infty$ and $0\le\lambda<\infty$.

## Existence and uniqueness by held mask

Write $S=\sum_i x_i$, $F=\sum_i x_i(x_i-1)$, $m=S/n$ and $v=n^{-1}\sum_i(x_i-m)^2$.

**Proved via the prior certificate.** For $S>0$ and both coefficients free, condition on $z=\sqrt{2\lambda+(\lambda/\mu)^2}$. In $t=\lambda/\mu\in(0,z)$ the dependent part of the log likelihood is $nt+S\log((z^2-t^2)/2)$. Its derivative is $n-2St/(z^2-t^2)$, strictly decreasing from $n$ to minus infinity. The root has $\lambda/t=m$. Every $z>0$ is available along $\mu=m$, since $z$ runs strictly from zero to infinity as shape increases. The joint problem therefore reduces exactly to the shape problem with mean $m$.

For fixed $\mu>0$, use the prior compact shape coordinate $w=(\sqrt{1+2\mu^2/\lambda}-1)/(\sqrt{1+2\mu^2/\lambda}+1)$. It decreases from one to zero as shape increases. The cited Bessel polynomial bound proves strict negative curvature in $0<w<1$ for each positive-count log mass. Zeros contribute a linear term. The total log likelihood extends to Poisson at $w=0$, tends to minus infinity as $w\uparrow1$ for $S>0$, and has inward score
$$
g_0=n\mu-2S+F/\mu.
$$
If $g_0>0$, some interior point strictly beats Poisson, and the endpoint behavior and strict concavity give one interior maximizer. If $g_0\le0$, the strictly decreasing score implies every interior value is strictly smaller than Poisson, including when $g_0=0$. The shape-zero member has likelihood zero on positive data. Multiplying the score by $\mu>0$ gives the sign of $n\mu^2-2S\mu+F$, which is also the sign of $v+(\mu-m)^2-m$. For the joint fit the equivalent integer test is $nF>S^2$.

For fixed $\lambda>0$ and $S>0$, let $q=t/z$ and $D(z)=S+\sum_i E_{x_i}(z)$, with $E_k$ the prior proof's mean Bessel polynomial degree. The stationary equation is $zD(z)=2n\lambda/[q(1+q)]$. By the prior degree-variance bound, $(zD)'=S+\sum_i(E_{x_i}-V_{x_i})\ge S>0$. The right side strictly decreases as $z$ increases from $\sqrt{2\lambda}$ to infinity, from infinity to $n\lambda$. The left side has a finite lower-end value and diverges at the upper end. The likelihood derivative with respect to $z$ is the right side minus the left side, divided by $z^2$. The crossing therefore gives the unique maximum, with $m/2<\hat\mu<2m$ from $\hat\mu=D(z)(1+q)/(2n)$.

**Proved for zero counts.** At positive shape write $d=z-t$. The zero mass is $e^{-d}<1$. At shape zero the zero mass is one for every positive mean. With positive shape held, $d$ decreases to zero as $\mu\downarrow0$, giving the excluded Poisson-zero endpoint. With both coefficients held, direct evaluation reduces the domain to a singleton. The public fitter treats shape zero with any positive count as incompatible. The fixed law then has log likelihood minus infinity.

## Joint paths to the boundary

**Proved.** The two positive interior coordinates $d=z-t$ and $t=\lambda/\mu$ range independently over $(0,\infty)$. The inverse map is $\lambda=d(t+d/2)$ and $\mu=d+d^2/(2t)$. Integrating the conditional Poisson pgf against the inverse Gaussian, or taking the ratio of its mixing normalization integrals after adding $1-s$ to the rate, gives
$$
G(s)=\exp\{-h(1-s)\},\qquad
h(a)=\sqrt{t^2+a\,d(d+2t)}-t,\quad 0\le a\le1.
$$
Concavity and monotonicity give $ad\le h(a)\le d$ since $h(0)=0$ and $h(1)=d$. Thus $d\to0$ forces the zero law, uniformly in $t$, since $p_0=e^{-d}$. For $d\to\infty$ every fixed mass tends to zero, since $p_k s^k\le G(s)\le e^{-(1-s)d}$ for any $0<s<1$.

If $d\to d_0\in(0,\infty)$, rationalizing the square root shows that $t\to\infty$ yields $h(a)\to ad_0$, giving Poisson at $d_0$. If $t\to0$, $h(a)\to d_0\sqrt a$, giving Poisson–Lévy with limiting shape $d_0^2/2$. If $t$ stays convergent to a strictly positive finite value the limit is an interior member. The pgf limits give the claimed fixed-count mass limits: every subsequence has a further subsequence converging coordinatewise in $[0,1]$, and the $s^k$ bound for $s<1$ permits summing along it. Equality of power series then identifies every coordinate of the limit. Subsequence compactness of the extended nonnegative axes exhausts all simultaneous escapes.

For $S>0$, zero mass and escape of mass to infinity both have limiting likelihood zero. The Poisson–Lévy face, when available, loses strictly. With both coefficients free, start at $t=0$ and $z=d_0$ and vary $t$ at that $z$. The fixed-$z$ derivative at the endpoint is $n>0$. With positive shape held, use $z=\sqrt{2\lambda+t^2}$. In the log mass for any count, the dependence other than $t$ comes through the smooth function of $z$ in the Bessel formula. Since $dz/dt=0$ at $t=0$, the sample log likelihood again has inward derivative $n$. A held finite mean excludes this face because $d\le\mu$ and $t\to0$ with $d\to d_0>0$ would force $\mu\to\infty$.

Consequently, for a positive sample, every maximizing sequence converges to the unique finite maximizer when one exists. On a Poisson decision every maximizing sequence satisfies $t\to\infty$ and $d\to m$ with both coefficients free, or $d\to\mu$ with mean held. This follows from the exhaustive limits and strict exclusions above, plus the unique Poisson rate maximizer at $m$ in the joint fit. Equivalently $\lambda\to\infty$ and the mean tends to that winning rate. For all-zero data any positive-shape maximizing sequence has $d\to0$. With both coefficients free this can happen even if the mean diverges, for example at $d=1/j$ and $t=1/j^3$ as $j\to\infty$. All such paths have the same limiting law as the attained shape-zero ray.

## Exact predicates and examples

**Proved.** Accumulate $n,S,F$ as integers. For a held mean $a/b$ in lowest terms with $a,b>0$, including the exact dyadic represented by any finite positive binary64 hold, compare
$$
n a^2+F b^2\quad\text{with}\quad 2Sab.
$$
This determines the sign including equality without looking at likelihood differences. When $v<m$ the Poisson region is the intersection of the allowed mean domain with $[m-\sqrt{m-v},m+\sqrt{m-v}]$. When $v=m$ the region consists of $m$ alone, and when $v>m$ every positive held mean gives a finite maximum.

| Exact sample | Hold | Proved outcome |
|---|---|---|
| $(1)$ | None | Poisson at one, with $v<m$ |
| $(0,2)$ | None, or mean one | Poisson at one, with exact boundary equality. Using variance with divisor $n-1$ would give a false finite decision. |
| $(0,3)$ | None | Unique finite fit at mean $3/2$, with $v=9/4>3/2$ |
| $(0,2)$ | Mean two | Unique finite fit at shape one despite $v=m$. In the shape score, $w=1/2$ gives $z=3/2$, $E_2=2/5$ and an exact zero. |
| $(1)$ | Mean two or its lower binary64 neighbor | Poisson at the held mean |
| $(1)$ | Mean $2+2^{-51}$, the upper neighbor of two | Unique finite shape, from $w=\sqrt{1-2/\mu}$ |
| $(1)$ | Shape one | Unique finite mean, despite $v<m$ |
| $(0)$ | Neither, or shape zero | The entire set $(0,\infty)\times\{0\}$, including $(1,0)$ and $(2,0)$ |
| $(0)$ | Mean two, shape free | Unique attained point $(2,0)$ |
| $(0)$ | Shape one, mean free | Unattained Poisson-zero supremum |
| $(0)$ or $(1)$ | Mean two and shape one | Direct evaluation |
| $(1)$ | Shape zero, with mean free or held | Incompatible held law |

The all-zero ray is a structural tie on integer data in the [tie classification](2026-11-25-ties-math-report.md#classification-of-the-constructions). The equality $v=m$ on positive data is a score equality at an excluded endpoint, with strict pointwise dominance by Poisson. It gives neither distinct attained maximizers nor a finite/face tie.

For $(0,0,0,0,0,1,1,2,2)$, $(n,S,F)=(9,6,4)$, so the joint test is exact equality. The held test polynomial is $(3\mu-2)^2$. At the binary64 rounding of $2/3$, `0x1.5555555555555p-1` or $6004799503160661/9007199254740992$, it is exactly $2^{-106}>0$. The held fit is strictly finite, while the joint fit escapes. Comparing the held mean with the rounded sample mean cannot identify the appropriate profile. In the checked revision, the padded held boundary test is unresolved here, and the small-$w$ score path selects its constant term by `mean==data.average`. That condition would also erase the positive held score if the exact boundary test alone were added. This is a source audit of the arithmetic expressions.

A local expansion quantifies this distinction. The first two Bessel polynomial coefficients give $\log Q_{k-1}(1/z)=k(k-1)/(2z)-k(k-1)/(4z^2)+O(z^{-3})$ for $k\ge1$, and the zero mass contributes no polynomial. Since $1/z=2w/\mu+O(w^3)$, $\ell''(0)=-2F/\mu^2$. Near a held-mean score equality with $F>0$ this gives $\hat w\sim g_0\mu^2/(2F)$ and gain $\sim g_0^2\mu^2/(4F)$ on the positive-score side, by Taylor expansion around the boundary root with nonzero derivative. For binary data $F=0$ the exact score is $n\mu-2S/(1-w^2)$, giving a square-root rather than linear approach of $\hat w$ to zero.

## Numerical checks

**Numerical.** The [Python reproducer](2026-11-25-pig-math-data/check.py) checks 11 exact arithmetic signs, bisects the positive roots using explicit polynomial coefficients, and evaluates the resulting likelihoods independently using the Bessel-K mass formula. The four shape solves and three held-shape mean solves agree at the displayed 16 significant digits between 150 and 200 decimal digits of working precision. Each root uses 440 bisections. Gains are total log likelihood differences. The numerical calculation corroborates the strict inequalities established by the proofs.

| Fit | Fitted free coefficient | Gain over the applicable face |
|---|---:|---:|
| $(0,3)$, joint | Shape $2.62130286219152$ | $0.1119916797936843$ over Poisson |
| $(0,2)$, mean two | Shape $1$ | $0.3136010464297713$ over Poisson |
| $(1)$, held mean one ulp above two | Shape $1.342177240000001\times10^8$ | $4.411629933616147\times10^{-24}$ over Poisson |
| Five zeros, two ones, two twos, held rounded $2/3$ | Shape $2.163457024389511\times10^{32}$ | $9.495567745759799\times10^{-66}$ over Poisson |
| $(0,3)$, shape $10^{-8}$ | Mean $1.250035353005896$ | $7.999773734362509\times10^{-9}$ over Poisson–Lévy |
| $(0,3)$, shape $1$ | Mean $1.455577030518856$ | $0.6257116493203719$ over Poisson–Lévy |
| $(0,3)$, shape $10^8$ | Mean $1.500000005624999$ | $28255.89613032785$ over Poisson–Lévy |

The final Python run exited zero on the workstation using the single-slot lock. The math-only job required no builds or package tests.

```sh
PYTHONDONTWRITEBYTECODE=1 /tmp/umvl-cpu/run nice -n 10 python3 -u dev/research/evaluations/2026-11-25-pig-math-data/check.py
```

## What this changes for the fitter

- For $S>0$ and free shape, dispatch on the integer sign of $nF-S^2$ for the joint fit or $na^2-2Sab+Fb^2$ for a held mean. A nonpositive sign proves `PoissonLimit` with the fitted or held rate as appropriate. Give constrained Poisson advice for a held mean. A positive sign proves one finite root and a strict improvement, however small. It does not supply its coefficient to the requested accuracy.
- For a finite shape fit, refine a signed bracket in $w$ or log shape and verify inclusion at the requested relative coefficient tolerance. The shape score is a rational function of $w$ over rational means, since $1/z=2w/[\mu(1-w^2)]$ and $E_k=yQ'_{k-1}(y)/Q_{k-1}(y)$ at $y=1/z$. All these polynomial denominators are positive in the interior. For held shape the coordinate $0<d<\sqrt{2\lambda}$ gives $t=\lambda/d-d/2$ and $z=\lambda/d+d/2$. At fixed shape the likelihood depends on $d$ through $-nd-S\log z+\sum_{x_i>0}\log Q_{x_i-1}(1/z)$ plus constants. Its score is rational in $d$ for rational held shape. Thus exact signed rational bracket endpoints are possible on both routes, subject to work cost. These identities also supply independent checks for a scaled recurrence. Pass the separate mean-free flag into the near-Poisson score and carry the exact small held boundary term before scaling or rounding. A coarse globally signed enclosure followed by raw polishing does not by itself establish inclusion at a finer tolerance.
- For $S>0$ and positive shape held with mean free, use the monotone mean equation and its finite bracket. On either finite route, return `Ok`, certified and unique once the coefficient check closes. If a certificate budget runs out after a successful accuracy-checked search, retain the result as uncertified according to the [outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter). If the search itself fails or coefficient accuracy cannot be established, preserve the appropriate failure or budget outcome. A padded ambiguous sign alone supplies no proved limit or tie.
- For $S=0$, return shape zero when available. Preserve a held mean, or return a representative with `unique = FALSE` and the complete shape-zero ray when mean is free. For positive held shape and free mean, use certified `ZeroLimit` with Poisson rate zero. At two compatible held coefficients evaluate the supplied point. For $S>0$ with shape zero held preserve the incompatible-law stop.

## Open

Existence, finite cardinality and the winning boundary are settled for every supported base-law mask. A production coefficient certificate still needs measured cost and error control for signed brackets at the requested tolerance, especially at very large count orders and near the Poisson seam. The exact rational scores give a verification route, but clearing or evaluating the high-degree polynomials may exhaust the allowed work. What is missing is either a bounded-cost exact or adaptive-precision sign method for these inputs, or a validated error allowance for the streamed, cancellation-aware scores and their conversion back to public coefficients. The 150- and 200-digit checks are reference computations and do not validate that production allowance.
