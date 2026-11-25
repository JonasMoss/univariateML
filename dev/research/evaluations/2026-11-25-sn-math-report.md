# Skew-normal existence, nonuniqueness and global certificates

TASK-251 · 2026-11-25 · source 19b2fb7b5ffd517cb9ac9cb63eedfd68d247f88f

Both **sn** and **snorm** can have distinct finite global maximizers with all coefficients free. For **sn**, a strict tail bound reduces exact attainment with moving location to a finite stationary-value comparison against two half-normal faces. The existing cover bounds the likelihood gap, but its allowance cannot decide exact attainment or uniqueness. For **snorm**, the existing algebraic profiles identify the global candidate sets. The fully free implementation retains one winner even when reflection proves a second. For **st**, the moving-endpoint argument gives a strict finite-slant cutoff at fixed df and a uniform cutoff over restricted free df on compact nuisance sets.

The starting points were the [original sn derivation](2026-10-29-sn.md), [implementation report](2026-10-29-sn-report.md), [literature check](2026-10-29-sn-literature-check.R), [outer search](2026-11-02-sn-cert-report.md), [inner acceptance](2026-11-07-sn-accept-report.md), [snorm held certificates](2026-11-07-snorm-held-report.md), and [st scope and open entries](2026-11-08-st-cert-report.md). The original literature replay already exhibits a finite local peak followed by a dip and a climb toward a better face. The examples below establish finite global ties. The published motivation concerns divergence at finite sample sizes. [Azzalini and Arellano-Valle](https://arxiv.org/abs/1203.2376)

## Azzalini skew-normal

### Existence under all holds

Write $n>0$, $m_0=\min_i x_i$, $M_0=\max_i x_i$, $\eta=1/\omega$, $h=\eta\xi$, $z_i=\eta x_i-h$, $C=n\log(2/\sqrt{2\pi})$, and $P(\alpha)=\max_{\eta,h}\ell(\alpha,\eta,h)$ with the relevant holds. Assume finite data and proper held coefficients. The table is **proved** by the fixed-shape concavity and fixed-location separation in the original note, and the moving-location comparison below.

| Held coefficients | Exact condition |
|---|---|
| None | Constant data are unbounded. Otherwise a finite MLE exists iff a finite stationary value of $P$ equals or exceeds the larger half-normal face value. |
| $\omega$ | With nonconstant data the same criterion applies with scale fixed. For constant data the two faces tie and no finite MLE exists. |
| $\xi$ | Both strict residual signs give a unique finite MLE. One strict sign, possibly with zeros, gives the corresponding half-normal escape. All zeros give unbounded scale collapse. |
| $\xi,\omega$ | Both strict residual signs give a unique finite shape. One strict sign gives the corresponding half-normal escape. All zeros give the entire finite shape line as maximizers. |
| $\alpha$ | Nonconstant data give a unique finite maximum. Constant data are unbounded. |
| $\xi,\alpha$ | A unique finite scale exists iff some $x_i\ne\xi$. Otherwise the likelihood is unbounded. |
| $\omega,\alpha$ | The unique finite location always exists. |
| $\xi,\omega,\alpha$ | The held point is the sole feasible point. |

For the right face use $c=m_0$ when location is free, or $c=\xi$ when held. With location held the face is feasible iff $x_i\ge c$ for every observation. A free scale converges to $\sqrt{\sum_i(x_i-c)^2/n}$ on a bounded maximizing escape; otherwise it retains the held value. The face log likelihood is $C-n\log\omega-\sum_i(x_i-c)^2/(2\omega^2)$, less $k\log 2$ if location is held and $k$ observations equal it. Reflection gives the left face, with $c=M_0$ when location is free. For free location the full value is reachable by approaching from outside with $\alpha(m_0-\xi)/\omega\to+\infty$ on the right. Conversely, nuisance compactness and the strict optimum of the Gaussian envelope on $\xi\le m_0$ force $\xi\to m_0$ and scale to the face scale along an escape attaining that value. Since every CDF factor is at most one, attaining the full value forces the same rate condition at the tied minimum. For constant data and held scale the sharp bound is $C-n\log\omega$. Equality would require both $z=0$ and a skew CDF of one. Both outside approaches reach it in the limit.

With location held, the sum of log CDFs is strictly concave in $\beta=\alpha\eta$ if any residual is nonzero. Both strict signs give a vanishing likelihood at both infinite shape ends. One strict sign gives strict monotonicity. With moving location and nonconstant data, a finite joint maximum must straddle its own location because otherwise its shape score has a strict sign. This does not give a sign test at a proposed center. For example, **proved**, every distinct two-point sample has no fully free MLE. Its face residual sum is twice the centered sum, so each face equals the maximum Gaussian envelope $C+n\log\eta-\sum z_i^2/2$ minus $\log 2$. For positive shape, if $\xi\le m_0$ the envelope is at most the face and the finite CDF factors give strict loss. If $\xi>m_0$, one factor is at most $1/2$ and the other is strictly below one. Reflection handles negative shape, and at zero both factors are $1/2$. This gives escape with probability one at sample size two for every proper continuous member.

### Strict tail exclusion and exact sample criterion

**Proved.** For nonconstant data and moving location, let $F_+$ be the right face log likelihood, $k=\#\{x_i=m_0\}$, $D=\sum_i(x_i-m_0)$, $S=\sum_i(x_i-\bar x)^2>0$, and $b=\sqrt{2/\pi}$. Choose $E$ bounding $\eta$ for every $\ell\ge F_+$. With free scale any $E>\sqrt{n/S}$ satisfying $C+n\log E-SE^2/2<F_+$ works by monotonicity of the envelope beyond $E$. With held scale take $E=1/\omega$.

For $\alpha\ge A_+=ED/(kb)$ and $\xi\le m_0$ the envelope is at most $F_+$, with strict loss from the finite CDF factors. For $\xi>m_0$ put $t=\eta(\xi-m_0)>0$. The envelope increases over its value at $\xi=m_0$ and the same $\eta$ by $\eta Dt-nt^2/2$. Each minimum observation adds at most $-\log 2-b\alpha t$ by the log-CDF tangent at zero. Their sum and the gain are bounded by $-k\log 2+(ED-kb\alpha)t-nt^2/2<0$. Points with $\eta>E$ were already below $F_+$. Thus $P(\alpha)<F_+$ on the entire finite right tail. The outside-endpoint construction, for example with standardized gap $\alpha^{-1/2}$, proves $P(\alpha)\to F_+$. Reflection gives $A_-$ and $F_-$. A uniform positive gap below the face would contradict this convergence.

The fixed-shape optimum exists and is unique. Its free nuisance Hessian is negative definite, so the analytic implicit-function theorem makes $P$ real analytic on the finite shape line. The tail inequality and limit exclude a constant profile. Therefore $P'$ has finitely many zeros on $[-A_-,A_+]$. If $\mathcal S$ contains its interior zeros and $F=\max(F_+,F_-)$, the exact condition is
$$
\text{finite MLE exists}\quad\Longleftrightarrow\quad
\max_{\alpha\in\mathcal S}P(\alpha)\ge F .
$$
The maximum of an empty set is $-\infty$. The interval endpoints are strictly below their faces. If a finite stationary point ties the best face and none beats it, that finite point attains the likelihood. If all stationary points lose, the best face or faces exhaust the maximizing escapes. This sample criterion includes multiple roots and needs no cutoff chosen by a shape grid.

### Exact finite global tie

**Proved.** Take $x=(-1,0,0,0,0,0,1)$. The faces have squared residual sum 9, while the normal fit has residual sum 2. The squared ratio of either face likelihood to the normal likelihood is $(8/9)^7<1$. At $(\alpha,\eta,h)=(1/2,437/250,-357/1000)$ the squared likelihood ratio to the normal fit is
$$
(2\eta^2/7)^7\exp(7-2\eta^2-7h^2)\prod_i[2\Phi(\alpha(\eta x_i-h))]^2.
$$
It lies strictly between $1.0009$ and $1.0010$. The rational check uses Machin's identity and 40-term alternating arctangent sums, square roots with integer floors, 32 alternating terms of the integrated normal series and its next term, and a 25-term exponential sum with geometric remainder. The normal arguments have absolute value below 2, so the integrated terms decrease. The absolute exponential argument is below 1, and negative arguments use reciprocals. The witness beats the normal fit and both faces. Thus a finite global maximizer exists at nonzero shape. Reflection maps any such maximizer $(\xi,\omega,\alpha)$ to a distinct maximizer $(-\xi,\omega,-\alpha)$. This disproves unimodality. [Exact examples](2026-11-25-sn-math-data/exact_examples.py)

### One-dimensional cover and source discrepancy

**Proved.** Reuse the existing tangent cover. For any reference $t_i$, bound the CDF term by $\log\Phi(t_i)+r_i(\alpha z_i-t_i)$, $r_i=\phi(t_i)/\Phi(t_i)$. The resulting objective is affine in shape at fixed nuisance coordinates, so its supremum over an interval is bounded by the larger of its two analytic nuisance suprema at the endpoints. The smaller bound from two anchors is also valid. A feasible inner point supplies a lower bound. Anchors converging to the exact inner optimum give convergent bounds under subdivision, since the touching tangent objective has that same inner optimum. The strict tail cutoffs close the infinite parts even against a face incumbent. The older quadratic tail bound can exclude a tighter region if it is already strictly below the incumbent.

For coefficient enclosures, center the data and use the nuisance Hessian minorant $G=\operatorname{diag}(S,n)$ on $(\eta,h)$ or $G=n$ on $h$ when scale is held. At a fixed feasible nuisance anchor $v$, enclose its gradient $g(\alpha,v)$ over a shape interval. For the exact inner optimum $u(\alpha)$,
$$
 \|u-v\|_G\le\|g(\alpha,v)\|_{G^{-1}},\qquad
 0\le P(\alpha)-\ell(\alpha,v)\le\tfrac12\|g(\alpha,v)\|_{G^{-1}}^2.
$$
Integration of the Hessian bound gives the first inequality using the zero score at $u$, and the second by maximizing the quadratic upper bound. Enclose $P'=\sum_i z_i r(\alpha z_i)$ over those nuisance boxes. Use sign exclusion, or opposite endpoint signs and a negative enclosure of $P''=\ell_{\alpha\alpha}-\ell_{\alpha u}\ell_{uu}^{-1}\ell_{u\alpha}$ for a surviving simple maximum. Subdivide competing boxes and compare their likelihood bounds against feasible witnesses. Convert surviving boxes to $(\xi,\omega,\alpha)$ before checking coefficient tolerance. A simple separated optimum or strict face win admits closure with enough exact-arithmetic refinement. Higher-order roots can retain likelihood boxes or use Taylor isolation. Reflection proves equality of isolated reflected optima; overlap alone leaves equality unresolved.

**Discrepancy with current code.** The implementation has the right both-sign test under held location, correct face coefficients and correct held-endpoint penalty. For moving location it accepts tail and interval bounds within $5\cdot10^{-9}n$ of an incumbent and uses a $10^{-8}n$ final face allowance. This is an approximate likelihood-gap comparison. A finite candidate within the comparison margin of the face can yield an unresolved stop even though exact equality would suffice for attainment. A selected face is flagged uncertified. Tail-closure and cover-budget failures can discard a successful search, contrary to the requested fallback. An ordinary successful fit reports uniqueness unless a maximizer set is present, and this route only populates that set for the all-zero held-location line. The seven-point counterexample disproves general uniqueness. The strict best-face update also retains one face on equal computed face values. These are source audit findings. [sn fitter](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp) · [sn binding](../../../bindings/common/distributions/sn/fit.hpp)

The held-endpoint penalty must travel with the stop. A closed-endpoint half-normal fit can reproduce the limiting law and coefficients but gives a larger likelihood at endpoint ties. [sn dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md)

## Fernández–Steel two-piece normal

Let $a,b>0$ be side scales, $c$ the mode, $m=\sqrt{2/\pi}$, $h_0=\sqrt{1-m^2}$ and $(\mu,\sigma,\kappa)$ the public mean, sd and dossier **xi**. Density on a side of scale $s$ is $m/(a+b)\exp[-(x-c)^2/(2s^2)]$. Here $\kappa=\sqrt{b/a}$, $\mu=c+m(b-a)$, and $\sigma^2=a^2-ab+b^2-m^2(b-a)^2$. For the right face $\kappa\to\infty$ with $a\to0$ and the broad scale tending to $b$; the limiting public moments are $\mu=c+mb,\ \sigma=h_0 b$. The left face reflects it with $\kappa\to0$. [snorm dossier](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md)

### Existence under all holds

**Proved**, using the prior inner solves and exhaustion of boundary paths. With free sd, scale collapse is unbounded precisely when all data coincide and the mean is free or held at that value. Otherwise two separated observations or a nonzero held-mean deviation prevent a maximizing collapse. Both side scales are at most $\sigma/h_0$ and their sum is at least $\sigma/h_0$, uniformly in skewness. Thus scale divergence loses all likelihood. With scale bounded above and below, escaping location also loses all likelihood. For free skew the only remaining maximizing escapes are the half-normal faces.

| Held coefficients | Exact condition |
|---|---|
| None | Nonconstant data have a finite MLE iff an interior mode candidate minimizes $Q$ over the candidates and the two endpoints. Constant data are unbounded. |
| $\mu$ | Except for collapse at all $x_i=\mu$, a finite MLE exists iff a finite skew stationary value reaches or exceeds both feasible face suprema. For constant $x\ne\mu$, the face terminating at $x$ wins strictly, as proved in the dossier. |
| $\sigma$ | The same stationary comparison with sd held. Constant data give a two-face limit tie. |
| $\mu,\sigma$ | The same comparison with the held-moment support and contact penalty. In particular, if neither face is feasible, a finite MLE exists. |
| $\kappa$ | A unique finite fit for nonconstant data. Constant data are unbounded. |
| $\mu,\kappa$ | A unique finite scale iff some datum differs from $\mu$. Otherwise unbounded. |
| $\sigma,\kappa$ | A unique finite mean always exists. |
| $\mu,\sigma,\kappa$ | The held point is the sole feasible point. |

With skew held, use the prior strict concavity in inverse sd and scaled mode, where each hold is affine. In the free-skew held-moment rows use the rational variance-ellipse coordinate from the dossier. The branch optimizers after observation splits are algebraic, given by a weighted mean or the positive inverse-scale quadratic root, or fixed by both holds. The scores are algebraic after clearing finite positive denominators. They admit a finite decomposition into stationary points, stationary intervals, and nonstationary intervals. Retain stationary intervals if a score vanishes identically on a feasible branch. The original likelihood is continuously differentiable at a moving observation join because squared residuals and their first derivatives vanish there. Include branch joins explicitly. Compactness away from the faces ensures a finite maximum when an interior witness beats them. If none beats them, finite equality suffices.

For the right face the objective is $n\log(m/b)-\sum_i(x_i-c)^2/(2b^2)$ with $c\le\min x$. With mean free take $c=\min x$ and either the endpoint RMS as $b$ or $\sigma/h_0$ if sd is held. With mean held and sd free let $d_i=x_i-\mu$, $c=\mu-mb$, $u=1/b$. Maximize $n\log(mu)-\sum_i(m+d_i u)^2/2$ on $u>0,\ m+d_i u\ge0$. The unconstrained positive score root solves $(\sum_i d_i^2)u^2+m(\sum_i d_i)u-n=0$. Clip to the support ceiling if it binds. The all-zero case falls under collapse. With both moments held take $b=\sigma/h_0,\ c=\mu-mb$. A datum below $c$ rules out the face; each contact costs $m^2/(8h_0^4)$ by the prior endpoint-layer proof. When either moment can move, an adjustment into support reaches the full endpoint value. For constant data with only sd held, $m/(a+b)<mh_0/\sigma$ for $ab>0$ and both face paths can put the mode at the datum, proving the equal, unattained bounds. Reflect data and held mean for the left face. [Held endpoint analysis](2026-11-07-snorm-held-report.md)

### Exact profile counterexamples

**Proved.** With all coefficients free let $A(c)=\sum_{x_i<c}(x_i-c)^2$, $B(c)=\sum_{x_i>c}(x_i-c)^2$. The prior profile minimizes $Q=(A^{1/3}+B^{1/3})^3$ over the sample range. For $f=Q^{1/3}$ the conditional scales are $(a,b)=(A^{1/3},B^{1/3})\sqrt{f/n}$. For $x=(-2,-1^{[10]},1^{[10]},2)$, where the superscripts denote multiplicities, $A(0)=B(0)=14$ and $Q(0)=112$, while both endpoints have $Q=116$. At the center each side has count 11 and absolute residual sum 12. The side cube root has second derivative with sign $3\cdot11\cdot14-4\cdot12^2=-114$, by differentiation. The center is a strict local maximum of $Q$ by reflection and negative curvature. Hence the global minima are strictly interior and off center, and come in reflected pairs. Their public skewness values are distinct because $\kappa=1$ gives the unique normal optimum at $c=0$ and any other $\kappa$ reflects to $1/\kappa$. Both the mode and skew profiles therefore fail unimodality. For a distinct two-point sample, the interior $Q$ strictly exceeds its common endpoint value by $t^{2/3}+(1-t)^{2/3}>1$ for $0<t<1$. Such a sample has only limiting maximizers.

The known held-moment counterexample uses $x=(-1,0,1)$, $\mu=0$, $\sigma=151/200$. Both faces are infeasible because $m\sigma/h_0<1$. At $\kappa=6$ the log likelihood exceeds the normal value by an amount in $(0.07685,0.07686)$. Put $v=\sqrt{1+h_0^2(35/6)^2}$, $r=v/\sigma$, $e=m(35/6)$. Substitution gives the gain $3\log(12v/37)+1/\sigma^2-[36(r-e)^2+(e^2+(r+e)^2)/36]/2$. The rational check bounds the logarithm by the atanh series and geometric remainder. Reflection gives a second maximum at the reciprocal shape. [Exact examples](2026-11-25-sn-math-data/exact_examples.py)

### Global cover and current scope

**Proved.** On a free-fit observation cell put $u=A'/2$, $v=-B'/2$, positive at interior points. A stationary mode solves $u^3B^2-v^3A^2=0$, the prior degree-seven polynomial. Include joins and sample endpoints. A direct enclosure supplements root isolation. $A$ increases and $B$ decreases, so on $[l,r]$,
$$
 A(l)^{1/3}+B(r)^{1/3}\le Q(c)^{1/3}
 \le A(r)^{1/3}+B(l)^{1/3}.
$$
The lower bound gives a log-likelihood upper bound, including at cells touching a face. The enclosure converges under subdivision. Compare surviving root boxes against feasible lower witnesses, retaining multiple roots and resolving coefficient widths in public coordinates. The existing held-moment Bernstein cover uses the same logic in the skew coordinate. Retain actual score roots after checking cells and positive scale. Use interval nuisance formulas on finite boxes, and the dossier's relaxed location/scale bound using the within-group and between-group sums for unresolved boxes. Near skew endpoints use the proved denominator factors and support tests. A support interval straddling zero needs an unpenalized upper bound until contact or infeasibility is established. [Held certificate](2026-11-07-snorm-held-report.md)

**Source audit.** Fully free fitting compares midpoint representatives and stores one winning root or endpoint. Its ordinary finite outcome declares uniqueness. The new symmetric example proves the representation cannot encode every fully free answer. An isolated winning root bracket bounds coefficients conditional on that root winning; selection among close candidates requires comparing likelihood enclosures with bracket widths and arithmetic included. The held fit already carries compared candidates. Its tolerance-based comparisons alone cannot prove exact ties. [snorm fitter](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/mle.hpp) · [held certificate source](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/certificate.hpp)

## Azzalini skew-t

The mask numbers here follow the prior report. Bits 1, 2, 4 and 8 denote free location, scale, slant and df. Held df may be any positive value; free df is at least one. **sn**, rather than **snorm**, is the normal face of **st**. The two-piece results instead pertain to the two-piece Student construction. The prior skew-t analysis supplies two compactness ingredients used here. Its mixed-sign held-location rectangle excludes the scale and slant tails at strict collapse exponent. Its held-scale moving-location bound confines any competitor to a finite location interval. For moving scale the uniform Student envelope gives a compact nuisance set when the reachable collapse exponents are strictly positive. All these statements are independent of the new skew bound. [st certificate report](2026-11-08-st-cert-report.md) · [Uniform compactness and collapse-face proofs](2026-11-07-skew-theory-data/proofs.md)

### A strict moving-endpoint cutoff

**Proved.** On a compact nuisance region containing all competitors, fix an orientation and let $m_0=\min x$, $k=\#\{x_i=m_0\}$. Compare each point with the closed right-half-t value at $\xi=m_0$ and the **same** $\omega,\nu$. This is a reachable face witness under the holds. For $\xi\le m_0$, the base Student densities are no larger than at $m_0$, and the finite CDF multipliers cost strictly positive log likelihood relative to the face. For $\xi>m_0$, put $t=(\xi-m_0)/\omega$, bound $0<t\le H$, and let $K>0$ bound the absolute sum of the base log-density derivatives in the standardized location shift on this segment. For fixed $\nu$ one may use $K=n(\nu+1)/(2\sqrt{\nu})$. For $\nu\ge1$ and all the relevant residuals bounded in absolute value by $Z$, one may use $K=2nZ$, since $(\nu+1)|z|/(\nu+z^2)\le2|z|$. Include the endpoint and the intervening straight segment when setting $Z$. If the box has no wrong-side locations, the first case suffices.

The base gain is at most $Kt$. Take $\delta=\min(H,k\log 2/(2K))$. For $0<t\le\delta$, the $k$ wrong-side factors cost at least $k\log 2$ relative to the face. For $t\ge\delta$, those factors are each at most $T_{\nu+1}(-\alpha q_*)$, where
$$
 q_*=\delta\sqrt{(\nu+1)/(\nu+\delta^2)}
$$
works at fixed $\nu$, and $\min(\delta,1)$ works uniformly in $\nu$. The function $t\sqrt{(\nu+1)/(\nu+t^2)}$ is increasing. The prior Student density bound $t_m(u)\le(1+u^2)^{-1}$ for $m\ge1$ gives $T_m(-u)\le1/u$. Hence $\alpha\ge\exp((KH+1)/k)/q_*$ forces wrong-side log loss at least $KH+1$, strictly exceeding the possible gain. A tighter Student tail bound can replace $1/u$. Reflection gives the other wing. The comparison is with a same-df face, so the argument remains valid as df escapes to infinity. Nuisance points outside the competitor box are excluded by the prior envelope against a feasible incumbent. The proof uses monotonicity and a tail inequality for Student CDFs. It requires no log-CDF concavity, which fails on the negative Student tail.

For finite fixed-df boxes, enclose each residual interval $Z_i$, the monotone image $q(Z_i)$, and the product of that image with the shape interval. An elementary likelihood upper bound uses the smallest possible $|z_i|$ for the base density, the smallest scale for its normalizer, and the largest product for the CDF. This enclosure converges as boxes shrink. Interval scores or Hessians can localize stationary components and verify coefficient precision for a separated maximum. Face comparisons use the prior exact endpoint and monotone free-scale profile. Together with the strict tails this closes the *mathematical search construction* for fixed-df moving-location/free-slant mask 5 and strict-compactness mask 7. In those scopes there is a finite MLE iff the compact finite-box maximum reaches or exceeds the best face. Mask 3 at strict compactness uses the same finite-box formula with shape fixed. The search still has to be implemented and tested. The construction allows more than one stationary component and preserves finite equality with a face.

### Held location and scale collapse

**Proved.** For masks 2 and 6 at held $\xi,\nu$, write $d_i=x_i-\xi$, $k=\#\{d_i=0\}$, $e=\nu(n-k)-k$, $u=\omega^2$, $c_\nu=t_\nu(0)$ and $m=\nu+1$. Factoring the powers of scale in the likelihood gives
$$
 L(u,\alpha)=u^{e/2}c_\nu^k
 \prod_{d_i\ne0}
 \left[
 2 c_\nu(u+d_i^2/\nu)^{-m/2}
 T_m\left(\frac{\alpha d_i\sqrt m}{\sqrt{\nu u+d_i^2}}\right)
 \right].
$$
This follows directly from the density, since tied observations contribute $c_\nu/\omega$ and each nonzero residual contributes a factor $\omega^\nu$. If $e<0$, any fixed finite shape witnesses divergence at zero scale. For $e>0$ the limit at zero is zero uniformly in slant, and the prior large-scale bound gives a finite upper cutoff on $u$. When $e=0$ this expression extends continuously and positively to $u=0$ at each finite slant. Thus scale-only, including nonzero held slant, has a direct scalar cover on a closed interval in $u$, with $u=0$ counted only as a limit. For mixed signs with free slant, the previous uniform slant score bracket remains valid for the whole closed scale interval: the lower bounds on nonzero $q$ use the upper scale limit only. One can therefore also cover the $e=0$ mixed-sign case by a compact rectangle, including the collapsed edge. Use interval products of the displayed factors with $u^0=1$ at equality. For $e>0$ the earlier strict rectangle already supplies attainment. For $e=0$ the comparison must test whether a finite box or the collapsed edge wins, preserving a finite equality. The existing one-sign routes already handle the monotone shape reduction. The moving-location equality layer has a further location-shift coordinate, which this factorization at held location does not remove. [Prior compact rectangle](2026-11-08-st-cert-data/proofs.md) · [Prior collapse layers](2026-11-07-skew-theory-data/proofs.md)

### Free df

**Proved.** The prior uniform normal-face result bounds likelihood error even at unbounded slant. An elementary decreasing majorant avoids evaluating polygamma functions. For $\nu\ge N\ge2$, residuals $|z|\le Z$, scale $\omega\ge w_0>0$, put $c_0=1/\sqrt{2\pi}$, $C_0=1/\sqrt{2\pi e}$ and
$$
 E_N=-\log(1-1/N)+Z^2/(2N)+(N+1)Z^4/(4N^2),
 \qquad
 d_N=\frac8{N+1}+C_0\sqrt{\frac2{N+1}}+\frac{C_0(1+Z^2)}{2N}.
$$
Here the $e$ in $C_0$ is Euler's number. For $W\sim\chi^2_\nu/\nu$, Jensen and $E(\sqrt W-1)^2\le E(W-1)^2=2/\nu$ give $1-1/\nu\le E\sqrt W\le1$. The normalizer ratio equals $E\sqrt W$. The prior log-kernel inequality using $u-u^2/2\le\log(1+u)\le u$ then gives log base error at most $E_N$. In the Student CDF mixture use $V\sim\chi^2_m/m$, $m=\nu+1$. On $V<1/2$ the CDF difference is at most one and the event has probability at most $8/m$ by Chebyshev. On $V\ge1/2$ the derivative of $\Phi(s\sqrt V)$ in $V$ is uniformly at most $C_0$ in absolute value. This contributes at most $C_0\sqrt{2/m}$. For the extra skew argument scale $\sqrt{(\nu+1)/(\nu+z^2)}$, use the prior log-scale derivative bound $C_0$ and $|\log((\nu+1)/(\nu+z^2))|\le(1+Z^2)/\nu$. This proves CDF error at most $d_N$ uniformly over the whole slant line. Thus the point density error is bounded by
$$
 \epsilon_N=\frac{2c_0}{w_0}\big(\exp(E_N)-1+d_N\big).
$$
Both densities are bounded above by $B=2c_0/w_0$, so the likelihood-product error is at most $nB^{n-1}\epsilon_N$ by telescoping. The bound decreases to zero on the entire df tail.

The **sn** cover bounds the hold-compatible normal-face supremum, including its half-normal corner. If a feasible finite **st** witness exceeds that upper bound by a positive separation, a sufficiently large $N$ excludes the whole df tail on the compact nuisance box. Use the strict slant wing bound to reduce finite-df/free-slant competitors to a bounded shape region plus the compared half-t faces. When the normal face is the incumbent, an error tending to zero does not determine the sign of the finite-df difference. That case needs a one-sided tail certificate or another separated upper bound. The finite-df cover must include the attainable restriction face $\nu=1$, when free, and the half-t comparisons must respect the same df restriction as the parent. A held-endpoint **ht** fit over all positive df cannot be substituted for the constrained face. This leaves masks 8–15 dependent on df-tail decisions, holds, and any reachable collapse layers, apart from the previously settled special cases. The two-piece normal result gives no further **st** reductions.

## Reproduction

The final runs all passed on the workstation JonasBI. Each ran under the one-slot wrapper and finished before this report was written. Mathematical conclusions use the proofs and exact rational inequalities above. The 70-decimal-digit mpmath calculations check particular evaluations and bounds. They provide no binary64 or outward-rounded certificate.

| Script | Result |
|---|---|
| [exact_examples.py](2026-11-25-sn-math-data/exact_examples.py) | Exact rational inequalities passed for the ordinary skew-normal witness and both two-piece examples. |
| [profile.py](2026-11-25-sn-math-data/profile.py) | Nine finite profile evaluations on shapes 0 through 10, three large-tail evaluations, and 33 profile evaluations in three tangent boxes. The symmetric half-cover initialized at spacing 1/4 over $[0,8]$ and used 120 bisections to close a numerical likelihood gap of $10^{-5}$. The feasible witness at shape $1/2$ gains about $0.0004873032553671$ log units over the normal fit. The six-df, seven-residual, seven-shape normal-layer grid passed all 294 checks. |
| [boundary.py](2026-11-25-sn-math-data/boundary.py) | 288 two-piece interval checks on 32 equal mode intervals, 104 skew-t endpoint probes with df in $\{1/2,1,4,20\}$ at the cutoff and twice the cutoff, and five positive scales plus the collapsed limit for held-location equality. |

Run from the worktree root.

~~~sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
for script in exact_examples profile boundary; do
    /tmp/umvl-cpu/run python3 -B "dev/research/evaluations/2026-11-25-sn-math-data/$script.py" || exit
done
~~~

The deliverables are this report and the three Python scripts. The math-only scope required no builds, compiled tests or production changes.

## What this changes for the fitter

- **sn.** Dispatch the exact held-location signs and constant-data cases first. Hold the appropriate scale and preserve the endpoint log penalty on escape. With moving location and free shape use the existing tangent cover with the strict cutoffs above. Certify a finite estimate only after global comparison and public-coordinate inclusion. Return proved finite ties with all maximizing candidates, and retain both reachable faces when only the limits tie. A finite equality with the best face still attains the maximum. A separated winning face gives proved nonattainment. A successful search whose certificate cannot decide a comparison or enclose coefficients gets the requested uncertified fallback. A separated feasible likelihood that refutes the candidate or a failed search precludes that fallback.
- **snorm.** Keep the algebraic profiles. Compare enclosing likelihood intervals for free-mode roots and endpoints, and carry every proved tied maximizer. Use the direct $Q$ bound on unresolved intervals. Preserve reachability and contact penalties under held moments. Exact branch logic handles collapse and fixed skewness.
- **st.** Use the strict moving-endpoint bound with the prior compact nuisance bounds. The held-location scale factor gives a regular equality edge for the fixed-df scale and mixed-sign scale/slant covers. Compare same-hold normal and half-t faces and keep the $\nu=1$ restriction. Budget cover evaluations, subdivision cells and solver iterations under the existing iterlim-scaled units. For each numerical bound propagate coordinate and termwise special-function error before comparing signed margins, with the repo's practical 128-epsilon working-magnitude allowance as a starting point to validate. Use scaled or log-space calculations for extreme products and cutoffs. The inequalities are proved in exact arithmetic. This job does not establish the adequacy of a production floating-point allowance.

## Open

1. **sn stationary isolation at singular roots and exact finite/face equality.** The compact list is finite, but interval signs and a simple-root Hessian test can leave singular stationary boxes, including shape zero. A complete practical root-count treatment needs higher-order Taylor bounds or other validated isolation. Accidental equality between unrelated finite and face values needs an equality argument; overlapping enclosures supply none. Benchmark the new tail cutoff and practical comparison margins against near-face and extreme-range inputs before certifying.
2. **snorm computed selection near ties.** The all-free algebraic candidate set and likelihood bounds are available. A production implementation needs verified root retention at clustered joins, two-sided enclosing likelihood comparisons, and a public-coordinate recovery check for each winner. Exact reflection establishes the displayed ties, but accidental equality requires additional evidence.
3. **st fixed df.** The missing moving-endpoint tail inequality is supplied here, and the fixed-location equality box is explicit. Numerical enclosures, coefficient tolerances, and benchmarking of joint searches remain. At moving-location collapse equality compare the prior layer supremum to the interior with quantitative remainders. The Cauchy semicircle in the prior report shows that whole stationary components can matter.
4. **st free df.** The uniform error bound closes a tail against a separated witness. To prove an attained normal-face supremum only at infinity, determine the sign of the df-tail profile difference uniformly over competitive nuisance and skew boxes, including vanishing leading terms, or provide an alternative one-sided bound. A validated finite-df search, constrained half-t face certificates, and numerical collapse-layer comparisons remain. The inherited Student delegation keeps its previously documented coefficient precision.
