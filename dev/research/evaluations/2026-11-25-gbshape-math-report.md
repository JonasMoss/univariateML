# Outer power profiles for GB2, generalized gamma and generalized beta

TASK-261 · 2026-11-25 · Worktree base `7ed10c94`

Generalized gamma and genbeta can have separated finite local maxima in power. An exact five-observation GB2 sample gives a strict trough in the all-free and held-scale power profiles. These results leave general finite global uniqueness open.

The scalar covers isolate a unique finite global maximum for the generalized-gamma and genbeta goldens, conditional on the computed comparisons. They also close the genbeta quantile and gamma-face hard cases. Uniform exterior bounds prove finite attainment for the GB2 golden. Its stored stationary point is a strict local maximum, and the two-dimensional finite cover leaves its global status open. A uniform score bound selects the inverse transformed-gamma face in both archived fixed-power Feller–Pareto cases.

Likelihoods below are per observation on log observations for GB2 and generalized gamma, and on positive log distances from the held endpoint for genbeta. Numerical certificates use exact-arithmetic bounds conditional on computed comparisons, with practical margins. The finite-multiplicity proofs explicitly identified as interval checks also enclose the sign computations with elementary interval functions and analytic digamma remainders.

## Existing certificates and searches

Here $a$ is power and $b$ is scale or endpoint. In GB2 $p=\text{shape3}$ controls the lower tail and $q=\text{shape1}$ controls the upper tail. In genbeta $p=\text{shape1}$, $q=\text{shape2}$, $a=\text{shape3}$, and $b>\max x_i$ is designated.

| Family and holds | Existing evidence |
|---|---|
| GB2, all four free | Numerical multibasin search with a strictly concave power/location fit at each pair of beta shapes. Local coefficient checks and numerical face comparisons leave globality open. Constant positive data have unbounded likelihood. |
| GB2, both beta shapes held | Strict concavity in power and scaled log intercept gives a unique finite fit for nonconstant data. This also covers the affine slices with power or scale held. |
| GB2, $a,b$ held | The logit transformation supplies a beta likelihood. Its free shape coefficients have a unique conditional fit for nonconstant data. Holding one beta shape gives the scalar fit also on constant data. |
| GB2, $a$ and one beta shape held, $b$ free | Conditional beta-prime certificates include finite fits and a gamma or inverse gamma face, subject to scale transport. An exact tie in this subset transfers from beta prime. |
| GB2, $b$ and one beta shape held, $a$ free | Global interval certificate for the remaining shape and power. Exact held-unit shapes also qualify for Burr or inverse-Burr delegation, with the parent's certificate or successful uncertified search. |
| GB2, remaining mixed holds | Generic free masks $\{p,q,b\}$, $\{p,q,a\}$, $\{p,a,b\}$, and $\{q,a,b\}$ use numerical searches outside the reductions above. The joint beta-prime transfer for $\{p,q,b\}$ preserves likelihood and feasible sets. Its parent joint search leaves globality open. |
| Generalized gamma, $(\mu,\sigma,Q)$ free | Two signed transformed-gamma covers, an exact-symmetry zero route, and strict power/Pareto comparisons certify separated cases. Successful locally checked fits can carry the uncertified flag. $Q=0$ is the attained lognormal. |
| Generalized gamma, $Q$ held | Strict conditional concavity, including singly held location or scale. Free-scale concentration at the common log observation, when compatible with the holds, is unbounded. |
| Generalized gamma, $Q$ free with $\mu$ or $\sigma$ held | Generic global comparisons are numerical. Held $\sigma$ gives finite attainment. Held $\mu$ allows one-sided limits with endpoint $e^\mu$, depending on which side contains the data. |
| Genbeta, all three shapes free, $b$ held | Optional global certificates for separated finite, moving-power and gamma winners. Unclosed comparisons retain the successful result with the uncertified flag. |
| Genbeta, $a$ held or both beta shapes held | At fixed $a$ the beta reduction supplies conditional shape uniqueness on nonconstant data. Holding both beta shapes gives a strictly concave power objective for every $p,q>0$, including constant data. |
| Genbeta, $p$ held, $q,a$ free | Complete signed compact cover for nonconstant data. The held $p=1$ route delegates to Kumaraswamy. |
| Genbeta, $q$ held, $p,a$ free | Global cover for separated finite winners at $q\ne1$. General winning-face decisions remain open. For $q=1,b=1,x=(1/6,1/2)$, all positive factorizations of $pa=3$ maximize. |

Holding both GB2 beta shapes gives a strictly concave power profile after optimizing the log intercept, by partial maximization of a jointly strictly concave function. At fixed generalized-gamma shape $k=Q^{-2}$, the location-profiled curvature in standardized positive power is $-1/t^2-kK''(t)<0$. In genbeta with both shapes held the curvature is $-1/h^2-(q-1)\overline{v_i^2/[4\sinh^2(hv_i/2)]}<0$, using $2\sinh(z/2)>z$ for the case $q<1$. These are unimodal slices. [GB2 dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md) · [Generalized-gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/README.md) · [Genbeta dossier](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) · [Fixed-power transfers](2026-11-25-transfer-math-report.md) · [Criteria for per-sample uniqueness](2026-11-25-uniq-cover-math-report.md)

## GB2

### Two-dimensional profile and cell upper bounds

**Proved.** For log observations $y_i$, set $r=p/(p+q)$, $\kappa=pq/(p+q)$, and $m=\log b+\log(r/(1-r))/a$. These coordinates also apply after standardizing the logs and expressing power and log scale in that unit. With $z=a(y-m)$, the log density is $\log a+\log C(\kappa,r)-\kappa D_r(z)$, where
$$
 D_r(z)=\frac{\log(1+r(e^z-1))-rz}{r(1-r)},\qquad
 D_r'(z)=\frac{e^z-1}{1+r(e^z-1)},\qquad
 D_r''(z)=\frac{e^z}{[1+r(e^z-1)]^2}.
$$
Both $D_r(0)$ and $D'_r(0)$ are zero. The endpoint kernels $D_0,D_1$ are $e^z-1-z$ and $e^{-z}-1+z$. Write $R(k)=\log\Gamma(k)-(k-\tfrac12)\log k+k-\tfrac12\log(2\pi)$. Substitution in the beta normalizer gives
$$
 \log C=\tfrac12\log(\kappa/2\pi)-R(p)-R(q)+R(p+q),
 \qquad p=\kappa/(1-r),\quad q=\kappa/r .
$$
The Stirling correction at either endpoint is $-R(\kappa)$. These are the transformed-gamma and inverse transformed-gamma faces, respectively. Beta prime is the attained slice $a=1$. Weibull, inverse Weibull, gamma and inverse gamma are held-shape or held-power specializations of the gamma faces. [Escape analysis](2026-10-28-gb2.md) · [Numerical escape probe](2026-10-29-gb2-escape-probe.md) · [Held-power certificates](2026-11-08-gb2-cert-report.md) · [Held-shape certificates](2026-11-08-gb2-power-report.md)

For fixed $a,r$, minimize $\overline{D_r(a(y_i-m))}$ over $m$. Strict convexity gives a unique root in the sample range, independently of $\kappa$. For its positive minimum $d$, the concentration objective is $\log C-\kappa d$. Its second derivative is minus the variance of $D_r$ under the density proportional to $e^{-\kappa D_r}$, so it is strictly concave. It tends to $-\infty$ at both ends by $\log C\le\tfrac12\log(\kappa/(2\pi))$ from the positivity and decrease of $R$. By convexity and $D_r(0)=0$, integration gives $C(s\kappa,r)\le s C(\kappa,r)$ for $s\ge1$, hence $\partial_\kappa\log C\le1/\kappa$. Thus the maximum has $\kappa\le1/d$. Both nuisance optimizations are scalar and a supporting tangent bounds either objective with an inexact proposal.

On $[l,u]\times[r_l,r_u]$, replace the loss right of $m$ by $D_{r_u}(l(y_i-m))$ and the loss left of $m$ by $D_{r_l}(l(y_i-m))$. The ordering follows by integrating $D''_r$ and using convexity along each ray. This yields a convex loss in $m$ whose tangent gives a lower bound $d_L>0$ on its minimum. With $S=\max y-\min y$, three inexpensive whole-cell uppers are
$$
 \log u-\tfrac12\log(4\pi e\,d_L),\qquad
 \log l-\tfrac12\log(4\pi e\,d_L)+\tfrac12(u-l)S,\qquad
 \log l-\log d_L-1 .
$$
The second uses $|\partial_z\log D''_r|\le1$ and compares the losses divided by squared power. The third follows from $C\le\kappa$, obtained by integrating the two limiting-slope exponential lower densities, and the increase of $D_r(ay)/a$ in $a$. It applies for every $a\ge l$ on the ratio cell.

For a sharper bound let $J(x)=R(1/x)$ with $J(0)=0$, and $r_0=(r_l+r_u)/2$. Binet's arctangent integral makes $J$ increasing and concave. The Laplace formula gives $J'(x)=x^{-1}\int_0^\infty e^{-s}g(xs)\,ds$, where $g(t)=1/(e^t-1)-1/t+1/2$. The derivative of $g$ is positive by $2\sinh(t/2)>t$. Its partial fractions give $0<g(t)<\min(1/2,t/12)$. Thus $0\le J'\le\min(1/12,1/(2x))$ and $0\le-J''\le1/(2x^2)$. In particular
$$
 |\partial_r\log C|\le\min(2,1/(4\kappa)).
$$
For $r\le1/2$, the derivative times $\kappa$ is $J'((1-r)/\kappa)-2rJ'(r/\kappa)+(1-2r)[J'(r(1-r)/\kappa)-J'(r/\kappa)]$. After division by $\kappa$, the three terms lie in $[0,1]$, $[-1,0]$, and $[0,1/2]$, using the bounds on $J',J''$. Symmetry covers $r>1/2$. The $1/(4\kappa)$ bound follows from the three original $J'$ terms each bounded by $1/12$. At likelihood level $W$, the peak must be at least $e^W$, hence $\kappa\ge k_L=2\pi e^{2W}/u^2$. Another absolute upper is the maximum of $W$ and
$$
 \log u+(r_u-r_l)\min(1,1/(8k_L))
       +\sup_{\kappa>0}[\log C(\kappa,r_0)-\kappa d_L].
$$
Use a concentration tangent on $[0,1/d_L]$ for the final supremum.

### Uniform exterior bounds

**Proved compactness at bounded power.** Let $H$ be the peak and $t_i=\log H-\log f(y_i)$. Log concavity and integration of the log-linear lower bound from $m$ to each $y_i$ give $1\ge H|y_i-m|(1-e^{-t_i})/t_i\ge H|y_i-m|/(1+t_i)$. Thus $\ell\le\log H+1-H\overline{|y_i-m|}$. A positive minimum mean absolute distance gives a peak cap $M$ at level $W$ for every nonconstant sample. It also gives $|m-\bar y|\le e^{-W}$ since $H\ge e^W$. Set $h=a^2\kappa$. The peak bound gives $h\ge2\pi e^{2W}$. From $D''_r(z)\ge e^{-|z|}$, the mean loss is at least $h e^{-aS}\overline{(y_i-m)^2}/2$ for free scale and the optimized $m$. A bounded mode instead bounds the displacements for a held scale. With bounded positive $a$, these estimates bound the relevant $h,m$ in a compact set. Continuity of $D_r,\log C$ includes $r=0,1$, so the two ratio edges cover every relevant bounded-power escape.

For a direct endpoint slab $0\le r\le\rho$, $a\le u$, the data losses satisfy $D_r\ge D_0/\lambda$, $\lambda=(1+\rho(e^{uS}-1))^2$. Replace $\kappa$ by $\kappa/\lambda$ and use $\partial_\kappa\log C\le1/\kappa$. This bounds the slab by its gamma upper plus $2\rho+\log\lambda$. Reflect for the other slab. At held $b$, the restriction $\operatorname{logit}(r)=a(m-\log b)$ already keeps the ratio away from the endpoints at bounded positive power and bounded mode.

**Proved small-power upper.** Write $N$ for the refitted normal value. The peak bound $\sqrt{h/(2\pi)}$ and the lower data loss give $\ell\le N+aS/2$ after optimizing $h,m$. For all $0<a\le A$ a sharper loss bound is $\min_m\overline{D_1(A|y_i-m|)}/A^2$ in place of $e^{-aS}\overline{(y_i-\bar y)^2}/2$, by integrating $e^{-A|y_i-m|}$. Holding $h>0$ as $a\downarrow0$ realizes the normal. At held $b$, choose the required ratio by the mode identity. If one beta shape is held, the earlier peak bound proportional to $a$ excludes the small-power tail. [Held-shape exteriors](2026-11-08-gb2-power-data/open-analysis.md)

**Proved large-power upper.** Let $L=\overline{(m-y_i)_+}$, $T=\overline{(y_i-m)_+}$. The asymmetric Laplace likelihood at a fixed kink, optimized over its rates, is $A(m)=-1-2\log(\sqrt L+\sqrt T)$. To derive it, write the two inverse rates as $v w,v(1-w)$, minimize $L/w+T/(1-w)$, then maximize over $v>0$. Between data knots the square-root sum is concave. Thus $A_*=\max_m A(m)$ is a knot comparison that includes power and Pareto at the extremes.

Fix $W>A_*$ and small $\delta>0$. For $p,q\le\delta$, the softplus hinge inequality gives $\ell\le A_*+\psi_1(1)\delta^2$. The normalizer correction is $\log\Gamma(1+p+q)-\log\Gamma(1+p)-\log\Gamma(1+q)\le\psi_1(1)pq$ by integrating trigamma over the two shapes. Otherwise use the peak cap $M$. The peak divided by $a$ increases in each beta shape since the derivative of its log in $p$ is $f(p)-f(p+q)>0$, where $f(k)=\log k-\psi(k)$. At $p=q=k\le1$, the peak divided by $a$ is at least $k/4$, by splitting the beta integral at $1/2$. For $a\ge A>4M/\delta$, one shape is at most $4M/a$. Suppose it is $p$, with $q\ge\delta$, and write $u=ap$. Integrating the left exponential lower density gives $H\le u$, hence $u\in[e^W,4M]$. The left loss at $y<m$ is at least $u(m-y)-u(1+p/q)/a$, by integrating the deficit from the limiting slope. The right derivative at distance $d$ is $u(e^{ad}-1)/(1+(p/q)e^{ad})$. For $d\ge\epsilon$ it is at least
$$
 B=\tfrac12\min\{e^W(e^{A\epsilon}-1), A\delta(1-e^{-A\epsilon})\}.
$$
It follows that a likelihood at level $W$ must obey the upper
$$
 \max_{u,m}\big[\log u-u\{\overline{(m-y_i)_+}-C_A\}
                 -B\overline{(y_i-m-\epsilon)_+}\big],
 \qquad C_A=(1+4M/(A\delta))/A .
$$
At fixed $u$ the expression is affine between sample knots and their shifts by $-\epsilon$. Test these knots and the range endpoints, maximizing the scalar rate term at each. Reflect for small $q$. As $A$ increases, $B$ diverges and $C_A$ vanishes. For sufficiently small fixed $\epsilon,\delta$ both comparisons close at any $W>A_*$. Points below the level are already bounded by $W$. The attainable limit equals $A_*$ by the two-rate paths and the one-sided paths with a slight outward endpoint shift to recover the full endpoint density. At held $b$ the two-small-shape kink stays at $\log b$, while the small-$p$, large-$q$ mode eventually satisfies $m\le\log b$ with the reflected constraint for small $q$. The unrestricted upper is safe for the held-scale comparison. [Limit paths and likelihood traces](2026-10-28-gb2.md) · [Moving held-scale endpoints and existing signed tails](2026-11-08-gb2-power-data/open-analysis.md)

A finite witness strictly above $N$, both whole gamma profiles and $A_*$ proves finite attainment, by the uniform exteriors and compactness. A winning face at equality requires a signed comparison.

### Exact non-unimodality and a fixed-power face test

**Proved trough.** Take $x=(e^{-1},e^{-1},1,e,e)$. The conclusion also holds at held $b=e^2$. For symmetric centered logs set $v=\overline{y^2}$, $m_4=\overline{y^4}$, $\beta=1-2r$. After fitting $m,h$, the small-power expansion is
$$
 P(a,r)=N+a^2\frac{m_4-3v^2+\beta^2(5v^2-3m_4)}{48v}+O(a^3).
$$
Indeed $D_r(ad)/a^2=d^2/2+\beta a d^3/6+[\beta^2-2r(1-r)]a^2d^4/24+O(a^3)$. The peak correction is $-a^2[1-r(1-r)]/(12h)+O(a^6)$. The normal nuisance fit is $m=0,h=1/v$. The first-order score displacement is in $m$ only and adds $\beta^2v/8$ to the second-order coefficient. The likelihood-level compactness gives a uniform positive $h$ range and bounded $m$ near zero. The expanded expressions and their nuisance derivatives are uniform there, including the ratio endpoints, and the normal Hessian is nonsingular. With held $b$, the mode identity gives $r=1/2+O(a)$ and the quadratic uses $\beta=0$ on symmetric logs.

Here $v=m_4=4/5$ and the coefficients at $\beta^2=0,1$ are $-7/240,-1/120$. Thus the profile drops below $N$. The large-power value is at least $-1>N$ from the fitted power face at $y=1$. Even at $b=e^2$, $p=1/a,\ q=\exp(a-(\log a)^2)$ reaches that face. Three ordered finite powers with a strictly lower middle value contradict unimodality. The separate fixed-power beta-prime transfer already supplies a finite tied sample in its held-shape GB2 subset. For nonsymmetric logs the first-order term of the all-free power profile is $a|\overline{(y-\bar y)^3}|/(6v)$. Nonzero log skewness therefore excludes the lognormal as the global winner. If log skewness is zero and $v^2<m_4<3v^2$, the quadratic is uniformly negative. [Held-power tied sample](2026-11-08-gb2-cert-report.md#nonuniqueness)

**Proved fixed-power comparison.** Let $s_3=\overline{(y-\bar y)^3}$ and $\tau=aS\le1$. After fitting the mode, every point of level at least $W$ satisfies
$$
 \left|h^{-1}\partial_r\ell-a s_3/3\right|
 \le a^2\left[S^4\left(\frac{e^\tau}{2}
                  +\frac{4e^{2\tau}+e^{3\tau}}{12}\right)
                  +\frac{1}{4h_L^2}\right],\qquad h_L=2\pi e^{2W}.
$$
The exact curvature derivative $\partial_r D''_r(z)=-2e^z(e^z-1)/[1+r(e^z-1)]^3$ differs from $-2z$ by at most $(4e^{2\tau}+e^{3\tau})z^2$ by the exponential Taylor inequalities. Integrating twice bounds the error from $-ad^3/3$ in the ratio derivative of $D_r(ad)/a^2$. The mode score and $|D''_r(z)-1|\le e^\tau |z|$ give $|m-\bar y|\le a e^\tau S^2/2$. The third-moment change is at most $3S^2|m-\bar y|$. The remaining term is $|\partial_r\log C|/h\le a^2/(4h^2)$. A strict sign across the level set, with a validated relevant face witness above $W$, excludes every interior maximum and the opposite endpoint of the completed ratio chart. At the optimized $r=0$ face the inward derivative reduces to $(\kappa\operatorname{Var}_{\rm emp}(e^{a(y-m)})-1)/2$, using $\overline{e^{a(y-m)}}=1$ and $f(\kappa)=\overline{D_0}$. This is the earlier local gamma correction in ratio coordinates. The uniform sign compares the entire slab on the archived fixed-power inputs. [Near-constant archive](2026-11-21-gb2-acc-report.md)

## Generalized gamma

Stacy's positive-power likelihood equations historically caused difficulties for iterative solvers. Prentice made the normal central and included both reflected signs. Noufaily and Jones prove uniqueness of individual conditional equations in their iteration and report secondary maxima in simulations. Their multistart evidence concerns numerical performance. [Stacy and Mihram (1965)](https://doi.org/10.1080/00401706.1965.10490268) · [Hager and Bain (1970)](https://scholarsmine.mst.edu/math_stat_facwork/1295/) · [Prentice (1974)](https://doi.org/10.1093/biomet/61.3.539) · [Noufaily and Jones](https://university.open.ac.uk/stem/mathematics-and-statistics/sites/www.open.ac.uk/stem-mathematics-and-statistics/files/files/Onthemaximisation.pdf)

For centered unit-variance logs $z_i$, use positive $t$ on each reflected side and $K(t)=\log\overline{e^{tz}}$. Put $B(k)=k\log k-k-\log\Gamma(k)$. The profile is $\log t+B(k)-kK(t)$, $f(k)=K(t)$. Prior bounds supply a signed normal seam, convex-conjugate secants on finite cells, and uniform power and Pareto tail comparisons. Gamma and Weibull are attained slices. The Prentice zero seam is an ordinary attained lognormal. [Stacy-side proof](2026-11-07-trgamma-seam-report.md) · [Prentice theory](2026-11-10-gg-theory-report.md) · [Existing fitter and held archive](2026-11-11-gg-fit-report.md)

**Proved finite multiplicity.** Take $x_i=\exp(z_i^{\rm raw})$ for the 39 exact decimal tokens under `multimodal.logs` in the data. The standardized positive-power score is positive at the left end and negative at the right end of each of $[0.64,0.69]$, $[11.9,12.3]$. Digamma recurrence to $k+64$ and the first two Stirling terms for $f$, with remainder in $[-1/(120(k+64)^4),0]$, enclose $k$ at the four arguments. Elementary intervals then enclose $1/t-kK'$. Analyticity and the endpoint signs give distinct finite local maxima. Their joint maxima persist under the smooth Stacy-to-Prentice map and profiling at fixed $Q$, whose nuisance fits are unique and locally continuous. The two $Q$'s differ since $k(t)$ decreases with positive $t$. A strictly higher power face wins on the whole sample. [Exact inputs and score intervals](2026-11-25-gbshape-math-data/gamma-results.json)

**Proved compact-cell refinement.** The curvature is
$$
 P''(t)=-1/t^2-kK''(t)+(K'(t))^2/(-f'(k)).
$$
Use increasing $K'$, decreasing $k(t)$, and decreasing positive $-f'(k)$. The ratio of the empirical tilt weights on $[l,u]$ is bounded between $e^{-(u-l)S}$ and $e^{(u-l)S}$, where $S$ is the range. Applying this inequality to the minimum of the respective tilted mean square distances bounds both endpoints of the variance term relative to $K''(l)$. Integrating the resulting curvature interval from a padded midpoint score excludes cells by sign. The prior value secant uses the convex conjugate $\sup_k[B(k)-kK]$ and its substituted concave upper's endpoint tangents. Strict sign rules out a stationary maximum even at an artificial subdivision boundary.

**Proved stable signed curvature through zero.** Define $D=2K/t^2$, $s=1/k$, $\eta=s/t^2$, and $f(1/s)=s/2+s^2\alpha(s)$. The partial fractions of $g$ above give
$$
 \tfrac1{12}-s^2/120\le\alpha(s)\le\tfrac1{12},\qquad
 -s/60\le\alpha'(s)\le0 ,
 \qquad
 \alpha(s)=2\int_0^\infty e^{-u}u\sum_{j\ge1}
               \frac{du}{s^2u^2+4\pi^2j^2}.
$$
The bound on the derivative follows by differentiating this positive integral and summing the squared denominators. Exact algebra gives
$$
 D=\eta+2t^2\eta^2\alpha,\quad
 P'=-D'/(2\eta)-2t\eta\alpha,\quad
 \eta'=\frac{D'-4t\eta^2\alpha-4t^3\eta^3\alpha'}
              {1+4t^2\eta\alpha+2t^4\eta^2\alpha'} .
$$
Differentiate the score to get curvature without cancellation of terms of order $1/t^2$. In particular $D'(0)=\overline{z^3}/3$, $D''(0)=(\overline{z^4}-3)/6$, and $|D''(t)-D''(0)|\le(11/10)S^5|t|$. The last uses $D=2\int_0^1(1-u)K''(ut)du$ and the fifth cumulant bound $|K^{(5)}|\le11S^5$. On the near-seam reconstruction, curvature on $[-3\cdot10^{-5},3\cdot10^{-5}]$ is enclosed by approximately $[-0.07438,-0.05889]$. The prior signed quadratic excludes the rest up to $|t|=0.04$. The numerical likelihood gain over zero is $3.6883\cdot10^{-16}$ per observation, below the existing production likelihood margin.

**Proved held-mode bounds.** At held $\mu$, replace $K$ by $J(t)=\overline{e^{t z_i}-1-t z_i}$, with $z_i$ the signed log deviations from $\mu$ in a fixed unit. The shape equation and convex-conjugate cover are unchanged in form. The small-power normal bound follows by integrating the exponential Taylor remainder directly, with no logarithm-of-moment term. For the far tail $B(k)\le\log k$, since $\Gamma(1+k)\ge\int_k^\infty k^k e^{-x}dx$. If $M=\max z_i>0$, the likelihood is bounded by $\log t-1-\log[(e^{tM}-1-tM)/n]$ by maximizing $\log k-kJ$. This decreases to $-\infty$ since the exponential remainder divided by $t$ increases. If all $z_i\le0$ with $d=-\bar z>0$, the face value is $-1-\log d$ and the excess is bounded by
$$
 \log(tdk)+1-tdk+k(\log k+\gamma_E).
$$
Use $J\ge td-1$ and $\log\Gamma(1+k)\ge-\gamma_E k$. Once $J\ge e^{\gamma_E}$, $k<1/J$ makes the bound strict. The face is reached as $k=1/(td)$, including the trace at observations tied with $\mu$. Reflection covers the other orientation. With $\sigma$ held, $k=Q^{-2}$ and $t=|Q|/\sigma$ in log units. The tail bound $\ell\le-\log(\sigma|Q|)$ now follows from $B(k)\le\log k$. On each nonzero $Q$ interval, bound $\log |Q|, B(k), k$, and $K$ or $J$ separately by their monotone endpoints. A zero-seam enclosure completes the compact scalar search domain.

**Proved balanced two-point case.** For equal counts at two distinct observations $K=\log\cosh t$ on both standardized sides, and the two face values are $-1$. On $0<t\le3$, $K/t^2$ decreases by decrease of $K''=\operatorname{sech}^2t$, and $K(3)/9\ge(3-\log2)/9>e/(4\pi)$. Stirling bounds the profile by $\log t-\tfrac12\log(4\pi e K)<-1$. For $t\ge3$, $k<1/K\le1/(3-\log2)$. The prior large-tail inequality bounds the excess by $\log(tk)+1-tk+k[\log(2k)+\gamma_E-1]<0$. No finite parameter reaches the supremum. Both power and Pareto reach it as limits.

## Genbeta

Use $v_i=\log(b/x_i)/\overline{\log(b/x)}$, $h=a\overline{\log(b/x)}$, $R_h=-\overline{\log(1-e^{-hv})}$, $c=-\overline{\log v}$. The objective is $F=\log h-ph-(q-1)R_h-\log B(p,q)$. The prior beta score rectangles, the gamma limit as $h\downarrow0$, moving-endpoint power limit as $h\to\infty$, and the $q$-eliminated upper handle diverging shape paths. Existing signed tails compare separated moving-power and gamma proposals. [Held-coordinate bounds](2026-11-07-genbeta-cert-report.md) · [Held-$q$ faces](2026-11-08-genbeta-rest-report.md) · [All-free bounds](2026-11-09-genbeta-allfree-data/proof.md) · [Gamma seam proof](2026-11-09-genbeta-allfree-data/review-gamma-expansion.md)

**Proved finite multiplicity.** Set $b=1$ and $x_i=e^{-d_i/100000}$, with
$$
 d=(262811,77046,47229,43074,2562,83263,31147,27686).
$$
The score has opposite endpoint signs in each of $[3.06994,3.29223]$, $[16.43531,17.62537]$ in $h$. The conditional beta score corners are checked by $\psi$ recurrence to $x+64$ and expansion through $1/(120(x+64)^4)$, with error between $-1/(252(x+64)^6)$ and zero. The remaining operations are elementary intervals on the exact $v_i=d_i/\bar d$. The analytic profile has an interior strict local maximum in each bracket. An independent exact trough is $x=(e^{-1/2},e^{-3/2})$ at $b=1$. Here $c=\frac12\log(4/3)$, $1<k<4$ from $f(k)=c$ and $\overline{v^2}=5/4$. The signed gamma quadratic $(k-1)(\overline{v^2}-1-1/k)/24$ is negative. Stirling bounds the gamma value by $c-\tfrac12\log(4\pi e c)<\log2-1$, the moving-power value. The profile dips below its left limit before rising above it. [Finite examples and interval signs](2026-11-25-gbshape-math-data/genbeta-modes-results.json)

### A centered chord and tighter score propagation

**Proved.** Put $\nu=h(p+(q-1)/2)$ and $T(z)=\log(\sinh(z/2)/(z/2))$. The earlier exact decomposition writes $F=g(q,\nu)+A_c+(q-1)\overline{T(hv)}$, where $g$ is the mean-one gamma likelihood and
$$
 A_c=\int_{-q/2}^{q/2}[\log(1+xh/\nu)+E(\nu/h+x)]\,dx,\qquad
 E(z)=\psi(z+1/2)-\log z .
$$
On $[l,u]$ with $d=\nu-qu/2>0$, differentiate at fixed $q,\nu$. The log term contributes negative curvature of magnitude at most $q^3/(12d^2)$. The positive integral kernel for $E$ gives $E''\ge0,\ E'\ge-1/(12z^3)$. The chain rule bounds the negative contribution after integration by $q\nu/(6d^3)$. Since $0\le T''\le1/12$ by the hyperbolic-sine product,
$$
 F_{hh}\ge-M,\qquad
 M=\frac{q^3}{12d^2}+\frac{q\nu}{6d^3}+\frac{(1-q)_+\overline{v^2}}{12}.
$$
The prior inequalities give $q_L=(e^{-u}-1/2)/c\le q\le q_U=(c-u^2\overline{v^2}/24)^{-1}$ when these bounds are positive, and $q/\operatorname{expm1}(h)<p<q/h+1$. Thus $\nu\le q_U+(q_U+1)u/2$ and $d\ge q_L[u/\operatorname{expm1}(u)-u/2]-u/2$ when the multiplier is positive. If $d>0$, transport any conditional maximizer at fixed $q,\nu$ to both endpoints. The whole profile is bounded by the larger endpoint profile upper plus $M(u-l)^2/8$, with $M$ bounded over the rectangle. This avoids the small-power $1/l^2$ chord excess.

For tighter conditional shape enclosures, propose a rectangle about the midpoint fit using the inverse information times the maximal statistic variation. Validate both inward scores at the lower/lower corner with their worst statistics, and at the upper/upper corner with the opposite statistics. Each score decreases in its own shape and increases in the other, so the four inward face signs follow. Strict concavity puts the true conditional maximum inside. Corner trigammas then bound the profile curvature through its Schur complement. Integrating the curvature interval from a padded point score supplies signed exclusions. A remaining strictly concave slab with opposite endpoint signs identifies the unique root. With $D_h=-R'_h=\overline{v/(e^{hv}-1)}$ and conditional information $\left(\begin{smallmatrix} A&-T_0\\-T_0&B_0\end{smallmatrix}\right)$, the envelope score is $1/h-p+(q-1)D_h$ and the curvature is
$$
 -1/h^2-(q-1)R''_h+\frac{B_0-2T_0D_h+A D_h^2}{A B_0-T_0^2}.
$$
Here $A=\psi_1(p)-\psi_1(p+q)$, $B_0=\psi_1(q)-\psi_1(p+q)$ and $T_0=\psi_1(p+q)$. [Proof criterion](2026-11-25-uniq-cover-math-report.md#proof-criterion)

### Profiled fourth-order gamma coefficient

**Proved.** The centered expansion is $F=g+h^2C+h^4D+O(h^6)$, with
$$
 C=\{(q-1)\overline{v^2}+q(1-q^2)/\nu^2\}/24,\qquad
 D=\frac{q(1-q^2)(3q^2-7)}{960\nu^4}
     -\frac{(q-1)\overline{v^4}}{2880}.
$$
At the gamma maximum $(k,k)$, let $I=-g''(k,k)$ and $c_1=\nabla C(k,k)$. The profiled fourth-order coefficient is $D(k,k)+c_1^\top I^{-1}c_1/2$. Held $q$ uses only the rate derivative, with information $1/q$.

For $h\le H$, set $b_0=qH/(2\nu)<1,\ d=\nu-qH/2$. An explicit upper on $|F-g-h^2C-h^4D|/h^6$ is
$$
 \frac{q^7}{2688(1-b_0^2)\nu^6}
 +\frac{q^5}{384(1-b_0^2)\nu^6}
 +\frac{7q^3}{1152d^6}+\frac{(1+1/32)q}{252d^6}
 +\frac{|q-1|\overline{v^6}}{181440}.
$$
For the proof, use the symmetric log series in $A_c$ and the exact reciprocal-square integral $q/[(\nu/h)^2-q^2/4]$. For the reciprocal fourth power the odd term integrates to zero and the second derivative is at most $20/(\nu/h-q/2)^6$. Digamma duplication gives $E(z)=f(z)-2f(2z)=1/(24z^2)-7/(960z^4)+\varepsilon$, with $|\varepsilon|\le(1+1/32)/(252z^6)$ by the alternating remainder. The product for $\sinh(z/2)/(z/2)$ gives the data term and remainder $z^6/181440$.

For the profiled remainder, use the existing centered score rectangle with $\|\Delta\|\le Lh^2$ for the nuisance displacement. Bound the raw remainder by $M_0$ over the rectangle and its segments to $(k,k)$. Taylor remainders add at most $h^6(G_3 L^3/6+C_2 L^2/2+D_1 L)$ where $G_3,C_2,D_1$ bound the derivative norms of $g,C,D$ at the indicated orders. Completing the square in $-\Delta^\top I\Delta/2+h^2c_1^\top\Delta$ yields the coefficient and upper. Evaluating the feasible $\Delta=h^2I^{-1}c_1$ gives the corresponding lower. A nonpositive quadratic upper and negative quartic upper can now close the seam, using the explicit sixth-order bound to choose the cutoff. Positive leading coefficients exclude gamma by a feasible lower witness.

## Python evidence and work

Each research script ran through `/tmp/umvl-cpu/run` on the workstation. The goldens use their stored binary64 observations. The genbeta quantile and gamma fixtures and generalized-gamma seam reflections reconstruct the mathematical input formulas. The Feller–Pareto cases use the exact binary64 archive observations and held minimum. Counts are visited cells, including splits and terminal cells. Inner solver iterations are separate.

| Problem | Decimal precision | Cells | Result |
|---|---:|---:|---|
| Generalized gamma golden | 60 | 415 negative + 71 positive | Unique finite winner, signed $t\approx-0.1625158867$. |
| Generalized gamma archived 39-point multiplicity case | 60 | 89 positive + 33 negative | Power face beats both finite modes. |
| Generalized gamma strict power and Pareto examples | 60 | 44 each | The named one-sided face wins each sample. |
| Generalized gamma balanced two-point archive | 60 | 46 | Both limits tie strictly above every finite point. The analytic proof covers equal counts. |
| Generalized gamma held $\mu=0$, $x=(1,2,3)$ | 60 | 22 | Pareto with endpoint 1 wins. |
| Generalized gamma near-seam pair | 85 | 289 per side, plus central slab | Unique finite winner for each reflected reconstruction. |
| Genbeta golden | 60 | 169 | Unique finite slab, $h\approx2.4280581509$. |
| Genbeta $(3,2,.7)$, $n=300$ midpoint-quantile hard case | 60 | 8,697 | Unique finite slab, $h\approx0.3424254768$, gain $2.7308252\cdot10^{-6}$ per observation over gamma. The centered seam bound reaches $h=0.06816727$. |
| Genbeta $n=300,\min Y=.05$ budget-limited gamma case | 60 | 371 | Gamma face wins. |
| Fixed-power Feller–Pareto archive 001625 and 001628 | 65 | One whole ratio slab each | Inverse transformed gamma wins. The score term is $4.49556\cdot10^{-13}$ with remainder $\le7.887\cdot10^{-22}$. Original log likelihood $127.81430798409595$ in both. |
| GB2 golden | 65 | 47 + 49 gamma-face cells, 779 finite cells | Both gamma faces and $a\le0.04862371,\ a\ge1144.94859$ are below the witness. This proves attainment. The finite cover leaves 395 cells. A separate box $[.249,.251]\times[.498,.502]$ is excluded. A radius-$10^{-10}$ ball contains the strict local root, with negative curvature lower magnitude $2.25\cdot10^{-6}$. |

The GB2 golden standardized witness is $-1.3958392060$ versus $-1.4049345843$ for the asymmetric Laplace optimum. The single uniform gamma endpoint slab over the retained range has $\log_{10}\rho\approx-3784$. The 720-sample GB2 scan used 52 powers on $[.075,60]$ and 23 ratios. Its one-dimensional checks confirmed zero pairs of finite power maxima. The genbeta proposal scan used 1,425 eight-point samples and 95 powers on $[.035,25]$ and found two finite-mode examples. The separate 85-digit genbeta quartic check tested 36 raw bounds and four profiled expansions on three decreasing power widths. [GB2 scan](2026-11-25-gbshape-math-data/gb2-modes-results.json) · [Genbeta modes](2026-11-25-gbshape-math-data/genbeta-modes-results.json) · [Quartic checks](2026-11-25-gbshape-math-data/genbeta-quartic-results.json)

The local GB2 check applies interval LDL pivots to the negative Hessian in the enclosing $(a,c,p,q)$ box, where $c=a\log b$ in the standardized coordinate. For a positive definite four-dimensional matrix, determinant divided by trace cubed lower-bounds its smallest eigenvalue. The score residual is smaller than that bound times the radius, giving an inward radial score and an interior stationary point, unique within the ball.

[Gamma cover](2026-11-25-gbshape-math-data/gamma_cover.py) · [Gamma results](2026-11-25-gbshape-math-data/gamma-results.json) · [Genbeta cover](2026-11-25-gbshape-math-data/genbeta_cover.py) · [Genbeta results](2026-11-25-gbshape-math-data/genbeta-results.json) · [Genbeta mode checker](2026-11-25-gbshape-math-data/genbeta_modes.py) · [Quartic checker](2026-11-25-gbshape-math-data/genbeta_quartic.py) · [GB2 cover](2026-11-25-gbshape-math-data/gb2_cover.py) · [GB2 results](2026-11-25-gbshape-math-data/gb2-results.json) · [GB2 mode scan](2026-11-25-gbshape-math-data/gb2_modes.py)

Reproduce from the worktree root.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
study=dev/research/evaluations/2026-11-25-gbshape-math-data
/tmp/umvl-cpu/run python "$study/gamma_cover.py"
/tmp/umvl-cpu/run python "$study/genbeta_cover.py"
/tmp/umvl-cpu/run python "$study/genbeta_quartic.py"
/tmp/umvl-cpu/run python "$study/genbeta_modes.py"
/tmp/umvl-cpu/run python "$study/gb2_cover.py"
/tmp/umvl-cpu/run python "$study/gb2_modes.py"
```

## What this changes for the fitter

- **GB2 and fixed-threshold Feller–Pareto.** For held $a$ with $\{p,q,b\}$ free, use relative logs, the lower $h_L$ and the explicit ratio-score error. A strict sign on the entire level slab and the corresponding fitted witness permit `TrGammaBoundary` or `InvTrGammaBoundary` with the power transported. For finite candidates the mode-centered chart gives scalar nuisance brackets, uniform exterior exclusions and whole-cell upper bounds. A stronger finite cover can sign the profiled scores $a^{-1}(1-\kappa\overline{zD'_r(z)})$ and $\partial_r\log C-\kappa\overline{\partial_rD_r(z)}$ over validated nuisance enclosures. A certified finite `Ok` requires the full finite-cell comparison and root inclusion, including transport $\log b=m-\operatorname{logit}(r)/a$.
- **Generalized gamma.** Use the stable signed central-slab curvature and the held-mode scalar cover with $J$. A separated finite root gives certified `Ok`, with $Q=0$ requiring its own stationary and global tests. A separated one-sided face gives the established limit outcome. Exact balanced two-point data permit a proved no-finite-MLE diagnostic retaining both equal unattained witnesses. Propagate the winning brackets through $Q=\operatorname{sign}(t)/\sqrt k$, $\sigma=s/(|t|\sqrt k)$, and the location map, using the public coefficient floors near zero.
- **Genbeta.** Use centered chords, inward-corner shape rectangles and curvature-transported scores within the existing compact cover. Retain the $q$-eliminated bound for large powers. The quartic and remainder formulas give a further signed seam test, including its one-rate version for held $q$. Finite acceptance requires joint coefficient inclusion after global exclusion. A proved gamma or moving-endpoint winner permits the established structured limit outcome and advice.
- **Fallback.** Meter cells and point bounds on the $128\min(\mathrm{iterlim},100000)$ ledger and each inner solve on its solver allowance. Unresolved optional comparison or budget retains the successful, accuracy-checked search with `certified=false`. A proved contradiction, nonexistence or strict limit decision, or an actual search or coefficient-accuracy failure takes its established diagnostic route.

## Open

- **GB2 golden and generic compact covers.** Finite attainment, strict local curvature and full exterior exclusions hold for the golden. The remaining 395 finite cells need sharper correlated nuisance and score enclosures outside a validated winning slab. Ratio and reciprocal-ratio arithmetic are needed close to the gamma seams. The analogous finite comparison remains open in the generic held-scale and single-held-beta-shape masks. At a winning face without a strict gap, uniform signed comparisons are still needed across intersecting exteriors outside the proved special regimes.
- **Multiplicity and global ties.** Two separated finite local maxima of the all-free GB2 power profile remain unverified. Its exact trough rules out unimodality. General finite global uniqueness and exact finite global ties for the all-free versions of all three families remain open.
- **Generalized-gamma held and flat cases.** The held-mode exterior proof is uniform and one archived held-mode case closes. Production covers for every held-mode and held-$\sigma$ mask and higher-order sign tests for flat zero seams remain to be validated.
- **Genbeta degenerate seams and deployment.** A simultaneous quadratic and quartic zero requires further expansion or a different comparison. The new covers close the mathematical hard-case reconstructions at high precision. Binary64 error propagation, generic held-$q$ winning faces and the broader seeded failure census still need targeted validation.
