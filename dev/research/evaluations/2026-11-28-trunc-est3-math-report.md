# Maximum likelihood with estimated truncation bounds in 25 continuous families

TASK-469. Source revision `bb912f76`. The 149 reviewed held masks give 447 estimated-window regimes. The [proof rows](2026-11-28-trunc-est3-math-data/proofs.csv) use the modifier atlas schema. Every result concerns positive likelihood and the ordinary fitted coefficient domain; a finite excluded face is a limit law, and an included parameter endpoint can itself maximize.

## Corrections and new exact cases

**Proved.** The same-law hypothesis in the estimated-bound transfer is essential. The earlier zeta review compares the tight parent-domain window $[2,3]$, with exponent $s>1$, and the looser declared $[1,3]$, with any finite real exponent. For $(2,3)$ the excluded harmonic height $6/25$ wins the joint comparison. For $(2,3,3,3)$ the looser signed fit wins, since its value at $s=-1$ is $1/24>24/625$, the tight-window supremum. Also, for held NB size zero with data $(1,2)$ and cuts $(1,\mathrm{NA})$, the tight event has zero ordinary parent mass, whereas the declared $[1,\infty)$ alias admits a logarithmic fit. The 25 families here have one common coefficient domain and parent law across the compared windows. [Earlier transfer](2026-11-27-trunc-est-math-report.md#joint-profiling-lemma), [zeta and alias review](2026-11-27-mod-rev5-report.md#proof-errors-and-scope-corrections), [known-window corrections](2026-11-27-mod-rev1-report.md#proof-errors-and-grading-corrections)

**Proved.** The Fréchet known-window proof omitted the increasing exponential on a physical upper-only window. With shape one, location $-D$, scale $D^2$, data $(-1,0)$ and fixed window $(-\infty,0]$, the conditional CDF at $x\le0$ tends to $e^x$. The review correctly restricted the original claim. The [profile below](#fréchet-upper-ray) completes the upper-only comparison at held shape and gives its full maximizing set on two distinct values with equal counts when shape is one. On that sample the exponential height is attained by a continuum of ordinary Fréchet laws, including when shape is free with its fitted lower bound of one. In contrast, with an estimated lower cut and two equally frequent values, the atom envelope strictly wins in the free-shape, free-location regime.

**Proved.** Positive likelihood is an explicit hypothesis for strictness. For example, hold Tukey $\lambda=1/2,\ \mu=0,\ \sigma=1$ and take data $(0,2)$ on $[0,2]$. The density at 2 is zero on every feasible window. At $\lambda=1$ with the same location and scale, the upper endpoint is 1 and has positive inward density. For data $(0,1)$ on $[0,\mathrm{NA}]$, every estimated upper cut at least 1 gives likelihood one. Likewise Fréchet data at a held location have zero density even though the endpoint belongs to the closed support. [Support convention](../../design/modifiers.md)

## Joint profiling and boundary conventions

Write $m=\min_i x_i,\ h=\max_i x_i$. The table gives the **nominal** window used in all subsequent formulas. Intersect it with the fitted parent support separately at each coefficient vector.

| Estimated cut | Tight window | Restrictions on the empirical sample |
| --- | --- | --- |
| Lower | $[m,b]$ | $h\le b$. The minimum is the lower cut. Upper equality requires $m=h=b$ for a singleton. |
| Upper | $[a,h]$ | $a\le m$. The maximum is the upper cut. Lower equality requires $a=m=h$ for a singleton. |
| Both | $[m,h]$ | Both extremes are included. For distinct data every strictly monotone transformed mean lies strictly between the transformed ends. |

**Proved.** Let $A_*$ denote the tight window. Any admissible nominal $A$ contains $A_*$. For each common coefficient vector $\theta$, with positive finite sample product $C_\theta=\prod_i f_\theta(x_i)$ and with $P_\theta(A_*)>0$, the conditional likelihood is $C_\theta/P_\theta(A)^n$, and
$$
\frac{C_\theta}{P_\theta(A)^n}\le \frac{C_\theta}{P_\theta(A_*)^n}.
$$
All observation densities use the same parent convention on both windows, including when a cut equals an observation. This inequality holds before and after intersecting with $S_\theta$, even when $S_\theta$ varies with $\theta$. For every joint candidate there is a tight candidate of at least that height, and every tight candidate is joint feasible. Their suprema coincide. A tight maximizing sequence is a joint maximizing sequence. If a finite joint winner exists, its coefficients must win on the tight window, and its nominal cuts tie exactly when $P_\theta(A\setminus A_*)=0$. Conversely every such pair wins. Thus a proven tight-window escape at positive mass cannot be rescued by moving a bound jointly with the coefficients. Arbitrarily loose cuts in a maximizing sequence can still be possible, especially where the support moves or the additional mass vanishes.

If the supremum is finite and positive, any joint maximizing sequence and its tightened version have likelihoods tending to the same value. Their ratio equals $[P_\theta(A_*)/P_\theta(A)]^n$, so the mass ratio tends to one. The corresponding conditional laws have total variation distance $1-P_\theta(A_*)/P_\theta(A)$, as can be read off the density on $A_*$ and its complement. Thus the same weak limits apply at a finite positive supremum. At an infinite supremum the mass-ratio conclusion need not hold.

For distinct data with positive parent densities, the span between the observations has positive mass in all 25 families. For a constant $c$ on a nominal positive-width window, the only additional obstruction is that all permitted overlap at a feasible coefficient is on a zero-width intersection. Such a coefficient admits **no** looser compatible positive-mass event if the only available expansion is towards the exterior of its support. Otherwise consider a compatible positive-mass expansion as below. In these families a coefficient with positive density at a sample and zero tight mass cannot yield a finite joint winner: the same coefficient with positive mass on progressively shorter windows yields unbounded likelihood. This also applies when some other coefficient vectors have positive tight mass. If the physical design contains $c$ on the boundary of a held support and offers expansion only out of that support, every window has zero event mass and the design is invalid.

**Proof for constants with coincident nominal cuts.** If both cuts are estimated, either inward side may be used. If only the lower cut is estimated and $b=c$, use the side to the left. If only the upper cut is estimated and $a=c$, use the side to the right. Fix any feasible coefficients positive at $c$ that have positive carrier length on that side. For a window shrunk to $c$ on that side, continuity from inside gives $P_\theta(A)\sim f_\theta(c)|A\cap S_\theta|$, so the likelihood is asymptotic to $|A\cap S_\theta|^{-n}$. For any neighborhood of $c$ the whole conditional mass eventually lies inside it. The weak limit is the point mass $\delta_c$. If the fixed cut already lies strictly beyond the constant, tightening alone gives a genuine ray or interval, and the appropriate coefficient escape test is still required. A constant alone does not imply nonexistence at positive width.

**Support and nominal bound sets.** At an interior observation the relevant estimated cut is unique at the sample extreme. At a fitted lower endpoint equal to $m$, every lower cut $\le m$ ties. At a fitted upper endpoint equal to $h$, every upper cut $\ge h$ ties. These are all the plateaus here because each parent has an interval support and is positive in its interior. Both extensions may apply on the same fit. At $0$ the Gompertz density is positive and lower cuts through $(-\infty,0]$ tie. At the open ends of Weibull, Lomax, log-logistic, inverse exponential and Power (zero), Kumaraswamy and logit-normal (zero and one), Double Pareto (zero) and log-gamma (one), an observation is incompatible. Fréchet requires $x_i>\mu$, including when location is held. Half-t and Pareto II accept equality at their moving lower endpoint. Power accepts equality at $\alpha$. Tukey with held positive shape has a bounded support with inward density zero at its ends for $0<\lambda<1$, and positive at $\lambda\ge1$. Such a zero-density contact cannot be a positive finite fit. At a fixed Gompertz or held half-t/Pareto II lower endpoint, a lower-estimated window whose fixed upper cut equals the endpoint has no inward expansion. The analogous upper-estimated exceptions include held Power $\alpha=c=a$ and a positive-density held Tukey upper endpoint. All other support cases follow the same inward-side test. In particular a nominal singleton is a different case from a coefficient-driven shrinking support at a positive-width nominal design.

There are no count families in this inventory. On an ordinary lattice with positive masses at adjacent counts, the real lower cut can tie on $(m-1,m]$ and the upper on $[h,h+1)$. If the adjacent mass is zero, apply the zero-removed-mass rule, including parent endpoint padding. Repeated extreme observations change the numerator product and sample moments, and leave the cut constraints unchanged. An admissible count singleton has unit conditional likelihood.

**Transfer of the reviewed known-window results.** For a fixed observed vector the tight bounds are numbers, and the positive-mass comparison holds for every candidate coefficient. A known-window proof quantified over every compatible sample and included endpoint observations therefore applies pointwise, with the transformed cuts recalculated and the nominal maximizing set restored as above. An excluded parameter path has the same joint sample height on the tight window. Conversely every joint path is bounded above by its tight path. Thus the listed failures classify causes of the supremum and give same-height tight witnesses. They need not classify the weak laws of *all* joint maximizing paths. A proved non-tight path with the same height can invalidate a stronger weak-limit claim. A one-sided known-window result stays one-sided unless a new argument covers the remaining cases. The [known-window family proofs](2026-11-27-trunc-shape-math-report.md) and [skew-family proofs](2026-11-27-trunc-skew-math-report.md) are used with their [shape](2026-11-27-mod-rev6-report.md#proof-errors-and-scope-corrections) and [skew](2026-11-27-mod-rev7-report.md#proof-errors-and-scope-corrections) corrections.

**Scalar test.** For an identifiable tilt $w(y)e^{tT(y)}$, the score per observation is $\bar T-E_tT$ and its derivative is $-\operatorname{Var}_t T<0$. A unique fit exists iff the statistic lies strictly inside the score range, except that an included finite coefficient at the winning endpoint attains. A finite excluded edge with integrable mass wins when the empirical mean is at or beyond its threshold. At an infinite tilt with all observations at the unique favored extremum, the normalized likelihood diverges and the law tends weakly to the corresponding atom. This follows by dividing numerator and denominator by their common value at the extremum, applying dominated convergence to the centered integral, and bounding mass away from the extremum by an exponential weight gap. A positive tilt on an interval with uniform carrier has endpoint means midpoint and upper cut; the negative tilt has lower cut and midpoint. The midpoint means $-\infty$ for the former if the lower cut is unbounded and $+\infty$ for the latter if the upper cut is unbounded. A signed tilt on a bounded interval matches the empirical mean strictly between the cuts, including ordinary uniform at zero. Use the integrable sign on a ray.

**General comparison.** The boundary height $B$ is the greatest approachable likelihood over the listed excluded faces, including contact factors, and zero when no positive limit applies. The cited exhaustion arguments bound every coefficient sequence that leaves compact subsets or approaches a nonadmissible endpoint. For finite $B$, attainment is equivalent to some positive finite feasible likelihood $\ge B$. Strict improvement forces a maximizing subsequence in the domain, while equality provides an attained fit unless another finite point wins. Include finite joins, support contacts and admitted coefficient ends. A supremum at $B=\infty$ is unbounded. A comparison against $B$ can decide attainment without bounding the number of finite maximizers. One-sided rows use only the named witnesses or scope. A named weak atom carries the indicated likelihood envelope, which is a limit of density products, rather than a density assigned to the point mass.

## Transformed positive families and normal or gamma reductions

The following coordinates define the thresholds and face families used in the regime sections. Always transform the **support-intersected** window and order its image cuts $L<U$, reversing them for decreasing transforms. The positive Jacobian factor in the observation product is independent of the fitted tilt or shape in each stated comparison. If a transform depends on a coefficient that is also profiled, retain its Jacobian in the joint comparison. A cut coinciding with a sample extreme freely estimated on the physical scale still coincides with the appropriate transformed extreme after any of these transformations.

| Mask | Coordinate and exact comparison on positive width |
| --- | --- |
| Weibull, held shape $k$ | $y=x^k$, negative exponential tilt of rate $\mathrm{scale}^{-k}$. Test $L<\bar y<(L+U)/2$ with the ray convention. Lower equality is collapse; upper failure gives $x^{k-1}dx$. |
| Weibull free and scale held | In $y=\log x$ use $e^{sy-ce^{sy}}$, $s=\mathrm{shape},\ c=\mathrm{scale}^{-s}$. Free scale gives signed log-coordinate exponential faces (positive slopes require a finite upper cut; nonpositive slopes require integrability); held scale leaves log-uniform on finite log width. Constants collapse at their common observation in the free fit, or at the projection of the held log scale in the held fit. |
| Log-logistic | In $y=\log x$ use logistic location $\log(\mathrm{scale})$ and inverse scale $\mathrm{shape}$. The same face and projection rules apply as for Weibull when shape is free. For held shape $k$, in $t=x^k$ use kernel $(t+D)^{-2}$, $D=\mathrm{scale}^k$. Compare against $t^{-2}$ at $D=0$ when $L>0$, and uniform in $t$ at $D=\infty$ when $U<\infty$. |
| Gompertz | $y=x$, kernel $e^{by-(a/b)e^{by}}$. Both free gives signed physical exponential faces, and constants (including zero) diverge. Held $a$ gives the rate-$a$ face at $b=0$; all-lower-cut constants at a positive cut diverge as $b\to\infty$. At zero, on finite width the density $a/[1-\exp(-a(e^{bU}-1)/b)]$ strictly decreases in $b$ since $(e^{bU}-1)/b=\int_0^U e^{bx}dx$ increases. On $[0,\infty)$ the density at zero equals $a$ at every $b>0$. Held $b$ gives a negative exponential tilt in $t=e^{bx}$ of rate $a/b$; upper failure gives the physical positive exponential of slope $b$. |
| Inverse exponential and Fréchet with shape and location held | Use $y=1/x$ and $y=(x-\mu)^{-\lambda}$, respectively, with a negative exponential tilt. Rate zero gives the physical carrier $x^{-2}$ or $(x-\mu)^{-1-\lambda}$, when integrable. Lower equality in $y$ collapses at the physical upper cut. |
| Fréchet with location held and shape free | In $y=-\log(x-\mu)$ use $e^{sy-ce^{sy}}$, $s=\lambda,\ c=\sigma^s$. The Weibull comparison in $y$ applies, with reversed order. Holding scale makes the projected log location $-\log\sigma$; the finite-width uniform face is log-uniform in the positive excess. |
| Lomax and Pareto II with threshold held | Put $y=x-L_x,\ W=U_x-L_x$, where $L_x$ is the effective lower support cut. Use $(s+y)^{-k-1}$ with $s=L_x+1/\lambda$ for Lomax and $s=\sigma+L_x-\mu$ for Pareto II. At fixed $s$ the negative tilt in $\log(1+y/s)$ has rate $k$. Its finite-width zero-rate carrier is the translated reciprocal $1/(s+y)$. Free $s$ may also give the power at its positive lower endpoint and, as $s\to\infty$, a decreasing exponential at matched $k/s$, uniform at zero slope on finite width. Held $k$ restricts the last face to uniform. At $s\downarrow0$ with $j>0$ lower ties and fixed $k$, the small-scale product is asymptotic to $k^ns^{k(n-j)-j}\prod_{y_i>0}y_i^{-k-1}$; use the negative, zero or positive exponent, with a weak atom for fixed $k$. This case is possible at the closed Pareto II threshold; the Lomax support excludes zero. An unbounded upper ray allows non-tight joint small-$s$, small-$k$ paths, as in the review. |
| Kumaraswamy | At held $a$, $T=\log(1-x^a)$ has positive tilt $b$ against uniform carrier; the lower face is the reciprocal carrier $x^{a-1}/(1-x^a)$. At held $b$, $y=-\log x$ and kernel $e^{-ay}(1-e^{-ay})^{b-1}$ give the small-$a$ log-power $y^{b-1}$ on finite upper $y$, and large-$a$ collapse for all data at the positive lower $y$ cut. At $b=1$ use the negative tilt in $y$ with its unique root. With both free, use the two integrable families $y^{b-1}$ and $1/(e^{ay}-1)$, including their joint $1/y$ corner where integrable. Constants collapse. |
| Logit-normal | $y=\log[x/(1-x)]$. Free location and scale: for distinct data match the mean in the signed linear tilt and require empirical variance strictly below its variance. The linear face wins at equality or above, uniform in logit at zero slope. On a lower ray compare with $(\bar y-L)^2$; on an upper ray compare with $(U-\bar y)^2$. Constants give atom divergence. With $\mu$ held, use the negative tilt in $(y-\mu)^2$ between its projected minimum and its uniform mean (infinite if unbounded), with the projection atom and uniform-logit failure modes. With $\sigma$ held use the linear Gaussian-carrier tilt with mean strictly between the cuts, either equality giving an atom. Each active coordinate is identified. |
| Log-gamma | $y=\log x$, kernel $y^{k-1}e^{-vy}$. At held $k$, test $\bar y$ between $L$ and the zero-rate carrier mean (infinite on a ray). At held $v$, test $\overline{\log y}$ between the zero-shape mean (minus infinity if $L=0$) and $\log U$. These give zero-rate power, zero-shape damped reciprocal and the corresponding endpoint atoms. Both free and nonconstant: at $k=0$ (allowed in the closure when $L>0$) match $\bar y$ then require $\overline{\log y}\le E\log Y$. At $v=0$ (when $U<\infty$) match $\overline{\log y}$ then require $\bar y\ge EY$. At their integrable corner use both inequalities. An edge with no tangential root is omitted. Coercivity on distinct observations and strict concavity give a unique fit iff no excluded face passes. Constants collapse. |

## Power and moving endpoints

**Proved.** For Power put $L=\max(0,a_*)$, $u=\min(b_*,\alpha)$, where $a_*,b_*$ are the nominal tight cuts. Require positive observations, $\alpha\ge h$ and $u>L$. With free $\alpha$ and $h>L$ choose $u=h$ because lowering $u$ strictly decreases the integral of $x^{\beta-1}$. At positive width with free $\beta$, the exact conditions are $\overline{\log x}<\log u$ and, if $L>0$, $\overline{\log x}>(\log L+\log u)/2$. The lower failure selects the excluded log-uniform, and the upper equality diverges towards $\delta_u$. This is the positive scalar tilt in $\log x$. If $\alpha$ is free and $h=L$, any compatible shrinking path gives an atom, for example $u\downarrow h$ when the upper window allows it. At an attained fit, if $b_*>h$ the unique $\alpha$ is $h$, and if $b_*=h$ every $\alpha\ge h$ wins. For an estimated upper cut, the exact joint set in this last branch has $\min(\alpha,b)=h$, with both arguments at least $h$. Thus the pair can move along either of two rays, including their meeting point. For held $\alpha=h$ the entire nominal upper ray ties. For example, take $(1,1,2)$ on $[0,\mathrm{NA}]$, for which $\widehat\beta=3/(2\log 2)$. On $[\mathrm{NA},\mathrm{NA}]$ instead take $(1,2,2)$, with mean log strictly above the log midpoint and below $\log 2$. Both demonstrate genuine coefficient and bound multiplicity.

**Proved.** With Half-t and Pareto II, write $c$ for the parent threshold. For a physical lower cut at $m$, every feasible $c\le m$ leaves the effective lower cut at $m$. In Half-t, fixing $v,\sigma>0$, set $d=m-c$, $s=\sigma\sqrt v$, and $y=x-m$. The kernel is $(s^2+(d+y)^2)^{-(1+v)/2}$ on $0\le y\le W$. Use the reviewed Half-t comparison with this domain $d\ge0$. In particular $c=m$ is **a candidate**, and exterior $c<m$ cannot be discarded. For example $(0,1)$, with $v=3,\ s=1$ on $[0,1]$, has supremum 1 as $c\to-\infty$. The angular chord inequality for inverse quadratics and Jensen for their square give strictly smaller likelihood at every finite $c$. For a fixed physical lower cut $a<m$, keep the branch $c\le a$, and among $c\ge a$ use $c=m$ whenever $m<h$ or $m<b_*$. This follows because raising $c$ up to $m$ increases every kernel value and reduces the residual integration interval $[0,b_*-c]$. At $a=-\infty$ this gives the unique threshold for fixed shape and scale if $m<b_*$. At $m=b_*$ with free $c$, shrinking the residual width gives unbounded likelihood. Any winner at $c=m$ permits the entire nominal estimated lower ray. [Half-t power comparison](2026-11-27-trunc-skew-math-report.md#fixed-power-shape)

**Proved.** In Pareto II put $p=c-\sigma$, with conditional kernel $(x-p)^{-k-1}$ for $x\ge c$. With $p,k$ held for the comparison, increasing $c$ up to $m$ improves the likelihood whenever it cuts out positive mass. For a tight lower cut at $m$, every feasible $p<m$ gives the whole fiber $p<c\le m,\ \sigma=c-p$ if both threshold and scale are available. For an estimated lower cut $\ell\le m$, the joint fiber additionally requires $\max(c,\ell)=m$. On an upper-estimated design with fixed $a<m$, a winning free threshold for this fixed-pole profile must be $m$. For free shape, threshold and scale, take $c=m$ and sufficiently small fixed $k>0$ in the small-scale exponent from the previous table, giving divergence for every sample. If $m=b_*$ and $a_*<m$, shrink the support width instead. On unbounded upper width the reviewed example $(1,2)$, $c=1,\ \sigma\downarrow0,\ k=\sigma^{1/4}$, also diverges as $\sigma^{-1/2}$ while the conditional mass leaves every bounded interval. The atom is an available witness, not a necessary weak limit.

**Proved scalar refinement on the Pareto II upper ray.** When the physical interval is lower-tight and the effective width is infinite, for held $k>0$ use $s=m-p,\ g_i=x_i-m,\ j=\#\{g_i=0\}$, and $E=k(n-j)-j$. The product is
$$
Q(s)=k^n s^E\prod_{g_i>0}(s+g_i)^{-k-1},\qquad
\partial_{\log s}\log Q=E-(k+1)\sum_{g_i>0}\frac{s}{s+g_i}.
$$
The score decreases strictly with $s$ for distinct data and its right limit is $-n$. On $s>0$, $E>0$ therefore gives one winning pole, $E=0$ a finite excluded lower atom envelope and $E<0$ divergence. Constant data have score $-n$. With both threshold and scale free the attained winner has the full fiber above. With held threshold $c\le m$ and free scale, replace $s>0$ by $s>m-c$. A root above this lower bound is unique; otherwise the open endpoint wins, with the small-scale atom when $c=m$ and the regular shifted power when $c<m$. With held scale $\sigma$ and free threshold, use $s\ge\sigma$; the maximum is the root clipped at $\sigma$, or $\sigma$ if no root exists. That endpoint is included and corresponds to $c=m$. These at-once tests can also be used for a known lower cut equal to $m$. On finite width use the reviewed face comparison including its normalization factor $1-(1+W/s)^{-k}$.

### Fréchet upper ray

**Proved.** Suppose the physical window is $(-\infty,h]$ with data maximum $h$, and shape $s>0$ is held. For $d=m-\mu>0$ set $g_i=x_i-m,\ G=h-m,\ c=\sigma^s$. Conditional on $X\le h$, the transformed variable $(X-\mu)^{-s}-(h-\mu)^{-s}$ is exponential of rate $c$, since the conditional CDF of $X$ is $\exp[-c((x-\mu)^{-s}-(h-\mu)^{-s})]$ on the parent interval. Distinct data give $G>0$ and
$$
H(d)=\sum_i[(d+g_i)^{-s}-(d+G)^{-s}]>0,\qquad
c_*(d)=n/H(d),\qquad
Q(d)=(ns/e)^n H(d)^{-n}\prod_i(d+g_i)^{-s-1}.
$$
This is obtained by differentiating $n\log c-cH(d)$, strictly concave in $c$. The profile is smooth for $d>0$. Write $j=\#\{g_i=0\}$ and $E=s(n-j)-j$. As $d\downarrow0$, $H(d)\sim j d^{-s}$, giving $Q(d)\sim(ns/(ej))^n d^E\prod_{g_i>0}g_i^{-1-s}$. At $E=0$ the conditional laws tend to $\delta_m$ since $c_*(d)\asymp d^s$ and the conditional CDF tends to one at every fixed point above $m$ and zero at each point below $m$. For $E<0$ the same path diverges. As $d\to\infty$, $H(d)\sim s d^{-s-1}\sum_i(h-x_i)$, so the profile tends to $(r/e)^n$ with $r=1/(h-\bar x)$. The normalized CDF converges to the reflected exponential of rate $r$ at $h$. For $E\ge0$ these are the only profile ends, and a finite fit exists iff a positive finite candidate meets or exceeds both limiting heights. At a constant $m=h$ the transformed sum vanishes and $c\to\infty$ at fixed $d$ gives the upper atom with unbounded likelihood. These results complete the held-shape theorem on the upper-only branch omitted by the known-window proof. For upper-estimated finite fixed lower cut use the finite-cut comparison, with its contact denominator switching at $a=m$.

**Proved.** If the upper-only sample has exactly two values at equal frequency, put their positive gap $G$ and let each count be $j$. With $t=d/(d+G)$, the profiled likelihood to power $1/j$ is
$$
4e^{-2}\frac{s^2t^{s+1}}{d^2(1-t^s)^2}\le \frac{4e^{-2}}{G^2}\qquad(s\ge1).
$$
The inequality follows from $\sinh(su)\ge s\sinh(u)$ for $u=-\log(t)/2>0$, with equality exactly at $s=1$. Thus held $s>1$ exclusively selects the reflected exponential, held $s=1$ has every $d>0$ with $c=c_*(d)$ maximizing, and held $s<1$ diverges by $E<0$. Free shape with free location on the fitted domain $s\ge1$ has exactly the same continuum at $s=1$, and the estimated upper cut itself is unique. At $(-1,0)$, e.g. $d=1$ and $d=2$ attain at scales 4 and 12. With lower-tight window instead, the known sharp half-contact bound applies at any distinct sample with half the observations at $m$, regardless of the positive remaining gaps. Its strict unattained height is $\prod_{g_i>0}g_i^{-2}$, using the CDF increment pairing and the $s=1,d\downarrow0,q\downarrow0$ contact witness. The difference is in the geometry of the physical window: the left ray retains probability below $m$, while a tight lower cut deletes it. [Half-contact proof](2026-11-27-trunc-shape-math-report.md#frechet-free-truncateknown)

## Particular skew-family comparisons

**Proved transfers.** The table identifies the coefficient comparison referred to by the regime rows. At a finite cut retain the pointwise edge factor from the cited proof. For a genuinely finite tight window, the endpoint-pair test below uses the entire retained window.

| Comparison | Faces and tests on the tight window |
| --- | --- |
| GED, skew GED and two-piece normal at fixed stretched-exponential power | Use the finite held-power coefficients and compare with uniform, admissible signed exponential tilts and concentration at the projected peak or cut. Evaluate the limits using the physical split location implied by the held mean and scale. For GED with mean and power held the negative tilt in $|x-\mathrm{mean}|^\nu$ uses the projected minimum and the uniform moment (infinite if unbounded). |
| Two-piece skew-only | Compare the two skew endpoints. The positive-width surviving side gives a half-density, with the actual moving join factor at observations, or an exterior exponential/power or collapse when the surviving side is cut away. A half-density here means the one-sided kernel normalized on its retained interval. |
| GED, skew GED and standardized Student shape-only | Evaluate the scalar shape ends with the retained support and contact values. GED's high-shape limit is a standardized uniform with its endpoint factor. In standardized Student the upper df face is Gaussian at fixed skew transformation; the low-df path uses shrinking scale at fixed SD. The strict $j>2n/3$ tie condition at the mean suffices for divergence as df decreases to 2. |
| Azzalini skew-normal, $\alpha$ held or only $\alpha$ free | Use the normal core times $\Phi(\alpha z)$. At fixed location and scale and free skew use the two signed half-normal limits, with factor $1/2$ at the join, and cut concentration where the retained interval is on the nonsurviving side. At fixed skew compare the allowed uniform and signed tilted limits as well as concentration. |
| Fixed power tails (sstd, ST, LST, Slash, Half-t and negative Tukey) | For tail exponent $\gamma>1$, small scale and a center tending to $z$ carrying $j$ observations has exponent $\gamma(n-j)-n$. Include the feasible critical angular product when zero, exterior powers $|x-z|^{-\gamma}$, and uniform only on bounded effective width. The half-t support changes the inward angular domain; upper contact with free endpoint can shrink the whole interval. |
| LST and Half-t with free df | In addition to fixed-df powers and contact, include the normal or half-normal, signed exponential tilts from remote centers (only the decreasing sign for Half-t), bounded uniform, and small-df inverse-square-root or reciprocal-distance kernels when integrable. With both location and scale held and effective center retained at a tie, the small-df normalizer on a bounded window is order $\log(1/s)$ for $s=\sigma\sqrt{\mathrm{df}}$, giving divergence. An effective ray instead has normalization of order $1/\mathrm{df}$ and this path gives zero likelihood. With scale free, non-tight divergent ray paths can occur as in the review. |
| Huber | Use hold-compatible uniform, exponential, Laplace and Gaussian faces and concentration. The physical core width and tail slopes determine the admissible envelope. Required positive $\epsilon$ holds are interpreted as in the parent dossier. |
| Tukey with shape held | At $\lambda<0$ the preceding power criterion uses $\gamma=1+1/|\lambda|$; at zero use logistic. Positive shape has finite support. Include contact of the retained interval, broad uniform and broad inward edge powers with exponent $\max(0,1/\lambda-1)$. At $\lambda=1$ the uniform is an ordinary member: compare the shortest overlap allowed by the holds and keep its whole preimage. Holding scale restricts the support lower edge to $[h-2\sigma,m]$. The overlap width is concave there, so a minimum is at an endpoint and every tied minimizer is allowed. Holding location instead uses the least feasible scale and its overlap plateau. |
| Bhattacharjee | Broad scale or width gives uniform on bounded width. At fixed positive half-width, sharp scale gives the retained uniform with half-density at included parent-edge contacts, or a cut atom if the overlap disappears. Zero half-width is the included Gaussian, with scalar projected-square moment when center is held. Remote location at fixed scale and width gives the favored cut atom for cut constants. |
| EMG | Held rate and location, varying Gaussian scale: broad uniform on bounded width, sharp translated exponential with half factor at the join, or upper-cut collapse. Held rate and scale, varying location: reflected location escape on the right gives upper-cut concentration for cut constants; the opposite end gives the retained exponential of rate $\lambda$ when integrable. Held location and scale, varying rate: Gaussian at rate infinity, Gaussian-CDF kernel at zero where integrable. |
| Double Pareto | In $y=\log x$, fixed log kink $c$ has kernel $e^{-\alpha(c-y)_+-\beta(y-c)_+}$. Rate zero replaces the finite side by uniform; on an infinite side its mass diverges. Infinite rate deletes a side and contributes numerator zero unless no observations have positive distance on that side. If both sides vanish, only all-kink data give divergence; otherwise the exponential penalty beats the normalization. Factor out the cut value for an exterior kink. Strict covariance identifies active rates; inactive free rates are unrestricted. For free kink the log numerator is affine between successive knots and the normalizer is strictly concave between finite cuts and interior knots. Thus the log likelihood is strictly convex in each gap. Compare all observed knots and finite cuts, including exterior kink plateaus, and keep every finite winning component. Every estimated window here has at least one finite effective cut, so whole-line interior plateaus from the known-window review do not arise. |

**Exact endpoint test.** On a finite physical interval $[m,h]$ with data $(m,h)$, a log-concave kernel $k$ satisfies $\int k\ge(h-m)\sqrt{k(m)k(h)}$, by integrating the exponential chord and using Jensen. Equality requires a constant kernel. Thus under the stated mask conditions the excluded physical uniform wins if all finite candidates are nonconstant and a broad flat path is available. For GED, skew GED and their held-shape examples require $\nu\ge1$; for other endpoint-pair cases use the reviewed log-concavity and broad path. For Half-t or Student with fixed df at least one use instead the angular inequality for an inverse quadratic and Jensen for its power $(1+\mathrm{df})/2$. The same chord proof in the transformed coordinate gives the log-uniform or logit-uniform examples; the sample Jacobian multiplies both sides. In a single-estimated design this exact witness requires the other cut to equal the opposite observed extreme, so the positive-width comparison is identical to the both-estimated witness.

**Proved Pareto II finite-width multiplicity.** At held $k=2$ and data $(1,1,11/10,2)$ on $[1,2]$, the small-$s$ exponent is 2, so the product tends to zero there. At $s\to\infty$ the height tends to the uniform height 1. At $s=1$, the exact product is $(8/3)^4 (10/11)^3/8>1$. Continuity then gives a finite winning pole. Each such pole gives every threshold in its fiber, with $\max(c,\ell)=1$ when the lower cut is estimated. This sample realizes the window under every estimated-cut design, fixing the opposite cut at the relevant sample extreme in the one-cut cases.

### Fréchet rays with scale held

**Proved.** On a physical upper-only ray with free location, $d=m-\mu\to\infty$ and $\lambda/d\to r<\infty$ give the same pivot-normalized decreasing exponential as the known-window proof. It has infinite integral to the left, including at $r=0$, so Fatou gives zero likelihood. If $\lambda/d\to\infty$ and the data are distinct, integrate to the right of $m$ for a distance $d/\lambda$, eventually shorter than the gap $h-m$. This bounds the minimum density by $O(\lambda/d)$, and the other observations lose an exponential factor in $\lambda/d$. The finite-location comparisons in the known-window proof use positive-width segments between the data or the contact formula and apply on this ray. Thus constants diverge, but with distinct data there is no positive excluded face and an attained fit follows. With both shape and scale held the remote-location limit likewise vanishes by Fatou, and with $m<h$ the contact bound also vanishes. If $m=h$, the moving location can shrink the retained width to zero at the upper cut. Neither argument proves general uniqueness of the finite locations.

### Exact reflected maxima for LST and Slash

**Proved.** Use the sample with one copy of each of $-100,100$ and 50 copies of each of $-4,4$. Its tight window is $[-100,100]$ under both estimation, lower estimation with fixed upper cut 100, and upper estimation with fixed lower cut $-100$. For LST hold df 3 and $\sigma=1/\sqrt3$, and for Slash hold $\sigma=1$. In each case the only free parent coefficient is the center. The objective is continuous and even in it. For either polynomial-tail kernel the uniform is the sole height at remote centers by compact ratio convergence. We show that the trial center 4 beats both remote uniform and the centered trial, so a global maximum occurs at a finite nonzero center and reflection gives a distinct one. The estimated cuts themselves are unique.

For LST use $k(z)=(1+z^2)^{-2}$, with full integral $\pi/2<2$. The normalizer at zero exceeds $\int_{-1}^1 k(z)\,dz=\pi/4+1/2>1$. The conditional inner-pair density product at center 4 is at least $1/(4\cdot65^2)$, over twice both the centered bound $17^{-4}$ and the uniform $1/40000$. The outer-pair product at 4 is at least $1/(4\cdot10817^4)$, against centered bound $10001^{-4}$ and uniform $1/40000$. Multiplication by the inner-pair ratio bound $2^{50}$ beats both losses.

For Slash use $k(z)=(1-e^{-z^2/2})/z^2$, with $k(0)=1/2$ and full integral at most 3 by splitting at $|z|=1$. The symmetric-window normalizer decreases strictly in the distance of the center from zero, since $k$ decreases strictly in $|z|$. The trial inner-pair numerator is $(1-e^{-32})/128$ and the centered one is below $1/256$, so their conditional inner-pair ratio exceeds $3/2$. The trial outer-pair numerator exceeds $1/(4\cdot96^2\cdot104^2)$, over one quarter of the centered bound $100^{-4}$. Thus the full trial exceeds the centered value by a factor above $(3/2)^{50}/4>1$. Against uniform each inner pair improves by more than a factor two, since the trial conditional pair exceeds $1/2304$. The trial conditional outer pair exceeds $1/(36\cdot104^4)$, so the factor $2^{50}$ beats its possible loss against uniform.

<!-- regime results -->

## Regime summary

“Iff” counts masks with a proved complete attainment criterion in each of their three designs. Other masks have scoped witnesses or necessary conditions. Every regime has the exact bound profile and singleton test.

| Family | Reviewed masks | Iff masks | Estimated regimes |
| --- | ---: | ---: | ---: |
| ged | 7 | 5 | 21 |
| sged | 12 | 6 | 36 |
| sn | 7 | 5 | 21 |
| snorm | 7 | 5 | 21 |
| sstd | 12 | 5 | 36 |
| st | 2 | 2 | 6 |
| lst | 7 | 7 | 21 |
| slash | 3 | 3 | 9 |
| huber | 7 | 7 | 21 |
| ht | 8 | 8 | 24 |
| tlambda | 6 | 4 | 18 |
| bhatt | 7 | 4 | 21 |
| emg | 7 | 4 | 21 |
| logitnorm | 4 | 4 | 12 |
| dblpareto | 8 | 8 | 24 |
| lgamma | 4 | 4 | 12 |
| weibull | 4 | 4 | 12 |
| lomax | 4 | 4 | 12 |
| llogis | 4 | 4 | 12 |
| gompertz | 4 | 4 | 12 |
| frechet | 8 | 7 | 24 |
| invexp | 2 | 2 | 6 |
| power | 4 | 4 | 12 |
| kumar | 3 | 3 | 9 |
| pareto2 | 8 | 8 | 24 |

## Results by family, hold and estimated cut

### ged all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint scale and shape limits, including compact-uniform contact at large shape. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-all-free-truncateknown)

### ged all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint scale and shape limits, including compact-uniform contact at large shape. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-all-free-truncateknown)

### ged all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint scale and shape limits, including compact-uniform contact at large shape. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-all-free-truncateknown)

### ged mean truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint scale and shape limits, including compact-uniform contact at large shape. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-mean-truncateknown)

### ged mean truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint scale and shape limits, including compact-uniform contact at large shape. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-mean-truncateknown)

### ged mean truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint scale and shape limits, including compact-uniform contact at large shape. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-mean-truncateknown)

### ged mean+nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, strictly bracket the sample mean of the absolute residual to the held power.

**Proved · Failure.** On compatible positive width, lower equality gives projection collapse. At or above the bounded upper threshold only the uniform wins.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meannu-truncateknown)

### ged mean+nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, strictly bracket the sample mean of the absolute residual to the held power.

**Proved · Failure.** On compatible positive width, lower equality gives projection collapse. At or above the bounded upper threshold only the uniform wins.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meannu-truncateknown)

### ged mean+nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, strictly bracket the sample mean of the absolute residual to the held power.

**Proved · Failure.** On compatible positive width, lower equality gives projection collapse. At or above the bounded upper threshold only the uniform wins.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meannu-truncateknown)

### ged mean+nu+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meannusd-truncateknown)

### ged mean+nu+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meannusd-truncateknown)

### ged mean+nu+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meannusd-truncateknown)

### ged mean+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meansd-truncateknown)

### ged mean+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meansd-truncateknown)

### ged mean+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-meansd-truncateknown)

### ged nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-nu-truncateknown)

### ged nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-nu-truncateknown)

### ged nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-nu-truncateknown)

### ged nu+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-nusd-truncateknown)

### ged nu+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-nusd-truncateknown)

### ged nu+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ged-nusd-truncateknown)

### sged all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-all-free-truncateknown)

### sged all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-all-free-truncateknown)

### sged all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-all-free-truncateknown)

### sged mean truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-mean-truncateknown)

### sged mean truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-mean-truncateknown)

### sged mean truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-mean-truncateknown)

### sged mean+nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannu-truncateknown)

### sged mean+nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannu-truncateknown)

### sged mean+nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannu-truncateknown)

### sged mean+nu+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannusd-truncateknown)

### sged mean+nu+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannusd-truncateknown)

### sged mean+nu+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannusd-truncateknown)

### sged mean+nu+sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannusdxi-truncateknown)

### sged mean+nu+sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannusdxi-truncateknown)

### sged mean+nu+sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannusdxi-truncateknown)

### sged mean+nu+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannuxi-truncateknown)

### sged mean+nu+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannuxi-truncateknown)

### sged mean+nu+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meannuxi-truncateknown)

### sged mean+sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meansdxi-truncateknown)

### sged mean+sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meansdxi-truncateknown)

### sged mean+sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meansdxi-truncateknown)

### sged mean+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meanxi-truncateknown)

### sged mean+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meanxi-truncateknown)

### sged mean+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-meanxi-truncateknown)

### sged nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nu-truncateknown)

### sged nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nu-truncateknown)

### sged nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nu-truncateknown)

### sged nu+sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nusdxi-truncateknown)

### sged nu+sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nusdxi-truncateknown)

### sged nu+sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nusdxi-truncateknown)

### sged nu+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nuxi-truncateknown)

### sged nu+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nuxi-truncateknown)

### sged nu+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-nuxi-truncateknown)

### sged xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-xi-truncateknown)

### sged xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-xi-truncateknown)

### sged xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum. A held nu must be at least one.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled shape, skew and scale motion, including moving uniform edges and skew joins. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sged-xi-truncateknown)

### sn all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-all-free-truncateknown)

### sn all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-all-free-truncateknown)

### sn all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-all-free-truncateknown)

### sn alpha truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alpha-truncateknown)

### sn alpha truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alpha-truncateknown)

### sn alpha truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alpha-truncateknown)

### sn alpha+omega truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaomega-truncateknown)

### sn alpha+omega truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaomega-truncateknown)

### sn alpha+omega truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaomega-truncateknown)

### sn alpha+omega+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaomegaxi-truncateknown)

### sn alpha+omega+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaomegaxi-truncateknown)

### sn alpha+omega+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaomegaxi-truncateknown)

### sn alpha+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaxi-truncateknown)

### sn alpha+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaxi-truncateknown)

### sn alpha+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-alphaxi-truncateknown)

### sn omega+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the two favored-half or wrong-side collapse ends of alpha. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-omegaxi-truncateknown)

### sn omega+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the two favored-half or wrong-side collapse ends of alpha. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-omegaxi-truncateknown)

### sn omega+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the two favored-half or wrong-side collapse ends of alpha. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-omegaxi-truncateknown)

### sn xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-xi-truncateknown)

### sn xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-xi-truncateknown)

### sn xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sn-xi-truncateknown)

### snorm all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint skew and scale motions at the moving one-sided normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-all-free-truncateknown)

### snorm all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint skew and scale motions at the moving one-sided normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-all-free-truncateknown)

### snorm all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint skew and scale motions at the moving one-sided normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-all-free-truncateknown)

### snorm mean truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint skew and scale motions at the moving one-sided normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-mean-truncateknown)

### snorm mean truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint skew and scale motions at the moving one-sided normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-mean-truncateknown)

### snorm mean truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint skew and scale motions at the moving one-sided normal join. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-mean-truncateknown)

### snorm mean+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meansd-truncateknown)

### snorm mean+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meansd-truncateknown)

### snorm mean+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meansd-truncateknown)

### snorm mean+sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meansdxi-truncateknown)

### snorm mean+sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meansdxi-truncateknown)

### snorm mean+sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meansdxi-truncateknown)

### snorm mean+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meanxi-truncateknown)

### snorm mean+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meanxi-truncateknown)

### snorm mean+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-meanxi-truncateknown)

### snorm sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-sdxi-truncateknown)

### snorm sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-sdxi-truncateknown)

### snorm sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-sdxi-truncateknown)

### snorm xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-xi-truncateknown)

### snorm xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-xi-truncateknown)

### snorm xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, broad uniform, allowed exponential tilt, or divergent projection or cut collapse.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#snorm-xi-truncateknown)

### sstd all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-all-free-truncateknown)

### sstd all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-all-free-truncateknown)

### sstd all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-all-free-truncateknown)

### sstd mean truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-mean-truncateknown)

### sstd mean truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-mean-truncateknown)

### sstd mean truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-mean-truncateknown)

### sstd mean+nu+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannusd-truncateknown)

### sstd mean+nu+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannusd-truncateknown)

### sstd mean+nu+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannusd-truncateknown)

### sstd mean+nu+sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannusdxi-truncateknown)

### sstd mean+nu+sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannusdxi-truncateknown)

### sstd mean+nu+sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannusdxi-truncateknown)

### sstd mean+nu+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannuxi-truncateknown)

### sstd mean+nu+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannuxi-truncateknown)

### sstd mean+nu+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meannuxi-truncateknown)

### sstd mean+sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meansd-truncateknown)

### sstd mean+sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meansd-truncateknown)

### sstd mean+sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meansd-truncateknown)

### sstd mean+sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meansdxi-truncateknown)

### sstd mean+sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meansdxi-truncateknown)

### sstd mean+sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meansdxi-truncateknown)

### sstd mean+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meanxi-truncateknown)

### sstd mean+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meanxi-truncateknown)

### sstd mean+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-meanxi-truncateknown)

### sstd nu+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-nuxi-truncateknown)

### sstd nu+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-nuxi-truncateknown)

### sstd nu+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-nuxi-truncateknown)

### sstd sd truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-sd-truncateknown)

### sstd sd truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-sd-truncateknown)

### sstd sd truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-sd-truncateknown)

### sstd sd+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-sdxi-truncateknown)

### sstd sd+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-sdxi-truncateknown)

### sstd sd+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-sdxi-truncateknown)

### sstd xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-xi-truncateknown)

### sstd xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-xi-truncateknown)

### sstd xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#sstd-xi-truncateknown)

### st alpha+nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#st-alphanu-truncateknown)

### st alpha+nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#st-alphanu-truncateknown)

### st alpha+nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#st-alphanu-truncateknown)

### st alpha+nu+xi truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#st-alphanuxi-truncateknown)

### st alpha+nu+xi truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#st-alphanuxi-truncateknown)

### st alpha+nu+xi truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#st-alphanuxi-truncateknown)

### lst all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-all-free-truncateknown)

### lst all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-all-free-truncateknown)

### lst all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-all-free-truncateknown)

### lst df truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-df-truncateknown)

### lst df truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-df-truncateknown)

### lst df truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-df-truncateknown)

### lst df+mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfmu-truncateknown)

### lst df+mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfmu-truncateknown)

### lst df+mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfmu-truncateknown)

### lst df+mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfmusigma-truncateknown)

### lst df+mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfmusigma-truncateknown)

### lst df+mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfmusigma-truncateknown)

### lst df+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** With df=3 and sigma=1/sqrt(3), take one copy each of -100 and 100 and 50 copies each of -4 and 4 on tight [-100,100]. The trial center 4 beats the centered model and the remote uniform; symmetry forces at least two finite maximizers.

[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfsigma-truncateknown)

### lst df+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** With df=3 and sigma=1/sqrt(3), take one copy each of -100 and 100 and 50 copies each of -4 and 4 on tight [-100,100]. The trial center 4 beats the centered model and the remote uniform; symmetry forces at least two finite maximizers.

[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfsigma-truncateknown)

### lst df+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** With df=3 and sigma=1/sqrt(3), take one copy each of -100 and 100 and 50 copies each of -4 and 4 on tight [-100,100]. The trial center 4 beats the centered model and the remote uniform; symmetry forces at least two finite maximizers.

[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-dfsigma-truncateknown)

### lst mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a retained tie at the held center diverges with free scale, and the joint df-down-to-zero bounds cover all other data. In the finite-supremum regime a finite feasible point must reach or exceed the greatest eligible boundary height.

**Proved · Failure.** On compatible positive width, with a retained center tie the likelihood diverges through scale collapse at small fixed df, and joint df and scale shrinkage on an effective ray can also diverge while mass escapes. In finite-supremum cases use the hold-compatible fixed-df, normal and small-df boundary comparisons.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-mu-truncateknown)

### lst mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a retained tie at the held center diverges with free scale, and the joint df-down-to-zero bounds cover all other data. In the finite-supremum regime a finite feasible point must reach or exceed the greatest eligible boundary height.

**Proved · Failure.** On compatible positive width, with a retained center tie the likelihood diverges through scale collapse at small fixed df, and joint df and scale shrinkage on an effective ray can also diverge while mass escapes. In finite-supremum cases use the hold-compatible fixed-df, normal and small-df boundary comparisons.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-mu-truncateknown)

### lst mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a retained tie at the held center diverges with free scale, and the joint df-down-to-zero bounds cover all other data. In the finite-supremum regime a finite feasible point must reach or exceed the greatest eligible boundary height.

**Proved · Failure.** On compatible positive width, with a retained center tie the likelihood diverges through scale collapse at small fixed df, and joint df and scale shrinkage on an effective ray can also diverge while mass escapes. In finite-supremum cases use the hold-compatible fixed-df, normal and small-df boundary comparisons.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-mu-truncateknown)

### lst sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-sigma-truncateknown)

### lst sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-sigma-truncateknown)

### lst sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lst-sigma-truncateknown)

### slash all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-all-free-truncateknown)

### slash all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-all-free-truncateknown)

### slash all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-all-free-truncateknown)

### slash mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-mu-truncateknown)

### slash mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-mu-truncateknown)

### slash mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-mu-truncateknown)

### slash sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** With sigma=1, take one copy each of -100 and 100 and 50 copies each of -4 and 4 on tight [-100,100]. The trial center 4 beats the centered model and the remote uniform; symmetry forces at least two finite maximizers.

[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-sigma-truncateknown)

### slash sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** With sigma=1, take one copy each of -100 and 100 and 50 copies each of -4 and 4 on tight [-100,100]. The trial center 4 beats the centered model and the remote uniform; symmetry forces at least two finite maximizers.

[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-sigma-truncateknown)

### slash sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** With sigma=1, take one copy each of -100 and 100 and 50 copies each of -4 and 4 on tight [-100,100]. The trial center 4 beats the centered model and the remote uniform; symmetry forces at least two finite maximizers.

[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#slash-sigma-truncateknown)

### huber all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-all-free-truncateknown)

### huber all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-all-free-truncateknown)

### huber all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-all-free-truncateknown)

### huber epsilon truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilon-truncateknown)

### huber epsilon truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilon-truncateknown)

### huber epsilon truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilon-truncateknown)

### huber epsilon+mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilonmu-truncateknown)

### huber epsilon+mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilonmu-truncateknown)

### huber epsilon+mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilonmu-truncateknown)

### huber epsilon+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilonsigma-truncateknown)

### huber epsilon+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilonsigma-truncateknown)

### huber epsilon+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-epsilonsigma-truncateknown)

### huber mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-mu-truncateknown)

### huber mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-mu-truncateknown)

### huber mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-mu-truncateknown)

### huber mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-musigma-truncateknown)

### huber mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-musigma-truncateknown)

### huber mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-musigma-truncateknown)

### huber sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-sigma-truncateknown)

### huber sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-sigma-truncateknown)

### huber sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#huber-sigma-truncateknown)

### ht all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-all-free-truncateknown)

### ht all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-all-free-truncateknown)

### ht all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-all-free-truncateknown)

### ht mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, with a retained center tie the likelihood diverges through scale collapse at small fixed df, and joint df and scale shrinkage on an effective ray can also diverge while mass escapes. In finite-supremum cases use the hold-compatible fixed-df, normal and small-df boundary comparisons.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-mu-truncateknown)

### ht mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, with a retained center tie the likelihood diverges through scale collapse at small fixed df, and joint df and scale shrinkage on an effective ray can also diverge while mass escapes. In finite-supremum cases use the hold-compatible fixed-df, normal and small-df boundary comparisons.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-mu-truncateknown)

### ht mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, with a retained center tie the likelihood diverges through scale collapse at small fixed df, and joint df and scale shrinkage on an effective ray can also diverge while mass escapes. In finite-supremum cases use the hold-compatible fixed-df, normal and small-df boundary comparisons.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-mu-truncateknown)

### ht mu+nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the effective interval [max(a,mu),b] and the two scale ends; a fit exists iff the boundary height is finite and some feasible point reaches or exceeds it.

**Proved · Failure.** On compatible positive width, the broad uniform is eligible whenever the effective interval is bounded, including physical left rays. The sharp end is the exterior power or the joint endpoint collapse governed by the fixed-df tie exponent.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-munu-truncateknown)

### ht mu+nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the effective interval [max(a,mu),b] and the two scale ends; a fit exists iff the boundary height is finite and some feasible point reaches or exceeds it.

**Proved · Failure.** On compatible positive width, the broad uniform is eligible whenever the effective interval is bounded, including physical left rays. The sharp end is the exterior power or the joint endpoint collapse governed by the fixed-df tie exponent.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-munu-truncateknown)

### ht mu+nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the effective interval [max(a,mu),b] and the two scale ends; a fit exists iff the boundary height is finite and some feasible point reaches or exceeds it.

**Proved · Failure.** On compatible positive width, the broad uniform is eligible whenever the effective interval is bounded, including physical left rays. The sharp end is the exterior power or the joint endpoint collapse governed by the fixed-df tie exponent.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-munu-truncateknown)

### ht mu+nu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-munusigma-truncateknown)

### ht mu+nu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-munusigma-truncateknown)

### ht mu+nu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-munusigma-truncateknown)

### ht mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, with center and scale held the large-df face is the retained normal or half-normal. At df down to zero a bounded effective window gives the inverse-distance face for a strictly exterior center, divergence for a retained center tie, and zero product for a retained center without ties. The small-df end has zero height on unbounded effective windows.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-musigma-truncateknown)

### ht mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, with center and scale held the large-df face is the retained normal or half-normal. At df down to zero a bounded effective window gives the inverse-distance face for a strictly exterior center, divergence for a retained center tie, and zero product for a retained center without ties. The small-df end has zero height on unbounded effective windows.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-musigma-truncateknown)

### ht mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, with center and scale held the large-df face is the retained normal or half-normal. At df down to zero a bounded effective window gives the inverse-distance face for a strictly exterior center, divergence for a retained center tie, and zero product for a retained center without ties. The small-df end has zero height on unbounded effective windows.

Use the effective lower cut $\max(a_*,\mu)$. In particular an upper-estimated physical left ray can have bounded effective width and must use the bounded-window tests. Where df can tend to zero, apply the reviewed effective-ray normalizer.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-musigma-truncateknown)

### ht nu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-nu-truncateknown)

### ht nu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-nu-truncateknown)

### ht nu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-nu-truncateknown)

### ht nu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

For fixed shape and scale, a free endpoint is profiled over $c\le m$ if the lower cut is $m$. If the fixed lower cut is $-\infty$ and $m<h$, the maximizing threshold is $c=m$. For any fitted $c=m$ an estimated lower cut is flat outwards.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-nusigma-truncateknown)

### ht nu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

**Proved case.** On a left ray with sample minimum below the upper cut, the unique endpoint is the sample minimum.

For fixed shape and scale, a free endpoint is profiled over $c\le m$ if the lower cut is $m$. If the fixed lower cut is $-\infty$ and $m<h$, the maximizing threshold is $c=m$. For any fitted $c=m$ an estimated lower cut is flat outwards.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-nusigma-truncateknown)

### ht nu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT.

For fixed shape and scale, a free endpoint is profiled over $c\le m$ if the lower cut is $m$. If the fixed lower cut is $-\infty$ and $m<h$, the maximizing threshold is $c=m$. For any fitted $c=m$ an estimated lower cut is flat outwards.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-nusigma-truncateknown)

### ht sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-sigma-truncateknown)

### ht sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-sigma-truncateknown)

### ht sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#ht-sigma-truncateknown)

### tlambda lambda truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambda-truncateknown)

### tlambda lambda truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambda-truncateknown)

### tlambda lambda truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambda-truncateknown)

### tlambda lambda+mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdamu-truncateknown)

### tlambda lambda+mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdamu-truncateknown)

### tlambda lambda+mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdamu-truncateknown)

### tlambda lambda+mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdamusigma-truncateknown)

### tlambda lambda+mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdamusigma-truncateknown)

### tlambda lambda+mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdamusigma-truncateknown)

### tlambda lambda+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdasigma-truncateknown)

### tlambda lambda+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdasigma-truncateknown)

### tlambda lambda+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-lambdasigma-truncateknown)

### tlambda mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** A tie at the held center diverges at sufficiently negative fixed shape as scale shrinks.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled support-edge motion near shapes zero and one and the allowed large negative shape end. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-mu-truncateknown)

### tlambda mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** A tie at the held center diverges at sufficiently negative fixed shape as scale shrinks.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled support-edge motion near shapes zero and one and the allowed large negative shape end. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-mu-truncateknown)

### tlambda mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** A tie at the held center diverges at sufficiently negative fixed shape as scale shrinks.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled support-edge motion near shapes zero and one and the allowed large negative shape end. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-mu-truncateknown)

### tlambda sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** A constant at a finite cut diverges by sliding the included shape-1 support against that cut.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled support-edge motion near shapes zero and one and the allowed large negative shape end. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-sigma-truncateknown)

### tlambda sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** A constant at a finite cut diverges by sliding the included shape-1 support against that cut.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled support-edge motion near shapes zero and one and the allowed large negative shape end. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-sigma-truncateknown)

### tlambda sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** A constant at a finite cut diverges by sliding the included shape-1 support against that cut.

At positive shape check the fitted endpoint density before evaluating the overlap. At an admissible shape-one uniform use the shortest overlap rule, with the nominal mass plateaus restored.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled support-edge motion near shapes zero and one and the allowed large negative shape end. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#tlambda-sigma-truncateknown)

### bhatt a truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-a-truncateknown)

### bhatt a truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-a-truncateknown)

### bhatt a truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-a-truncateknown)

### bhatt a+mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, at held width zero use the fixed-center retained normal moment test; at positive width compare the two stated scale ends and accept equality with a finite feasible candidate.

**Proved · Failure.** On compatible positive width, at width zero the retained normal has broad uniform or sharp projection collapse at the sample projection. At positive width the sharp limit is the specified retained uniform with its edge factors or cut collapse, and the broad positive limit is uniform.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-amu-truncateknown)

### bhatt a+mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, at held width zero use the fixed-center retained normal moment test; at positive width compare the two stated scale ends and accept equality with a finite feasible candidate.

**Proved · Failure.** On compatible positive width, at width zero the retained normal has broad uniform or sharp projection collapse at the sample projection. At positive width the sharp limit is the specified retained uniform with its edge factors or cut collapse, and the broad positive limit is uniform.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-amu-truncateknown)

### bhatt a+mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, at held width zero use the fixed-center retained normal moment test; at positive width compare the two stated scale ends and accept equality with a finite feasible candidate.

**Proved · Failure.** On compatible positive width, at width zero the retained normal has broad uniform or sharp projection collapse at the sample projection. At positive width the sharp limit is the specified retained uniform with its edge factors or cut collapse, and the broad positive limit is uniform.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-amu-truncateknown)

### bhatt a+mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-amusigma-truncateknown)

### bhatt a+mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-amusigma-truncateknown)

### bhatt a+mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-amusigma-truncateknown)

### bhatt a+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-asigma-truncateknown)

### bhatt a+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-asigma-truncateknown)

### bhatt a+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-asigma-truncateknown)

### bhatt all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-all-free-truncateknown)

### bhatt all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-all-free-truncateknown)

### bhatt all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-all-free-truncateknown)

### bhatt mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-mu-truncateknown)

### bhatt mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-mu-truncateknown)

### bhatt mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-mu-truncateknown)

### bhatt mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the width endpoints (zero is included). A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-musigma-truncateknown)

### bhatt mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the width endpoints (zero is included). A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-musigma-truncateknown)

### bhatt mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the width endpoints (zero is included). A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#bhatt-musigma-truncateknown)

### emg all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-all-free-truncateknown)

### emg all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-all-free-truncateknown)

### emg all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-all-free-truncateknown)

### emg lambda truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambda-truncateknown)

### emg lambda truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambda-truncateknown)

### emg lambda truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambda-truncateknown)

### emg lambda+mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad-scale uniform or diffusion and the appropriate sharp-scale face or cut collapse. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the broad scale gives a bounded uniform, and the sharp scale gives a retained translated exponential with its half join factor or the upper-cut collapse described in the scalar analysis.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdamu-truncateknown)

### emg lambda+mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad-scale uniform or diffusion and the appropriate sharp-scale face or cut collapse. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the broad scale gives a bounded uniform, and the sharp scale gives a retained translated exponential with its half join factor or the upper-cut collapse described in the scalar analysis.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdamu-truncateknown)

### emg lambda+mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the broad-scale uniform or diffusion and the appropriate sharp-scale face or cut collapse. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, the broad scale gives a bounded uniform, and the sharp scale gives a retained translated exponential with its half join factor or the upper-cut collapse described in the scalar analysis.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdamu-truncateknown)

### emg lambda+mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdamusigma-truncateknown)

### emg lambda+mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdamusigma-truncateknown)

### emg lambda+mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdamusigma-truncateknown)

### emg lambda+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, a remote location to the left gives the integrable decreasing exponential where eligible, and a remote location to the right gives divergent collapse only for a constant at the finite upper cut.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdasigma-truncateknown)

### emg lambda+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, a remote location to the left gives the integrable decreasing exponential where eligible, and a remote location to the right gives divergent collapse only for a constant at the finite upper cut.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdasigma-truncateknown)

### emg lambda+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, a remote location to the left gives the integrable decreasing exponential where eligible, and a remote location to the right gives divergent collapse only for a constant at the finite upper cut.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-lambdasigma-truncateknown)

### emg mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-mu-truncateknown)

### emg mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-mu-truncateknown)

### emg mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved case.** For distinct endpoint data (m,h) on the tight window [m,h], the excluded physical uniform gives the strict supremum.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

**Open.** Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition. The stated cases leave the general attainment and failure classification open.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-mu-truncateknown)

### emg mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the normal at infinite rate and the zero-rate Gaussian CDF kernel where integrable. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, at infinite rate the excluded limit is the retained normal, and at rate zero it is the Gaussian CDF kernel where integrable.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-musigma-truncateknown)

### emg mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the normal at infinite rate and the zero-rate Gaussian CDF kernel where integrable. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, at infinite rate the excluded limit is the retained normal, and at rate zero it is the Gaussian CDF kernel where integrable.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-musigma-truncateknown)

### emg mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the normal at infinite rate and the zero-rate Gaussian CDF kernel where integrable. A finite fit exists iff a finite feasible point reaches the finite excluded threshold.

**Proved · Failure.** On compatible positive width, at infinite rate the excluded limit is the retained normal, and at rate zero it is the Gaussian CDF kernel where integrable.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#emg-musigma-truncateknown)

### logitnorm all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, for nonconstant logits compare their variance with that of the mean-matched linear tilt.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** The sample (1/4,3/4) on [1/4,3/4] escapes to a law uniform on the logit scale.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-all-free-truncateknown)

### logitnorm all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, for nonconstant logits compare their variance with that of the mean-matched linear tilt.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** The sample (1/4,3/4) on [1/4,3/4] escapes to a law uniform on the logit scale.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-all-free-truncateknown)

### logitnorm all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, for nonconstant logits compare their variance with that of the mean-matched linear tilt.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** The sample (1/4,3/4) on [1/4,3/4] escapes to a law uniform on the logit scale.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-all-free-truncateknown)

### logitnorm mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the projected minimum and the uniform mean of the squared logit residual.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-mu-truncateknown)

### logitnorm mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the projected minimum and the uniform mean of the squared logit residual.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-mu-truncateknown)

### logitnorm mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the projected minimum and the uniform mean of the squared logit residual.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-mu-truncateknown)

### logitnorm mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-musigma-truncateknown)

### logitnorm mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-musigma-truncateknown)

### logitnorm mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-musigma-truncateknown)

### logitnorm sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the average logit must lie strictly between the transformed cuts.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-sigma-truncateknown)

### logitnorm sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the average logit must lie strictly between the transformed cuts.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-sigma-truncateknown)

### logitnorm sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the average logit must lie strictly between the transformed cuts.

**Proved · Failure.** On compatible positive width, the applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The logit cuts use $-\infty$ at zero and $+\infty$ at one. Both are finite when both cuts are estimated.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#logitnorm-sigma-truncateknown)

### dblpareto all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-all-free-truncateknown)

### dblpareto all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-all-free-truncateknown)

### dblpareto all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-all-free-truncateknown)

### dblpareto left_rate truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rate-truncateknown)

### dblpareto left_rate truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rate-truncateknown)

### dblpareto left_rate truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rate-truncateknown)

### dblpareto left_rate+right_rate truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** On proper windows restore the winning exterior plateaus to the winning cut or knot solutions. On the whole line include every winning sample gap on which the fixed-rate log likelihood is flat. The estimated cuts are uniquely tight at attained fits.

**Proved case.** Not unique in general: sample (1,1) on [1,e], left_rate=right_rate=1, with every scale in (0,1] maximizing.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rateright_rate-truncateknown)

### dblpareto left_rate+right_rate truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** On proper windows restore the winning exterior plateaus to the winning cut or knot solutions. On the whole line include every winning sample gap on which the fixed-rate log likelihood is flat. The estimated cuts are uniquely tight at attained fits.

**Proved case.** With both rates one, data (1,1) and fixed lower cut 1/e, every kink scale at least one maximizes. Reflect the log window and the slope argument for the lower-estimated case.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rateright_rate-truncateknown)

### dblpareto left_rate+right_rate truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** On proper windows restore the winning exterior plateaus to the winning cut or knot solutions. On the whole line include every winning sample gap on which the fixed-rate log likelihood is flat. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rateright_rate-truncateknown)

### dblpareto left_rate+right_rate+scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rateright_ratescale-truncateknown)

### dblpareto left_rate+right_rate+scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rateright_ratescale-truncateknown)

### dblpareto left_rate+right_rate+scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_rateright_ratescale-truncateknown)

### dblpareto left_rate+scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_ratescale-truncateknown)

### dblpareto left_rate+scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_ratescale-truncateknown)

### dblpareto left_rate+scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-left_ratescale-truncateknown)

### dblpareto right_rate truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-right_rate-truncateknown)

### dblpareto right_rate truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-right_rate-truncateknown)

### dblpareto right_rate truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-right_rate-truncateknown)

### dblpareto right_rate+scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-right_ratescale-truncateknown)

### dblpareto right_rate+scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-right_ratescale-truncateknown)

### dblpareto right_rate+scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-right_ratescale-truncateknown)

### dblpareto scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-scale-truncateknown)

### dblpareto scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-scale-truncateknown)

### dblpareto scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum.

**Proved · Failure.** On compatible positive width, if no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure.

**Proved · Cardinality.** The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. The estimated cuts are uniquely tight at attained fits.

Use the log images of the tight cuts. In moving-kink comparisons the estimated cuts are sample knots, so evaluate each of those only once. A winning finite cut still represents its entire exterior kink plateau.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#dblpareto-scale-truncateknown)

### lgamma all free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, exclude constants and test the transformed gamma shape edge, rate edge and corner by the inward moments.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** The sample (e,e^4) on [e,e^4] has an unattained log-gamma corner supremum.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-all-free-truncateknown)

### lgamma all free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, exclude constants and test the transformed gamma shape edge, rate edge and corner by the inward moments.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** The sample (e,e^4) on [e,e^4] has an unattained log-gamma corner supremum.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-all-free-truncateknown)

### lgamma all free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, exclude constants and test the transformed gamma shape edge, rate edge and corner by the inward moments.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

**Proved case.** The sample (e,e^4) on [e,e^4] has an unattained log-gamma corner supremum.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-all-free-truncateknown)

### lgamma ratelog truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, match the mean of log log x between the zero-shape and upper thresholds.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-ratelog-truncateknown)

### lgamma ratelog truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, match the mean of log log x between the zero-shape and upper thresholds.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-ratelog-truncateknown)

### lgamma ratelog truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, match the mean of log log x between the zero-shape and upper thresholds.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-ratelog-truncateknown)

### lgamma ratelog+shapelog truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-ratelogshapelog-truncateknown)

### lgamma ratelog+shapelog truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-ratelogshapelog-truncateknown)

### lgamma ratelog+shapelog truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, a compatible held model with positive likelihood is the sole feasible model.

**Proved · Failure.** On compatible positive width, a compatible fixed model has no parameter escape.

**Proved · Cardinality.** The fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-ratelogshapelog-truncateknown)

### lgamma shapelog truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, match the mean of log x between the lower cut and the zero-rate mean.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-shapelog-truncateknown)

### lgamma shapelog truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, match the mean of log x between the lower cut and the zero-rate mean.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-shapelog-truncateknown)

### lgamma shapelog truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, match the mean of log x between the lower cut and the zero-rate mean.

**Proved · Failure.** On compatible positive width, the applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition.

**Proved · Cardinality.** Every attained fit is unique. The estimated cuts are uniquely tight at attained fits.

The transformed lower cut is $\log(\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.

[Known-window proof](2026-11-27-trunc-skew-math-report.md#lgamma-shapelog-truncateknown)

### weibull free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed power tilts, including log-uniform where integrable. Constants diverge to an atom.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-free-truncateknown)

### weibull free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed power tilts, including log-uniform where integrable. Constants diverge to an atom.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-free-truncateknown)

### weibull free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed power tilts, including log-uniform where integrable. Constants diverge to an atom.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-free-truncateknown)

### weibull scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform where integrable. Constant data at the projected scale diverge to an atom.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-scale-truncateknown)

### weibull scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform where integrable. Constant data at the projected scale diverge to an atom.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-scale-truncateknown)

### weibull scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform where integrable. Constant data at the projected scale diverge to an atom.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-scale-truncateknown)

### weibull scale+shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-scaleshape-truncateknown)

### weibull scale+shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-scaleshape-truncateknown)

### weibull scale+shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#weibull-scaleshape-truncateknown)

### weibull shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the exponential criterion to X^shape.

**Proved · Failure.** On compatible positive width, lower collapse or the finite-window power-law face.

**Proved · Cardinality.** Unique finite fit. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-expfam-math-report.md#weibull-shape-truncateknown)

### weibull shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the exponential criterion to X^shape.

**Proved · Failure.** On compatible positive width, lower collapse or the finite-window power-law face.

**Proved · Cardinality.** Unique finite fit. The estimated cuts are uniquely tight at attained fits.

On $a\le0$ the transformed lower cut is zero, so lower equality is impossible for admissible data.

[Known-window proof](2026-11-27-trunc-expfam-math-report.md#weibull-shape-truncateknown)

### weibull shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, apply the exponential criterion to X^shape.

**Proved · Failure.** On compatible positive width, lower collapse or the finite-window power-law face.

**Proved · Cardinality.** Unique finite fit. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-expfam-math-report.md#weibull-shape-truncateknown)

### lomax free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among truncated exponential, power and translated reciprocal limits. All-lower-cut data diverge to a lower atom.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-free-truncateknown)

### lomax free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among truncated exponential, power and translated reciprocal limits. All-lower-cut data diverge to a lower atom.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-free-truncateknown)

### lomax free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among truncated exponential, power and translated reciprocal limits. All-lower-cut data diverge to a lower atom.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-free-truncateknown)

### lomax kappa truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the integrable truncated power or uniform.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-kappa-truncateknown)

### lomax kappa truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the integrable truncated power or uniform.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-kappa-truncateknown)

### lomax kappa truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the integrable truncated power or uniform.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-kappa-truncateknown)

### lomax kappa+lambda truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-kappalambda-truncateknown)

### lomax kappa+lambda truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-kappalambda-truncateknown)

### lomax kappa+lambda truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-kappalambda-truncateknown)

### lomax lambda truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of log(1+y/s) is strictly between zero and the zero-shape mean.

**Proved · Failure.** On compatible positive width, lower collapse or the excluded translated reciprocal law.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-lambda-truncateknown)

### lomax lambda truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of log(1+y/s) is strictly between zero and the zero-shape mean.

**Proved · Failure.** On compatible positive width, lower collapse or the excluded translated reciprocal law.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-lambda-truncateknown)

### lomax lambda truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of log(1+y/s) is strictly between zero and the zero-shape mean.

**Proved · Failure.** On compatible positive width, lower collapse or the excluded translated reciprocal law.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#lomax-lambda-truncateknown)

### llogis free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed power tilts, including log-uniform where integrable. Constants diverge to an atom.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-free-truncateknown)

### llogis free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed power tilts, including log-uniform where integrable. Constants diverge to an atom.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-free-truncateknown)

### llogis free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed power tilts, including log-uniform where integrable. Constants diverge to an atom.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-free-truncateknown)

### llogis scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform where integrable. Constant data at the projected scale diverge to an atom.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-scale-truncateknown)

### llogis scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform where integrable. Constant data at the projected scale diverge to an atom.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-scale-truncateknown)

### llogis scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform where integrable. Constant data at the projected scale diverge to an atom.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-scale-truncateknown)

### llogis scale+shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-scaleshape-truncateknown)

### llogis scale+shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-scaleshape-truncateknown)

### llogis scale+shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-scaleshape-truncateknown)

### llogis shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the integrable powers of exponents shape-1 and -shape-1.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-shape-truncateknown)

### llogis shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the integrable powers of exponents shape-1 and -shape-1.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-shape-truncateknown)

### llogis shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the integrable powers of exponents shape-1 and -shape-1.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#llogis-shape-truncateknown)

### gompertz free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among signed exponential tilts, including uniform. Constants diverge to an atom.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-free-truncateknown)

### gompertz free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among signed exponential tilts, including uniform. Constants diverge to an atom.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-free-truncateknown)

### gompertz free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among signed exponential tilts, including uniform. Constants diverge to an atom.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-free-truncateknown)

### gompertz a truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the rate-a truncated exponential. Positive lower-cut constants diverge. All-zero data select the rate-a face on a finite window and attain throughout the domain on the full positive half-line.

**Proved · Cardinality.** At sample (0), fixed upper cut infinity and held a>0, every b>0 and every estimated lower cut at or below zero maximize. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

At all-zero data, finite positive retained width selects the rate-$a$ face. The unbounded upper window gives the entire $b>0$ plateau. An upper-estimated cut at zero cannot retain positive mass with a fixed lower cut below zero.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-a-truncateknown)

### gompertz a truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the rate-a truncated exponential. Positive lower-cut constants diverge. All-zero data select the rate-a face on a finite window and attain throughout the domain on the full positive half-line.

At all-zero data, finite positive retained width selects the rate-$a$ face. The unbounded upper window gives the entire $b>0$ plateau. An upper-estimated cut at zero cannot retain positive mass with a fixed lower cut below zero.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-a-truncateknown)

### gompertz a truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the rate-a truncated exponential. Positive lower-cut constants diverge. All-zero data select the rate-a face on a finite window and attain throughout the domain on the full positive half-line.

At all-zero data, finite positive retained width selects the rate-$a$ face. The unbounded upper window gives the entire $b>0$ plateau. An upper-estimated cut at zero cannot retain positive mass with a fixed lower cut below zero.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-a-truncateknown)

### gompertz a+b truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-ab-truncateknown)

### gompertz a+b truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-ab-truncateknown)

### gompertz a+b truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-ab-truncateknown)

### gompertz b truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of exp(b X) lies strictly between the lower endpoint and the zero-rate mean on the transformed window.

**Proved · Failure.** On compatible positive width, lower collapse or an excluded positive exponential tilt of slope b.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-b-truncateknown)

### gompertz b truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of exp(b X) lies strictly between the lower endpoint and the zero-rate mean on the transformed window.

**Proved · Failure.** On compatible positive width, lower collapse or an excluded positive exponential tilt of slope b.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-b-truncateknown)

### gompertz b truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of exp(b X) lies strictly between the lower endpoint and the zero-rate mean on the transformed window.

**Proved · Failure.** On compatible positive width, lower collapse or an excluded positive exponential tilt of slope b.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#gompertz-b-truncateknown)

### frechet free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, attainment requires strictly fewer than half the observations at m.

**Proved · Failure.** On compatible positive width, a minimum majority gives divergent lower contact; exact half gives the strict atom envelope equal to the product of inverse squared gaps.

**Proved case.** At exact half at the lower cut the strict supremum is the product of inverse squared positive gaps.

**Proved case.** At exact half and shape one a contact path yields the explicit finite positive atom likelihood.

**Proved case.** Increasing shape with fixed limiting center and scaled width yields the same-window Gumbel maximum law.

For the Gumbel witness take $\lambda\to\infty$, $\mu+\sigma\to z$ and $\sigma/\lambda\to t>0$. The CDF and sample densities converge to those of $G(x)=\exp[-e^{-(x-z)/t}]$ before dividing by the tight retained mass.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-free-truncateknown)

### frechet free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, attainment requires at most half the data at the minimum, strictly less than half when the fixed lower cut a=m. For a=-infinity, equal frequencies at exactly two distinct values attain on the full shape-one fiber.

**Proved · Failure.** On compatible positive width, a strict minimum majority gives an unbounded likelihood with a point-mass limit.

**Proved case.** At exact half at the lower cut the strict supremum is the product of inverse squared positive gaps. Here this requires fixed a=m.

**Proved case.** At exact half and shape one a contact path yields the explicit finite positive atom likelihood.

**Proved case.** Increasing shape with fixed limiting center and scaled width yields the same-window Gumbel maximum law.

**Proved case.** For a=-infinity and two distinct values with equal counts, shape one with any location below m and its profiled scale is the entire maximizing set. The upper bound h is unique.

For the Gumbel witness take $\lambda\to\infty$, $\mu+\sigma\to z$ and $\sigma/\lambda\to t>0$. The CDF and sample densities converge to those of $G(x)=\exp[-e^{-(x-z)/t}]$ before dividing by the tight retained mass.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-free-truncateknown)

### frechet free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, attainment requires strictly fewer than half the observations at m.

**Proved · Failure.** On compatible positive width, a minimum majority gives divergent lower contact; exact half gives the strict atom envelope equal to the product of inverse squared gaps.

**Proved case.** At exact half at the lower cut the strict supremum is the product of inverse squared positive gaps.

**Proved case.** At exact half and shape one a contact path yields the explicit finite positive atom likelihood.

**Proved case.** Increasing shape with fixed limiting center and scaled width yields the same-window Gumbel maximum law.

For the Gumbel witness take $\lambda\to\infty$, $\mu+\sigma\to z$ and $\sigma/\lambda\to t>0$. The CDF and sample densities converge to those of $G(x)=\exp[-e^{-(x-z)/t}]$ before dividing by the tight retained mass.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-free-truncateknown)

### frechet lambda truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the divergence branches, attainment is equivalent to a positive finite candidate reaching the best eligible escape height.

**Proved · Failure.** On compatible positive width, for finite lower cut compare shifted power, integrable nonnegative exponential tilt and critical contact. On a physical upper ray compare the reflected exponential and the contact height. Negative contact exponent and the applicable constant branches diverge.

Let $s=\lambda$, $j$ count minima, $E=s(n-j)-j$, $d=m-\mu$, $q=(\sigma/d)^s$. Negative $E$ gives divergence. At $E=0$ optimize $q^ne^{-jq}/D(q)^n$ times $s^n\prod_{x_i>m}(x_i-m)^{-1-s}$, where $D(q)=1-e^{-q}$ at lower contact and $D(q)=1$ below it. On a physical upper ray use the exact scale profile in [Fréchet upper ray](#fréchet-upper-ray).

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambda-truncateknown)

### frechet lambda truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the divergence branches, attainment is equivalent to a positive finite candidate reaching the best eligible escape height.

**Proved · Failure.** On compatible positive width, for finite lower cut compare shifted power, integrable nonnegative exponential tilt and critical contact. On a physical upper ray compare the reflected exponential and the contact height. Negative contact exponent and the applicable constant branches diverge.

**Proved case.** With a=-infinity and two distinct values of equal frequency, shape above one selects the reflected exponential, shape one attains on the entire profiled fiber, and shape below one diverges.

Let $s=\lambda$, $j$ count minima, $E=s(n-j)-j$, $d=m-\mu$, $q=(\sigma/d)^s$. Negative $E$ gives divergence. At $E=0$ optimize $q^ne^{-jq}/D(q)^n$ times $s^n\prod_{x_i>m}(x_i-m)^{-1-s}$, where $D(q)=1-e^{-q}$ at lower contact and $D(q)=1$ below it. On a physical upper ray use the exact scale profile in [Fréchet upper ray](#fréchet-upper-ray).

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambda-truncateknown)

### frechet lambda truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the divergence branches, attainment is equivalent to a positive finite candidate reaching the best eligible escape height.

**Proved · Failure.** On compatible positive width, for finite lower cut compare shifted power, integrable nonnegative exponential tilt and critical contact. On a physical upper ray compare the reflected exponential and the contact height. Negative contact exponent and the applicable constant branches diverge.

Let $s=\lambda$, $j$ count minima, $E=s(n-j)-j$, $d=m-\mu$, $q=(\sigma/d)^s$. Negative $E$ gives divergence. At $E=0$ optimize $q^ne^{-jq}/D(q)^n$ times $s^n\prod_{x_i>m}(x_i-m)^{-1-s}$, where $D(q)=1-e^{-q}$ at lower contact and $D(q)=1$ below it. On a physical upper ray use the exact scale profile in [Fréchet upper ray](#fréchet-upper-ray).

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambda-truncateknown)

### frechet lambda+mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the inverse-power excess mean is strictly between its lower transformed cut and zero-rate mean.

**Proved · Failure.** On compatible positive width, an excluded inverse-power carrier or upper x collapse.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdamu-truncateknown)

### frechet lambda+mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the inverse-power excess mean is strictly between its lower transformed cut and zero-rate mean.

**Proved · Failure.** On compatible positive width, an excluded inverse-power carrier or upper x collapse.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdamu-truncateknown)

### frechet lambda+mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the inverse-power excess mean is strictly between its lower transformed cut and zero-rate mean.

**Proved · Failure.** On compatible positive width, an excluded inverse-power carrier or upper x collapse.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdamu-truncateknown)

### frechet lambda+mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdamusigma-truncateknown)

### frechet lambda+mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdamusigma-truncateknown)

### frechet lambda+mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdamusigma-truncateknown)

### frechet lambda+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform. Upper width collapse gives unbounded likelihood.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdasigma-truncateknown)

### frechet lambda+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform. Upper width collapse gives unbounded likelihood.

On the physical upper-only ray, distinct data attain because all excluded profile heights vanish. Constants at the upper end diverge. [Upper-ray proof](#fréchet-rays-with-scale-held)

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdasigma-truncateknown)

### frechet lambda+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform. Upper width collapse gives unbounded likelihood.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-lambdasigma-truncateknown)

### frechet mu truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed powers in the positive excess. Constants diverge to an atom.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-mu-truncateknown)

### frechet mu truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed powers in the positive excess. Constants diverge to an atom.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-mu-truncateknown)

### frechet mu truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the signed powers in the positive excess. Constants diverge to an atom.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-mu-truncateknown)

### frechet mu+sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform in the excess where integrable. Constants at the projected excess scale diverge to an atom.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-musigma-truncateknown)

### frechet mu+sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform in the excess where integrable. Constants at the projected excess scale diverge to an atom.

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-musigma-truncateknown)

### frechet mu+sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among log-uniform in the excess where integrable. Constants at the projected excess scale diverge to an atom.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-musigma-truncateknown)

### frechet sigma truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among a nonincreasing exponential tilt on the physical window. Constants diverge to an atom.

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-sigma-truncateknown)

### frechet sigma truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among a nonincreasing exponential tilt on the physical window. Constants diverge to an atom.

On the physical upper-only ray, distinct data attain because all excluded profile heights vanish. Constants at the upper end diverge. [Upper-ray proof](#fréchet-rays-with-scale-held)

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-sigma-truncateknown)

### frechet sigma truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among a nonincreasing exponential tilt on the physical window. Constants diverge to an atom.

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#frechet-sigma-truncateknown)

### invexp free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the reciprocal mean lies strictly between the lower reciprocal cut and the zero-rate mean.

**Proved · Failure.** On compatible positive width, upper collapse or the excluded power x^-2.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#invexp-free-truncateknown)

### invexp free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the reciprocal mean lies strictly between the lower reciprocal cut and the zero-rate mean.

**Proved · Failure.** On compatible positive width, upper collapse or the excluded power x^-2.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#invexp-free-truncateknown)

### invexp free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the reciprocal mean lies strictly between the lower reciprocal cut and the zero-rate mean.

**Proved · Failure.** On compatible positive width, upper collapse or the excluded power x^-2.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#invexp-free-truncateknown)

### invexp scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#invexp-scale-truncateknown)

### invexp scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#invexp-scale-truncateknown)

### invexp scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. The estimated cuts are uniquely tight at attained fits.

The decreasing coordinate sends the estimated upper cut to the lower transformed endpoint, and the estimated lower cut to the upper endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#invexp-scale-truncateknown)

### power free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the sample maximum exceeds the lower cut and the log mean is strictly between the zero-shape mean and log of the sample maximum.

**Proved · Failure.** On compatible positive width, shrinking window or infinite-shape collapse, or excluded log-uniform.

**Proved · Cardinality.** Shape, when free, is unique. With free alpha a lower-estimated fit uses alpha=h if b>h and any alpha>=h if b=h.

Use $L=\max(0,a_*)$ and $u=h$ after profiling the free endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-free-truncateknown)

### power free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the sample maximum exceeds the lower cut and the log mean is strictly between the zero-shape mean and log of the sample maximum.

**Proved · Failure.** On compatible positive width, shrinking window or infinite-shape collapse, or excluded log-uniform.

**Proved · Cardinality.** Shape, when free, is unique. The whole endpoint locus has alpha>=h, upper cut>=h and min(alpha, upper cut)=h.

Use $L=\max(0,a_*)$ and $u=h$ after profiling the free endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-free-truncateknown)

### power free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the sample maximum exceeds the lower cut and the log mean is strictly between the zero-shape mean and log of the sample maximum.

**Proved · Failure.** On compatible positive width, shrinking window or infinite-shape collapse, or excluded log-uniform.

**Proved · Cardinality.** Shape, when free, is unique. The whole endpoint locus has alpha>=h, upper cut>=h and min(alpha, upper cut)=h.

Use $L=\max(0,a_*)$ and $u=h$ after profiling the free endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-free-truncateknown)

### power alpha truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, on the retained interval the log mean lies strictly between the zero-shape mean and the upper log endpoint.

**Proved · Failure.** On compatible positive width, excluded log-uniform or upper collapse.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

For held $\alpha$, first require $h\le\alpha$. At $h=\alpha$ the upper-estimated bound has its outward plateau. Use $L=\max(0,a_*)$ and $u=\min(b_*,\alpha)$.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-alpha-truncateknown)

### power alpha truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, on the retained interval the log mean lies strictly between the zero-shape mean and the upper log endpoint.

**Proved · Failure.** On compatible positive width, excluded log-uniform or upper collapse.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

For held $\alpha$, first require $h\le\alpha$. At $h=\alpha$ the upper-estimated bound has its outward plateau. Use $L=\max(0,a_*)$ and $u=\min(b_*,\alpha)$.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-alpha-truncateknown)

### power alpha truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, on the retained interval the log mean lies strictly between the zero-shape mean and the upper log endpoint.

**Proved · Failure.** On compatible positive width, excluded log-uniform or upper collapse.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

For held $\alpha$, first require $h\le\alpha$. At $h=\alpha$ the upper-estimated bound has its outward plateau. Use $L=\max(0,a_*)$ and $u=\min(b_*,\alpha)$.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-alpha-truncateknown)

### power alpha+beta truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

For held $\alpha$, first require $h\le\alpha$. At $h=\alpha$ the upper-estimated bound has its outward plateau. Use $L=\max(0,a_*)$ and $u=\min(b_*,\alpha)$.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-alphabeta-truncateknown)

### power alpha+beta truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

For held $\alpha$, first require $h\le\alpha$. At $h=\alpha$ the upper-estimated bound has its outward plateau. Use $L=\max(0,a_*)$ and $u=\min(b_*,\alpha)$.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-alphabeta-truncateknown)

### power alpha+beta truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

For held $\alpha$, first require $h\le\alpha$. At $h=\alpha$ the upper-estimated bound has its outward plateau. Use $L=\max(0,a_*)$ and $u=\min(b_*,\alpha)$.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-alphabeta-truncateknown)

### power beta truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the sample maximum strictly exceeds the lower cut.

**Proved · Failure.** On compatible positive width, an interval shrinking to the lower cut has unbounded likelihood.

**Proved · Cardinality.** Shape, when free, is unique. With free alpha a lower-estimated fit uses alpha=h if b>h and any alpha>=h if b=h.

Use $L=\max(0,a_*)$ and $u=h$ after profiling the free endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-beta-truncateknown)

### power beta truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the sample maximum strictly exceeds the lower cut.

**Proved · Failure.** On compatible positive width, an interval shrinking to the lower cut has unbounded likelihood.

**Proved · Cardinality.** Shape, when free, is unique. The whole endpoint locus has alpha>=h, upper cut>=h and min(alpha, upper cut)=h.

Use $L=\max(0,a_*)$ and $u=h$ after profiling the free endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-beta-truncateknown)

### power beta truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the sample maximum strictly exceeds the lower cut.

**Proved · Failure.** On compatible positive width, an interval shrinking to the lower cut has unbounded likelihood.

**Proved · Cardinality.** Shape, when free, is unique. The whole endpoint locus has alpha>=h, upper cut>=h and min(alpha, upper cut)=h.

Use $L=\max(0,a_*)$ and $u=h$ after profiling the free endpoint.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#power-beta-truncateknown)

### kumar free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the powers in negative log x or the reciprocal kernel 1/(exp(A y)-1). Constants diverge to an atom.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

The estimated lower cut is unique at $m$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-free-truncateknown)

### kumar free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the powers in negative log x or the reciprocal kernel 1/(exp(A y)-1). Constants diverge to an atom.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

The estimated upper cut is unique at $h$ whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-free-truncateknown)

### kumar free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the powers in negative log x or the reciprocal kernel 1/(exp(A y)-1). Constants diverge to an atom.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

Both estimated cuts are unique at the observed extremes whenever a fit attains. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-free-truncateknown)

### kumar a truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of log(1-x^a) lies strictly between its zero-shape mean and upper endpoint.

**Proved · Failure.** On compatible positive width, excluded reciprocal carrier or lower x collapse.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-a-truncateknown)

### kumar a truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of log(1-x^a) lies strictly between its zero-shape mean and upper endpoint.

**Proved · Failure.** On compatible positive width, excluded reciprocal carrier or lower x collapse.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-a-truncateknown)

### kumar a truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of log(1-x^a) lies strictly between its zero-shape mean and upper endpoint.

**Proved · Failure.** On compatible positive width, excluded reciprocal carrier or lower x collapse.

**Proved · Cardinality.** Unique if attained. The estimated cuts are uniquely tight at attained fits.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-a-truncateknown)

### kumar b truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the power in negative log x when integrable. A constant sample at the included upper x cut diverges to an atom.

**Proved · Cardinality.** For held b=1, the finite maximizer is unique whenever it exists. The estimated cuts are uniquely tight at attained fits.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-b-truncateknown)

### kumar b truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the power in negative log x when integrable. A constant sample at the included upper x cut diverges to an atom.

**Proved · Cardinality.** For held b=1, the finite maximizer is unique whenever it exists. The estimated cuts are uniquely tight at attained fits.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-b-truncateknown)

### kumar b truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the power in negative log x when integrable. A constant sample at the included upper x cut diverges to an atom.

**Proved · Cardinality.** For held b=1, the finite maximizer is unique whenever it exists. The estimated cuts are uniquely tight at attained fits.

The effective upper cut is clipped at 1. For the coordinate $y=-\log x$, a retained end at 1 is $y=0$, and a retained end at zero is $y=\infty$. The order is reversed.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#kumar-b-truncateknown)

### pareto2 free truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, no compatible sample with all three coefficients free attains.

**Proved · Failure.** On compatible positive width, every compatible sample has an unbounded likelihood. A small-scale lower atom path suffices when the sample minimum is below the upper cut, and shrinking retained width suffices at upper-cut equality.

**Proved · Cardinality.** No attained branch. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-free-truncateknown)

### pareto2 free truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, no compatible sample with all three coefficients free attains.

**Proved · Failure.** On compatible positive width, every compatible sample has an unbounded likelihood. A small-scale lower atom path suffices when the sample minimum is below the upper cut, and shrinking retained width suffices at upper-cut equality.

**Proved · Cardinality.** No attained branch. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-free-truncateknown)

### pareto2 free truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, no compatible sample with all three coefficients free attains.

**Proved · Failure.** On compatible positive width, every compatible sample has an unbounded likelihood. A small-scale lower atom path suffices when the sample minimum is below the upper cut, and shrinking retained width suffices at upper-cut equality.

**Proved · Cardinality.** No attained branch. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-free-truncateknown)

### pareto2 min truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, away from divergence, the excluded regular faces are the shifted Lomax exponential, power and translated reciprocal. Threshold-contact spikes and all-lower-cut collapse give unbounded likelihood in the specified branches.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-min-truncateknown)

### pareto2 min truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, away from divergence, the excluded regular faces are the shifted Lomax exponential, power and translated reciprocal. Threshold-contact spikes and all-lower-cut collapse give unbounded likelihood in the specified branches.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-min-truncateknown)

### pareto2 min truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, away from divergence, the excluded regular faces are the shifted Lomax exponential, power and translated reciprocal. Threshold-contact spikes and all-lower-cut collapse give unbounded likelihood in the specified branches.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-min-truncateknown)

### pareto2 min+scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of the log transformed excess lies strictly between zero and the zero-shape mean.

**Proved · Failure.** On compatible positive width, lower collapse or the excluded translated reciprocal law.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minscale-truncateknown)

### pareto2 min+scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of the log transformed excess lies strictly between zero and the zero-shape mean.

**Proved · Failure.** On compatible positive width, lower collapse or the excluded translated reciprocal law.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minscale-truncateknown)

### pareto2 min+scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, the mean of the log transformed excess lies strictly between zero and the zero-shape mean.

**Proved · Failure.** On compatible positive width, lower collapse or the excluded translated reciprocal law.

**Proved · Cardinality.** Unique if attained. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minscale-truncateknown)

### pareto2 min+scale+shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minscaleshape-truncateknown)

### pareto2 min+scale+shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minscaleshape-truncateknown)

### pareto2 min+scale+shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, evaluate the specified coefficients on a positive-mass compatible window.

**Proved · Failure.** On compatible positive width, an invalid window or incompatible sample has no positive-likelihood fit.

**Proved · Cardinality.** At most one specified coefficient vector. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minscaleshape-truncateknown)

### pareto2 min+shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the truncated power, uniform or exact critical contact envelope. A negative small-scale exponent gives an unbounded atom path.

**Proved case.** For infinite fixed upper cut, the retained pole likelihood has a strictly decreasing log-scale score on distinct data. Use its unique root after applying the open or included lower scale constraint for the holds, or its excluded-end limit. Restore the entire pole fiber when applicable.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minshape-truncateknown)

### pareto2 min+shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the truncated power, uniform or exact critical contact envelope. A negative small-scale exponent gives an unbounded atom path.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minshape-truncateknown)

### pareto2 min+shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the truncated power, uniform or exact critical contact envelope. A negative small-scale exponent gives an unbounded atom path.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-minshape-truncateknown)

### pareto2 scale truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the translated reciprocal or shifted exponential limits. Constants diverge by lower collapse or upper width collapse.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-scale-truncateknown)

### pareto2 scale truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the translated reciprocal or shifted exponential limits. Constants diverge by lower collapse or upper width collapse.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-scale-truncateknown)

### pareto2 scale truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the translated reciprocal or shifted exponential limits. Constants diverge by lower collapse or upper width collapse.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-scale-truncateknown)

### pareto2 scale+shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform. Upper width collapse gives unbounded likelihood.

**Proved case.** For infinite fixed upper cut, the retained pole likelihood has a strictly decreasing log-scale score on distinct data. Use its unique root after applying the open or included lower scale constraint for the holds, or its excluded-end limit. Restore the entire pole fiber when applicable.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-scaleshape-truncateknown)

### pareto2 scale+shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform. Upper width collapse gives unbounded likelihood.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-scaleshape-truncateknown)

### pareto2 scale+shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform. Upper width collapse gives unbounded likelihood.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme. General parent cardinality remains open beyond the stated cases.

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-scaleshape-truncateknown)

### pareto2 shape truncate:lower-estimated

Use $[m,b]$ with the observed minimum included and $h\le b$. At a constant $m=h=b$, only the left side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform or critical contact envelope. A negative small-scale exponent or upper width collapse gives unbounded likelihood.

**Proved · Cardinality.** Whenever attained with the lower cut at m, any winning pole p has the full threshold-scale fiber p<c<=m, scale=c-p. With an estimated lower cut the joint fiber also requires max(c,cut)=m. The finite-width witness establishes nonuniqueness in this design. On a positive-mass fit the lower cut is unique at m, or ranges over all cuts at or below m if the fitted support starts there.

**Proved case.** For infinite fixed upper cut, the retained pole likelihood has a strictly decreasing log-scale score on distinct data. Use its unique root after applying the open or included lower scale constraint for the holds, or its excluded-end limit. Restore the entire pole fiber when applicable.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-shape-truncateknown)

### pareto2 shape truncate:upper-estimated

Use $[a,h]$ with the observed maximum included and $a\le m$. At a constant $a=m=h$, only the right side can be opened. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform or critical contact envelope. A negative small-scale exponent or upper width collapse gives unbounded likelihood.

**Proved · Cardinality.** Whenever attained with the lower cut at m, any winning pole p has the full threshold-scale fiber p<c<=m, scale=c-p. With an estimated lower cut the joint fiber also requires max(c,cut)=m. The finite-width witness establishes nonuniqueness in this design. On a positive-mass fit the upper cut is unique at h, or ranges over all cuts at or above h if the fitted support ends there.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-shape-truncateknown)

### pareto2 shape truncate:both-estimated

Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts. Every constant gives a nominal singleton, with either side available to open. Use the [singleton test](#joint-profiling-and-boundary-conventions) there.

**Proved · Attainment.** On compatible positive width, outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.

**Proved · Failure.** On compatible positive width, an excluded winning limit is among the finite-window uniform or critical contact envelope. A negative small-scale exponent or upper width collapse gives unbounded likelihood.

**Proved · Cardinality.** Whenever attained with the lower cut at m, any winning pole p has the full threshold-scale fiber p<c<=m, scale=c-p. With an estimated lower cut the joint fiber also requires max(c,cut)=m. The finite-width witness establishes nonuniqueness in this design. On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.

Check threshold contact at $L_x=\max(a_*,c)$. A closed observation there has positive parent density. An estimated lower cut ties outward exactly at $c=m$. The fixed-pole fibers and lower-ray score are in [Power and moving endpoints](#power-and-moving-endpoints).

[Known-window proof](2026-11-27-trunc-shape-math-report.md#pareto2-shape-truncateknown)

<!-- end regime results -->

## Implementation audit

**Source audit.** None of the scoped routes is implemented, including when all parent coefficients are held with data supplied. Each falls outside the continuous-case allowance in the common modifier dispatcher and reaches the documented unsupported-truncation refusal after the applicable argument checks. The atlas agrees for all 447 rows. For example, mlpower(c(1, 1, 2), truncate = c(0, NA)) is a reserved route with the attained joint set proved above. There is no in-scope implemented status to contradict. The routes for normal, Poisson, geometric, binomial, NB and zeta are outside these 25 families. Their estimated statuses, including the exact samples where the extension, numerical tie margin or implemented window domains matter, are audited in TASK-448 and its review. Right censoring of Weibull and Lomax uses a different observation likelihood. [Modifier availability](../../design/modifiers.md#availability-and-refusals), [common dispatcher](../../../bindings/common/modifiers.hpp), [R resolution and dispatch](../../../bindings/r/R/modifiers.R), [previous implementation audit](2026-11-27-trunc-est-math-report.md#implementation-audit)

## What this changes for the fitter

Compute the tight window before the parent search and retain the original estimate flags. Distinguish invalid nominal inputs, support incompatibility, loss of positive sample density and feasible shrinking-window divergence. An effective zero-width continuous event does not define a conditional density. For each positive-width fit carry the full plateau constraints when an estimated endpoint is hidden by a moving support. For example, Power uses $\min(\alpha,b)=h$ and Pareto II uses $\max(c,\ell)=m$ in their endpoint fibers. Report parent uniqueness only when it has been proved for the fitted branch, and account for nominal bound multiplicity in joint uniqueness.

For exact scalar criteria, transform the observed cuts before testing the score, including log, logit and reversed reciprocal cuts. Evaluate a regular face at its own normalized likelihood and use a cut-aware limit for a critical contact. The reported model at a finite excluded height should distinguish physical uniform, transformed uniform, signed truncated or reflected exponential, power and the boundary height of a collapsing law. An excluded face tied with a feasible ordinary model still allows attainment. For the stationary comparisons, a successful search with an unresolved certificate retains its uncertified flag. The exact comparison alone does not furnish a numerical certificate.

## Open

- For case-only GED, skew GED, Azzalini skew-normal, two-piece normal, standardized skew Student, Tukey, Bhattacharjee and EMG masks, the named endpoint, shape or tie witnesses do not exhaust mixed escapes on the profiled windows. The remaining task is to control the coupled free scale, skew and shape directions and their surviving-support normalizers. Detailed masks and missing directions are linked from the regime sections.
- For all-free Fréchet the remaining cases include a strict minimum minority and most exact-half samples with a fixed lower cut below the minimum. The sharp lower-contact bound and the upper-ray equal-frequency two-value result settle their stated branches. General comparison still needs simultaneous shape, scale and location control, including the retained Gumbel family's own escape behavior and variable-shape remote locations.
- Where a proof supplies scores and an exhaustive threshold, general pointwise cardinality still needs root isolation or a shape-of-profile argument together with an exact rule for ties. The padded LST and Slash examples prove nonuniqueness in each estimated design. They leave general cardinality, and the number of maximizers even on those examples, open.
- Critical-density heights and weak limits require separate arguments on unbounded windows. In particular the divergent Pareto II and small-df Student witnesses with escaping mass preclude an exhaustive description purely in terms of ordinary point masses. The statements here give same-height tight witnesses and preserve the reviewers' weaker scope.
- Comparisons involving exact equality between an excluded face and a finite candidate still require implementable comparison or isolation bounds where the report gives a mathematical criterion. Numerical tables do not resolve equality.

## Checks

All final runs used one workstation CPU slot under /tmp/umvl-cpu/run. The numerical replay uses 80 decimal digits and records its explicit grids in the [check table](2026-11-28-trunc-est3-math-data/checks.csv). It verifies the Fréchet upper-ray exponential and lower-contact heights, the held-shape plateau, the Pareto II pole and escaped-mass examples, ordinary width collapse, and transformed endpoint comparisons. The replay includes exact rational checks where available. The [regime generator](2026-11-28-trunc-est3-math-data/regimes.py) reconciles the reviewed known-window inventory with the implemented flags and estimated keys in the atlas. The atlas validator checks keys and evidence anchors without changing the atlas. [Numerical replay](2026-11-28-trunc-est3-math-data/check.py)
