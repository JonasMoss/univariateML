# Maximum likelihood under truncation for the GB2 tree

TASK-461, base eeda124e. The parent coefficient domains and the inside density convention at closed parent endpoints are used. Let $A=[a,b]$ intersect the parent support, with positive retained mass. Samples are nonempty and compatible with that intersection. For positive parents the three windows are $0<a<b<\infty$, $0<a<b=\infty$ and $a=0<b<\infty$. For the shifted parent the cuts are on the original real line and may lie beyond the current parent threshold. The full parent window is also covered by the rules below where their stated integrability conditions apply.

The [exponential-family truncation theorem](2026-11-27-trunc-expfam-math-report.md#general-theorem) supplies the conditional moment test. The [positive-family analysis](2026-11-27-trunc-shape-math-report.md) supplies its reciprocal and Lomax comparisons subject to [reviewed scope](2026-11-27-mod-rev6-report.md). The [half-Cauchy review](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy) and [Fréchet review](2026-11-27-mod-rev6-report.md#fréchet-with-shape-held-misses-the-upper-only-exponential) show why each proposed kernel must be checked separately for integrability on both one-sided windows. The [joint likelihood envelope](../ml-behaviour.tex) also requires retaining the values at observations on a limiting support boundary.

## Likelihood formulas and comparison rules

**Proved.** For an observation coordinate $z=\phi(x)$, its window $J$, and a kernel $w$ relative to $dz$, the exact likelihood is
$$
{\cal L}_J(w)=\frac{\prod_i|\phi'(x_i)|\, w(\phi(x_i))}
                      {(\int_J w(z)\,dz)^n}.
$$
All limits below use this formula, with any specified contact factor multiplying the whole likelihood. It suffices to compare values before multiplying by the Jacobian. In particular $y=\log x$ gives a power $x^{r-1}/Z$ from $w=e^{ry}$, with $Z=(b^r-a^r)/r$ for finite positive endpoints and $Z=\log(b/a)$ at zero tilt. At $a=0$ require $r>0$, with $Z=b^r/r$. At $b=\infty$ require $r<0$, with $Z=-a^r/r$. On an interior finite window $r=1$ gives ordinary uniform and $r=0$ gives log-uniform. On a positive lower-only window the fitted slope in the unrestricted power face is $-1/(\overline{\log x}-\log a)$, when the denominator is positive. On the upper-only window it is $1/(\log b-\overline{\log x})$. On a bounded log interval the fitted slope solves $E_r Y=\bar y$, subject to the slope restrictions stated in each face. Exponentials in the $x$ or shifted $x$ coordinate use the same $e^{rz}$ kernel and integral $(e^{rU}-e^{rL})/r$, with the integrable one-sided limits. A weak atom does not carry a Lebesgue density likelihood. Its path height is specified separately.

**Proved, normalization.** Center an unnormalized kernel at an interior reference point by an arbitrary positive parameter-dependent multiplier. If the centered kernels converge pointwise almost everywhere and have an integrable dominator, dominated convergence gives convergence of their integrals. A finite nonzero integral then gives total variation convergence and the displayed likelihood at every observation with a pointwise kernel limit. If the limiting integral diverges while each centered observation kernel is bounded, Fatou gives likelihood zero and excludes this path from a positive supremum. This applies also when the window is narrow. Recenter the integral in actual window coordinates before evaluating it numerically, following the [short-window analysis](2026-11-22-short-window-report.md).

**Proved, scalar test.** For $h(z)e^{\eta T(z)}$ with a free positive tilt $\eta$, the score matches $\bar T$ to $E_\eta T$. The latter is strictly increasing by its positive variance. A unique fit exists iff $\lim_{\eta\downarrow0}E_\eta T<\bar T<\sup_J T$. At a finite integrable zero-tilt edge its height is ${\cal L}_J(h)$. In the instances below where that integral diverges, mass goes to the singular end, $T$ tends there to $-\infty$, and the lower threshold is $-\infty$. To see this, $\{T\ge M\}$ has finite zero-tilt integral in those instances, and its first moment above $M$ remains bounded by the corresponding integral at a positive tilt. At an observed upper extremum equality all data maximize $T$. The likelihood diverges and the weak atom is at the unique extremizer by comparing tilted integrals on separated statistic levels. Parameter-independent carrier terms at observations must be finite and positive for this rule.

**Proved, two-shape tests.** At fixed GB2 scale and power, $z=x^d/(S^d+x^d)$ has beta shapes $p,q$. At fixed generalized-beta scale and power, $z=(x/S)^d$ again has beta shapes $p,q$. For strictly interior parent observations apply the beta criterion in this transformed window. The nonconstant-data test uses the edges $p=0,q>0$ if the retained lower cut in $z$ is positive, $q=0,p>0$ if the retained upper cut is below 1, and the corner if both cuts are strict. Match the mean of the still-free log statistic on each edge, then require the empirical mean of each missing statistic to be at most its edge expectation. The kernel is $z^{p-1}(1-z)^{q-1}$ throughout. A passing edge wins outside the parent domain. No passing edge gives a unique finite fit. Constants collapse at their common value if both shapes are free. With one shape free use the scalar test with its log statistic. For clarity, strict concavity follows from covariance of the log statistics. At infinity in the nonnegative quadrant the leading kernel is $u\log z+v\log(1-z)$ with at most one maximizer for every nonzero direction. For distinct data the finite-cover proof in the exponential-family theorem therefore gives coercivity. Fatou rejects nonintegrable edges and one-sided differentiation gives the inward tests. The same argument applies to the kernel $u^{k-1}e^{-v u}$ with $k,v>0$ after $u=x^d$, or $u=x^{-d}$ for inverse transformed gamma. Its $k=0$ edge requires the lower cut positive, its $v=0$ edge requires finite upper cut, and the corner requires both. The missing-statistic inequality on $k=0$ is $\overline{\log u}\le E\log U$, and on $v=0$ it is $\bar u\ge E U$. The leading kernel is $s\log u-t u$. Scalar reductions of either coordinate use the scalar rule. With $k$ held and rate $v>0$ free use $T=-u$, so lower collapse is at the lower cut of $u$.

**Proved, stationary comparison.** In the comparisons below a feasible score root meeting or exceeding *every* listed excluded height gives attainment, provided the listed limits exhaust positive-height escapes and the constant-divergence cases have been taken out. If a finite competitor is higher, a maximum attains because a maximizing sequence above all excluded heights has an interior convergent subsequence. Conversely an interior maximum is a root and meets all approachable heights. If a finite join or closed support contact is listed, include its value in the feasible candidate set. The number and multiplicity of winning roots require a separate proof.

**Proved, log-concavity bound.** GB2 and transformed gamma are log-concave in $y$ on each retained log interval. For a normalized log-concave density there, let $M$ be its supremum and take an approximate maximizer with height at least $M/2$. For distinct observations choose one at distance at least half the sample range from this point. On the segment at distance $8/M$ the height is at most $M/4$, since otherwise log concavity gives area greater than one on that segment. For large $M$ this segment fits before the selected observation. Concavity of the log gives at the observation a bound $(M/2)\exp(-M D\log 2/16)$, where $D$ is the sample range. The likelihood thus vanishes along $M\to\infty$. On subsequences with $M$ bounded, a centered kernel whose log on compact subsets, divided by a diverging positive multiplier, converges to a leading function with a strictly better interval than at one observation also forces likelihood zero. Integrating on that interval sends the selected density to zero, and the others are bounded. The improvement argument applies to a constant sample whenever its common observation has a strictly better interval. These bounds apply to included cut observations because they use segments inside the closed retained interval.

## Fixed-power beta-prime escapes

**Proved.** Write $u=x^d$, $t=S^{-d}$. The density on the transformed window is proportional to
$$
w=u^{p-1}(1+tu)^{-p-q},\qquad p,q,t>0.
$$
Reciprocation $u\mapsto1/u$ interchanges $p,q$ and $t\mapsto1/t$. Any held shape remains held through the limits, with that interchange when appropriate. For nonconstant data and free $t$, the complete positive-height list consists of the following approachable profiles, with inadmissible integrals deleted.

| Path | Kernel in $u$ | Integrability at open cuts |
| --- | --- | --- |
| $t\to t_0\in(0,\infty)$, at least one shape $\to0$ | $u^{p-1}(1+t_0u)^{-p-q}$ | If the lower cut is zero require $p>0$. If upper is infinite require $q>0$. |
| $t\to0$, $p\to p_0\ge0$, $(p+q)t\to v\ge0$ | $u^{p_0-1}e^{-v u}$ | Require $p_0>0$ at zero. At infinite upper cut require $v>0$. |
| $t\to\infty$ | Reciprocal of preceding row, $u^{-q_0-1}e^{-v/u}$ with $v=\lim(p+q)/t$ | Require $v>0$ at zero and $q_0>0$ at infinite upper cut. |

When the opposite shape is held the gamma rate at that end must be zero. A finite positive rate requires the opposite shape to grow without bound. At rate zero these are pure powers, and they give uniform in $x$ if the transformed exponent yields $r=1$. At fixed $t_0$ and zero shapes the edge includes the log-uniform corner. With $p=1$ the gamma face is exponential in $u$, including shifted exponential after conditioning at a positive lower cut. The table specifies likelihoods through ${\cal L}_J$, with each face varied over its stated parameters. Gamma and reciprocal gamma likelihoods can be written with incomplete gamma integrals, and the first row with the incomplete beta integral on a cut interval.

For proof, at finite $t_0$, bounded shapes give dominated convergence at an integrable limit by choosing fixed smaller positive exponents on any uncut ends. If shapes diverge, divide the log kernel in $\log u$ by $p+q$. Its limit on compact sets is $s\log u-\log(1+t_0u)$, $s\in[0,1]$, strictly concave in $\log u$. For $t\to0$ bounded $p,v$ give the gamma kernel by Taylor expansion. At infinite upper cut and $v>0$, for any fixed $K_0>0$ and $K=p+q\ge K_0$, monotonicity of $(1+v u/K)^K$ in $K$ gives the integrable majorant $u^{p-1}(1+v u/K_0)^{-K_0}$ with $K_0$ chosen above the upper bound of $p$. The small-$u$ bound uses a fixed smaller exponent when needed. If either $p$ or $v$ diverges, scaling by $\max(p,v)$ gives $s\log u-r u$ as leading log-coordinate kernel, with $\max(s,r)=1$. It is strictly concave or strictly monotone there. The log-concavity bound excludes every such path for distinct data. Nonintegrable finite-coefficient limits give zero by Fatou. The reciprocal proof covers $t\to\infty$. Each regular face in the table is realized by keeping the limiting shapes fixed where positive, approaching zeros within the positive domain, and choosing the appropriate opposite shape and $t$ product. These arguments give the stationary comparison with the table. 

If both shapes are free, constants concentrate at any $u_0>0$, even at a known cut, by multiplying positive shapes with mode $u_0$ in the log coordinate. This is a scalar tilt of a log kernel with a strict maximum there. If only $q$ is free, divergence is possible for constants exactly at the positive lower cut, by $q\to\infty$ at fixed scale. For any other constant all divergent-shape paths above have a strictly decreasing leading kernel in $\log u$. In the reciprocal $t\to\infty$ analysis the ratio $(p+q)/(tq)\to0$ whenever $q\to\infty$. Its leading kernel is therefore increasing in $\log(1/u)$, also decreasing in $\log u$. The strict improvement bound excludes these paths. Reflect for only $p$ free. With both shapes held only scale-end limits in the table apply and there is no concentration. This gives the same comparison for constants outside the divergent branches. When scale is held, the two-shape and scalar tests give the stronger direct criterion and uniqueness.

## Varying the power

**Proved, smooth reductions.** In $y=\log x$, write $m=\log S$, $d>0$, and $z=y-m$. GB2 and transformed gamma have kernels
$$
 e^{p d z}(1+e^{dz})^{-p-q},\qquad e^{k d z-e^{dz}},
$$
respectively. Burr has $p=1,q=\text{shape1},d=\text{shape2}$ and paralogis ties $q=d,p=1$. Inverse versions reflect $y$. At fixed $d$, sending $q\to\infty$, $q/S^d\to v>0$ gives transformed gamma of shape $p$ and rate $v$; sending $p\to\infty$, $pS^d\to v>0$ gives its reciprocal with shape $q$. The fixed-power lemma proves these limits on each admissible window. Truncated exponential arises in physical $x$ when $d=p=1$ on the first face, and truncated reciprocal exponential when $d=q=1$ on the second.

**Proved, concentrated or very broad log densities.** Consider Burr with $q>0$ held, paralogis with $q=d$, or transformed gamma with $k>0$ held. The log-coordinate mode is $c=m-\log q/d$ for Burr and paralogis, and $c=m+\log k/d$ for transformed gamma. At $d\to\infty$, away from any fixed neighborhood of $c$ the derivative of the log kernel is positive to the left and negative to the right, with magnitude tending uniformly to infinity there. For Burr and paralogis this follows from $d(1-q e^{dz})/(1+e^{dz})$ and $q$ being fixed positive or $d$. For transformed gamma it follows from $kd(1-e^{d(y-c)})$. For two distinct observations, along any subsequence either the mode lies below their midpoint or it lies above it. In either case an interval towards the midpoint beats the farther observation by a diverging log difference. The log-concavity bound gives zero likelihood for distinct data. With free scale a constant at any admissible cut or interior value concentrates by aligning the log mode with that value. Indeed the parent probability away from that point tends to zero, by the slope bound and a strict comparison with an intervening interval. More directly, the ratio of the kernel to its value at the mode tends pointwise to zero off that point, bounded by 1 near the mode and by an integrable exponential envelope beyond a fixed neighborhood on each tail. The normalizer relative to the modal kernel tends to zero, also after conditioning, so the likelihood diverges.

With held scale the modes converge to $m$. Constants away from the projection of $m$ onto the retained log interval have a strictly better interval at $d\to\infty$ and likelihood zero along that end. At the projection the likelihood diverges for Burr and transformed gamma. If the projection differs from $m$, the conditional derivative has one sign and magnitude bounded below by a quantity tending to infinity throughout the window, so the endpoint density tends to infinity by integration. At $m$ the normalized parent density at $m$ is proportional to $d$ by a change of variable. Paralogis has the same conclusion except at $c_{\rm obs}=m$ if the window includes an interval to its left. In that case the density at $m$ tends to zero: the parent log-density there is $d^2 2^{-d-1}$, and the retained probability is bounded below. Indeed the variable $d(y-m)+\log d$ has survivor $(1+e^z/d)^{-d}$ in its own coordinate $z$, converging to $\exp(-e^z)$, and any fixed compact interval in $z$ eventually lies inside the left neighborhood. If $m$ is the lower cut, the log derivative on $y\ge m$ is at most $-d(d-1)/2$ and gives lower collapse. All the divergence branches just described have weak atom limits in the retained window, by the same slope comparison, or by the survivor formula when the mode moves toward $m$ from inside.

At $d\downarrow0$ for Burr with held $q$, or paralogis, the log derivative is bounded in absolute value by $d\max(1,q)$, independently of scale. Centered kernels tend uniformly to 1 on compact sets, giving log-uniform on a bounded log window and zero likelihood on either unbounded side by Fatou. For transformed gamma with held $k$ and held $m$ the same small-power conclusions follow by local expansion at fixed $m$. Thus the regular boundary comparison for these Burr and paralogis masks, and the transformed-gamma mask holding both $k,m$, is solely the log-uniform on a bounded log window, with the constant divergences described above. Every boundary height used in this comparison is a limit along feasible coefficients.

**Proved, fixed gamma shape with free scale.** Put $v=S^{-d}$ and center the log kernel at $y=0$, so it is $kd y-v(e^{dy}-1)$. At $d\downarrow0$, $dv\to\lambda\in[0,\infty)$ gives the power slope $-\lambda$. For $\lambda>0$ use an upper-tail majorant $e^{-(\lambda/2)y}$ for large indices and $y\ge0$, by $e^{dy}-1\ge dy$. A finite lower cut is then sufficient. At $\lambda=0$ only a bounded log window is eligible. Without an integrable limit Fatou gives zero. If $dv\to\infty$, the leading kernel divided by $dv$ is $-y$ on compact sets. With a finite lower cut the log slope there is negative and decreases for larger $y$, so this branch concentrates at the lower cut. At finite $d>0$ and $v\downarrow0$, the power slope is $kd>0$, integrable iff the upper cut is finite. Domination on the lower half-line follows from $e^{kd y}$ with $kd$ bounded below. At $v\to\infty$ the leading kernel divided by $v$ is strictly decreasing. These branches and the large-power bound exhaust escapes. Therefore for distinct data the comparison is against all integrable power slopes, including zero on the bounded log window. Constants diverge by free alignment of the mode. Under reciprocation the slopes and the windows reverse.

**Proved, hard-wall paths and contact.** At held $m$, for transformed gamma let $kd\to r\ge0$, $d\to\infty$. On a window with positive length below $m$, the limit density in $y$ is $e^{r y}/Z$ on the retained part below $m$ if that integral is finite. If it diverges, likelihood is zero. On the far left take $kd\ge r/2$ when $r>0$, and on the right use the superexponential cutoff as an integrable envelope even if the upper window is infinite. At $y=m$ the pointwise trace is $e^{-1}$ times the from-below value, so the joint likelihood includes $e^{-j}$ for $j$ contacts and is zero if any observation is above $m$. With free $m$ the full from-below trace can be obtained by taking the wall to move to $m$ from above with $d(m_d-m)\to+\infty$. For GB2 at fixed $m$, $pd\to r\ge0$, $q\to q_0>0$, $d\to\infty$, the analogous upper wall has trace $2^{-q_0}$, and log-kernel on the left $r(y-m)$. Tail domination follows from $pd\ge r/2$ on the far left when needed, and from a negative slope bounded away from zero on the far right. Reflection gives the lower wall with trace $2^{-p_0}$ and slope $-r$. A distinct path $pd\to r>0,\ qd\to s>0,\ d\to\infty$ yields the continuous two-piece exponential in $y$ with wall at $m$, slopes $r,-s$. To verify it, write $\log(1+e^{dz})=d z_+ +\epsilon$, $0\le\epsilon\le\log2$. The error multiplied by $p+q$ tends uniformly to zero. The pieces are integrable also on their entire half-lines, so their retained integrals simply add, and the sample at the join carries full density. This is a double power in $x$, with a common multiplicative scale chosen for continuity.

**Proved, remaining escapes at held transformed-gamma scale.** Write $\rho=kd$; the kernel becomes $e^{\rho(y-m)-e^{d(y-m)}}$. At finite $d>0,\rho\downarrow0$ use the excluded kernel $e^{-e^{d(y-m)}}$ on a log interval with finite lower cut. At $d\downarrow0,\rho\to r\ge0$, use the power of slope $r$ when integrable, via the same local expansion and lower-tail bounds. At bounded $d$ and $\rho\to\infty$ the scaled log kernel is $y-m$ on compact sets. For $d,\rho\to\infty$ the log slope is $\rho(1-e^{d(y-c)})$ about $c=m+\log(\rho/d)/d$, so the separated-observation bound excludes this path for distinct data regardless of where $c$ tends. For $d\to\infty$ and bounded $\rho$, the hard-wall path applies if the window begins below $m$. If its lower cut is at or above $m$, the log derivative there tends to $-\infty$ and decreases for larger $y$. Thus the conditional law collapses at the lower cut, with zero likelihood for distinct data by the log-concavity bound and strict improvement above the cut. These cases exhaust escaping sequences by taking subsequences in $(d,\rho)$.

For constant $y_0$, divergence occurs exactly at $y_0\ge m$ or $y_0$ equal to the finite upper cut. At or above $m$, choose $d\to\infty,\ \rho=d e^{d(y_0-m)}$ to put the mode there. The log slope on either side separated by any fixed positive distance diverges with the appropriate sign, and its integral relative to the mode tends to zero by dominated convergence. At the upper cut use $\rho\to\infty$ with fixed $d$. For a constant $y_0<m$ strictly below the upper cut, an adjacent interval to the right and still below $m$ improves by a diverging log difference whenever $\rho\to\infty$, also for $d\to\infty$. The remaining paths have the regular boundary heights. This proves the stationary comparison after excluding divergent constants, with the hard-wall trace as given. Both shape and scale changes must be allowed to retrieve the reflected versions.

**Proved, reachable Gaussian in logarithms.** In the all-free transformed gamma and GB2 trees a normal in $y$ of mean $c$, variance $s^2>0$, is reachable. For transformed gamma choose $d\downarrow0$, set $k=1/(s^2d^2)$ and set $m=c-\log k/d$. The centered log kernel at $c$ is $-k(e^{d(y-c)}-1-d(y-c))$. For GB2 use $m=c,p=q=2/(s^2d^2)$; the centered log kernel is $-2p\log\cosh(d(y-c)/2)$. Their Taylor limits agree with the normal quadratic. Their log kernels are concave, hence the slopes at fixed points on either side bound the tails by integrable exponentials uniformly for small $d$. The conditional normalizer and sample values converge. The conditioned normal in $y$ is a truncated lognormal in $x$. This does not classify other joint small-power paths. In Prentice generalized gamma the lognormal is already the finite $Q=0$ member.

## Moving support and endpoint conventions

**Proved.** For generalized beta with scale $S$ and shapes $p,q,d$ (repository shape1, shape2, shape3), use $u=x^d$ with kernel $u^{p-1}(1-u/S^d)^{q-1}$, $0<u\le S^d$. If $q<1$ is feasible and $S$ is free, approaching $M=\max_i x_i$ from above yields unbounded likelihood whenever $M>a$, since the retained probability converges to a positive value and every observation at $M$ has diverging density. The retained laws converge to the conditional generalized beta at $S=M$: parent laws are scaled copies with densities converging pointwise almost everywhere, hence converge in total variation, and their retained masses converge as well. At $M=a>0$ all observations are at the lower cut, and $S\downarrow a$ shrinks the width. For any fixed shapes the likelihood diverges with weak collapse. In detail, the density at $a$ divided by the interval mass is asymptotic to $q/(S-a)$ from the endpoint expansion. At exact contact $S=M>a$, the inside density convention gives $+\infty$ at $q<1$, zero at $q>1$, and the finite power value at $q=1$. With $S$ held and a contact observation, these same values decide the objective before any shape score. Thus a contact singularity is a failure of finite likelihood fitting even when the extended likelihood is literally attained by a singular density. If all feasible coefficients give zero at contact there is no positive-likelihood report.

**Proved, generalized beta at $q=1$.** Only $r=pd$ and the effective upper end $B=\min(b,S)$ enter the density, $x^{r-1}/\int_a^B x^{r-1}dx$. At fixed $r$, if $S$ is free, the smallest feasible $B$ maximizes the likelihood strictly in $B$ whenever $B$ varies. It is $M$ at $S=M$ if $a<M<b$, and $b$ for every $S\ge b=M$ at upper-cut contact. If $M=a>0$ with $S$ free the shrinking-width path diverges. Otherwise when $r$ is free, on the selected or held effective window use $T=\log x$ with positive tilt $r$ against $dx/x$. The unique product exists iff its log mean is between the zero-rate mean and the upper log cut. At finite lower cut the excluded lower face is log-uniform. Equality at the upper cut gives upper collapse. Whenever $r$ fits, the entire coefficient set consists of the feasible factor pairs with product $r$ and the optimizing scales just described. With both factors held just evaluate the fixed exponent. Exact example with held $q=1$, window $(0,2]$, sample $(1,2)$: the fitted product is $2/\log 2$, with all corresponding factor pairs if both can vary, and all $S\ge2$ if scale can vary.

**Proved, generalized beta free endpoint with all shapes held.** For $q>1,M>a$, the value tends to zero at $S\downarrow M$, since the retained integral has a finite positive limit. As $S\to\infty$ the $pd$ power is the only excluded positive height, on finite upper windows. On an infinite upper window the limiting normalizer diverges by Fatou. The fixed positive-power kernel converges locally to that power and is dominated on a finite upper window. Use stationary comparison in $S>M$. The case $M=a$ is the width collapse just proved.

**Proved, shifted GB2.** For held threshold $h=\text{min}$, apply the GB2 formulas to $x-h$ on the translated window if all excesses are positive. At a contact with $h$, use the closed inside density: infinite if $dp<1$, zero if $dp>1$, and finite if $dp=1$. The $dp=1$ contact value follows by evaluating $1/(1+t u)^{p+q}$ at $u=0$ in the physical excess kernel $(x-h)^{dp-1}(1+t(x-h)^d)^{-p-q}$. For free $h$, if $dp<1$ is feasible, approach $h\uparrow m_x=\min x_i$ holding $d,p,q,S$ fixed. When $m_x<b$, the conditional probability converges to a positive value and the likelihood diverges. Translation gives almost-everywhere density convergence and therefore total variation on the parent line and the retained event. If $m_x=b<\infty$ all observations are at that cut. Then the numerator at $b$ divided by the shrinking mass is asymptotic to $dp/(b-h)$ for any fixed shapes and scale, so likelihood diverges with weak collapse. Neither path claims that all divergent sequences are tight.

**Proved, shifted Lomax specialization.** At $p=d=1$ the physical kernel is $(x-h+S)^{-q-1}$. Use the corresponding reviewed Pareto II rules with threshold $h$ and scale $S$. For example with $q=2$ also held, $S,h$ free, window $[1,\infty)$ and sample $(1,2)$, the conditional likelihood in $s=S+1-h$ is $4s/(1+s)^3$ for $h\le1$, maximized exactly at $s=1/2$ by differentiation. Every $h\in(1/2,1]$, $S=h-1/2$ is a winner. This is an exact nonunique sample under the three-shape hold. It also shows why deleting the unobserved part of the support can produce coefficient redundancy distinct from equality of separated stationary heights.

## Transformed gamma with all coefficients free

**Proved.** For distinct data, write the log-coordinate kernel as $e^{\rho y-v e^{dy}}$, with $d,\rho,v>0$. The complete list of approachable positive-height boundaries, using the upper envelope at any contact, is:

| Boundary | Kernel in $y$ and admissible cuts |
| --- | --- |
| Finite $d>0$, $\rho=0$, $v>0$ | $e^{-v e^{dy}}$, finite lower cut |
| Finite $d>0$, $v=0$, $\rho\ge0$ | $e^{\rho y}$, when integrable |
| $d\downarrow0$ | $e^{B y-Cy^2/2}$, $C\ge0$, when integrable |
| $d\to\infty$ | $1_{\{y\le h\}}e^{r y}$, $r\ge0$, positive width below $h$, when integrable there |

The hard-wall row uses full density at $h$. Other paths to the same wall can have a smaller joint height. The first row with $d=1,v>0$ is $x^{-1}e^{-vx}$, distinct from the ordinary truncated exponential. The third row covers all integrable powers and all truncated lognormals. The kernel formulas use ${\cal L}_J$ including the cutoff indicator. Constants diverge by modal alignment.

At finite $d>0$, bounded $\rho,v$ give pointwise convergence. Integrable tails are dominated using $\rho$ bounded below on an uncut left side and $v$ bounded below on an uncut right side. If $\max(\rho,v)$ diverges, its scaled leading kernel is $s y-t e^{d y}$ and distinct data force zero likelihood by the log-concavity bound. For $d\downarrow0$, recenter at zero and put $B=\rho-vd,\ C=vd^2$. The log kernel is
$$
B y-C\,\frac{e^{dy}-1-dy}{d^2}.
$$
Bounded $B,C$ give the claimed limit. On an uncut tail, integrability implies a strictly inward tail slope at a fixed finite point, which gives a uniform integrable exponential bound by concavity. If $\max(|B|,C)$ diverges, division by it gives a strictly concave quadratic or nonconstant linear leading kernel, so the likelihood vanishes for distinct data. Every $C>0$ and $B\in\mathbb R$ is approached with $v=C/d^2,\ \rho=C/d+B$. For $C=0$ and $B>0$, take fixed small positive $v$ and $\rho=B+vd$. For $B\le0$, take fixed positive $\rho$ and $v=(\rho-B)/d$.

For $d\to\infty$ and $\rho\to\infty$, the modal slopes $\rho(1-e^{d(y-c)})$ exclude positive height for distinct observations, as in the held-scale proof. Otherwise take a subsequence with $\rho\to r\ge0$ and $h=-\log v/d$ convergent in the extended real line. If the limiting wall is at or below the lower cut, an observation strictly above that wall has a strictly better interval between the cut and itself, giving likelihood zero by the log-concavity bound. The same argument applies when $h\to-\infty$ on a window unbounded below, by using a fixed interval below a selected observation. If the wall is finite and there is positive width below it, the pointwise limits and domination of the hard-wall proof apply with $h$ in place of $m$. For $r=0$ and lower infinite, Fatou applies, since observation kernels are bounded. If the wall tends to $+\infty$, the pointwise limit is the pure power of slope $r$. Fatou excludes nonintegrable cases and the lower-tail power bound covers integrable cases. A wall with full trace is realized by $h_d=h+d^{-1/2}$ and suitable positive $\rho_d\to r$. Thus in the last row the full trace is an upper bound and is approachable. The list with stationary comparison gives an exact attainment test on each window. Reflect for inverse transformed gamma.

For all-free Prentice generalized gamma put $y=\log x$. For $Q\ne0$, use sign $\varepsilon=\operatorname{sgn}(Q)$, $d=|Q|/\sigma$, $k=Q^{-2}$ and $m=\varepsilon\mu-(\log k)/d$ in the transformed-gamma kernel for $\varepsilon y$. This is bijective on each sign. The $Q=0$ seam is the normal family in $y$. For distinct data compare candidates from both signed score systems with the best *attained* lognormal from the normal truncation criterion and the two signed boundary lists. The excluded list may include normals for comparison even though those laws are attained on the seam. Any maximizing sequence on a sign either converges to a finite fit there or has an upper height covered by the list. On the seam the normal theorem covers all maximizing sequences and its power edge is already listed. Thus a feasible finite score root or seam maximum meeting all listed heights is equivalent to attainment. For held nonzero $Q$ apply the fixed-gamma-shape analysis in the reflected coordinate if needed. With $Q,\mu$ held, the log-coordinate density is a fixed log-gamma template scaled about the mode $\mu$, with slope multiplier $d$. The small-power and large-power proof for a held scale applies to this template, so the only excluded regular boundary is log-uniform on a bounded log window. Constants at the mode's projection diverge. With $Q,\sigma$ held and nonzero $Q$, $d,k$ are fixed and moving $\mu$ frees the rate, which is the scalar two-shape reduction above. For $Q=0$, all three masks reduce to the respective normal rules on the log window.

With $\mu,\sigma$ held the free parameter $Q$ has the two ends $Q\to\pm\infty$. In the signed coordinate the moving wall approaches $\varepsilon\mu$, the slope tends to zero, and the unnormalized value at the wall's limiting location tends to one since $v e^{d\varepsilon\mu}=k\to0$. If the window contains positive width below that location, the hard-wall dominated or divergent-normalizer argument gives the log-uniform on that part (finite lower cut required). Its likelihood includes the full trace. With the lower cut strictly above the wall, the derivative there tends to minus infinity, giving lower collapse and divergence for constants at that cut. At equality with the lower cut, write $z=y-\varepsilon\mu$. The density at $z=0$ diverges since the integral over $z\ge0$ tends to zero, via domination on the right by the slope at any positive $z$ and convergence to zero there. These are weak atoms at that cut, and the log-concavity bound excludes positive height for distinct data there. For constants above the projected wall on those windows a better interval gives zero. At finite $Q\to0$ the centered kernel converges to the normal quadratic with the given mean and variance, with tail domination by concavity. The likelihood is continuous at zero. Compare the feasible candidate $Q=0$ and the nonzero score roots against the stated faces, after excluding divergent constants. Cardinality of winning roots is open.

## Held-endpoint generalized beta with only power free

**Proved.** Suppose $S,p,q$ are held, the sample lies strictly below $S$, and $m=\log S,\ z=y-m\le0$. The log-window kernel is $e^{pd z}(1-e^{dz})^{q-1}$. As $d\downarrow0$, divide by $d^{q-1}$ to get $(-z)^{q-1}$, integrable iff the lower log cut is finite. On a finite window, $(1-e^{dz})/d$ is comparable to $-z$ uniformly, and the endpoint exponent $q-1>-1$ gives domination even when the cut reaches $S$. Otherwise Fatou gives zero. This edge is a power in distance from the upper log endpoint, with its likelihood computed by ${\cal L}_J$. As $d\to\infty$ and the upper cut is strictly below $S$, $1-e^{dz}$ tends uniformly to one on the whole window. Thus the large-power likelihood is asymptotic to the increasing exponential's height, unbounded exactly for constants at the upper cut and zero otherwise, with a weak atom at that cut. If the upper cut is $S$, normalized parent mass retained tends to one: the latent beta variable lower cut tends to zero. At each strictly interior observation the parent log density, proportional to $d e^{pd z}(1-e^{dz})^{q-1}$, tends to zero. These ends give stationary comparison and no finite cardinality result in general. At parent contact use the endpoint convention instead. At $q=1$ the product rule above proves the stronger direct criterion.

## Prentice generalized gamma with one location or scale hold

**Proved, held $\sigma>0$.** For each sign use the all-free transformed-gamma coordinates with $\rho=1/(\sigma^2d)$, $v=\rho e^{-d\varepsilon\mu}/d$. At finite $d>0$ the regular excluded limit is $v\to0$, giving the positive signed power of slope $\rho$ on a finite signed upper window. At $v\to\infty$ the leading kernel is strictly decreasing. At $d\to\infty$, $\rho\to0$ and the only regular limit is log-uniform below an arbitrary signed wall $h$ on the retained part, provided that part has finite positive width. This includes a wall above the window and uses full trace, achievable with freely varying $\mu$. If $d\downarrow0$, use $B=\rho-vd$, $C=vd^2=\sigma^{-2}-dB$. Bounded $B$ forces $C\to\sigma^{-2}$ and $\varepsilon\mu\to \sigma^2 B$, by the expression for $v$. This is a finite normal seam fit. If $|B|\to\infty$ then $C/|B|\to0$, so the leading scaled kernel is strictly monotone. The bounds in the all-free proof reject those paths for distinct data. Constants at any finite log cut diverge, using the normal seam with location receding beyond the cut. For any other constant, every path just rejected gives a strictly better retained interval and hence zero likelihood, including when a hard wall lies at or below the lower cut or when an observation lies strictly above a hard wall. The exact comparison includes the eligible seam maximum, the two signed stationary sets, and the positive powers and wall log-uniforms above.

**Proved, held $\mu$.** In the signed centered coordinate $z=\varepsilon(y-\mu)$, the log kernel relative to its value at zero is $k[dz-(e^{dz}-1)]$, with $k,d>0$ independently free. At finite $d>0,\ k\to0$ it gives log-uniform on bounded log windows. If $d\downarrow0$, $kd^2\to C\ge0$ gives a normal of mean $\mu$ and variance $1/C$ in logs when $C>0$, already feasible on the seam, and log-uniform when $C=0$ and the window is bounded. A diverging $kd^2$ has leading log kernel $-z^2/2$ after scaling. At finite $d>0,\ k\to\infty$ the leading function $dz-e^{dz}+1$ is strictly concave with maximum at zero. At $d\to\infty$ with $kd\to\infty$, use the modal slopes about zero. These three diverging cases give zero likelihood for distinct data, or a strictly better interval for a constant away from the projected mode. A constant at the projected mode diverges on the normal seam with variance tending to zero.

For $d\to\infty$ with $kd\to r\in[0,\infty)$, $k\to0$. The wall coordinate is $h=-\log k/d$, with any subsequential limit $h\in[0,\infty]$. If $r>0$, then $h\to0$. The regular limits are $1_{\{z\le0\}}e^{r z}$ for $r>0$ and $1_{\{z\le h\}}$ for $r=0$, subject to integrability and positive retained width. The trace is at most full. For $r>0$ take $k=r/d$, giving trace one in the limit. For $r=0$, take $k=d^{-2}$ for $h=0$, $k=e^{-d(h+d^{-1/2})}$ for finite $h>0$, or $k=e^{-d^2}$ for $h=\infty$. Dominated convergence and the bounds for wall and divergent-normalizer cases are the same as in the all-free transformed-gamma proof. When there is no retained width below a wall, an above-cut observation has a strictly better interval, and a constant at that cut can already diverge at the projected mode. Thus the two signed candidate sets and the held-normal-mean seam are compared to the indicated powers and gated log-uniforms. The power at the wall can be nonzero on an unbounded left log window.

## Complete varying-power GB2 and Burr boundaries

**Proved.** Consider $p,q,d>0$, $m=\log S$, with scale free and with any subset of the beta shapes held positive. For distinct data the following list exhausts excluded positive heights, interpreted as upper envelopes at wall contacts.

- At $d\to d_0\in(0,\infty)$ use the fixed-power beta-prime list at $d_0$, keeping the stated shape holds. Its proof applies to varying $d\to d_0$ in $y=\log x$. Local convergence uses $u=e^{dy}$ on compact sets. Integrable tails use $d$ bounded above and away from zero and the same positive exponent and gamma bounds in $u$, or concavity of the log kernel with strictly inward tail slopes. Divergent coefficients give the same leading strictly concave or strictly monotone functions as before.
- At $d\downarrow0$ the kernel is $e^{B y-Cy^2/2}$, $C\ge0$, when integrable. If both shapes are free, all $B\in\mathbb R,\ C\ge0$ are available. With $p$ held, $C=0$ and $B\le0$; with $q$ held, $C=0$ and $B\ge0$. Holding both allows only $B=C=0$.
- At $d\to\infty$ put $\alpha=pd,\ \beta=qd$. If both stay bounded, their limits $r,s\ge0$ give the continuous two-piece exponential $e^{r(y-m)}$ below a finite wall $m$ and $e^{-s(y-m)}$ above it. At a wall tending to $+\infty$ or $-\infty$ the limit is respectively the whole-window tilt of slope $r$ or $-s$. A zero tail slope is allowed only when that retained tail is finite. This branch requires both shapes free.
- If $\alpha\to r\ge0,\ \beta\to\infty$, the limit is the hard upper wall $1_{\{y\le h\}}e^{r y}$, with full trace as upper envelope, if the retained part has positive width and that integral is finite. The wall can lie beyond the window, including at $+\infty$, giving a pure power. This branch requires $p$ free. When $\beta$ is bounded and $\alpha$ diverges, reflect the result and exchange $p,q$, giving a lower wall and nonpositive slope.

For the small-power proof, set $K=p+q,\ s_0=(1+e^{dm})^{-1}$, $B=d(p-Ks_0)$, $C=d^2K s_0(1-s_0)$. The derivative and negative curvature at zero are $B,C$, respectively. The negative curvature at $y$, divided by $C$, is $e^{dy}/(1-s_0+s_0 e^{dy})^2$, tending uniformly to 1 on compact sets for all $s_0\in(0,1)$. Therefore the centered kernel converges to the indicated quadratic if both quantities are bounded. Integrable tails use uniform bounds by the tangent slopes on finite points, and nonintegrable limits are rejected by Fatou. If $\max(|B|,C)$ diverges, divide the centered kernel by this maximum to obtain a strictly concave quadratic or nonconstant linear leading kernel. Distinct data have zero likelihood in this case by the log-concavity bound. With $p$ held, $B\le dp$ and $C\le d(dp-B)$; the restrictions on finite limits follow. For fixed $m=0$, choose $p+q=4C/d^2,\ p-q=2B/d$ to realize $C>0$ with both shapes free. For $C=0$, fixed $m=0$ and $p-q=2B/d$ with the smaller shape fixed realizes either nonzero slope. If a shape is held, this construction realizes the permitted sign. Fixed shapes give the zero tilt.

For the large-power proof, if both slopes diverge, use the derivative
$$
\frac{\alpha(1-e^{d(y-c)})}{1+(\alpha/\beta)e^{d(y-c)}},
\qquad c=m+\log(\alpha/\beta)/d.
$$
Away from a fixed neighborhood of $c$ its magnitude diverges on both sides uniformly in $c$ and in the slope ratio. For example on the left at distance at least $\delta>0$ it is at least $(1-e^{-d\delta})\min(\alpha,\beta e^{d\delta})/2$; the reflected bound controls the right. Distinct data force zero likelihood. For bounded slopes, the softplus error bound in the two-piece proof is uniform even when $m$ varies. Center at zero and take $m$ convergent in the extended line. This yields the stated kernel with uniform convergence on compacts, including at joins. Concavity and convergence of slopes away from the join give the inward-tail bounds wherever the limit is integrable.

For the hard upper wall, put $K=(\alpha+\beta)/d$, $h=m-\log K/d$. The penalty in the log kernel after dropping a constant is
$$
P_d(y)=K\log(1+e^{d(y-h)}/K).
$$
It tends uniformly to zero on fixed intervals separated below the limiting wall, by $P_d(y)\le e^{d(y-h)}$. Above the wall it diverges and its derivative diverges uniformly on fixed separated intervals: $P_d'(y)=d e^{d(y-h)}/(1+e^{d(y-h)}/K)\ge\min(d e^{d(y-h)},dK)/2$. On the left, integrable tails are dominated by the positive limiting slope if the lower end is uncut. The right slope bound gives domination beyond a fixed point above a finite wall, including on an unbounded upper side. If the upper wall is at or below the lower cut, distinct data lose likelihood by strict improvement towards the cut and the log-concavity bound. The same proof applies on a window unbounded below when $h\to-\infty$, by comparing a fixed interval below a selected observation. At contact $P_d\ge0$, and the full trace is reachable by setting $h_d=h+d^{-1/2}$. For finite $r$ take $\alpha\to r$, and take diverging $\beta$ consistent with the upper shape hold, if any. If the wall tends to $+\infty$, the earlier pure-power integrability rule applies, including Fatou when integral is infinite. The reflection proof exchanges the cut requirements. This proves the list. Constants concentrate by fixed-shape free alignment of the mode, which makes the normalized integral relative to the modal kernel vanish. For distinct data use stationary comparison against the eligible list.

**Proved, Burr with scale held and $q$ free.** Here $p=1$ and $m$ is fixed. At finite $d>0$ the only regular excluded height is $q=0$, the kernel $e^{d(y-m)}/(1+e^{d(y-m)})$ in $y$, integrable iff the upper log cut is finite. As $d\downarrow0$ the allowed kernel is the nonpositive power tilt from the small-power proof with $s_0\to1/2$. Any diverging $|B|$ is negative with $C/|B|\to0$. As $d\to\infty$ the only eligible regular branch has $\beta=qd\to s\ge0$ and gives the lower wall $1_{\{y\ge m\}}e^{-s(y-m)}$ with a factor of $1/2$ at each observation equal to $m$. This follows directly from $e^{d(y-m)}(1+e^{d(y-m)})^{-1-q}$, whose value at $m$ tends to $1/2$. It is eligible when the window contains positive width above $m$ and the kernel is integrable there. For integrable tails use $qd\ge s/2$ if needed on the right and the diverging positive slope on the left. If the retained window lies at or below $m$, a strict improvement toward the upper cut rejects the branch for distinct data. Diverging $\alpha,\beta$ are already excluded for distinct data by the derivative bound.

Constants diverge at the lower cut, or when their common log value $y_0\le m$. The first uses $q\to\infty$ at finite $d$; at $y_0\le m$ take $q=e^{-d(y_0-m)}$ so the mode is $y_0$ and both modal slopes diverge. For constants strictly above both the lower cut and $m$, any path with $d\to\infty,\ qd\to\infty$ has $\limsup c\le m$, since eventually $q\ge 1/d$. An interval left of the observation beats that observation by an unbounded log difference. Finite-$d$, infinite-$q$ paths and diverging small-$d$ slopes also improve towards the lower cut. This proves the stationary comparison for every other constant, as well as for distinct data. Reciprocation proves the inverse Burr result, interchanging cuts and retaining the same contact penalty.

## Generalized beta with scale and one beta shape held

**Proved, held upper shape $q>0$.** Use $z=y-\log S\le0$, $\rho=pd>0$, and kernel $e^{\rho z}(1-e^{dz})^{q-1}$. For strictly interior parent observations, $q=1$ has the product criterion. For $q\ne1$, the complete excluded positive-height list is
$$
\begin{array}{ll}
d\to d_0\in(0,\infty),\ \rho\downarrow0 &: (1-e^{d_0z})^{q-1},\\
d\downarrow0,\ \rho\to r\ge0 &: e^{rz}(-z)^{q-1},\\
d\to\infty,\ \rho\to r\ge0 &: e^{rz}.
\end{array}
$$
Keep only integrable kernels. Each is integrable at the upper end of its log window, including zero. At lower infinite cut the first row is always excluded as nonintegrable, and the others require $r>0$. For proof of the second row, divide the power factor by $d^{q-1}$. For $q>1$, use $(1-e^{dz})/d\le -z$. For $q<1$, when $d\le1$ the quotient is at least $1-e^z$, giving a bounded factor away from the upper endpoint and an integrable singularity at it. For the third row, when $d\ge1$ the factor is bounded above by $1+(1-e^z)^{q-1}$. For the first row use the same local endpoint comparisons and Fatou at a divergent lower end. These bounds also apply to the varying-$\rho$ exponential tails. The listed kernels are all reachable by the indicated parameter limits.

If $\rho\to\infty$ and the effective upper cut is below the parent endpoint, the log derivative differs from $\rho$ by at most $|q-1|/|z_{\rm upper}|$ throughout the window, uniformly in $d$, by $e^s-1\ge s$. The height at the cut is of order $\rho$, and all observations strictly below it have exponential suppression. Thus the only divergence branch is a constant at that cut, with a weak upper atom. If the retained window reaches the parent endpoint and all observations are strictly below it, choose an interval between the maximum observation and that endpoint. On the compact region covering observations and this interval the derivative is $\rho+O(1)$, so the density at each observation tends to zero. This gives the root comparison. At parent contact use the inside density rule.

**Proved, held lower shape $p>0$.** With $q,d$ free and all observations strictly below $S$, the entire excluded positive-height list for distinct data is
$$
\begin{array}{ll}
d\to d_0\in(0,\infty),\ q\downarrow0 &: e^{pd_0z}(1-e^{d_0z})^{-1},\\
d\downarrow0,\ q\to q_0\ge0 &: (-z)^{q_0-1}.
\end{array}
$$
The first row requires the upper cut below the parent endpoint. The second requires a finite lower cut and if the upper cut equals the parent endpoint also $q_0>0$. The same domination as for held $q$, using a fixed exponent strictly greater than $-1$ when the singular end is retained and $q_0>0$, proves integral convergence. At a divergent integral use Fatou. At $q\to\infty$ with $d$ bounded, center the log kernel at a fixed interior point and divide by $q-1$. At $d_0>0$ the limiting leading function is $\log(1-e^{d_0z})$ up to a constant, and at $d_0=0$ it is $\log(-z)$ up to a constant. Both strictly decrease. The log-concavity bound applies since $q>1$, by the negative derivative of the log slope. At $d\to\infty,\ q>1$ the log derivative is $pd-(q-1)d/(e^{-dz}-1)$. Relative to its zero at the log-mode $c<0$, the ratio of the second term to $pd$ is at most $e^{-d\delta}$ at least $\delta$ left of $c$, and at least $e^{d\delta}$ at least $\delta$ right of $c$. This follows from the ratio $(e^{-dc}-1)/(e^{-dz}-1)$. The separated-observation log-concavity bound rejects these paths. For $q\le1$, below any fixed cut $z_{\rm upper}<0$ the correction to $pd$ is bounded by $1/|z_{\rm upper}|$; if the window reaches the parent endpoint a strictly better interval right of the observations gives the same pointwise suppression. Thus large power cannot compete for distinct data. Every constant strictly below the parent endpoint diverges by choosing $q=1+p(e^{-dz_0}-1)$ so the log kernel has its mode at its common observation $z_0<0$ and using the separated-slope bounds to concentrate, including at a known cut. At parent endpoint contact free $q$ allows singularity. This proves the comparison with the two regular faces.

## Generalized beta with free endpoint and held superunit upper shape

**Proved.** Fix $q>1$, let $h=\log S\ge y_{\max}$ and $\rho=pd$, and use
$$
w(y)=1_{\{y<h\}}e^{\rho y}(1-e^{d(y-h)})^{q-1}.
$$
A constant at the lower retained cut admits the width-collapse path for every mask. For the three remaining masks with $q$ held and at least one of $p,d$ free, every constant also admits an unbounded path elsewhere. If $p$ is free, take a fixed $d$, $p\to\infty$ and choose $S$ so the latent beta argument at the common observation is $1-1/p$. Its beta density there is asymptotic to $p e^{-1}/\Gamma(q)$, using $p^q B(p,q)\to\Gamma(q)$ by the substitution $s=p(1-z)$ and dominated convergence. The physical Jacobian at that observation tends to a positive constant. If $p$ is held and $d$ is free, choose $S$ so the log mode stays at the observation and take $d\to\infty$. The parent log density is $d$ times a fixed log-beta template (since $p,q$ are fixed), so the height diverges. Conditioning can only raise these parent heights. The parent laws concentrate at the observation by the same transformations. On a window cut at the observation, the beta-scaled limit when varying $p$, or the fixed template when varying $d$, gives a positive limiting probability on each side of the observation. Thus the conditioned laws also converge to the atom.

For distinct data the table gives the full list of positive-height escapes. Keep only integrable kernels, and only parameter limits compatible with the held masks.

| Regime | Kernel in $y$ |
| --- | --- |
| $d\to d_0\in(0,\infty)$, $\rho\to0,\ h\to h_0<\infty$ | $1_{\{y<h_0\}}(1-e^{d_0(y-h_0)})^{q-1}$ |
| $d\to d_0\in(0,\infty),\ \rho\to r\ge0,\ h\to\infty$ | $e^{ry}$ |
| $d\downarrow0,\ \rho\to r\ge0,\ h\to h_0<\infty$ | $1_{\{y<h_0\}}e^{r y}(h_0-y)^{q-1}$ |
| $d\downarrow0,\ \rho\to r\ge0,\ h\to\infty$ | $e^{ry}$ |
| $d\to\infty,\ \rho\to r\ge0,\ h\to h_0$ | $1_{\{y\le h_0\}}e^{ry}$, with full trace and $h_0=\infty$ allowed |

For finite $h_0$ the relevant part of the log window ends there or at the known upper cut, whichever is lower. A nonzero integral requires positive width. The lower infinite cut requires $r>0$ for all power or log-distance rows and rules out the first row. A pure power with $r\ge0$ cannot normalize at an infinite upper end. Fixed $p$ forces $\rho\to0$ for small $d$ and $\rho\to\infty$ for large $d$. Fixed $d$ retains only the first two rows. At finite $d_0,h_0,r>0$, the model is feasible when $h_0>y_{\max}$. At $h_0=y_{\max}$ a contact observation gives zero in that finite regime. The first row and the log-distance row also have zero contact value, while the last row uses the approachable full trace.

For proof of exhaustion, if $\rho\to\infty$, choose an interval strictly between the smallest and largest observations, separated positively from $y_{\max}$. On the log segment from the smallest observation through that interval, the log slope differs from $\rho$ by at most $(q-1)/(y_{\max}-y)$; this follows from $e^s-1\ge s$. There is strict improvement by a diverging amount. Log concavity for $q>1$ gives the distinct-data bound even if a high density develops elsewhere. Otherwise take subsequential limits of $d,\rho,h$. At finite $d$, the pointwise limits are immediate. For small $d$ and finite $h_0$, divide the upper factor by $d^{q-1}$. The bound $(1-e^{-d(h-y)})/d\le h-y$ gives domination on finite intervals and on the lower unbounded side when $r>0$, with an exponential times polynomial bound. When $h\to\infty$, the log slope correction tends to zero on every compact set since it is bounded by $(q-1)/(h-y)$. Centering at a finite reference point gives the pure power. For large $d$ the pointwise limit below the wall is $e^{r y}$ and the density vanishes above it. The factor is bounded by 1 and supports are uniformly bounded above for finite $h_0$. At the wall the factor is at most 1, with trace one approachable via $h=h_0+d^{-1/2}$. In the other integrable cases uniform domination follows from a finite upper endpoint or retained support bounded above, and an inward slope at a sufficiently low point by log concavity when the lower end is uncut and $r>0$. In nonintegrable cases use Fatou after the indicated scaling, since sample kernels remain bounded. Every listed positive height follows by fixing the eligible limiting quantities or taking the indicated parameter limits, with $\rho=pd$ respecting the holds. For distinct data the usual stationary comparison therefore gives attainment, with equality allowed. Use the preceding subunit and unit shape proofs on their respective branches.

## Shifted GB2 with only the threshold free

**Proved.** With all three shapes and scale held, let $h$ be the freely varying threshold. If $dp<1$, or if all observations equal the finite upper cut, use the spike or width argument. Otherwise $dp\ge1,\ m_x<b$. On $h\le m_x$ compare the feasible score roots, the join $h=a$ if feasible and finite, and $h=m_x$ when $dp=1$. At the last point the value is zero if $dp>1$, by positive-width retained mass and the inside trace. The only positive excluded height is ordinary uniform when both imposed cuts are finite. Indeed as $h\to-\infty$, on every compact interval in $x$ the ratio of the kernels at $x$ and a fixed reference point tends uniformly to one, by the fixed-scale Pareto tail $ (x-h)^{-dq-1}$. This gives uniform on a bounded window, and zero likelihood on any unbounded window by Fatou. The proof of stationary comparison now applies on the two threshold branches. General uniqueness of winning roots is open.

## Shifted GB2 with scale and both low-end factors held

**Proved.** Let $S,p,d$ be held and $q,h$ free. Use the threshold spike when $dp<1$ and the upper-cut width collapse whenever the sample is all at that cut. For $dp\ge1$, constants at any compatible $c$ can diverge with $q\to\infty,\ h=c-Sq^{-1/d}$. In the variable $V=q^{1/d}(X-h)/S$ the parent kernel tends to $V^{dp-1}e^{-V^d}$ on $(0,\infty)$, with the gamma domination proved for fixed power. Its positive limiting density at 1 implies an unbounded physical sample density. The limit has positive mass to each side of 1, which also proves the conditioned weak atom at $c$ on a cut containing $c$.

For distinct data $m_x<b$. At finite $h\le m_x$ write the physical kernel as $H(u)e^{-qT(u)}$ for $u=x-h\ge0$, where
$$
H(u)=u^{dp-1}(1+(u/S)^d)^{-p},\qquad T(u)=\log(1+(u/S)^d).
$$
With bounded $q$, continuous numerator limits apply also at $u=0$ with the inside convention. On an infinite upper end the tail is integrable iff the limiting $q>0$. On bounded upper windows the only excluded finite-coordinate height is $q=0$, with kernel $H$ and the limiting threshold $h\le m_x$. The boundary $h=m_x$ is feasible for $q>0$, with zero joint value if $dp>1$.

For bounded $h$ and $q\to\infty$, the retained excess lower end is $\ell=\max(a-h,0)$, taking $\ell=0$ when $a=-\infty$. The retained width stays bounded below because the window has positive width and $m_x<b$. The integral over $[\ell+\delta,\ell+2\delta]$ with $\delta=q^{-\max(1,1/d)}$ is at least a positive constant times $\delta^{dp}e^{-qT(\ell)}$. Indeed $H$ there is bounded below by a constant times $\delta^{dp-1}$, and $T$ is Hölder with exponent $\min(d,1)$ on the bounded excess interval. At all observations $H$ is bounded and $T(u_i)\ge T(\ell)$, with a uniform positive gap at the largest observation. Hence their joint likelihood vanishes.

For $h=-D\to-\infty$, $H(x+D)/H(x_0+D)\to1$ and $T'(x+D)\sim d/D$ uniformly on compact $x$-intervals. Thus if $dq/D\to\nu\ge0$ the limit is a decreasing exponential of rate $\nu$, including uniform at zero, on the imposed window. A finite lower cut is required for strictly positive rate, and both cuts finite for zero. On a positive-rate lower-only window, center at $a$, use $T'(u)\ge d/(2u)$ for $u\ge D+a$, and use $H(u)/H(D+a)\le C$ there. The centered kernel is then bounded by $C(1+(x-a)/(D+a))^{-dq/2}$. For all sufficiently large $D$, this is bounded by a fixed integrable reciprocal power by the same monotone-power inequality used on the beta-prime gamma face. Divergent normalizers give zero by Fatou. If $q/D\to\infty$ and $a>-\infty$, integrate relative to the kernel at $a$ over length $(D+a)/q$, using $T'(u)\le d/u$ and $H$ ratios bounded below there. This bounds each observation's height by a constant times $q/(D+a)$, with exponential suppression at any observation strictly above $a$. If $a=-\infty$, a fixed interval to the left of the sample beats every observation by a diverging difference since the centered log-kernel divided by $q/D$ tends to a strictly decreasing linear function. Thus no distinct-data height survives at diverging rate.

The regular limits are all approachable, with exact likelihood given by the kernel formula. For existence compare those heights with the feasible $(q,h)$ stationary points, the feasible fixed-cut join $h=a$ with the remaining $q$ score, and the closed support contact $h=m_x$ when $dp=1$ with its remaining $q$ score. No root-count bound is used.

## Shifted GB2 with the shapes held

**Proved.** Let $p,d,q$ be held, with threshold $h$ and scale $S$ free. Use the subunit spike if $dp<1$. For $dp\ge1$ and a constant at $c$, take $h=c-Sr$ with fixed $r>0$, $S\downarrow0$. Its parent density diverges as $g(r)/S$, where $g$ is the unit-scale GB2 density. The retained probability is bounded below since the unit-scale law has positive mass on both sides of $r$, and the conditioned laws converge weakly to the atom at $c$. This covers closed cuts as well. 

For distinct data put $m_x=\min x_i$, $j=\#\{x_i=m_x\}$, $\beta=dq$, and $E=\beta(n-j)-j$. Write the unit-scale density and survivor as
$$
g(v)=\frac{d\,v^{dp-1}}{B(p,q)(1+v^d)^{p+q}},\qquad
T(v)=\int_v^\infty g(w)\,dw,
$$
so $g(v)\sim C v^{-\beta-1}$, $T(v)\sim(C/\beta)v^{-\beta}$ with $C=d/B(p,q)$. If $E<0$, $S\downarrow0,\ h=m_x-Sr$ for fixed $r>0$ gives unbounded likelihood with an atom at $m_x$. If $E\ge0$, the full list of positive excluded heights is as follows. A kernel in this list is normalized on the imposed retained window by the likelihood formula.

- $h\to-\infty$ gives uniform when both cuts are finite. The physical log derivative is bounded by a fixed constant divided by $x-h$, independently of $S$. Hence centered ratios tend to one on compacts, with a divergent normalizer on either unbounded side.
- $h\to h_0\le m_x$ finite, $S\to\infty$, gives the increasing kernel $1_{\{x\ge h_0\}}(x-h_0)^{dp-1}$ on a finite upper window. At $dp=1$ use full contact. At $dp>1$ contact gives zero. These statements follow from bounded convergence on the bounded effective support, or Fatou on an infinite upper interval.
- $S\downarrow0,\ h\to h_0<m_x$ gives $(x-h_0)^{-\beta-1}$ if $h_0<a$ with finite lower cut $a$. If the pole lies at or above the lower cut, the centered normalizer diverges by Fatou while the observations remain a positive distance from it. If $h_0<a$, domination on an unbounded upper side is by this same reciprocal power, uniformly up to a constant for $h$ near $h_0$.
- $S\downarrow0,\ h\to m_x$ contributes a positive envelope only at $E=0$. Put
$$
D_0=C^{n-j}\prod_{x_i>m_x}(x_i-m_x)^{-\beta-1}.
$$
If $a<m_x$, the height is $D_0\max_{r\ge0}g(r)^j$. If $a=m_x$, it is $D_0\max_{r\in[0,\infty]}g(r)^j/T(r)^n$, with the ratio at infinity $\beta^n C^{j-n}$. In both cases $g(0)$ uses the inside value. The corresponding weak limit is an atom at $m_x$.

For the last branch let $\Delta=m_x-h\ge0$ and $r=\Delta/S$, possibly varying. All nonminimum density factors are asymptotic to $C S^\beta(x_i-m_x)^{-\beta-1}$. If $a<m_x$, the retained probability tends to 1. If $a=m_x$, it is asymptotic to $T(r)$, since the upper survival is $O(S^\beta)$ when the upper cut is finite and $T(r)$ is either bounded below or of order $r^{-\beta}$, with $Sr=\Delta\to0$. Thus the joint likelihood is asymptotic to $D_0 S^E g(r)^j$ divided by 1 or $T(r)^n$, with zero trace treated exactly when $r=0$ and $dp>1$. At $r\to\infty$, $g(r)^j/T(r)^n\sim\beta^n C^{j-n}r^E$, so for $E>0$ the likelihood vanishes also on that end, as $(Sr)^E\to0$. For $E=0$ the functions extend continuously to the compactified $r$ interval and every positive value is realized as a limit. Total retained probability tends to one if $a<m_x$; if $a=m_x$, the ratio of survival at a positive distance above $m_x$ to $T(r)$ vanishes by the same tail calculation. This proves the asserted weak atom. At finite positive $S$ and finite $h$, the objective is continuous up to feasible contact, including the inside trace. Thus the listed comparison for $E\ge0$ uses stationary points, the feasible fixed-cut join, and support contact when $dp=1$. The closed threshold range and shape holds give no general cardinality claim; the shifted Lomax sample is a nonunique case.

## Generalized beta with scale held and all shapes free

**Proved.** At an observation equal to the closed parent endpoint, free $q<1$ gives a singular likelihood. With all observations strictly below the endpoint, every constant also permits unbounded likelihood, by the fixed-$p$ modal alignment above. For distinct interior data write $m=\log S,\ z=y-m$, $\rho=pd$ and $\gamma=q-1>-1$. The log-coordinate kernel is $e^{\rho z}(1-e^{dz})^{\gamma}$ on $z<0$. The following excluded heights exhaust all positive-height escapes, allowing upper envelopes at ties:

| Path | Kernel in $z$ |
| --- | --- |
| $d\to d_0\in(0,\infty)$, bounded $\rho,\gamma$, at least one shape zero in the limit | $e^{r z}(1-e^{d_0 z})^{q_0-1}$, $r,q_0\ge0$ |
| $d\downarrow0$, bounded $\rho,\gamma$ | $e^{r z}(-z)^{q_0-1}$, $r,q_0\ge0$ |
| $d\to\infty$, bounded $\rho$ | $1_{\{z\le h\}}e^{r z}$, $r\ge0,\ h\le0$, full trace |

For the first two rows require $r>0$ at an infinite lower cut and $q_0>0$ at parent upper contact of the window. For the last row require positive retained width and $r>0$ at an infinite lower cut. The wall $h=0$ gives the pure power on the whole effective interval. Every eligible positive likelihood is approachable.

At finite $d_0$ or $d\downarrow0$, bounded $\rho,\gamma$ give the preceding held-shape limits. In the small-$d$ case divide by $d^\gamma$ before taking limits. The same dominance works with bounded varying exponents, using a fixed exponent strictly larger than $-1$ at a retained parent endpoint and an exponential times polynomial at an unbounded lower end. Fatou rejects divergent normalizers. If $\max(\rho,\gamma)\to\infty$ with $\gamma\ge0$ and bounded $d$, center at a fixed $z_0<0$, then divide by that maximum. The leading kernel is $s(z-z_0)+t[\log(1-e^{d_0 z})-\log(1-e^{d_0 z_0})]$ for $d_0>0$, or has $t[\log(-z)-\log(-z_0)]$ for $d_0=0$. At least one of $s,t$ is positive. The leading function is strictly concave if $t>0$ and strictly monotone otherwise, so the log-concavity bound yields zero. For $\gamma<0$, any diverging $\rho$ gives zero via the slope comparison in the held-upper-shape proof, uniformly in $\gamma\in(-1,0)$. If the upper cut lies below the parent endpoint, the correction is uniformly bounded on the whole window and the maximum height is $O(\rho)$. If the window reaches the parent endpoint, a fixed interval above the maximum observation suppresses every observation without needing a density bound at the endpoint.

At $d\to\infty$ and $\rho\to\infty$ with $\gamma>0$, the modal-slope separation from the fixed-$p$ proof applies with $\rho$ in place of $pd$, by the same derivative ratio. This gives zero for distinct samples. If $\rho$ is bounded and $\gamma>0$, set $h_d=-\log\gamma/d$. On compact intervals strictly below $z=0$, the penalty $-\gamma\log(1-e^{dz})$ and its derivative are asymptotic to $e^{d(z-h_d)}$ and $d e^{d(z-h_d)}$, respectively. A finite limiting wall strictly below zero therefore has the hard-wall behavior. At the wall a contact observation has height at most the full power trace, achieved by placing $h_d=h+d^{-1/2}$. When the wall tends to or above zero, the penalty vanishes locally, with no sample at zero. Integrable left tails use $\rho$ bounded below if the lower end is infinite, and the remaining tail lies in a finite log interval. If the wall tends to or below the lower cut, use the diverging slope penalty to compare the larger observation to a better fixed interval below it, then use the distinct-data bound. If the lower cut is infinite and the limit slope is zero, Fatou rejects the nonintegrable limit with nonzero width. Finally, for $\gamma\le0$ and bounded $\rho$, the kernel converges pointwise at strictly negative $z$ to the pure power. With $q\to0$ its mass near zero can be unbounded after centering. Fatou shows that any such extra mass only lowers the height, and the power height is approachable at $q=1$. This proves the comparison including subunit-shape paths, without equating a pointwise kernel with a normalized limit in the possible presence of a near-endpoint spike.

## Shifted GB2 with tail shape, scale and threshold free

**Proved.** Suppose $p,d$ are held and $q,S,h$ are free. At $dp<1$ use the endpoint spike. At $dp\ge1$, for any distinct sample choose a fixed $q>0$ so that $dq(n-j)-j<0$, with $j$ the multiplicity of the sample minimum. Then the fixed-shape small-scale path above applies with $h=m_x-Sr$ for any fixed $r>0$, giving unbounded likelihood and a weak atom at the sample minimum. Constants use the same shifted and shrinking scale with any held tail shape along the path. This settles nonattainment using one witness per sample. Other divergent paths can have different weak limits or lose retained mass on unbounded windows.

## Score equations for the comparisons

**Proved.** In any fixed observation coordinate with fixed integration ends, the equations for a candidate are
$$
\frac1n\sum_i \partial_\theta \log w(z_i)
 =\frac{\int_J w(z)\,\partial_\theta\log w(z)\,dz}{\int_J w(z)\,dz}
$$
for each free coordinate $\theta$. These are explicit equations in the sample and the listed kernel. For GB2 in log coordinates put $F=\log(1+e^{d(y-m)})$ and $P=(1+e^{-d(y-m)})^{-1}$. The scores to match are $d(y-m)-F,-F,(y-m)[p-(p+q)P],d[(p+q)P-p]$, respectively for $p,q,d,m$. In paralogis add the $q$ derivative to the $d$ derivative because $q=d$. In nonzero signed Prentice coordinates use the chain rule through $d,k,m$ on the specified holds. For transformed gamma in $(\rho,v,d)$ the functions are $y,-e^{dy},-vy e^{dy}$. When $k=\rho/d$ or $m=-\log v/d$ is held, differentiate along that constraint. Reflection and reciprocal changes contribute only a parameter-independent sample factor.

For generalized beta use $z=y-\log S$ and differentiate $\rho z+(q-1)\log(1-e^{dz})$. In a moving-support case use the original log coordinate $y$ for the expectation and include a derivative of the integral endpoint. For the free-$S$, $q>1$ case the kernel at that endpoint is zero and the remaining derivative is integrable, with a possible $(h-y)^{q-2}$ factor at $h=\log S$. For shifted GB2 with $dp\ge1$, use the physical kernel and differentiate its integrated excess interval, adding $w(U)\partial_\theta U-w(L)\partial_\theta L$ for moving finite ends $L,U$. The formulas on each open branch follow by the product and integral derivative rules. At $h=a$ include that branch's lower-dimensional candidates directly, and at an eligible $h=m_x$ apply only the tangential equations with the specified finite observation trace. Equivalently the score on each branch is obtained by differentiating the incomplete beta normalizer. This supplies the stationary sample conditions without substituting an unconditional score or treating a closed endpoint as if it were interior.

**Proved, exact excluded winner.** For GB2 hold $S=d=1$, take the known window $[1/3,3]$ and sample $(1/3,3)$. The latent beta window is $[1/4,3/4]$ with the same two endpoints observed. On the $p=q=0$ corner the density is symmetric in $z$ and $1-z$. The function $\log[z(1-z)]$ is strictly greater in the interior than its common value at the endpoints. Hence by symmetry both inward scores at the corner are strictly negative. Strict concavity proves that the log-uniform on the physical window wins outside the positive-shape domain, with joint height $1/(\log 9)^2$.
## Family and held-mask results

### betapr free truncate:known

**Proved.** Use $d=1,p=shape1,q=shape2$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Neither beta shape is held. Every constant sample collapses at its common value. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### betapr scale truncate:known

**Proved.** Use $z=x^d/(S^d+x^d)$ at the held scale and power. Here $d=1$. Use both beta-shape scores and the edge and corner inward tests. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### betapr scale+shape1 truncate:known

**Proved.** Use $z=x^d/(S^d+x^d)$ at the held scale and power. Here $d=1$. Use the scalar positive tilt on $log(1-z)$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### betapr scale+shape1+shape2 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### betapr scale+shape2 truncate:known

**Proved.** Use $z=x^d/(S^d+x^d)$ at the held scale and power. Here $d=1$. Use the scalar positive tilt on $log z$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### betapr shape1 truncate:known

**Proved.** Use $d=1,p=shape1,q=shape2$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold the lower beta shape p. Constants at a positive lower cut collapse. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### betapr shape1+shape2 truncate:known

**Proved.** Use $d=1,p=shape1,q=shape2$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold both beta shapes. There is no divergence branch. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### betapr shape2 truncate:known

**Proved.** Use $d=1,p=shape1,q=shape2$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold the upper beta shape q. Constants at a finite upper cut collapse. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### burr free truncate:known

**Proved.** Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Use p=1 and respect the held scale when specified. Every constant sample collapses by free modal alignment.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### burr scale truncate:known

**Proved.** Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Use p=1 and respect the held scale when specified. Constants at the lower log cut or at or below the held log scale collapse.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### burr scale+shape1 truncate:known

**Proved.** Use the original positive window. Apply the fixed-q Burr comparison in the [varying-power proofs](#varying-the-power). Constants at the projected held log scale collapse. The only regular excluded height is log-uniform on a two-cut log window. Either unbounded log side gives zero.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### burr scale+shape1+shape2 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### burr scale+shape2 truncate:known

**Proved.** Use the original positive window. Use $z=x^d/(S^d+x^d)$ at the held scale and power. Set $p=1$. Use the scalar positive tilt on $log(1-z)$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### burr shape1 truncate:known

**Proved.** Use the original positive window. Apply the fixed-q Burr comparison in the [varying-power proofs](#varying-the-power). Every constant sample collapses at its common value. The only regular excluded height is log-uniform on a two-cut log window. Either unbounded log side gives zero.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### burr shape1+shape2 truncate:known

**Proved.** Use the original positive window. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold $p=1,q=shape1,d=shape2$. There is no divergence branch. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### burr shape2 truncate:known

**Proved.** Use the original positive window. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold $d=shape2,p=1$. Constants at the transformed positive lower cut collapse. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gb2 free truncate:known

**Proved.** Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Keep the indicated beta shape fixed and discard every incompatible boundary. Every constant sample collapses by free modal alignment.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gb2 scale+shape1+shape2 truncate:known

**Proved.** Use $q=shape1,d=shape2,p=shape3$. Use $z=x^d/(S^d+x^d)$ at the held scale and power. Use the scalar positive tilt on $log z$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### gb2 scale+shape2 truncate:known

**Proved.** Use $q=shape1,d=shape2,p=shape3$. Use $z=x^d/(S^d+x^d)$ at the held scale and power. Use both beta-shape scores and the edge and corner inward tests. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### gb2 scale+shape2+shape3 truncate:known

**Proved.** Use $q=shape1,d=shape2,p=shape3$. Use $z=x^d/(S^d+x^d)$ at the held scale and power. Use the scalar positive tilt on $log(1-z)$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### gb2 shape1 truncate:known

**Proved.** Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Keep the indicated beta shape fixed and discard every incompatible boundary. Every constant sample collapses by free modal alignment.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gb2 shape1+shape3 truncate:known

**Proved.** Use $q=shape1,d=shape2,p=shape3$. The fixed-shape large- and small-power arguments in [varying-power proofs](#varying-the-power) extend to any fixed p,q>0. The mode is $m+\log(p/q)/d$, the small-power log slope is bounded by $d\max(p,q)$, and the large-power slope bounds away from the mode hold with limiting magnitudes $pd,qd$. Consequently the only regular excluded height is log-uniform on a bounded log window.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gb2 shape3 truncate:known

**Proved.** Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Keep the indicated beta shape fixed and discard every incompatible boundary. Every constant sample collapses by free modal alignment.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta free truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions). Free scale and q allow a subunit upper shape and an endpoint spike at the sample maximum. All samples at the lower cut instead allow width collapse.

### genbeta scale truncate:known

**Proved.** Use the [full held-scale comparison](#generalized-beta-with-scale-held-and-all-shapes-free) for distinct interior observations. Constants or observations at the parent endpoint have unbounded likelihood.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta scale+shape1 truncate:known

**Proved.** Use the held lower shape proof in [held-scale generalized beta comparisons](#generalized-beta-with-scale-and-one-beta-shape-held).

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta scale+shape1+shape2+shape3 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### genbeta scale+shape1+shape3 truncate:known

**Proved.** Use $z=(x/S)^d, p=shape1,q=shape2,d=shape3$. Use the beta moment tests with upper shape free for strictly interior observations. At closed parent contact use [moving-support proofs](#moving-support-and-endpoint-conventions). If the upper shape is held at one the product test applies also at contact.

### genbeta scale+shape2 truncate:known

**Proved.** Use the held upper shape proof in [held-scale generalized beta comparisons](#generalized-beta-with-scale-and-one-beta-shape-held) and the unit-shape and endpoint rules in [moving-support proofs](#moving-support-and-endpoint-conventions).

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta scale+shape2+shape3 truncate:known

**Proved.** Use $z=(x/S)^d, p=shape1,q=shape2,d=shape3$. Use the beta moment tests with lower shape free for strictly interior observations. At closed parent contact use [moving-support proofs](#moving-support-and-endpoint-conventions). If the upper shape is held at one the product test applies also at contact.

### genbeta scale+shape3 truncate:known

**Proved.** Use $z=(x/S)^d, p=shape1,q=shape2,d=shape3$. Use the beta moment tests with both shapes free for strictly interior observations. At closed parent contact use [moving-support proofs](#moving-support-and-endpoint-conventions). If the upper shape is held at one the product test applies also at contact.

### genbeta shape1 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions). Free scale and q allow a subunit upper shape and an endpoint spike at the sample maximum. All samples at the lower cut instead allow width collapse.

### genbeta shape1+shape2 truncate:known

**Proved.** Use the [held superunit upper shape comparison](#generalized-beta-with-free-endpoint-and-held-superunit-upper-shape) for q above one and distinct observations. For all other cases consult the width, subunit, and unit rules there and in the moving-support proof.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta shape1+shape2+shape3 truncate:known

**Proved.** Use the free endpoint with all shapes held proof in [moving-support proofs](#moving-support-and-endpoint-conventions).

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta shape1+shape3 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions). Free scale and q allow a subunit upper shape and an endpoint spike at the sample maximum. All samples at the lower cut instead allow width collapse.

### genbeta shape2 truncate:known

**Proved.** Use the [held superunit upper shape comparison](#generalized-beta-with-free-endpoint-and-held-superunit-upper-shape) for q above one and distinct observations. For all other cases consult the width, subunit, and unit rules there and in the moving-support proof.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta shape2+shape3 truncate:known

**Proved.** Use the [held superunit upper shape comparison](#generalized-beta-with-free-endpoint-and-held-superunit-upper-shape) for q above one and distinct observations. For all other cases consult the width, subunit, and unit rules there and in the moving-support proof.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### genbeta shape3 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions). Free scale and q allow a subunit upper shape and an endpoint spike at the sample maximum. All samples at the lower cut instead allow width collapse.

### gengamma free truncate:known

**Proved.** Apply [all-free transformed gamma](#transformed-gamma-with-all-coefficients-free) in both signs and include the attained normal seam via the [normal truncation tests](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown) in log coordinates. Constants collapse.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gengamma Q truncate:known

**Proved.** For nonzero Q use the held-shape signed comparison in [all-free transformed gamma](#transformed-gamma-with-all-coefficients-free). At Q=0 apply the [normal truncation tests](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown). Constants collapse at their common value.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gengamma Q+mu truncate:known

**Proved.** For nonzero Q use the fixed template about the held log-mode in [all-free transformed gamma](#transformed-gamma-with-all-coefficients-free). At Q=0 use the held-normal-mean test in the [normal truncation tests](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown). Constants at the projection of mu collapse.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gengamma Q+sigma truncate:known

**Proved.** For nonzero Q use $u=x^{Q/\sigma}$ and the scalar positive tilt on $-u$ with fixed shape $Q^{-2}$. At $Q=0$ use the held-normal-SD location test in the [normal truncation tests](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown).

### gengamma mu truncate:known

**Proved.** Use the signed fixed-mode analysis in [held Prentice comparisons](#prentice-generalized-gamma-with-one-location-or-scale-hold) and the held-normal-mean seam. The candidate bound includes log-uniform and the permitted power at the wall.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gengamma mu+sigma truncate:known

**Proved.** Use the two signed log-uniform wall limits in [all-free transformed gamma](#transformed-gamma-with-all-coefficients-free) and include $Q=0$ as a feasible candidate.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### gengamma sigma truncate:known

**Proved.** Use the signed fixed-scale analysis in [held Prentice comparisons](#prentice-generalized-gamma-with-one-location-or-scale-hold) and the held-normal-SD seam. The excluded list has finite-power signed tilts and arbitrary log-uniform cutoffs subject to integrability.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### trgamma free truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the full list in [all-free transformed gamma](#transformed-gamma-with-all-coefficients-free). Constants collapse at their common value.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### trgamma scale truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the held-scale escape list in [varying-power proofs](#varying-the-power), including the exact wall trace. Constants diverge at or above the held log scale, or at the finite upper cut, in the forward coordinate.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### trgamma scale+shape1 truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the held-shape held-scale comparison in [varying-power proofs](#varying-the-power). Constants at the projected held log scale collapse.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### trgamma scale+shape1+shape2 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### trgamma scale+shape2 truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the scalar tilt on log u with fixed rate. See [two-shape tests](#likelihood-formulas-and-comparison-rules).

### trgamma shape1 truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the fixed-gamma-shape comparison in [varying-power proofs](#varying-the-power). Constants collapse at their common value.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### trgamma shape1+shape2 truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the scalar positive tilt on -u. See [two-shape tests](#likelihood-formulas-and-comparison-rules).

### trgamma shape2 truncate:known

**Proved.** Use $u=x^d$, $y=\log x$ in the forward kernel. Use the two-coordinate gamma test. See [two-shape tests](#likelihood-formulas-and-comparison-rules).

### invtrgamma free truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the full list in [all-free transformed gamma](#transformed-gamma-with-all-coefficients-free). Constants collapse at their common value.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invtrgamma scale truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the held-scale escape list in [varying-power proofs](#varying-the-power), including the exact wall trace. Constants diverge at or above the held log scale, or at the finite upper cut, in the forward coordinate.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invtrgamma scale+shape1 truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the held-shape held-scale comparison in [varying-power proofs](#varying-the-power). Constants at the projected held log scale collapse.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invtrgamma scale+shape1+shape2 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### invtrgamma scale+shape2 truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the scalar tilt on log u with fixed rate. See [two-shape tests](#likelihood-formulas-and-comparison-rules).

### invtrgamma shape1 truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the fixed-gamma-shape comparison in [varying-power proofs](#varying-the-power). Constants collapse at their common value.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invtrgamma shape1+shape2 truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the scalar positive tilt on -u. See [two-shape tests](#likelihood-formulas-and-comparison-rules).

### invtrgamma shape2 truncate:known

**Proved.** Use $u=x^{-d}$, $y=-\log x$, and reciprocal scale in the forward kernel. Use the two-coordinate gamma test. See [two-shape tests](#likelihood-formulas-and-comparison-rules).

### invburr free truncate:known

**Proved.** First reciprocate observations and scale. Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Use p=1 and respect the held scale when specified. Every constant sample collapses by free modal alignment.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invburr scale truncate:known

**Proved.** First reciprocate observations and scale. Apply the [varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries). Use p=1 and respect the held scale when specified. Constants at the lower log cut or at or below the held log scale collapse.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invburr scale+shape1 truncate:known

**Proved.** Use reciprocal observations $1/x$ and scale $1/S$ before applying Burr. All lower and upper cuts below refer to that transformed window. Apply the fixed-q Burr comparison in the [varying-power proofs](#varying-the-power). Constants at the projected held log scale collapse. The only regular excluded height is log-uniform on a two-cut log window. Either unbounded log side gives zero.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invburr scale+shape1+shape2 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### invburr scale+shape2 truncate:known

**Proved.** Use reciprocal observations $1/x$ and scale $1/S$ before applying Burr. All lower and upper cuts below refer to that transformed window. Use $z=x^d/(S^d+x^d)$ at the held scale and power. Set $p=1$. Use the scalar positive tilt on $log(1-z)$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### invburr shape1 truncate:known

**Proved.** Use reciprocal observations $1/x$ and scale $1/S$ before applying Burr. All lower and upper cuts below refer to that transformed window. Apply the fixed-q Burr comparison in the [varying-power proofs](#varying-the-power). Every constant sample collapses at its common value. The only regular excluded height is log-uniform on a two-cut log window. Either unbounded log side gives zero.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invburr shape1+shape2 truncate:known

**Proved.** Use reciprocal observations $1/x$ and scale $1/S$ before applying Burr. All lower and upper cuts below refer to that transformed window. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold $p=1,q=shape1,d=shape2$. There is no divergence branch. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invburr shape2 truncate:known

**Proved.** Use reciprocal observations $1/x$ and scale $1/S$ before applying Burr. All lower and upper cuts below refer to that transformed window. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold $d=shape2,p=1$. Constants at the transformed positive lower cut collapse. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invparalogis free truncate:known

**Proved.** First reciprocate observations and scale. Apply the tied $q=d$ proof in [varying-power proofs](#varying-the-power). All constants collapse by free modal alignment. The sole regular excluded height is log-uniform on a bounded log window.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invparalogis scale truncate:known

**Proved.** First reciprocate observations and scale. Apply the tied $q=d$ proof in [varying-power proofs](#varying-the-power). Constants at the projected held log scale collapse, except at the held log scale when the retained log interval extends to its left. The sole regular excluded height is log-uniform on a bounded log window.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invparalogis scale+shape truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### invparalogis shape truncate:known

**Proved.** First reciprocate observations and scale. Set $d=q=shape,p=1$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). The two beta shapes are fixed. There is no divergence branch. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### paralogis free truncate:known

**Proved.** Apply the tied $q=d$ proof in [varying-power proofs](#varying-the-power). All constants collapse by free modal alignment. The sole regular excluded height is log-uniform on a bounded log window.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### paralogis scale truncate:known

**Proved.** Apply the tied $q=d$ proof in [varying-power proofs](#varying-the-power). Constants at the projected held log scale collapse, except at the held log scale when the retained log interval extends to its left. The sole regular excluded height is log-uniform on a bounded log window.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### paralogis scale+shape truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### paralogis shape truncate:known

**Proved.** Set $d=q=shape,p=1$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). The two beta shapes are fixed. There is no divergence branch. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invpareto free truncate:known

**Proved.** Reciprocate $x$ and the scale. The resulting Lomax has $p=d=1,q=shape$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Free the upper beta shape in the reciprocal coordinate. Constants at the transformed positive lower cut collapse. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### invpareto scale truncate:known

**Proved.** Reciprocate $x$ and the scale. The resulting Lomax has $p=d=1,q=shape$. Use $z=x^d/(S^d+x^d)$ at the held scale and power. Use the scalar positive tilt on $log(1-z)$. Apply the [two-shape tests](#likelihood-formulas-and-comparison-rules).

### invpareto scale+shape truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### invpareto shape truncate:known

**Proved.** Reciprocate $x$ and the scale. The resulting Lomax has $p=d=1,q=shape$. In the transformed coordinate use [fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes). Hold both beta shapes. There is no divergence branch. Outside divergence compare the scale and free-shape scores to the eligible face heights, with equality allowed.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto free truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto min truncate:known

**Proved.** For strictly positive excesses use [the GB2 comparison](#gb2-free-truncateknown) on the translated window. At compatible threshold contact at least one factor of dp can vary, permitting a subunit product and an infinite endpoint density.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto min+scale+shape1+shape2+shape3 truncate:known

**Proved.** Evaluate the supplied coefficients with the window likelihood formula. At closed parent contact use the inside density.

### fpareto min+scale+shape2 truncate:known

**Proved.** For strictly positive excesses use [the GB2 comparison](#gb2-scaleshape2-truncateknown) on the translated window. At compatible threshold contact at least one factor of dp can vary, permitting a subunit product and an infinite endpoint density.

### fpareto min+shape1 truncate:known

**Proved.** For strictly positive excesses use [the GB2 comparison](#gb2-shape1-truncateknown) on the translated window. At compatible threshold contact at least one factor of dp can vary, permitting a subunit product and an infinite endpoint density.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto min+shape1+shape3 truncate:known

**Proved.** For strictly positive excesses use [the GB2 comparison](#gb2-shape1shape3-truncateknown) on the translated window. At compatible threshold contact at least one factor of dp can vary, permitting a subunit product and an infinite endpoint density.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto min+shape3 truncate:known

**Proved.** For strictly positive excesses use [the GB2 comparison](#gb2-shape3-truncateknown) on the translated window. At compatible threshold contact at least one factor of dp can vary, permitting a subunit product and an infinite endpoint density.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto scale truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto scale+shape1 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto scale+shape1+shape2 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto scale+shape1+shape2+shape3 truncate:known

**Proved.** Apply the [threshold-only comparison](#shifted-gb2-with-only-the-threshold-free), retaining the feasible closed contact when the low-end exponent is zero.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto scale+shape1+shape3 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto scale+shape2 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto scale+shape2+shape3 truncate:known

**Proved.** Apply the [shifted GB2 comparison with scale held](#shifted-gb2-with-scale-and-both-low-end-factors-held). Use the feasible fixed-cut join and the feasible support contact in addition to the interior scores.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto scale+shape3 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto shape1 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto shape1+shape2 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto shape1+shape2+shape3 truncate:known

**Proved.** Use the [fixed-shape shifted GB2 comparison](#shifted-gb2-with-the-shapes-held), including the small-scale contact exponent and its exact critical envelope. The shifted Lomax specialization gives the explicit nonunique sample (1,2) on [1,infinity) at q=2 and p=d=1.

**Open.** Bound the number of winning roots or exhibit a sample with multiple winners.

### fpareto shape1+shape3 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto shape2 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

### fpareto shape2+shape3 truncate:known

**Proved.** Use the [free tail-shape comparison](#shifted-gb2-with-tail-shape-scale-and-threshold-free). A sufficiently small positive tail shape makes the small-scale contact exponent negative even when the fixed lower-end product is at least one.

### fpareto shape3 truncate:known

**Proved.** Apply [moving-support proofs](#moving-support-and-endpoint-conventions) to the free threshold. At least one factor of $dp$ is free, giving a subunit product. This gives a lower-endpoint spike if the sample minimum is below the upper cut and a width collapse at upper-cut equality.

## Summary

| Family | Masks | Full criterion for finite positive fits | Full cardinality claim |
| --- | ---: | ---: | ---: |
| betapr | 8 | 8 | 4 |
| burr | 8 | 8 | 2 |
| gb2 | 7 | 7 | 3 |
| genbeta | 15 | 15 | 8 |
| gengamma | 7 | 7 | 1 |
| trgamma | 8 | 8 | 4 |
| invtrgamma | 8 | 8 | 4 |
| invburr | 8 | 8 | 2 |
| invparalogis | 4 | 4 | 1 |
| paralogis | 4 | 4 | 1 |
| invpareto | 4 | 4 | 2 |
| fpareto | 22 | 22 | 15 |
| Total | 103 | 103 | 47 |

## Numerical checks

**Numerical.** [The reproducible script](2026-11-27-trunc-gb2-math-data/check_limits.py) computes conditional likelihood ratios at 60 and 90 decimal digits on the grid $n=16,64,256,1024$. The grid uses inverse scale or small power $1/n$, large power $n$, and moving wall offsets $n^{-1/2}$, as appropriate. The checked samples, windows, and held coefficients are specified in the script. Integrals use split quadrature near moving walls and the exact incomplete-gamma integral for the infinite lower-cut-window check. All 36 cross-precision differences are below $10^{-54}$. The [numeric table](2026-11-27-trunc-gb2-math-data/limit-checks.csv) records the ratio, its limiting target, and both errors. The grid checks examples of the gamma and exponential faces, one-sided powers, held and mobile wall traces, the two-piece power, the generalized-beta log-distance face, and the critical shifted GB2 atom envelope. These are checks of the formulas. Attainment and cardinality grades in [the candidate atlas rows](2026-11-27-trunc-gb2-math-data/proofs.csv) are based on the analytic arguments.

## What this changes for the fitter

The scalar and two-coordinate rules give routes with monotone moment inversion or a strictly concave solve and exact inward-edge signs. For the larger families a certified route needs a global bound over the finite parameter region and the entire family of boundary profiles. It must compare each side on the same window, allow equality with an attained fit, and carry the specified likelihood trace through a wall or support contact. The conditional normalizer needs stable short-window and tail evaluation. In shifted GB2 the critical contact envelope must be compared as a sample likelihood even though its weak limit is an atom. Divergent contact and width-collapse paths justify specific unbounded-likelihood outcomes. On ordinary data where the global comparison is unresolved, the existing search, if available and successful, supplies an explicitly uncertified result under the project rule. All routes here are mathematical designs.

## Open

- For the masks labeled with a stationary comparison, determine pointwise cardinality beyond the listed exact examples. The proof bounds the boundary likelihood and identifies finite candidates, but it provides no uniform bound on the number of stationary components or on how many can tie. For GB2 and generalized gamma in particular, competing signed, two-piece and smooth profiles remain to be separated.
- Replace the stationary comparisons by finite procedures with practical height bounds. The unresolved step is a bound that isolates or excludes every stationary component and bounds every fitted limiting profile, including near-equality and wall contact. A finite set of evaluation starts gives only lower witnesses.
- For unbounded cases the displayed paths certify divergence. A classification of all maximizing sequences would need joint tightness or an extended-line envelope when endpoint, scale and tail shape can drift together. The held-scale generalized-beta proof gives a likelihood upper bound in the subunit large-power corner even when mass accumulates at the closed endpoint. Classifying the corresponding normalized-law limits requires controlling that extra mass separately.
