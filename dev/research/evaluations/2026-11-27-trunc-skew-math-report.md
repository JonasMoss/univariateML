# Maximum likelihood under known truncation for skew and shape-indexed families

TASK-458 studies a nonempty finite sample on a closed window $A=[a,b]$. Infinite cuts are allowed. Intersect $A$ with the parent support and require positive retained probability. A data value outside the parent support is infeasible. A Tukey endpoint with density zero cannot yield a positive likelihood. Shape domains are the fitted domains in the dossiers. In particular estimated GED shapes are at least 1, estimated skew-Student df are at least 1, and the lower fitted bounds for LST, half-t and Tukey lambda are exempt with held location as specified in their dossiers. The Tukey upper fitted bound is 1. All claims in the proofs below are **proved**. Numerical checks have their own section. The whole-line window is the base regime, with the results and frontiers in the [base atlas](../existence-atlas/proofs.csv).

## Likelihood comparisons

Write $m=min x_i$, $M=max x_i$, $I=A\cap\operatorname{supp}(f)$, and
$$P(k,I)=\frac{\prod_i k(x_i)}{(\int_I k)^n}.$$
Here $k$ can be an unnormalized kernel. The uniform on a bounded $I$ has likelihood $|I|^{-n}$. An exponential face of signed slope $v$ on $I$ has kernel $e^{v(x-o)}$ for a finite origin $o$. Its normalizer is $(e^{v(b-o)}-e^{v(a-o)})/v$, with value $b-a$ at zero. Infinite endpoints give zero exponential terms only on the decaying end. Optimize over allowed slopes by matching the sample mean and including slope-range endpoints. The log likelihood is strictly concave in slope by the positive conditional variance. On an integrable ray, the unrestricted rate is the reciprocal of the mean distance to the cut, giving $(\mathrm{rate}/e)^n$ when the mean distance is positive. At zero mean distance, increasing the rate without bound gives a weak atom and unbounded likelihood.

For a face list proved exhaustive, let $B$ be the supremum of its attainable sample likelihoods, including zero. Unattained boundary values use actual joint density limits, including any endpoint factors. For $B<\infty$, attainment is equivalent to a finite feasible point with $P\ge B$. If the inequality is strict, a maximizing sequence must have a finite subsequence. At equality, either that point is a maximizer or another point exceeds $B$. Conversely any maximum beats every boundary path. For $B=\infty$ there is no finite fit. A finite witness can be restricted to stationary points on smooth pieces, plus finite joins, support contacts and parameter endpoints, with the score restricted to each such piece. In fixed support coordinates $v$ the equation is
$$\overline{\partial_v\log k(x)}=\frac{\int_I k\,\partial_v\log k}{\int_I k}.$$
Differentiate moving endpoints separately. The criterion requires proof that every escaping sequence has upper envelope at most $B$ and an accessible path or sequence of paths for every height used. It gives no cardinality theorem. These are the [joint envelope](../ml-behaviour.tex) and the [earlier location-scale comparison](2026-11-27-trunc-ls-math-report.md#likelihood-and-comparison) specialized to this problem.

### Concentration and endpoint pairs

A normalized log-concave density with peak height $H$ at $z$ has $q(x)\le H\exp(1-H|x-z|/e)$ on its interval. On each side it drops below $H/e$ within distance $e/H$ or the support ends by then, since its integral is one. Concavity of the log extends the resulting slope bound. Thus $H\to\infty$ makes the product vanish on nonconstant samples. If local log slopes tend uniformly to a finite $v$ on fixed compact intervals, normalized ratios converge to an exponential. On the integrable ray the concavity tangent at the cut supplies an integrable bound when $v\ne0$. A nonintegrable ratio has infinite relative integral by Fatou, so its density goes to zero at each fixed observation.

For $x=(a,b)$ on a bounded window, a log-concave kernel satisfies $Z\ge(b-a)\sqrt{k(a)k(b)}$ by integrating the exponential chord and applying Jensen. Equality requires a constant kernel throughout. Hence a strict inequality at every finite parameter and an accessible uniform give exact nonattainment. This argument uses the actual retained interval and a positive endpoint density. A zero endpoint density gives zero likelihood directly.

### Two-piece parametrization

For SGED, skew normal of Fernández and Steel (snorm), and skew standardized Student (sstd), use the residual kernel
$$g((x-c)/s_\pm),\qquad s_+=d\xi/D,\quad s_-=d/(D\xi),\quad
c=\mathrm{mean}-d\,m_1(\xi-\xi^{-1})/D,$$
where $d=\mathrm{sd}$, $m_1=E|Z|$ for the unit-variance symmetric parent and $D^2=(1-m_1^2)(\xi^2+\xi^{-2})+2m_1^2-1$. This follows by the change of variable on each side of the kink, whose masses are proportional to $\xi$ and $\xi^{-1}$. Use $g(z)=\exp(-C_p|z|^p)$ for SGED with $C_p=(\Gamma(3/p)/\Gamma(1/p))^{p/2}$, $g(z)=e^{-z^2/2}$ for snorm, and $g(z)=(1+z^2/(\nu-2))^{-(\nu+1)/2}$ for sstd. Holds on the reported mean are constraints on $c$, not holds on the kink when skew or shape varies.

### Fixed stretched-exponential shape

For GED, fixed-shape fixed-skew SGED, and fixed-skew snorm, write $k=e^{-t V(x-c)}$ with $V(u)=C_\pm|u|^p$, $C_\pm>0$ and $t=d^{-p}$. The kink equals $\mathrm{mean}-d K$ with constant $K$, possibly zero. For held location and free scale, as $d\to0$ the kink tends to the held mean. Inside the window the conditional height of a constant at that mean is of order $1/d$, including a cut since the standardized offset $K$ is finite. For exterior limiting center the closest cut has height asymptotic to $t p C_\pm|x-c|^{p-1}$ at the cut. In either case the weak law is the projection atom and other constants vanish. More generally, when $c$ is bounded and $d\to0$, subtract the minimum of $V$ on $A$. The integral of this relative kernel is bounded below by a polynomial in $d$ (integrate an inward segment of length proportional to $d^{\max(1,p)}$). On nonconstant data the sum of potential gaps is bounded away from zero, hence their exponential dominates this polynomial. For a fixed exterior distance, the cut asymptotic follows by scaling the distance inward by the reciprocal slope. The local potential increment tends to the scaled distance. The near-cut integral has a linear lower bound on the gap, and outside a fixed neighborhood direct integration of the polynomial potential gives an exponentially small remainder. At broad scale with bounded standardized center the kernel ratios tend to one on compacts, giving uniform on bounded windows and zero on rays by Fatou. This includes broad scale with held mean.

For remote $c$ put $v=\operatorname{sgn}(c) t p C_\pm |c|^{p-1}$, using the side containing the window's fixed points. The log slopes are $v(1+O(1/|c|))$ on each fixed compact. Thus finite limiting $v$ gives the stated exponential or zero when nonintegrable. For $p\ge1$ log concavity gives integrable domination on rays. For $0<p<1$ with an integrable nonzero slope on a ray, the center is outside the ray, at distance $D\to\infty$ from the cut. The relative tail potential at inward distance $y$ is $t C[(D+y)^p-D^p]$. For $y\le D$ it is bounded below by a constant times $|v|y$, and for $y>D$ by a constant times $|v|D^{1-p}y^p$. This proves dominated integration. The same bounds at a diverging integrable slope give a cut atom, with height asymptotic to $|v|$ by the inverse-slope scaling. On any window with infinite limiting slope, integrate an inward segment of length $1/|v|$ at an observation to bound its height by $O(|v|)$. If the slope points into the window at that observation, integrate instead near a fixed point strictly in the uphill direction for an exponentially small bound. Therefore the product vanishes unless all observations equal the cut toward which the slope points. This covers remote centers inside an unbounded window as well.

At fixed scale only the slopes dictated by $p<1$, $p=1$ and $p>1$ are available at each remote end (zero, fixed signed rate, and infinite, respectively). With location and scale free, all finite nonzero slopes and infinite slopes are available when $p\ne1$, by varying $t$ with $|c|$. For $p=1$ the exponential slopes are represented by finite centers at or outside the matching cut. The broad uniform is always accessible when scale is free on a bounded window. The infinite threshold for a constant with free mean and scale follows by setting the kink at the observation and shrinking scale. With scale held and $p>1$ exactly the cut constants give divergence. For held mean use the two scale ends above. This proves exhaustion for every fixed-shape fixed-skew mask.

For GED with both mean and $p$ held, the score is the strictly monotone scalar exponential-family mean for $T=|x-\mathrm{mean}|^p$. The unique finite fit requires $\min_A T<\bar T<E_{\mathrm{unif}}T$, with upper threshold infinite on a ray by the residual rescaling. Equality at the lower end gives the projection collapse, and at or above the bounded upper end the uniform wins. The same holds for two-piece shapes with fixed kink, in particular skew one. Finite root counts in the general comparisons are open.

### Fixed power shape

The [prior power envelope](2026-11-27-trunc-ls-math-report.md#power-tails-and-critical-collapse) applies directly to LST and HT with $s=\sigma\sqrt{\nu}$ and exponent $\gamma=\nu+1>1$. For fixed-skew sstd and ST, and Slash, the following derivation gives the asymmetric extension. Use $k=g((x-c)/s)$ where $g$ is positive, continuous and has $g(u)\sim C_\pm |u|^{-\gamma}$ at the two ends. For Slash $g(u)=(1-e^{-u^2/2})/u^2$ with $g(0)=1/2$, $\gamma=2$, $C_\pm=1$. For sstd use the two-piece parametrization with $s=d$ and absorb both fixed multipliers into $g$. For ST use
$$g(z)=(1+z^2/\nu)^{-(\nu+1)/2}T_{\nu+1}\left(\alpha z\sqrt{(\nu+1)/(\nu+z^2)}\right).$$
The tail constants are $\nu^{(\nu+1)/2}T_{\nu+1}(\pm\alpha\sqrt{\nu+1})$. The center $c$ equals the dossier location except for sstd, where $c=\mathrm{mean}-sK$ for fixed $K$.

When $s+|c|\to\infty$, ratios converge to one uniformly on compact intervals. For a bounded standardized argument this follows by continuity and positivity, and for a diverging one by the tail equivalents. On rays Fatou gives zero pointwise likelihood. When $s\to0$ and $c$ tends to an exterior point $z$, the exact face is $|x-z|^{-\gamma}$, with normalizer
$$\frac{D_{\rm near}^{1-\gamma}-D_{\rm far}^{1-\gamma}}{\gamma-1}.$$
Here distances are to the cuts, with infinite distance contributing zero. Domination follows from the positive distance and $\gamma>1$.

At a limiting center $z\in A$, put $\rho=\sqrt{s^2+(c-z)^2}$, $d=(c-z)/\rho$, $h=s/\rho$, and $G_{d,h}(u)=h^{-\gamma}g((u-d)/h)$. The exact product is
$$\rho^{\gamma(n-k)-n}\frac{G_{d,h}(0)^k\prod_{x_i\ne z}s^{-\gamma}g((x_i-c)/s)}
{(\int_{(I-z)/\rho}G_{d,h})^n},$$
where $k$ observations equal $z$. At $h=0$, take $G_{d,0}(u)=C_{\operatorname{sgn}(u-d)}|u-d|^{-\gamma}$. The integration domain tends to $J=\mathbb R$ or the inward half-line, intersected with $[d,\infty)$ for HT. For $h>0$ the integral converges by continuity and the uniform tail bound $O(|u|^{-\gamma})$. At $h=0$ it converges when the pole is exterior to $J$, and diverges when the pole belongs to $J$ with positive one-sided length, by Fatou. In all cases it is uniformly bounded below by integrating a fixed interval away from the pole. The factor $G(0)$ is bounded since $h^2+d^2=1$. These arguments apply to HT for $z<b$; the shrinking interval at $z=b$ is handled separately.

Hence negative exponent gives divergence by choosing a feasible direction with $h>0$, positive exponent gives zero, and zero exponent gives the supremum over feasible directions of
$$ \frac{G_{d,h}(0)^k\prod_{x_i\ne z} C_{\operatorname{sgn}(x_i-z)}|x_i-z|^{-\gamma}}
{(\int_J G_{d,h})^n}.$$
A held original location fixes $d/h=-K$, with $K=0$ for all except sstd. When free, every direction is available unless HT feasibility $c\le m$ intervenes. For a positive-height HT collapse one has $z=m$ and $d\le0$; at fixed $z=m<b$, all those directions are feasible. The normalizer away from $z$ is negligible relative to the total (a factor $O(\rho^{\gamma-1})$), proving weak collapse. In HT broad scale with bounded $c$ and finite $b$, one obtains a uniform on $[\max(a,c),b]$ and can use $c=m$ for the greatest height when free. With $c\to-\infty$ the broad analysis gives uniform on bounded $A$ and zero on rays, using Fatou on the growing effective interval. With $b=\infty$ and bounded $c$, broad scale gives zero. The feasibility constraint rules out $c\to+\infty$. If all data equal $b<\infty$ and the endpoint is free, $c\uparrow b$ at fixed scale makes the likelihood diverge like $(b-c)^{-n}$. When endpoint $c\ge a$ is free, increasing it to $m<b$ strictly improves likelihood at fixed scale and df, since the numerator increases and the residual normalizer decreases. This proves uniqueness on left rays with scale and shape held, except for the upper-cut collapse.

All fixed-shape power regimes in this report use the same threshold rule, with the stated holds, the exterior power optimized over allowed positions, and the critical value computed by the direction formula. On rays, a held interior center with subcritical ties always fits when scale is free. Fixed scale with remote free center gives only the bounded uniform for full-support profiles, so every sample then fits on rays. For LST the sample $(-4,4)$ on $[-100,100]$, df 3 and $s=1$ gives distinct reflected maxima by the [held Student proof](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown). For Slash take the same sample and window and hold $\sigma=1$. The integral decreases as the center moves from 0 to 4 because $g$ is even and strictly decreasing in $|u|$ (write $g(u)=\int_0^{1/2}e^{-tu^2}dt$). The numerator at 4 is $(1-e^{-32})/128>1/256>g(4)^2$. Since $\int g\le3$ by splitting at $|u|=1$, the height at 4 exceeds the uniform. Attainment and evenness then force distinct maxima.

### Huber envelopes in physical coordinates

Write $c=\mu$, $h=\sigma^{-2}$, $r=\epsilon/\sigma$. The kernel is $e^{-V(x-c)}$ with $V(0)=0$ and $V'(y)=\operatorname{sgn}(y)\min(h|y|,r)$. This parametrization preserves the physical holds by imposing $h=\sigma^{-2}$ or $r=\epsilon\sqrt h$. It yields the following exhaustive list on proper windows.

- At finite limiting $c$, $h,r>0$ finite is an ordinary fit. Either coordinate zero gives uniform on bounded windows and zero on rays. At $h=\infty$ and $0<r<\infty$ the kernel is Laplace of rate $r$, and at $r=\infty$ and $0<h<\infty$ it is normal of variance $1/h$. If both diverge, the law collapses at the projection of $c$.
- For remote $c$ evaluate the signed slope at any fixed observation, $v=\operatorname{sgn}(c-x_i)\min(h|c-x_i|,r)$. All finite limiting slopes give the exponential comparison, with zero interpreted as uniform or diffusion. For infinite slope the only positive joint envelope is a constant at the finite cut in the direction of increase, with divergent likelihood and weak collapse there.
- With $\epsilon$ held, remote finite nonzero slopes require finite positive limiting $h$ and give slopes of either allowed sign and magnitude $\epsilon\sqrt h$. With $\sigma$ held and $\epsilon$ free any remote slope of the correct sign can occur. With both scale and threshold held the only remote slopes are $\pm\epsilon/\sigma$. When both are free all slopes are available. A held location removes all remote cases. The fixed-location Laplace face needs both $\sigma$ and $\epsilon$ free; the fixed-location normal face needs free $\epsilon$, and the normal variance respects the scale hold. Zero slopes at finite location are available iff $\epsilon$ or $\sigma$ is free.

For the proof, finite-center regular limits follow by integrating $V'$ and by the slope domination lemma or Fatou. When both $h,r$ diverge, $V$ rises without bound at each positive distance, with diverging slope there. The normalized law collapses at the projection: the increase in potential between any two fixed distances $0<\delta_1<\delta_2$ inward from the projection tends to infinity, and the concavity tangent bounds all remaining mass relative to the nearer segment. Thus the peak diverges and nonconstant products vanish. A constant at the projection can diverge along a fixed-center path; if the center is held, other constants vanish by the peak bound. When location is free, constant divergence for finite-center collapse requires free scale and can be obtained by fixing the center at the datum. For remote centers, changing $x$ over any fixed compact multiplies $|c-x|$ by $1+O(1/|c|)$. The same relative bound applies to the minimum with $r$. Finite slopes therefore exhaust regular remote faces. For diverging slope the argument in the stretched-exponential proof applies, without a restriction on the sign. The cut-normalized kernel converges after inverse-slope rescaling to $e^{-y}$, dominated by concavity if it decreases into the window, giving height asymptotic to $|v|$. Infinite slope requires both $h|c|$ and $r$ to diverge and is accessible precisely when at least one of scale and threshold is free. This proves the threshold criterion with the indicated holds, including constant samples. Include finite joins $|x_i-c|=r/h$ when scoring. Finite cardinality remains open.

### Azzalini skew-normal

For fixed $\alpha$ use $g(z)=e^{-z^2/2}\Phi(\alpha z)$. Its negative log has second derivative between 1 and $1+\alpha^2$. Indeed $-(\log\Phi)''(t)=1-\operatorname{Var}(N\mid N\le t)$, by integrating normal moments, and $\log\Phi$ is concave since the inverse Mills ratio is decreasing (its derivative is $-\phi(t)(t\Phi(t)+\phi(t))/\Phi(t)^2<0$). Its unique mode is at a finite standardized offset. Each log tail slope divided by $-z$ tends to 1 on the favored side and $1+\alpha^2$ on the other side. This follows from $\Phi(-u)\sim\phi(u)/u$, by l'Hôpital. The fixed-shape stretched-exponential proof applies with $p=2$: for a bounded center and vanishing scale, the curvature bounds give concentration at the projection and negligible nonconstant product, and for a distant center the slope equivalents give the exponential faces and cut collapse. At a held $\xi$ with scale tending to zero, an interior constant at $\xi$ has height proportional to $1/\omega$ because $g(0)>0$ and the retained standardized integral tends to a positive constant. For an exterior held $\xi$, the cut height is asymptotic to the local absolute slope, by the curvature bound after inverse-slope scaling. Both free give all signed slopes when integrable. Held scale gives only infinite remote slopes, hence any sample other than finite-cut constants fits. Held $\xi$ and free scale gives bounded uniform or zero at broad scale, and sharp collapse only at the projection for a constant. At $\alpha=0$ the normal rules below give cardinality as well.

With $\xi,\omega$ held and $\alpha$ free, both ends of the search have explicit heights. At $\alpha\to+\infty$ the favored interval is $A\cap[\xi,\infty)$ and at $\alpha\to-\infty$ it is $A\cap(-\infty,\xi]$. With positive length take the Gaussian kernel there with a factor $1/2$ at $x=\xi$, zero across the join, and divide by the Gaussian integral over the favored interval. Dominated convergence proves this joint likelihood. If the favored interval has zero length, the inverse Mills ratio gives diverging inward decay at fixed positive distance from $\xi$, and a half-normal-tail factor $\Phi(-y)$ on the $\omega/|\alpha|$ scale if the cut equals $\xi$. This gives a cut atom with diverging height there. In the first case (cut distinct from $\xi$), slopes on compacts divided by $\alpha^2$ tend to the signed linear function $-(x-\xi)/\omega^2$; normalization at the cut is asymptotic to the inverse cut slope by rescaling and concavity. In the second case use domination by $\Phi(-y)$ times a uniformly bounded Gaussian after rescaling, with positive integral. All other data products vanish by the exponential tail bound. The exact comparison is with these two excluded shapes. Root counts are open.

### Tukey lambda at fixed shape

Let $Q(u)=(u^\lambda-(1-u)^\lambda)/\lambda$ for $\lambda\ne0$ and $Q(u)=\log(u/(1-u))$ at zero. The density kernel is $g(Q(u))=1/[u^{\lambda-1}+(1-u)^{\lambda-1}]$; the $1/\sigma$ factor cancels. At $\lambda<0$ the tails have power $1+1/|\lambda|$, since $Q(u)\sim-u^\lambda/|\lambda|$ at zero, so use the fixed-power proof. At zero use the [logistic rules](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown).

At $\lambda>0$ the support is $[l,u]=[\mu-\sigma/\lambda,\mu+\sigma/\lambda]$. At a support edge the asymptotic kernel, in standardized inward distance $y$, is $C y^\beta$, with $\beta=1/\lambda-1$ for $0<\lambda<1$ and $\beta=0$ for $\lambda\ge1$. For $\lambda<1$, expand the quantile at zero to get $y\sim u_0^\lambda/\lambda$ for the small quantile probability $u_0$, and $g\sim u_0^{1-\lambda}$. For $\lambda\ge1$ the quantile derivative tends to 2 at 1 and to 1 at larger shape. Use the inward endpoint values. A finite feasible limit with positive retained width has an upper semicontinuous sample product, even if the density jumps to zero outside the support. Take a subsequence of the two support edges. With width tending to zero or the overlap shrinking to zero, feasibility forces constants. If scale and location are free, collapse at their common observation by shrinking a centered interval. With held location, collapse at the location if it equals the datum in $A$, or at the nearer cut if held location is outside $A$ and all data equal that cut. With held scale and free location, collapse by sliding support against a finite cut if all data equal that cut. The contact asymptotic gives cut height $(\beta+1)/|I|$ for sliding or exterior contact. Other constants cannot have shrinking retained width with those holds: either the support width stays positive and meets the interior of the window, or the interval stays centered at a different fixed point.

The only remaining escaping case with positive retained mass along the path is $\sigma\to\infty$. If both edges are remote, ratios converge to one on compacts, by kernel continuity in residual coordinates or by the edge equivalent when the standardized residual approaches an edge (the absolute edge distance then diverges). This gives a bounded uniform or zero on a ray. If an edge tends to $j$ finite, the face kernel is $(x-j)^\beta$ inward to the right of a lower edge, or $(j-x)^\beta$ inward to the left of an upper edge. On its bounded effective interval its normalizer is the difference of the far and near cut distances raised to $\beta+1$, divided by $\beta+1$. Uniform convergence of the rescaled kernel on bounded intervals inside the support follows from the edge equivalent. For $\beta=0$ use the inward endpoint value as the attainable upper envelope at a sample equal to $j$ (approach with supports containing the sample). At $\beta>0$ a contact sample contributes zero. On an unbounded effective interval the relative integral diverges by Fatou, including for observations approaching the support edge, whose rescaled kernels stay bounded. With $\mu$ held finite both edges must escape; with scale held this case is unavailable. These cases and the finite endpoint convention prove the exact comparison at fixed positive shape. At shape 1 an attained uniform member is compared using the shortest retained interval allowed by the holds, as in the [uniform rules](2026-11-27-trunc-ls-math-report.md#unif-all-free-truncateknown). General finite cardinality at fixed shape is open.

For the included Tukey shape 1, the shortest retained interval comparison with holds is explicit. At fixed center, the retained width is nondecreasing in scale. Use the smallest scale covering the data, $\max_i|x_i-\mu|$, and include every larger scale with the same retained width. A zero minimal width is the collapse case. At held scale let $W=2\sigma$ and let the lower support edge vary over $[M-W,m]$. The retained width $\min(b,l+W)-\max(a,l)$ is concave on this interval. Its minimum is the smaller of its endpoint values, and the maximizers are the feasible edges giving that width. Zero width is a contact collapse. With both center and scale free the unique retained interval for nonconstants is $[m,M]$ and every compatible physical support yielding that interval maximizes. These width criteria apply on either ray.

### Convolution kernels

For EMG use $k=e^{-\lambda(x-\mu)}\Phi((x-\mu)/\sigma-\lambda\sigma)$. At fixed $\mu,\sigma$, $\lambda\to\infty$ yields the normal via pointwise Gaussian convolution convergence and convergence of the masses (both parent densities integrate to one). At $\lambda\to0$ the kernel tends to $\Phi((x-\mu)/\sigma)$, integrable on a left ray and on bounded windows. On a left ray domination follows from the Gaussian bound for $\Phi$ times $e^{\lambda_0|x-\mu|}$ for $0<\lambda\le\lambda_0$. Its integral is the difference at the cuts of $(x-\mu)\Phi(z)+\sigma\phi(z)$, $z=(x-\mu)/\sigma$. On a right ray the limiting integral is infinite by Fatou. At fixed $\mu,\lambda$, $\sigma\to\infty$ gives bounded uniform or zero on rays: the log derivative $-\lambda+\phi(z)/(\sigma\Phi(z))$ tends to zero on compacts by the Mills equivalent. At $\sigma\to0$ the translated exponential with rate $\lambda$ remains on $A\cap[\mu,\infty)$ and takes half density at $\mu$. To justify its normalization, use $e^{-\lambda(x-\mu)}$ to the right of $\mu$, and $e^{-\lambda(x-\mu)}\Phi((x-\mu)/\sigma_0)$ to the left. If $b\le\mu$ the envelope instead is cut collapse, divergent for constants at $b$ and zero otherwise. For $b<\mu$ scale inward by the inverse cut slope, which tends to zero at order $\sigma^2$ by the Mills equivalent, and use concavity domination. For $b=\mu$ scale inward by $\sigma$ and use the integrable Gaussian tail bound for the translated $\Phi$ in the rescaled coordinate. At fixed $\lambda,\sigma$, $\mu\to-\infty$ gives a decreasing tilt of rate $\lambda$ (or zero on a left ray), and $\mu\to+\infty$ gives infinite positive slope, with divergence only for constants at a finite upper cut. The finite slope and infinite slope arguments apply because ratios of the local large slopes tend to one. These three scalar masks are exhausted. Finite cardinality is open.

For Bhattacharjee let $w$ denote the dossier half-width $a$. With $w>0$, use $k=\int_{-w}^{w}\phi((x-\mu-v)/\sigma)\,dv/(2w)$, and take the normal limit at $w=0$, which is an included member. At fixed $\sigma,\mu$, $w\to\infty$ gives uniform on bounded windows and zero on rays since ratios tend to one on compacts. At fixed $\mu,w>0$, $\sigma\to\infty$ gives the same faces by the compact ratio argument. With $\sigma\to0$, a positive-length intersection with $[\mu-w,\mu+w]$ gives the uniform there, with half density at each support edge of the parent and zero beyond. Convergence of the probabilities follows by integrating the normal smearing of the uniform or by bounded convergence on bounded windows plus Gaussian domination off the fixed support. For an empty or one-point intersection, the conditional law collapses at the adjacent cut; divergence requires constants there. If the cut is at positive distance $D$ from the parent interval, use the Mills equivalent to see a cut slope of magnitude asymptotic to $D/\sigma^2$. If contact occurs, use the inward distance to the cut divided by $\sigma$, with relative kernel proportional to $\Phi(-y)$ in the limit. In both cases the scaled integral converges, by the concavity tangent at the cut (at contact the scaled cut slope tends to $\phi(0)/\Phi(0)>0$ in magnitude). At fixed $w,\sigma$ and distant $\mu$, the log slope is the weighted mean of $-(x-\mu-v)/\sigma^2$, bounded between its extremes over $|v|\le w$. Hence the only positive boundary height is divergence at the favored finite cut for constants. Nonconstant data and interior constants fit in this mask, with the same normal result when $w=0$. For the concavity used here, differentiating the Gaussian interval integral in $x$ gives log curvature $(-1+\operatorname{Var}(N\mid L\le N\le U))/\sigma^2$. Integration by parts gives variance $1+[(L-m_N)\phi(L)-(U-m_N)\phi(U)]/[\Phi(U)-\Phi(L)]<1$, where $m_N$ lies strictly between $L,U$. This proves strict log concavity. Finite cardinality in the scalar masks is open except for the normal member.

### Log-scale reductions

If $Y=g(X)$ is strictly increasing on the parent support, truncate $Y$ to the image of the effective interval. Every finite sample product and every boundary envelope is multiplied on returning to $X$ by $J_x=\prod_i g'(x_i)$, independent of the coefficients. For logit-normal $g(x)=\log[x/(1-x)]$ and $g'(x)=1/[x(1-x)]$. Require $0<x_i<1$. With both parameters free and nonconstant logits, fit the signed linear tilt on the logit window, matching the mean. A finite logit-normal exists iff the empirical variance with divisor $n$ is strictly less than the tilt variance. Otherwise the excluded tilt wins (on the entire logit line there is no tilt and all nonconstant samples fit). Constants collapse. With $\mu$ held use the held-normal mean test for squared distance to $\mu$, comparing the sample mean of this statistic with its projected minimum and the uniform mean (infinite if unbounded). With $\sigma$ held match the sample logit mean against the strictly increasing mean of the linear tilt on the Gaussian carrier. A finite fit exists iff the mean lies strictly between the logit cuts, with equality giving divergent cut collapse. All attained fits are unique by positive covariance of $(Y,Y^2)$ or positive scalar variance. These are the [normal proofs](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown). The exponential tilt face on the original scale has kernel $x^{v-1}(1-x)^{-v-1}$ on the retained interval, with normalizer given by the tilt integral on the logit scale. The zero tilt is uniform in logit. For example $x=(1/4,3/4)$ on $[1/4,3/4]$ selects zero tilt and its likelihood is $[(16/3)/(2\log 3)]^2$.

For log-gamma $g(x)=\log x$, $g'(x)=1/x$, and the transformed interval is $[L,U]$ with effective lower cut at least 0. Require $x_i>1$. Use the [gamma rules](2026-11-27-trunc-expfam-math-report.md#gamma-free-truncateknown). With $k=\mathrm{shapelog}$ held, match $\bar Y$ in the negative rate tilt of $Y^{k-1}$. A finite fit requires $L<\bar Y<M_0$ where $M_0$ is the zero-rate mean on a bounded interval or infinity on a ray. At the lower endpoint there is collapse, otherwise failure is the zero-rate log-power. With $v=\mathrm{ratelog}$ held, match $\overline{\log Y}$ between the limiting carrier log mean at $k=0$ (minus infinity when $L=0$) and $\log U$. Failure below or at a finite lower threshold selects the carrier $Y^{-1}e^{-vY}$ and equality at the upper end gives collapse. Free $k,v$ give constant collapse; for nonconstants test eligible edges and the corner by the gamma rule. At $k=0,v>0,L>0$ match $\bar Y$ and require $\overline{\log Y}\le E\log Y$. At $v=0,k>0,U<\infty$ match $\overline{\log Y}$ and require $\bar Y\ge EY$. At $(0,0)$ with $0<L<U<\infty$ require both inequalities. A passing excluded edge or corner gives nonattainment, otherwise a unique fit exists. Uniqueness in all attained masks follows from the positive covariance or variance in the gamma proof. The original-scale comparison uses $k_x=x^{-v-1}(\log x)^{k-1}$, with normalizer $\int_L^U y^{k-1}e^{-vy}dy$ on each eligible edge. This gives $(U^k-L^k)/k$ at $v=0,k>0$ and $\log(U/L)$ at the corner. For example $(e,e^4)$ on $[e,e^4]$ selects the corner by the gamma example and has likelihood $1/[4e^5(\log 4)^2]$.

### Double Pareto on the log scale

Put $y=\log x$, $c=\log(\mathrm{scale})$, and use log window $[L,U]$ with $L=-\infty$ permitted if the physical lower cut is nonpositive. Require $x_i>0$. The kernel is $e^{-\alpha(c-y)_+-\beta(y-c)_+}$ with $\alpha=\mathrm{left\_rate}$ and $\beta=\mathrm{right\_rate}$. Multiply the log-scale joint density by $\prod_i x_i^{-1}$. The integral is the sum of exponential integrals over the two sides. A finite held kink partitions the retained interval into the two side lengths (possibly infinite or zero). With both lengths positive, the conditional log likelihood is strictly concave in the free rates, since no free nonzero linear combination of the two hinge statistics is constant on the retained interval. With one side empty only its rate is redundant. A rate tending to zero on a finite side replaces its exponential by the uniform kernel one on that side. On an infinite side it gives zero joint likelihood by Fatou. An infinite rate deletes its side from the integral; its numerator contribution is zero if any observation has strictly positive distance on that side, otherwise it is one, including at the kink. When the surviving side has a finite positive integral, these rules give the exact joint limit. If its integral diverges the joint likelihood vanishes. If the total integral tends to zero, each nonempty side with a diverging rate $r$ contributes order $1/r$ when the kink is inside the closure. Any strictly positive observed distance in a nonempty side then gives a vanishing product, since the normalizer is at least order $1/r$ from that side. With every distance zero, the product diverges and the weak law is the kink atom. For an exterior kink factor out its kernel value at the nearer cut first. The retained law is then the decreasing exponential of the inward distance, with the single active rate. A diverging rate on this side gives cut divergence for constants there and zero otherwise. These arguments exhaust the extended rate rectangle for each held kink. Take the greatest eligible boundary height, including mixed zero and infinite rates, and use the threshold rule in the active coordinates. Each attained component has a unique active-rate fit; all inactive free rates are arbitrary.

With kink free at fixed rates, proper windows reduce to a finite list. On a gap between observations and window cuts, the numerator log is affine in $c$ and
$$Z=\frac{1-e^{-\alpha(c-L)}}{\alpha}+\frac{1-e^{-\beta(U-c)}}{\beta}$$
inside the window. $Z$ has strictly negative second derivative when at least one cut is finite, so the likelihood log is strictly convex on each gap. Outside a finite cut the density is a one-sided exponential independent of the kink, and the cut represents the entire exterior plateau. Thus evaluate the sample knots and finite cuts, with all winning exterior plateaus restored. The same pointwise reduction applies with free rates. For each knot or cut solve the held-kink problem, compare the finite winners (including ties) with all component suprema and restore the plateaus. A boundary in one component can have a finite representative in another. The reduction proves exhaustion even for joint rate and kink escape: at each rates pair a knot or cut attains the kink maximum, and a subsequence uses the same cut or knot. It classifies failure by the winning component faces and exactly specifies the maximizing set. On the whole line use the [double-Pareto base scan](2026-11-25-dblpareto-math-report.md). For example with both rates held at 1, $x=(1,1)$ on $[1,e]$ has every kink scale in $(0,1]$ maximizing. At $L=0$, the kernel ratio at every $y\ge0$ to the kernel at 0 is at least $e^{-y}$ and is strictly greater on a positive-length interval when $c>0$, proving the claim.

### Skew-only two-piece envelopes

Fix the mean, sd $d$ and symmetric shape in the two-piece parametrization. Let $A_1=1-m_1^2>0$ and send $\xi\to\infty$. Then
$$c_0=\mathrm{mean}-d m_1/\sqrt{A_1},\qquad S=d/\sqrt{A_1},\qquad s_-\sim S\xi^{-2},\qquad (c-c_0)/s_-\to H_1=m_1/(2A_1).$$
The last identity comes from expanding $D/\xi$ in $\xi^{-2}$ and using $2A_1+(2m_1^2-1)=1$. The limit $\xi\to0$ is the reflection of this calculation about the held mean. The exact product at $\xi\to\infty$ has three cases.

1. If $A\cap[c_0,\infty)$ has positive length, use $g((x-c_0)/S)$ on that interval, zero below, and replace its numerator value at $c_0$ by $g(-H_1)$. Integrate only the surviving kernel. The integral on the shrinking side is $O(s_-)$. Convergence on the other side follows by substitution on the standardized half-line.
2. If $b<c_0$ and $g$ has power tails of exponent $\gamma$, use $(c_0-x)^{-\gamma}$ and its exterior normalizer. Divide the kernel by $s_-^\gamma$ and apply tail domination. For a stretched-exponential kernel the law instead collapses at $b$ and only a constant there has divergent product, by the fixed exterior-distance argument.
3. If $b=c_0$ the whole window lies on the shrinking side for sufficiently large $\xi$, since $H_1>0$. In units of $s_-$ the retained integral divided by $s_-$ tends to $J=\int_{-\infty}^{-H_1}g(z)dz$, since $a<b$ is fixed. The weak limit is $\delta_b$. Light tails give divergence for constants at $b$ and zero otherwise. For power tails with coefficient $C_-$, the product is asymptotic to
$$s_-^{\gamma(n-k)-n}\frac{g(-H_1)^k C_-^{n-k}\prod_{x_i<b}(b-x_i)^{-\gamma}}{J^n},$$
where $k$ counts the upper-cut ties. This gives zero, a positive finite critical height, or infinity according to the sign of the exponent.

The two excluded skew ends and the finite score exhaust this scalar mask. This proof also supplies accessible face heights in any regime with fewer holds by freezing the other coordinates. In particular a skew half-law can have a positive contact value different from both its inward density and half its inward density.

### Accessible free-shape faces

The following paths keep the remaining coordinates fixed unless a change is specified. They give exact lower bounds on the excluded envelope where the listed coefficients are free. A regular face with positive retained mass follows from pointwise parent density convergence at continuity points and convergence of the parent integrals to one (integrate the minimum against the limiting density). At a support join the pointwise limit is given separately where it is used. These paths leave the coupled boundary comparisons in the open list below.

- **GED and SGED.** As $p\to\infty$ with held scale $d$ fixed, the symmetric parent tends to uniform on $[-\sqrt3,\sqrt3]$ in standardized coordinates. The gamma expansion $\Gamma(\epsilon)=1/\epsilon+O(1)$ gives $C_p^{1/p}\to1/\sqrt3$ and $m_1\to\sqrt3/2$. Thus for fixed skew the uniform lies between $c_\infty-\sqrt3,s_{-,\infty}$ and $c_\infty+\sqrt3,s_{+,\infty}$, with substitutions from the two-piece formula. For samples strictly away from its two support edges and positive overlap, the height is the reciprocal retained width to power $n$ when all data are inside, zero otherwise. Parameters approaching a support edge simultaneously require a separate pointwise calculation. For the exact sample $(a,b)$ on a proper bounded window all fitted $p\ge1$ kernels satisfy the endpoint-pair bound. With free sd, uniform on $A$ is accessible at broad scale even with mean held. With sd held and fitted $p$ free, the argument also applies when both cuts are strictly inside an accessible limiting uniform support and there is no finite plateau. The same endpoint-pair example works for snorm with free scale.
- **sstd, ST, LST and HT.** At df infinity, Student kernels converge to their normal counterparts with the physical scale held. This follows from $(1+z^2/\nu)^{-(\nu+1)/2}\to e^{-z^2/2}$ and the gamma ratio from Stirling. For ST the cdf argument tends to $\alpha z$, yielding SN, and for sstd the two-piece parameters converge to snorm by the gamma ratio. HT gives half-normal with its inward value at the fixed support edge. Insert each kernel in $P$. At df 2 for sstd with sd fixed and skew fixed, $m_1=O(\sqrt{\nu-2})$ and the effective power scale is order $\sqrt{\nu-2}$. At a held mean within $A$, the kink displacement is of the same order. Rescaling as in the power proof gives finite positive integral and tail factors, with exponent $3(n-k)-n$ since $s^{\nu-2}\to1$. A negative exponent gives divergent collapse for more than two thirds tied at that mean; equality gives the power direction formula evaluated at the limiting $t_2$ shape and displacement. With free mean choose the held-mean value at any observed tie. For fixed df the majority divergence tests of the power lemma also apply to any mask allowing the small-scale path at the relevant center.
- **ST skew.** At fixed df, center and scale, $\alpha\to\pm\infty$ with positive-length favored overlap yields the half-Student kernel, with a factor $1/2$ at the center, zero on the wrong side. This follows by dominated integration against the parent Student. On a window strictly on the wrong side, the cdf power tail of order $\nu+1$ instead gives a normalized inverse power $|x-\xi|^{-(\nu+1)}$: the factor $(\nu+z^2)^{(
u+1)/2}$ from the cdf tail cancels the Student factor. The tail equivalent follows by integrating the density equivalent; if the window lies a positive distance from the center the argument divided by $|\alpha|$ is bounded away from zero, giving domination on rays. Contact and coupled df and skew paths require additional envelopes. These paths freeze both center and scale regardless of whether either is held.
- **LST and HT small df.** With location held the fitted df can approach zero. On a bounded effective interval at positive distance from the fixed center and fixed positive scale, divide the kernel by $(\nu\sigma^2)^{(\nu+1)/2}$. It tends uniformly there to $|x-\mu|^{-1}$, with normalizer $\log(D_{\rm far}/D_{\rm near})$. On an exterior ray the relative normalizer diverges by Fatou and the sample product vanishes. For HT this exterior case lies to the right of the center. Contact at the center is outside this calculation because the reciprocal-distance kernel is singular there.
- **Tukey lambda.** An admissible shape 1 is the uniform and is already included. As $\lambda$ approaches any finite interior shape with fixed center and scale, pointwise density and interval probabilities converge away from support edges by the quantile formula. With shape free and a tied center at an allowed location, fixed admissible negative shape gives the power divergence test whenever scale can shrink, with exponent $(1+1/|\lambda|)(n-k)-n$. For held $\mu$ and scale free, any positive tie count can use sufficiently negative allowed shape to make this exponent negative. With location free on the fitted branch, strict majority ties suffice at $\lambda=-1$. With held scale and location free, any cut constant diverges by sliding the included shape-1 support toward the cut. With held location and free scale, shape 1 gives the held-center collapse conditions from the fixed-shape proof.
- **EMG and Bhattacharjee.** Each scalar face above remains an accessible path when its physical coefficients are freed. The normal member of Bhattacharjee at $w=0$ remains attainable; EMG's normal is excluded. With all three coefficients free, the exact sample $(a,b)$ on a bounded window with $a<b$ has strict uniform escape in both models, using strict log concavity and broad Gaussian scale at fixed remaining coefficients. The same proof works in every mask with free Gaussian scale. EMG strictness follows from strict concavity of $\log\Phi$ and the linear exponential term, and Bhattacharjee strictness follows from the conditional variance calculation. Positive overlap in these fixed-component paths does not address a moving component edge approaching a data value on the scale of the Gaussian smearing.

### Shape-only GED and standardized Student

For GED with mean and sd held, and SGED with mean, sd and skew held, shape varies on $[1,\infty)$. The lower endpoint is an included two-piece Laplace (symmetric for GED). At the upper end use the uniform interval from the preceding section, with the numerator value $e^{-e^{-\gamma_E}}$ times the ordinary uniform height at either parent edge, where $\gamma_E$ is Euler's constant. In fact $\log\Gamma(t)=-\log t-\gamma_E t+O(t^2)$ by differentiating $\log\Gamma(1+t)$ at zero. This gives $\log C_p=-(p/2)\log 3-\gamma_E+O(1/p)$. In the asymmetric case the linear term in $1/p$ cancels from $\log m_1$ because $2=(1+3)/2$, so the moving $c,s_-,s_+$ differ from their limits by $O(p^{-2})$. Therefore the exponent of the kernel tends to $e^{-\gamma_E}$ exactly at either nominal edge. In positive overlap the bounded-window integral converges directly and the outside tails on rays are dominated by an exponential, since for $z$ beyond a sufficiently large fixed residual $C_p|z|^p$ is bounded below by a constant times $|z|$ uniformly at large $p$.

When the window lies entirely outside this limiting interval, the peak is at its near cut for large $p$. If separated, the inward decay at that cut tends to infinity and grows inward by log concavity, so the relative normalizer is at most the inverse cut slope. If touching, normalize relative to the cut and rescale inward distance by $1/p$. The kernel tends to $\exp(-e^{-\gamma_E} e^{y/D})$ at scaled distance $y$, where $D>0$ is the limiting distance from kink to contacted edge. The scaled slope at the cut is bounded away from zero, supplying integrable exponential domination. Hence the peak diverges and the weak law concentrates there, with vanishing joint product for all other samples by the peak bound. These are the only excluded paths. Compare the heights with finite scalar candidates, including shape 1. General uniqueness remains open.

For sstd with mean, sd and skew held, the only shape ends are df 2 and infinity. Infinity is the truncated snorm. At 2, use the exterior inverse cube when the held mean is outside $A$ by positive distance, by tail domination. For the held mean in $A$ put $s=d\sqrt{\nu-2}$ in the power rescaling, absorbing the two piece constants into
$$g_\nu(u)=(1+(D\xi^{\mp1}u)^2)^{-(\nu+1)/2}.$$
The sign of $u$ selects $\xi^{-1}$ on the right. The shift $c-\mathrm{mean}=-d m_1(\xi-\xi^{-1})/D$ divided by $s$ has a finite limit since $m_1/\sqrt{\nu-2}\to1$. The functions $g_\nu$ converge on compacts and are dominated in the tails by a constant times $|u|^{-3}$, so the integral and critical formula apply with limiting coefficients, $\gamma=3$ and this fixed scaled shift. Powers $s^{\nu-2}$ tend to one. For negative exponent the product diverges, for positive exponent it vanishes, and at equality the formula gives the exact atom envelope. This exhausts the scalar shape comparison. Finite cardinality remains open.


### Additional exact sample cases

For fixed-Student LST with df at least 1 and half-t with df at least 1, $x=(a,b)$ on a bounded window is an exact uniform escape when scale is free and location can be placed at or below $a$ (or is already held there). For HT the two observations force the retained interval to equal the entire window. The [angular inequality for Student](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown) only needs $p=(\mathrm{df}+1)/2\ge1$ for Jensen on the inverse quadratic, so the finite kernel always gives product strictly below $(b-a)^{-2}$. A broad scale with location fixed gives equality in the limit. For every Huber mask with free scale or free threshold the same sample forces uniform escape by the endpoint-pair argument: a finite threshold gives either nonzero linear slope or positive quadratic curvature on some portion of the window. These example statements on varying-shape models require the finite-domain inequality to hold for *every* admitted shape. The Student example consequently concerns held df only.

For ST, LST and HT with free shape and scale, choose any admissible fixed df and apply the fixed-power collapse test. If location can move, a strict majority tie suffices for ST and LST by choosing df 1. For HT the tie must be at the sample minimum and with positive remaining width ($m<b$); a constant at the upper cut has the separate support-shrinking path. For held location and free scale, the tie must be at that location and it must be retained. LST and HT may then choose arbitrarily small positive df under their fitting restrictions, so any positive tie count suffices. ST retains the lower bound df 1 when location is held. These divergence witnesses apply on either ray and on bounded windows.

### Free degrees of freedom in LST and HT

Use $c=\mu$, physical scale $\sigma$, df $v$, and $k(x)=(v\sigma^2+(x-c)^2)^{-(v+1)/2}$. The constrained free-df interval is $[1,\infty)$ when $\mu$ is free and $(0,\infty)$ otherwise. The effective interval, the HT upper-cut constant, and the improvement to $m$ on the HT branch $c\ge a$ follow the fixed-power proof.

A finite positive limiting df $v_0$ uses the uniform, exterior power and collapse limits from that proof, with the following critical qualification. For shrinking scale $s=\sigma\sqrt v$, the power of $\rho$ is now $\rho^{(v+1)(n-k)-n}$, and if $(v_0+1)(n-k)-n=0$ the extra factor is $\rho^{(v-v_0)(n-k)}$. If lower df are feasible and $n-k>0$, a fixed lower df already gives divergence. If $v_0=1$ is the fitted lower endpoint, the extra factor is at most one and equality is attained by holding df at 1. In all cases where the global height is finite, the fixed-df critical maximum therefore applies. The normalization proof uses an integrable common power bound near each positive $v_0$, and Fatou when a rescaled pole enters the interval. In particular, with free center and scale strict majority ties permit divergence at df 1. For HT only ties at $m$ matter, with the separate shrinking-support exception.

At $v\to\infty$ with finite $c$ and finite positive $\sigma$ there is the truncated normal or half-normal. With finite $c$ and $\sigma\to\infty$ the log slopes vanish on compact intervals, giving uniform on a bounded effective window or zero on an effective ray. Let $v\to\infty$, $\sigma\to0$, and $c$ tend to a finite limit. Take $z$ the projection of $c$ onto the retained interval, $D=|z-c|$, and $Q=\sigma^2+D^2/v$. Factoring out $k(z)$, a segment of length comparable to $\min(\sqrt Q,Q/D)$ at $z$ has kernel ratio bounded below by a positive constant (use $\log(1+u)\le u$ and omit $Q/D$ if $D=0$). Hence all densities are $O(1+Q^{-1})$ as long as the retained width has a positive lower bound. Away from the limiting projection the ratio is bounded by $(1+C/(vQ))^{-v/2}$ for some $C>0$. For $v\ge N$ this is at most $(1+C/(NQ))^{-N/2}$ by monotonicity in $v$. Choosing $N>2n$ proves zero joint likelihood unless all data are at the limiting projection. The same bound with an inward squared-distance term, integrated off a neighborhood, proves weak concentration by taking $N>4$. A divergent constant at that projection is already accessible at fixed df if center and scale allow collapse there. For a held exterior center, the $v\to\infty$, $\sigma\to0$ path **adds** cut divergence when all data are at its projection. At the cut its log slope magnitude tends to infinity, with a normalized relative integral of order its reciprocal by the remote-slope calculation below (using fixed positive $D$). Any held center inside $A$ permits constant divergence only at that center. For HT the sharp limit has positive-width exception only at an upper-cut constant, already treated.

When $|c|\to\infty$ and $v\to\infty$, the signed local slope is
$$w=\operatorname{sgn}(c-x)\frac{(v+1)|c-x|}{v\sigma^2+(c-x)^2}.$$
Its ratio to the slope at a fixed origin tends uniformly to one on compacts. Every finite limit gives an exponential or uniform on the bounded window, and only the integrable sign survives on an unbounded window. For HT a remote center can only tend to $-\infty$, so only negative slopes occur. On an integrable ray with exterior center, at inward distance $y$ from the cut the ratio is bounded by
$$(1+2|w|y/(v+1))^{-(v+1)/2},$$
by expanding the squared residual at cut distance $D$. For finite nonzero limiting slope this is dominated by an integrable common power, by monotonicity in $v+1$. For diverging slope with an integrable sign, substitute $y=u/|w|$. The cut distance $D$ is bounded away from zero in this remote case or in the finite exterior-center case just above. The quadratic term is negligible after rescaling, and the integrated kernel tends to $\int_0^\infty e^{-u}du=1$ by the same common power. The expanding inward interval on the rescaled bounded window obeys the same domination. The cut density is asymptotic to $|w|$ and the law concentrates at the cut. With diverging slope pointing up an unbounded ray, the segment comparison of the stretched-exponential proof gives zero joint likelihood. For HT with remote center the sample is eventually on the tail side. When location is free and either ray sign is allowed, a finite tilt of any nonzero magnitude $r$ is accessible by $v\sim r|c|$ at fixed physical scale. An infinite slope is accessible with $v$ increasing faster than $|c|$ at fixed physical scale, so cut constants can diverge even with held $\sigma$. HT only allows the negative sign, and an upper-cut constant also diverges by shrinking retained support.

At $v\downarrow0$ the center is held. Subsequence $s=\sigma\sqrt v$ in $[0,\infty]$. On a bounded effective interval, $0<s<\infty$ in the limit gives the kernel $(s^2+(x-c)^2)^{-1/2}$ with normalizer given by the difference of $\operatorname{asinh}((x-c)/s)$ at its cuts. At $s\to\infty$ use the uniform. At $s\to0$ with exterior center use the reciprocal-distance face above. On rays all three paths give zero pointwise where data have positive distance from $c$: for finite limiting $s$ the relative normalizer diverges by Fatou, using the ratio kernel for positive-distance data. With retained $c$ and $s\to0$ the normalization also diverges on bounded effective windows because the limit is reciprocal distance and the interval includes a positive-length one-sided neighborhood of $c$. Thus in the absence of ties this last case gives zero. When scale is free and there are ties at retained $c$, the fixed small-df divergence witness already proves nonattainment and an infinite supremum. This test does not classify the laws of all divergent paths with such ties.

The remaining case is HT with held $\mu,\sigma$ and ties at $c$. Then $s=\sigma\sqrt v$. On a bounded effective interval starting at $c$, $Z\asymp \log(1/s)$. Integrate $s\le x-c\le B$ for a fixed $B$, using $s^{-v}\to1$, and bound the part $0\le x-c<s$ by $s^{-v}$. The product diverges for any tie, with a weak atom at $c$ since mass at fixed positive distance is negligible relative to $Z$. On its ray, $Z$ is of order $1/v$: rescaling by $s$ gives a prefactor $s^{-v}$ and the integral from standardized radius one to infinity lies between $2^{-(v+1)/2}/v$ and $1/v$. Each tie density is then of order $\sqrt v$ and each off-center density is of order $v$. The product tends to zero. This is a lost-mass example with zero sample likelihood even for ties.

For the exact attainment test, first apply the divergence cases. Otherwise compare the finite scores (including df 1 when allowed) with the greatest eligible height from the fixed-df limits, the infinite-df limits, and the zero-df limits. In the positive-df boundary family optimize over feasible df as well as exterior locations or HT effective widths. A finite fit can tie the boundary. The exhausted cases validate this comparison for all masks of LST and HT, including one-sided windows. Outside these divergence conditions the finite cardinality except for the stated fixed-df subcases remains open.
## Family and mask results

### ged all free truncate:known

**Open.** Uniform-edge layers with shape tending to infinity while location or scale drifts. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### ged mean truncate:known

**Open.** Uniform-edge layers with shape tending to infinity while location or scale drifts. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### ged mean+nu truncate:known

**Proved.** Strictly bracket the sample mean of the absolute residual to the held power. Lower equality gives projection collapse. At or above the bounded upper threshold only the uniform wins. Every attained fit is unique. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### ged mean+nu+sd truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### ged mean+sd truncate:known

**Proved.** Compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment. General finite cardinality is open. [Shape-only GED and standardized Student](#shape-only-ged-and-standardized-student)

### ged nu truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### ged nu+sd truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

### ged sd truncate:known

**Open.** Uniform-edge layers with shape tending to infinity while location or scale drifts. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sged all free truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged mean truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged mean+nu truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged mean+nu+sd truncate:known

**Proved.** Compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows. General finite cardinality is open. [Skew-only two-piece envelopes](#skew-only-two-piece-envelopes)

### sged mean+nu+sd+xi truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### sged mean+nu+xi truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged mean+sd truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sged mean+sd+xi truncate:known

**Proved.** Compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment. General finite cardinality is open. [Shape-only GED and standardized Student](#shape-only-ged-and-standardized-student)

### sged mean+xi truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged nu truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged nu+sd truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sged nu+sd+xi truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

### sged nu+xi truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sged sd truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sged sd+xi truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sged xi truncate:known

**Open.** Uniform-edge layers jointly with one-sided compression, including both moving joins and retained mass tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. For GED and SGED this requires held shape at least 1 if held. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sn all free truncate:known

**Open.** Wrong-side conditioning with alpha unbounded while squared-scale tilt or the moving join changes. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sn alpha truncate:known

**Proved.** Use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask. General finite cardinality is open. [Azzalini skew-normal](#azzalini-skew-normal)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sn alpha+omega truncate:known

**Proved.** Use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask. General finite cardinality is open. [Azzalini skew-normal](#azzalini-skew-normal)

### sn alpha+omega+xi truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### sn alpha+xi truncate:known

**Proved.** Use the fixed-alpha Gaussian tail slopes, constrained by the location and scale holds. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask. General finite cardinality is open. [Azzalini skew-normal](#azzalini-skew-normal)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sn omega truncate:known

**Open.** Wrong-side conditioning with alpha unbounded while squared-scale tilt or the moving join changes. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sn omega+xi truncate:known

**Proved.** Compare the two favored-half or wrong-side collapse ends of alpha. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The positive envelopes are exactly the hold-compatible Gaussian-tail tilts and collapse or the two scalar skew ends specified for this mask. General finite cardinality is open. [Azzalini skew-normal](#azzalini-skew-normal)

### sn xi truncate:known

**Open.** Wrong-side conditioning with alpha unbounded while squared-scale tilt or the moving join changes. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### snorm all free truncate:known

**Open.** Moderate-side broad or remote tails coupled to the collapsing side and a moving kink. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### snorm mean truncate:known

**Open.** Moderate-side broad or remote tails coupled to the collapsing side and a moving kink. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### snorm mean+sd truncate:known

**Proved.** Compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows. General finite cardinality is open. [Skew-only two-piece envelopes](#skew-only-two-piece-envelopes)

### snorm mean+sd+xi truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### snorm mean+xi truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### snorm sd truncate:known

**Open.** Moderate-side broad or remote tails coupled to the collapsing side and a moving kink. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### snorm sd+xi truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

### snorm xi truncate:known

**Proved.** Use the broad uniform, allowed remote slopes and concentrated projection. Finite Laplace plateaus remain eligible. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Broad uniform, allowed exponential tilt, or divergent projection or cut collapse. General finite cardinality is open. [Fixed stretched-exponential shape](#fixed-stretched-exponential-shape)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### sstd all free truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd mean truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd mean+nu truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sstd mean+nu+sd truncate:known

**Proved.** Compare both skew ends including a moving join, exterior power or collapse as applicable to the window. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Both skew ends give the exact half-family, exterior power or cutoff collapse products in their applicable windows. General finite cardinality is open. [Skew-only two-piece envelopes](#skew-only-two-piece-envelopes)

### sstd mean+nu+sd+xi truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### sstd mean+nu+xi truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### sstd mean+sd truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd mean+sd+xi truncate:known

**Proved.** Compare the explicit scalar shape endpoints, retaining the lower endpoint when included. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the specified shape endpoint laws or their contact or collapse envelopes can give nonattainment. General finite cardinality is open. [Shape-only GED and standardized Student](#shape-only-ged-and-standardized-student)

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd mean+xi truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at the held mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd nu truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sstd nu+sd truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### sstd nu+xi truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### sstd sd truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd sd+xi truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### sstd xi truncate:known

**Open.** Joint df, skew and scale limits, especially near df 2 when the surviving tail and compressed side both contribute. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** More than two thirds of the data tied at a sample value chosen as the mean give divergence along df down to 2 at fixed sd and skew. [Accessible free-shape faces](#accessible-free-shape-faces).

### st all free truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### st alpha truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### st alpha+nu truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### st alpha+nu+xi truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### st alpha+xi truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### st nu truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### st nu+xi truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### st omega truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### st xi truncate:known

**Open.** Contact at the moving skew join and uniform bounds when shape, scale or location co-escape. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### lst all free truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### lst df truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### lst df+mu truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### lst df+mu+sigma truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### lst df+sigma truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

**Proved case.** At df 3 and sigma 1/sqrt(3), (-4,4) on [-100,100] has distinct reflected maximizers. [Fixed power shape](#fixed-power-shape).

### lst mu truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### lst sigma truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### slash all free truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### slash mu truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### slash sigma truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

**Proved case.** At fixed sigma 1, (-4,4) on [-100,100] has distinct reflected maximizers. [Fixed power shape](#fixed-power-shape).

### huber all free truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### huber epsilon truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### huber epsilon+mu truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### huber epsilon+sigma truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

### huber mu truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### huber mu+sigma truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### huber sigma truncate:known

**Proved.** Use the physical holds in the finite-center normal, Laplace, uniform and collapse rules and in any remote tilt. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The hold-compatible uniform, exponential, Laplace and normal faces and concentration exhaust positive envelopes. General finite cardinality is open. [Huber envelopes in physical coordinates](#huber-envelopes-in-physical-coordinates)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### ht all free truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### ht mu truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### ht mu+nu truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### ht mu+nu+sigma truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### ht mu+sigma truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### ht nu truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

### ht nu+sigma truncate:known

**Proved.** Compare the bounded uniform, any compatible exterior powers and the critical joint products. Test for divergence first. Include effective uniforms, upper-cut contact and the sample-minimum branch. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The applicable positive envelopes are effective uniform, exterior power, and critical or divergent collapse, with shrinking support handled for HT. General finite cardinality is open. [Fixed power shape](#fixed-power-shape)

**Proved case.** On a left ray with sample minimum below the upper cut, the unique endpoint is the sample minimum. [Fixed power shape](#fixed-power-shape).

### ht sigma truncate:known

**Proved.** Apply the stated divergence tests, then compare the eligible finite-df, infinite-df and zero-df boundary heights. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Divergence in the stated regimes or a finite winning uniform, tilted, normal, power or critical-collapse boundary from the df comparison. General finite cardinality is open. [Free degrees of freedom in LST and HT](#free-degrees-of-freedom-in-lst-and-ht)

### tlambda all free truncate:known

**Open.** Coupled support-edge and shape motion near shape 0 and 1, and the large negative shape end where allowed. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### tlambda lambda truncate:known

**Proved.** Negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape. General finite cardinality is open. [Tukey lambda at fixed shape](#tukey-lambda-at-fixed-shape)

### tlambda lambda+mu truncate:known

**Proved.** Negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape. General finite cardinality is open. [Tukey lambda at fixed shape](#tukey-lambda-at-fixed-shape)

### tlambda lambda+mu+sigma truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### tlambda lambda+sigma truncate:known

**Proved.** Negative shape uses power collapse and exterior powers, zero uses logistic, and positive shape uses retained-support contact and broad edge powers. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. The sign-specific face list gives the positive envelopes, including shrinking support and broad edge powers at positive shape. General finite cardinality is open. [Tukey lambda at fixed shape](#tukey-lambda-at-fixed-shape)

### tlambda mu truncate:known

**Open.** Coupled support-edge and shape motion near shape 0 and 1, and the large negative shape end where allowed. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** A tie at the held center diverges at sufficiently negative fixed shape as scale shrinks. [Accessible free-shape faces](#accessible-free-shape-faces).

### tlambda sigma truncate:known

**Open.** Coupled support-edge and shape motion near shape 0 and 1, and the large negative shape end where allowed. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** A constant at a finite cut diverges by sliding the included shape-1 support against that cut. [Accessible free-shape faces](#accessible-free-shape-faces).

### bhatt all free truncate:known

**Open.** Joint Gaussian smearing and width or center motion, with normalized overlap tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### bhatt a truncate:known

**Open.** Joint Gaussian smearing and width or center motion, with normalized overlap tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### bhatt a+mu truncate:known

**Proved.** Use the broad-scale uniform or diffusion and the appropriate sharp-scale face or cut collapse. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included. General finite cardinality is open. [Convolution kernels](#convolution-kernels)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### bhatt a+mu+sigma truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### bhatt a+sigma truncate:known

**Proved.** Use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included. General finite cardinality is open. [Convolution kernels](#convolution-kernels)

### bhatt mu truncate:known

**Open.** Joint Gaussian smearing and width or center motion, with normalized overlap tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### bhatt mu+sigma truncate:known

**Proved.** Compare the width endpoints (zero is included). A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included. General finite cardinality is open. [Convolution kernels](#convolution-kernels)

### bhatt sigma truncate:known

**Open.** Joint Gaussian smearing and width or center motion, with normalized overlap tending to zero. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### emg all free truncate:known

**Open.** A moving join at a cut or sample, and joint changes of the physical Gaussian and exponential tail parameters. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### emg lambda truncate:known

**Open.** A moving join at a cut or sample, and joint changes of the physical Gaussian and exponential tail parameters. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### emg lambda+mu truncate:known

**Proved.** Use the broad-scale uniform or diffusion and the appropriate sharp-scale face or cut collapse. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included. General finite cardinality is open. [Convolution kernels](#convolution-kernels)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### emg lambda+mu+sigma truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### emg lambda+sigma truncate:known

**Proved.** Use the distant-location comparison at held Gaussian scale and held shape. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included. General finite cardinality is open. [Convolution kernels](#convolution-kernels)

### emg mu truncate:known

**Open.** A moving join at a cut or sample, and joint changes of the physical Gaussian and exponential tail parameters. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

**Proved case.** On a bounded window [a,b], (a,b) has unattained uniform supremum. [Concentration and endpoint pairs](#concentration-and-endpoint-pairs).

### emg mu+sigma truncate:known

**Proved.** Compare the normal at infinite rate and the zero-rate Gaussian CDF kernel where integrable. A finite fit exists iff a finite feasible point reaches the finite excluded threshold. Only the two stated ends of the scalar parameter can give nonattainment, and Bhattacharjee width zero is included. General finite cardinality is open. [Convolution kernels](#convolution-kernels)

### emg sigma truncate:known

**Open.** A moving join at a cut or sample, and joint changes of the physical Gaussian and exponential tail parameters. The accessible faces give exact path heights. Attainment, the exhaustive failure list and finite cardinality on this mask require the joint comparison. [Accessible free-shape faces](#accessible-free-shape-faces)

### logitnorm all free truncate:known

**Proved.** For nonconstant logits compare their variance with that of the mean-matched linear tilt. The applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask. Every attained fit is unique. [Log-scale reductions](#log-scale-reductions)

**Proved case.** The sample (1/4,3/4) on [1/4,3/4] escapes to a law uniform on the logit scale. [Log-scale reductions](#log-scale-reductions).

### logitnorm mu truncate:known

**Proved.** Use the projected minimum and the uniform mean of the squared logit residual. The applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask. Every attained fit is unique. [Log-scale reductions](#log-scale-reductions)

### logitnorm mu+sigma truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### logitnorm sigma truncate:known

**Proved.** The average logit must lie strictly between the transformed cuts. The applicable failures are projection or cut collapse and, when scale is free, the excluded logit tilt or logit-uniform specified by the held mask. Every attained fit is unique. [Log-scale reductions](#log-scale-reductions)

### dblpareto all free truncate:known

**Proved.** Compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

### dblpareto left_rate truncate:known

**Proved.** Compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

### dblpareto left_rate+right_rate truncate:known

**Proved.** Compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

**Proved case.** At rates 1 and 1, (1,1) on [1,e] has every kink scale in (0,1] maximizing. [Double Pareto on the log scale](#double-pareto-on-the-log-scale).

### dblpareto left_rate+right_rate+scale truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### dblpareto left_rate+scale truncate:known

**Proved.** Use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

### dblpareto right_rate truncate:known

**Proved.** Compare the held-kink boxes at the sample knots and finite cuts and restore all winning exterior plateaus. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

### dblpareto right_rate+scale truncate:known

**Proved.** Use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

### dblpareto scale truncate:known

**Proved.** Use the held-kink rate box. Attainment holds iff a finite component winner reaches every component supremum. If no component attains the global height, the winning zero-rate, infinite-rate or collapse component gives the failure. The entire maximizing set is given by the finite winning components, with inactive rates and exterior plateaus restored. [Double Pareto on the log scale](#double-pareto-on-the-log-scale)

### lgamma all free truncate:known

**Proved.** Exclude constants and test the transformed gamma shape edge, rate edge and corner by the inward moments. The applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition. Every attained fit is unique. [Log-scale reductions](#log-scale-reductions)

**Proved case.** The sample (e,e^4) on [e,e^4] has an unattained log-gamma corner supremum. [Log-scale reductions](#log-scale-reductions).

### lgamma ratelog truncate:known

**Proved.** Match the mean of log log x between the zero-shape and upper thresholds. The applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition. Every attained fit is unique. [Log-scale reductions](#log-scale-reductions)

### lgamma ratelog+shapelog truncate:known

**Proved.** A compatible held model with positive likelihood is the sole feasible model. A compatible fixed model has no parameter escape. The fit is unique.

### lgamma shapelog truncate:known

**Proved.** Match the mean of log x between the lower cut and the zero-rate mean. The applicable transformed gamma edges and corner win when their moment tests pass. Projection or cut collapse occurs under the stated scalar or constant-data condition. Every attained fit is unique. [Log-scale reductions](#log-scale-reductions)
## Summary

The entries below describe the analyzed limits on proper windows. Usual zero-density ends are included in the comparisons. For an open mask, a listed path supplies an accessible value and the corresponding per-mask section identifies the unresolved joint comparison.

| Family | Bounded window | Right ray | Left ray |
| --- | --- | --- | --- |
| GED | Held shape gives uniform, signed tilts or collapse. Shape alone gives the finite-width uniform or a cut atom. | For held shape only decaying tilt and lower-cut or centered collapse can compete. Shape-only uniform is clipped at the lower cut. | Reflection of the right ray. |
| SGED | Fixed shape and skew use the GED comparison with side-specific rates. Scalar skew and shape have their own support joins. | Only the decaying remote tilt survives at fixed shape and skew. Shape-only and skew-only paths clip the corresponding half-family or uniform. | Only the increasing remote tilt survives at fixed shape and skew. |
| SN | Held alpha gives both tilts, uniform and collapse. Skew alone gives Gaussian half-faces or cut collapse. | Negative tilts or concentration at lower cut, and valid half-faces at fixed center and scale. | Positive tilts or concentration at upper cut, and valid half-faces at fixed center and scale. |
| snorm | Fixed skew gives the stretched-exponential comparison. Skew alone gives half-normal or cut collapse with a moving-join factor. | Negative slope for held skew when accessible. The skew-only half follows the effective overlap. | Positive slope for held skew when accessible. The skew-only half follows the effective overlap. |
| sstd | With power shape and skew held, uniform, exterior powers and critical ties. With shape or skew alone free, compare the two explicit scalar ends. | Exterior power can compete from below the cut. The normal-skew and small-df faces apply when accessible. | Exterior power can compete from above the cut. The same small-df comparison uses the opposite projected cut. |
| ST | Fixed df and skew use the power comparison. Infinite skew or df supply explicit shape paths. | Fixed shape allows exterior power below the cut. | Fixed shape allows exterior power above the cut. |
| LST | Power envelopes and, with df free, normal, tilted or reciprocal-distance limits. | Exterior power or a decaying large-df tilt, with the stated divergence tests. | Exterior power or an increasing large-df tilt, with the stated divergence tests. |
| Slash | Effective inverse-square power and critical collapse, uniform at infinity. Exact reflected pair with held scale. | Exterior inverse-square below the cut or critical or divergent collapse. | Exterior inverse-square above the cut or critical or divergent collapse. |
| Huber | Exhaustive comparison of normal, Laplace, signed tilts, uniform and collapse. | Decaying tilt, normal or Laplace. Divergence at a retained cut or center with the stated holds. | Increasing tilt, normal or Laplace. The concentration test uses the upper cut. |
| HT | Effective uniform, exterior power, tied collapse and upper-cut contact. Free df adds normal and reciprocal-distance alternatives. | Exterior power below the cut, a decaying large-df tilt or tie collapse. | Effective uniform, clipped half-normal, tie collapse or upper-cut contact. |
| Tukey lambda | Held negative shape uses power, zero uses logistic, and positive shape uses contact and broad edge powers. | Only an inward half of the expanding support with bounded effective overlap retains a positive broad edge-power face. | Reflected positive-shape rule, along with the logistic or power comparison for nonpositive shape. |
| Bhattacharjee | Scalar masks use included normal, broad uniform or a clipped uniform with endpoint factors. | Sharp-scale uniform on the clipped component interval, or lower-cut collapse when disjoint. | Sharp-scale uniform on the clipped component interval, or upper-cut collapse when disjoint. |
| EMG | Scalar masks compare normal, translated exponential, broad uniform, Gaussian CDF kernel and eligible remote ends. | The zero-rate CDF kernel loses. A remote decreasing exponential or the sharp translated exponential may compete. | The zero-rate CDF kernel is integrable. The decreasing remote exponential loses. |
| Logit-normal | Exact normal moments on the transformed interval, with Jacobian-weighted tilt. | Use the normal rule on the transformed interval, which can be bounded or unbounded. | Same transformation with its upper cut. |
| Double Pareto | Exact component comparison gives the full maximizing set on the log scale. | The same comparison uses the right-ray rate box when the log window is a ray. | The same comparison uses the left-ray rate box when the log window is a ray. |
| Log-gamma | Exact gamma moment and edge tests in log coordinates. | Rate-zero excluded face loses on an unbounded log interval. | The fixed physical support gives a bounded effective log window when the upper cut is finite. |

## What this changes for the fitter

The proposed routes are mathematical. Logit-normal and log-gamma reduce to existing moments and edge scores in transformed coordinates. Their certificate needs accurate transformed cuts and moments, a signed comparison at equality, and the Jacobian in the reported likelihood. The held-mean, held-shape GED route needs a monotone score bracket in inverse scale to the held power. Double Pareto needs strictly concave active-rate solves at a finite set of log knots and cuts, followed by interval bounds on all component heights. Inactive rates and winning exterior plateaus affect parameter cardinality, even when the conditional law is unique.

The other exhaustive comparisons need whole-neighborhood bounds near each excluded face and root isolation or a certified compact cover on the surviving finite domain. A strict separation above the boundary proves existence. The coefficient accuracy and cardinality claims each require additional work. Endpoint factors, critical products and collapse limits must be evaluated jointly on the sample. Direct CDF subtraction can lose the normalizer in a short retained window or a far tail. Relative kernels and centered statistics give the required numerical scale. [Short-window study](2026-11-22-short-window-report.md) · [Truncated-normal boundary payloads](2026-11-21-tnorm-limit-report.md)

When a successful search has a face comparison that remains unresolved within its allowance, the successful candidate retains its uncertified flag. A separated feasible lower bound that refutes it, a proof of nonexistence, or search failure supplies the corresponding failure. [Wrapper rules](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Numerical checks

**Numerical.** The workstation replay passed 68 mpmath checks at 80 decimal digits. The grid uses steps $10^{-3},10^{-6},10^{-12},10^{-20}$ in 17 calculations. It checks the GED increasing left-ray tilt and its large-shape endpoint factor, the finite-center Huber Laplace face, SN at its half-Gaussian join, the moving snorm join and critical sstd skew contact, the critical Slash product, the Tukey broad edge, a joint LST large-df tilt, both HT small-df tie regimes, two EMG faces, the Bhattacharjee endpoint factors, a double-Pareto infinite rate, and both transformed corner or tilt values. Each row records the finite-path result, predicted limit, relative error and tolerance. These checks sample the formulas at the stated paths. All atlas proposals use analytic proof grades. [Python replay](2026-11-27-trunc-skew-math-data/check.py) · [Results](2026-11-27-trunc-skew-math-data/checks.csv) · [Atlas proposals](2026-11-27-trunc-skew-math-data/proofs.csv)

## Open

- **GED and SGED.** With free shape and location or scale, bound the gamma-normalized uniform-edge layers when the admissible physical support moves with the shape. A sample simultaneously at both limiting edges requires the same location and scale offsets in both numerator factors. SGED also requires uniform bounds when skew compresses one side while shape, scale or mean changes.
- **SN and snorm.** With free skew and an additional free coefficient, the scalar envelopes leave open the use of the moving join to trade a half-family against conditioning on its suppressed side. For SN this requires bounds when the inverse Mills ratio and the Gaussian slope change simultaneously. For snorm, the two physical scales and the kink constraint must hold along the same path.
- **sstd and ST.** The fixed-shape power and scalar skew comparisons do not cover arbitrary coupled Student tails. In sstd the coefficient near df 2 and the shrinking side under extreme skew can have comparable orders. In ST the standardized skew-cdf argument can stay finite near a moving join while its tail equivalent applies at the other data; the required bounds must also handle df going to infinity and change of scale or location. A one-sided tail equivalent at each observation alone cannot give the joint envelope.
- **LST and HT.** The threshold comparison settles attainment, including the distinction between small-df ties on bounded windows and rays when both location and scale are held. Where scale is free and a tie at the held center already gives infinite likelihood, classification of all escaping laws at infinite height remains open. Root isolation and global cardinality remain open beyond the proved subcases.
- **Tukey lambda.** With variable shape the distance to a support edge and the quantile power can vary together, including near the logistic and uniform shapes. With held center the fitted lower endpoint can be −∞. A joint bound must track both ratios at observations and the integral over the retained window.
- **Bhattacharjee and EMG.** With more than one coefficient free, the fixed-component limits need envelopes that include a moving support join within a Gaussian scale of a cut or datum, including paths with vanishing retained probability. Rate-width ratios can change the normalizer on rays. The scalar endpoint likelihoods do not settle these corners.
- **Finite cardinality.** The scalar mean and component rules identify their maximizing sets. In the other threshold comparisons the remaining general question is which finite stationary roots can be global and whether separated optima can tie. The reflected examples and plateaus are exact exceptions. A certified numerical search would need bounds on the whole finite comparison region, especially when a finite winner ties an excluded threshold.
