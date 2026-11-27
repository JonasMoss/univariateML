# Maximum likelihood for truncated positive shape families

TASK-450. The window is closed, intersected with the parent support. Its integral must be positive. Observations are finite and nonempty. Open support endpoints remain excluded. A Fréchet observation at its closed lower support endpoint has density zero and supplies no positive-likelihood fit. Use the fitted domain $\lambda\ge1$ for free Fréchet shape with free location, and positive shape otherwise. For Gamma, TASK-439 already covers all masks in scope on all three windows. Its concavity proof includes the lower-only window, the zero-lower-cut window and the bounded positive window. There is no Gamma mask to add.

## Conditional likelihood and comparisons

**Proved.** For a fixed monotone coordinate $y=T(x)$, use the transformed retained interval $I=(L,U)$ with the original inclusion convention at finite ends, a positive kernel $h$, and
$$Q_I(h)=\frac{\prod_i h(y_i)}{(\int_I h(y)\,dy)^n}\prod_i |T'(x_i)|.$$
Use the interior density at included cut points. This formula also defines the likelihood of each regular face below. In particular a tilt $e^{r y}$ has normalizer $(e^{rU}-e^{rL})/r$ at nonzero $r$ and $U-L$ at zero when finite. On a half-line use the convergent improper integral. In $y=\log x$, this is a truncated power density $x^{r-1}$, log-uniform at $r=0$. In $y=-\log(x-\mu)$ it is a power density $(x-\mu)^{-r-1}$. In the original coordinate it is a signed truncated exponential, uniform at $r=0$. A point mass is a weak limit, with its likelihood envelope specified separately.

**Scalar rule.** For $h=w e^{\theta T}$, the score per observation is $\bar T-E_\theta T$, with derivative $-\operatorname{Var}_\theta T$. The root exists uniquely exactly between the endpoint expectations. At a finite excluded integrable edge the score test gives the edge optimum, including equality. At an infinite tilt toward an included extremizer, equality means all the data are there, and the centered normalizer tends to zero, with weak collapse. An excluded finite edge with divergent integral gives zero likelihood at fixed points with finite carrier. These conclusions follow from the [scalar theorem](2026-11-27-trunc-expfam-math-report.md#scalar-and-two-coordinate-checks). We use moment matching to maximize one-dimensional face families, clipping to the allowed slope range. A face with free positive parameters may itself have limiting edges. In comparisons include those edges, or take the stated face supremum.

**Stationary comparison.** The sample criterion below uses *all* roots of specified scalar scores or derivatives of the displayed likelihood. More explicitly, for a kernel parameter $v$, the equation is $\sum_i\partial_v\log h(y_i)=nE_h[\partial_v\log h(Y)]$. Include constrained endpoints or joins when specified. If an exhaustive escape bound is $B$ on the ordinary likelihood scale and every feasible finite score candidate is included, attainment is equivalent to at least one candidate having $Q\ge B$ with $Q>0$. A positive supremum above $B$ belongs to a compact parameter sublevel of the escape exhaustion and so attains. A candidate equal to $B$ either wins or a higher finite candidate does. If there is no such candidate and the bound is approached, the winner escapes at height $B$. When $B=0$, any positive witness forces a finite winner. Cardinality is the number of candidates that achieve the largest height, with any parameter plateaus counted in full. This criterion requires solving equations and comparing likelihoods. It supplies no root-count bound, effective equality decision or unconditional uniqueness theorem.

**A concentration bound.** For a log-concave conditional density with two distinct observed points, a sequence with positive lower bound on sample likelihood has bounded log densities at all sample points. In fact, if a sample density $H\to\infty$, an observation at distance at least half the sample range has density $D$ with $\int f\ge d(H-D)/\log(H/D)$ by log-linear interpolation (with continuous interpretation at equality). Thus $D$ is exponentially small in $H$, while every sample density is at most $H$ when $H$ is the largest. The product tends to zero. This bounds all observation densities above and then below. Concavity gives a lower bound on the interval between the extremes. A log density going to $+\infty$ at any point in that interval would then contradict the same integral bound from that point to the more distant sample extreme. In particular the slopes on interior compact subintervals stay bounded. For one observation, if a segment from it toward a finite or infinite endpoint has derivative unbounded in the increasing direction uniformly on a fixed subsegment, integrating on that segment bounds its density by a quantity tending to zero.

### Gumbel kernel

**Proved.** Consider $h(y)=\exp(sy-c e^{sy})$, $s,c>0$. It is log-concave. With $s$ tending to a positive finite value, $c\to0$ gives the tilt of slope $s$ if $U<\infty$. With $c\to\infty$ the log density differences between any two distinct observations diverge, or a single observation is at the finite lower cut and collapses there. At an interior observation in that case, integrate over a fixed interval to its left to show vanishing density. With $s\to0$, if $sc\to r<\infty$, the log kernel centered at any fixed pivot converges at each finite point to the linear tilt of slope $-r$, by $e^{sy}=1+sy+O(s^2)$. For these pointwise finite limits Fatou gives divergent normalization when the limiting integral is infinite. When it is finite, dominated convergence applies on compact sets, and on each unbounded side a log-concave tangent with slope bounded away from zero in the correct direction gives a common integrable bound. If $sc\to\infty$, the same lower-side collapse or vanishing argument applies as for $c\to\infty$ at finite $s$.

With $s\to\infty$, bounded log density differences at $u<v$ would require $c(e^{sv}-e^{su})=s(v-u)+O(1)$. At the midpoint the exponential term and its value at $u$ are negligible compared with $s$, and the log density rises by $s(v-u)/2+o(s)$ above its value at $u$. This contradicts the concentration bound. The all-free constant sample at $y_0$ diverges on $c=e^{-sy_0}$ by scaling $t=s(y-y_0)$, including one-sided conditioning at a cut. The limiting mass is at $y_0$. With $c=e^{-sd}$ for fixed $d$, the only large-$s$ collapse point is the projection of $d$ onto the retained interval. At $d$ rescale as before. Below $d$, on an interval ending at $U<d$, the log slope is asymptotic to $s$ uniformly and the upper endpoint density diverges. Above $d$, on an interval starting at $L>d$, the log slope is negative and bounded above by its value at $L$, which tends to $-\infty$, so the lower endpoint density diverges. These estimates give weak collapse by comparing integrals on separated intervals. A single observation away from the projection has vanishing density by comparing with a subinterval strictly closer to it, on the same side of $d$ as the observation. This also covers constant samples off the projection.

For the two-free-parameter kernel one can reduce the stationary comparison to one shape profile. At fixed $s$, $t=e^{sy}$ has a negative exponential tilt of rate $c$ and uniform carrier. Keep the unique positive rate root when the empirical $t$ mean is strictly above the lower $t$ cut and below the zero-rate mean (infinite if the upper $t$ cut is infinite). Solve the shape score on that profiled set, retaining the fixed rate in the partial derivative with respect to shape. This gives the same interior candidates as the two scores.

### Positive-scale powers with a lower cut

**Proved.** For $0\le y\le W$ ($W=\infty$ allowed) consider the conditional Lomax kernel $(s+y)^{-k-1}$, $s,k>0$. If $W<\infty$, $s\to\infty$ with $k/s\to r$ finite gives $e^{-ry}$, including uniform. If $W=\infty$, the same limit holds for $r>0$, from the exact survival $(1+y/s)^{-k}$. At $r=0$ on the infinite window all fixed densities vanish. When $k/s\to\infty$ and $s$ is bounded away from zero, density at each $y>0$ vanishes and the law collapses to zero. In fact the density is
$$\frac{k}{s}(1+y/s)^{-k-1}\big/[1-(1+W/s)^{-k}],$$
where the bracket is one for infinite $W$. With $s\ge\epsilon$, $s\log(1+y/s)$ is bounded below by a positive number for $y>0$, and the bracket tends to one. Even with observations at zero the joint product tends to zero if any observation is positive, since the zero density then grows only as $k/s$. With all observations at zero and $k/s\to\infty$, the likelihood is unbounded. At finite $s>0$, $k\downarrow0$ gives the carrier $(s+y)^{-1}$ for finite $W$ and vanishing likelihood for infinite $W$. As $s\downarrow0$ with all $y_i>0$, the density at each $y_i$ is bounded by $1/[(s+y_i)\log(1+y_i/s)]$, by integrating the decreasing tilt in $z=\log(1+y/s)$ up to $z_i$. For fixed $k$ and $j>0$ observations at zero, the small-scale envelope follows from
$$Q\sim s^{k(n-j)-j}\,k^n\prod_{y_i>0}y_i^{-k-1}.$$
The weak limit is the lower atom. The likelihood diverges when the exponent is negative, tends to the displayed positive constant when it is zero, and vanishes when it is positive. In the critical case the limiting likelihood need not be the supremum, since the finite-window normalization raises finite-parameter likelihoods. With both parameters free and any observations at zero one can fix a small enough positive $k$ and get divergence. All these conclusions apply to $y=x-L_x$ for effective lower endpoint $L_x$ and $s$ equal to the parent scale plus the distance from the parent endpoint to $L_x$.

### weibull free truncate:known

**Proved.** Take $y=\log x$, $s=\text{shape}$ and $c=\text{scale}^{-s}$ in the Gumbel kernel. Constants diverge with a point mass at their common value. For distinct data use stationary comparison against the best tilt with slope $r\ge0$ when $U$ is finite and with slope $r\le0$ when $L$ is finite. Include only integrable tilts. For $[a,\infty)$ this is the negative-slope power with $r=-1/(\overline{\log x}-\log a)$. For $(0,b]$ it is the positive-slope power with $r=1/(\log b-\overline{\log x})$. For $[a,b]$, $a>0$, the union is the full real tilt family, and matches the empirical log mean, with the log-uniform face at the midpoint. Boundary-only wins give precisely these powers. General pointwise uniqueness of finite winners is open.

### weibull scale truncate:known

**Proved.** In the same coordinate let $d=\log(\text{scale})$. The only regular excluded face is log-uniform on the bounded log window. On a one-sided infinite log window it has zero limiting likelihood. Constants at the projected $d$ diverge. Otherwise the stationary comparison in $s$ gives attainment and boundary failure, including constants off the projection. General finite-winner uniqueness is open.

### weibull scale+shape truncate:known

**Proved.** Evaluate the one specified conditional law. The held-shape-only route is already reviewed in TASK-439.

### gompertz free truncate:known

**Proved.** Use $y=x$, $s=b$ (the repository coefficient), $c=a/b$. For distinct data apply stationary comparison against positive exponential slopes when the upper cut is finite and nonpositive slopes with integrable normalization. Thus on finite windows the best face is the signed exponential matching $\bar x$, including uniform at the midpoint. On $[L,\infty)$ it is the shifted exponential of rate $1/(\bar x-L)$. Constants diverge at their common value even at zero. General finite-winner uniqueness is open.

### gompertz a truncate:known

**Proved.** The regular face is the truncated exponential of rate $a$ on all windows. As $b\to\infty$ the collapse point is the effective lower cut: on $x\ge0$, the log derivative $b-ae^{bx}$ at a positive cut goes to $-\infty$. At cut zero the density at zero tends to $a$, and the mass collapses there since the Gompertz survival at every fixed positive point tends to zero. Consequently an all-zero sample needs separate treatment: its conditional density is $a/[1-\exp(-(a/b)(e^{bU}-1))]$, strictly decreasing in $b$ for finite positive $U$, so the excluded rate-$a$ face wins. For $U=\infty$ the density at zero is exactly $a$ for every $b$, an exact nonunique sample $(0)$ on $[0,\infty)$. For a positive lower cut all-lower-end data diverge; away from the lower endpoint large $b$ gives vanishing density by integrating on a fixed interval to the left. For nonconstant data use the Gumbel two-point bound. All other compatible samples use stationary comparison against the rate-$a$ face. General cardinality there is open.

### gompertz b truncate:known

**Proved.** Use $T=e^{bx}$ and tilt $-a/b$ against $e^{bx}dx$. The transformed law is an exponential on the $T$ interval. A unique finite root requires $T(L)<\bar T<(T(L)+T(U))/2$ for finite $U$, with upper threshold $\infty$ otherwise. Lower equality is collapse; at or above the finite upper threshold the excluded law has density proportional to $e^{bx}$. This is a positive exponential tilt with rate parameter $-b$ in the convention $e^{-rx}$.

### gompertz a+b truncate:known

**Proved.** Evaluate the specified law.

### llogis free truncate:known

**Proved.** In $y=\log x$ use the kernel $e^{s(y-d)}/(1+e^{s(y-d)})^2$, with $s>0$, $d=\log(\text{scale})$. The log slope is $-s\tanh(s(y-d)/2)$, strictly decreasing. As $s\to0$ the slope is bounded in magnitude by $s$, so the pivot-normalized limit is 1. Fatou gives zero likelihood at fixed observations if the interval is infinite, and uniform convergence works if it is bounded. At finite positive $s$, $d\to\pm\infty$ gives slopes $\pm s$ respectively (positive for $d\to+\infty$), with the same tangent and Fatou arguments as above. At $s\to\infty$, away from the transition at $d$ the slope has magnitude at least $s/2$ on some fixed positive-width subinterval strictly between two distinct observations, on taking a subsequence of $d$ in the extended real line. This contradicts the concentration bound for competitive likelihood. Constant data diverge by putting $d$ at their log value and rescaling by $s$. The best regular face for nonconstant data is therefore exactly the power in the Weibull free section, with the same stationary comparison for attainment. General finite cardinality is open.

### llogis scale truncate:known

**Proved.** With $d$ held, the sole regular excluded face is log-uniform on a bounded log window; otherwise its limiting likelihood is zero. Large shape collapses at the projection of $d$. At an included cut below or above $d$, the slopes on the retained side have magnitude at least $s/2$, which bounds the integral relative to the cut density by $2/s$. At $d$ itself use rescaling. Constant data at the projection diverge; elsewhere density vanishes for large shape by comparison with a fixed interval nearer $d$. For other data use the scalar stationary comparison. General finite cardinality is open.

### llogis shape truncate:known

**Proved.** With $s$ held, in $t=x^s$ the kernel is $(t+D)^{-2}$, $D=\text{scale}^s$. Apply the fixed-$k$ lower-cut power calculation with $k=1$, $y=t-T(L_x)$ and $s$ in that calculation equal to $D+T(L_x)$. The $D\to\infty$ face is uniform in $t$ for finite upper cut, or zero likelihood otherwise. The $D\downarrow0$ face is proportional to $t^{-2}$ if the lower cut is positive. At cut zero observed $t_i$ are strictly positive and the likelihood tends to zero. Use the stationary comparison. Finite uniqueness is open.

### llogis scale+shape truncate:known

**Proved.** Evaluate the specified law.

### invexp free truncate:known

**Proved.** Put $y=1/x$, $L=1/b$, $U=1/a$ with the extended endpoint conventions. The kernel is $e^{-\text{scale}\,y}$. A unique positive scale exists exactly when $L<\bar y<(L+U)/2$ on finite reciprocal windows and $\bar y>L$ when $U=\infty$. At the admissible lower equality there is collapse at $x=b$; at or above the upper threshold the excluded power $x^{-2}$ wins. No infinite observation is allowed. Hence on $[a,\infty)$ only the upper-mean test matters, on $(0,b]$ only lower equality matters, and on $[a,b]$ both matter.

### invexp scale truncate:known

**Proved.** Evaluate the specified law.

### power free truncate:known

**Proved.** Write $M=\max x_i$, $L=\max(a,0)$ and $U=b$. With $\alpha$ free, the conditional upper end $u=\min(U,\alpha)$ must be at least $M$. For fixed shape $\beta$ the kernel $x^{\beta-1}$ is independent of $u$, so its likelihood strictly decreases with $u$ on $u>L$. If $M=L$ the width can shrink to zero and likelihood diverges, with a point mass at $L$. Otherwise set $u=M$. In $y=\log x$ the kernel is the tilt $e^{\beta y}$ on $[\log L,\log u]$. A unique positive shape exists iff $\overline{\log x}$ is strictly below $\log u$ and strictly above the log midpoint when $L>0$. When $L=0$ the lower threshold is $-\infty$. Lower failure chooses the log-uniform face on $[L,u]$. At upper equality the excluded infinite shape collapses at $u$. In the lower failure the profile bound at $u=M$ also bounds every wider interval, and the face is reached with $u=M$. If the positive shape exists, the endpoint coefficient is uniquely $M$ when $M<U$. When $M=U$ all $\alpha\ge U$ attain. Exact sample $(1,2)$ on $(0,2]$ has the full ray of maxima at $\beta=2/\log 2$.

### power alpha truncate:known

**Proved.** Use the same positive log tilt with $u=\min(U,\alpha)$ held fixed. The same two strict inequalities give a unique shape and the same log-uniform or collapse failure applies. The window needs positive width, independently of the sample.

### power beta truncate:known

**Proved.** Use the endpoint argument above. A finite endpoint estimate exists iff $M>L$. It is unique when $M<U$, and every $\alpha\ge U$ maximizes when $M=U$. Otherwise the shrinking interval gives unbounded likelihood and a lower atom. The exact sample $(1,2)$ on $(0,2]$ works at every held positive shape.

### power alpha+beta truncate:known

**Proved.** Evaluate the specified law on the effective interval.

### lomax free truncate:known

**Proved.** Shift by the effective lower cut $a_0\ge0$ and let $s=a_0+1/\lambda$, $k=\kappa$. Here $s>a_0$. Use the lower-cut power calculation. At a fixed $s$, fit $k$ by the scalar rule in $z=\log(1+y/s)$, with slope $-k$ and uniform carrier. In particular on an infinite upper window the root is $1/\bar z$ if $\bar z>0$. On finite width it requires $0<\bar z<\log(1+W/s)/2$. For samples not covered by an unbounded branch, keep positive roots and apply stationary comparison in $s$ after profiling, or use both scores before profiling. The regular escape candidates are:

- $s=a_0>0$ with $k>0$, a truncated power in $x$ with exponent $-k-1$. Optimize by the same log-tilt rule at $s=a_0$.
- $k=0$ at finite positive $s\ge a_0$, only on finite windows, the density proportional to $(s+y)^{-1}$. Its supremum can be found by comparing its stationary points in $s$ and its endpoint heights.
- $s\to\infty$, the shifted exponential of rate $r\ge0$ on finite width, or $r>0$ on infinite width. Fit the mean in that exponential family with this rate restriction.

All shapes and regular limits named here have likelihood $Q_I$. The only other branches are those of the lower-cut power calculation. In particular all-lower-cut samples diverge, and if $a_0=0$ and that cut is an admissible observation, even one observation there makes the both-free likelihood unbounded. For Lomax itself the open parent endpoint excludes this last observation. The scalar profile and escape set give an exact attainment comparison on all three windows, including equality. General cardinality is open.

### lomax kappa truncate:known

**Proved.** Hold $k$ in the same kernel. Compare the $s$ score roots to the truncated power at $s=a_0>0$ and to uniform at $s=\infty$ for finite $W$. On the upper-infinite window the latter contributes zero. At $a_0=0$ with strictly positive data the lower escape contributes zero. It contributes the collapse envelope in the lower-cut lemma if contact at zero is allowed by the parent (used below for Pareto II). With held positive shape the same stationary comparison applies to the finite critical envelope. General cardinality is open.

### lomax lambda truncate:known

**Proved.** Hold $s$. The unique positive root requires $0<\bar z<\log(1+W/s)/2$ for finite width, and $\bar z>0$ for infinite width. Zero gives unbounded lower collapse and at or above the finite upper threshold the excluded translated reciprocal density wins. The original Lomax support may exclude cut contact.

### lomax kappa+lambda truncate:known

**Proved.** Evaluate the specified law.

### pareto2 min truncate:known

**Proved.** Use the Lomax free proof in excess coordinates, allowing observation zero, since Pareto II has a closed threshold with inside density. Thus put $y=x-\max(a,\mu)$, $s=\sigma+\max(a,\mu)-\mu$ and $a_0=\max(a,\mu)-\mu$ in that section. Shape and finite candidates have the same comparison. At $a_0=0$ and with any threshold observations, the small-scale path with small positive shape diverges. If $a_0>0$ and all data equal the cut, infinite shape gives lower collapse. General finite uniqueness is open.

### pareto2 min+shape truncate:known

**Proved.** Use the held-$k$ Lomax comparison with the preceding shift. With observations at the closed parent threshold, include the zero-scale envelope and its strict, critical and vanishing regimes. A positive critical envelope is a candidate even though its limiting law is an atom. General finite uniqueness is open.

### pareto2 min+scale truncate:known

**Proved.** Use the Lomax fixed-$s$ scalar criterion with the preceding shift, including lower collapse if the observations are all at the effective lower cut.

### pareto2 min+scale+shape truncate:known

**Proved.** Evaluate the specified law.

### pareto2 free truncate:known

**Proved.** For threshold $\mu$, scale $\sigma$ and shape $k$, the conditional kernel is $(x-p)^{-k-1}$, $p=\mu-\sigma$. At fixed pole $p$ and shape, raising the threshold to $m=\min x_i$ strictly improves the likelihood whenever this removes positive retained width and the new interval has positive width. When $m<U$ the branch $\mu=m$ covers every feasible pole $p<m$, and has $y=x-m$. Lower-contact small-scale divergence follows by choosing $k(n-j)<j$ where $j$ is the multiplicity of $m$. The conditional laws tend weakly to the atom at $m$. If $m=U$, all observations are at the upper cut. Holding the pole fixed and bringing $\mu\uparrow U$ shrinks the interval and the likelihood diverges, with an atom at $U$. No finite MLE occurs.

### pareto2 shape truncate:known

**Proved.** With $k$ held and scale free, the same pole argument applies. When $m=U$ there is width-collapse divergence. Otherwise apply the lower-cut power calculation on $[m,U]$ with $s=m-p>0$. If $k(n-j)<j$ the likelihood diverges. In the other cases compare the $s$ score roots to the finite-window uniform ($s\to\infty$) and, at equality $k(n-j)=j$, the finite collapse envelope. On an unbounded upper window the infinite-scale likelihood vanishes. Every winning pole corresponds to exactly $\mu=m$ when $m>a$ and to every $p<\mu\le m$ (with $\sigma=\mu-p$) when $m=a$. For a lower cut at the sample minimum, the conditional density never sees $\mu$ in that range. Exact nonunique example on $[1,\infty)$: data $(1,2)$ and held $k=2$ give kernel likelihood $4s/(1+s)^3$, with unique pole scale $s=1/2$ and a continuum of thresholds as above. Away from the plateau, general root cardinality on finite windows is open.

### pareto2 scale truncate:known

**Proved.** Let $\sigma$ be fixed, $L=a$ (use $a=0$ for a closed lower cut at zero) and $U=b$. The threshold is real, so a genuinely open cut $(0,b]$ can also be used, with zero observations forbidden and the same positive-width integrals. For $\mu\ge L$ and $\mu<m<U$, the conditional log density at each observation increases strictly in $\mu$ with shape fixed. Indeed the numerator $(1+(x_i-\mu)/\sigma)^{-k-1}$ increases, and the probability above $\mu$ up to $U$, namely $1-(1+(U-\mu)/\sigma)^{-k}$, decreases or is constant if $U=\infty$. Thus use the separate branch $\mu=m$ and the branch $\mu\le L$. If $m=U$ shrinking width diverges, and if $m<U$ and data are constant, shape going to infinity on the $\mu=m$ branch diverges. For nonconstant data, the branch $\mu=m$ uses the scalar shape criterion with lower endpoint $m$. In the other branch put $s=\sigma+L-\mu\ge\sigma$. Use the joint power calculation restricted to $s\ge\sigma$, including the score along the join $s=\sigma$. The excluded faces are the $k\downarrow0$ translated reciprocal on either branch when $U$ is finite and the $s\to\infty$ decreasing exponential (including uniform on finite width). Both have the likelihood formulas above. Compare all branch candidates to the union of the excluded faces. General finite cardinality is open.

### pareto2 scale+shape truncate:known

**Proved.** Use the same two threshold branches at fixed $k$. If $m=U$ shrinking width diverges. Otherwise compare the value at $\mu=m$, the value at $\mu=L$ and the threshold score roots on $\mu<L$ against uniform on finite width when $\mu\to-\infty$. On an infinite upper window this last path gives zero likelihood. Do not count the joined endpoint twice. This covers constant samples as well when $m<U$. General finite cardinality is open.

### kumar a truncate:known

**Proved.** At held $a$ use $T=\log(1-x^a)$ with tilt $b>0$ and carrier $x^{a-1}(1-x^a)^{-1}dx$. This is uniform carrier in $T$. Its upper endpoint is zero if the lower cut is zero, otherwise $T(L_x)$. Its lower endpoint is $T(U_x)$ if $U_x<1$, otherwise $-\infty$. The unique finite root requires the empirical $T$ mean strictly between the midpoint (or $-\infty$) and the upper endpoint. The finite lower failure gives the excluded reciprocal carrier just specified. At upper equality the likelihood diverges with collapse at $L_x$, if that point is admissible.

### kumar free truncate:known

**Proved.** Use $y=-\log x>0$ and write the kernel $h=e^{-ay}(1-e^{-ay})^{b-1}$. In this coordinate the lower-only $x$ window has finite upper $y$ and lower $y$ zero, the upper-only $x$ window has infinite upper $y$ and lower $y$ positive if the cut is below 1, and the strictly interior bounded window has both positive finite cuts. Intersect with $(0,1)$ in each case. Constants at $x_0$ diverge on $a\to\infty$, $b=x_0^{-a}$. The substitution $t=a(y-y_0)$ yields a Gumbel density and a conditional atom at $x_0$. At the exact observation its parent density is asymptotic to $a/(e x_0)$, so conditioning can only increase it.

For nonconstant data use stationary comparison. The finite regular faces are:

- $a\downarrow0$, $b\to B>0$, with kernel $y^{B-1}$ on finite upper $y$. Maximize over positive $B$ by the scalar log tilt. Allow $B=0$ at a positive lower cut.
- $b\downarrow0$, $a\to A>0$, with kernel $1/(e^{Ay}-1)$ if the lower $y$ cut is positive. Compare the $A$ score roots to the endpoint values. At $A\downarrow0$ the kernel becomes $1/y$ if both cuts are positive and finite. In $x$, the finite-$A$ face is proportional to $x^{A-1}/(1-x^A)$, and the first face to $(-\log x)^{B-1}/x$.

The first limit follows by dividing by $a^{b-1}$ and using $(1-e^{-ay})/(ay)\to1$. On finite upper $y$ an integrable majorant when the lower cut is zero follows from $b$ bounded below by a positive constant. The second limit follows by continuity and the $e^{-ay}$ upper tail bound when upper $y$ is infinite. Fatou applied to a kernel centered at a positive finite pivot handles all nonintegrable edges and the nonintegrable zero-zero corner. The same expansion gives the integrable zero-zero corner on an interior window, and its likelihood is $Q_I(1/y)$.

For $b\to\infty$ with $a$ bounded, the log slope $-a+(b-1)a/(e^{ay}-1)$ tends uniformly to $+\infty$ on a compact interval strictly between two observations, including when $a\to0$. This contradicts the log-concavity bound. For $a\to\infty$, $b\ge1$, bounded slopes at two distinct interior points would require $(b-1)/(e^{ay}-1)\to1$ at both, which is impossible. For $a\to\infty$, $b<1$, the factor $(1-e^{-ay})^{b-1}$ is between 1 and 2 on any compact positive $y$ interval eventually. At the smallest observed $y$ the density is $O(a)$ by integrating to its right for a distance $1/a$. On every larger observed $y$ the exponential density ratio is at most $2e^{-a(y-y_{\min})}$. This proves vanishing likelihood. These arguments exhaust the nonconstant escape paths. If the regular-face list is empty set $B=0$ in the stationary criterion. General finite cardinality is open.

### kumar b truncate:known

**Proved.** Hold $b$ in the Kumar kernel. For $a\downarrow0$ the power in $y$ just described is eligible exactly when the upper $y$ cut is finite. For $a\to\infty$ nonconstant data have vanishing likelihood. At a positive lower $y$ cut all-lower-cut data have diverging likelihood, since the density there grows as $a$ and the law collapses there. At every other single observed $y$, choose a compact subinterval strictly to its left and bounded away from zero, and use the bound on $(1-e^{-ay})^{b-1}$ there to show vanishing density. Apply stationary comparison in $a$ on all other data. At $b=1$ the kernel is the negative exponential tilt in $y$, so the scalar rule gives a unique finite root when the empirical mean lies between the lower cut and the midpoint (infinite if no upper cut). General finite cardinality for other $b$ is open.

### frechet mu truncate:known

**Proved.** Put $y=-\log(x-\mu)$, $s=\lambda$, $c=\sigma^s$ in the Gumbel kernel. Reverse the interval, taking its infinite endpoints after support intersection. Constants diverge. For distinct data the Gumbel criterion uses the best positive tilt when the upper $y$ cut is finite and the best nonpositive tilt when integrable. Thus the lower-only excess window has the positive-slope power, the upper-only excess window has the negative-slope power, and a two-cut excess window uses the full signed power family. Match $\bar y$ with clipping if needed, as for Weibull. General uniqueness is open.

### frechet mu+sigma truncate:known

**Proved.** Use the fixed-$d$ Gumbel comparison, with $d=-\log\sigma$. The only regular face is log-uniform in positive excess on a bounded log-excess window. Constants at the projected excess scale collapse; otherwise use the stationary comparison. Finite cardinality is open.

### frechet lambda+mu truncate:known

**Proved.** With $s=\lambda$ held put $t=(x-\mu)^{-s}$ and fit the negative exponential tilt with rate $\sigma^s$. The criterion and uniqueness are the inverse exponential scalar result with these transformed cut endpoints and sample mean. The zero-rate face on a finite $t$ window is the truncated power $(x-\mu)^{-s-1}$. Lower $t$ equality collapses at the finite upper $x$ cut.

### frechet lambda+mu+sigma truncate:known

**Proved.** Evaluate the specified law at compatible data. The zero-density endpoint has the convention stated at the start.

### frechet lambda truncate:known

**Proved.** Write $s=\lambda$, $m=\min x_i$, $j=\#\{x_i=m\}$, $g_i=x_i-m$, $d=m-\mu>0$, $q=(\sigma/d)^s$. Constants diverge by taking $d\downarrow0$ with $q$ fixed when $m<U$, or by shrinking the retained width when $m=U$. For distinct data set $E=s(n-j)-j$. If $E<0$, $d\downarrow0$ at fixed $q>0$ gives an unbounded lower atom likelihood. In all other cases compare both score roots with the following faces:

- Finite location $\mu_0<a$, scale zero. The law is proportional to $(x-\mu_0)^{-s-1}$ on the window, bounded or lower-only. Include the supremum over $\mu_0<a$. For $\mu_0\uparrow a$ with $m>a$ its likelihood vanishes. For $m=a$ it joins the contact analysis below.
- $\mu\to-\infty$ on finite windows. The possible regular laws are increasing exponentials of slope $r\ge0$, including uniform. Use the empirical mean and clip the slope at zero. On lower-only windows these tilted integrals diverge.
- Critical contact $E=0$ at $d=0$. Compare the explicit envelope below. This is an atom at $m$ with a sample likelihood that depends on the approach.

To verify contact, the CDF at $x=m+g$ is $\exp[-q(d/(d+g))^s]$. In the limit $d\downarrow0$ the lower window CDF is zero if $a<m$, or $e^{-q}$ if $a=m$. Write $v=(d/(U-m+d))^s$ for finite $U$ and $0$ for $U=\infty$. The denominator is exactly $e^{-qv}$ in the first case once $\mu>a$, and $e^{-qv}(1-e^{-q(1-v)})$ in the second. The numerator product is
$$s^n q^n d^E\prod_{g_i>0}(d+g_i)^{-s-1}
  \exp[-q\textstyle\sum_i (d/(d+g_i))^s].$$
After removing $d^E$, the ratio is uniformly bounded in $q$: large $q$ is bounded by a polynomial times $e^{-jq/2}$ for small enough $d$, and near zero $1-e^{-q(1-v)}$ is bounded below by a constant times $q$. Thus $E>0$ gives zero. For $E=0$ the exact envelope is
$$s^n \prod_{g_i>0}g_i^{-s-1}\,
\sup_{q>0}\frac{q^n e^{-jq}}{D(q)^n},\qquad
D(q)=\begin{cases}1&a<m,\\1-e^{-q}&a=m.\end{cases}$$
Include the continuous $q=0$ value, attained as a limit. The maximizing $q$ when $a<m$ is $n/j$. The conditional law tends to an atom at $m$, also on the path $q\to0$ when $a=m$, as follows by dividing the CDF difference at any fixed $x>m$ by the retained probability. For $a<m$, a path with $q\to0$ still has mass concentrating near $\mu\to m$ and has zero likelihood at critical contact.

For the other escapes, with $\mu$ finite and bounded away from $m$, scale zero gives the stated power by cancellation of $\sigma^s$, when the cut is strictly above the limiting location. If the cut is at or below the limiting location, the kernel centered at $m$ has divergent integral by Fatou from the singularity on the right of that location. Scale infinity has divergent positive log slopes on compact intervals to the right of $\mu$, so all sample densities vanish on an unbounded window. On a finite window it collapses at $U$. The density there is of order $\sigma^s$ (integrate on a left interval of length $\sigma^{-s}$), while any point strictly below $U$ has an exponential loss in $\sigma^s$ relative to the density at $U$. When $\mu\to-\infty$, on compact intervals the log slope is
$$-\frac{s+1}{x-\mu}+\frac{s\sigma^s}{(x-\mu)^{s+1}}.$$
If $s\sigma^s/d^{s+1}\to r<\infty$, pivot normalization gives the stated exponential and Fatou handles the unbounded window. If the ratio diverges the slopes are uniformly asymptotic to that ratio on compacts, and the preceding comparison and short-interval integral give vanishing likelihood for distinct data. This proves the escape exhaustion. General finite uniqueness is open.

### frechet lambda+sigma truncate:known

**Proved.** With both positive coefficients fixed, vary $\mu<m$. As $\mu\to-\infty$ the only regular face is uniform on a finite physical window by the slope calculation above. On a lower-only window Fatou gives zero limiting likelihood. If $m<U$, letting $\mu\uparrow m$ sends $q\to\infty$ at polynomial rate in $1/d$, giving zero likelihood by the contact formula, including constants with $m<U$. For $m=U$, fix the positive scale and shape and bring $\mu\uparrow U$. The support intersection then has shrinking width, and the density is increasing throughout that intersection for sufficiently small width by the displayed slope formula. Its upper endpoint density is at least the reciprocal width. Thus likelihood diverges and the law converges to the upper atom. Otherwise apply the threshold score comparison. General finite cardinality is open.

### frechet sigma truncate:known

**Proved.** With held scale $\sigma$ and free $\lambda\ge1$, constants diverge by setting $\mu=m-\sigma$ and taking $\lambda\to\infty$. For nonconstant data the only competitive regular escapes are $\mu\to-\infty$ with $\lambda/d\to r\ge0$, giving the decreasing exponential $e^{-rx}$ on finite windows and $r>0$ on lower-only windows. To see convergence, on compact $x$ intervals the inverse-power factor $(\sigma/(x-\mu))^\lambda$ tends uniformly to zero. The log slope tends to $-r$, giving the bounded-window convergence and, for $r=0$, the divergent-normalizer result on an unbounded upper window. For $r>0$ there, use the exact CDF difference or survival $1-e^{-(\sigma/(x-\mu))^\lambda}$; the conditional survival tends to $e^{-r(x-a)}$. If $\lambda/d\to\infty$ the log slopes on compact intervals are asymptotic to $-\lambda/d$. Integrating on a right interval of length $d/\lambda$ at the smallest observation bounds its density by $O(\lambda/d)$, and the other observation has exponentially smaller density.

For finite limiting location away from $m$, bounded shape is an ordinary parameter limit, allowing $\lambda=1$. For $\lambda\to\infty$ apply the large-shape Gumbel argument between any two observations in the reversed log-excess coordinate. Their coordinate distance has a positive finite limit and their Jacobians stay bounded, so the contradiction is unchanged. At $\mu\uparrow m$ and bounded shape, the contact formula again gives vanishing likelihood at fixed scale. At $\mu\uparrow m$ and $\lambda\to\infty$, choose a fixed subinterval strictly between $m$ and the next distinct observation. Its excesses stay bounded away from zero. At the minimum the unnormalized log density includes $-(\sigma/d)^\lambda$ and Jacobian $1/d$. Over the chosen subinterval all log densities differ from the unnormalized maximum over the nonminimum observations by at most $O(\lambda+C^\lambda)$ for some fixed $C>1$, after discarding parameter-only factors. The negative term at $m$ dominates $\lambda|\log d|+C^\lambda$. Lower bounding the normalizer on that subinterval therefore makes the joint likelihood vanish. Compare the regular faces with score roots, also checking the boundary $\lambda=1$ in the fitted domain. General finite cardinality is open.

### frechet free truncate:known

**Proved.** At more than half of the observations at the minimum, choose $\lambda=1$ and use the preceding contact path to get unbounded likelihood and a lower atom. Constants at the finite upper cut can also use width collapse. At exactly half at the minimum and distinct data, the $\lambda=1$ contact formula gives a finite positive approached height. Without the fitted lower bound on shape, choosing $0<\lambda<j/(n-j)$ gives divergence for every distinct sample.

**Proved sharp half-contact bound at the cut.** For distinct data with $j=n/2$ and $a=m$, the supremum is $\prod_{g_i>0} g_i^{-2}$, attained only as a limit. Pair each minimum with a positive gap $g$, and put $t=d/(d+g)$. The retained mass is at least the CDF increment on $[m,m+g]$. Dividing the two parent densities by this increment gives the pair bound
$$
\frac{\lambda^2q^2t^{\lambda+1}}{4d^2\sinh^2(q(1-t^\lambda)/2)}
<\frac{\lambda^2t^{\lambda+1}}{d^2(1-t^\lambda)^2}
\le g^{-2}.
$$
For the last step, $1-t^\lambda\ge\lambda(1-t)t^{(\lambda-1)/2}$ follows from $\sinh(\lambda u)\ge\lambda\sinh u$ with $u=-\log(t)/2$ and $\lambda\ge1$. The strict inequality uses $q>0$. Multiplying over pairs gives the upper bound. The contact formula reaches it by $\lambda=1$, $d\downarrow0$ and $q\downarrow0$. The same bound holds when the lower cut is strictly below the minimum, but sharpness there does not follow.

**Proved regular paths.** In addition to the fixed-shape paths, $\lambda\to\infty$ with $\mu+\sigma\to z$ and $\sigma/\lambda\to t>0$ gives Gumbel maximum CDF $G(x)=\exp[-e^{-(x-z)/t}]$. Indeed $\lambda\log[(x-\mu)/\sigma]\to(x-z)/t$. Both the CDF and the density converge at every finite window endpoint and data point. The face likelihood is $\prod_i G'(x_i)/(G(U)-G(a))^n$, with the extended endpoint conventions. This yields a computable lower bound on the superior escape height. **Open.** For minimum minority or exact half strictly above the lower cut, the complete constrained escape comparison under truncation and the cardinality of any finite winner remain unresolved. In particular, the untruncated pair bound for exact half does not absorb the parameter-dependent retained-mass divisor. Joint corner paths with $\lambda$, location gap and scale varying must be bounded before a complete criterion can be asserted.

### gamma free truncate:known

**Proved in TASK-439.** The [Gamma section](2026-11-27-trunc-expfam-math-report.md#gamma-free-truncateknown) gives the two inward edge scores and the corner test, even when one or more edges have divergent normalizer. Constants collapse. No gap under the window and coefficient conventions used here.

## Exact face and non-uniqueness examples

**Proved.** In a finite transformed interval $[L,U]$, take the two endpoints, each once. For any log-concave normalized density $f$ on this interval, integrate the exponential chord of $\log f$ and apply Jensen on the interval. This gives $1\ge (U-L)\sqrt{f(L)f(U)}$, strictly for a strictly log-concave density. The uniform in that coordinate therefore wins strictly whenever it is an excluded face. This gives exact nonattainment for Weibull and log-logistic, both free or scale held, at data $(1,2)$ on $[1,2]$, for both-free Gompertz at $(1,2)$ on $[1,2]$, and for Fréchet with location held at $\mu$ at $(\mu+1,\mu+2)$ on that same window. It also gives the Fréchet result with scale additionally held. The Jacobian factors multiply both likelihoods equally. The exact attained non-singletons are the endpoint plateaus for [power](#power-free-truncateknown) and [Pareto II](#pareto2-shape-truncateknown), and the all-zero unrestricted-upper-window fit with [Gompertz coefficient a held](#gompertz-a-truncateknown).

**Proved interior witness.** For Weibull with both parameters free, take log-data $(-1/100,1/100)$ on the log window $[-1,1]$. The best power has slope zero and log-coordinate likelihood $1/4$. The trial $s=4,c=1$ has $4y-e^{4y}>-3/2$ at both observations, since $e^{1/25}\le25/24$. Its normalizer is at most the full-line integral $1/4$, by substitution $t=e^{4y}$. The trial likelihood exceeds $16e^{-3}>1/4$, so a finite maximum exists. The change-of-variable factors are constant in the parameters.

## Summary

In the table, “compare” means the exact stationary comparison with the displayed escape height, including a finite tie. Scalar moment tests give the exact coefficient or excluded edge.

| Family | Held masks | Lower-only, upper-only and bounded windows |
| --- | --- | --- |
| Weibull | free, scale | Compare against the eligible signed power or log-uniform. Check the common observation or projected held scale for collapse. |
| Log-logistic | free, scale, shape | Compare against signed powers, log-uniform or the fixed-shape power endpoints. |
| Gompertz | free, a, b | Signed exponential comparison, fixed-rate comparison or scalar moment test. All-zero data with held a are a separate case. |
| Inverse exponential | free | Reciprocal-mean test, power at zero rate, upper collapse. |
| Power | free, alpha, beta | Log-mean test with the fitted or fixed effective endpoint. Free alpha can have an attained plateau. |
| Lomax | free, kappa, lambda | Lower-cut power comparison or scalar log-mean test. Exponential, power and translated reciprocal faces. |
| Pareto II | min held | Shifted Lomax comparison, with closed threshold contact. |
| Pareto II | free, shape, scale, scale+shape | Free pole reduction or comparison of two threshold branches. Include the contact envelope and threshold plateaus where applicable. |
| Kumaraswamy | free, a, b | Compare two power or reciprocal edges, or use the held-a scalar tilt. Clip the window to the parent upper endpoint 1. |
| Fréchet | mu held | Reversed Gumbel kernel or inverse-power scalar tilt on the excess window. |
| Fréchet | lambda, lambda+sigma, sigma | Compare eligible physical exponential or power limits, plus the critical contact envelope with lambda held. |
| Fréchet | free | Proved divergence in the minimum-majority branch. At exact half and lower-cut contact, the finite atom envelope is the strict supremum. Complete comparison open elsewhere. |
| Gamma and previously reviewed Weibull shape hold | applicable masks | TASK-439 already covers these. |
| All held | all applicable families | Evaluate on the compatible positive-mass window. |

## What this changes for the fitter

The new routes remain unimplemented. The scalar cases need a strict moment bracket, log-domain normalizers and coefficient-width stopping tests. Piecewise support routes need to check positive retained width and carry the maximizing coefficient set when an endpoint is hidden by a cut. A stationary comparison needs a compact cover of all competitive roots and bounds excluding unresolved regions, including finite joins and small-scale corners. Bound the conditional likelihood directly near a face or a cut. Raw CDF subtraction may lose the retained mass on a short window. A tied finite maximizer and a tied excluded face leave the finite fit admissible. At critical contact, carry the limiting joint sample height separately from the label of the weak atom. When a successful search remains unrefuted and a certificate cannot decide, preserve the search result with its uncertified flag.

## Numerical checks

**Numerical.** The [reproducer](2026-11-27-trunc-shape-math-data/check.py) evaluates the analytic limits and exact examples at 80 and 120 decimal digits on the workstation. The [table](2026-11-27-trunc-shape-math-data/checks.csv) records the actual grid and absolute discrepancies, including both critical Fréchet contact denominators, the small-scale Pareto II envelope, transformed powers, both Kumaraswamy edges, the Gumbel limit, and the exact plateau examples. It also evaluates a bunched two-point sample whose finite Weibull trial exceeds the exact best power height. These checks test specified comparisons and formulas. The finite Weibull witness also has a direct inequality proof above.

From the worktree root, run:

    export TMPDIR="$PWD/build/tmp"
    mkdir -p "$TMPDIR"
    data=dev/research/evaluations/2026-11-27-trunc-shape-math-data
    /tmp/umvl-cpu/run nice -n 10 python3 -B "$data/check.py" > "$data/checks.csv"
    /tmp/umvl-cpu/run nice -n 10 python3 -B "$data/atlas.py"

The [atlas rows](2026-11-27-trunc-shape-math-data/proofs.csv) use the modifier-atlas schema. The checker verifies masks, key uniqueness, evidence anchors and the exclusion of already reviewed rows.

## Open

1. **All-free Fréchet with a minimum minority or an exact half strictly above the cut.** An exhaustive compactification still needs the joint $\mu\uparrow m$, $\lambda\to\infty$, variable $\sigma$ bounds and the simultaneous large-negative-location, variable-shape tail comparison. At exact half strictly above the lower cut, the conditional pair bound has yet to be sharpened to the best approached height. For the Gumbel competitor, a certified comparison needs control of the face's own conditional location-scale optimization.
2. **Pointwise finite cardinality on the comparison routes.** The scores and exact comparisons identify candidates, but where general cardinality is marked open there is no proof here controlling multiple roots and exact ties across distinct candidates. The base Lomax and Fréchet tie constructions do not transfer by subtracting the conditional normalizer, which varies with the fitted parameters. The exact endpoint and all-zero plateaus above do transfer directly by the conditional formulas.
3. **Effective tests near an exact tie.** The stationary criterion is mathematical. Numerical certification would need root isolation or equivalent bounds, analytic control of the candidate-to-face gap near an approaching boundary, and an exact equality rule where strict height separation cannot be guaranteed. For critical Pareto II and the remaining critical Fréchet branches, comparing the asymptotic height to potential finite winners is still required.
