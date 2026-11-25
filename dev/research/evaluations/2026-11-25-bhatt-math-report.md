# Bhattacharjee existence, uniqueness and faces (TASK-353)

Global finite uniqueness fails even with all three coefficients free. An exact symmetric sample ties a normal member to a positive-width maximum. At held Gaussian scale there is an exact sample with **exactly two** positive-width maxima, and the same construction works with location free or held at zero. A third sample ties a positive-width maximum to the excluded uniform face. All three equalities are real crossings. Their binary64 status is open. The finite stationary-width criterion below covers all hold masks and distinguishes attainment at equality from escape.

## Existence and the faces

Take a nonempty finite sample and valid holds. Write $m$ for the held location or, if it is free, the arithmetic mean, and put $V=n^{-1}\sum_i(x_i-m)^2$. Write $r$ for the maximum distance from held location or, if location is free, half the sample range. When $r>0$, let $c$ be the held location or the midrange, respectively. The tests $r=0$ and $V=0$ are equivalent and reduce to exact equality of all observations with a compatible centre.

| Holds | Exact condition and outcome |
|---|---|
| $\sigma$ and $a$ held, with either location mask | Always a unique attained point. Free location has a strictly concave likelihood. With location held the feasible set is a singleton. |
| $a=0$ held, $\sigma$ free | If $V>0$, the unique normal fit has $\sigma=\sqrt V$ and location $m$. At $V=0$, scale collapse is unbounded. |
| $a>0$ held, $\sigma$ free | If $r>a$, the unique positive-scale maximum is attained. If $r\le a$, the uniform limit strictly dominates every finite fit. The equality penalties are given below. |
| $a$ free, $\sigma$ held | Always attained. The normal member is the unique global fit exactly when $V\le\sigma^2$, including equality. Above that threshold all global fits have positive $a$ and the stationary-width criterion applies. |
| Both scales free | At $V=0$ the likelihood is unbounded. At $V>0$, compare the finite candidate maximum $M$ from the stationary-width criterion with $U=-n\log(2r)$. An MLE exists exactly when $M\ge U$. At $M=U$ the finite maximizers are attained. When $M<U$, report the uniform limit. |

The held-width claims use the strict endpoint Hessian and complementary endpoint comparison already proved in TASK-116.43. With free location and $r=a>0$, let $j,l$ count the lower and upper endpoints. The limiting log likelihood is
\[
-n\log(2a)+j\log\frac{j}{j+l}+l\log\frac{l}{j+l}.
\]
The limiting centre is the midrange. If $r<a$ and location is free, every centre in $[x_{\max}-a,x_{\min}+a]$ can be the limit of a supremizing sequence. At an end of this centre interval choose centres approaching it from inside with inward displacements large compared with $\sigma$. With held location, subtract $\log 2$ per endpoint observation from $-n\log(2a)$, including zero penalty when $r<a$. The density bounds are strict at finite scale. For example, $x=(-1,1)$ at held $a=1$ has parent supremum $-4\log2$ with or without holding $\mu=0$. The closed uniform on $[-1,1]$ has log likelihood $-2\log2$. These are different quantities even with exactly representable endpoints. [Held-width proof](2026-11-08-bhatt-cert-report.md#positive-physical-width-held)

The fixed-$\sigma$ normal comparison at $V\le\sigma^2$ is the strict truncated-normal variance bound from TASK-116.58. Necessity follows by holding location at $m$ and expanding in $k=a/\sigma$. The $k^2$ coefficient is $n(V/\sigma^2-1)/6$, so $V>\sigma^2$ gives an improving feasible width. An exact free-location comparison uses $n\sum_i x_i^2-(\sum_i x_i)^2$ versus $n^2\sigma^2$, with equivalent anchored sums available to avoid large offsets. At held location compare $\sum_i(x_i-\mu)^2$ with $n\sigma^2$. [Fixed-scale domination proof](2026-11-09-bhatt-allfree-report.md#proved-normal-domination-with-sigma-held)

When both scales are free and $r>0$, $U$ is reachable at $(\mu,a)\to(c,r)$ by choosing $a-r>0$ with $(a-r)/\sigma\to\infty$. This path gives full density at the endpoints. The normal member is $a=0$ and is in the parameter domain. With both scales free its candidate value is $N=-n\log\sqrt{2\pi e V}$, so $N=U$ exactly when $V=2r^2/(\pi e)$. This comparison alone leaves positive widths to check. The peak bounds exclude diverging scales, and uniformly bounded scales give zero likelihood as location escapes. In joint scale collapse, some observation has distance at least $r$ from the centre. Eventually $a\le r/2$, so bounding its density by $\phi_\sigma(r/2)$ and the others by $\phi_\sigma(0)$ gives a product tending to zero. If $r=0$, the feasible normal path $\sigma\downarrow0$ instead gives unbounded likelihood. A constant sample away from a held centre has $r>0$ and is subject to the same finite criterion as a nonconstant sample. In fact its uniform face wins by the equal-distance bound below. These are all the nonzero escapes for the supported masks. [Previous face analysis](2026-11-09-bhatt-allfree-report.md#global-width-argument-and-acceptance) · [Domain rules](../../roadmap/2026-10-29-parameter-rules.md#rules)

## Finite stationary-width criterion (proved)

For free $a$ put $k=a/\sigma$, $b=1/\sigma$ and $u=b\mu$. The log density at positive $k$ is $\log b+\log D_i-\log(2k)$, where
\[
D_i=\Phi(bx_i-u+k)-\Phi(bx_i-u-k).
\]
Fix $b$ for held scale and impose $u=b\mu$ for held location. At every fixed $k>0$ the nuisance maximum exists. With free scale, divergence of $\sigma$ loses by the peak bound and collapse with $a=k\sigma$ loses by the fixed-$r$ argument. With free location, location escape loses as above. The normal interval endpoint Hessian is strictly negative definite. Its restriction is negative in any varying centre direction at fixed $b$, and $n\log b$ supplies strict curvature in any direction with varying $b$. Thus there is a unique nuisance optimizer at each $k$. A zero-dimensional nuisance block needs only evaluation. At $k=0$ the same conclusion follows from $n\log b-\tfrac12\sum_i(bx_i-u)^2$ on the held slice. [Endpoint Hessian proof](2026-11-08-bhatt-cert-report.md#positive-physical-width-held)

The density also has the form $b\,E[\phi(bx_i-u-kT)]$ for $T$ uniform on $[-1,1]$. This expression is analytic, positive, and even in $k$ near zero. At every finite $k$ the nuisance Hessian is nonsingular, so the analytic implicit function theorem makes the profile $P(k)$ analytic across zero and at every positive $k$. The derivative at positive $k$ can be computed at the enclosed nuisance root as
\[
P'(k)=\sum_i\frac{\phi(bx_i-u+k)+\phi(bx_i-u-k)}{D_i}-\frac{n}{k}.
\]

Use $K=e^{-P(0)/n}/(2\sigma)$ for held scale. Then $P(k)<P(0)$ for $k\ge K$ by the strict interval-mass bound. In this case $P(k)\to-\infty$. For free scale and $r>0$, use $K=2n$. The prior large-$k$ bound is strict at every finite parameter point there. For $a\ge r$ it uses $f<1/(2a)$, and for $a<r$ the retained extreme tail gives at most half the uniform likelihood. Also $P(k)\to U$ by taking $\mu=c$, $a=r(1+k^{-1/2})$ and $\sigma=a/k$. In particular $P$ is nonconstant in both cases. Analyticity and nonconstancy imply finitely many zeros of $P'$ on $[0,K]$, since a sequence of zeros would accumulate inside the analytic domain, including if its limit were an endpoint.

Take the normal fit plus the points at every distinct root in $(0,K)$ as the candidate list. With held scale its largest likelihood gives the whole set of maximizers. With free scale the list gives $M$ in the table, and the points at $M$ give the whole finite maximizing set whenever $M\ge U$. If any finite point ties or beats the relevant tail level, compactness and continuity give a maximizer on $[0,K]$, which is normal or stationary. Conversely, candidates outside the list cannot beat its best value and the reachable face together. This is a finite per-input criterion even in equality cases and with degenerate stationary points. Exact value equality and isolation of multiple roots can still require more than finite-precision bounds. [Large-width bound and width cover](2026-11-09-bhatt-allfree-report.md#global-width-argument-and-acceptance)

## Exact singleton slices

### Held scale and equal absolute residuals (proved)

At held $\sigma$ and location, let every absolute residual equal $d$. This also covers free location for a constant sample or a balanced two-point sample, because reflection and strict location concavity fix the centre for every shape and scale. Write the density at $d$ and positive width as $a^{-1}\int_0^a h(v)\,dv$, where
\[
 h(v)=\phi_\sigma(d)e^{-v^2/(2\sigma^2)}\cosh(dv/\sigma^2).
\]
If $d\le\sigma$, $h$ strictly decreases for $v>0$, including when $d=\sigma$. If $d>\sigma$, the derivative of $\log h(v)$ has the sign of $d\tanh(dv/\sigma^2)-v$. This function has positive slope at zero, is strictly concave thereafter, and is negative for $v>d$. Hence $h'$ changes sign just once. The width score has the sign of $a h(a)-\int_0^a h(v)\,dv$, whose derivative is $a h'(a)$. This expression starts at zero, increases, then strictly decreases to a negative value. Thus there is exactly one positive root and it is simple. For example, $(-1,1)$ with $\sigma=1$ selects $a=0$ at equality, and $(-2,2)$ with the same hold has a unique positive-width fit.

### Two free scales, equal distances or sparse symmetric spikes (proved)

If every distance from a compatible enforced centre equals $d>0$, free scales give the uniform limit. The enforced centre can be held, or fixed by reflection for a balanced two-point sample. It suffices to scale to $d=1$. For $a\in[1/2,1]$ the normal interval at 1 is on the positive side and has mass strictly below $1/2$, hence $f(1)<1/2$. For $a\ge1$ the strict peak bound gives the same conclusion. For $0\le a<1/2$ the density averages Gaussian values at distances greater than $1/2$. Maximizing a Gaussian density at a nonzero distance $y$ over scale gives $1/(\sqrt{2\pi e}|y|)$, which is less than $1/2$ here since $\sqrt{2\pi e}>4$. As $a\downarrow1$ from above with smoothing vanishing faster, the likelihood approaches the uniform value. This proves nonattainment for $(-1,1)$ with both scales free, and for a repeated value displaced from its held centre with both scales free.

A different explicit slice has observations at the centre and at two equidistant extremes, with equal counts at the extremes. Let $0<p\le2^{-20}$ be the total extreme fraction and $d_{\rm phys}>0$ their distance. Then the **unique global fit** is normal at that centre and $\sigma=d_{\rm phys}\sqrt p$, whether location is free or held at the symmetry centre. The following proof supplies the anchor for the all-free finite tie.

Scale to variance one, so the extreme distance is $d=1/\sqrt p$. To match the normal mean log likelihood, a competitor must have $a<5/2$ and $\sigma<2$ by the two peak bounds. Its mean log likelihood gain is also bounded above by $\log b+1/2-b^2(1-a\sqrt p)^2/2$, using the Gaussian density at distance $d-a$ for the extreme points. Because $(1-a\sqrt p)>9/10$, this excludes $b\ge3$ by the sign and monotonic decrease of $\log b+1/2-(81/200)b^2$ there. Thus $1/2<b<3$ and $k<15/2$ for a possible competitor.

Define $C(k)=E[e^{-k^2 T^2/2}]$ with $T$ uniform on $[-1,1]$, and $M_k(z)$ the moment generating function of the standard normal restricted to $[-k,k]$. The exact gain is
\[
\log b+(1-b^2)/2+\log C(k)+p\log M_k(b/\sqrt p).
\]
The first two terms together are nonpositive. For $k\le1$, Taylor's exponential upper bound gives $C(k)\le1-k^2/6+k^4/40$, so $\log C(k)\le-k^2/8$. Monotonicity extends this to $\log C(k)\le-\min(k^2,1)/8$. The bound $\log M_k(z)\le k|z|$ now excludes $k\ge1$ and implies $k\le24\sqrt p$ for any remaining competitor. Put $y=kb/\sqrt p\le72$. Restriction to the weighted density proportional to $e^{-k^2 T^2/2}$ decreases the expectation of $\cosh(yT)$ because the former weight decreases with $|T|$ and the latter function increases with $|T|$. This gives $M_k(b/\sqrt p)\le\sinh(y)/y$.

For $0\le y\le72$ use
\[
\log\frac{\sinh y}{y}\le y^2/6-y^4/150000.
\]
For $y\le6$, coefficientwise comparison of $e^{y^2/6}$ with $\sinh(y)/y$ gives a difference of at least $y^4/180$. In fact $(2j+1)!\ge6^j j!$ by induction and the degree-four difference is $1/180$. Taking logs gives a gap at least $y^4e^{-6}/180$. For $6\le y\le12$, $\log(\sinh y/y)\le y-\log(2y)$ gives a gap at least $\log12>2$. For $12\le y\le72$, the gap is at least $y^2/12$. Each bound dominates $y^4/150000$. After optimizing the surviving quadratic in $b$, the gain for $0<k<1$ satisfies
\[
\text{gain}\le \log C(k)-\tfrac12\log(1-k^2/3)-\frac{k^4 b^4}{150000p}
 < k^4\left(\tfrac18-\frac{1}{16\cdot150000p}\right)<0.
\]
Here $\log C(k)\le-k^2/6+k^4/40$, and $-\frac12\log(1-k^2/3)\le k^2/6+k^4/24$ by summing the logarithm series. This excludes every positive-width competitor.

## Normal contact with both scales free (proved)

Use total variance $v=\sigma^2+a^2/3$, normalized width $\eta=a/\sqrt v$, and normal standardized residuals $z_i=(x_i-m)/\sqrt V$. Write $S_j=\sum_i H_j(z_i)$ for the probabilists' Hermites and $m_j=n^{-1}\sum_i z_i^j$. In the total-variance coordinates, the matching-normal density ratio has Hermite coefficients given by the even Taylor coefficients of $\exp(-w^2/6)\sinh(w)/w$. This follows by Fourier inversion of the standardized characteristic function $e^{-t^2/2}e^{\eta^2t^2/6}\sin(\eta t)/(\eta t)$. A Gaussian envelope times a polynomial allows differentiation under the integral near $\eta=0$. Taking the logarithm yields the coefficients $A=-S_4/180$, $B=S_6/2835$, and
\[
C=-S_8/90720-\sum_i H_4(z_i)^2/64800
\]
at orders four, six, and eight for fixed regular coordinates at the normal optimum.

Let $I$ be its normal information in the free regular coordinates $(\mu,\log\sqrt v)$, dropping location if held. The nuisance root moves by $\eta^4 I^{-1}\nabla A+O(\eta^6)$. Taylor expansion of the objective at that root adds $\tfrac12\nabla A^{\mathsf T}I^{-1}\nabla A$ at order eight. Profiling at fixed $\eta$ is equivalent to profiling at fixed $k$, since $\eta=k/\sqrt{1+k^2/3}$. The first nonzero coefficient determines the strict comparison with the normal value throughout a punctured neighbourhood of $k=0$ after profiling. Positive coefficients refute the normal proposal. Negative coefficients exclude that neighbourhood as a source of competitors. The second case still needs a global comparison.

At $S_4=S_6=0$, $m_2=1$, $m_4=3$ and $m_6=15$. The log-scale derivative of $A$ vanishes since $zH_3=H_4+3H_2$. For free location the information has diagonal entries $n/V,2n$ and zero off-diagonal, and $\partial_\mu A=4S_3/(180\sqrt V)$. Thus the profiled eighth coefficient per observation is
\[
 (91-m_8)/37800+\mathbf{1}_{\mu\text{ free}}\,m_3^2/4050.
\]
The tests through this order use rational moments on dyadic data, since the standardized powers enter as even moments or squared odd moments. The following symmetric examples work at held location zero or free location. Superscripts give multiplicities and a signed level carries the specified count **at each sign**.

| Exact sample | First nonzero width coefficient |
|---|---|
| $(-1,0^{[4]},1)$ | $S_4=0$, $S_6/n=-6$. The normal collar is strictly lower at positive widths. |
| $(0^{[16]},(\pm1)^{[27]},\pm3)$ | $S_4=0$, $S_6/n=6$. Positive widths beat the normal. |
| $(0^{[70]},(\pm1)^{[45]},(\pm2)^{[9]},\pm3)$ | $S_4=S_6=0$, $m_8=99$. The normal collar is strictly lower. |
| $((\pm1/2)^{[28899]},(\pm3/4)^{[534736]},(\pm9/4)^{[44720]},(\pm5/2)^{[14085]})$ | $S_4=S_6=0$, $m_8=335079/4096$. Positive widths beat the normal. |

These are exact local classifications. The rational moment and Taylor-coefficient checks are retained with the proof constants. [Exact checks](2026-11-25-bhatt-math-data/exact_moments.py)

## Exact crossings

### All-free normal and positive-width global maxima (proved)

Let $p=2^{-20}$, $D=1024$, and take one observation at each of $\pm D$ and $2^{20}-1$ observations at each of $\pm t$, for $0\le t\le10$. Reflection fixes the centre at zero, so the same argument covers location held at zero. The normal variance is $V(t)=1+(1-p)t^2$. The uniform face has mean log likelihood $-\log2048$, strictly below the normal throughout since $\sqrt{2\pi e V(t)}<44$. The empirical fourth moment about zero is at least $1/p>3\cdot101^2$, hence $S_4>0$ everywhere along the path. The analytic expansion and nonsingular normal nuisance optimum give a common $\delta>0$ with $P_t(k)<N_t$ for $0<k\le\delta$. To see uniformity, the analytic nuisance branch depends locally on $(t,k)$, the quartic coefficient is bounded away from zero on the compact path, and finitely many local Taylor bounds cover it.

At $t=0$ the sparse-spike singleton proof gives strict global normal domination. At $t=10$ take $(\sigma,a)=(2,12)$. At $t$ the interval $[-1,11]$ has normal mass above $3/4$, so $f(t)>1/32$. At $D$ the interval $[506,518]$ has mass at least $\phi(507)$ by retaining its first unit length. Thus the witness mean log likelihood exceeds $-\log32-1/8$, using $p(507^2/2+1)<1/8$. This is above the normal value because $\sqrt{2\pi e V(10)}>40>32e^{1/8}$. The uniform face is below both. The exact dyadic endpoint $t=10$ therefore has a strict local normal maximum and at least one higher positive-width global maximum.

Define the exact observation $t_{\rm N}$ as the first zero in $[0,10]$ of
\[
F(t)=\max_{\delta\le k\le 2n}(P_t(k)-N_t).
\]
Any $\delta$ with the common collar property gives the same first zero, since values omitted from this compact interval are strictly negative. $F$ is continuous, $F(0)<0$, and $F(10)>0$. Consequently $t_{\rm N}$ is interior, and at this exact sample the normal member and at least one separated positive-width root have the global value. The finite set is non-singleton. The proof establishes at least two members of that set.

### Held sigma with exactly two widths (proved)

Hold $\sigma=1$ and take $x=(-d,0^{[19998]},d)$, where $d\in[332,333]$. Location can be free or held at zero. Let $p=1/10000$, let $G_d(a)$ be mean log likelihood gain over $a=0$, and use $C(a)$ as above. For $a\le1$, the central contribution to the gain is at most $-(1-p)a^2/8$ and the tail contribution is at most $pda$. Hence all $a\in[1/2,1]$ have negative gain, the entire small branch has gain at most $2(pd)^2/(1-p)<0.0023$, and $a\le3/100$ gives gain at most $0.000999$. At $a=1/10$, Jensen gives $\log C(a)\ge-a^2/6$ and the tail density ratio is at least $e^{-a^2/2}\sinh(da)/(da)$. Since $da\ge33.2$, $2da\le66.6$, $\log66.6<5$ and $\log(1-e^{-66.4})>-0.01$, this witness gains more than $0.001$. Thus the small maximizer is in $(3/100,1/2)$.

On that interval $(\log C)''=-E_a T^2+a^2\operatorname{Var}_a(T^2)\le-7/24+1/16$. Here $E_a$ uses the density weighted by $e^{-a^2 T^2/2}$, and $E_a T^2\ge e^{-1/8}/3\ge7/24$. The positive-width normal interval log mass is concave, so the tail log density contributes curvature at most $1/a^2$ before multiplying by $p$. The combined curvature is strictly negative by the rational bound at $a=3/100$. The small branch has one simple maximum.

For $a\in[1,2]$ use $\log C(a)\le-1/8$ and $pda\le0.0666$ to exclude positive gain. For $2\le a\le150$, $D_0\le1$ and the outlier interval has mass at most $\exp(-(d-a)^2/2)$, giving
\[
G_d(a)\le\chi_d(a)=\log(\sqrt{2\pi}/(2a))+p(da-a^2/2).
\]
For $d\le333$ the bound with $d=333$ suffices. It is convex from 2 to 100 and increasing from 100 to 150, since on the latter interval the derivative decreases but is still positive at 150. Both $\chi_{333}(2)$ and $\chi_{333}(150)$ are negative, using $\sqrt{2\pi}<8/3$, $\log(2/3)<-1/3$ and $\log(2/225)<-4$. Thus $[1,150]$ cannot win.

On $[150,d]$ write $l=d-a$, $h=d+a$, $D_a=\Phi(h)-\Phi(l)$, $A=\phi(l)/D_a$, $B=\phi(h)/D_a$. The second derivative of $\log D_a$ is $lA-hB-(A+B)^2\le lA-A^2\le-R'(l)$, where $R=\phi/Q$ is the normal hazard. For $l\ge0$, $R'(l)\ge1/2$. Indeed $M=Q/\phi$ satisfies $M'-lM+1=0$, while $m(l)=\sqrt{l^2+2}-l$ satisfies $lm+m^2/2=1$ and $m'-lm+1\le0$. Integrating the inequality for $(m-M)e^{-l^2/2}$ to infinity gives $M\le m$ and hence $R(R-l)\ge1/2$. Adding $1/a^2$ for the common normalizing factor and dropping the nonpositive central interval-mass curvature gives $G_d''\le 1/150^2-p/2<0$. At 150 the score is positive since $A\ge R(d-150)>d-150$ and $p(d-150)>1/150$. At $a\ge d$ the score is negative. The central and outlier masses are then each above $1/4$. For the outlier, $a\phi(a-d)$ and $a\phi(a+d)$ are each at most $d+1$, using $(a-d)\phi(a-d)\le1$. Its contribution to $a$ times the mass score is at most $8p(d+1)<0.27$. The central contribution is at most $8a\phi(a)<1/4$ on this region. Their sum is below the normalization score of one. This proves one simple large maximum in $(150,d)$.

At $d=332$, the bound $\chi_{332}$ is maximized on $[150,332]$ at $(332+\sqrt{332^2-40000})/2\in(298,299)$. Its value is less than $-5.46+p(332^2-33^2)/2<0$. At $d=333$, use $a=300$, retain $[33,33+1/33]$ in the outlier interval and bound $\log D_0>-10^{-6}$. Since $\log(600/\sqrt{2\pi})<5.484$ and $\log33+\frac12\log(2\pi)+(33+1/33)^2/2<551$, the gain exceeds $0.005$. This beats the entire small branch. The exponential comparisons used here follow from the rational Taylor enclosures in the checker and $25/8<\pi<22/7$. At exact dyadic $d=333$ there are two separated local maxima and the large one is the unique global maximum. [Exact checks](2026-11-25-bhatt-math-data/exact_moments.py)

Define $d_*$ as the zero in $[332,333]$ of the large maximum minus the small maximum. These two continuous heights can be defined by maximizing on the fixed intervals $[150,333]$ and $[0,1]$. Their difference increases strictly. For each fixed pair of widths with the large one above the small one, the derivative in $d$ of their log-density difference at the outlier is the difference of tilted uniform means. At width $a$ the tilted mean equals the average of $v\tanh(dv)$ under the positive density proportional to $e^{-v^2/2}\cosh(dv)$ restricted to $[0,a]$. It strictly increases with $a$ for $d>0$. To compare two outlier positions, use the large maximizing width at the first position and the small maximizing width at the second. The strict pairwise inequality gives the strict inequality of profiled height differences. Thus $d_*$ exists and is unique, and its maximizing set consists of exactly $(0,1,a_{\rm small})$ and $(0,1,a_{\rm large})$.

### Positive width and the excluded uniform face (proved)

Take $x=(-1,(-t)^{[32]},t^{[32]},1)$ for $2/5\le t\le1$, with both scales free and location free or held at zero. The uniform value is $U=-n\log2$. Set $A=4^{-1/n}$. Every $a\ge A$ has likelihood strictly below $U$. For $A\le a<1$, the two extreme interval masses are each strictly below $1/2$ at the symmetry centre, which gives the bound $(2a)^{-n}/4\le2^{-n}$. At $a\ge1$ use the strict peak bound. For $a\le A$ the product is bounded above by $(\sqrt{2\pi}\sigma)^{-n}\exp(-(1-A)^2/\sigma^2)$ using the two extremes. This excludes a common interval $0<\sigma<\sigma_0$, for some $\sigma_0>0$. Also $\sigma\ge1$ loses by the normal peak bound. Thus any finite fit that can match $U$ is tested on the common compact rectangle $0\le a\le A$, $\sigma_0\le\sigma\le1$.

The maximum $J(t)$ on this rectangle is continuous and strictly decreasing, since $f(t)$ is strictly decreasing in $t>0$ for every member. At $t=2/5$ the normal member exceeds $U$ by the variance comparison $2\pi e V<4$. At $t=1$ the equal-distance proof gives a strict loss everywhere on the rectangle. Define the exact observation $t_{\rm U}$ by the unique root $J(t_{\rm U})=U$. The normal member cannot attain this equality. The standardized fourth moment is $33(1+32t^4)/(1+32t^2)^2$, decreases on $[2/5,1]$, and starts below 3. Every normal optimum on the path is therefore locally refuted. At $t_{\rm U}$ at least one positive-width fit attains the global value, and the excluded uniform face ties it. For $t<t_{\rm U}$ on this path there is an attained positive-width winner above $U$. For $t>t_{\rm U}$ the appropriate outcome is the uniform limit.

## Numerical evidence and public coefficient accuracy

The retained Python replays solve local scores and height equalities at 70 and 105 decimal digits. The two runs agree at the displayed precision. Score sweeps at 55 digits use 620 held-scale widths up to 1000, 383 widths up to $k=132$ for the uniform comparison, and 284 widths up to $k=8$ for the sparse sample. They find three, two, and two sign changes, respectively. The grids do not establish exhaustive root isolation. In the two free-scale paths the numerical equalities refer to the branches located by the script. Identifying those branches with all finite maximizers at the exact thresholds requires a full cover. The held-scale proof supplies the complete count independently. [Branch replay](2026-11-25-bhatt-math-data/crossings.py) · [Final output](2026-11-25-bhatt-math-data/crossings.txt)

| Numerical branch equality | Varied level | Positive-width coefficients $(\sigma,a)$ |
|---|---:|---|
| Held scale, large vs small | $332.655021649750412$ | $(1,299.270314828764069)$ and $(1,0.096799738191459638)$ |
| Free scale, finite vs uniform | $0.790219989806205082$ | $(0.060617627850898965,\ 0.939888219039200308)$ |
| Free scale, positive width vs normal | $3.027472110305764239$ | $(1.878100361844013848,\ 4.207505072408212377)$ |

| Signed mean log likelihood gap | Lower adjacent binary64 input and gap | Upper adjacent binary64 input and gap |
|---|---|---|
| Large width minus small width | `0x1.4ca7af7fb3d99p+8`, $-9.226\times10^{-16}$ | `0x1.4ca7af7fb3d9ap+8`, $+7.779\times10^{-16}$ |
| Finite branch minus uniform | `0x1.9497b6e9b9e8cp-1`, $+2.391\times10^{-17}$ | `0x1.9497b6e9b9e8dp-1`, $-9.937\times10^{-18}$ |
| Positive-width branch minus normal | `0x1.838434c3a85dcp+1`, $-1.337\times10^{-17}$ | `0x1.838434c3a85ddp+1`, $+1.362\times10^{-17}$ |

The floats in the second table are exact dyadic observations used in the replay. At these scales a likelihood tolerance would conflate distinct branch comparisons. The numerical signs are evidence about these inputs and branches, not exact equality predicates or estimates of a production error margin. [Tie classification and strict comparison rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

The accuracy replay uses 20 stored public records in the native and double-only modes, with input fingerprints checked for the 14 snapshot rows. It uses exact binary64 inputs, jointly refines the free parameters using the original physical density, and checks the score and positive information at regular roots. It uses the normal closed form and a one-sided check on the normal members. The reference uses 85 digits on the ordinary snapshot rows, 380 for held-width case `000410` with $a/\sigma$ of order $10^{-150}$, and 105 for the near-normal and far-tail rechecks. All normalized free coefficient errors were below $3.6\times10^{-15}$, and the 16 available log likelihoods agreed at the public coefficients to relative error below $2.0\times10^{-16}$. These are historical public records from the November 15 baseline and the November 21–22 rechecks, with no new package execution. [Accuracy checker](2026-11-25-bhatt-math-data/public_accuracy.py) · [Final output](2026-11-25-bhatt-math-data/public_accuracy.txt) · [Snapshot provenance](2026-11-15-fit-snapshot-report.md) · [Far-tail rechecks](2026-11-21-mode-kind-report.md) · [Near-normal recheck](2026-11-22-render-fu-report.md)

The offset control illustrates why physical evaluation and coefficient estimation are separate. For twenty observations at $s=10^{16}$ and one at each of $s-10,s+12$, the Gaussian joint optimum has $\mu_*=s+1/11$ and $\sigma_*^2=1341/121$. Rounding $\mu_*$ to $s$ and then refitting scale changes the variance to $1342/121$, for relative scale error $3.7278659\times10^{-4}$. This is a counterfactual refit on exact binary64 data. Evaluating the reported likelihood at rounded public coefficients does not require altering the joint scale estimate. The earlier physical-likelihood guard study used this same sample. [Prior offset check](2026-11-09-bhatt-allfree-report.md#implementation-and-regression-behavior) · [Accuracy contract](../../design/accuracy-contract.md)

All three final Python checks exited zero on the workstation under the one-slot lock. From the worktree root run the commands in sequence.

```sh
d=dev/research/evaluations/2026-11-25-bhatt-math-data
export PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/exact_moments.py"
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/public_accuracy.py"
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/crossings.py"
```

[Exact-check output](2026-11-25-bhatt-math-data/exact_moments.txt)

## What this changes for the fitter

- Apply the exact collapse and held-width predicates before normalizing. When $\sigma$ is held and $a$ is free, $V\le\sigma^2$ certifies the attained normal singleton, including exact equality. The equal-distance slice gives a unique bracketable positive-width solution above that threshold. With both scales free, the sparse-spike predicate gives an exact normal singleton, and the compatible equal-distance predicate gives a strict uniform win. All finite certificates still require public coefficient inclusion.
- With free width, keep the normal as an attained candidate and carry the strict tail exclusion to the chosen finite $K$. Exact signs of the fourth, sixth, and profiled eighth coefficients give starting tests for a normal collar. A production collar needs a bound on the profiled remainder to cover an explicit positive interval. In the remaining cells a signed profile score can exclude stationary points, and a signed bracket with strictly negative profile curvature can certify one local maximum. Evaluate all surviving height bounds on a common scale, allowing for root movement and original-coordinate arithmetic.
- Return a certified singleton after proving global dominance and root identity. At a proved finite/face equality return the finite fit, with the face retained as a diagnostic. With proved multiple attained maximizers and a complete set, return `certified=TRUE, unique=FALSE` and the typed set, including $a=0$ where applicable. The exact held-scale construction gives a complete pair. Where a finite candidate is proved globally maximizing and its coefficients are enclosed but cardinality is unresolved, use `unique=NA`. With overlapping heights and insufficient global evidence, retain a successful accuracy-checked search as `certified=FALSE, unique=NA`. Eliminate by value only at strict separation unless an independent exact or strict pointwise test applies. Preserve existing failure handling when the search itself fails or a separated witness refutes the candidate. [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)
- A strict uniform win gives `UniformLimit`. Preserve the compatible Bhattacharjee path supremum for comparisons and label the closed uniform model's point likelihood separately. The public `bhatt_mle` currently recomputes `limit_loglik` and can replace the uniform candidate's bounds with its closed-model point value. For held-width endpoint contact that value exceeds the parent supremum by the exact penalty above. Validate the represented interval and any extrapolated `mlunif` advice against both held width and held location. [Public limit payload](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/certificate.hpp#L650) · [Binding messages](../../../bindings/common/distributions/bhatt/fit.hpp)
- Transport joint coefficient boxes to physical units before rounding. Reevaluate the density at the published point and retain the joint coefficient estimate. The offset case gives an exact regression against a conditional refit after publication rounding. [Accuracy contract](../../design/accuracy-contract.md)

## Open

- A complete count of finite maximizers at $t_{\rm N}$ and $t_{\rm U}$ needs certified root isolation and competitor exclusion at the thresholds. The all-free example proves an attained non-singleton, and the uniform comparison proves attainment at equality. The numerical grids leave undetected or degenerate roots possible.
- None of the real-crossing constructions proves an exact tie on binary64 data. Resolving that arithmetic case requires an exact represented witness or an equality obstruction for the Gaussian interval masses at the stationary parameters. Sign changes on neighboring floats supply near-tie fixtures.
- A production normal collar needs explicit validated remainder bounds after profiling, including higher orders when the fourth, sixth and eighth coefficients vanish. Practical isolation of multiple stationary roots and exact equal-height comparisons remain necessary for a general finite-width decision. A successful search retains its uncertified outcome when these checks do not close.
