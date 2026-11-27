# Maximum likelihood under fixed truncation for threshold and shape families

TASK-462. Throughout, the window is closed, its intersection with the parent support has positive mass, and the sample is nonempty and finite. Write $m,M$ for the sample extrema, and $L,U$ for the effective cuts, which can depend on the parameter. An endpoint observation uses the density value declared by the parent. A divergent sequence of finite likelihoods is distinguished from a finite parameter with infinite endpoint density in a reference model. Attainment here means a positive finite MLE unless the infinite-density convention is explicitly discussed. Estimated cuts are outside the scope. [Modifier design](../../design/modifiers.md) · [Reviewed location-scale cases](2026-11-27-mod-rev4-report.md) · [Reviewed positive shape cases](2026-11-27-mod-rev6-report.md)

## Normalization and comparison lemmas

**Proved.** For a parent CDF $F$, the likelihood is $\prod_i f(x_i)/(F(U)-F(L))^n$ in continuous cases. For integer data retain cells $[j,j+1)$ for $j=\max(0,\lceil a\rceil),\ldots,\lfloor b\rfloor$ and normalize their integrals over the union. This yields a single latent interval, even when the first or last retained observation falls on a window cut. Density or mass outside the effective support gives zero likelihood. Strictly positive competitors are assumed in the comparison tests. A held lower endpoint with zero density excludes an observation there. A held singular endpoint yields an infinite factor if the remaining densities are positive.

For an unnormalized kernel $k$ with finite positive integral $Z_k$, put $J(k)=\prod_i k(x_i)/Z_k^n$. If after multiplication by a parameter-dependent constant the kernels converge at the observations and have an integrable dominating function, the conditional likelihood converges to $J(k)$ and the conditional densities converge in total variation. The latter follows by dominated convergence applied to the absolute difference. Restrict domination to the retained interval. If the limiting integral is infinite and the numerator stays bounded on that scale, Fatou gives likelihood zero. When a branch has a finite excluded endpoint, an approachable threshold $B$ can include its limiting likelihood even if the associated weak limit is an atom. If all escaping sequences have limsup at most $B$ and $B$ is the supremum of their achievable heights, a positive finite MLE exists iff some feasible finite point reaches $B$. Indeed, any higher level strictly above $B$ confines a maximizing sequence to a compact subset of the feasible region, where upper semicontinuity gives a winner. This argument permits equality at an attainable model. For scalar analytic branches one can check stationary values and included branch endpoints, with a constant branch represented by any point. This rule supplies a sample criterion once the threshold and candidate equations are explicit. It gives no general bound on the number of winning roots.

**Proved.** The exponential, uniform and power heights are respectively
$$
 J(e^{t x})=\frac{e^{t\sum x_i}}{\left(\int_L^U e^{t x}\,dx\right)^n},\qquad
 (U-L)^{-n},\qquad
 J((x-d)^{q-1})=\frac{\prod (x_i-d)^{q-1}}{\left(\int_L^U(x-d)^{q-1}\,dx\right)^n}.
$$
The integrals use exponential differences, power differences divided by $q$, or a log ratio at $q=0$. A right exponential requires $t<0$, a left exponential $t>0$. A right power requires $q<0$, a lower contact $L=d$ requires $q>0$, and both cannot hold on the same interval. For reverse powers replace $x-d$ by $d-x$. These formulas use the stated sample values of the kernel, including any separately established endpoint trace. In particular a weak atom supplies no continuous sample likelihood by itself. For grouped data use cell integrals in the numerator. Scalar exponential tilts use the strictly increasing retained statistic mean, since its derivative is the positive variance. The endpoints of the mean range give the exact feasibility test and the corresponding one-sided boundary likelihood. [Exponential-family theorem](2026-11-27-trunc-expfam-math-report.md#general-theorem)

**Proved.** On $0<L<U<\infty$ a broad-scale limit of a scale kernel with small-argument behavior $k(v)\sim C v^{q-1}$ gives $x^{q-1}$, by uniform convergence on the rescaled window. The same argument works at $L=0$ for $q>0$ using a bound $2C v^{q-1}$. On a right ray, for a proper scale parent with $q>0$ the retained mass tends to one and fixed positive observations have density $O(s^{-q})$. Conversely a small-scale tail $k(v)\sim C v^{q-1}$ with $q<0$ gives the conditional power on $L>0$, including $U=\infty$, by uniform relative convergence for $v\ge L/s$. At a fixed lower support endpoint with raw singularity $p-1<0$, moving the endpoint toward $m$ gives a factor of order $(m-d)^{p-1}$ per minimum tie if the other density factors and retention probability converge to positive values. For a constant sample at a finite upper cut one must also test vanishing retained width. If the interval retained above a moving endpoint is $[d,U]$ and its raw kernel is asymptotic to $C(x-d)^{p-1}$, $p>0$, its conditional density at $U$ is asymptotic to $p/(U-d)$. Thus even a density vanishing at the parent endpoint can diverge at an upper cut. Reflection exchanges the cuts. [Shared positive-family limits](2026-11-27-trunc-shape-math-report.md#positive-scale-powers-with-a-lower-cut)

**Proved.** A useful remote-coordinate rule is the following. For smooth positive kernels $k_j$, if $\partial_x\log k_j\to t$ locally uniformly in $x$, pivot at a fixed point inside the window. The kernel ratios converge to $e^{t(x-x_0)}$ by integration. This gives an exponential on compact windows. On a ray it gives the integrable exponential only with a global integrable tail bound. For log-concave kernels the tangent at a fixed pivot provides this bound on the relevant side when $t$ has the requisite sign. When a derivative instead tends uniformly to infinity with one sign and is of order $r_j\to\infty$ near the projected cut, the density at that cut is of order $r_j$ if integrating on a cut neighborhood gives order $1/r_j$ and the rest is negligible. At a fixed positive distance exponential loss then beats any polynomial growth in $r_j$. The folded normal and fatigue calculations below give the needed tails in those uses. [Normal limits](2026-11-21-tnorm-limit-report.md) · [Short windows](2026-11-22-short-window-report.md)

In the sections below, a fully held mask evaluates the parent conditional likelihood at its only feasible coefficient vector, with unique coefficients for compatible positive finite density. For open masks, the listed paths are proved inclusions in the closure and are used only for one-sided conclusions unless an exhaustion argument is supplied.
## Pareto III and IV

Write $d=\mathrm{min}$, $s=\mathrm{scale}$, $p=\mathrm{shape}$ for III or $p=\mathrm{shape2}$ for IV, and $h=1$ for III or $h=\mathrm{shape1}$ for IV. For $y=x-d>0$ survival is $(1+(y/s)^p)^{-h}$ and the kernel is $y^{p-1}(1+(y/s)^p)^{-h-1}$.

**P1. Free threshold and spike.** If $p$ is free choose $0<p<1$, or use such a held $p$. With other coordinates fixed, $d\uparrow m$ gives divergence if $m<U$ by the contact lemma. At $m=U$ the sample is constant and the shrinking-width argument applies for every $p>0$. When $m<U$ and $p<1$, the weak limit is the finite-scale parent conditional on the window at $d=m$, and the likelihood diverges, even though the limit has no atom. At the contact itself the reference density is infinite. This covers all locations of the fixed cuts compatible with the sample.

**P2. Held $d,s,p$, free $h$.** Transform $T=\log(1+(y/s)^p)$. The conditional law in $T$ is a decreasing exponential with rate $h$. A unique fit occurs iff $T_L<\overline T<E_{h=0}T$, where the upper threshold is $(T_L+T_U)/2$ for finite $U$ and infinity otherwise. At the lower equality, collapse at $L$ gives divergence. At or above the finite upper threshold the excluded kernel $T'(x)$ wins. These statements require positive finite $T'(x_i)$, which includes $y_i=0$ when $p=1$. Zero and infinite carriers at a held contact follow the stated endpoint convention.

**P3. Held $d,p,h$, free $s$.** Let $l=L-d$. At infinity the only positive limit is the bounded-window power $y^{p-1}$. At zero, for $l>0$, the limit is the integrable power $y^{-ph-1}$. For $l=0$ and all $y_i>0$, it is zero on the likelihood scale, since retention tends to one and each raw density is $O(s^{ph})$. With ties at $l=0$, $p<1$ gives the infinite endpoint and $p>1$ gives zero for every finite model. At $p=1$, with $k$ ties, the small-$s$ height is asymptotic to
$$s^{h(n-k)-k}h^n\prod_{y_i>0} y_i^{-h-1}.$$
This follows from the raw factor $h/s$ at zero and retention tending to one. The weak limit for $l=0$ is the atom at $d$, also at a positive critical height. If the exponent is negative, the likelihood diverges. In all other positive-compatible cases, compute the two endpoint heights as above and compare them with the finite scale-score roots of $\sum_i\partial_s\log k_s(x_i)-n\partial_s\log Z_{k_s}=0$. This is iff by scalar compactness. If neither endpoint height is positive there is always an attained positive fit. Cardinality for general roots is open.

**P4. Held $s,p,h$, free $d$.** For $p<1$ use P1. For $p\ge1$, $d$ ranges over $(-\infty,m]$ with positive retention, and $d=m$ uses the inside limit when $m<U$. The join $d=a$ (for finite lower cut) separates a fixed interval from the moving lower support. It is included if feasible. The only excluded finite limit is constant upper-cut shrinking, which diverges. At $d\to-\infty$ regular variation gives uniform on a bounded window. On a right ray, the density at any fixed observation is asymptotic to $ph/(a-d)$ by the survival ratio. On a left ray the retained mass tends to one and all fixed densities vanish. These are all endpoints of the scalar domain. Compare finite stationary values and included joins and endpoints with the remote height using the same comparison as P3. This is iff. General cardinality is open. For $p=1$, $h$ fixed but $s,d$ both free, the kernel depends only on the pole $d-s$ and its support. Increasing $d$ up to $m$ at fixed pole preserves the numerator and decreases the denominator, strictly when cutting positive retained mass. Thus one may fit with $d=m$, using P3, unless $m$ is the upper cut and gives divergence. If $a=m$, every $d\le m$ above the fitted pole represents the same law and gives additional maximizers. If $a>m$ the data are incompatible.

When $p$ is free with held $d$, $s\to\infty$ gives the bounded power $y^{p-1}$. With $l>0$, $s\to0$ gives the tail power $y^{-ph-1}$. There is a second joint limit when $s$ grows and $h/s^p\to\lambda>0$, namely Weibull survival $\exp(-\lambda y^p)$, by $h\log(1+(y/s)^p)\to\lambda y^p$ and differentiation. Mass differences converge to positive values on every compatible nondegenerate window. Free $p$ can access a critical tie together with these scale changes. P1–P4 do not exhaust those joint regimes.

## Exponentiated Weibull and PGW

Write $v=(x/s)^p$, with $s,p>0$. The EW CDF is $(1-e^{-v})^c$. PGW survival is $\exp(-((1+v)^c-1))$, where $c>0$. The following path likelihoods are obtained by differentiation at fixed positive observations and subtraction of CDFs at the cuts. All regular claims below are for positive observations. At zero, the raw inside densities behave respectively as a positive constant times $x^{pc-1}$ and $x^{p-1}$.

**E1. EW, scale and shape held.** With $T=\log(1-e^{-v})$, the conditional law has density proportional to $e^{cT}\,dT$. It has a unique fit iff $E_{c=0}T<\overline T<T_U$. The lower threshold is $(T_L+T_U)/2$ if $L>0$, and $-\infty$ otherwise. At or below the finite lower threshold the excluded kernel $T'(x)$ wins, including on a right ray with $T_U=0$. At the upper equality with finite $U$ the density diverges at $U$ and gives collapse. The integral and likelihood are the exponential ones in $T$, multiplied by $\prod_i T'(x_i)$.

**E2. Additional EW paths.** For fixed $p,c$, broad scale on a finite upper window gives $x^{pc-1}$. At small scale on a right ray with $L>0$, or on a bounded window with $L>0$, the density is asymptotically that of a conditional Weibull with rate $s^{-p}\to\infty$. Indeed $f/(c p s^{-p}x^{p-1}e^{-v})\to1$ uniformly for $x\ge L$. Thus it loses except for a sample constant at the lower cut, where its density diverges. When $L=0$, small scale instead retains asymptotically all the mass and loses at every fixed positive observation. These two limits and a scalar scale-score comparison give iff when exponent and shape are held. When $c=1$ the scale-only result also follows directly from the gamma tilt in $x^p$. At fixed $p$ with $s\downarrow0$, $s^p\log c\to w>0$ makes the weak limit a point at $w^{1/p}$. A path centered exactly at a positive constant $x_0$ takes $\log c=(x_0/s)^p$ and its density grows like $p x_0^{p-1}s^{-p}/e$. It diverges after conditioning even if $x_0$ is a cut. At fixed $s$ with $p\to\infty$ and $pc\to q>0$, the CDF tends to $(x/s)^q$ below $s$ and to one above. The density tends to its power value below $s$, to zero above, and to $q/((e-1)s)$ at $s$ (substitute $v=1$). Thus at a sample tie at $s$ the face likelihood uses the factor $1/(e-1)$ per tie relative to the inside power. If $L\ge s$, this CDF limit alone provides no conditional likelihood. For a finite window away from zero, $p\downarrow0$ at fixed $s,c$ gives $x^{-1}$, since the kernel divided by $p$ converges uniformly to a positive multiple of $1/x$. If $L=0$ or $U=\infty$, Fatou in the same normalization shows loss at every fixed positive observation. Where $p$ is free and the sample is constant at $s$, holding $c$ fixed and increasing $p$ gives density proportional to $p$ at $s$. A free scale can match that constant.

**W1. Additional PGW paths.** At fixed $p,c$, broad scale gives $x^{p-1}$ on a finite upper window. At small scale, $\partial_x H=cp s^{-pc}x^{pc-1}(1+(s/x)^p)^{c-1}$. On $L>0$, rescale $x=L+z/H'(L)$ in $H(x)-H(L)$ to get an exponential of rate one in $z$. Equivalently, the exact conditional density denominator after division by $S(L)$ is $1-\exp(-(H(U)-H(L)))\to1$. The density at $L$ grows as $H'(L)$ and every fixed point above loses exponentially. At $L=0$ retention tends to one and the likelihood loses on positive data. Thus at positive data the scale-only branch has the exact two-end comparison of E2 with broad power $p$. When $c=1$ it is the conditional Weibull. For fixed $s,c$, $p\downarrow0$ gives $x^{-1}$ or zero likelihood according to the same integrability rule as E2.

For fixed $p,s$, $c\downarrow0$ gives density proportional to $\partial_x\log(1+v)$ on a finite upper window since $H'(x)/c\to\partial_x\log(1+v)$ and $H\to0$. Domination on that window follows from $(1+v)^c\le(1+(U/s)^p)$ for $c\le1$. The kernel is nonintegrable on a right ray, hence fixed positive data lose. At $c\to\infty$ with $L>0$, the same survival-difference argument as for small scale gives divergence for a constant at $L$ and loss otherwise. At $L=0$ retention tends to one and there is loss for positive data. These are both ends for held scale and shape, so the exponent-score comparison is iff. For $c\to\infty$ and $s=s_0c^{1/p}$ at fixed $p$, the limit survival is $\exp(1-\exp((x/s_0)^p))$ and its kernel is the derivative of its negative survival. For fixed $s$, $p\to\infty$ with $pc=q>0$ gives survival one up to $s$ and $\exp(1-(x/s)^q)$ above. The density at $s$ has half the right-limit value $q/s$, since $v/(1+v)=1/2$ there. If $U\le s$ the limiting model has zero retained mass. This case cannot use the threshold Weibull likelihood formula and needs separate normalization. Finally fixed $c,s$ and $p\to\infty$ has density proportional to $p$ at $s$, proving constant-at-scale divergence when such a shape path is feasible, including when $s$ is either cut.

**E3/W2. Held scale and exponent.** With $s,c$ held the shape-only branch has the $p\downarrow0$ log-uniform trace on a bounded window away from zero and zero height otherwise, as proved above. The $p\to\infty$ limit concentrates at the projection of $s$ onto the window and its sample height diverges at a constant there and otherwise tends to zero. If $U<s$, the CDFs are uniformly asymptotic to $(x/s)^{pc}$ for EW and $c(x/s)^p$ for PGW on $0<x\le U$. After differentiation the conditional density grows linearly in $p$ at $U$ and loses geometrically at a fixed smaller positive observation. If $L>s$, severe-tail survival differences are asymptotic to the survival at $L$. The EW log survival derivative is asymptotic to $-p(L/s)^p/L$ at $L$, and the PGW derivative to $-cp(L/s)^{pc}/L$. At a fixed point above $L$, the ratio of survivals loses faster than any power of the raw derivative growth there. If $L\le s\le U$, the mass tends to a positive constant since the CDF at $s$ is fixed in $(0,1)$, and the density at $s$ is proportional to $p$. Density at every other fixed positive observation goes to zero geometrically or faster, which beats any finite number of factors proportional to $p$. These are all shape ends. Thus the scalar root comparison with the small-shape face is iff outside the divergent case.

An all-free EW inverse-Weibull path takes $c\to\infty$, $p\log c\to k>0$ and $s(\log c)^{1/p}\to s_0>0$. Then $v-\log c\to k\log(x/s_0)$ and $c e^{-v}\to(s_0/x)^k$, yielding CDF $\exp(-(s_0/x)^k)$. The derivative tends to that of this inverse Weibull because $pv\to k$. Subtracting CDFs gives its exact conditional likelihood on every window of positive limiting mass. This path does not imply every EW inverse-Weibull comparison is decisive.

## Fatigue

For $y=x-\mu>0$ let $z(y)=\sqrt{y/\beta}-\sqrt{\beta/y}$. The CDF is $\Phi(z/\alpha)$. The kernel at fixed $\mu,\beta$ is $z'(y)\exp(-t z(y)^2)$, $t=1/(2\alpha^2)>0$. It has a unique fitted $t$ iff $\inf_{[L,U]}z^2<\overline{z^2}<E_0 z^2$. The upper threshold is the uniform-in-$z$ second moment when both transformed cuts are finite, and infinity otherwise. At or beyond the finite upper threshold the excluded uniform-in-$z$ wins, with height $\prod z'(y_i)/(z_U-z_L)^n$. At the lower equality the normal tilt concentrates at the unique projected zero of $z$ and diverges. The infimum really is attained at a finite positive excess for a positive-width compatible window, since $z$ ranges from $-\infty$ to $+\infty$ over the parent domain. The statistic is nonconstant. This proves the alpha-only test.

At fixed $\mu,\alpha$, the remaining scale kernel is
$$ (y+\beta)y^{-3/2}\exp[-y/(2\alpha^2\beta)-\beta/(2\alpha^2y)]. $$
As $\beta\downarrow0$ it concentrates at $l=L-\mu$ if $l>0$, and otherwise the unconditional weak limit is at zero. For $l>0$, normalizing after pivoting at $l$ gives width of order $\beta$ and exponential loss at each fixed point above $l$. For $l=0$, retention tends to one and positive observations lose. As $\beta\to\infty$, a finite upper excess $u=U-\mu$ gives concentration there and width of order $1/\beta$. To see the tail bound toward zero, split at $u/2$. On $[u/2,u]$ the derivative of the exponent is comparable to $\beta$, and on $(0,u/2)$ integration of $y^{-3/2}e^{-\beta/(2\alpha^2 y)}$ by $w=1/y$ gives an exponentially smaller contribution. For $U=\infty$, retention tends to one, and fixed densities vanish exponentially. For the small-$\beta$ right tail above $l$, the polynomial factor $y^{-1/2}+\beta y^{-3/2}$ is decreasing and the exponent has derivative bounded above by $-1/(4\alpha^2\beta)$ eventually, uniformly for $y\ge l>0$. These estimates also bound each other factor in a nonconstant product by a polynomial at the favored cut or exponential loss elsewhere. Therefore the scale-only fit attains exactly unless the positive sample is constant at an available cut admitting concentration, in which case the likelihood diverges. General scale-root cardinality is open.

When $\mu,\beta$ are free, or when $\alpha,\beta$ are free at a compatible held $\mu$, constants diverge. With $\alpha$ free use $\beta=x_0-\mu$ and shrink $\alpha$. With $\alpha$ held, set $\mu=x_0-\beta$ and send $\beta\downarrow0$, obtaining raw density proportional to $1/\beta$. Retention is at most one. With $\alpha,\beta$ held and free $\mu$, a constant at finite $U$ gives divergence as $\mu\uparrow U$. Indeed $\Phi(z(u)/\alpha)\sim\phi(z(u)/\alpha)\alpha/|z(u)|$ for $u=U-\mu\downarrow0$. The conditional density at $U$ is then asymptotic to $\beta/(2\alpha^2u^2)$. The normal tail equivalence follows by l'Hôpital's rule.

With $\alpha,\beta$ held and $\mu\to-\infty$, the remote kernel has slope $-1/(2\alpha^2\beta)$. It gives a decreasing exponential on bounded and right windows. For the right-ray bound, above a fixed pivot the exponential slope tends uniformly to that rate, with the polynomial log derivative $O(1/y)$, so a tangent comparison with half the rate suffices. On a left ray retention tends to one and all fixed densities lose. At the other end $\mu\uparrow m<U$, raw density at $m$ vanishes and retention has a positive limit. Hence outside the upper-cut constant the threshold-only criterion is the scalar stationary comparison with the stated remote height, and cardinality is open. Joint escapes with one or both of $\alpha,\beta$ free and moving $\mu$ require further bounds.

## Nakagami

Put $y=x^2$. The transformed law is gamma with shape $k>0$, rate $r=k/\Omega>0$ (scale $\Omega$), and the likelihood acquires the fixed factor $\prod 2x_i$. Square the effective positive cuts. With both free use the gamma edge test on the closed quadrant in $(k,r)$ with log likelihood $k\overline{\log y}-r\bar y-\log\int y^{k-1}e^{-ry}\,dy$. A nonconstant sample has a unique fit iff no integrable edge or corner passes its inward score tests. On $k=0$ optimize $e^{-ry}/y$ along the edge then check $\overline{\log y}-E\log y\le0$. On $r=0$ optimize $y^{k-1}$ and check $E y-\bar y\le0$. At the corner use both inequalities. Tangential optimization below refers to the full closed edge, including its corner if integrable. The only integrable $k=0$ edge requires positive lower cut and either finite upper cut or $r>0$. The $r=0$ edge with $k>0$ requires finite upper cut. The corner requires both cuts positive and finite. The gamma coercivity, strict concavity and inward score proof apply because $\log y$ and $y$ are linearly independent modulo constants. Constants collapse via $k\to\infty$, $r=k/y_0$. [Gamma edge proof](2026-11-27-trunc-expfam-math-report.md#scalar-and-two-coordinate-checks)

At held $k$, use the decreasing tilt in $y$ with rate $r>0$ and carrier $y^{k-1}$. At held $\Omega$ the carrier is $1/y$ and the tilt is $kT$ for $T=\log y-y/\Omega$. The unique fit on either ray exists exactly when the sample mean of its statistic (use $-y$ for held $k$) lies strictly between the zero-tilt mean and the statistic supremum. If the zero kernel is nonintegrable, its mean threshold is $-\infty$. For $1/y$ this follows by monotone convergence after shifting the maximum statistic to zero. As $k\downarrow0$ every fixed bounded statistic superlevel then carries vanishing conditional mass, so the mean tends to $-\infty$. The finite zero-tilt kernel wins at or below the lower threshold. At the upper equality the unique projected maximizer of the statistic gives divergent density and weak collapse. The all-held case is evaluation.

## Discretized gamma

Let $C_j(k,r)=\int_j^{j+1} y^{k-1}e^{-ry}\,dy$ for integer $j\ge0$. Use $C_j/\sum C_j$ over the retained cells. If there is one cell this equals one for every compatible parameter. Otherwise the following boundary values and scalar comparison are exhaustive at each single hold.

At held shape, $r\downarrow0$ gives grouped $y^{k-1}$ on a finite latent interval and likelihood zero on a right ray by Fatou. At $r\to\infty$ the first retained cell gets all mass. This follows by comparing its subinterval strictly below its right edge to the higher-cell tail, using one finite reference rate for domination. The collapsed likelihood is one for a sample solely in that cell and zero otherwise.

At held rate, $k\downarrow0$ gives grouped $y^{-1}e^{-ry}$ if the first cell is positive. Dominate by $y^{-1}(1+y)e^{-ry}$ there. If cell zero is retained its unnormalized mass diverges and all other masses sum to a bounded quantity, yielding the point mass at zero. As $k\to\infty$, the last cell of a finite window gets all mass by comparing its upper subinterval to the lower cells. On a right ray any fixed cell loses by comparison to a strictly larger fixed cell. In both single-hold cases compute the endpoint heights and compare with finite roots of the sample score obtained from the $C_j$. This is iff; positive endpoint atom likelihood one cannot be attained on a window with multiple cells. General cardinality of score roots is open. With exactly two retained cells the odds of the upper cell increase strictly in $k$ and decrease strictly in $r$, since the derivative of its log odds is the difference of conditional means of $\log y$ or $-y$ in the two cells. Thus a single hold yields at most one maximum in that case.

With both free and at least two retained cells, constant data fail to attain, as do samples supported on two adjacent cells if there is another retained cell. A gamma with mean strictly in one cell and $k\to\infty$ gives that cell's atom. For adjacent cells split by $j>0$, take mean $j(1+z/\sqrt{k})$ with rate $k$ divided by this mean. Along integer $k$, the standardized gamma is a standardized sum of unit exponentials, so its normal limit gives lower-cell mass $\Phi(-z)$. All other cells lose by Chebyshev. Choose $z$ to match the exact sample proportion. For two occupied cells the limit likelihood is $(n_1/n)^{n_1}(n_2/n)^{n_2}$, an upper bound for every probability vector by allocating all mass to those cells and maximizing the resulting binomial product. Every finite model with another retained cell is strictly below it. On a window of exactly two retained cells and data using both, a finite pair instead attains this height, and infinitely many do. At any fixed sufficiently large $k$ the $r\downarrow0$ odds exceed the desired odds (grouped powers concentrate in the upper cell as $k$ grows) and $r\to\infty$ gives zero odds, so there is a unique rate for each such $k$. The adjacent-cell nonattainment statement thus requires at least three retained cells. For samples with two occupied nonadjacent cells the only competitive joint escapes are the grouped faces $k=0$ ($r>0$, first cell positive), $r=0$ ($k>0$, upper cell finite), and their integrable corner (first cell positive and upper cell finite). To see this, when $k\to\infty$ compare the two occupied cells separated by a full cell. If $r/(k-1)\to t<\infty$, use the latent exponent $(k-1)(\log y-t_j y)$, $t_j=r/(k-1)$. On finite cells the maximum of the limiting exponent over each closed cell exists (the value at zero is $-\infty$). If the two maxima differ, a positive-width interval near the larger beats the entire smaller cell strictly. If equal, strict concavity gives an intervening interval beating both. These strict bounds persist at large $j$, using uniform convergence of $t_j y$ over the finite span. Ratios to the dominating interval then show loss of at least one occupied cell. If $r/(k-1)\to\infty$, the log slope above any positive pivot is eventually at most $-r/2$. An interval in the intervening cell beats the upper occupied cell exponentially. The same slope argument applies if $r\to\infty$ with bounded $k$. For finite excluded limits, dominated convergence applies on finite intervals with positive lower cut, or whenever a positive limiting shape controls zero and a positive limiting rate controls infinity. If $k\to0$ with $r$ bounded and cell zero retained, its integral diverges and every fixed positive cell loses. If the upper cut is infinite and the limiting kernel is nonintegrable at infinity, use Fatou while fixed positive-cell numerators stay bounded. These arguments exhaust subsequential limits. Hence with nonadjacent occupied cells compare against the supremum of the integrable grouped-face likelihoods just listed, or zero if none. Attainment is iff a finite fit reaches that threshold. Cardinality beyond the two-cell case needs control of all winning root components.

## Shifted Gompertz

Use $v=e^{-bx}$ and $F(x)=(1-v)e^{-\eta v}$, $\eta\ge0,b>0$. Its raw density is $bv e^{-\eta v}(1+\eta(1-v))$. For fixed $b$, the only excluded end is $\eta\to\infty$. On a finite upper window, $F(L)/F(U)\to0$ and the density at $U$ is asymptotic to $b\eta e^{-bU}$. At every observation below $U$ an exponential penalty beats this growth. On a right ray $F(L)\to0$ and fixed densities vanish. Thus with $b$ held the fit always attains except for a sample constant at finite $U$, where there is divergent collapse. This includes the candidate $\eta=0$. General cardinality is open.

With $\eta$ held, $b\downarrow0$ gives uniform on a bounded window by the density formula, and zero likelihood on a right ray because $F(L)\to0$. At $b\to\infty$, the density ratio to $be^{-bx}$ lies between $e^{-\eta}$ and $1+\eta$ uniformly in $x\ge0$. For $L>0$ this ratio tends uniformly to $1+\eta$ on $x\ge L$. Thus the small-width branch collapses at $L$, with divergent height iff the sample is constant there, and loss otherwise. The scalar rate-score comparison against the broad endpoint is iff. At $\eta=0$ the decreasing exponential mean test is exact and unique on attainment, with the uniform excluded if the bounded-uniform mean is reached or exceeded.

With both free, constants diverge by $b\to\infty$, $\eta=e^{b x_0}$ at $x_0>0$, or by $\eta=0$ and $b\to\infty$ at zero. One extra bounded-window face is $b\downarrow0$ with $b\eta\to t\ge0$. Dividing the CDF by $b e^{-\eta}$ gives $x e^{tx}$ uniformly on compact intervals, with derivative $(1+tx)e^{tx}$. The face likelihood uses that derivative and differences of $x e^{tx}$. This kernel is nonintegrable on a right ray, giving loss by Fatou. It includes the uniform at $t=0$. The remaining joint escapes lose for nonconstant data, as follows. In the coordinate $z=bx$, set $w=\eta e^{-z}$. The log-density derivative in $z$ is
$$D=-1+w+\frac{w}{1+\eta(1-e^{-z})},\qquad -1+w\le D\le-1+2w.$$
It decreases in $z$. For $b\to\infty$, let $x_*$ be its projected mode, and $w_* = \eta e^{-bx_*}$. If $w_*\le2$, integrating over distance $1/b$ on an available side of $x_*$ gives a pivoted normalizer bounded below by a constant times $1/b$, since $w\le2e$ there. The transition band $1/4\le w\le2$ has physical width at most $\log(8)/b$. Outside it the slopes in $x$ are at least $b$ on the increasing side and at most $-b/2$ on the decreasing side. Thus an observation a fixed distance from $x_*$ has exponential loss relative to the pivot. If $w_*>2$, the projected mode must be at finite $U$. The normalization strip of width $1/(bw_*)$ gives a lower bound of that order since $w/w_*\le e$ there and the log slope is at most $2ebw_*$. Everywhere on the window the log slope is at least $bw_*/2$, again yielding exponential loss at a fixed distance. The case $\eta=0$ uses the decreasing slope $-b$ directly. In each case a nonconstant sample has an observation a fixed distance from the projected mode, hence its product tends to zero.

For $b\downarrow0$ and $b\eta\to\infty$ on a bounded window, $bD$ is uniformly between positive constant multiples of $b\eta$ eventually. Integrate over width $1/(b\eta)$ near $U$ to bound the height there, while every fixed distance to its left incurs exponential loss. On a right ray compare each observed density to the kernel integral over a fixed interval strictly above the largest observation. On this finite comparison span the same positive slope lower bound gives loss. The only remaining joint end, bounded positive $b$ and $\eta\to\infty$, is covered by the fixed-rate estimates uniformly over $b$ in a compact positive interval. This exhausts all joint escapes. Thus on nonconstant data attainment is iff a finite fit reaches the largest height of the $ (1+tx)e^{tx}$ face over $t\ge0$ on a bounded window, or always on a right ray. The bounded-window face height tends to zero at large $t$ for a nonconstant sample by the same slope estimate. General cardinality is open.

## Generalized extreme value

Use the fitted shape interval $[-1,1]$ when shape is free. The unrestricted reference domain has an additional singularity. At fixed $\xi<-1$ write $e=\mu-\sigma/\xi$ and $z=(|\xi|(e-x)/\sigma)^{1/|\xi|}$. The CDF is $e^{-z}$. Moving $e\downarrow M$ at fixed positive scale gives an upper density spike of order $(e-M)^{1/|\xi|-1}$ when the retained interval at contact has positive mass. If the retained interval shrinks instead, the normalization lemma gives divergence at the occupied cut. The fixed endpoint at contact can have infinite reference density. At $\xi=0$ the model is exactly Gumbel, including its fixed-window and held-parameter results. [Reviewed Gumbel cases](2026-11-27-mod-rev4-report.md)

At held $\xi=-1$, the model is a reverse exponential of rate $t=1/\sigma$ with upper endpoint $e=\mu+\sigma$. At fixed rate, reducing $e$ to $M$ improves the likelihood whenever it removes positive retained mass and preserves the numerator. Thus for $M>a$ fit the increasing exponential on $[a,M]$ by the scalar mean test. The fit has a unique rate exactly when the sample average is strictly between the uniform mean (or $-\infty$ if $a=-\infty$) and $M$. The lower threshold gives the excluded uniform and the upper equality gives divergent collapse. If $M=a$ the sample is constant at the lower cut. Letting $e\downarrow a$ gives divergence at any fixed rate. For an attained rate, the endpoint is uniquely $M$ if $M<b$ and arbitrary $e\ge M$ if $M=b$. For instance $[0,1]$ and sample $(1/2,1)$ give infinitely many maxima, with the unique rate solving $E_t X=3/4$.

At fixed admissible $\xi>0$ the path $\mu=m$, $\sigma\downarrow0$ yields $F(m)=e^{-1}$ and $f(m)=e^{-1}/\sigma$. At each $x_i>m$,
$$ f(x_i)\sim\xi^{-1-1/\xi}\sigma^{1/\xi}(x_i-m)^{-1-1/\xi}.$$
The retained mass tends to one if the fixed lower cut is below $m$ and $M>m$, and to $1-e^{-1}$ if the lower cut equals $m<M$. Therefore $k$ minimum ties yield divergence if $k>(n-k)/\xi$, and the same product with the displayed mass denominator is the positive critical trace at equality. For $m<M$, the weak law is the atom at $m$, even at the critical height. This path is feasible at $\xi=1$ when shape is free and location is free or held at $m$. Critical attainability needs a global comparison.

An increasing exponential is accessible even on an upper-only window at fixed positive shape. Set the lower endpoint to $-D$ and write $F(x)=\exp[-C(x+D)^{-1/\xi}]$ above it. Choose $C=t\xi D^{1+1/\xi}$, $t>0$. Relative to $F(b)$, the CDF and density limits at each fixed $x\le b$ are $e^{t(x-b)}$ and $te^{t(x-b)}$. This follows directly by Taylor expansion and differentiation. CDF convergence and the fact that the limit CDF tends to zero at $-\infty$ show there is no lost mass. The likelihood on a bounded interval follows by subtracting the two limiting CDF values. On a right ray this increasing tilt is nonintegrable, and the same path loses at fixed observations since $F(a)\to0$, $F(x_i)\to0$ and the log CDF derivative stays bounded. For $-1\le\xi<0$ an analogous upper-endpoint path $e=D$, with $z=C(D-x)^q$, $q=1/|\xi|$ and $C=t/(qD^{q-1})$, gives the same increasing exponential on a left or bounded window. On a right ray with $q>1$ it loses at fixed points, since $F(a)\to0$ and the log CDF derivative stays bounded. At $q=1$ this path instead keeps the actual rate fixed and translates the upper endpoint to infinity, also losing at fixed points on a right ray. These constructions show why neither a full-line face list nor a uniform-only remote envelope suffices. The other moving support traces and global comparisons are open.

## Generalized Pareto

In the fitted branch $\xi\ge-1$ with threshold $d=\mu$, write $y=x-d$ and $S(y)=(1+\xi y/\sigma)^{-1/\xi}$ in its support. At $\xi=0$ use $S=e^{-y/\sigma}$. Above the threshold the density is $S(y)/(\sigma+\xi y)$. For $\xi<-1$ a movable upper end has the same spike exponent as for GEV, with finite retained mass at contact or the shrinking-window rule. Free unconditional shape in the reference model has this hazard as soon as a feasible choice with $\xi<-1$ can contact $M$. The subsequent scalar results use $\xi\ge-1$.

**G1. Held threshold and shape, free scale.** At $\xi>0$, use P3 with $p=1$, $h=1/\xi$, $s=\sigma/\xi$ since these are identical laws. At $\xi=0$, use the decreasing exponential test. At $-1<\xi<0$, write $e=d+\sigma/|\xi|$, with kernel $(e-x)^{1/|\xi|-1}$ on $[d,e]$. If $M>d$, contact at $e=M$ has zero density at $M$ unless retention shrinks there. Shrinking occurs if the sample is constant at the lower cut $a=M$ and $a>d$. Then $e\downarrow a$ gives density $(1/|\xi|)/(e-a)$ at $a$. If $M=d$ on a compatible fixed window, shrinking scale likewise gives divergence at $d$. Otherwise the only positive excluded limit is uniform on a bounded window as $e\to\infty$, and on a right ray the likelihood goes to zero because the retained width diverges and the normalizer grows as a power of $e$. Compare against the included joins $e=b$ and score roots. At $\xi=-1$ minimize the retained width of $[d,e]$ containing the data. For $M>L$ the minimal width $M-L$ is attained at $e=M$, with plateau $e\ge M$ when $M=b$. For $M=L$ all observations equal the lower effective cut and shrinking gives divergence. These conclusions include windows outside the support via the positive-retention and compatibility conditions.

**G2. Held threshold and scale, free shape.** Use $y_i\ge0$. The feasible shape interval is the intersection of $[-1,\infty)$ with the support inequalities. For negative shape with a sample at its upper endpoint the likelihood is zero except the included uniform at $\xi=-1$. Joins where $e=b$ are included. Products at a finite feasible edge are upper semicontinuous: the contact density is zero for $-1<\xi<0$ and at $\xi=-1$ the nonnegative power kernel at any approaching upper endpoint has limsup at most its uniform value. If the interval shrinks at a lower cut $a=M$ with $a>d$ and $-\sigma/(a-d)\ge-1$, letting $e\downarrow a$ from above gives divergence. For $\xi>0$ large and finite $U$, setting $l=L-d>0$ gives the log-uniform kernel $1/y$ since
$$
 S(l)-S(u)\sim \xi^{-1}\log(u/l),\qquad f(y)\sim(\xi y)^{-1}\quad(y>0).
$$
If $l=0$, the mass is asymptotic to $\log(1+\xi u/\sigma)/\xi$. With $k$ threshold ties the product height is asymptotic to $\xi^k\sigma^{-k}\prod_{y_i>0}y_i^{-1}/(\log(1+\xi u/\sigma))^n$. This is divergent for any tie and loses otherwise. The conditional weak limit here is the threshold atom, since for every positive excess the CDF normalized by the upper CDF tends to one. On a right ray $S(l)\to1$, all fixed positive excess densities vanish, and an all-threshold sample has constant likelihood $\sigma^{-n}$ over the full compatible shape interval (retention one for these windows). These are all scalar ends. Compare the nondivergent remote thresholds to included joins, support edges with positive retention and scalar score roots. General cardinality is open.

**G3. Held uniform shape with threshold free.** At $\xi=-1$ the conditional density is the reciprocal of retained width. With scale free the optimal width is $M-m$ for nonconstant data, since no containing interval is shorter and $[m,M]$ is feasible. Every interval of that width after intersection is optimal. Constants allow arbitrarily small width. At held positive scale $\sigma$, the feasible thresholds are $[M-\sigma,m]$ with positive retained width. On it $\min(b,d+\sigma)-\max(a,d)$ is a concave piecewise-linear function, so it has infimum at one of the extremes. For positive minimal width include both extremes if equal and include the whole segment if the width is constant. Concavity implies there are no other minimizers unless the whole segment is constant. Zero minimal width gives divergence along positive-width thresholds if feasible. For instance at scale 3 and window $[0,1]$ with sample $(0,1)$, any $d\in[-2,0]$ maximizes at held $\xi=-1$.

**G4. Free threshold paths.** At held $\xi=0$ and fixed $\sigma$, the conditional law has kernel $e^{-x/\sigma}$ on $[\max(a,d),b]$. Set $d=m$ since the denominator decreases strictly once $d>a$. The plateau for $m=a$ includes every $d\le a$. At $m=b$ constant upper-cut data give shrinking-width divergence. With $\sigma$ also free and $m<b$, the maximal branch at $d=m$ uses the decreasing exponential test, with all threshold representatives recovered by the same plateau. For $\xi>0$ and free threshold and scale, the fixed-pole argument in P4 reduces to $d=m$. With shape also free, choose a fixed $\xi>(n-k)/k$ for the $k\ge1$ threshold ties and let scale collapse in G1. This proves all-free divergence on every compatible sample, including constants at a finite upper cut by shrinking width with a uniform member. With only threshold held and shape and scale free, this divergence persists whenever the threshold is occupied. On lower-cut-separated data the positive-shape remote pole yields further integrable bounded-window limits, so that case needs a joint envelope. Other combinations of free threshold, held scale or free negative shape require global comparisons.

## Folded normal

Use $x\ge0$, $\mu\ge0$, $\sigma>0$. Up to a parameter constant its kernel is
$$
 e^{-(x-\mu)^2/(2\sigma^2)}(1+e^{-2x\mu/\sigma^2}).
$$
The second factor lies in $[1,2]$. Thus its conditional likelihood is between $2^{-n}$ and $2^n$ times the corresponding clipped normal likelihood, an estimate used for zero and divergent limits. A bounded window with $\sigma\to\infty$ and $\mu/\sigma^2\to t<\infty$ gives the kernel $\cosh(tx)$, by the equivalent kernel $e^{-x^2/(2\sigma^2)}\cosh(\mu x/\sigma^2)$. This is a two-sided exponential sum, not a single increasing exponential at finite $t>0$. The exact height uses normalizer $(\sinh(tU)-\sinh(tL))/t$, with the uniform at $t=0$. On a right ray the same finite-$t$ path loses by Fatou.

Here are all other escapes. For small $\sigma$ let $v$ be the projection of $\mu$ on the retained interval. If $\mu$ is inside, the pivoted Gaussian normalizer has order $\sigma$ (including near a finite cut). If $\mu$ is outside, write $h=|\mu-v|/\sigma^2$. The pivoted kernel at distance $z$ from $v$ is $e^{-hz-z^2/(2\sigma^2)}$ and its integral is of order $1/(h+1/\sigma)$, using an interval of that width for a lower bound and either exponential or Gaussian tail for the upper bound. The same estimate applies when $h\to\infty$ at bounded or large $\sigma$ and $U$ is finite. On nonconstant data some observation remains a fixed distance from $v$. Its exponential penalty dominates $n$ powers of the inverse width, uniformly in these paths. On a right ray with $\mu\to\infty$ and $\sigma$ bounded away from zero, for bounded $\sigma$ the retained mass is bounded below and fixed points lose. For $\sigma\to\infty$ on a right ray, mass is again bounded below since $\mu\ge0$ and $L$ is fixed, and raw densities are $O(1/\sigma)$. These cases exhaust sequences leaving compact sets after taking subsequences.

For a nonconstant sample with both coefficients free, attain iff a finite likelihood reaches the maximum bounded-window $\cosh$ height over $t\ge0$ (zero threshold on a right ray). The height tends to zero as $t\to\infty$ for a nonconstant sample, by the same exponential bound. Constant data diverge via $\mu=x_0$, $\sigma\to0$. Holding $\mu$ fixes the small-scale projection and reduces the broad bounded face to uniform. Hence constants at that projection diverge and otherwise the scalar comparison with the uniform, or zero on a right ray, is iff. Holding $\sigma$ removes all ends except $\mu\to\infty$. It gives divergence at a finite upper-cut constant and zero likelihood otherwise, so attainment is iff the sample is not such a constant. The included $\mu=0$ boundary may win. At held $\mu=0$, $-x^2$ gives a strict scalar tilt and therefore uniqueness whenever attained. General conditional root cardinality remains open. As an exact no-attainment example at held $\mu=0$, take the sample $(1,2)$ on $[1,2]$. Its empirical second moment is $5/2$, above the uniform second moment $7/3$. The positive inverse-variance tilt has strictly smaller second moment than the uniform, so increasing the inverse variance strictly decreases the likelihood. The excluded uniform has height one.

## Nonstandard beta

Let the parent support be $[d,e]$, and its shapes $u=\mathrm{shape1}$ and $v=\mathrm{shape2}$. The kernel on the retained interval is $(x-d)^{u-1}(e-x)^{v-1}$. Endpoint rules use the corresponding power. If an endpoint is free and its associated shape is free or held below one, fix the exponent negative and move that endpoint to the corresponding sample extreme. Choose the opposite endpoint strictly beyond that contact on its side if it is free. For positive retention at contact the one-sided density diverges while the ordinary conditional law converges to the corresponding truncated beta, with finite normalizer. With constant data at a cut and vanishing retained width, the normalization lemma gives divergence. These conclusions apply on the positive-compatible domain, with an endpoint fixed exactly at an observation handled by its specified inside density. If that opposite endpoint is held exactly at the same constant as the moving contact, direct scaling of the beta interval gives a factor proportional to the inverse width whenever the held-side density is positive finite, and the singular case already has infinite density.

With both endpoints held and observations strictly between them, transform to ordinary beta and use the reviewed beta moment tests with statistics $\log(x-d)$ and $\log(e-x)$. With both shapes free, constant interior data diverge by letting the common shape multiplier grow along a beta family with mean equal to the observation. For nonconstant data the unique maximizer of the full quadrant closure lies in the fitted positive quadrant iff no integrable edge or corner passes the inward score tests. The zero-shape edge on either side is integrable precisely if the retained interval is separated from that side's endpoint, provided the other shape stays positive. At the corner both cuts must be separated. Each edge kernel is the beta kernel with that zero shape substituted. With one shape held, use the other statistic's scalar positive tilt with the carrier obtained by setting the free shape to zero. The two endpoint means decide existence and give the excluded zero-shape likelihood and the divergent upper-extremum collapse. At observed held endpoints, a free associated shape admits an infinite endpoint density, and a fixed associated shape yields zero, positive or infinite inside density according as it is above, at, or below one. For a fixed associated shape equal to one, the other statistic remains finite at that endpoint and the scalar proof applies. [Reviewed beta fixed-window proof](2026-11-27-trunc-expfam-math-report.md#beta-free-truncateknown)

With both shapes held at least one and just $d$ free, consider $d=m$ and $d=a$ when feasible, the interior score roots, and the remote limit $d\to-\infty$. When the window has a finite lower cut, the remote law is the reverse power $(e-x)^{v-1}$ on $[a,\min(b,e)]$. When there is no finite lower cut its integral diverges, giving loss by Fatou after division by $(-d)^{u-1}$. The finite limit $d=m$ is included when retention is positive, with zero density at $m$ for $u>1$. The only positive excluded finite trace is divergent shrinking at an upper-cut constant. These are all ends of the scalar domain, so the endpoint-height comparison is iff. If $u=1$, the kernel is independent of $d$ and the fit, when it exists, attains at $d=m$ with the full plateau $d\le a$ when $m=a$ and positive retention. Reflect to obtain the free-$e$ result. For arbitrary held shapes, a held-side singular or zero endpoint density is handled first as above, and when the observed carriers are positive finite the same remote proof holds with an integrable fixed-end exponent greater than $-1$.

With both endpoints free and both shapes held at least one, exhaustive remote limits follow by taking subsequences of $d\le m$, $e\ge M$ in the extended line. If one endpoint is remote and the other finite, divide the kernel by the appropriate power of the remote endpoint to obtain the corresponding one-sided power. Its normalizer is finite exactly when the effective retained interval is bounded. Its finite endpoint may coincide with an observation, with zero trace for a positive associated power or the constant factor for power zero. If both endpoints are remote, divide by $(-d)^{u-1}e^{v-1}$ to get uniform on a bounded window or loss on a ray by Fatou. Finite positive-width contacts are included. A nonconstant sample prevents shrinking and bounds the remote power heights, since on an interval of width at least $M-m$ the conditional power with exponent $h\ge0$ has maximum $(h+1)/\text{width}$. Each listed positive trace is accessible by moving the remote endpoint with the finite endpoint held fixed. Compare the supremum of those one-dimensional power heights and the uniform to the finite branches; this is iff for nonconstants. Constant data diverge by scaling the whole support around the observation. For $u=v=1$ both-free endpoints reduce to uniform width minimization. For general powers, deciding the stationary set and cardinality is open. So is the joint escape test when there is no direct spike and one shape is still free.

## Triangular

Write parent endpoints $d<e$ and mode $c\in[d,e]$. Drop the common factor $2/(e-d)$ from the densities. The remaining kernel rises as $(x-d)/(c-d)$ up to $c$ and falls as $(e-x)/(e-c)$ thereafter. At an endpoint mode use the nonempty branch and assign density kernel one at the mode. If the mode lies outside the retained interval just the corresponding affine branch remains. The integral is the sum over retained pieces of their widths times their average endpoint heights. This formula applies when a cut or observation equals the mode, including an endpoint mode, because a zero-length piece contributes zero mass. The closest support edges and window cuts enter explicitly.

**Proved exact algebraic criterion for every hold and window.** Subdivide the feasible parameter domain by order comparisons among the finite cuts, the sample, and $d,c,e$. Include equality strata, and omit infinite cuts from the subdivision. For each cell form the numerator $P$ and denominator $Q>0$ of $J$, clearing all affine-branch divisors by multiplying numerator and denominator by the same positive product. Construct its attainable height set in $(0,\infty)$ by eliminating $d,c,e$ from the cell inequalities and $Qw=P$ over the ordered real field generated by data and holds. The union is a univariate semialgebraic set, described by a finite union of intervals and points, calculated for example by real quantifier elimination. The attainment condition is that its rightmost endpoint is finite and included. In the attained case substituting that endpoint into $Qw=P$ on each cell and retaining the real feasible solutions gives the full maximizing set. For algebraic inputs isolation and endpoint inclusivity are exact. This procedure provides an explicit finite computation from the sample for both attainment and cardinality. If there is no positive height, the positive-likelihood fit is incompatible. This test uses the window-dependent conditional integral; base-paper stationary observation knots are insufficient as the input candidate list.

**Proved bounds and escape localization.** Concavity of the nonnegative height kernel on the retained interval bounds every conditional density by $2/(U-L)$: integrate the two triangles from a peak to each retained endpoint. Thus for nonconstants no divergence is possible and sequences with retained width tending to infinity have likelihood zero. For constants, shrinking compatible widths can diverge, for example with free endpoints and mode all near the common observation. Under holds the algebraic test detects divergence even at a constant at the window cut. If a nonconstant supremum is excluded, a competitive sequence has a subsequence with finite limiting retained endpoints and peak position projected onto the retained interval, and with convergence of the nonnegative piecewise-linear kernel normalized to peak one. In fact its values at the retained endpoints lie in $[0,1]$, and together with the projected knot and peak determine the graph in the interior. For positive limiting width the graph converges Lebesgue almost everywhere by the affine formulas, so its integral converges. The limiting conditional law is a normalized one- or two-piece affine kernel. A zero-length side at a cut can have an observation trace different from the a.e. graph. Its sample factor is found by taking the corresponding rational function on the cell and using its one-sided limit. The algebraic criterion tests whether such competing heights are reachable, including remote endpoints leaving a uniform or affine kernel. An explicit human-readable partition into winning face types for the eight masks is open.

**Exact non-uniqueness.** Hold $d=-1,e=2$, take $[a,b]=[0,1]$ and sample $(0,1)$. For $c\le0$ the kernel is proportional to $2-x$, and for $c\ge1$ it is proportional to $x+1$. Each gives $J=8/9$. On $0<c<1$ put $D=(c+1)(2-c)>2$. The kernel product is $1/D$, and the integral equals $\tfrac32(1-1/D)$ by integration of the two lines. Thus $J=4D/[9(D-1)^2]<8/9$ by strict decrease for $D>1$. The entire maximizing set is $c\in[-1,0]\cup[1,2]$.
## Mask decisions

### dgamma free truncate:known

[Derivation](#discretized-gamma)

- **Attainment · Proved.** Singleton windows are flat. On multiple cells constants do not attain. Adjacent occupied cells attain iff they exhaust the window, and nonadjacent occupied cells attain iff a finite model reaches the grouped-face threshold.
- **Failure · Proved.** A first or other occupied cell atom, an adjacent empirical Bernoulli, or an integrable grouped zero-shape, zero-rate or corner face.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** Two occupied adjacent cells give a nonattained empirical Bernoulli on windows with other cells, and infinitely many fits on exactly two cells.

### dgamma rate truncate:known

[Derivation](#discretized-gamma)

- **Attainment · Proved.** On multiple cells, compare scalar shape roots with the explicit grouped-kernel or atom heights. A singleton is flat.
- **Failure · Proved.** Small-shape first atom or grouped inverse-power exponential, or large-shape last atom.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### dgamma rate+shape truncate:known

[Derivation](#discretized-gamma)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### dgamma shape truncate:known

[Derivation](#discretized-gamma)

- **Attainment · Proved.** On multiple cells compare scalar rate roots with the explicit grouped-power or atom heights. A singleton is flat.
- **Failure · Proved.** Small-rate grouped power or large-rate first-cell atom.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### expweibull free truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Proved subcase.** Every positive constant can be matched by scale and has a divergent free-shape path.

### expweibull exponent truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.

### expweibull exponent+scale truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** For positive data outside projected-scale constant divergence, a scalar shape-score root must reach the small-shape threshold.
- **Failure · Proved.** For positive data the only competitive ends are projected-scale collapse and, on a bounded window away from zero, log-uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### expweibull exponent+shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** For positive data outside the lower-cut constant collapse case, a scale-score root must reach the larger explicit endpoint height.
- **Failure · Proved.** For positive data, small-scale lower-cut constant collapse or the bounded broad-scale power face.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### expweibull scale truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.

### expweibull scale+shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** On positive observations use E1's strictly increasing transformed mean range.
- **Failure · Proved.** On positive observations, upper-cut collapse or the integrable exponent-zero derivative carrier.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### expweibull shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.

### fatigue free truncate:known

[Derivation](#fatigue)

- **Attainment · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Failure · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Cardinality · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Proved subcase.** On positive-compatible constants a free scale or threshold enables concentration.

### fatigue alpha truncate:known

[Derivation](#fatigue)

- **Attainment · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Failure · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Cardinality · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Proved subcase.** On positive-compatible constants a free scale or threshold enables concentration.

### fatigue alpha+beta truncate:known

[Derivation](#fatigue)

- **Attainment · Proved.** Outside the upper-cut constant, compare threshold-score roots with the remote decreasing exponential height or zero.
- **Failure · Proved.** Upper-cut constant shrinking or a remote integrable decreasing exponential.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### fatigue alpha+mu truncate:known

[Derivation](#fatigue)

- **Attainment · Proved.** Attains for positive-compatible data except a constant at an accessible finite concentration cut.
- **Failure · Proved.** Small-scale lower-cut or large-scale upper-cut constant collapse.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### fatigue beta truncate:known

[Derivation](#fatigue)

- **Attainment · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Failure · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Cardinality · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.

### fatigue beta+mu truncate:known

[Derivation](#fatigue)

- **Attainment · Proved.** The empirical squared normal coordinate has mean strictly inside the inverse-shape tilt range.
- **Failure · Proved.** Projected-center collapse or the integrable uniform-in-normal-coordinate face.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### fatigue mu truncate:known

[Derivation](#fatigue)

- **Attainment · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Failure · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Cardinality · Open.** Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.
- **Proved subcase.** On positive-compatible constants a free scale or threshold enables concentration.

### foldnorm free truncate:known

[Derivation](#folded-normal)

- **Attainment · Proved.** Nonconstant data attain iff a finite fit reaches the optimized bounded-window cosh height or zero on a right ray.
- **Failure · Proved.** Constant collapse or the bounded-window cosh family, including uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### foldnorm mu truncate:known

[Derivation](#folded-normal)

- **Attainment · Proved.** Outside constants at the projected held location, compare finite scale roots with the bounded uniform height or zero.
- **Failure · Proved.** Projected-location collapse or bounded uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** At mu=0 the inverse-variance tilt has at most one maximum.

### foldnorm mu+sigma truncate:known

[Derivation](#folded-normal)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### foldnorm sigma truncate:known

[Derivation](#folded-normal)

- **Attainment · Proved.** Attains except for constants at a finite upper cut.
- **Failure · Proved.** Concentration at an occupied upper cut under remote location.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### gev free truncate:known

[Derivation](#generalized-extreme-value)

- **Attainment · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Failure · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Cardinality · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Proved subcase.** An increasing exponential is an accessible face on upper-only and bounded windows.
- **Proved subcase.** In the held xi=-1 submodel the truncated reverse-exponential mean test and endpoint plateau give the exact winning set.
- **Proved subcase.** The xi=1 small-scale path diverges at a strict majority minimum, when mu can equal that minimum and the window has positive width.

### gev mu truncate:known

[Derivation](#generalized-extreme-value)

- **Attainment · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Failure · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Cardinality · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Proved subcase.** The xi=1 small-scale path diverges at a strict majority minimum, when mu can equal that minimum and the window has positive width.

### gev sigma truncate:known

[Derivation](#generalized-extreme-value)

- **Attainment · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Failure · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Cardinality · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.

### gev xi truncate:known

[Derivation](#generalized-extreme-value)

- **Attainment · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Failure · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Cardinality · Open.** Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.
- **Proved subcase.** An increasing exponential is an accessible face on upper-only and bounded windows.
- **Proved subcase.** In the held xi=-1 submodel the truncated reverse-exponential mean test and endpoint plateau give the exact winning set.

### gpd free truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Proved.** Every compatible sample on the fitted shape branch admits a divergent path.
- **Failure · Proved.** Positive-shape scale collapse with threshold at the sample minimum, or uniform shrinking on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### gpd mu truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Failure · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Cardinality · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Proved subcase.** Threshold ties allow divergent positive-shape scale collapse when shape and scale are free.

### gpd mu+sigma truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Proved.** Compare finite shape roots and included joins to G2's remote height after testing shrinking support and threshold divergence.
- **Failure · Proved.** Shrinking support, divergent threshold atom, bounded log-uniform or the attained all-threshold right-ray plateau.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### gpd mu+sigma+xi truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### gpd mu+xi truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Proved.** Use G1's two-end comparison, decreasing exponential moment test or uniform width minimization, according to held shape.
- **Failure · Proved.** Threshold collapse, shrinking at a lower cut, integrable tail power, critical contact trace or bounded uniform, according to shape.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### gpd sigma truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Failure · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Cardinality · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.

### gpd xi truncate:known

[Derivation](#generalized-pareto)

- **Attainment · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Failure · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Cardinality · Open.** Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.
- **Proved subcase.** For xi=-1 width minimization gives every maximizer and all constant data diverge with both endpoints free.

### naka free truncate:known

[Derivation](#nakagami)

- **Attainment · Proved.** Nonconstant data attain iff no integrable edge or corner of the gamma quadrant passes its inward scores.
- **Failure · Proved.** Collapse or one of the integrable gamma zero-shape, zero-rate and log-uniform faces.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### naka scale truncate:known

[Derivation](#nakagami)

- **Attainment · Proved.** The mean of log squared observation minus squared observation divided by held scale is strictly inside the positive-tilt range.
- **Failure · Proved.** Projected-center collapse or zero-tilt log-uniform on a window cut away from both ends.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### naka scale+shape truncate:known

[Derivation](#nakagami)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### naka shape truncate:known

[Derivation](#nakagami)

- **Attainment · Proved.** The empirical squared observation mean is strictly inside the decreasing-rate range.
- **Failure · Proved.** Lower-cut collapse or finite-window power.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### nsbeta free truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta max truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta max+min truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On positive finite carriers transfer the ordinary beta inward-score or scalar moment test.
- **Failure · Proved.** On positive finite carriers, integrable zero-shape beta edges or corner, or collapse.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### nsbeta max+min+shape1 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On positive finite carriers transfer the ordinary beta inward-score or scalar moment test.
- **Failure · Proved.** On positive finite carriers, integrable zero-shape beta edges or corner, or collapse.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### nsbeta max+min+shape1+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### nsbeta max+min+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On positive finite carriers transfer the ordinary beta inward-score or scalar moment test.
- **Failure · Proved.** On positive finite carriers, integrable zero-shape beta edges or corner, or collapse.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### nsbeta max+shape1 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Open.** Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.
- **Failure · Open.** Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.
- **Cardinality · Open.** Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta max+shape1+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** For shapes at least one, use the endpoint comparison with explicit remote powers and uniform. Free-side held shapes below one yield a spike on compatible data.
- **Failure · Proved.** On the positive-carrier domain there is a moving-endpoint spike, shrinking constant, integrable power or bounded uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta max+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta min truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta min+shape1 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta min+shape1+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** For shapes at least one, use the endpoint comparison with explicit remote powers and uniform. Free-side held shapes below one yield a spike on compatible data.
- **Failure · Proved.** On the positive-carrier domain there is a moving-endpoint spike, shrinking constant, integrable power or bounded uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta min+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Open.** Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.
- **Failure · Open.** Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.
- **Cardinality · Open.** Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta shape1 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta shape1+shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** For shapes at least one, use the endpoint comparison with explicit remote powers and uniform. Free-side held shapes below one yield a spike on compatible data.
- **Failure · Proved.** On the positive-carrier domain there is a moving-endpoint spike, shrinking constant, integrable power or bounded uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### nsbeta shape2 truncate:known

[Derivation](#nonstandard-beta)

- **Attainment · Proved.** On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.
- **Failure · Proved.** An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.
- **Cardinality · Proved.** There is no attained positive finite fit.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pareto3 free truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** Positive-compatible data admit no finite MLE.
- **Failure · Proved.** At any sample minimum choose a fixed shape below one and approach contact, or shrink retained width on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### pareto3 min truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Failure · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Cardinality · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.

### pareto3 min+scale truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Failure · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Cardinality · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.

### pareto3 min+scale+shape truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### pareto3 min+shape truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** With a held threshold and all shapes held, compare scalar scale roots with the explicit two ends (P3), on the positive finite likelihood domain.
- **Failure · Proved.** Broad bounded power, narrow integrable tail power, or a contact atom with a critical finite trace or divergence.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### pareto3 scale truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** Positive-compatible data admit no finite MLE.
- **Failure · Proved.** At any sample minimum choose a fixed shape below one and approach contact, or shrink retained width on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### pareto3 scale+shape truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** With scale and all shapes held, apply P4 to scalar threshold roots and included contact and cut joins on the positive finite likelihood domain.
- **Failure · Proved.** Spike for shape below one, upper-cut constant shrinking, or remote uniform on bounded windows.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pareto3 shape truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Failure · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Cardinality · Open.** Bound joint pole, shape and scale escapes, including ties and the upper cut.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pareto4 free truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** Positive-compatible data admit no finite MLE.
- **Failure · Proved.** At any sample minimum choose a fixed shape below one and approach contact, or shrink retained width on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### pareto4 min truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.

### pareto4 min+scale truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.

### pareto4 min+scale+shape1 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.

### pareto4 min+scale+shape1+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### pareto4 min+scale+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** On positive finite carriers, the empirical transformed mean lies strictly inside the positive-rate range (P2).
- **Failure · Proved.** Lower constant collapse or zero-rate uniform in the log-survival coordinate.
- **Cardinality · Proved.** Every attained fit on the stated domain is unique by strict concavity.

### pareto4 min+shape1 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.

### pareto4 min+shape1+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** With a held threshold and all shapes held, compare scalar scale roots with the explicit two ends (P3), on the positive finite likelihood domain.
- **Failure · Proved.** Broad bounded power, narrow integrable tail power, or a contact atom with a critical finite trace or divergence.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### pareto4 min+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.

### pareto4 scale truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** Positive-compatible data admit no finite MLE.
- **Failure · Proved.** At any sample minimum choose a fixed shape below one and approach contact, or shrink retained width on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### pareto4 scale+shape1 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** Positive-compatible data admit no finite MLE.
- **Failure · Proved.** At any sample minimum choose a fixed shape below one and approach contact, or shrink retained width on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### pareto4 scale+shape1+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** With scale and all shapes held, apply P4 to scalar threshold roots and included contact and cut joins on the positive finite likelihood domain.
- **Failure · Proved.** Spike for shape below one, upper-cut constant shrinking, or remote uniform on bounded windows.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pareto4 scale+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pareto4 shape1 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Proved.** Positive-compatible data admit no finite MLE.
- **Failure · Proved.** At any sample minimum choose a fixed shape below one and approach contact, or shrink retained width on an upper-cut constant.
- **Cardinality · Proved.** There is no attained positive finite fit.

### pareto4 shape1+shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pareto4 shape2 truncate:known

[Derivation](#pareto-iii-and-iv)

- **Attainment · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Failure · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Cardinality · Open.** Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.
- **Proved subcase.** If a free endpoint's corresponding power is below one or its shape can be chosen below one, contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge.

### pgw free truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Proved subcase.** Every positive constant can be matched by scale and has a divergent free-shape path.

### pgw exponent truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.

### pgw exponent+scale truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** For positive data outside projected-scale constant divergence, a scalar shape-score root must reach the small-shape threshold.
- **Failure · Proved.** For positive data the only competitive ends are projected-scale collapse and, on a bounded window away from zero, log-uniform.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### pgw exponent+scale+shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### pgw exponent+shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** For positive data outside the lower-cut constant collapse case, a scale-score root must reach the larger explicit endpoint height.
- **Failure · Proved.** For positive data, small-scale lower-cut constant collapse or the bounded broad-scale power face.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### pgw scale truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.

### pgw scale+shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Proved.** On positive data, outside collapse, compare exponent-score roots with the small-exponent boundary height.
- **Failure · Proved.** On positive data, lower-cut constant collapse or a finite-upper window log-coordinate carrier.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### pgw shape truncate:known

[Derivation](#exponentiated-weibull-and-pgw)

- **Attainment · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Failure · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.
- **Cardinality · Open.** Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.

### sgomp free truncate:known

[Derivation](#shifted-gompertz)

- **Attainment · Proved.** Nonconstant data attain on a right ray, or iff a finite value reaches the optimized onset-kernel height on a bounded window.
- **Failure · Proved.** Constant collapse or a bounded-window onset kernel proportional to (1+tx)exp(tx) for t nonnegative.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### sgomp b truncate:known

[Derivation](#shifted-gompertz)

- **Attainment · Proved.** Attains except on a constant at the finite upper cut.
- **Failure · Proved.** Constant-at-upper-cut collapse.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### sgomp b+eta truncate:known

[Derivation](#shifted-gompertz)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.

### sgomp eta truncate:known

[Derivation](#shifted-gompertz)

- **Attainment · Proved.** Outside a lower-cut constant, compare finite rate roots to the broad-scale threshold.
- **Failure · Proved.** Lower-cut constant collapse or uniform on a bounded window.
- **Cardinality · Open.** Isolate and rank the winning conditional score roots or determine the maximizing component.

### triang free truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

### triang a truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

### triang a+b truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.
- **Proved subcase.** At support [-1,2], window [0,1] and sample (0,1), modes in [-1,0] or [1,2] all maximize.

### triang a+b+c truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** The compatible positive finite conditional likelihood is attained at the held model.
- **Failure · Proved.** There is no parameter escape on the single feasible coefficient vector.
- **Cardinality · Proved.** The feasible coefficient vector is unique.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

### triang a+c truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

### triang b truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

### triang b+c truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

### triang c truncate:known

[Derivation](#triangular)

- **Attainment · Proved.** Eliminate coefficients cellwise from the cleared conditional-height equation. Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points.
- **Failure · Open.** Find simple face-specific inequalities without algebraic elimination.
- **Cardinality · Proved.** The rightmost included height fiber gives the exact maximizing set.
- **Proved subcase.** The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge.

## Summary

| Family | Exact fixed-window result | Remaining frontier |
| --- | --- | --- |
| Pareto III and IV | Free-threshold spikes, held-shape scalar endpoint comparisons, and the Pareto IV log-survival moment test | Joint shape, pole and scale limits without a direct spike |
| Exponentiated Weibull | Held-coordinate scalar comparisons for positive data, including the exponent moment test | Joint escapes and zero observations |
| PGW | Held-coordinate scalar comparisons for positive data, including the exponent-zero carrier | Joint escapes, especially zero retained mass in the threshold limit |
| Fatigue | Three scalar regimes with the correct projected or shrinking cut | Simultaneous threshold, shape and scale changes |
| Nakagami | Gamma edge comparison and unique scalar moment fits | No open positive-carrier regime in these masks |
| Discretized gamma | Grouped faces, adjacent-cell empirical Bernoulli, and the two-cell odds ridge | Cardinality of scalar and joint winners beyond two cells |
| Shifted Gompertz | Joint onset face and exact attainment comparisons for both single holds | Conditional stationary winner cardinality |
| GEV | Reverse-exponential submodel, strict-majority spike, and upper-only exponential path | Critical height and competing escaped parameters |
| GPD | All-free divergence and exact scale-only and shape-only boundary comparisons | Joint held-threshold or held-scale fits and remaining fixed-shape comparisons |
| Folded normal | Full attainment criterion with the bounded hyperbolic-cosine face, plus both single holds | Conditional stationary winner cardinality |
| Nonstandard beta | Fixed-support beta, moving-support spikes, and held-shape power comparisons | Joint shape and endpoint escapes in nonsingular masks |
| Triangular | Exact semialgebraic attainment and maximizing-set test, with an exact plateau example | Explicit elementary partition by winning limit law |

## Numerical checks

**Numerical.** The retained Python script checks 39 values at 100 decimal digits on the workstation. For folded normal the grid is $\sigma=10^j$ for $j=1,2,3,4$ with $\mu=2\sigma^2$. For shifted Gompertz use $b=10^{-j}$ for $j=2,4,6,8$ with $b\eta=2$. The GEV upper-only face uses $D=10^j$ for $j=2,4,6,8$ at $\xi=1,t=2$. The GPD uses $\xi=\exp(10^j)$ for $j=0,1,2,3$ with unit scale. The Pareto upper-cut shrinking path uses gap $10^{-j}$ for $j=2,4,6,8$. The EW and PGW threshold traces use $p=10^j$ for $j=2,3,4$ with $pc=2$. The triangular check uses modes from $-1$ to $2$ in quarter-unit increments on the exact sample in the proof. Errors for the asymptotic checks decrease across their respective grids. These checks illustrate the analytic limits without ranking fitted roots. [Script](2026-11-27-trunc-misc-math-data/check_limits.py) · [Table](2026-11-27-trunc-misc-math-data/limit-checks.csv)

The mask assembly script checks membership in the modifier atlas, uniqueness of row keys, existence of every evidence anchor and presence of a frontier on each open row. Both final Python runs use the workstation's single-slot wrapper. [Assembly and validation](2026-11-27-trunc-misc-math-data/assemble.py) · [Proof rows](2026-11-27-trunc-misc-math-data/proofs.csv)

## What this changes for the fitter

The routes derived here are unimplemented. A certified scalar route would need stable CDF or survival differences, finite brackets or root coverage on each support-compatible branch, and comparisons to explicitly computed face heights. The monotone moment branches give the root sign and uniqueness directly. Joint fits with proved closure lists still need finite candidate coverage or another verified global bound. In particular a successful finite numerical search may be returned as uncertified when a sign or global ranking remains unresolved. A proved strict loss to an excluded face must report the corresponding limit or nonexistence. A finite tie with a face permits attainment. Return all proved finite maximizers when feasible to represent them, including parameter plateaus. At cut contacts, use the trace for the actual observation and preserve the declared inside density at included support endpoints. Continuous collapse and bounded discrete point-mass height require distinct checks. [Failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

The per-mask rows specify unresolved questions. The most consequential missing bounds are the mixed shape and scale paths through vanishing retained mass for EW and PGW, the moving-threshold plus changing-shape escape for fatigue and nonsingular nonstandard beta, and the joint GEV comparison at critical minimum ties. For GPD, the movable threshold with held scale and the joint shape-scale problem above a separated lower cut still need exhaustive bounds. General scalar critical-root cardinality remains open wherever the only proof is a compactness comparison. For triangular fits the algebraic test includes endpoint contacts and remote affine limits, but simpler inequalities identifying the winning face on each hold remain open.
