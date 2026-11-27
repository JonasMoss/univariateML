# Maximum likelihood under fixed truncation for location-scale families

TASK-449. Let the closed window be $A=[a,b]$, of positive length, possibly with one infinite end. The sample is nonempty and finite. Write $m=min x_i$, $M=max x_i$, and $n$ for sample size. Observations outside the retained support and holds giving zero retained mass are infeasible. All statements below use the pointwise closed-support convention. For Student, the main classification holds any specified $\nu>2$ fixed, with $\nu=3$ used for examples. Claims are **proved** unless explicitly marked numerical or open.

## General lemmas

### Likelihood and comparison

For a positive kernel $k$ on the effective interval $I$ the likelihood is
$$P(k,I)=\frac{\prod_i k(x_i)}{(\int_I k)^n}.$$
It is invariant under common rescaling of $k$. Thus even for an excluded boundary law, the likelihood can be calculated without evaluating parent tail differences. For an exponential tilt of signed slope $\lambda$, use $k=e^{\lambda(x-o)}$ with any finite origin $o$. The integral on a bounded interval is $(e^{\lambda(b-o)}-e^{\lambda(a-o)})/\lambda$ with continuous value $b-a$ at zero. On the right ray only $\lambda<0$ is integrable, with integral $e^{\lambda(a-o)}/(-\lambda)$; on the left ray only $\lambda>0$ is integrable, with integral $e^{\lambda(b-o)}/\lambda$. The maximizing slope matches the sample mean, since the second derivative of log likelihood is $-n\operatorname{Var}_\lambda X$. On rays the decay rate is the reciprocal of the mean distance from the finite end, and the likelihood is $(\text{rate}/e)^n$. On a bounded window the uniform value is $(b-a)^{-n}$. For an endpoint-only sample the unrestricted tilt supremum is infinite, by concentration at that endpoint.

Here is the **score-and-boundary criterion** used below. It is an implicit sample condition, with explicit equations and thresholds. For coordinates $v$ and fixed effective interval, take every solution in the finite parameter domain of
$$\sum_i \partial_v\log k(x_i)
= n\frac{\int_I k\,\partial_v\log k}{\int_I k}.$$
Add interval endpoints of finite parameter pieces, kink lines and support breakpoints, solving the score along their free coordinates or taking the entire piece if the likelihood is constant there. For a moving integration endpoint, differentiate the integral with its endpoint term, or use the separate fixed-interval branches below. The kernels here are differentiable under the integral on compact subsets of each smooth piece, by their exponential or integrable power tails. For Laplace the integral can also be split at its kink. Let $B$ be the largest of the listed excluded-face likelihood suprema, including zero. With $B<\infty$, a fit exists iff **some finite candidate has $P\ge B$**. All candidates of greatest likelihood are precisely the maximizing set. If there is no such candidate, the supremum is $B$ and its maximizing boundary paths come from the listed faces. If $B=\infty$, there is no finite maximum. These statements follow by subsequence extraction in the extended coordinates and the boundary bounds below. A point strictly above $B$ forces a finite maximum. A point at $B$ either maximizes already or there is a point strictly above $B$. Conversely every finite maximum satisfies the candidate conditions. This also proves the criterion with a finite witness anywhere in the domain in place of a candidate. It does not bound the number of roots or supply a stopping rule for isolating them. See the [joint envelope and closure lemma](../ml-behaviour.tex) and [exponential-family theorem](2026-11-27-trunc-expfam-math-report.md#general-theorem).

When both coefficients are held (or all three for Student), there is exactly one fit for any compatible sample and positive retained mass, with likelihood $P$. We use **diffusion** for paths on unbounded windows with density tending to zero at each finite point. A weak atom is described together with the joint sample density limit; an atomic probability mass cannot substitute for a density in this comparison.

### Symmetric exponential tails

For Laplace and logistic put $r=1/\sigma$. The respective kernels are $e^{-r|x-\mu|}$ and $e^{-r|x-\mu|}/(1+e^{-r|x-\mu|})^2$. The latter lies between one quarter and one times the former. For $r\downarrow0$ the log kernels are $r$-Lipschitz uniformly in location, giving uniform convergence of normalized densities on bounded windows. On a ray integrate a segment of length $1/r$ extending inward from each observation to get $q(x_i)\le er$. For $r\to r_0\in(0,\infty)$ and escaping location the kernel ratio at fixed points converges to the signed exponential, increasing for $\mu\to+\infty$ and decreasing for $\mu\to-\infty$. On an integrable ray the Laplace ratio is eventually exactly its exponential and the logistic correction is uniformly bounded by the exponential envelope, so normalization converges. On the other ray Fatou gives an infinite relative normalizer and zero pointwise density.

For $r\to\infty$, let $z$ be the projection of $\mu$ onto $A$. The Laplace centered kernel is exactly $e^{-r|x-z|}$ on the window, since $|x-\mu|-|z-\mu|=|x-z|$. The logistic density is within factors $1/4$ and $4$ of the normalized Laplace density. Thus $q(x)\le C r e^{-r|x-z|}$ for large $r$, with the reverse bound up to a constant at $z$. Nonconstant samples have likelihood zero in the limit, since $\sum_i |x_i-z|\ge M-m$. The laws concentrate within distance $O(1/r)$ of $z$, hence converge weakly to $\delta_c$ when $z\to c$, or drift if $z$ escapes. Constants diverge if $z$ can be set at the observation; with held location they diverge only at its projection, and otherwise have zero collapse likelihood.

### Power tails and critical collapse

For Cauchy, half-Cauchy and held Student set
$$k(x)=((x-\mu)^2+s^2)^{-p}.$$
For Cauchy and half-Cauchy $p=1$, $s=\sigma$. For Student $p=(\nu+1)/2$, $s=\mathrm{sd}\sqrt{\nu-2}$. For the half-law, $I=[\max(a,\mu),b]$ and $\mu\le m$. The full laws use $I=A$.

If $s+|\mu|\to\infty$, ratios of the kernel at fixed points tend to one, by expansion of the quadratic. On bounded windows this convergence is uniform, on the limiting effective interval for half-Cauchy. In the half-law with finite $b$, finite limiting endpoint $c\le m$ and $s\to\infty$, the result is uniform on $[\max(a,c),b]$, including when $a=-\infty$. The largest such likelihood for free location and nonconstant data is $(b-m)^{-n}$. For held location use its effective interval. In all other unbounded-window cases at this parameter end the density at each observation goes to zero. Indeed on an unbounded retained ray with stable finite cut, integrate a segment of length proportional to $D=(s^2+(x_i-\mu)^2)^{1/2}$ extending into the ray. The triangle inequality bounds the quadratic on it by $4D^2$. In a half-law with $\mu\to-\infty$, a segment of diverging length to the left of each observation is retained when $a=-\infty$, and its decreasing kernel gives the same zero-density conclusion. On bounded windows the limiting uniforms follow by integration; diffuse cases have vanishing mass on every fixed bounded set by the same estimates uniformly on compacts.

If $s\downarrow0$ and $\mu\to c$ strictly outside the closed retained window $A$, with allowed location, the limit kernel is $|x-c|^{-2p}$ on $A$, with integral
$$Z_c=
\begin{cases}
((a-c)^{1-2p}-(b-c)^{1-2p})/(2p-1),&c<a,\\
((c-b)^{1-2p}-(c-a)^{1-2p})/(2p-1),&c>b .
\end{cases}$$
An infinite distance contributes zero. The likelihood is $\prod_i|x_i-c|^{-2p}/Z_c^n$. Dominated convergence applies even on the retained ray because $2p>1$. Holds specify which $c$ are permitted.

For $c\in A$ put $\rho=\sqrt{s^2+(\mu-c)^2}$ and $d=(\mu-c)/\rho$. The substitution $z=(x-c)/\rho$ gives the exact rescaled integral $\rho^{1-2p}J_\rho$, where $J_\rho$ integrates $(1-2dz+z^2)^{-p}$ over the rescaled retained interval, intersected with $[d,\infty)$ for a half-law. If $k_c$ observations equal $c$, the joint likelihood is
$$\rho^{2p(n-k_c)-n}\,J_\rho^{-n}\,\prod_{x_i\ne c}((x_i-\mu)^2+s^2)^{-p}.$$
For each subsequential $d\to d_0$, the integration domain tends to the line or the inward half-line at a cut, with the half-law constraint as above. The limiting integral $J(d_0)$ can be infinite. Fatou gives the lower bound; convergence when finite follows by bounded-domain convergence and an integrable uniform tail bound $O(|z|^{-2p})$. At $|d_0|=1$, a singularity at $z=d_0$ in the limiting interval gives infinite integral, also when approached from the retained side. The intervals have a uniformly positive integral for all such paths when $c<b$ for the half-law or for any $c$ for the full law. A half-law can reach $c=b$ only on an upper-end constant sample, already unbounded when both parameters are free. Nonconstant cases are therefore zero above exponent zero, unbounded below it (take $\mu=c$), and at equality the **largest** joint likelihood at $c$ is $J_*^{-n}\prod_{x_i\ne c}|x_i-c|^{-2p}$. The constants are
$$C_p=\int_{\mathbb R}(1+z^2)^{-p}\,dz
=\sqrt\pi\,\Gamma(p-1/2)/\Gamma(p).$$

| Collapse position | Full law | Half-law |
| --- | --- | --- |
| Interior of $A$ | $J_*=C_p$ | $J_*=C_p/2$ |
| Finite cut, location held at cut | $J_*=C_p/2$ | $J_*=C_p/2$ at lower cut |
| Finite cut, location free | $J_*=1/(2p-1)$ | $J_*=1/(2p-1)$ at lower cut |

At interior points translate the rescaled integral by $d$ and scale by $\sqrt{1-d^2}$. This gives $C_p(1-d^2)^{(1-2p)/2}$ or half that value, minimized at zero. The gamma expression follows by inserting the gamma integral for $(1+z^2)^{-p}$ and performing the Gaussian integral. At a cut and free location, aiming $d$ outward gives the least integrand pointwise on the inward ray, with infimum at $d_0=\pm1$. The half-law constraint retains the whole ray on that path and can remove no part of that ray when there are ties at the lower cut ($\mu\le m=c$). The outward path is realized by $s=o(|\mu-c|)$. The weak limit at a collapse point is $\delta_c$: the integral away from any fixed neighborhood is bounded, whereas $\rho^{1-2p}J_\rho$ diverges. For a free half-law constant at $b$, take $\mu\uparrow b$ and $s\downarrow0$ with $b-\mu=s$. Its support shrinks to $b$ and $q(b)$ is proportional to $1/s$.

For the nondegenerate effective intervals under discussion, these formulas exhaust the ends: for any sequence take subsequences of $s$ in $[0,\infty]$ and $\mu$ in the extended line. A finite half-law endpoint crossing $a$ leaves the likelihood continuous on the feasible domain, by the closed endpoint convention. For power faces with free exterior $c$, their exact threshold is the maximum of the indicated one-variable function and its endpoint limits. Evaluate its derivative in $c$ at its roots and include limits at finite cuts (the same exterior collapse formula) and infinite distance (uniform on bounded windows, zero on rays).

## Family and hold regimes

### laplace all free truncate:known

Constants collapse at the observation. For nonconstant data on a ray a finite fit always exists. On a bounded window the score-and-boundary criterion has $B=(b-a)^{-n}$. Finite-rate integrable exponential faces are already represented by finite locations outside the interval, including its cuts, so they do not require a strict comparison. One can take the finite candidate list in location from the held-scale section, then solve the rate score at each location or exponential plateau. If a maximizing sequence of nonconstant data on a ray escapes with positive likelihood it must have a finite-rate represented limit by the exponential-tail lemma. Otherwise it has a finite subsequence. On a bounded window, balanced observations $(a,b)$ have no fit and approach only the uniform likelihood supremum. For these data, log concavity gives $k(x)\ge k(a)^{(b-x)/(b-a)}k(b)^{(x-a)/(b-a)}$. Integration via Jensen yields $Z\ge(b-a)\sqrt{k(a)k(b)}$, with equality requiring a constant kernel. The same argument applies to logistic and Gumbel below. In general the criterion gives the maximizing set by comparison of candidate rates and location pieces. On a right ray the exact sample $(a,a+1)$ has a continuum of maximizers: for each $r$, the location candidates are the left plateau and $\mu=a+1$. The ratio of the latter likelihood to the plateau is $(2-e^{-r})^{-2}<1$. The plateau optimizes at $r=2$.

### laplace mu truncate:known

Set $T(x)=|x-\mu|$. The rate score per observation is $E_rT-\bar T$ and has derivative $-\operatorname{Var}_rT<0$. As $r\to\infty$, the mean tends to the minimum at the projection; this follows by direct exponential integration on the two monotone pieces. At $r\downarrow0$ the limiting mean is the uniform mean on a bounded window and infinite on a ray, by integration on its unbounded linear piece. Attainment and uniqueness hold exactly when $\inf_A T<\bar T<E_{0}T$. Lower equality gives unbounded atom collapse. At or above the finite upper threshold the uniform wins by strict decrease of the score.

### laplace sigma truncate:known

For fixed $r$, on each location gap inside the window the numerator log likelihood is affine and the normalizer is $(2-e^{-r(\mu-a)}-e^{-r(b-\mu)})/r$, omitting terms at infinite cuts. Its second derivative is negative for the windows considered, so the log likelihood is strictly convex on each gap. The maxima are the best of the sample locations, finite cuts, and integrable exterior plateaus. The nonintegrable exterior limit is zero. This proves attainment on all windows and specifies all maximizing locations. If all observations are the finite lower cut on a right ray or bounded interval, all $\mu\le a$ maximize. Indeed $k(x)/k(a)\ge e^{-r(x-a)}$ for $x\ge a$, with equality throughout exactly on that plateau. Reflect for upper cuts.

### laplace mu+sigma truncate:known

Both held. Apply the fixed coefficient rule in the likelihood comparison section.

### logis all free truncate:known

Use $y=e^{r(x-o)}$, $c=e^{r(\mu-o)}>0$, $L=e^{r(a-o)}$, $U=e^{r(b-o)}$. For scoring, use
$$q(x)=\frac{r y (L+c)(U+c)}{(U-L)(y+c)^2}.$$
This follows from the Jacobian $dy/dx=ry$ and integration of $(y+c)^{-2}$. For $U=\infty$ replace $(U+c)/(U-L)$ by one. $L=0$ is permitted. Constants collapse. For nonconstant samples the threshold is the maximum exponential tilt on the window, including zero slope on bounded windows. Both signs are accessible on bounded windows and only the integrable sign on rays. The exponential-tail lemma covers every other end. Apply the score-and-boundary criterion with $(r,c)$, including equality. For $(a,b)$ on a bounded window the likelihood at any $c$ is $[r/(2\sinh(r(b-a)/2))]^2<(b-a)^{-2}$, an exact uniform escape. The general maximizing set is determined by the candidate comparison; a global singleton theorem on the attainment region is open.

### logis location truncate:known

For fixed location insert $c=e^{r(\mu-o)}$ in the logistic density and take the total rate derivative. The threshold is uniform on bounded windows and zero on rays. A constant at the projection diverges; all other scale-collapse likelihoods are zero. Consequently all other samples on rays attain. On bounded windows use the rate roots and uniform comparison. Global root count and uniqueness on attainment are open.

### logis scale truncate:known

For fixed $r$ use the positive roots of
$$\frac{1}{L+c}+\frac{1}{U+c}-\frac{2}{n}\sum_i\frac1{y_i+c}=0,$$
with $1/(U+c)=0$ when $U=\infty$. This is a polynomial after clearing positive denominators. If identically zero, all locations maximize. Otherwise compare its roots with the exponential likelihoods of slopes $\pm r$ on the bounded window or the integrable slope on a ray. No collapse is possible; a positive threshold without a finite winner is exponential escape. The root/value comparison is exact, including ties. The sample $(a,b)$ on a bounded window has every location maximizing, by the logistic formula above.

### logis location+scale truncate:known

Both held. Apply the fixed coefficient rule.

### cauchy all free truncate:known

Apply the power formulas and the two score equations. Any strict majority diverges. Otherwise compare with the uniform on bounded windows, the exterior inverse-square faces (either exterior side of a bounded window, and only the finite-cut side on a ray), and the critical-tie products. There is an exact bounded-window failure with $x=(a,b)$. To see it, put $\Delta=\arctan((b-\mu)/s)-\arctan((a-\mu)/s)$. The two densities have product $(\sin\Delta/\Delta)^2/(b-a)^2$, since the sine of the difference of angles is $s(b-a)$ divided by the product of the two quadratic radii. This is strictly below uniform. Both a diffuse-parent uniform path and an outward critical endpoint collapse path reach the same likelihood with different limiting laws. General finite cardinality is open.

### cauchy location truncate:known

Take the scale score of the power kernel. A strict majority at the held location diverges. Use uniform on bounded windows, a single inverse-square face if the location is exterior, and the held-location critical product if exactly half the sample equals the location in the window. On rays with the held location in the window and fewer than half its observations there, all ends have likelihood zero, so attainment follows. For a critical-tie failure take $[-1,1]$, location zero, and sample $(0,1)$. The normalization is $2\arctan(1/s)/s$ and the likelihood is $[4(1+s^2)\arctan^2(1/s)]^{-1}$. Its collapse value is $1/\pi^2$, and the uniform value is $1/4$. This sample instead gives uniform escape: $\arctan(1/s)>1/\sqrt{1+s^2}$ by $t>\sin t$ for $t=\arctan(1/s)$. Criticality can also attain. For the same holds and data on $[-1,\infty)$ the product is $[(1+s^2)(\pi/2+\arctan(1/s))^2]^{-1}$. Its right derivative at zero is $2/\pi^3>0$, using $\arctan(1/s)=\pi/2-\arctan s$. It beats the collapse product, the only positive boundary. General uniqueness is open.

### cauchy scale truncate:known

Use the location score and uniform on bounded windows. On rays the threshold is zero and every compatible sample fits. On bounded windows a finite maximum need not exist, as shown by $(a,b)$ above with scale held. Exact nonuniqueness is proved below for the held-scale power kernels.

### cauchy location+scale truncate:known

Both held. Apply the fixed coefficient rule.

### gumbel all free truncate:known

Use $r=1/\sigma$, $c=e^{r(\mu-o)}$, $y=e^{-r(x-o)}$, $L=e^{-r(b-o)}$, $U=e^{-r(a-o)}$, and $D=U-L$. The conditional density is
$$q(x)=r y\frac{c\,e^{-c(y-L)}}{1-e^{-cD}}.$$
It is the exponential with rate $c$ on $[L,U]$ in $y$, with Jacobian $ry$. At fixed $r$, $c\downarrow0$ gives the decreasing tilt of slope $-r$ if $a$ is finite, and zero density if $U=\infty$. At $c\to\infty$, a finite $b$ gives the atom at $b$, with divergent product for constants there and zero for all other samples. If $b=\infty$ and $r$ stays finite positive, the laws drift to the right and the product vanishes. This follows directly from the exponential law in $y$.

For $r\to\infty$ and nonconstant data put $S=\sum_i (y_i-L)>0$. Since $c/(1-e^{-cD})\le c+1/D$, maximizing the bound $(c+1/D)^n e^{-cS}$ gives at most $(n/S+1/D)^n$ (the maximizing $c$ is at most $n/S$). Both $S$ and $D$ are at least $\max y_i-\min y_i$, asymptotic to $\max y_i$ for distinct data, and $\prod_i y_i/(\max y_i)^n=e^{-r\sum_i(x_i-m)}$. Thus the product vanishes uniformly in $c$. Constants can collapse at any observation by taking $\mu$ there and $r\to\infty$. To identify weak limits of this end, the $u$-quantile of $Y$ is $L-c^{-1}\log(1-uK)$, $K=1-e^{-cD}$. For $0<\epsilon<1/2$, the ratio of the $(1-\epsilon)$-quantile to the $\epsilon$-quantile is at most $(1-\epsilon)/\epsilon^2$, using $t\le-\log(1-t)\le t/(1-t)$. The corresponding central interval in $x$ has length at most $r^{-1}\log((1-\epsilon)/\epsilon^2)$. Thus along subsequences the laws collapse at a finite limit of their medians or drift if the medians escape. The nonconstant product bound holds in both cases.

For $r\downarrow0$ and $b=\infty$, $q\le r$ because $cD\ge cy$ and $cy/(e^{cy}-1)\le1$. For finite $b$ put $v=crL$ and $t=b-x$. The density is
$$\frac{v e^{rt}\exp[-(v/r)(e^{rt}-1)]}{1-\exp[-(v/r)(e^{r(b-a)}-1)]}.$$
If $v\to v_0>0$ this is the increasing tilt of slope $v_0$, with full integral one on either window. If $v\to0$, the limit is uniform on a bounded window and diffuse on the left ray. If $v\to\infty$ it concentrates weakly at $b$, with zero likelihood if any $t_i>0$. These assertions follow by the displayed ratio and its integrated survival in $t$, which is a truncated exponential survival in $(e^{rt}-1)/r$. For finite positive $v_0$, pointwise density convergence to a probability density gives total variation convergence by integrating the minimum. The same argument applies to the other normalized, nonsingular faces in this report.

Consequently for nonconstant data the threshold is the maximized exponential likelihood, with unrestricted slope on a bounded window, negative slope on a right ray, and positive slope on a left ray. Each face is accessible, by the displayed paths. Score-and-boundary comparison gives the exact attainment condition. The $(a,b)$ example is uniform escape by the strict log concavity argument. For each rate there is at most one finite maximizing $c$ by strict concavity in $c$. Global uniqueness in rate is open.

### gumbel mu truncate:known

Insert $c=e^{r(\mu-o)}$ in the Gumbel density and use the total rate derivative. For $r\downarrow0$ the only positive face is uniform on bounded windows ($v\to0$). For $r\to\infty$ nonconstant samples vanish by the bound above. For constants this path diverges exactly at the projection of the held location. Indeed if $\mu\in A$, the standardized window retains a positive limiting probability and $q(\mu)$ is proportional to $r$. If $\mu<a$, $cy$ tends to zero on $A$ and $q(a)\sim r$, with a factor $e^{-r(x-a)}$ away from $a$. If $\mu>b$, the finite-$b$ formula has $v=r e^{r(\mu-b)}\to\infty$, giving divergence at $b$ and zero elsewhere. These estimates also give weak collapse at the projection. For $\mu\in A$, constants distinct from it have vanishing density by the parent kernel and the retained-mass bound. Other samples on rays attain; on bounded windows compare rate roots to uniform. General uniqueness is open.

### gumbel sigma truncate:known

For fixed $r$, the unique finite root in $c>0$ exists iff
$$L<\bar y<E_{c=0}Y,$$
where the upper threshold is $(L+U)/2$ for $U<\infty$ and infinity otherwise. The score is $n(E_cY-\bar y)$ and its derivative is $-n\operatorname{Var}_cY$. The thresholds follow by integrating the exponential in $y$. Equality at $L$ is possible only for constants at a finite $b$ and gives unbounded collapse there. At or above the finite upper threshold the excluded decreasing exponential of slope $-r$ wins, including equality. This applies to all three window types.

### gumbel mu+sigma truncate:known

Both held. Apply the fixed coefficient rule.

### hnorm all free truncate:known

For any feasible $\mu\ge a$, the kernel at each observation increases with $\mu$, and the integral in residual coordinates $\int_0^{b-\mu}e^{-u^2/(2\sigma^2)}du$ is nonincreasing. The numerator increases strictly for a strict move toward $m$, since all $x_i\ge m$. Thus this whole branch reduces to $\mu=m$. If $a=-\infty$ it is the entire problem. Otherwise a second branch is the truncated normal with $\mu\le a$. Within that branch use $(\eta_1,\eta_2)=(\mu/\sigma^2,-1/(2\sigma^2))$, with $\eta_2<0$, $\eta_1\le-2a\eta_2$. For nonconstant data its likelihood is strictly concave and coercive at infinity. Indeed each unit direction in the closed wedge gives a nonconstant concave quadratic or a nonconstant linear exponent with at most one maximizing point on $A$. Some positive-length interval therefore beats the empirical average, and a finite cover of directions gives a common gap. At the zero-quadratic boundary, maximize the slope $\eta_1\le0$ exponential. A divergent normalization gives zero, and an integrable boundary converges along a segment by convexity of the log integral and Fatou. The finite test is explicit in the first two moments. In the wedge interior match $\bar x$ and $\overline{x^2}$. On $\eta_1=-2a\eta_2$ match $\overline{(x-a)^2}$ and require $\bar x-E X\ge0$, since the inward direction decreases $\eta_1$. Each test suffices by the tangent inequality for a concave function. If neither holds, the maximized face supplies the branch supremum. Strict concavity gives at most one finite winner on this normal branch. Use the held-$\mu$ rule at $m$ for the other branch. The full fit exists iff a finite branch winner reaches or beats the other branch's supremum. Hence there are at most two fits, one if the branch winners coincide or only one beats the other branch, and two if distinct finite winners tie. Whether distinct ties occur is open. Constants collapse at $m$, by holding the endpoint there and shrinking scale when $m<b$, or by taking endpoint $b-\sigma$ for constants at a finite upper cut.

### hnorm mu truncate:known

On $I$ let $T=(x-\mu)^2$ and use the tilt $-T/(2\sigma^2)$. The conditional mean of $T$ increases strictly with that tilt by the positive variance. At the negative infinite end it tends to its value at the lower cut of $I$, by Gaussian tail concentration; at the zero end it tends to the uniform mean on bounded $I$ and infinity when $I$ is unbounded. The latter follows by rescaling $x-\mu$ by $\sigma$; the broad law then retains half the normal, with original second moment growing as $\sigma^2$. Thus exactly $\inf_I T<\bar T<E_0T$ yields a unique fit. Lower equality diverges with weak collapse at the lower cut. For $\mu$ below that cut, expanding $T$ there gives a decay rate $(\inf I-\mu)/\sigma^2$ to leading order, so the endpoint density diverges as well. At or above the bounded uniform threshold only the uniform wins. This applies even to a left window whose moving support makes $I$ bounded.

### hnorm sigma truncate:known

As in the free case, compare $\mu=m$ where $m\ge a$ and $m<b$ with the restricted normal branch when $a$ is finite. The normal branch is the scalar linear tilt $\eta_1\le a/\sigma^2$ against the carrier $e^{-x^2/(2\sigma^2)}$. Match the mean if it lies below the model mean at the upper parameter bound and strictly above $a$, otherwise take the upper parameter bound when the mean is at or above its model mean. At $\bar x=a$ (all observations at $a$) the negative tilt diverges and weakly collapses to $a$. These claims follow from the strictly positive tilted variance and the limit of the mean at negative infinite tilt. There is another divergence for a constant at finite $b$: $\mu\uparrow b$ at held scale shrinks the effective window, with $q(b)\sim1/(b-\mu)$. All other compatible samples attain. The exact cardinality test compares the two finite branch winners, counting coincident endpoints once. The occurrence of distinct ties is open.

### hnorm mu+sigma truncate:known

Both held. Apply the fixed coefficient rule.

### hcauchy all free truncate:known

Apply the power formulas with $\mu\le m$. On the branch $\mu\ge a$ likelihood strictly improves toward $m$ at each scale by the same residual-integral argument as for half-normal. On $\mu<a$ the full Cauchy kernel applies. The score comparison therefore uses the scale roots at $m$ when $a\le m<b$, and the two-dimensional roots for $\mu<a$ when that branch exists, including its boundary if feasible. Collapse can contribute at a sample level only at $m$ by feasibility. Strict majority there diverges; exactly half gives the critical product in the power lemma. The remaining positive faces are the best uniform $(b-m)^{-n}$ if $b$ is finite and the exterior powers with $c<a$ for finite $a$. For $x=(a,b)$ on the bounded window every feasible location retains $A$. The angular identity then proves nonattainment, with both the uniform and the outward critical collapse reaching the supremum. General cardinality is open.

### hcauchy mu truncate:known

Use the held-location power comparison on its effective window. Strict majority at $\mu$ diverges. Exactly half there contributes the half-kernel critical likelihood. A held $\mu<a$ contributes the single exterior inverse-square face. The uniform on bounded $I$ is the broad-scale face. With unbounded $I$, held location in $A$, and fewer than half tied there, both scale ends have zero likelihood and a fit exists. If $I=[\mu,\infty)$, the normalizer is $\pi/(2s)$. The log-scale score is $n-2\sum_i s^2/(s^2+(x_i-\mu)^2)$, strictly decreasing if some residual is nonzero. Thus in this subcase attainment is unique below half ties, and exactly half gives finite nonattainment by collapse. General finite cardinality is open.

### hcauchy sigma truncate:known

Use the finite candidates of the half-Cauchy branches above, now with scale fixed. When $a=-\infty$ and $m<b$, $\mu=m$ is the unique winner by strict improvement throughout the feasible location range. With finite $a$, compare $\mu=m$ if feasible and roots on $\mu<a$, with the uniform on $A$ as the only positive remote-location face when $b$ is finite. On a right ray every sample fits. On any window with finite $b$, a constant at $b$ diverges by moving the endpoint up to it, even at fixed scale, as for half-normal. The sample $(a,b)$ on a bounded window has uniform escape by the angular argument. Distinct finite ties outside the monotone branch are open.

### hcauchy mu+sigma truncate:known

Both held. Apply the fixed coefficient rule.

### std nu truncate:known

Apply the two power score equations with the specified held $\nu>2$. The collapse threshold is $k_c>\nu(n-k_c)$ for divergence and equality for the critical product. There is no equality for irrational $\nu$ since both counts are integers. Other positive faces are the bounded uniform and exterior powers of exponent $\nu+1$. The criterion and all finite comparisons use the window-specific faces of the power lemma. For an exact failure take $(a,b)$ on a bounded window. The angular formula implies that the integral average of $k_1=((x-\mu)^2+s^2)^{-1}$ strictly exceeds $\sqrt{k_1(a)k_1(b)}$. Jensen for $t^p$ then gives the same strict inequality for $k_1^p$, so no finite fit reaches the uniform supremum. General uniqueness is open.

### std mean+nu truncate:known

Apply the one-dimensional scale-score test using the same power faces, now with center fixed and the held-position critical product. On rays with the held center in $A$ and $k_c<\nu(n-k_c)$ a fit always exists. Outside that regime compare the applicable positive faces and check for divergence. Uniqueness on the attainment region is open.

### std nu+sd truncate:known

At fixed $\nu$ and positive sd, use the location score with $B$ equal to uniform on bounded windows and zero on rays. Thus all samples on rays fit. On a bounded window the following exact examples show nonuniqueness of this regime and held-scale Cauchy. Take $A=[-100,100]$, $x=(-4,4)$, and effective scale $s=1$. For Cauchy use $p=1$, and for Student use $\nu=3$ and $p=2$. By symmetry the likelihood is even. The centered integral exceeds the integral at location 4 since differentiating a translated symmetric window of a decreasing radial kernel shows its integral decreases for positive translation. The likelihood ratio at 4 relative to zero is therefore at least $[(1+16)^2/(1+64)]^p>1$. Also its likelihood at 4 exceeds $100^{-2}/4$, the uniform threshold, since it is at least $65^{-p}/C_p^2$ and $C_1=\pi<4$, $C_2=\pi/2<2$. Therefore there is an attained global maximizer away from zero and its reflection is also a maximizer. The exact number and general conditions for ties are open.

### std mean+nu+sd truncate:known

All held. Apply the fixed coefficient rule.

### std all free truncate:known

Free degrees of freedom are outside the held-df classification. A proved sufficient condition for divergence is a multiplicity $k>2(n-k)$: choose any fixed $\nu>2$ sufficiently near 2 and use the power-collapse proof. This works on each window including at a cut. The free-shape failure list and global uniqueness are open.

### std mean truncate:known

With held mean, the same sufficient divergence test applies to its ties in the window. The free-shape criterion and uniqueness are open.

### std sd truncate:known

With sd held and free location, as $\nu\downarrow2$ the effective scale $s=\mathrm{sd}\sqrt{\nu-2}$ still tends to zero. At $\mu=c\in A$, the same test $k_c>2(n-k_c)$ implies divergence. For a fixed center at $c$, the proof uses the rescaling with $d=0$. The integral converges to the full or half $p=3/2$ integral by domination and $s^{\nu-2}\to1$. The criterion and uniqueness with variable shape are open.

### std mean+sd truncate:known

With sd and mean held the preceding sufficient divergence test applies only at the held mean. There are also exact regular shape-face likelihoods. For fixed mean and sd, $\nu\to\infty$ gives the truncated normal. If sd may vary, $\nu\downarrow2$ with fixed effective scale $s>0$ gives truncated $t_2$ with power $p=3/2$. These follow from the kernel limits in the [Student dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md#details), pointwise convergence of the parent densities and convergence of their integrals via the minimum-density argument. The retained mass converges to a positive number and the products converge to $P$ for the stated kernels. These paths require the relevant holds, and do not settle their joint remote-location or varying-scale limits. Global attainment and uniqueness for this variable-shape mask are open.

### unif all free truncate:known

Let parent support be $[l,u]$. The effective interval starts at $L=\max(a,l)$ and ends at $U=\min(b,u)$. Its likelihood is $(U-L)^{-n}$ if it contains all data and $L<U$. For nonconstant data the unique optimal effective interval is $[m,M]$. All parent endpoints realizing it maximize: $l=m$ if $m>a$, otherwise $l\le a$; $u=M$ if $M<b$, otherwise $u\ge b$. Thus the parameter fit is unique iff neither extremum equals its window cut. Constants can be surrounded by positive retained intervals shrinking to that observation, with unbounded likelihood and weak atomic limit. The characterization works with either cut infinite by the same inequalities.

### unif max truncate:known

With $u$ held, require $m\le M\le U=\min(b,u)$ and $U>a$. The maximum uses $L=m$ if $m<U$; it is unique in $l$ iff $m>a$, and otherwise all $l\le a$ maximize. If $m=U$, all data equal $U$ and the likelihood diverges as $L\uparrow U$, with weak collapse there.

### unif min truncate:known

With $l$ held use $L=\max(a,l)$. Require $L\le m\le M$ and $L<b$. If $M>L$ the maximum uses $U=M$ and is unique in $u$ iff $M<b$. Otherwise all $u\ge b$ maximize when $M=b$ and $M>L$. If $M=L$ all data equal $L$ and the likelihood diverges as $U\downarrow L$, with weak collapse there.

### unif max+min truncate:known

Both held. Apply the fixed coefficient rule.

## Summary

The face lists below concern positive likelihoods on compatible data, with collapse envelopes accounted for separately. A restricted support can turn a left ray into a bounded effective interval.

| Family | Bounded window | Right ray | Left ray |
| --- | --- | --- | --- |
| Laplace | Uniform escape with both free or location held; finite exponential plateaus | Finite exponential plateau; free-location/free-scale nonconstant fits always attain | Reflected right-ray rule |
| Logistic | Uniform or signed exponential; fixed-rate rational score | Decreasing exponential when location escapes at finite rate | Increasing exponential when location escapes at finite rate |
| Cauchy | Uniform, exterior powers, tied collapse | Exterior powers at lower cut, tied collapse | Exterior powers at upper cut, tied collapse |
| Gumbel | Uniform or signed exponential, plus upper-end atom | Decreasing exponential from the $c=0$ face | Increasing exponential from $r=0$, plus upper-end atom |
| Half-normal | Restricted normal and endpoint branches; uniform or negative slope at broad boundary | Same branch comparison; uniform is unavailable | Endpoint branch only for free location; uniform on bounded effective interval |
| Half-Cauchy | Uniform, exterior powers, tied collapse, shrinking support | Exterior powers and tied collapse | Uniform on bounded effective interval, tied collapse, shrinking support |
| Student, held df | Cauchy comparison with exponent $\nu+1$ | Cauchy comparison at lower cut | Cauchy comparison at upper cut |
| Uniform | Shortest compatible effective interval | Same interval rule | Same interval rule |

## What this changes for the fitter

These routes are mathematical proposals. The direct certificates are scalar monotone mean brackets (held-location Laplace and half-normal, held-scale Gumbel), shortest-support comparisons for uniform, and finite location comparisons for held-scale Laplace and logistic. Other routes need an exhaustive score-root enclosure or a global compact likelihood cover and bounds excluding all competing faces. A strict comparison with the boundary establishes existence, but does not bound the coefficient error or certify cardinality. A tie of a finite candidate with an excluded face still permits attainment.

Evaluate conditional normalizers relative to a retained kernel value and use centered coordinates for exponential comparisons. Short windows and extreme tails require cancellation-safe integration, as in the [short-window study](2026-11-22-short-window-report.md) and the [truncated-normal limit payloads](2026-11-21-tnorm-limit-report.md). A reportable escape requires a global boundary comparison; critical collapse diagnostics must carry the joint likelihood rather than the likelihood of an atomic replacement. Where a future route has a successful search but its certificate cannot settle the comparison, preserve the search result with its uncertified flag as specified in the job.

## Numerical checks

**Numerical.** The workstation run passed 40 mpmath comparisons at 80 decimal digits, at path steps $10^{-3},10^{-6},10^{-12},10^{-20}$. The grid checks the critical Cauchy and $\nu=3$ Student products at an exterior endpoint and held interior center, the held half-Cauchy endpoint, Gumbel's increasing exponential on a bounded window and a left ray, and the logistic endpoint identity at rates $1/4,1,4$. The script applies scale-dependent finite-path tolerances and stores the raw products and errors. The repository atlas validator also accepted all 91 written rows. These checks assess the displayed paths; they do not count finite roots or establish an equality of numerical maxima. [Script](2026-11-27-trunc-ls-math-data/check.py) · [Results](2026-11-27-trunc-ls-math-data/checks.csv) · [Atlas proposals](2026-11-27-trunc-ls-math-data/proofs.csv)

## Open

- For the score-and-boundary regimes, reduce the root tests to direct data inequalities where possible. In particular, at critical power ties the untruncated likelihood criterion need not carry over: conditioning can reward low retained mass, and outward cut collapse changes the boundary constant. A usable certificate needs root isolation with boundary remainder bounds, especially at equality.
- Determine finite cardinality for the implicit regimes apart from the proved examples. Half-normal reduces this to the possibility of distinct equal-valued branch optima. Logistic with held location and Gumbel with scale free reduce it to scalar rate profiles whose global root counts remain open.
- For Student with free df, derive uniform bounds as location, effective scale and df move together, including at $\nu=2$ and infinity. The shape-face paths and strict tie test above do not give this joint classification, in particular at $k=2(n-k)$.
