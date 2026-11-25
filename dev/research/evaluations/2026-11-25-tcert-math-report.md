# Student t stationary points and per-input joint certificates

TASK-252, base 19b2fb7b. The 32,400,002-observation sample specified below has two **proved interior shape modes** in both the location-scale and mean/standard-deviation parameterizations. The certificate uses a compact cover in inverse df, with interval nuisance solves where the conditional maximizer is unique and full location/shape boxes where it may have several branches. The Python cover closed all six Student golden routes, both domains of the two-mode sample, and both domains of the earlier negative-kurtosis-slope example. This is a mathematics and numerical study. All changes are confined to this report and its evidence directory.

The current all-free `std` and `lst` entry points already call `t_certificate`. It uses geodesic conditional optimization, shape-profile value bounds, a signed normal tail and a finite-root basin, followed by a separate accuracy refinement. The finite uncertified routes motivating this job are the joint **held-location** and **held-scale or held-sd** fits. The earlier studies supply the fixed-df theorem, the shape-only proofs and existing normal-face machinery. This report gives an extension to those joint holds, a single box calculus that remains regular at the normal face, and a quantitative check on critical collapse collars. [Current all-free certificate](../../../univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp), [TASK-140](2026-11-04-t-cert-report.md), [held-df and held-subset study](2026-11-07-std-held-report.md), [free-shape held subsets](2026-11-08-std-free-report.md).

## 1. Stationary points

Write $m$ for location, $s$ for ordinary t scale, $\nu$ for df, $a=s\sqrt{\nu}$, and $r_i=x_i-m$. With $\nu$ held, `std` has $a=\mathrm{sd}\sqrt{\nu-2}$. With shape and scale both held, the location score has the sign of $\sum_i r_i/(a^2+r_i^2)$. This is the distinction behind Copas's Cauchy result on unimodality with both nuisances free and Gabrielsen's Student-t extension. [Copas (1975), *Biometrika* 62(3), 701–704](https://doi.org/10.1093/biomet/62.3.701), [Gabrielsen (1982), *Biometrika* 69(3), 677–678](https://doi.org/10.1093/biomet/69.3.677).

**Proved, fixed scale and shape.** For $d$ distinct observations $y_j$ of counts $k_j$, the score numerator is
$$
F(m,z)=\sum_{j=1}^d k_j(y_j-m)\prod_{i\ne j}[(y_i-m)^2+z],
\qquad z=a^2>0 .
$$
The denominator is positive and the degree in $m$ is exactly $2d-1$, with leading coefficient $-n$. There are at most $2d-1$ real roots counting multiplicities. All are in the sample range by the sign of each residual outside that range. For nonconstant data with $a\ge R=x_{\max}-x_{\min}$, every interior location has $|r_i|<a$ and hence strictly negative location curvature. The score is strictly inward at the two endpoints, so there is exactly one root. The bound can be attained for any fixed distinct support by taking $a$ sufficiently small. At $y_j-a$ and $y_j+a$ the own-atom term has opposite signs and magnitude $k_j/(2a)$, and all other terms are bounded by the fixed gaps. There is a root within each atom neighbourhood and one in each intervening gap. As a two-point illustration, the previously proved $(-b,b)$ example has two equal maxima at $\pm\sqrt{b^2-a^2}$ for $a<b$, with a minimum at zero. [Held-subset derivation](2026-11-07-std-held-data/derivation.md).

**Proved, fixed shape with both nuisances free.** With $k_{\max}<\nu(n-k_{\max})$, the known hyperbolic Busemann argument gives a unique stationary point and maximizer when $\nu\ge1$. At $\nu=1$ the strict mass condition implies at least three support points. The balanced two-point Cauchy boundary has a continuum of stationary maximizers with $s^2=(m-x_{\min})(x_{\max}-m)$. The fixed-df proof and classification of critical atoms are in the earlier derivations. [Geodesic proof](2026-11-04-t-cert-data/derivation.md), [critical atoms and balanced Cauchy](2026-11-07-std-held-data/derivation.md).

For any held positive $\nu$, the number of **isolated** joint stationary points is at most $2d(2d-1)$. To prove this, clear denominators in the log-scale score:
$$
G(m,z)=(\nu+1)\sum_j k_j(y_j-m)^2\prod_{i\ne j}[(y_i-m)^2+z]
       -n\prod_i[(y_i-m)^2+z].
$$
In the variables $(m,z)$ the total degrees of $F,G$ are at most $2d-1,2d$. At a positive-$z$ root of $G$, the scale score is strictly decreasing in $z$ whenever any residual is nonzero. Thus $G_z\ne0$ there and $G=0$ is an analytic graph. A common complex component through such a real point would make $F$ vanish identically on that graph locally and give a real continuum through the point. An isolated real root therefore survives removal of the common polynomial factors, after which the plane Bézout bound applies. Under the strict atom inequality **all** stationary points are isolated, even for $\nu<1$. The positive conditional scale root then exists for every real $m$, since the scale score decreases strictly from $\nu(n-k(m))-k(m)>0$ to $-n$. Its graph is analytic on the entire real location line. The profile location score has strict sign outside the data range, so the identity theorem rules out vanishing on an interval.

The existing exact symmetry argument for $(-100,-99,0,99,100)$ at $\nu=1/2$ proves distinct global maxima. The exact polynomial elimination here gives a degree-25 resultant and 21 real eliminant locations, including denominator artefacts. Recovering positive scales by 80-digit bisection gives five stationary points **numerically**. The locations $\pm99.47738607$ and $0$ are local maxima, and $\pm62.93873196$ are saddles. The strict mass condition holds. [Earlier exact comparison](2026-11-08-std-free-report.md#proved-nonuniqueness), [elimination script](2026-11-25-tcert-math-data/fixed_df.py), [results](2026-11-25-tcert-math-data/fixed_df.json).

### An exact sample with two interior shape maxima

Use both signs of each displayed radius, with the indicated multiplicity **at each sign**.

| Radius | Multiplicity |
|---:|---:|
| $4/100$ | 300,000 |
| $80/100$ | 4,000,000 |
| $5/100$ | 4,000,000 |
| $15/100$ | 4,800,000 |
| $33/100$ | 3,100,000 |
| $300$ | 1 |

**Proved.** For every $\nu\ge1$, the strict atom inequality and the fixed-shape theorem give a unique nuisance fit. Reflection forces $m=0$. The conditional scale root is strictly monotone in its score and smooth in $\nu$. With $P(\nu)$ the profiled total log likelihood and $u_i=r_i^2/(\nu s^2)$ at that root, the envelope derivative is
$$
\frac{2}{n}P'(\nu)=
 \psi((\nu+1)/2)-\psi(\nu/2)-1/\nu+
 \frac1n\sum_i\left[-\log(1+u_i)+(1+1/\nu)\frac{u_i}{1+u_i}\right].
$$
At integer df the digamma difference satisfies $D_1=2\log 2$ and $D_{j+1}=2/j-D_j$. The checker bisects the squared-scale equation with 75 exact rational comparisons. Since $\log(1+u)$ and $u/(1+u)$ increase in $u$, the resulting rational residual bounds enclose the expression term by term. For each logarithm, reducing by powers of two gives an argument $x\in[1,2]$. With $b=(x-1)/(x+1)$, the first 18 terms of $2\sum_{j\ge0} b^{2j+1}/(2j+1)$ have nonnegative remainder at most $2b^{37}/(37(1-b^2))$. The following signs are strict rational comparisons. Displayed values summarize the intervals.

| df | $2P'(\nu)/n$, approximately | Sign |
|---:|---:|:---:|
| 4 | $4.02705\,10^{-4}$ | $+$ |
| 6 | $-2.27682\,10^{-4}$ | $-$ |
| 20 | $-9.02338\,10^{-6}$ | $-$ |
| 50 | $2.69341\,10^{-7}$ | $+$ |
| 100 | $-6.18175\,10^{-8}$ | $-$ |

The maximum on each of $[4,6]$ and $[50,100]$ is interior by the derivative signs at its two endpoints, and is strict by analyticity. Both intervals apply to both families because unrestricted positive ordinary scale and unrestricted positive sd are equivalent at each $\nu>2$. Numerically, safeguarded 80-digit profiling locates maxima near df $4.491610680369$ and $89.433644232070$, with a minimum near $38.545619803808$. The certificate comparison in Section 4 selects the first maximum in both domains. An earlier counterexample had a locally preferred normal face beaten by a finite witness. Here the modes are both finite and strictly inside the shape domains. [Rational checker](2026-11-25-tcert-math-data/exact_modes.py), [rational intervals](2026-11-25-tcert-math-data/exact_modes.json), [80-digit replay](2026-11-25-tcert-math-data/check.txt), [earlier exact normal-seam example](2026-11-04-t-cert-report.md#the-normal-seam-and-counterexamples).

## 2. Compactness and face comparisons

All bounds below use total log likelihood unless divided explicitly by $n$. At a finite feasible witness record an enclosed lower value $L$. An excluded face is a candidate supremum and can also supply an enclosed lower bound on the supremum. Any strict cutoff uses the lower endpoint of such an enclosure. Let $q_0=1$ for all-free `lst` and $q_0=2$ for all-free `std`. The former endpoint is attained when its nuisance optimum exists. At $\nu=2$, positive ordinary scale is an excluded `lst` face in `std`.

**Proved, location and upper scale.** With location free, projection into $[x_{\min},x_{\max}]$ increases the likelihood, since it decreases every residual magnitude. At either extreme, for nonconstant data and positive scale the strictly inward location score excludes a maximum. A symmetric decreasing probability density satisfies $f(r)\le 1/(2|r|)$ away from zero by integration over $[-|r|,|r|]$. This also gives a value cutoff outside a distance $D>\exp(-L/n)/2$ of the range, uniform over shape and scale. Writing $A(\nu)=\log t_\nu(0)$ for the ordinary unit-scale peak, $A(\nu)\le-\frac12\log(2\pi)$. The peak bound follows from the Gaussian scale mixture $t_\nu(0)=\phi(0)E\sqrt{V/\nu}$ for $V\sim\chi^2_\nu$, by Jensen. Thus any $S>\exp(-L/n)/\sqrt{2\pi}$ excludes $s\ge S$ uniformly.

**Proved, positive lower scale under strict mass.** Let
$$
d_j=\tfrac12\min_i(x_{(i+j)}-x_{(i)}),\quad j=1,\ldots,n-1.
$$
At least $p=n-j$ residual magnitudes are at least $d_j$. The scale score identity at any conditional stationary scale is
$$
\frac{n}{\nu+1}=\sum_i\frac{r_i^2}{\nu s^2+r_i^2}.
$$
Keeping those $p$ terms gives $s^2\ge d_j^2[p-j/\nu]/n$. Jensen's inequality for the concave function $w/(\nu s^2+w)$ gives $s^2\le\overline{r^2}$. Choosing $j=k_{\max}$ gives a positive uniform lower bound for the nuisance optimizers when $k_{\max}<q_0(n-k_{\max})$. Maximizing over more $j$ improves the same bound. If a value cutoff is needed rather than a stationary-region cutoff, on $[q_0,N]$, with $d=d_j>0$,
$$
\ell\le n\log\phi(0)-n\log s
       -\tfrac12(q_0+1)p\log(1+d^2/(Ns^2)).
$$
This follows by retaining only the $p$ residual penalties and using $\nu\le N$. Its limit is $-\infty$ when $(q_0+1)p>n$, and replacing $\log(1+w)$ by $\log w$ gives an explicit cutoff linear in $\log s$. For $\nu\ge N$, $v\log(1+w/v)$ increases with $v$, since its derivative is $\log(1+w/v)-(w/v)/(1+w/v)\ge0$. Replace $q_0+1$ in that bound by $N$ for the infinite tail. Choosing $Np>n$ makes the small-scale exclusion uniform there too. With location held, the actual positive residuals can replace the sample-range counts.

**Proved, unrestricted low df with location held.** In `lst` holding location removes the fitting floor. Put
$$
B(\nu)=\frac{\Gamma((\nu+1)/2)}{\sqrt{\pi}\Gamma(\nu/2)}
       =\sqrt{\nu}\,\exp A(\nu).
$$
At residual $r\ne0$, the density is at most $B(\nu)/|r|$: in $a=s\sqrt{\nu}$ coordinates its kernel is $B(\nu)(a^2+r^2)^{-1/2}(1+r^2/a^2)^{-\nu/2}$. Recurrence writes $B(\nu)/\nu=\Gamma((\nu+1)/2)/(2\sqrt{\pi}\Gamma(\nu/2+1))$, which decreases by monotonicity of digamma and has limit $1/2$ at zero. With no exact residual zeros the bound $n\log(\nu/2)-\sum_i\log|r_i|$ gives a uniform low-df cutoff over all scales. With scale free and at least one exact tie, a small fixed $\nu$ makes the collapse exponent negative and proves unboundedness. With both nuisances held, the scalar certificate already handles the zero-df tail.

**Proved, ordinary-scale critical atoms.** For an atom $b$ with $k$ ties the log-scale coefficient is $\nu(n-k)-k$. With both nuisances free, an atom exceeding the critical mass at $q_0$ proves unboundedness by choosing an admissible shape with a negative exponent. At equality the sharp collapse value is
$$
C_b=n\log B(q_0)-(q_0+1)\sum_{x_i\ne b}\log|x_i-b|.
$$
The fixed-$q_0$ geodesic argument identifies this as the sharp conditional supremum when both nuisances are free, including the balanced Cauchy exception. For a held location equal to the atom, the monotone scale score yields the same supremum. The earlier proof gives the exact mixed-path limit, including the loss from nonzero displacements on the scale of $a$. [Collapse proof](2026-11-04-t-cert-data/derivation.md#collapse-and-the-lower-face), [conditional critical case](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one).

A new explicit punctured collar follows without resolving a collapsing nuisance root. Set $u=m-b$, $p=n-k$, and take $|u|\le\delta<\min_{x_i\ne b}|x_i-b|$, $a\le1$. Let $M$ bound $\left|\log((x_i-b-u)^2+a^2)\right|$ over this region, e.g. by the endpoints of each positive distance bound. In $a$ coordinates the log likelihood equals
$$
n\log B(\nu)+[\nu p-k]\log a-\frac{\nu+1}{2}
 \left[k\log(1+u^2/a^2)+\sum_{x_i\ne b}\log((x_i-b-u)^2+a^2)\right].
$$
The derivative of $\log B(\nu)$ is between $0$ and $1/\nu$ by digamma monotonicity and recurrence. Subtract the expression at the same $(m,a)$ and shape $q_0$, and discard the nonpositive atom term. This gives
$$
\ell_\nu-\ell_{q_0}\le (\nu-q_0)[K+p\log a],
\qquad K=n/q_0+pM/2 .
$$
With $a\le \exp(-1-K/p)$ it is strictly negative for every $\nu>q_0$. The same construction works in each critical neighbourhood. Elsewhere partition the location range into atom neighbourhoods and their complement and apply the strict-mass small-scale bound using the positive remaining distances. If the collapse face is the incumbent, the rest of a bounded-shape collar is now a compact positive-scale comparison with that face. At $q_0$ it has a strict value gap by the fixed-shape nonattainment proof, except for the balanced Cauchy continuum. That case can be removed analytically for the free-shape fit. For all $\nu>1$, symmetry and the scale score give $m$ at the midpoint and $s$ at the half-gap. Along these coefficients the derivative per observation is $A'(\nu)+(1/\nu-\log(1+1/\nu))/2>0$. The centered Cauchy point reaches the whole continuum's value, so normal strictly wins for the balanced two-point free-shape fit.

**Proved, moving location with sd held in `std`.** Let $d$ denote the held sd, $w=\nu-2>0$, and $a=d\sqrt w$. On a location interval $J$ let $\delta_i=\operatorname{dist}(x_i,J)$. For $0<w\le W\le1$ take any subset $I$ with $\delta_i>d\sqrt W$ and $3|I|>n$. The uniform bound is
$$
U_J(W)=n[\log B(2+W)-\log d]
      +\tfrac12(3|I|-n)\log W-3\sum_{i\in I}\log(\delta_i/d).
$$
Indeed the peak has log factor $-n\log w/2$. For $i\in I$, the log kernel is at most $-\tfrac32\log(\delta_i^2/(wd^2))$, since the ratio exceeds one and $3+w\ge3$. The remaining terms are nonpositive. Monotonicity of $B$ and the positive net log coefficient give $U_J$. If all masses are strict, a fixed finite partition can keep more than $n/3$ residuals a positive distance from each cell. Thus small enough $W$ closes the whole collar. Above critical mass, $m=b$ already proves divergence, with exponent $(2n-3k)/2$ in $w$.

At $k=2n/3$ put $p=n-k$, $S_b=\sum_{x_i\ne b}\log(|x_i-b|/d)$ and $C_b^\mathrm{sd}=-n\log(2d)-3S_b$. Take a radius $\delta<\min_{x_i\ne b}|x_i-b|$ so small that $T\delta\le(3k/2)\log2$, with $T=4\sum_{x_i\ne b}1/(|x_i-b|-\delta)$. Pull out each nonzero residual's logarithm from its kernel and drop the remaining nonpositive correction. Varying $m$ from $b$ changes the nonatom sum by at most $T|u|$, by the mean-value theorem and $3+w\le4$. The atom penalty is at most $-(3k/2)\log(1+u^2/(wd^2))$. For $|u|\ge d\sqrt w$ it absorbs $T|u|$ throughout the neighbourhood. For smaller $|u|$, $\log(1+z)\ge z/2$ on $[0,1]$, and completing the square bounds the net gain by $T^2d^2 w/(3k)$. Since $(\log B)'(2+w)\le1/2$, it follows that
$$
\ell\le C_b^\mathrm{sd}+
 w\left[\frac n2+|S_b|+\frac{T^2d^2}{3k}+\frac p2\log w\right].
$$
The bracket is negative for explicitly small $w$, and $m=b,\ w\downarrow0$ reaches the face value in the limit. This excludes moving-location improvement in the critical collar. Holding sd and attaining a positive ordinary-scale $t_2$ face are incompatible because $a=d\sqrt w\to0$. Thus the eligible excluded lower face here is collapse, with a different comparison from the free-scale route.

### Normal face

**Proved.** Use $h=1/\nu$ so the normal is at $h=0$, with the Gaussian fit respecting the holds. Under the strict scale bounds the log density extends continuously to this endpoint uniformly over the nuisance rectangle. The formulas in Section 3 extend its first two derivatives as well. The bound
$$
  \ell(m,s,h)-\ell(m,s,0)
   \le h\sum_i\frac{z_i^4-2z_i^2-1}{4}
          +\frac{h^2}{4}\sum_i z_i^4+\frac{n h^3}{24},
   \qquad z_i=(x_i-m)/s
$$
follows from $\log(1+v)\ge v-v^2/2$ and the normalizer bound proved below. On a nuisance rectangle with ordinary scale free or held, its right-hand side gives a uniform cutoff when $L$ beats the fitted normal. The held-sd path uses its own Gaussian scale adjustment. The signed alternative uses the enclosed profile derivative on $[0,H]$. When the conditional nuisance optimum is unique and the tube includes it, $P'(h)=\ell_h$ there. At the Gaussian maximum with scale free its value at zero is $n(\overline{z^4}-3)/4$, using $\sum_i z_i^2=n$. With ordinary scale fixed, use the first coefficient displayed above. With sd fixed, the chain term changes it to $\sum_i(z_i^4-6z_i^2+3)/4$. If the shape derivative is strictly negative in an inward collar, the normal bounds the entire collar from above. A positive derivative supplies a direction beating the normal. If the derivative at zero is exactly zero, a signed profile-curvature enclosure can instead settle the local comparison. Neither sign decides what happens away from this collar. With location free and scale held, a sufficiently small $h$-collar also satisfies $a\ge R$, so the unique location branch can be treated by the same argument. [Prior signed-face bounds](2026-11-08-std-free-data/signed-derivation.md), [joint normal tail](2026-11-04-t-cert-data/local-tail.md).

## 3. Box likelihood, scores and curvature

**Proved.** Work in $(m,\eta,h)$ with $\eta=\log s$. Put $q_i=r_i^2e^{-2\eta}$, $u_i=h q_i$, $D_i=1+u_i$ and $W_i=q_i/D_i$. Write $C(h)=A(1/h)+\tfrac12\log(2\pi)$. Define the moments $J_j^{(p)}(u)=\int_0^1 t^j(1+ut)^{-p}\,dt$. The log density is
$$
-\tfrac12\log(2\pi)+C(h)-\eta
 -\tfrac{1+h}{2}\,q_i J_0^{(1)}(u_i).
$$
Thus the likelihood upper bound on a box with $h\in[l,b]$ uses $C(l)$, the lower $\eta$ endpoint, and the lower kernel $(1+l)q_i^- J_0^{(1)}(bq_i^-)/2$. The monotonicities follow directly from the integral: $qJ_0^{(1)}(hq)$ increases in $q$ and decreases in $h$. The formulas at $h=0$ have their continuous values.

The table gives the contributions of one observation to the gradient and Hessian, with $r,q,D,W,u$ standing for its quantities. Constants $C'$ and $C''$ occur once per observation.

| Entry | Contribution |
|---|---|
| $g_m$ | $(1+h)e^{-2\eta}r/D$ |
| $g_\eta$ | $-1+(1+h)W$ |
| $g_h$ | $C'-[W-q^2J_1^{(2)}(u)]/2$ |
| $H_{mm}$ | $-(1+h)e^{-2\eta}(1-u)/D^2$ |
| $H_{m\eta}$ | $-2(1+h)e^{-2\eta}r/D^2$ |
| $H_{\eta\eta}$ | $-2(1+h)q/D^2$ |
| $H_{mh}$ | $e^{-2\eta}r(1-q)/D^2$ |
| $H_{\eta h}$ | $q(1-q)/D^2$ |
| $H_{hh}$ | $C''+W^2/2-q^3J_2^{(3)}(u)$ |

Differentiate the integral or the equivalent log kernel. The identities $J_0^{(2)}(u)=1/(1+u)$, $J_1^{(3)}(u)=1/[2(1+u)^2]$, and differentiation under the integral give the two shape entries. To bound terms, square the residual interval including zero if crossed, then use interval arithmetic. For $W$ and $q^pJ_j^{(p)}(hq)$, use the exact monotonicities rather than multiplying independent intervals. The integrands of the latter are powers of $q/(1+hqt)$, which increase in $q$ and decrease in $h$. Thus $[q^-,q^+]\times[l,b]$ gives the lower endpoint at $(q^-,b)$ and the upper at $(q^+,l)$. The formulas
$$
 J_1^{(2)}(u)=\frac{\log(1+u)-u/(1+u)}{u^2},\qquad
 J_2^{(3)}(u)=
 \frac{\log(1+u)-u/(1+u)-u^2/[2(1+u)^2]}{u^3}
$$
follow by substitution $v=1+ut$. Near zero, use $\sum_{k\ge0}(-u)^k\binom{p+k-1}{k}/(j+k+1)$. For $u\le1/8$ the terms decrease for $p\le3$, giving the first omitted term as a rigorous bound. The implementation can use this bound and a magnitude allowance for the elementary operations.

The normalizer integral in TASK-157 gives, after scaling its variable,
$$
 C(h)=-\tfrac12\int_0^\infty e^{-t}\tanh(ht/2)\,\frac{dt}{t}.
$$
Differentiation under the integral is valid at and above zero by the integrable bounds on each derivative. With $T=\tanh(ht/2)$, one obtains $C'=-\tfrac14\int e^{-t}(1-T^2)\,dt$, so $-1/4\le C'\le-1/4+h^2/8$ by $0\le T\le ht/2$. Then $C''=\tfrac14\int t e^{-t}T(1-T^2)\,dt$ gives $0\le C''\le h/4$. Its derivative has integrand $t^2 e^{-t}(1-T^2)(1-3T^2)/8$, bounded in absolute value by $t^2 e^{-t}/8$. Thus $|C'''|\le1/4$. On any $h$-interval, $C'$ is enclosed by its endpoint values and $C''$ by a midpoint value plus or minus one eighth the interval width, intersected with $[0,b/4]$. Integrating the bound on $C'$ gives the normal-relative likelihood bound in Section 2. For held sd, set $\eta(h)=\log d+\frac12\log(1-2h)$, and use $g_h+\eta'g_\eta$, $H_{mh}+\eta'H_{m\eta}$, and $H_{hh}+2\eta'H_{\eta h}+(\eta')^2H_{\eta\eta}+\eta''g_\eta$. Here $\eta'=-1/(1-2h)$, $\eta''=-2/(1-2h)^2$. These derivatives apply on the finite box up to the checked collapse cutoff. [Normalizer and moment derivation](2026-11-08-sstd-moment-report.md#evaluation-and-error-bounds).

### Cover and inclusion

**Proved, conditional on the inclusion and sign comparisons.** The t normal/chi-squared mixture gives conditional Gaussian weights $(1+h)/D_i$. Weighted mean and scale updates, followed by Newton steps, supply proposals. Root inclusion follows from the Hessian, independently of how a proposal was found.

1. With ordinary scale free, use the unique fixed-shape nuisance optimum for $\nu\ge1$ when both nuisances are free, or the strictly monotone log-scale score when location is held. For each $h$-interval, enclose this root in a nuisance box centered at $p$ with component radii $\rho$. For a fixed numerical inverse $E$, require
   $$
   |E g_y(p,h)|+\sup|I-E H_{yy}|\,\rho<\rho
   $$
   componentwise and uniformly in $h$, with $y$ the free nuisances. Enclose $g_y(p,h)$ by the midpoint score plus the midpoint-to-endpoint displacement times the mixed Hessian column. The inequality maps the closed box strictly into itself under $y\mapsto y-Eg_y$ and gives a contraction in the radius-weighted maximum norm. It proves existence and uniqueness in the box. The conditional uniqueness theorem identifies the root as the global nuisance optimum. With exactly symmetric data the location coordinate can instead be eliminated by reflection and that same theorem.
2. For a tube enclosing $y(h)$, the implicit function theorem gives
   $$
      P'=\ell_h,\qquad P''=H_{hh}-H_{hy}H_{yy}^{-1}H_{yh}.
   $$
   An interval with negative nuisance curvature and a separated determinant gives an explicit interval inverse in dimensions one and two. Enclose $P'$ directly or from its enclosed midpoint and $P''$. A strict sign gives an endpoint value bound, a curvature sign resolves an isolated scalar maximum, and integration gives $P(h)\le P(h_c)^+ +\sup |P'|\,|h-h_c|$. The upper value at a proposed conditional root follows by integrating the enclosed nuisance score from the proposal to its root box. As a cheap additional comparison, reuse the TASK-140 chord bound in $\nu$. At fixed $(m,a)$, $\ell-n\log B(\nu)$ is affine in $\nu$; its supremum is convex. Since $0<-(\log B)''<1/\nu^2$, on $[c,d]$ the chord bound is $\max(P(c)^+,P(d)^+)+n(d-c)^2/(8c^2)$. For the normalizer inequality the trigamma difference integral for $-(\log B)''$ has integrand $t e^{-\nu t/2}/[4(1+e^{-t/2})]$, which integrates below $1/\nu^2$. The interpolation remainder is at most the maximum second derivative times $(d-c)^2/8$. This comparison also applies with location held and ordinary scale free, including an endpoint defined as a finite collapse supremum. A fixed ordinary scale or fixed sd makes $(m,a)$ depend on shape and requires the box comparison instead. [TASK-140 profile envelope](2026-11-04-t-cert-data/derivation.md).
3. With scale or sd held, cover all location/shape boxes, including the two shape ends as separate location-only problems. For general fixed subunit shape with both nuisances free, cover location/log-scale boxes instead. Discard a box if its likelihood upper bound is below $L$, if an active score component excludes zero for an interior candidate, or if a quadratic Taylor upper bound does so. The score Taylor enclosure is $g(p)+H(B)(B-p)$, and the value enclosure is $\ell(p)+g(p)^\top\Delta+\Delta^\top H(B)\Delta/2$, where each $\Delta_i\in B_i-p_i$ and repeated-square intervals include zero when needed. These follow by integration along the segment, with the Hessian integral an entrywise convex average. Shape endpoints require only the tangential score and the eligible one-sided comparison. Subdivision keeps all unresolved branches.
4. For a finite winner, verify a joint contraction enclosure by the same test in all free coordinates. Use either $P''<0$ throughout a tube about the enclosed shape or a negative-definite joint Hessian throughout a convex basin to give at most one root there. Exclude everything outside that basin by the previous comparisons, and compare the faces. Competing basins require separate inclusions and strict dominance or proved equality. Transport the entire root enclosure to public coefficients, using $s=e^\eta$, $\nu=1/h$, and $\mathrm{sd}=s/\sqrt{1-2h}$ where appropriate. Compare the returned point's worst-case component error against $\mathrm{tol}$ in public units, including the scale floor for location and rounding on conversion.

The exact-arithmetic implication uses the same package convention as the earlier certificates. Production decisions are conditional on the computed comparisons with practical floating-point margins. The research driver uses 64 binary64 epsilons on interval operands and reduction magnitudes, extra relative allowances up to $8\,10^{-13}$ for log-moment cancellation, and $10^{-11}$ to $4\,10^{-11}$ average-log-likelihood comparison allowances. The Python preprocessing uses ordinary binary64 recentering. The seven raw-golden replays use the original binary64 observations and exact high-precision recentering and verify the reported root inclusions independently. A C++ implementation should enclose normalization and transport, and use the existing stable gamma-ratio seam and cancellation-aware summation. The study uses mpmath for point normalizer values.

## 4. Numerical evidence and box counts

**Numerical.** The following are visited shape cells or location/shape boxes. For profiled routes the last column gives conditional point solves, and for held sd it gives the separately visited collapse-collar cells, including unsuccessful trial collars. Profile-tube counts and exclusion reasons are retained in the output. All listed covers have zero unresolved boxes. Holds on the 500-point golden samples use the corresponding stored coefficient. [Python cover](2026-11-25-tcert-math-data/boxes.py), [full counts and widths](2026-11-25-tcert-math-data/boxes.json).

| Sample and route | Winner | Main cells | Point solves or collar cells |
|---|---|---:|---:|
| `lst` golden, all free | finite, df 3.0242427094 | 81 | 65 points |
| `lst` golden, location held | finite | 45 | 48 points |
| `lst` golden, scale held | finite | 295 | — |
| `std` golden, all free | finite, df 3.9776663070 | 83 | 66 points |
| `std` golden, mean held | finite | 41 | 50 points |
| `std` golden, sd held | finite | 323 | 61 collar cells |
| Two interior modes, `lst` / `std` | lower-df finite branch | 97 / 91 | 127 / 125 points |
| Earlier negative-slope example, `lst` / `std` | attained Cauchy / positive-scale $t_2$ face | 37 / 43 | 39 / 42 points |
| `ht` golden, endpoint held | finite, df 2.4095833450 | 43 | 47 points |
| Archived bimodal `ht`, endpoint held | finite, df 0.41507917 | 45 | 45 points |

The two symmetric adversarial inputs use the proved symmetry reduction. The generic two-coordinate nuisance tube did not close the winning basin for the rare-outlier sample in a preliminary run. The exact symmetry route closed it with public shape and scale enclosure errors below $10^{-10}$. For the six golden Student routes the widest full enclosure in $h$ was $8.40\,10^{-13}$, and the widest full enclosure in log ordinary scale was $9.10\,10^{-13}$. Transforming the endpoints preserves the requested $10^{-10}$ error bound on the reported coefficients in these workloads. On the held-sd golden the collapse collar excluded $0<\nu-2\le1/16$. The independent replay used 80 digits, checked 1,344 likelihood, gradient and curvature inclusions over two widths at each of three inverse-shape locations in each workload, and checked 150 critical-collar comparisons. A separate 80-digit run checked 570 exterior and normal-bound probes, including 81 locations and shapes in the successful held-sd collar. The probes and counts measure these inputs. A flatter maximum, closer likelihood competitor, thinner clusters or poorly scaled nuisance directions can require much finer subdivision. [Independent replay](2026-11-25-tcert-math-data/check.py), [replay output](2026-11-25-tcert-math-data/check.txt), [exterior replay](2026-11-25-tcert-math-data/tail_grid.py), [exterior counts](2026-11-25-tcert-math-data/tail_grid.json).

## 5. Variants

### Fernandez–Steel `sstd`

**Proved, symmetric case and fixed-skew transport.** At exactly held $\xi=1$, the likelihood equals `std`, including actual mean and sd holds. For fixed finite positive skew, use raw mode $c$ and common ordinary scale $s$, with left and right scale multipliers $\kappa_-=1/\xi,\ \kappa_+=\xi$. The density is $2t_\nu((x-c)/(s\kappa))/(s(\xi+\xi^{-1}))$ on the selected side. If $M_\nu=(\xi-\xi^{-1})E|T_\nu|$ for an ordinary unit-scale t, the public mean and sd are $c+sM_\nu$ and $s\sqrt{V_\nu}$, where
$$
V_\nu=\frac{\nu}{\nu-2}[1+(\xi-\xi^{-1})^2]-M_\nu^2 .
$$
The moments follow by the one-sided substitutions $z=(x-c)/(s\kappa_\pm)$, since the $j$-th raw moment about $c$ has side multipliers $\kappa_\pm^{j+1}$ in the respective integrals. The stable Student moment evaluator is already available. [Skew moment derivation](2026-11-08-sstd-moment-report.md), [existing skew certificates](2026-11-08-sstd-free-report.md).

With both moments free and skew fixed, projection brings $c$ into the sample range and the compact bounds apply to side-scaled distances, using $\kappa_{\max}$ for the residual lower bound. Each side has the same kernel and a fixed additive log-normalization offset. On a box crossing $c=x_i$, enclose the two expressions together. The first derivatives agree at the join and the second derivatives have finite one-sided bounds on a compact positive-scale box. Gradient and Taylor value enclosures then follow by integrating the piecewise Hessian along segments. There is a unique conditional scale root at fixed raw mode in the strict atom case. Geodesic uniqueness of the raw mode and scale cannot be inherited. For example, the covariant horizontal curvature of $\log(((x-c)/\kappa)^2+a^2)-\log a$ at the join is $(2/\kappa^2-1)/a^2$, from the $c,c$ second derivative minus $(\partial_a\ell)/a$. The sign can be negative on the broad side. The same positive-scale compact cover can reach the ordinary two-piece $t_2$ face under strict masses and finite fixed skew. With sd held and mean free, $s=d/\sqrt{V_\nu}$ is asymptotically proportional to $\sqrt{\nu-2}$, so the moving raw mode admits the same strict-mass collar using fixed side multipliers and bounds for $s/\sqrt{\nu-2}$. The constrained critical and joint skew corners need further comparisons.

**Proved, held mean, held df and held asymmetric skew.** The raw mode moves with $s$, so the conditional scale score is no longer monotone. It is nevertheless algebraic on finitely many branches. Set $y=1/s$, $d_i=x_i-\mathrm{mean}$, $R_i=d_i y+M_\nu$, and $a_i=\sqrt{\nu}\kappa_i$. The log-$y$ score is
$$
Q(y)=n-(\nu+1)\sum_i \frac{(R_i-M_\nu)R_i}{a_i^2+R_i^2}.
$$
The at most $d$ positive join locations $y=-M_\nu/d_i$, where $d$ here counts distinct observations, split the line into at most $d+1$ branches. Clearing denominators after merging repeats gives a polynomial of degree at most $2d$ on each branch. Its constant term is $n\prod_i(a_i^2+M_\nu^2)>0$, so it has at most $2d$ roots. Add the joins themselves as candidates. For a positive collapse exponent $e=(\nu+1)(n-k)-n$, with $k=\#\{d_i=0\}$, the score limits are $n$ and $-e$ as $y\to0$ and $y\to\infty$. Quantitatively $|(R_i-M_\nu)R_i/(a_i^2+R_i^2)|\le y|d_i|/(2a_{\min})$. At the large end require $y|d_i|-|M_\nu|\ge D>0$ for $d_i\ne0$. Each such term then differs from one by at most $|M_\nu|/D+a_{\max}^2/D^2$. Choose $D$ so the sum of these errors times $\nu+1$ is less than $e$; the exterior score signs follow. At $e=0$, the explicit branch limit of the likelihood must be compared and a polynomial or asymptotic bound can isolate remaining large-$y$ roots. If $e<0$, the likelihood diverges like $s^e$, directly by expansion of its fixed-shape kernels.

**Numerical.** A full interval score cover on the strict side isolated one stationary point on the sstd golden in 64 cells, and three on the archived asymmetric example in 134 cells. The latter used the exact stored binary64 sample, held mean zero, df three and skew ten. The two maxima have log sd $3.764665559515$ and $5.894765235144$; score integration over their root brackets separates their likelihoods by more than 11.6244. The 80-digit calculation verifies their values within the boxes. This closes a fixed-skew joint-moment subproblem that the prior numerical multimodality example exposed. General free skew still needs the left/right scale and joint corner analysis of the earlier reports. Azzalini's `st` is a separate skew mechanism. Exactly held slant zero can inherit the `lst` certificate as already implemented there. [Archived sstd example](2026-11-06-sstd-cert-report.md#why-general-skew-remains-open), [score cover](2026-11-25-tcert-math-data/skew_scale.py), [interval and root results](2026-11-25-tcert-math-data/skew_scale.json), [Azzalini skew-t report](2026-11-08-st-cert-report.md).

### Half-t `ht`

**Proved.** For a free threshold the monotone half-t density gives the exact endpoint $\mu=x_{\min}$, even with repeats. For any valid fixed $\mu$, on residuals $x_i-\mu\ge0$ the log likelihood is that of an ordinary t with location held at zero, plus $n\log 2$. The conditional scale bracket, inverse-shape derivatives, Schur curvature and root inclusion therefore transfer directly. The normal comparator is the half-normal at this endpoint. The fitted shape domain is $\nu\ge1$ for free threshold and $\nu>0$ for held threshold. With free threshold compare $k=\#\{x_i=\mu\}$ against half the sample; equality has the single endpoint collapse collar. With held threshold, free scale and an exact endpoint tie, unrestricted shape permits unboundedness. With no ties, the pointwise low-df bound transfers. These conclusions use the existing half-t endpoint and scale results. The new profile cover supplies a coefficient enclosure as well as the value comparison. The old archived five-point sample already demonstrates that a unimodality shortcut is unsuitable. [Half-t dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md), [existing half-t certificate](2026-11-02-ht-cert-report.md).

## Reproduction

The final Python runs passed on the workstation, sequentially through the single-slot reservation with single-thread BLAS/OpenMP and `nice -n 10`. The exact calculations use Python rationals and SymPy, and independent replays use mpmath at 80 digits. No builds, production tests, goldens or fitter changes were part of the job. Run from the worktree root.

```sh
data=dev/research/evaluations/2026-11-25-tcert-math-data
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run nice -n 10 python3 "$data/exact_modes.py" > "$data/exact_modes.json"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/fixed_df.py" > "$data/fixed_df.json"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/skew_scale.py" > "$data/skew_scale.json"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/boxes.py" --out "$data/boxes.json"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/check.py" > "$data/check.txt"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/tail_grid.py" > "$data/tail_grid.json"
```

The optional [initial search](2026-11-25-tcert-math-data/search_modes.py) and [rational simplification search](2026-11-25-tcert-math-data/reduce_modes.py) document how the exact example was selected. The sign proof uses only the fixed rational sample.

## What this changes for the fitter

- **Shape free, location held.** Add a certified finite route for `lst` and `std` when the monotone scale tube and global shape cover close. For `lst`, an occupied held location with both shape and scale free proves unboundedness, and otherwise the $B(\nu)$ bound closes the small-df tail. For `std`, use the actual occupied fraction at the held mean to decide the $t_2$ or collapse comparison. Reuse the existing scalar-shape route when both nuisances are held.
- **Shape free, ordinary scale or sd held.** Turn a locally checked finite nomination into a certified result only after exhausting the location/shape cover, verifying its stationary root enclosure in public units, and clearing the applicable faces. Keep actual fixed-sd derivatives and include both sides of all multimodal location branches. Use the moving-location sd collar at $\nu=2$. Leave the existing exact reflected two-maximum route in place.
- **Both nuisance parameters free.** The existing all-free certificate can use inverse-shape curvature and point inclusion to tighten its separate $10^{-6}$ basin enclosure when needed. Keep the geodesic conditional theorem, lower-face classifications and exact special cases. For held $0<\nu<1$ in `lst`, the finite algebraic root bound and compact location/log-scale cover give an exhaustive route when the mass condition is strict.
- **Outcome and work.** Return `Ok, certified=true` for an included finite winner meeting `tol`. Mark an attained Cauchy endpoint as restricted. Use the existing normal or positive-scale $t_2$ limit statuses only after dominance over the finite domain is established, and use proved unbounded or unattained-collapse outcomes for their respective proofs. A finite root tying a face stays attained. Keep both coefficient enclosures when maxima are proved equal; overlapping value intervals alone are an unresolved comparison. Under the fallback rule, a spent certificate budget or unresolved comparison leaves the converged, locally checked search result flagged `certified=false`. Use the existing failed-search or proved-wrong handling when that check or nomination fails. Charge every interval, face and tube evaluation against $128\min(\mathrm{iterlim},100000)$, with solves bounded by $\min(\mathrm{iterlim},100000)$.
- **Variants.** The `ht` hot path can reuse scalar scale tubes, inverse-shape derivatives and coefficient inclusion. Exact symmetric `sstd` can inherit the improved hold routes. At fixed asymmetric skew, mean and df, the interval log-scale cover can certify finite strict-mass cases even with multiple stationary points.

## Open

- General degenerate stationary points and equality between distinct modes or a finite point and a limiting face can prevent strict dominance at any budget. These need problem-specific equality proofs or higher-order derivative bounds. The critical collars above close the tails but still leave their compact competitors to compare. The subunit fixed-shape equality boundary with moving location remains outside the fixed-$\nu\ge1$ geodesic proof.
- The general nonsymmetric rare-outlier case needs tighter nuisance preconditioning or narrower verified starting tubes than the preliminary two-coordinate attempt. The exact symmetry reduction suffices for this report's counterexample. The observed box counts provide no uniform complexity bound.
- Production arithmetic requires intervalized normalization, stable $C'$ and $C''$ over the input range, and output-transport checks when shape is very near two, observations nearly coincide at large absolute location, or coefficients are subnormal. The high-precision checks here test the formulas and practical margins on the stated workloads.
- For asymmetric `sstd` with a held public moment and free df, the constrained mode/scale branches still need verified nuisance covers at the skew-normal and lower faces. With skew also free, uniform joint skew/df corner bounds and near-contact half-t comparisons remain open. The fixed-skew, fixed-df strict-mass scale result leaves its critical tail comparison to the polynomial or asymptotic step above.
