# Uniform GEV nuisance bounds

This is an algebraic result, independent of the delivered numerical scalar
certificate. It does not certify the existing joint fitter.

Let the fitted shape satisfy $-1\le a\le1$, and write the standardized GEV
density as $h_a(z)$. In the interior put
$L=\log(1+az)/a$, so

$$z=(e^{aL}-1)/a,\qquad
h_a(z)=\exp(-(1+a)L-e^{-L}).$$

The zero-shape formulas are their continuous limits. At the closed
reverse-exponential endpoint, $z=1$ and $h_{-1}(1)=1$.

Uniformly over the entire fitted branch,

$$h_a(z)\le1,\qquad z^2h_a(z)\le4.$$

For the first bound, set $y=e^{-L}$ and $c=1+a\in[0,2]$.
The maximum of $y^ce^{-y}$ over $y>0$ is $c^ce^{-c}$, with value one
in the $c=0$ closure. Its maximum over $c\in[0,2]$ is one.

For the second bound first suppose $|a|\ge1/2$.
Since $|e^{aL}-1|\le\max(1,e^{aL})$,

$$z^2h_a(z)\le4\exp\{\max(0,2aL)-(1+a)L-e^{-L}\}.$$

For $L\ge0$ the exponent is nonpositive. For $L=-s\le0$ it is at most
$2s-e^s$, whose exponential is at most $4e^{-2}<1$.
For $|a|<1/2$, the integral formula for $(e^{aL}-1)/a$ gives
$|z|\le |L|\max(1,e^{aL})$. If $L\ge0$, this bounds the product by
$L^2e^{-L/2}\le16e^{-2}<4$. If $L=-s\le0$, it bounds the product by
$s^2e^{3s/2-e^s}\le e^{5s/2-e^s}\le(5/2)^{5/2}e^{-5/2}<4$;
the middle inequality uses $s^2\le e^s$ for $s\ge0$.
These bounds extend to support closures by limits, and are trivially valid
outside support where the density is zero.

Consequently the original density satisfies

$$f(x;\mu,\sigma,a)\le\min\{1/\sigma,
 4\sigma/(x-\mu)^2\}.$$

Given any finite feasible log-likelihood incumbent $b$, put $B=e^{-b/n}$.
Every competitor with log likelihood at least $b$ has

$$\sigma\le B,\qquad m-2B\le\mu\le M+2B,$$

where $m=\min x_i$ and $M=\max x_i$. Indeed $f\le1/\sigma$ proves
the scale bound, and outside the stated location interval every observation
is more than $2B$ from $\mu$, giving $f<1/B$ and log likelihood below $b$.

For a nonconstant sample, let $k_*$ be its largest multiplicity at *any*
value and $\delta>0$ its minimum distinct-value spacing. At most $k_*$
observations are within strict distance $\delta/2$ of $\mu$.
For $\sigma\le\delta/4$, multiplying the two density bounds gives

$$\ell\le(n-2k_*)\log\sigma+(n-k_*)\log(16/\delta^2).$$

The qualification $\sigma\le\delta/4$ is necessary: it makes the bound for
far observations no larger than the near-observation bound, so replacing
the actual near count by its upper bound $k_*$ preserves the inequality.
If $k_*<n/2$, every competitor at least as good as $b$ therefore has

$$\sigma\ge\min\left\{\delta/4,
 \exp\left[\frac{b-(n-k_*)\log(16/\delta^2)}{n-2k_*}\right]\right\}>0.$$

These are explicit, uniform nuisance bounds including both shape endpoints
and zero shape. Arithmetic should evaluate them in log coordinates; their
size can make them impractical for enumeration. They apply to all untied
samples with $n\ge3$ and some tied samples. They do not apply when a
nonminimum value has multiplicity at least half, even if the minimum does
not. The half-minimum proof in the dossiers covers a different, sharp case.

The remaining implementation problem for a joint certificate on this box is
an efficient global likelihood/derivative enclosure across moving support
faces, including $a=-1$, with coefficient isolation and a margin-aware
comparison against the reverse-exponential face. A fixed mesh of the box
would not provide the required coefficient accuracy or exclude missed modes.
The later minimum/left-tail split supplies the lower bound when
$k_*\ge n/2$ but the minimum multiplicity is below half.

## Held-scale Fréchet consequence, with its constraint preserved

For free Fréchet location and shape with **held** scale $s>0$, enforce the
map exactly:

$$\mu_G=\mu_F+s,\quad \sigma_G=s/\lambda,\quad \xi=1/\lambda.$$

This is a constrained GEV surface, not the unconstrained GEV fit. The preceding
bounds, when $k_*<n/2$, give $\lambda\le A=s/S$, where $S$ is the explicit
positive GEV scale lower bound. A feasible incumbent implies $A\ge1$.
They also bound $\mu_F$ below by $m-2B-s$; support gives $\mu_F<m$.
One can separate the latter boundary effectively. Put
$r=s/(m-\mu_F)>1$. At a minimum observation,

$$\log f(m)\le\log(A/s)+(A+1)\log r-r,$$

using $1\le\lambda\le A$ and $r^\lambda\ge r$.
Every other density is at most $A/s$, by the GEV uniform density bound.
Thus

$$\ell\le n\log(A/s)+(A+1)\log r-r.$$

Any $R>A+1$ such that
$R-(A+1)\log R>n\log(A/s)-b$ excludes all $r\ge R$ from the incumbent
superlevel set, since the left side is then strictly increasing. Such an $R$
is effectively found by geometric expansion. Hence
$m-\mu_F\ge s/R>0$. Together these provide a compact location/shape box
for this constrained held-scale problem. They do not establish unimodality
or enumerate its maxima; the current held-scale fitter still lacks that
certificate. An implementation must meter both the bound solve and subsequent
box search and must stop rather than clamp when its budget is exhausted.

For the all-free Fréchet model, this argument cannot bound $\lambda$:
$\sigma_F=\sigma_G/\xi$ diverges as $\xi\downarrow0$ while the GEV
nuisance coordinates stay bounded. The next section supplies a conservative uniform remainder near the Gumbel
face; a practical margin-aware finite-versus-face comparison and coefficient
isolation remain necessary.

## Uniform face bounds on a compact nuisance box

These are further algebraic results, not routes in the delivered joint fitter.
They supply explicit one-sided bounds; a global multivariate search and
coefficient isolation still have to use them.

For the reverse-exponential neighbourhood use the **common upper endpoint**
$E$ and scale $\sigma$, with $\xi=-1+\eta$, $0<\eta\le1/2$.
For $x\le E$ put $v=(E-x)/\sigma$, $t=(1-\eta)v$ and
$p=1/(1-\eta)$. The GEV and reverse-exponential log densities are

$$-\log\sigma+(p-1)\log t-t^p,
\qquad -\log\sigma-v.$$

At $t=0$ the first expression has its limiting value $-\infty$.
On $0\le v\le T$, define

$$C_T=\max\{e^{-1},T^2\log\max(1,T)\},\qquad
D_T=2\log\max(1,T)+2C_T+T.$$

The mean-value theorem in the exponent gives
$|t^p-t|\le(p-1)C_T$, since
$\sup_{0\le t\le1,1\le q\le2}|t^q\log t|\le e^{-1}$ and
$\sup_{1\le t\le T,1\le q\le2}|t^q\log t|\le T^2\log T$.
Also $(p-1)\log t\le2\eta\log\max(1,T)$ and
$v-t=\eta v\le\eta T$. Therefore

$$\ell_{-1+\eta}(E,\sigma)
 \le\ell_{-1}(E,\sigma)+nD_T\eta.$$

This is uniform even arbitrarily close to an observed upper endpoint.
A two-sided remainder cannot hold: taking
$t=\exp(-c/\eta)$ produces a finite negative extra limit $-c$ in
$(p-1)\log t$, and still smaller $t$ produces arbitrarily negative
values. The one-sided bound is exactly what exterior likelihood exclusion
needs. **The common-endpoint transformation does not preserve held public
GEV location**; a held-location implementation must enforce its own constraint.
For all-free fits the explicit nuisance box bounds $E$ as well, giving finite
$T$ from its scale lower bound. For instance, when $\eta\le1/2$,
$E=\mu+\sigma/(1-\eta)\le M+4B$ and
$T=(M+4B-m)/S$ is sufficient, where $S$ is the scale lower bound above.

For the Gumbel neighbourhood keep the public GEV $\mu,\sigma$ fixed.
If $|z_i|\le Z$ on the nuisance box and $|\xi|Z\le r<1$, then

$$|L_i|\le Z/(1-r),\qquad
 |L_i-z_i|\le |\xi|Z^2/[2(1-r)].$$

The latter follows directly by bounding the tail of
$\log(1+\xi z_i)/\xi$ from its quadratic term onward.
The exponential mean-value theorem then gives

$$|\ell_\xi-\ell_0|\le n|\xi|\left\{
 \frac{Z}{1-r}+\frac{Z^2}{2(1-r)}
 [1+\exp(Z/(1-r))]\right\}.$$

This is an explicit uniform remainder through zero shape, including held
public GEV nuisance coordinates. Its constant may be far too large for a
practical search, and overflow must be a numerical stop, not exclusion.
For all-free Fréchet the GEV zero-shape face is the Gumbel limit; the
coefficient transformation is singular there, so the remainder alone does
not establish a finite Fréchet maximizer or its coefficient accuracy.

## Attainment consequence

For $k_*<n/2$, the GEV incumbent superlevel set is contained in the explicit
compact box with scale bounded away from zero. The joint density is upper
semicontinuous on the fitted branch with its declared support closure.
Interior points and zero shape are continuous. At an ordinary support seam
the density tends to zero. At the exceptional $\xi=-1$ endpoint, the uniform
bound $y^ce^{-y}\le c^ce^{-c}\to1$ as $c=1+\xi\downarrow0$ bounds the
limsup by its declared endpoint density $1/\sigma$. Thus the likelihood
attains a finite global maximum on the box. This proves existence for every
untied sample of size at least three and for the stated tied samples; it is
not a numerical location of that maximum or a uniqueness theorem.

The compact held-scale Fréchet box similarly proves finite attainment when
$k_*<n/2$, by continuity away from its now-separated location boundary.
For all-free Fréchet, compactness in the GEV coordinates instead proves
attainment in its closure $0\le\xi\le1$: zero shape may still select a Gumbel
limit, which is not a finite Fréchet fit. The delivered code does not use
these existence consequences as substitutes for coefficient certificates.

## Fixed-shape nonpositive inner profiles

For fixed $-1<\xi<0$, put $\alpha=-1/\xi>1$, Weibull scale
$s=-\sigma/\xi$, endpoint $E=\mu+s$, and natural coordinates
$a=E/s$, $b=1/s$. On the convex domain $b>0$, $t_i=a-bx_i>0$, the
log likelihood is

$$n\log\alpha+n\log b+(\alpha-1)\sum_i\log t_i-\sum_i t_i^\alpha.$$

The one-variable term has curvature
$-(\alpha-1)/t^2-\alpha(\alpha-1)t^{\alpha-2}<0$.
Together with $n\log b$, this makes the likelihood strictly concave in
$(a,b)$. Holding public GEV scale fixes $b$; holding public GEV location
imposes the affine constraint $a=1+\mu b$. Thus these fixed-negative-shape
inner problems admit global concavity certificates without an assumption
of concavity in public $(\mu,\sigma)$ coordinates. At $\xi=-1$ the limiting
linear endpoint problem selects $E=\max x$ and the usual reverse-exponential
scale. At $\xi=0$, use $a=\mu/\sigma$, $b=1/\sigma$ instead:

$$\ell=n\log b+na-b\sum_i x_i-\sum_i e^{a-bx_i}$$

is strictly concave as well; held public location again gives an affine
constraint, now $a=\mu b$.

For all-free nuisance coordinates at fixed positive shape, the earlier
fixed-shape Fréchet certificate supplies the unique inner fit under its
strict collapse criterion. This transport is valid only with the exact
coefficient constraint for any public hold; it does not turn arbitrary held
GEV subsets into unconstrained Fréchet fits.

These observations establish globality of an exact finite inner stationary
point, not accuracy or successful termination of the current Newton code.
A practical certified profile still needs bounded root or concave-system
inclusion with a coefficient-width check, followed by exhaustive outer-shape
coverage and the face comparisons above. The existing all-free GEV root
scan has not acquired that certificate merely because its inner objective
has these concave coordinates.

## Order-statistic strengthening and the remaining numerical scale

The scale lower bound can be strengthened without assuming small minimum
spacing. For sorted data and each integer $k_*\le j<n/2$, put
$\delta_j=\min_i(x_{i+j}-x_i)>0$. An open interval of width $\delta_j$
contains at most $j$ observations. Repeating the near/far proof gives

$$S_j=\min\left\{\delta_j/4,
 \exp\left[\frac{b-(n-j)\log(16/\delta_j^2)}{n-2j}\right]\right\},
 \qquad \sigma\ge\max_j S_j.$$

This finite order-statistic optimization can improve the box greatly. It still
does not make the analytic Gumbel constant uniformly practical: with
$Z=(M-m+2B)/S$, the bound contains $\exp(Z/(1-r))$. A certificate cannot
replace an overflowing constant or an unrepresentable required face slab by
a fixed nudge. It needs sharper profile-dependent superlevel sets or an
adaptive enclosure which excludes those nuisance regions before the shape
comparison. The accompanying `bound-scales.json` evaluates the original and
strengthened boxes on the two frozen golden samples; it is a numerical
illustration of this scale problem, not a global fit certificate.

## Sharp envelope and held-minimum equality

The preceding conservative envelope can be sharpened to
$h_\xi(z)\le\min(1,z^{-2})$. With $y=e^{-L}$, convexity of $\sinh$ gives

$$z^2h_\xi(z)=4[\sinh(\xi L/2)/\xi]^2e^{-L-y}
 \le(1-y)^2e^{-y}\le1.$$

Zero shape follows by continuity. For $y>1$ the latter function is at most
$4e^{-3}<1$, and for $0<y\le1$ it is strictly below one. The only attained
finite equality is the reverse-exponential endpoint $\xi=-1,z=1$.
This reduces the location box to $[m-B,M+B]$. In the spacing/order-statistic
lower bounds, replace $16/\delta_j^2$ by $4/\delta_j^2$ and the qualification
$\sigma\le\delta_j/4$ by $\sigma\le\delta_j/2$. The final numerical table
retains the conservative results and also evaluates this sharp improvement.
Its $Z,T$ and face constants use the improved box; $T=(M-m+3B)/S$ suffices.

For **held public GEV location $\mu=m$**, the minimum densities are exactly
$e^{-1}/\sigma$. At a half-minimum tie the sharp envelope on all other
observations gives

$$\ell\le-k-2\sum_{x_i>m}\log(x_i-m)=A_m.$$

The positive-shape collapse with $\xi=1$ and $\sigma\downarrow0$ attains
this bound in the limit. Equality at finite parameters occurs exactly when
all nonminimum observations equal one $M$, at $\sigma=M-m$, $\xi=-1$.
This preserves the public location hold and is a separate global proof from
the all-free pair bound, whose supremum differs by $n(\log2-1/2)$.
The delivered core now implements this held-location classification too.

## Uniform bounds without a largest-multiplicity restriction

The sharp envelope also supplies a uniform left-tail bound. If $z=-r$,
$r\ge1$, then $y=e^{-L}\ge1+r$ for every fitted shape. For negative shape
this follows from $(1+|\xi|r)^{1/|\xi|}\ge1+r$; for nonnegative shape,
$y\ge e^r\ge1+r$ on its support. Since $1+\xi\le2$,

$$h_\xi(-r)\le y^2e^{-y}\le(1+r)^2e^{-1-r},\qquad r\ge1.$$

Let $k$ be the **minimum** multiplicity, $k<n/2$, and let $d>0$ be the gap
from the minimum $m$ to the next distinct observation. Split the nuisance
location domain at $m+d/2$.

* If $\mu\le m+d/2$, all $n-k$ nonminimum observations are at least $d/2$
  above $\mu$. Multiplying their sharp tail bounds and the $k$ remaining
  density bounds gives
  $\ell\le(n-2k)\log\sigma+(n-k)\log(4/d^2)$.
  Thus every incumbent competitor has
  $\sigma\ge S_A=\exp\{[b-(n-k)\log(4/d^2)]/(n-2k)\}>0$ in this region.
* If $\mu>m+d/2$, at least one minimum is on the left. For
  $R=d/(2\sigma)\ge1$, the left-tail bound and the other $n-1$ density bounds
  give
  $\ell\le n\log(2R/d)+2\log(1+R)-1-R$.
  The right side decreases strictly for $R\ge n+2$ and tends to minus
  infinity. Choose any $R_0\ge n+2$ for which it is below $b$; geometric
  expansion finds one in exact arithmetic. Then
  $\sigma\ge S_B=d/(2R_0)>0$ in this region.

Consequently $\sigma\ge\min(S_A,S_B)$ uniformly whenever the **minimum**
multiplicity is below half, even if some other value occurs at least half the
time. This closes the multiplicity gap in the earlier conservative box.
Together with $\sigma\le B$, $m-B\le\mu\le M+B$ and upper semicontinuity,
it proves finite GEV attainment for every nonconstant sample with $k<n/2$.
The dossiers' pair bound settles $k=n/2$, and the $\xi=1$ collapse diverges
for $k>n/2$. This is a full existence classification on the fitted branch;
the ordinary joint numerical fitter still has no coefficient certificate.

For a **held** location the bounds simplify. If it is not an observation,
$\ell\le n\log\sigma-2\sum_i\log|x_i-\mu|$ bounds scale away from zero.
If it equals $k<n/2$ observations, the bound is
$\ell\le-k+(n-2k)\log\sigma-2\sum_{x_i\ne\mu}\log|x_i-\mu|$.
If a minimum lies strictly below held $\mu$, the exponential left-tail bound
bounds scale away from zero regardless of ties at $\mu$. Therefore the only
held-location obstructions are precisely the minimum-majority divergence and
the held-minimum half-tie cases already classified above. The runtime outer
shape/scale search outside those cases remains uncertified.

## Held-scale Fréchet compactness for every nonconstant sample

A separate argument preserves held public Fréchet scale $s>0$ and removes
the earlier $k_*<n/2$ qualification. With minimum $m$, range $d=M-m>0$
and location gap $\varepsilon=m-\mu_F$, the universal GEV location bound gives
$0<\varepsilon\le B+s$. Put
$u=\min(s/2,d/2)$, $q=1-u/s\in[1/2,1)$,
$c=\log(1+d/(2s))>0$ and $a=-\log q>0$.
The Fréchet density is everywhere at most $\lambda/s$ for $\lambda\ge1$.

* For $\varepsilon\ge s-u$, the observation $M$ has normalized distance at
  least $1+d/(2s)$. Hence
  $\ell\le n\log(\lambda/s)-c\lambda$.
* For $\varepsilon<s-u$, the minimum has normalized distance at most $q$.
  Writing $w=((m-\mu_F)/s)^{-\lambda}\ge e^{a\lambda}$, its log density is
  at most $\log(\lambda/s)+2\log w-w$. When $e^{a\lambda}\ge2$,
  $\ell\le n\log(\lambda/s)+2a\lambda-e^{a\lambda}$.

Both upper envelopes tend to minus infinity. The first decreases for
$\lambda>n/c$; the second decreases for
$\lambda\ge\max(n/a,\log(3)/a)$. Expanding beyond these thresholds produces
an explicit finite shape upper bound for the incumbent superlevel set, for
every nonconstant sample, including majority-minimum ties. The earlier
held-scale location-gap exclusion then supplies a positive gap lower bound.
Thus the held-scale free-location/free-shape Fréchet problem always attains
a finite maximum for nonconstant data. Constants instead have the proved
shape divergence. This argument supplies a compact search box, not a root
count, a uniqueness result or an implemented global two-coordinate search.

## All-free two-point global member

For minimum fraction $p=k/n\le1/2$ and distance $d=M-m$, the reverse
exponential at $\sigma=pd$, $\mu=M-pd$ has likelihood
$e^{-n}(pd)^{-n}$. For reflected Weibull shape $a\ge1$ and endpoint-distance
ratio $r\in(0,1)$, scale profiling gives the relative likelihood to the
power $1/n$ as

$$R_W=\frac{ap(1-r)r^{(a-1)(1-p)}}{p+(1-p)r^a}.$$

For Fréchet with shape $a\ge1$ it gives
$R_F=R_Wr^{1-2p}$. Since $p\le1/2$, the denominator is at least
$p(1+r^a)$, the exponent in $R_W$ is at least $(a-1)/2$, and
$r^{1-2p}\le1$. Both ratios are bounded by the strict unweighted pair
ratio proved in the dossiers. At zero GEV shape, the limiting pair ratio is
$t/\cosh t<1$ for every finite Gumbel scale. The only finite equality is
reflected Weibull shape one, endpoint $M$, and scale $pd$.

Thus this is the unique global member for every supported all-free two-point
sample with $p\le1/2$; a strict minimum majority remains unbounded. The core
uses counts rather than a rounded mean, a stable half-tie midpoint, and
explicit coefficient-precision/endpoint checks. Those numerical checks can
stop without returning an estimate when a relative location or subnormal
scale cannot be certified. In particular, a nearly cancelling midpoint is
never replaced silently by zero.
