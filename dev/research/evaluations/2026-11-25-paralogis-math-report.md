# Global ties in paralogistic likelihoods

TASK-271 · 2026-11-25

**Proved.** Universal uniqueness fails for both the all-free and held-scale
paralogistic fits, even on nonconstant samples. Each construction below has
exactly two global maxima at one exactly specified sample. On either side of
that sample, the two strict local maxima persist over the stated parameter
interval and the global winner is identified. All these results transfer to
inverse paralogistic. There are also linear-time sufficient sample conditions
for uniqueness on both routes.

Let $a>0$ be shape, $s>0$ be scale, $y_i=\log x_i$, $b=-a\log s$, and
$z_i=ay_i+b$. Write $A=1+a$, $h(z)=\log(1+e^z)$, $p(z)=h'(z)$ and
$q(z)=p(z)(1-p(z))$. Use the mean log likelihood with its data-only term
removed,

$$L(a,b)=2\log a+\mathbb E[z-Ah(z)].$$

Here $\mathbb E$ denotes the sample mean, including multiplicities. The
prior certificate work proves finite attainment on both routes for
nonconstant samples and unique scale at every fixed shape. It also proves
unboundedness for constant all-free samples and attainment for all positive
held-scale samples. The earlier held-scale modes example establishes
competing local maxima at different heights. [Paralogis dossier](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) · [Certificate report](2026-11-06-paralogis-cert-report.md) · [Modes report](2026-11-06-paralogis-modes-report.md) · [Transfer report](2026-11-25-transfer-math-report.md#open)

## Held scale

**Proved by interval exclusion and a strict crossing.** Hold scale at 1
and use the exact two-point sample $(e^{-1/1000},e^{-r})$,
$3/1000\le r\le 5/1000$. Let $M_l(r)$ and $M_h(r)$ be the maxima of
$L(a,0)$ on $[2,4]$ and $[6500,10000]$, respectively. The exact tied
sample uses the unique $r_*$ in $(3/1000,5/1000)$ satisfying
$M_h(r_*)=M_l(r_*)$. Each band contributes one global maximizer. The high
band wins for $r<r_*$ and the low band wins for $r>r_*$.

For distances $d\in\{1/1000,r\}$, the criterion and shape derivatives are

$$
\begin{aligned}
H(a,r)&=2\log a+\mathbb E[-ad-Ah(-ad)],\\
H_a&=2/a+\mathbb E[-d-h(-ad)+Adp(-ad)],\\
H_{aa}&=-2/a^2+\mathbb E[dp(-ad)\{2-Ad(1-p(-ad))\}].
\end{aligned}
$$

For $a\le1$, $H_a\ge 2-.005-\log 2>0$. For $a\ge20000$,
$d e^{-ad}\le .001 e^{-.001a}$ and $(1+a)e^{-.001a}$ decreases. Hence
$H_a\le 2/20000-.002+20001(.001)e^{-20}<0$. The interval checker verifies
inward score signs at the endpoints of each band and $H_{aa}<0$ throughout
each band. For the high band the curvature follows from
$6501(.001)(1-p(-6.5))>2$ and monotonicity of the product in $a$ and $d$.

On the complement of the two bands in $[1,20000]$, the checker bisects
shape intervals separately on each of 40 equal rational $r$ slabs. It
accepts a cell only when the interval score excludes zero or the
upper likelihood bound lies strictly below the lower bound at feasible
shape $3$ throughout that slab. The cover needed 838 cell evaluations
including subdivisions. Together with the tail signs this puts every
global maximum in one of the two bands. Strict curvature and inward
scores give one interior root per band, with smooth dependence on $r$.

At either band maximum the derivative of its height with respect to $r$
is $a( A p(-ar)-1)/2$. It is positive throughout the low band, since
$3p(-4\cdot .005)>1$, and negative throughout the high band, since
$10001p(-6500\cdot .003)<1$. The high-minus-low difference is thus
strictly decreasing. Interval bisection of the two shape roots at each
endpoint gives the following outward summaries for the height difference.

| Exact $r$ | $M_h(r)-M_l(r)$ | Global band |
|---|---:|---|
| $3/1000$ | $(1.34121,1.34122)$ | High |
| $5/1000$ | $(-5.91757,-5.91756)$ | Low |

Continuity, strict ordering of derivatives and complete exclusion of the
complement prove the tie and its exact cardinality.

## All-free fit

**Proved by analytic profiling bounds and interval comparisons.** Put
$\epsilon=1/10000$, $w=1-\epsilon$ and $7/5\le K\le8/5$. Take one
observation equal to 1 and 9,999 observations equal to
$\exp(K/\epsilon^2)$. Thus the log sample consists of 0 and
$\Delta=K/\epsilon^2$ with weights $\epsilon$ and $w$. Put $t=a/\epsilon$.
Let $M_l(K)$ and $M_h(K)$ be the maxima of the scale-profiled criterion,
shifted by $-2\log\epsilon$, on $[3/4,9/10]$ and $[11/10,17/10]$ in $t$.
The exact tied sample uses the unique $K_*$ in $(7/5,8/5)$ satisfying
$M_h(K_*)=M_l(K_*)$. Each band contributes one global maximizer. The high
band wins for $K<K_*$ and the low band wins for $K>K_*$.

### Global exclusion by a profile bound

Put $D=a\Delta=Kt/\epsilon$. Replacing $h(z)$ by $\max(z,0)$ gives an
upper bound. Its scale profile is

$$G(t;K)=2\log t-K\min(wt^2,t).$$

Indeed the two logits are $b$ and $b+D$. The piecewise linear intercept
objective has a maximum at $b=0$ or $b=-D$. Evaluating these two choices
gives the costs $waD$ and $\epsilon D$.

For the exact scale profile $P(t;K)$ after the shift, the lower bound is

$$G(t;K)-A\mathcal H(1/A)\le P(t;K)\le G(t;K),\qquad
\mathcal H(u)=-u\log u-(1-u)\log(1-u).$$

To see the lower bound, take the exact optimizing logits with probabilities
$p_l,p_h$. The intercept score gives $\epsilon p_l+wp_h=1/A$.
Using $h(z)=p(z)z+\mathcal H(p(z))$, the intercept term cancels and
the remaining nonentropy term besides $2\log t$ is $Dw(1-Ap_h)$.
The constraint implies $p_h\le\min(1,1/(Aw))$, so that term is at
least $-D\min(wa,\epsilon)$. Jensen bounds the weighted entropy by
$\mathcal H(1/A)$.

The two hard-profile branches meet at $t=1/w$. Their maxima have shapes
$t_l=1/\sqrt{wK}$ and $t_h=2/K$, and values

$$g_l=-1-\log(wK),\qquad g_h=2\log(2/K)-2.$$

These witness shapes lie strictly inside their respective bands. Since they
are below $2$, the entropy error at each is below $1/500$. This follows from
$A\mathcal H(1/A)\le a[1+\log((1+a)/a)]$, which increases in $a$ and
is below $1/500$ at $a=2\epsilon$. The checker verifies that the gap from
each hard maximum to either edge of its band exceeds $1/500$ over the
whole $K$ range. For a fixed edge $t$ the derivative of this gap with respect
to $K$ is $wt^2-1/K$ on the low branch and $t-2/K$ on the high branch.
Thus each edge gap is smallest at the endpoint placing the hard maximum
closest to that edge. Monotonicity of each hard profile on either side of
its peak extends the edge exclusions to the rest of each branch, including
the joining point. Within each band there is an interior maximum of the
exact profile, and no global maximizer lies outside the bands.

### Strict curvature and the tie

Use an intercept $v$ anchored at the low logit in the low band and the
high logit in the high band. The shifted unprofiled criteria in these two
charts are

$$
\begin{aligned}
R_l &= 2\log t - Kwt^2+
  \epsilon[(1-wt)v-Ah(v)]-Aw h(-v-D),\\
R_h &= 2\log t - Kt+v-Aw h(v)-A\epsilon h(v-D).
\end{aligned}
$$

The maximizing intercepts are in $(-3,0)$ and $(8,12)$, respectively.
The checker verifies the inward $v$ score signs on those faces for each
full band and $K$ range. In both rectangles the argument of the last
softplus term is below $-10000$. There its first two derivatives are
bounded by $\eta=e^{-10000}$. With $V=K/\epsilon$ and
$q_0=p(v_{\min})(1-p(v_{\max}))$, the following bounds follow by
differentiating the displayed charts and applying the tail bounds.

| Band | Upper bound on $R_{tt}$ | Upper bound on $\lvert R_{tv}\rvert$ | Lower bound on $-R_{vv}$ |
|---|---|---|---|
| Low | $-2/t^2-2Kw+2\epsilon wV\eta$ | $\epsilon+w(\epsilon+AV)\eta$ | $\epsilon q_0$ |
| High | $-2/t^2+2\epsilon^2 V\eta$ | $\epsilon w+\epsilon(\epsilon+AV)\eta$ | $wq_0$ |

The checker combines these bounds to verify
$R_{tt}+R_{tv}^2/(-R_{vv})<0$, the scale-profile curvature, over both
bands. Each band therefore has exactly one strictly maximizing shape.
Together with the unique scale solve this identifies both coefficient pairs
at the tie. With $b$ the low observation's logit, the scale is
$\exp(-b/a)$, where $b=v$ in the low band and $b=v-D$ in the high band.

At an optimizing scale, the derivative of the profile with respect to
$K$ is $wt(1-Ap_h)/\epsilon=-t(1-Ap_l)$. The derivative of the low
band's maximizing height is at least $-9/10$. On the high band
$p_l\le\eta$ and $1-A\eta>.99$, so the derivative is at most
$-.99(11/10)<-9/10$. The high-minus-low difference is strictly
decreasing. At each endpoint, its deviation from $g_h-g_l$ is less
than $1/500$ because each band height is in $[g_j-1/500,g_j]$.

| Exact $K$ | Interval comparison for $g_h-g_l$ | Global band |
|---|---:|---|
| $7/5$ | $(.04972,.04973)$ | High |
| $8/5$ | $(-.08381,-.08380)$ | Low |

The endpoint signs, continuity and strict decrease establish exactly one
crossing. The strict band curvature and global exclusion establish
exactly two global maxima at that crossing.

## Cheap sufficient sample conditions

**Held scale, proved.** Write $y_i=\log(x_i/s)$. The fit has a unique
shape if

$$
\frac{1}{n}\sum_{y_i<0}\frac1{-y_i}<\frac{e^2}{4}.
$$

In particular this holds when every observation is at or above the held
scale. In the curvature $-2/a^2-2\mathbb E[y p(ay)]
-A\mathbb E[y^2 q(ay)]$, the only potentially positive contribution comes
from negative $y=-d$. It is bounded by $2d e^{-ad}$. Multiplication by
$a^2$ and $(ad)^2 e^{-ad}\le4/e^2$ give strict negativity under the
stated condition. Attainment then gives a unique maximum.

**All-free, proved.** A nonconstant sample with
$\Delta=\max y_i-\min y_i<1/240$ has a unique fit. The profile has
positive score for $a\le15$, and at all stationary shapes $a\ge15$
its curvature is strictly negative.

For the proof let $F(a)=\max_b L(a,b)$, $\mu=1/A$, $u=1-\mu$,
$C=\mathbb E[q(z)]$ and $m=\mathbb E[q(z)y]/C$, evaluated at the
unique optimal intercept. The intercept score gives $\mathbb E[p]=\mu$.
It also implies $\min z\le-\log a\le\max z$, so
$\mathbb E[h(z)]\le\log(1+1/a)+a\Delta$. The shape score is

$$aF'(a)=2-a\mathbb E[h(z)]+\mathbb E[(1-Ap)z].$$

The factor $1-Ap$ has mean zero and mean positive part at most one.
Since $\max z-\min z=a\Delta$, the final term is at least $-a\Delta$.
As $a\log(1+1/a)<1$, this proves $aF'(a)>1-a(a+1)\Delta\ge0$
for $a\le15$ under the condition.

The scale-profile curvature is

$$
F''(a)=-2/a^2-2\mathbb E[(y-m)p]
       -A\mathbb E[q(y-m)^2]+\frac{\mu^2}{AC}.
$$

This follows by subtracting $L_{ab}^2/L_{bb}$ from $L_{aa}$.
The term $\mathbb E[(y-m)p]$ is nonnegative. Dividing by $C$ writes it
as the covariance of $y$ and $p/q=1/(1-p)$ under the $q$-weighted
sample, and both functions increase in $y$. Set $\rho=C/(\mu u)>0$.
If $F''\ge0$, the resulting upper bound
$a^2F''\le -2+u/\rho$ implies $\rho\le u/2$.

To bound the score at such a point, give each observation masses
$p_i/(n\mu)$ and $(1-p_i)/(nu)$ under two probability measures.
Let $J$ be the sum of their two directed relative entropies. Their log
density ratio is $z_i$ plus a constant, so
$\mathbb E[(1-Ap)z]=-uJ$. Passing both measures through the same
Bernoulli experiment with success probability $p_i$ at observation $i$
gives success means $1-u\rho$ and $\mu\rho$. The log-sum inequality
applied separately to successes and failures gives

$$
J\ge(1-\rho)\log
\frac{(1-u\rho)(1-\mu\rho)}{\mu u\rho^2}.
$$

If $a\ge15$ and $F''\ge0$, then $\rho\le1/2$ and $\mu\le1/16$.
The ratio inside the logarithm is at least
$(1-\rho)^2/(\mu\rho^2)\ge1/\mu$. Also
$a\mathbb E[h]\ge -a\log(1-\mu)\ge u$ by Jensen. Consequently

$$aF'\le 2-u[1+(1-\rho)\log(1/\mu)]
       \le 2-\frac{15}{16}(1+\tfrac12\log16)<0.$$

Thus every stationary point above 15 has strictly negative curvature.
A continuous score with only strictly downward crossings can have at
most one zero on that interval. Attainment, the positive low-shape
score and the unique scale at fixed shape complete the proof.

## Reciprocal transfer and verification

**Proved.** On inverse data $x_i$, the parent uses observations $1/x_i$
and scale $1/s$. Shape is unchanged and the log Jacobian is
$-2\sum_i\log x_i$, independent of the coefficients. The map transports
the entire maximizing set and all strict comparisons. The held-scale
counterexample thus has observations $(e^{1/1000},e^{r_*})$ at scale 1.
The joint counterexample has one unit observation and 9,999 observations
$e^{-K_*/\epsilon^2}$. The joint spread condition is invariant under
reflection of the log data. The held-scale condition uses
$y_i=\log(s/x_i)$ in the inverse orientation. Held shape has a unique
scale for every positive sample by the prior monotone scale result.
[Inverse dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) · [Transfer report](2026-11-25-transfer-math-report.md)

**Numerical illustrations.** The 60-decimal mpmath solves start with one
shape in each proven band and solve the two shape stationarity equations
and equality of heights, plus the intercept equations on the joint route.
The displayed digits are numerical. The equality and peak counts follow
from the proofs above, using 50-decimal interval comparisons. The held
cover uses its 40 rational sample-parameter slabs, adaptive dyadic shape
cells and 85 shape bisections per root at the endpoints. The joint
comparisons use the complete rectangles and the analytic endpoint and
tail bounds, with no shape grid.

| Route | Sample parameter at the tie | Low shape | High shape |
|---|---:|---:|---:|
| Held scale | $r_*\approx .003358304097588778$ | $2.90729263988569$ | $7427.69354664871$ |
| Joint | $K_*\approx 1.470845371979718$ | $.0000824616573952476$ | $.000135881672765889$ |

The final Python run passed on the workstation under one reserved CPU
slot. Its assertions cover the strict comparisons used in both
constructions. [Interval and numerical reproducer](2026-11-25-paralogis-math-data/certify.py)

```sh
/tmp/umvl-cpu/run nice -n 10 python3 -u dev/research/evaluations/2026-11-25-paralogis-math-data/certify.py
```

## What this changes for the fitter

- The held-scale sufficient condition supports an analytic singleton
  route with a scalar score bracket. The joint spread condition supports
  a singleton route with the profiled score bracket and the unique
  conditional scale solve. Require a strict, numerically resolved
  sufficient-condition comparison, plus a root and coefficient
  enclosure at the requested accuracy.
- The existing compact-domain certificate can set `unique = TRUE`
  when the full comparison leaves a single verified contraction region
  on the actual input. A value certificate with unresolved cardinality
  reports `certified = TRUE` and `unique = NA`. A successful search with
  an unclosed certificate keeps the uncertified fallback and
  `unique = NA` when there is no independent singleton proof. Preserve
  the stop on proven refutation or search failure. [Uniqueness cover](2026-11-25-uniq-cover-math-report.md) · [2026-11-25 decision](../../roadmap/decisions.md#2026-11-25)
- A proved tie calls for `unique = FALSE` and the full maximizing set.
  Multiple local modes or overlapping height enclosures by themselves
  leave cardinality undetermined. Transfer a parent cardinality proof to
  the inverse fit only with comparisons valid for the original inverse
  observations, for example through enclosed reflected log spacings.
  [Uniqueness cover](2026-11-25-uniq-cover-math-report.md)

## Open

- An exact joint tie with observations all in binary64's finite positive
  range remains open here. The joint example has a log spread above
  $1.4\times10^8$. The held-scale construction uses ordinary-magnitude
  real inputs. Rounding its inputs can change the peak ordering and
  carries no equality certificate.
- A broader cheap singleton criterion would need additional control of
  stationary roots below shape 15 or strict comparisons among them.
  The small-spread proof excludes that entire shape interval by a uniform
  score bound. General exact tie detection would require an equality
  proof for the actual input in addition to isolating competing maxima.
