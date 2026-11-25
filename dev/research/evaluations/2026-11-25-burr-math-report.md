# Burr XII root counts, exact ties and escape faces

TASK-286 · 2026-11-25 · source `eb93bd90`

**Proved.** All-free Burr XII can have two separated strict finite local
maxima. In the six-observation example below, both beat Weibull and both
lose to Pareto, which is the supremum. With power held, a four-observation
sample has exactly two finite global maximizers, strictly above its only
escape face. Holding scale or the first shape gives a unique maximum on
every slice where a finite maximum exists. These conclusions transfer to
inverse Burr and to shifted Burr with its threshold held. With a free
threshold and free power, shifted Burr has unbounded likelihood.

The comparison framework comes from the [closure proof](2026-11-03-burr-closure-report.md),
[profile bounds](2026-11-03-burr-cert-report.md), and
[root-box derivation](2026-11-04-burr-root-data/derivation.md).
The [transfer report](2026-11-25-transfer-math-report.md#open) and
[uniqueness inventory](2026-11-25-uniq-cover-math-report.md) distinguish
a unique nuisance fit from a unique joint fit. Shao discusses competing
maxima in the context of the two limiting distributions. The constructions
below explicitly distinguish finite local maxima, finite global ties and
finite ties with an escape value. [Shao (2004)](https://doi.org/10.1016/S0167-9473(02)00367-5)

## All-free Burr XII

Write $k,c,s$ for the first shape, power and scale. Use $y_i=\log x_i$,
$b=-c\log s$, $z_i=cy_i+b$ and $h(z)=\log(1+e^z)$.
The mean log likelihood after removal of the data-only Jacobian is
$L=\log(kc)+\overline{z-(1+k)h(z)}$. Bars denote sample means including
multiplicities. For nonconstant data, at each fixed $k>0$ the nuisance
maximum over $(c,b)$ exists and is unique by strict concavity and
coercivity. Let $F(k)$ be its value. This profiling preserves finite
local and global maxima.

**Proved by interval inclusion and exclusion.** Take the exact sample
$x_i=\exp(y_i)$ with $8y=(0,2,2,5,6,8)$. The table reports
80-digit numerical centers and per-observation differences.

| $k$ | $c$ | $b$ | $F(k)-P$ | $F(k)-W$ |
|---:|---:|---:|---:|---:|
| 0.2233701496 | 11.14593955 | −1.163835689 | −0.12350347 | +0.00331247 |
| 7.868416575 | 3.372077051 | −4.157978078 | −0.12660259 | +0.00021335 |

The checker encloses the Burr scores and Hessian from the root-box
derivation in two disjoint boxes of radius approximately $10^{-9}$
in each coordinate. The centers and preconditioners are treated as exact
dyadics in the inclusion test. For score $g$, preconditioner $R$
and radii $r$, each component of
$|Rg(\theta_0)|+|I+R Dg(B)|r$ is bounded strictly below $r$.
The map $\theta\mapsto\theta+Rg(\theta)$ therefore contracts a closed box
into itself. The norm bound also implies $R$ is nonsingular, since
otherwise $I+R Dg$ would have eigenvalue one. The three leading minors
of the negative Hessian are positive throughout each box. Thus each
box contains a unique stationary point and it is a strict local maximum.
The enclosed mean likelihoods are in
$(-0.387796687,-0.387796662)$ and
$(-0.390895801,-0.390895793)$.

The Pareto theorem strictly excludes $0<k\le 1/5$. The unsigned
Weibull bound at $k=128$, propagated from a signed Weibull power
bracket, is strictly below $P$ and decreases thereafter. On
$[1/8,128]$ the checker uses 13 entropy duals on 12 accepted chord
intervals. At each knot it corrects one logistic weight to make
$\overline p=1/(1+k)$ exact, checks every weight is interior, and encloses
the positive denominator
$A=\overline{[(1+k)p-1]y}$. The existing dual gives
$U=\log(k/A)-1-(1+k)\overline{\mathcal H(p)}$. On a chord
$[a,d]$ the upper bound is
$\max(U_a,U_d)+(d-a)^2/(8a^2)$. Every accepted upper lies strictly
below $P$. Thus this exact sample has no finite global MLE, including
at any unobserved stationary point outside the two boxes. The
two maxima persist under sufficiently small perturbations by the
nondegenerate Hessians. [Interval checker](2026-11-25-burr-math-data/check.py)

**Numerical search.** The reconnaissance used all 2,002 ordered
normalized log patterns with $n=2,\ldots,7$, endpoints 0 and 1, and
interior knots on the eighth grid. It also used 768 seeded structured
samples with three to five clusters, seven gap ratios from $2^{-10}$
to $2^{10}$, and integer multiplicities drawn from
$\{1,2,5,20,100,1000\}$. It checked 97 log-spaced $k$ values from
$2^{-12}$ to $2^{12}$; the held-power scan used six log spreads
from 1 to 64 and 381 log-rate points. The scans identify candidate
crossings and can miss narrow bands or poorly conditioned conditional
roots. The interval checks above are independent of those limitations.
A second scan varied two log levels of the six-observation example on
a $31\times31$ grid and found 27 two-mode patterns. The lower
candidate height was at least 0.109 per observation below the larger
of Pareto and the profile value at $k=128$ in these comparisons.
The scan saw a reversal in the relative heights while both candidates
were below Pareto. The independent four-observation held-power continuation
found 489 grid patterns with two positive candidate gaps on a
$20\times17\times17$ data grid and 821 log-rate knots.
[Reconnaissance](2026-11-25-burr-math-data/search.py) ·
[Continuation](2026-11-25-burr-math-data/explore.py)

The all-free [paralogistic tie sample](2026-11-25-paralogis-math-report.md#all-free-fit)
normalizes to two log levels with weights $1/10000,9999/10000$ when the
shapes are untied. Positive affine changes of log data only rescale
$c$ and transform $s$, so the exact span of that sample is immaterial
to the Burr profile. The scan included this pattern, its reflection,
and six three-level splits. At $k=0.01,1,100$ the independently solved
profile on the first pattern was below Weibull. For the held-scale
paralogistic tie family, freeing the shapes gives one maximum throughout
the prior tie interval by the held-scale theorem below. The numerical
replay checks $r=.003,.004,.005$. [Lift replay](2026-11-25-burr-math-data/bounds-results.txt)

## Single holds

**Proved.** With $k$ held, the strictly concave and coercive
$(c,b)$ likelihood has one stationary point and one maximizer on
nonconstant data. Constant data are unbounded by setting $s$ to
their common value and increasing $c$. Neither named escape is
available at fixed positive $k$.

With $s$ held and $z_i=\log(x_i/s)$, profiling $k$ gives
$k=1/\overline{h(cz)}$. The [held-scale proof](2026-11-06-burr-scale-report.md)
gives strict concavity in $c$. For nonconstant data with any $z_i<0$
there is exactly one stationary point and it is the global maximum.
When every $z_i\ge0$ and some is positive, the strict supremum is
$-1-\log\bar z-(Z/n)\log2$ in the same likelihood units, where $Z$
counts zeros. The Pareto endpoint stays $s$, including when
$s<\min x_i$. This formula also covers constant data above $s$.
Constant data below or at $s$ are unbounded. The cases at and below
$s$ use the distinct concentration paths in the
[collapse proof](2026-11-02-burr-collapse-report.md). No Weibull escape
retains the held scale on this route.

### Held power has an exact global tie

**Proved by interval exclusion and a strict crossing.** For any fixed
$c=c_0>0$, define the powered observations by
$$
 (x_1^{c_0},\ldots,x_4^{c_0})=(1,e^{5.1},e^{r_*},e^8).
$$
The exact $r_*$ is the unique zero of the height difference defined
below in $(5.99,6.02)$. The transformation $u=x^{c_0}$ has a
parameter-independent Jacobian and gives Lomax with rate
$\lambda=s^{-c_0}$. For $t=\log\lambda$ and $v_i=\log u_i$, put
$$
 G=\overline{h(t+v)},\qquad Q=\overline{(1+e^{t+v})^{-1}},\qquad
 H(t)=t-\log G-G+\log\bar u .
$$
Here $k=1/G$ is the unique conditional shape and $H$ is the mean
profile gap above the optimized exponential. Write
$D=GQ-(1-Q)$, $q_i=(1+e^{t+v_i})^{-1}$. Then
$$
 H'=D/G,\qquad D'=(1-Q)Q-(1+G)\overline{q(1-q)}.
$$

For the whole interval $5.99\le r\le6.02$, the lower and higher
$t$ bands are $[-4.56,-4.50]$ and $[-1.39,-1.32]$, with
endpoints represented by the same dyadic knots throughout the checker.
Their inward scores and negative $D'$ give a unique interior maximum
in each band. A uniform feasible witness at $t=-4.53$ gives $H>0.25$.
For $t\le-16$, $G\ge\lambda\bar u-\lambda^2\overline{u^2}/2$ and
$\overline{u^2}/\bar u\le e^8$ give
$H\le-\log(1-e^{-8}/2)<0.001$.
For $t\ge16$, $G\ge t+\bar v$ gives
$H\le\log\bar u-\bar v-\log(t+\bar v)<0$.
On the compact complement of the bands, the checker divides the
$r$-range into 30 closed slabs of width $0.001$ and bisects $t$
cells. It excludes each cell by a nonzero score enclosure or by a
likelihood upper bound below the uniform witness. The 15,318 examined
cells include subdivisions. The curvature check subdivides each band
into 16 cells per slab.

Let $M_l(r),M_h(r)$ be the two band maxima. The derivative of either
height with respect to $r$ is
$e^r/\sum_i e^{v_i}-(1+1/G)(1-q_3)/4$ at its band root. The first
term cancels in $M_h'-M_l'$, which the checker encloses strictly
above zero on every slab. It brackets the endpoint roots and bounds
$M_h-M_l$ at $5.99$ in $(-0.0004220,-0.0004219)$ and at $6.02$
in $(0.0002413,0.0002414)$. Continuity and strict increase give the
exact tie and its cardinality. Below the tie the lower $t$ band
wins, and above it the higher band wins. The exponential face loses
throughout the stated interval. The two fitted scales follow from
$s=\exp(-t/c_0)$.

**Numerical.** The crossing is at $r_*\approx6.00930125958914913$
with $t\approx-4.5336755721,-1.3535077879$ and mean exponential gap
$0.2652480606$. At $r_*\pm10^{-8}$, the signed height differences
are approximately $\pm2.23\times10^{-10}$. A separate four-point
sample $v=(0,5,6,8)$ also has two finite local maxima by inward
score signs in disjoint bands, with unequal numerical heights.
[Held-power interval proof and replay](2026-11-25-burr-math-data/held_power_tie.py)

## Face comparisons and equality

Constant all-free samples are unbounded by the same concentration path as
with the first shape held. For nonconstant data the face values are finite.

**Proved.** The all-free likelihood-retaining ends are the Weibull
path $k\to\infty, s k^{-1/c}\to s_W>0$ and the Pareto path
$c\to\infty, kc\to\alpha>0, s\to x_{\min}$ at the optimized
endpoint. In the latter path the approach from below can remove the
endpoint penalty. The mean face values in the log-data likelihood are
$$
 P=-1-\log\overline{(y-y_{\min})},\qquad
 W=\max_{c>0} W(c),\qquad
 W(c)=\log c+c\bar y-\log\overline{e^{cy}}-1 .
$$
Holding $c$ keeps the Weibull path with its power fixed and removes
Pareto. Holding $s$ keeps the held-endpoint Pareto path, with the
penalty above, and removes Weibull. The [closure proof](2026-11-03-burr-closure-report.md)
gives the exact attainment criterion for nonconstant all-free data.
A finite MLE exists iff some finite point reaches or exceeds
$\max(P,W)$. A strict witness forces compact containment. At equality,
either the candidate already maximizes globally or a strictly better
finite witness gives compact containment. Equality of the two faces
alone gives no attainment conclusion. The prior exact finite/Pareto
tie is a tie between an attained maximizer and an escape sequence.
It does not imply two maximizing Burr parameter triples.

The following comparisons are sufficient and can use outward-rounded
inputs and arithmetic.

- **All-free finite witness.** Compute $P$ directly and bracket the
  unique zero of $W'(c)$, whose derivative is
  $-1/c^2-\operatorname{Var}_{e^{cy}}(y)<0$.
  For a signed bracket $[l,u]$, the tangent upper is
  $W(l)+W'(l)(u-l)$. A feasible Burr likelihood lower above both
  face uppers proves attainment. These are linear-time evaluations
  at each scalar bracket endpoint. The first-order seam coefficient
  is $\overline{v^2}/2-1$ at the Weibull fit with $\bar v=1$.
  When $W\ge P$, a positive coefficient suffices, as does exact
  zero with the positive profiled second-order coefficient from
  the closure proof. A negative coefficient only excludes a
  sufficiently small neighborhood of the Weibull end, using the
  signed tail bound in the [root-box derivation](2026-11-04-burr-root-data/derivation.md).
- **All-free face witness.** At least half the original observations
  tied at the minimum proves strict Pareto dominance, including
  every nonconstant two-observation sample. More generally use
  the pointwise Pareto exclusion for $0<k\le m/(n-m)$, where $m$
  counts minima, and the Weibull bounds on the other tail. For example,
  $F(k)\le W+n(k+1)/[2(k+1-n)^2]$ in mean units for $k>n-1$.
  If $P>W$, a single strict comparison at a large cutoff excludes
  the whole large-$k$ tail. A strictly winning named face also
  requires strict exclusion of the remaining compact region and
  the other face. A finite witness can lose to either face while
  exceeding the other.
- **Held power finite witness.** Any $H(t)>0$ suffices and exact
  $H(t)=0$ also gives attainment by the same closure argument with
  just one face. The exponential corresponds to fixed-power Weibull.
  The next two bounds give exact strict face tests or exclusions.

**Proved, held-power range test.** If $u_{\max}\le 2u_{\min}$, the
profile is strictly decreasing in $t$, so the exponential face
strictly wins, including at the range bound's equality. Put
$B=1-Q$, $C=\overline{(1-q)q}$, and $J=B/Q-G$.
Then $J'=C/Q^2-B$. For $\lambda>0$,
$(1+\lambda u_{\min})^2>1+\lambda u_{\max}$ implies
$q_{\min}>q_{\max}^2\ge Q^2$, and $C\ge Bq_{\min}$.
Thus $J'>0$, $J(-\infty)=0$, and $H'=-QJ/G<0$.
The raw Burr check is $c_0(\log x_{\max}-\log x_{\min})\le\log2$.
Intervals that overlap the threshold need another test.

**Proved, held-power signed tail.** Normalize $u$ to mean one and
let $a=\overline{u^2}/2<1$. From $G\ge\lambda-a\lambda^2$ and
$-\log(1-z)\le z+z^2$ for $0\le z\le1/2$,
$$
 H\le -(1-a)\lambda+(a+a^2)\lambda^2<0
 \quad\text{for}\quad
 0<\lambda\le\min\{1/(2a),(1-a)/[2a(1+a)]\}.
$$
This exclusion compares to the same exact exponential and has no
additive likelihood tolerance. It controls the small-rate tail, and
the remaining profile can still contain a finite winner.

**Proved, held-power equality with the face really occurs.** For two
powered observations $(1,r)$, normalize by their mean. For fixed
positive normalized rate, increasing $r>1$ strictly decreases $G$
by strict log concavity and hence strictly increases $H$. On
$2\le r\le100$, $a<1$ uniformly and the signed small-rate exclusion
is uniform. So is the large-rate exclusion, since $u_{\min}\ge2/101$
and $H\le-\log[u_{\min}\log(1+\lambda u_{\min})]$. At $r=2$
the range test gives strict loss, uniform on the remaining compact
rate interval. At $r=100$ the raw rate $1/2$ has a strictly
positive gap, enclosed in $(0.28607,0.28609)$. Continuity and
compactness give an interior transition where some finite point
equals the exponential and none is better. Strict monotonicity at
each fixed rate gives strict nonattainment below the transition and
strict improvement above it. The signed two-point branch replay is
near $r=28.80920557873453$. This numerical branch location is
separate from the compactness proof of the exact transition.
[Face checks](2026-11-25-burr-math-data/bounds.py) ·
[Prior Lomax criterion](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md)

## Inverse and shifted Burr

**Proved, exact transfers.** Inverse Burr uses $y_{\rm Burr}=1/x$
with the same shapes and reciprocal scale. The log likelihood changes
by $-2\sum_i\log x_i$. All local maximizers, global maximizers and
ties are carried bijectively, and all strict gaps are preserved.
The faces are inverse Weibull with scale $s k^{1/c}$ on its path
and power with upper endpoint $\max x_i$ when scale is free.
The minimum multiplicity test becomes a maximum multiplicity test.
With scale held the power endpoint stays $s$, and the trace penalty
is unchanged. Use $-\log x_i$ as Burr log observations and $x_i^{-c_0}$
as the conceptual powered observations when $c_0$ is held.
[Inverse Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md)

At held threshold $\mu$ in shifted Burr, use positive excesses
$x_i-\mu$, with unit Jacobian and unchanged regular coefficients.
Weibull and Pareto are fitted on these excesses, and the Pareto
endpoint on the input scale is $\min x_i$ with scale free, or
$\mu+s$ with scale held. The transfer preserves all four root-count
conclusions and both types of equality. [Shifted Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md)

**Proved, original-input requirement.** Exact bijectivity does not
justify a certificate on prematurely rounded reciprocal or excess
samples. Certifying attainment, root count or dominance requires an
enclosure covering the actual reflected logs or exact differences,
and their use throughout the root and competitor comparisons.
For close shifted observations, one can enclose
$$
 \log\frac{x_i-\mu}{x_0-\mu}
 =\log1p\!\left(\frac{x_i-x_0}{x_0-\mu}\right).
$$
Preserve ordering and ties by comparing original values. For the held
scale test compare $x_i$ directly with the inverse scale on the
inverse route, reversing the inequality as required. Compare the
exact excess to $s$ on the shifted route. A checked reevaluation
of one physical coefficient tuple only supplies a witness, since
it leaves the rest of the profile uncertified. The numerical
examples include adjacent binary64 values at 1.75 whose reciprocals
merge, and adjacent values at 1 whose excesses with
$\mu=-2^{54}$ merge. Outward log differences distinguish both.
[Original-input checks](2026-11-25-burr-math-data/bounds-results.txt)

**Proved, free threshold.** Let $j$ observations equal $x_{\min}$.
Set $\mu=x_{\min}-\varepsilon$ and fix $0<c<1$, $k,s>0$.
The $j$ minimum factors are asymptotic to positive constants times
$\varepsilon^{c-1}$, while the remaining factors have positive
limits. This endpoint spike gives $+\infty$ log likelihood, so the
two shifted Burr faces cannot compete on the fully free route.
Even with $c\ge1$ held, free $k,s$ permit the joint collapse
$k=1/T, s=e^{-T}, \mu=x_{\min}-s$. The minimum densities grow
as $c e^T/(2T)$, and each nonminimum density is asymptotic to
$c e^{-c}/[T(x_i-x_{\min})]$. The log likelihood grows as
$jT-n\log T+O(1)$. For fixed $k,s$, $c=1$ instead gives an
attained endpoint at $\mu=x_{\min}$ under the closed-support
convention, since decreasing each excess increases its Lomax density.
[Earlier threshold paths](2026-11-14-core5b-c-data/mathematics.md)

A free threshold also admits a proper limit as it recedes. Fix $k$
and set $\mu=-T, s=T+a, c=T/d$ with $d>0$. Then the Burr logit
tends to $(x-a)/d$ and $kc/(x+T)\to k/d$. The limiting density
is the generalized logistic density
$(k/d)e^{(x-a)/d}[1+e^{(x-a)/d}]^{-k-1}$ on the line, also obtained
pointwise from the survival function. This path has finite likelihood
on a fixed sample, so the unboundedness proof still determines the
fully free outcome.

## Verification

All five Python scripts finished successfully on the workstation
through the one-slot runner. Interval claims use 65-decimal
`mpmath.iv` enclosures. Likelihood and transition replays use
80-decimal `mpmath`. The two exploratory grids use binary64 NumPy.
No C++ or R build or test ran.

| Original binary64 sample | Joint $k,c,s$ from 80-digit solve | Total log likelihood | Total gain over Pareto or power | Total gain over Weibull or inverse Weibull |
|---|---|---:|---:|---:|
| Burr golden | $1.3346374721, 1.8604177041, 1.1969911178$ | −613.679796750830 | 631.47764214 | 41.49673700 |
| Inverse Burr golden | $0.8552562965, 2.2945593267, 1.1483262992$ | −625.406476968357 | 325.86134561 | 96.98461537 |

Both fixtures contain 500 observations. Direct inverse likelihood
evaluation and the reciprocal log-likelihood identity agreed within
$10^{-66}$. The replays include $k=1$, $c=2$ and $s=1$ held
separately on both samples. Independent outward witness comparisons
strictly beat both face uppers. The reported score roots and the
outward face comparisons serve different purposes. Existing
successful global covers supply the per-input uniqueness proof.
[Golden and transition replay](2026-11-25-burr-math-data/check-results.txt) ·
[Outward comparisons](2026-11-25-burr-math-data/bounds-results.txt)

The transition replays include the prior finite/Pareto equality family
at its local-branch crossing near $q=0.992910205805$, with offsets
$\pm10^{-8}$. They include the two-face crossing at
$0.238266027164$, where the finite witness exceeds both by more than
0.026 in total, and the zero Weibull slope near $0.043924384415$,
where the mean quadratic coefficient is positive. The two latter
checks use offsets $\pm10^{-8}$ and $\pm10^{-9}$, respectively.
For held scale, the log ratios $(\varepsilon,1)$ for
$\varepsilon=-10^{-12},0,10^{-12}$ give a finite root near
$c=1.6651\times10^6$, then the penalized Pareto value, then the
unpenalized value. The shifted checks use $c=1/2,1,2$ and
$\varepsilon=e^{-10},e^{-30},e^{-60}$, plus both simultaneous
location paths. These numerical checks locate replays of the
stated mechanisms. A local branch crossing alone does not identify
which branch globally wins.

Reproduce from the worktree root.

```sh
set -e
export PYTHONDONTWRITEBYTECODE=1
d=dev/research/evaluations/2026-11-25-burr-math-data
for script in search.py explore.py held_power_tie.py check.py bounds.py; do
  /tmp/umvl-cpu/run python3 -u "$d/$script"
done
```

## What this changes for the fitter

- All-free Burr needs a global comparison. A finite certificate can
  claim unique attainment when it encloses a unique conditional branch
  throughout the winning slab and excludes every other slab and both
  faces. The six-observation sample is an exact fixture for refuting
  any acceptance based only on finite stationarity and improvement
  over Weibull.
- Held scale and held first shape can certify uniqueness from their
  existing concavity and existence guards with coefficient enclosures.
  Use the original-data side and tie tests for the held endpoint.
  The scale-held Pareto or power stop must preserve the endpoint penalty.
- Held power can give a proved Weibull or inverse-Weibull stop from
  the range test. The signed rate bound can close part of a strict
  face certificate when the range test is inconclusive. Several
  finite roots may have to survive until the final comparison.
  A proven two-point maximizing set should retain both coefficient
  vectors with the non-unique flag. Overlapping likelihood
  enclosures alone cannot prove an exact tie.
- Use a strictly separated face certificate for the corresponding
  named stop. A proved equality with an attained finite maximum is a
  finite outcome. If the comparison or certificate allowance does
  not close and the existing search succeeds without a separated
  refutation, keep the search result marked uncertified. Failed
  searches and mathematically refuted results require a specific
  diagnostic. Transfer the certificate on original-input enclosures.
  Free shifted threshold with either proved divergence path takes
  the unbounded-likelihood outcome.

## Open

- **All-free finite global ties and the total stationary count.**
  Two strict finite local maxima are proved, but the exhibited
  pair and the reported nearby reversal are below Pareto. Resolving
  global multiplicity requires an exhaustive two-band cover and
  a strict crossing while both bands beat both faces, or a theorem
  excluding equal winning heights above $\max(P,W)$. A bound on
  the number of stationary points over all samples is also missing.
- **Weibull equality.** The attainment criterion includes a finite
  point tied with a winning Weibull face, but a separate exact
  all-free example of that type was not isolated. Near vanishing
  seam coefficients, a signed higher-order bound or a separated
  finite witness is needed for a strict decision.
- **Other free-threshold held slices.** The spike and simultaneous
  collapse cover the requested free-shape routes. Slices that hold
  $c\ge1$ and block the simultaneous collapse need their own
  attainment and root-count analysis if free-threshold support is
  extended.
